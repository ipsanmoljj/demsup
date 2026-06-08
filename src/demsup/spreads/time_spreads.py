"""
src/demsup/spreads/time_spreads.py
------------------------------------
Computes time spreads, butterflies, and curve metrics from FuturesFile data.
All outputs are regime-aware: each series comes with its own z-score,
percentile rank, and rolling statistics for regime-conditional comparison.

Primary consumers:
    - demsup.regime.classifier  → uses curve shape for regime classification
    - demsup.signals.generator  → uses z-scores for opportunity ranking
    - energy-dashboard API      → serves current spread values

Usage:
    from demsup.data.futures_reader import FuturesFile
    from demsup.spreads.time_spreads import SpreadEngine

    ff = FuturesFile("cl_outrights_1min.csv")
    engine = SpreadEngine(ff, field="weighted_mid")

    spreads = engine.compute_all(resample_to="1h")
    print(spreads["time_spreads"])     # M1M2, M1M3, M1M6, M1M12
    print(spreads["butterflies"])      # M1M2M3, M1M3M5, M2M4M6 etc.
    print(spreads["curve_metrics"])    # slope, curvature, roll_yield
    print(spreads["stats"])            # z-scores, percentiles, rolling stats
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any

import numpy as np
import pandas as pd

from demsup.data.futures_reader import FuturesFile


# ─── Configuration ────────────────────────────────────────────────────────────

# Time spread pairs: (label, near_contract, far_contract)
TIME_SPREAD_PAIRS = [
    ("M1M2",  "c1", "c2"),
    ("M1M3",  "c1", "c3"),
    ("M1M6",  "c1", "c6"),
    ("M1M12", "c1", "c12"),
    ("M2M3",  "c2", "c3"),
    ("M3M6",  "c3", "c6"),
    ("M6M12", "c6", "c12"),
]

# Butterfly structures: (label, front, belly, back)
# Value = front - 2*belly + back  (zero = linear curve; +ve = concave up)
BUTTERFLY_DEFS = [
    ("M1M2M3",   "c1", "c2",  "c3"),
    ("M1M3M5",   "c1", "c3",  "c5"),
    ("M1M3M6",   "c1", "c3",  "c6"),
    ("M2M4M6",   "c2", "c4",  "c6"),
    ("M3M6M12",  "c3", "c6",  "c12"),
    ("M1M6M12",  "c1", "c6",  "c12"),
]

ROLLING_WINDOWS = {
    "short":  21,    # ~1 month of trading days
    "medium": 63,    # ~1 quarter
    "long":   252,   # ~1 year
}


# ─── SpreadEngine ─────────────────────────────────────────────────────────────

class SpreadEngine:
    """
    Computes the full spread and butterfly universe from a FuturesFile.

    Attributes
    ----------
    ff : FuturesFile
        Source data.
    field : str
        Price field to use ("weighted_mid" recommended; "close" acceptable).
    prices : pd.DataFrame
        Wide price DataFrame (timestamp × contract).
    """

    def __init__(self, ff: FuturesFile, field: str = "weighted_mid"):
        if field not in ff.fields:
            available = ff.fields
            # Fall back gracefully
            fallback = "close" if "close" in available else available[0]
            import warnings
            warnings.warn(
                f"Field {field!r} not found; falling back to {fallback!r}. "
                f"Available: {available}"
            )
            field = fallback

        self.ff     = ff
        self.field  = field
        self.prices = ff.prices()

    # ── Primary compute method ────────────────────────────────────────────────

    def compute_all(
        self,
        resample_to: str | None = None,
        rolling_window: int = ROLLING_WINDOWS["medium"],
    ) -> dict[str, pd.DataFrame]:
        """
        Compute the full spread analytics bundle.

        Parameters
        ----------
        resample_to : str, optional
            Pandas offset alias ("1H", "4H", "1D"). None = use raw freq.
        rolling_window : int
            Window for z-score and rolling stats (default: 63 bars = 1 quarter).

        Returns
        -------
        dict with keys:
            "time_spreads"   pd.DataFrame  raw M1M2, M1M3, M1M6, M1M12 ...
            "butterflies"    pd.DataFrame  raw M1M2M3, M2M4M6 ...
            "curve_metrics"  pd.DataFrame  slope, curvature, roll_yield, contango_flag
            "stats"          pd.DataFrame  z-scores and percentiles for all above
            "current"        dict          latest values for dashboard display
        """
        prices = self.prices
        if resample_to:
            prices = prices.resample(resample_to).last()

        time_spreads  = self._compute_time_spreads(prices)
        butterflies   = self._compute_butterflies(prices)
        curve_metrics = self._compute_curve_metrics(prices)

        # Combine all series for stats
        all_series = pd.concat([time_spreads, butterflies, curve_metrics], axis=1)
        stats      = self._compute_stats(all_series, rolling_window)

        current = self._extract_current(all_series, stats)

        return {
            "time_spreads":  time_spreads,
            "butterflies":   butterflies,
            "curve_metrics": curve_metrics,
            "stats":         stats,
            "current":       current,
        }

    # ── Time spreads ──────────────────────────────────────────────────────────

    def _compute_time_spreads(self, prices: pd.DataFrame) -> pd.DataFrame:
        """near - far  (positive = backwardation, negative = contango)"""
        result = {}
        for label, near, far in TIME_SPREAD_PAIRS:
            if near in prices.columns and far in prices.columns:
                result[label] = prices[near] - prices[far]
        return pd.DataFrame(result, index=prices.index)

    # ── Butterflies ───────────────────────────────────────────────────────────

    def _compute_butterflies(self, prices: pd.DataFrame) -> pd.DataFrame:
        """
        Butterfly = front - 2*belly + back

        Interpretation:
            > 0  curve is concave up (hump at belly — unusual, bearish for belly)
            < 0  curve is concave down (common in deep contango)
            = 0  perfectly linear curve
        """
        result = {}
        for label, front, belly, back in BUTTERFLY_DEFS:
            if all(c in prices.columns for c in [front, belly, back]):
                result[label] = (
                    prices[front]
                    - 2 * prices[belly]
                    + prices[back]
                )
        return pd.DataFrame(result, index=prices.index)

    # ── Curve metrics ─────────────────────────────────────────────────────────

    def _compute_curve_metrics(self, prices: pd.DataFrame) -> pd.DataFrame:
        """
        Derived curve shape features used by the regime classifier.

        slope        M1 - M6  (strong signal: +ve backwardation, -ve contango)
        curvature    M1 - 2*M3 + M6  (convexity of the term structure)
        roll_yield   annualised (M1 - M2) / M1  (carry for storage holders)
        contango_flag  1 if M1 < M2 else 0  (binary regime signal)
        m1_level     Absolute M1 price (for regime-price interaction features)
        """
        df = pd.DataFrame(index=prices.index)

        if "c1" in prices.columns and "c6" in prices.columns:
            df["slope"] = prices["c1"] - prices["c6"]

        if all(c in prices.columns for c in ["c1", "c3", "c6"]):
            df["curvature"] = prices["c1"] - 2 * prices["c3"] + prices["c6"]

        if "c1" in prices.columns and "c2" in prices.columns:
            m1, m2 = prices["c1"], prices["c2"]
            df["roll_yield"]    = (m1 - m2) / m1.replace(0, np.nan) * (365 / 30)
            df["contango_flag"] = (m1 < m2).astype(int)

        if "c1" in prices.columns:
            df["m1_level"] = prices["c1"]

        return df

    # ── Statistics ────────────────────────────────────────────────────────────

    def _compute_stats(
        self, all_series: pd.DataFrame, window: int
    ) -> pd.DataFrame:
        """
        For each series compute rolling z-score, percentile rank, mean, std.
        Output columns are multi-indexed: (series_name, stat).
        """
        cols: dict[tuple[str, str], pd.Series] = {}

        for col in all_series.columns:
            s = all_series[col].dropna()
            if len(s) < window:
                continue

            roll = s.rolling(window, min_periods=window // 2)
            mu   = roll.mean()
            sigma = roll.std().replace(0, np.nan)

            # Z-score: deviation from rolling mean in rolling std units
            z = (s - mu) / sigma

            # Percentile rank over full history (unconditional)
            pct = s.rank(pct=True)

            # Rolling percentile (conditional on recent window)
            roll_pct = s.rolling(window).apply(
                lambda x: pd.Series(x).rank(pct=True).iloc[-1], raw=False
            )

            cols[(col, "value")]       = s
            cols[(col, "z_score")]     = z
            cols[(col, "pct_rank")]    = pct
            cols[(col, "roll_pct")]    = roll_pct
            cols[(col, "roll_mean")]   = mu
            cols[(col, "roll_std")]    = sigma

        if not cols:
            return pd.DataFrame()

        result = pd.DataFrame(cols)
        result.columns = pd.MultiIndex.from_tuples(result.columns)
        return result

    # ── Current snapshot ──────────────────────────────────────────────────────

    def _extract_current(
        self, all_series: pd.DataFrame, stats: pd.DataFrame
    ) -> dict[str, Any]:
        """
        Extract the latest value for each series for dashboard display.

        Returns a flat dict ready for JSON serialisation:
            {
                "M1M2": {
                    "value": 0.42,
                    "z_score": 1.8,
                    "pct_rank": 0.87,
                    "signal": "elevated_backwardation"
                },
                ...
            }
        """
        current: dict[str, Any] = {}

        for col in all_series.columns:
            latest_val = all_series[col].dropna().iloc[-1] if not all_series[col].dropna().empty else None

            entry: dict[str, Any] = {"value": latest_val}

            if not stats.empty and col in stats.columns.get_level_values(0):
                for stat in ["z_score", "pct_rank", "roll_mean", "roll_std"]:
                    if stat in stats[col].columns:
                        v = stats[col][stat].dropna()
                        entry[stat] = float(v.iloc[-1]) if not v.empty else None

            # Add human-readable signal label
            entry["signal"] = _classify_spread_signal(col, entry)
            current[col] = entry

        return current


# ─── Signal classification helpers ───────────────────────────────────────────

def _classify_spread_signal(name: str, entry: dict) -> str:
    """
    Map a spread's current z-score into a trading signal label.
    These labels feed directly into the opportunity ranker.
    """
    z = entry.get("z_score")
    val = entry.get("value")

    if z is None or val is None:
        return "insufficient_data"

    # For the M1-M2 spread (backbone of curve regime)
    if name == "M1M2":
        if val > 0:
            if z > 2.0:   return "extreme_backwardation"
            if z > 1.0:   return "elevated_backwardation"
            return "mild_backwardation"
        else:
            if z < -2.0:  return "extreme_contango"
            if z < -1.0:  return "elevated_contango"
            return "mild_contango"

    # For butterflies
    if "M" in name and name.count("M") == 3:
        if abs(z) > 2.0:  return "butterfly_extreme"
        if abs(z) > 1.0:  return "butterfly_dislocated"
        return "butterfly_fair"

    # Generic
    if z > 2.0:  return "extreme_high"
    if z > 1.0:  return "elevated"
    if z < -2.0: return "extreme_low"
    if z < -1.0: return "depressed"
    return "normal"


# ─── Convenience function ─────────────────────────────────────────────────────

def build_spread_bundle(
    csv_path: str | Path,
    field: str = "weighted_mid",
    resample_to: str | None = "1h",
) -> dict[str, pd.DataFrame]:
    """
    One-liner: load a CSV and compute full spread analytics.

    Usage:
        bundle = build_spread_bundle("cl_outrights_1min.csv", resample_to="1h")
        m1m2 = bundle["time_spreads"]["M1M2"]
        fly  = bundle["butterflies"]["M1M2M3"]
        z    = bundle["stats"]["M1M2"]["z_score"]
    """
    ff     = FuturesFile(csv_path)
    engine = SpreadEngine(ff, field=field)
    return engine.compute_all(resample_to=resample_to)
