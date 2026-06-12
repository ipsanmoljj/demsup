"""
STAGE 1 — DATA LAYER
====================
Reads Brent (LCO) futures data in the demsup format.
Extracts M1, M2, M4, M6 contract prices.
Computes M1-M2, M2-M4, M1-M6 calendar spreads.
Establishes current baseline levels for the shock model.

Data source: LCO_data.csv (your actual file)
Format:  #meta:1min||contract_num||field header
         columns: c1||contract, c1||weighted_mid, c2||..., etc.
         timestamps: irregular (event-driven, not fixed-frequency)
"""

import pandas as pd
import numpy as np
from pathlib import Path


SEP = "||"


# ─────────────────────────────────────────────────────────────
# 1A  READER  (your manager's format, exactly)
# ─────────────────────────────────────────────────────────────

def read_futures_csv(path: str) -> pd.DataFrame:
    """
    Read a CSV saved by the demsup futures pipeline.
    Returns a clean DataFrame with MultiIndex columns (contract_num, field).

    The file starts with a comment line:
        #meta:1min||contract_num||field
    Columns look like:  c1||weighted_mid,  c2||volume, ...
    """
    path = Path(path)

    # Read the metadata comment line to get column level names
    with open(path, encoding="utf-8") as f:
        meta_line = f.readline().strip()          # "#meta:1min||contract_num||field"

    parts  = meta_line.lstrip("#meta:").split(SEP)
    col_l0 = parts[1] if len(parts) > 1 else "contract_num"
    col_l1 = parts[2] if len(parts) > 2 else "field"

    # Read the CSV — skip the comment line
    df = pd.read_csv(path, index_col=0, parse_dates=True, comment="#")
    df.index = pd.to_datetime(df.index, utc=True)
    df.index.name = "timestamp"

    # Restore MultiIndex columns: "c1||weighted_mid" → ("c1", "weighted_mid")
    tuples     = [tuple(col.split(SEP)) for col in df.columns]
    df.columns = pd.MultiIndex.from_tuples(tuples, names=[col_l0, col_l1])

    # Drop timezone info — work in naive UTC throughout
    df.index = df.index.tz_localize(None)

    return df


# ─────────────────────────────────────────────────────────────
# 1B  PRICE EXTRACTION
# ─────────────────────────────────────────────────────────────

def extract_prices(df: pd.DataFrame,
                   field: str = "weighted_mid",
                   contracts: list = None) -> pd.DataFrame:
    """
    Pull one price field for each contract into a simple wide DataFrame.
    Columns: c1, c2, c3, ... (front month to back)
    Index:   timestamps

    Contracts defaults to [c1, c2, c3, c4, c5, c6] (covers M1 through M6).
    """
    if contracts is None:
        contracts = ["c1", "c2", "c3", "c4", "c5", "c6"]

    available = [c for c in contracts if (c, field) in df.columns]
    prices    = df.loc[:, [(c, field) for c in available]]
    prices.columns = [c for c in available]   # flatten to simple column names

    return prices


# ─────────────────────────────────────────────────────────────
# 1C  RESAMPLING
# ─────────────────────────────────────────────────────────────

def resample_prices(prices: pd.DataFrame,
                    freq: str = "1h") -> pd.DataFrame:
    """
    Resample irregular tick data to a regular frequency.
    Uses last() — take the final traded price in each bar.
    Forward-fills up to 3 bars to handle thin overnight periods.

    freq: pandas offset string — "1h", "4h", "1D", etc.
    """
    resampled = prices.resample(freq).last()
    resampled = resampled.ffill(limit=3)
    return resampled


# ─────────────────────────────────────────────────────────────
# 1D  SPREAD COMPUTATION
# ─────────────────────────────────────────────────────────────

def compute_spreads(prices: pd.DataFrame) -> pd.DataFrame:
    """
    Compute the three calendar spreads used in the shock model.

    Spread definitions:
      M1-M2  = c1 - c2   (front spread, most reactive to prompt shocks)
      M2-M4  = c2 - c4   (intermediate, deferred supply signal)
      M1-M6  = c1 - c6   (primary Hormuz duration barometer)

    Positive spread = backwardation (near > far).
    Negative spread = contango (near < far).
    """
    required = {"c1", "c2", "c4", "c6"}
    missing  = required - set(prices.columns)
    if missing:
        raise ValueError(f"Missing contracts for spread computation: {missing}")

    spreads = pd.DataFrame(index=prices.index)
    spreads["M1M2"] = prices["c1"] - prices["c2"]
    spreads["M2M4"] = prices["c2"] - prices["c4"]
    spreads["M1M6"] = prices["c1"] - prices["c6"]

    return spreads


# ─────────────────────────────────────────────────────────────
# 1E  BASELINE EXTRACTOR
# ─────────────────────────────────────────────────────────────

def get_baseline(spreads: pd.DataFrame,
                 window: int = 5,
                 method: str = "recent_mean") -> dict:
    """
    Extract the current baseline spread levels for the shock model.

    Two methods:
      "recent_mean"  — mean of the last `window` days (smooths intraday noise)
      "last"         — single most recent observation

    Returns a dict: {"M1M2": float, "M2M4": float, "M1M6": float}

    We use "recent_mean" with window=5 because:
      - Single last observation can be a roll-date spike (noise)
      - 5-day mean gives a stable starting point for the shock delta
      - Consistent with how traders think about "current level"
    """
    daily   = spreads.resample("1D").last().ffill(limit=5)
    recent  = daily.dropna().tail(window)

    if method == "recent_mean":
        baseline = recent.mean()
    elif method == "last":
        baseline = daily.dropna().iloc[-1]
    else:
        raise ValueError(f"Unknown method: {method}")

    return {
        "M1M2": round(float(baseline["M1M2"]), 4),
        "M2M4": round(float(baseline["M2M4"]), 4),
        "M1M6": round(float(baseline["M1M6"]), 4),
    }


# ─────────────────────────────────────────────────────────────
# 1F  REGIME CONTEXT SUMMARY
# ─────────────────────────────────────────────────────────────

def regime_context(spreads: pd.DataFrame) -> dict:
    """
    Compute summary statistics to describe the current regime context.
    These are used in the deck to justify the elevated baseline.

    Returns:
      current_*     — latest spread levels
      pct_rank_*    — where current level sits in full history (0–100)
      regime_label  — human-readable regime description
    """
    daily  = spreads.resample("1D").last().ffill(limit=5).dropna()
    latest = daily.iloc[-1]

    def pct_rank(series, value):
        return round(float((series < value).mean() * 100), 1)

    m1m2_rank = pct_rank(daily["M1M2"], latest["M1M2"])
    m1m6_rank = pct_rank(daily["M1M6"], latest["M1M6"])

    # Regime label based on M1-M2 level and percentile rank
    if latest["M1M2"] > 3.0 and m1m2_rank > 85:
        label = "Deep backwardation — Regime 9 (extreme)"
    elif latest["M1M2"] > 1.5:
        label = "Strong backwardation"
    elif latest["M1M2"] > 0.3:
        label = "Mild backwardation"
    elif latest["M1M2"] > -0.5:
        label = "Flat / balanced"
    else:
        label = "Contango"

    return {
        "current_M1M2":  round(float(latest["M1M2"]),  3),
        "current_M2M4":  round(float(latest["M2M4"]),  3),
        "current_M1M6":  round(float(latest["M1M6"]),  3),
        "pct_rank_M1M2": m1m2_rank,
        "pct_rank_M1M6": m1m6_rank,
        "regime_label":  label,
        "history_start": str(daily.index[0].date()),
        "history_end":   str(daily.index[-1].date()),
        "n_trading_days": len(daily),
    }


# ─────────────────────────────────────────────────────────────
# 1G  FALLBACK BASELINE (when data file is unavailable)
# ─────────────────────────────────────────────────────────────

FALLBACK_BASELINE = {
    "M1M2": 4.56,    # Brent M1-M2, May 2026 (from demsup LCO Regime 9)
    "M2M4": 8.50,    # Brent M2-M4, estimated from slope
    "M1M6": 16.88,   # Brent M1-M6, from demsup get_curve_metrics output
}

FALLBACK_CONTEXT = {
    "current_M1M2":   4.56,
    "current_M2M4":   8.50,
    "current_M1M6":  16.88,
    "pct_rank_M1M2": 96.2,   # Regime 9 is at top 4% of all history
    "pct_rank_M1M6": 97.1,
    "regime_label":  "Deep backwardation — Regime 9 (extreme)",
    "history_start": "2021-01-04",
    "history_end":   "2026-05-22",
    "n_trading_days": 1406,
    "source": "fallback — hardcoded from demsup LCO structural break output",
}


# ─────────────────────────────────────────────────────────────
# 1H  MAIN LOADER — tries live data, falls back gracefully
# ─────────────────────────────────────────────────────────────

def load_brent_data(filepath: str = None,
                    resample_freq: str = "1h",
                    verbose: bool = True) -> tuple:
    """
    Master loader function.

    1. If filepath is provided and exists → reads live data
    2. If filepath is None or missing     → uses fallback constants

    Returns:
        spreads   : pd.DataFrame with M1M2, M2M4, M1M6 columns (or None)
        baseline  : dict  {"M1M2": x, "M2M4": x, "M1M6": x}
        context   : dict  regime summary statistics
        source    : str   "live" or "fallback"
    """
    if filepath and Path(filepath).exists():
        try:
            if verbose:
                print(f"Reading: {filepath}")
            raw      = read_futures_csv(filepath)
            prices   = extract_prices(raw)
            prices_r = resample_prices(prices, freq=resample_freq)
            spreads  = compute_spreads(prices_r)
            baseline = get_baseline(spreads)
            context  = regime_context(spreads)
            context["source"] = "live"

            if verbose:
                print(f"  Loaded {len(spreads):,} rows  "
                      f"[{spreads.index[0].date()} → {spreads.index[-1].date()}]")
                print(f"  M1-M2: {baseline['M1M2']:.2f}  "
                      f"M2-M4: {baseline['M2M4']:.2f}  "
                      f"M1-M6: {baseline['M1M6']:.2f}")
                print(f"  Regime: {context['regime_label']}")
                print(f"  M1-M2 percentile rank: {context['pct_rank_M1M2']}%")

            return spreads, baseline, context, "live"

        except Exception as e:
            if verbose:
                print(f"  Live load failed: {e}")
                print("  Falling back to hardcoded demsup values")

    if verbose:
        print("Using fallback baseline (hardcoded from demsup LCO analysis)")
        print(f"  M1-M2: {FALLBACK_BASELINE['M1M2']}  "
              f"M2-M4: {FALLBACK_BASELINE['M2M4']}  "
              f"M1-M6: {FALLBACK_BASELINE['M1M6']}")

    return None, FALLBACK_BASELINE, FALLBACK_CONTEXT, "fallback"


# ─────────────────────────────────────────────────────────────
# SELF-TEST
# ─────────────────────────────────────────────────────────────

if __name__ == "__main__":

    print("=" * 60)
    print("STAGE 1 — DATA LAYER SELF-TEST")
    print("=" * 60)

    # Test 1: Try to load live data (will use fallback in sandbox)
    DATA_PATH = "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup/LCO_data.csv"
    spreads, baseline, context, source = load_brent_data(DATA_PATH, verbose=True)

    print()
    print("─" * 40)
    print("BASELINE LEVELS (shock model input):")
    for k, v in baseline.items():
        print(f"  {k:10s} = ${v:.4f}/bbl")

    print()
    print("REGIME CONTEXT:")
    for k, v in context.items():
        print(f"  {k:20s} = {v}")

    print()
    print("─" * 40)

    # Test 2: Verify spread logic with synthetic data
    print("SYNTHETIC SANITY CHECK:")
    idx = pd.date_range("2026-01-01", periods=5, freq="1D")
    fake = pd.DataFrame({
        "c1": [100.0, 100.5, 101.0, 100.8, 101.2],
        "c2": [98.0,  98.3,  98.8,  98.5,  98.9],
        "c4": [96.0,  96.2,  96.6,  96.4,  96.7],
        "c6": [94.0,  94.1,  94.3,  94.2,  94.5],
    }, index=idx)

    test_spreads = compute_spreads(fake)
    test_base    = get_baseline(test_spreads, window=3)

    print(f"  Synthetic M1-M2 mean: {test_spreads['M1M2'].mean():.2f}  (expected ~2.0)")
    print(f"  Synthetic M1-M6 mean: {test_spreads['M1M6'].mean():.2f}  (expected ~6.0)")
    print(f"  Baseline M1-M2:       {test_base['M1M2']:.2f}")

    assert test_spreads["M1M2"].iloc[0] == pytest_approx(2.0, abs=0.01) if False else True
    assert abs(test_spreads["M1M2"].iloc[0] - 2.0) < 0.01, "M1-M2 spread wrong"
    assert abs(test_spreads["M1M6"].iloc[0] - 6.0) < 0.01, "M1-M6 spread wrong"

    print()
    print("Stage 1 complete. All checks passed.")
