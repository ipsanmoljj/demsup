"""
src/demsup/data/futures_reader.py
----------------------------------
Reads the proprietary futures CSV format.

Confirmed format (from actual data inspection):
  Line 1 : #meta:1min||contract_num||field
  Index   : timestamp (UTC, irregular — event-driven, not every minute)
  Columns : c1||contract, c1||volume, c1||weighted_mid,
            c2||contract, c2||volume, c2||weighted_mid, ...  (up to c14)
  Fields  : 'contract' (ticker e.g. CLG2), 'volume' (float), 'weighted_mid' (float)
  NaNs    : far contracts frequently have NaN weighted_mid in off-hours

Public API
----------
    from demsup.data.futures_reader import FuturesFile

    ff = FuturesFile("CL_outrights_1min_t.csv")
    print(ff)                                   # summary

    prices = ff.prices()                        # weighted_mid wide DataFrame
    spreads = ff.spreads()                      # M1M2, M1M3, M1M6, M1M12
    ts = ff.term_structure()                    # full bundle
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Literal

import numpy as np
import pandas as pd

SEP = "||"


# ─── Core reader ──────────────────────────────────────────────────────────────

def read_futures_csv(path: str | Path) -> pd.DataFrame:
    """
    Read a futures CSV with #meta: header.
    Returns MultiIndex DataFrame (timestamp × (contract_num, field)).

    Faithful to manager's reader. Index is tz-aware UTC.
    """
    path = Path(path)

    with open(path, encoding="utf-8") as f:
        meta_line = f.readline().strip()

    parts  = meta_line.lstrip("#meta:").split(SEP)
    col_l0 = parts[1] if len(parts) > 1 else "contract_num"
    col_l1 = parts[2] if len(parts) > 2 else "field"

    df = pd.read_csv(path, index_col=0, parse_dates=True, comment="#")
    df.index = pd.to_datetime(df.index, utc=True)
    df.index.name = "timestamp"

    tuples = [tuple(col.split(SEP, 1)) for col in df.columns]
    df.columns = pd.MultiIndex.from_tuples(tuples, names=[col_l0, col_l1])

    df = df.sort_index()
    df = df[~df.index.duplicated(keep="first")]

    df.attrs.update({
        "source_path": str(path),
        "col_l0": col_l0,
        "col_l1": col_l1,
    })
    return df


# ─── FuturesFile ──────────────────────────────────────────────────────────────

@dataclass
class FuturesFile:
    """
    Enriched wrapper around a single futures CSV file.

    The data has 3 fields per contract: contract (ticker), volume, weighted_mid.
    weighted_mid is the primary price series for all spread and regime work.
    Timestamps are irregular (event-driven), not every-minute.
    """

    path: Path
    _df: pd.DataFrame = field(init=False, repr=False)

    def __post_init__(self):
        self.path = Path(self.path)
        self._df  = read_futures_csv(self.path)

    # ── Metadata properties ───────────────────────────────────────────────────

    @property
    def product(self) -> str:
        """e.g. 'CL' from 'CL_outrights_1min_t.csv'"""
        return self.path.stem.split("_")[0].upper()

    @property
    def contracts(self) -> list[str]:
        """Sorted list: ['c1', 'c2', ..., 'c14']"""
        names = self._df.columns.get_level_values(0).unique().tolist()
        return sorted(names, key=lambda x: int(re.sub(r"\D", "", x) or 0))

    @property
    def fields(self) -> list[str]:
        """e.g. ['contract', 'volume', 'weighted_mid']"""
        return sorted(self._df.columns.get_level_values(1).unique().tolist())

    @property
    def start(self) -> pd.Timestamp:
        return self._df.index.min()

    @property
    def end(self) -> pd.Timestamp:
        return self._df.index.max()

    # ── Primary accessors ─────────────────────────────────────────────────────

    def prices(
        self,
        contracts: list[str] | None = None,
        start: str | None = None,
        end: str | None = None,
        resample_to: str | None = None,
        fill_method: str | None = "ffill",
    ) -> pd.DataFrame:
        """
        weighted_mid prices — wide DataFrame, columns = c1 .. c14.

        Parameters
        ----------
        contracts : list, optional
            Subset of contracts. Default = all available.
        start, end : str, optional
            Date range filter, e.g. "2022-06-01".
        resample_to : str, optional
            Pandas offset: "1min", "5min", "1H", "4H", "1D".
            Because timestamps are irregular, resampling to a regular
            grid is almost always needed for spread calculations.
        fill_method : str or None
            How to fill NaN weighted_mids after resampling.
            "ffill" (default) carries last price forward.
            None = leave NaNs as-is.

        Returns
        -------
        pd.DataFrame
            timestamp × contract, values = weighted_mid price.
        """
        cols = contracts or self.contracts
        result = self._df.xs("weighted_mid", axis=1, level=1)[cols].copy()

        if start:
            result = result.loc[start:]
        if end:
            result = result.loc[:end]

        if resample_to:
            result = result.resample(resample_to).last()

        if fill_method == "ffill":
            result = result.ffill()

        return result

    def volumes(
        self,
        contracts: list[str] | None = None,
        resample_to: str | None = None,
    ) -> pd.DataFrame:
        """Volume wide DataFrame. Resample sums (not last) volume."""
        cols = contracts or self.contracts
        result = self._df.xs("volume", axis=1, level=1)[cols].copy()
        if resample_to:
            result = result.resample(resample_to).sum()
        return result

    def tickers(self, contract: str = "c1") -> pd.Series:
        """
        Return the actual ticker series for a contract (e.g. CLG2 → CLH2 on roll).
        Useful for identifying roll dates.
        """
        return self._df[contract]["contract"].dropna()

    # ── Spreads ───────────────────────────────────────────────────────────────

    def spreads(
        self,
        pairs: list[tuple[str, str]] | None = None,
        resample_to: str = "1h",
        fill_method: str = "ffill",
    ) -> pd.DataFrame:
        """
        Compute time spreads: near - far (positive = backwardation).

        Default pairs: M1M2, M1M3, M1M6, M1M12, M2M3, M3M6.

        Parameters
        ----------
        pairs : list of (near, far) tuples, optional
            e.g. [("c1","c2"), ("c1","c6")]. Default = standard set.
        resample_to : str
            Regular grid frequency. Strongly recommended given irregular source.
        """
        if pairs is None:
            pairs = [
                ("c1", "c2"),
                ("c1", "c3"),
                ("c1", "c6"),
                ("c1", "c12"),
                ("c2", "c3"),
                ("c3", "c6"),
            ]

        prices = self.prices(resample_to=resample_to, fill_method=fill_method)
        labels = {
            ("c1","c2"):  "M1M2",
            ("c1","c3"):  "M1M3",
            ("c1","c6"):  "M1M6",
            ("c1","c12"): "M1M12",
            ("c2","c3"):  "M2M3",
            ("c3","c6"):  "M3M6",
            ("c6","c12"): "M6M12",
        }

        result = {}
        for near, far in pairs:
            if near in prices.columns and far in prices.columns:
                label = labels.get((near, far), f"{near}_{far}")
                result[label] = prices[near] - prices[far]

        return pd.DataFrame(result)

    # ── Roll dates ────────────────────────────────────────────────────────────

    def roll_dates(self, contract: str = "c1") -> pd.DatetimeIndex:
        """
        Return timestamps where the front-month ticker changes (roll events).
        e.g. CLG2 → CLH2 on the Feb roll.
        """
        tickers = self.tickers(contract)
        rolls = tickers[tickers != tickers.shift(1)].iloc[1:]  # skip first row
        return rolls.index

    # ── Term structure bundle ─────────────────────────────────────────────────

    def term_structure(
        self,
        resample_to: str = "1h",
    ) -> dict[str, pd.DataFrame]:
        """
        Full term-structure bundle for the regime engine.

        Returns dict with:
            "prices"      wide price DataFrame (c1..c14)
            "spreads"     M1M2, M1M3, M1M6, M1M12, M2M3, M3M6
            "curve"       slope (M1-M6), curvature (M1-2×M3+M6), roll_yield
            "volumes"     total volume per contract per bar
            "roll_dates"  index of front-month roll events
        """
        prices = self.prices(resample_to=resample_to)
        spds   = self.spreads(resample_to=resample_to)
        vols   = self.volumes(resample_to=resample_to)

        curve = pd.DataFrame(index=prices.index)
        if "c1" in prices and "c6" in prices:
            curve["slope"] = prices["c1"] - prices["c6"]
        if all(c in prices for c in ["c1","c3","c6"]):
            curve["curvature"] = prices["c1"] - 2*prices["c3"] + prices["c6"]
        if "c1" in prices and "c2" in prices:
            p1, p2 = prices["c1"], prices["c2"]
            curve["roll_yield_ann"] = (p1 - p2) / p1.replace(0, np.nan) * (365/30)
            curve["contango"] = (p1 < p2).astype(int)

        return {
            "prices":     prices,
            "spreads":    spds,
            "curve":      curve,
            "volumes":    vols,
            "roll_dates": self.roll_dates(),
        }

    # ── Diagnostics ───────────────────────────────────────────────────────────

    def info(self) -> pd.DataFrame:
        """Per-contract data coverage summary."""
        prices = self._df.xs("weighted_mid", axis=1, level=1)
        rows = []
        for c in self.contracts:
            s = prices[c].dropna()
            rows.append({
                "contract": c,
                "non_null_rows": len(s),
                "null_pct": f"{prices[c].isna().mean():.0%}",
                "first_price": s.index.min().date() if len(s) else None,
                "last_price":  s.index.max().date() if len(s) else None,
                "price_min":   round(s.min(), 3) if len(s) else None,
                "price_max":   round(s.max(), 3) if len(s) else None,
            })
        return pd.DataFrame(rows)

    def __repr__(self) -> str:
        rolls = len(self.roll_dates())
        return (
            f"FuturesFile — {self.product}\n"
            f"  Source    : {self.path.name}\n"
            f"  Contracts : {self.contracts}\n"
            f"  Fields    : {self.fields}\n"
            f"  Date range: {self.start.date()} → {self.end.date()}\n"
            f"  Total rows: {len(self._df):,}  (irregular timestamps)\n"
            f"  Roll events (c1): {rolls}"
        )
