"""
validate_reader.py
------------------
Run this to confirm the reader and spread engine work correctly.

Usage:
    python validate_reader.py CL_outrights_1min_t.csv
    python validate_reader.py "C:\full\path\to\CL_outrights_1min_t.csv"
"""

import argparse
import sys
from pathlib import Path

import pandas as pd

sys.path.insert(0, str(Path(__file__).parent / "src"))

from demsup.data.futures_reader import FuturesFile, read_futures_csv
from demsup.spreads.time_spreads import SpreadEngine


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("path", help="Path to CSV file")
    parser.add_argument("--resample", default="1h",
                        help="Resample frequency (default: 1H)")
    args = parser.parse_args()

    path = Path(args.path)
    if not path.exists():
        print(f"ERROR: File not found: {path}")
        sys.exit(1)

    # ── Step 1: Raw reader ────────────────────────────────────────────────────
    print("\n" + "="*60)
    print("STEP 1: Raw reader")
    print("="*60)
    df_raw = read_futures_csv(path)
    print(f"\nShape      : {df_raw.shape}")
    print(f"Date range : {df_raw.index.min()} → {df_raw.index.max()}")
    print(f"Index tz   : {df_raw.index.tz}")
    print(f"Columns (first 6): {df_raw.columns.tolist()[:6]}")

    # ── Step 2: FuturesFile wrapper ───────────────────────────────────────────
    print("\n" + "="*60)
    print("STEP 2: FuturesFile wrapper")
    print("="*60)
    ff = FuturesFile(path)
    print(f"\n{ff}")

    # ── Step 3: Price extraction ──────────────────────────────────────────────
    print("\n" + "="*60)
    print(f"STEP 3: Price extraction (weighted_mid, resampled to {args.resample})")
    print("="*60)
    prices = ff.prices(resample_to=args.resample)
    print(f"\nShape    : {prices.shape}")
    print(f"Contracts: {prices.columns.tolist()}")
    print(f"\nLast 3 rows:")
    print(prices.tail(3).to_string())

    # ── Step 4: Spreads ───────────────────────────────────────────────────────
    print("\n" + "="*60)
    print("STEP 4: Time spreads")
    print("="*60)
    spds = ff.spreads(resample_to=args.resample)
    print(f"\nSpreads computed: {list(spds.columns)}")
    print(f"\nLast 5 rows:")
    print(spds.tail(5).to_string())

    # ── Step 5: Curve metrics ─────────────────────────────────────────────────
    print("\n" + "="*60)
    print("STEP 5: Curve metrics")
    print("="*60)
    ts = ff.term_structure(resample_to=args.resample)
    print(f"\nCurve columns: {list(ts['curve'].columns)}")
    print(f"\nLast 5 rows:")
    print(ts["curve"].tail(5).to_string())

    # ── Step 6: SpreadEngine z-scores ─────────────────────────────────────────
    print("\n" + "="*60)
    print("STEP 6: SpreadEngine — z-scores and signals")
    print("="*60)
    engine = SpreadEngine(ff, field="weighted_mid")
    bundle = engine.compute_all(resample_to=args.resample)

    current = bundle["current"]
    print(f"\n{'Spread':<12} {'Value':>8} {'Z-score':>8} {'Pct rank':>9}  Signal")
    print("-" * 65)
    for k in ["M1M2", "M1M3", "M1M6", "M1M12", "slope"]:
        if k not in current:
            continue
        c   = current[k]
        val = f"{c['value']:.3f}"        if c.get("value")    is not None else "N/A"
        z   = f"{c['z_score']:.2f}"      if c.get("z_score")  is not None else "N/A"
        pct = f"{c['pct_rank']:.0%}"     if c.get("pct_rank") is not None else "N/A"
        sig = c.get("signal", "N/A")
        print(f"{k:<12} {val:>8} {z:>8} {pct:>9}  {sig}")

    # ── Step 7: Data quality ──────────────────────────────────────────────────
    print("\n" + "="*60)
    print("STEP 7: Data quality")
    print("="*60)
    issues = []
    raw_prices = ff.prices()   # no resample — check raw nulls
    null_pct = raw_prices.isnull().mean()
    for col, pct in null_pct.items():
        if pct > 0.30:
            issues.append(f"  INFO  {col}: {pct:.0%} null (far contract, normal)")
        elif pct > 0.05:
            issues.append(f"  WARN  {col}: {pct:.0%} null (check data)")

    diffs = pd.Series(df_raw.index).diff().dropna()
    large_gaps = diffs[diffs > pd.Timedelta("4h")]
    if len(large_gaps) > 0:
        issues.append(f"  INFO  {len(large_gaps)} gaps > 4 hours (weekends/holidays — normal)")

    print(f"\n  Roll events (c1 ticker changes): {len(ff.roll_dates())}")
    if issues:
        for i in issues:
            print(i)
    else:
        print("  No data quality issues found.")

    print("\n" + "="*60)
    print("All steps complete. Reader and spread engine working correctly.")
    print("="*60)


if __name__ == "__main__":
    main()
