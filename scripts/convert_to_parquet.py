"""
scripts/convert_to_parquet.py
------------------------------
One-time conversion: reads your 6 CSV files, writes Parquet.
Run once. After this you load from Parquet (10x faster).

All 6 files have the same #meta: format — confirmed from data inspection.

HOW TO RUN (from your demsup_repo folder):
    venv\Scripts\activate
    python scripts\convert_to_parquet.py
"""

from pathlib import Path
import sys

import pandas as pd

SEP = "||"

# ── Edit these two paths ──────────────────────────────────────────────────────
RAW_DIR = Path(
    r"C:\Users\kanwar.singh\OneDrive - hertshtengroup.com\Documents\demsup"
)
OUT_DIR = RAW_DIR / "parquet"   # output goes into a parquet/ subfolder
# ─────────────────────────────────────────────────────────────────────────────

FILES = [
    "CL_data.csv",
    "CL_outrights_1min_t.csv",
    "HO_data.csv",
    "LCO_data.csv",
    "LGO_data.csv",
    "wtcl_lco_outrights_1min.csv",
]


def read_and_flatten(path: Path) -> pd.DataFrame:
    """
    Read meta-format CSV and flatten MultiIndex columns for Parquet storage.
    MultiIndex ("c1","weighted_mid") → flat column name "c1__weighted_mid".
    """
    with open(path, encoding="utf-8") as f:
        meta_line = f.readline().strip()

    parts  = meta_line.lstrip("#meta:").split(SEP)
    col_l0 = parts[1] if len(parts) > 1 else "contract_num"
    col_l1 = parts[2] if len(parts) > 2 else "field"

    df = pd.read_csv(path, index_col=0, parse_dates=True, comment="#")
    df.index = pd.to_datetime(df.index, utc=True)
    df.index.name = "timestamp"

    # Rebuild MultiIndex then flatten to single-level for Parquet
    tuples = [tuple(col.split(SEP, 1)) for col in df.columns]
    df.columns = pd.MultiIndex.from_tuples(tuples, names=[col_l0, col_l1])
    df.columns = ["__".join(t) for t in df.columns]   # c1__weighted_mid

    df = df.sort_index()
    df = df[~df.index.duplicated(keep="first")]

    # Store metadata so we can reconstruct MultiIndex when loading
    df.attrs["col_l0"] = col_l0
    df.attrs["col_l1"] = col_l1
    return df


def convert(path: Path, out_dir: Path) -> None:
    print(f"\n  Reading  : {path.name}")
    df = read_and_flatten(path)
    print(f"  Rows     : {len(df):,}")
    print(f"  Columns  : {len(df.columns)}")
    print(f"  Range    : {df.index.min().date()} → {df.index.max().date()}")

    out_path = out_dir / (path.stem + ".parquet")
    df.to_parquet(out_path, compression="zstd", index=True)

    raw_mb = path.stat().st_size / 1e6
    out_mb = out_path.stat().st_size / 1e6
    print(f"  Size     : {raw_mb:.0f} MB → {out_mb:.0f} MB  "
          f"({raw_mb/out_mb:.1f}x smaller)")
    print(f"  Saved    : {out_path.name}")


def main():
    print(f"Raw data : {RAW_DIR}")
    print(f"Output   : {OUT_DIR}")

    if not RAW_DIR.exists():
        print(f"\nERROR: folder not found:\n  {RAW_DIR}")
        print("Edit RAW_DIR at the top of this script.")
        sys.exit(1)

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    ok, skip, fail = [], [], []

    for name in FILES:
        path = RAW_DIR / name
        print(f"\n{'─'*55}")
        if not path.exists():
            print(f"  SKIP     : {name}  (not found)")
            skip.append(name)
            continue
        try:
            convert(path, OUT_DIR)
            ok.append(name)
        except Exception as e:
            print(f"  ERROR    : {e}")
            fail.append(name)

    print(f"\n{'='*55}")
    print(f"Done.  OK={len(ok)}  Skip={len(skip)}  Fail={len(fail)}")
    total = sum(f.stat().st_size for f in OUT_DIR.glob("*.parquet")) / 1e6
    print(f"Total Parquet size: {total:.0f} MB  →  {OUT_DIR}")
    if fail:
        print(f"Failed files: {fail}")
        print("Paste the error above and we will fix the reader.")


if __name__ == "__main__":
    main()
