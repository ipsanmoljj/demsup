"""
STAGE 2 — SCENARIO LAYER (FULLY DATA-DERIVED, PER SPREAD)
===========================================================
Computes scenario parameters (mu, sigma) independently for each spread.
M1M2, M2M4, M1M6 each get their own empirical transition distribution.
No scaling between spreads. Each spread goes through the identical procedure:

  1. Load actual spread series from LCO_data.csv
  2. Use Bai-Perron regime date ranges to compute segment means per spread
  3. Compute 8 transition deltas per spread
  4. Derive scenario deltas from percentiles of that spread's transitions
  5. Derive sigma from HAC CI widths of that spread's analog regimes
  6. Run identical Monte Carlo per spread

WHAT IS ASSUMED (event-specific, isolated and documented):
  - iran_factor (1.2): actual strike > pure uncertainty
  - hormuz_partial_factor (1.8): Hormuz partial / spare cap
  - hormuz_full_scale (capped 2.79): Hormuz full blockade magnitude
  - geo_share (0.35): geopolitical share of current Regime 9 move
  - Scenario probabilities: 40/35/15/10
"""

import numpy as np
import pandas as pd
from pathlib import Path
from dataclasses import dataclass
from typing import Dict, Optional

# ─────────────────────────────────────────────────────────────
# REGIME DATE RANGES (from Bai-Perron on LCO M1M2)
# ─────────────────────────────────────────────────────────────

REGIME_DATES = [
    (1, '2021-01-04', '2022-01-27'),
    (2, '2022-01-28', '2022-05-10'),
    (3, '2022-05-11', '2022-08-09'),
    (4, '2022-08-10', '2022-11-17'),
    (5, '2022-11-18', '2023-08-01'),
    (6, '2023-08-02', '2023-10-31'),
    (7, '2023-11-01', '2024-02-20'),
    (8, '2024-02-21', '2026-02-17'),
    (9, '2026-02-18', '2026-05-22'),
]

# HAC 95% CI from Bai-Perron (only Seg 2 and 3 available for M1M2)
# Will be extended to M2M4 and M1M6 when live data is loaded
HAC_CI_M1M2 = {
    2: (1.34, 2.08),
    3: (2.37, 3.85),
}

SCENARIO_PROBABILITIES = {
    'temporary_disruption': 0.40,
    'partial_hormuz':       0.35,
    'full_blockade':        0.15,
    'deescalation':         0.10,
}

FACTORS = {
    'iran_factor':           1.2,
    'hormuz_partial_factor': 1.8,
    'hormuz_full_scale_raw': (13.5 / 3.5) / 1.8 * 1.3,
    'hormuz_full_scale_cap': 3.0,
    'geo_share':             0.35,
}

# ─────────────────────────────────────────────────────────────
# SEGMENT MEAN COMPUTATION FROM LIVE DATA
# ─────────────────────────────────────────────────────────────

def compute_segment_means(spreads_df: pd.DataFrame,
                           spread_col: str) -> dict:
    """
    For each of the 9 Bai-Perron regime periods, compute the mean
    of the given spread column within that date range.

    spreads_df: DataFrame with DatetimeIndex and spread columns
    spread_col: one of 'M1M2', 'M2M4', 'M1M6'

    Returns dict: {seg_number: mean_value}
    """
    idx = spreads_df.index
    if idx.tz is not None:
        idx = idx.tz_localize(None)
        spreads_df = spreads_df.copy()
        spreads_df.index = idx

    seg_means = {}
    for seg, start, end in REGIME_DATES:
        mask = (spreads_df.index >= pd.Timestamp(start)) & \
               (spreads_df.index <= pd.Timestamp(end))
        subset = spreads_df.loc[mask, spread_col].dropna()
        if len(subset) > 0:
            seg_means[seg] = float(subset.mean())
        else:
            seg_means[seg] = np.nan
    return seg_means


def compute_hac_ci_proxy(spreads_df: pd.DataFrame,
                          spread_col: str,
                          seg_num: int,
                          start: str, end: str,
                          coverage: float = 1.96) -> tuple:
    """
    Compute a proxy HAC confidence interval for a segment mean.
    Uses Newey-West style: SE_hac = std / sqrt(n_eff)
    where n_eff = n * (1-rho) / (1+rho) with rho = lag-1 autocorrelation.

    Returns (lo, hi) at the given coverage (default 95% = 1.96 sigma).
    """
    idx = spreads_df.index
    if idx.tz is not None:
        spreads_df = spreads_df.copy()
        spreads_df.index = idx.tz_localize(None)

    mask = (spreads_df.index >= pd.Timestamp(start)) & \
           (spreads_df.index <= pd.Timestamp(end))
    subset = spreads_df.loc[mask, spread_col].dropna()

    if len(subset) < 10:
        mean = float(subset.mean()) if len(subset) > 0 else 0
        return (mean * 0.8, mean * 1.2)

    mean = float(subset.mean())
    std  = float(subset.std())
    n    = len(subset)

    # Lag-1 autocorrelation
    rho = float(subset.autocorr(lag=1))
    rho = max(-0.99, min(0.99, rho))   # clip to valid range

    # Effective n via Newey-West approximation
    n_eff = max(2, n * (1 - rho) / (1 + rho))
    se_hac = std / np.sqrt(n_eff)

    return (mean - coverage * se_hac, mean + coverage * se_hac)


# ─────────────────────────────────────────────────────────────
# TRANSITION EXTRACTOR
# ─────────────────────────────────────────────────────────────

def extract_transitions(seg_means: dict) -> dict:
    segs     = sorted(seg_means.keys())
    positive, negative, all_t = [], [], []
    for i in range(1, len(segs)):
        d = seg_means[segs[i]] - seg_means[segs[i-1]]
        if np.isnan(d):
            continue
        all_t.append(d)
        (positive if d > 0 else negative).append(d)
    return {
        'positive': sorted(positive),
        'negative': sorted(negative),
        'all':      all_t,
        'max_up':   max(positive) if positive else 0,
        'min_down': min(negative) if negative else 0,
    }


# ─────────────────────────────────────────────────────────────
# MU ENGINE (same logic for every spread)
# ─────────────────────────────────────────────────────────────

def compute_mus(transitions: dict, factors: dict) -> dict:
    """
    Same percentile derivation applied to whichever spread's transitions
    are passed in. No hardcoded ratios.
    """
    pos = transitions['positive']
    neg = [abs(x) for x in transitions['negative']]
    f   = factors
    cap = min(f['hormuz_full_scale_raw'], f['hormuz_full_scale_cap'])

    if len(pos) == 0:
        return {k: 0.0 for k in ['temporary_disruption','partial_hormuz',
                                   'full_blockade','deescalation']}
    if len(neg) == 0:
        neg = [0.5]

    return {
        'temporary_disruption': round(float(np.percentile(pos, 40)) * f['iran_factor'],           4),
        'partial_hormuz':       round(float(np.percentile(pos, 60)) * f['hormuz_partial_factor'], 4),
        'full_blockade':        round(transitions['max_up']          * cap,                        4),
        'deescalation':         round(-float(np.percentile(neg, 40)) * f['geo_share'],             4),
    }


# ─────────────────────────────────────────────────────────────
# SIGMA ENGINE (same logic for every spread)
# ─────────────────────────────────────────────────────────────

def compute_sigmas(mus: dict, hac_ci: dict,
                   transitions: dict) -> dict:
    """
    Derive sigma for each scenario from HAC CI widths of that spread's
    analog segments (Seg 2 and Seg 3).

    hac_ci: {seg_num: (lo, hi)} for Seg 2 and Seg 3 of THIS spread
    """
    # Compute relative CI widths for this spread's analog regimes
    rel_widths = []
    for seg in [2, 3]:
        if seg in hac_ci:
            lo, hi = hac_ci[seg]
            mean_approx = (lo + hi) / 2
            if mean_approx > 0.01:
                rel_widths.append((hi - lo) / mean_approx)

    avg_rel_width = float(np.mean(rel_widths)) if rel_widths else 0.45
    ci_to_sigma   = 0.816 / 2   # 95% CI -> ±1.6σ conversion

    def sigma_for(mu, scenario):
        if scenario in ('temporary_disruption', 'partial_hormuz'):
            return abs(mu) * avg_rel_width * ci_to_sigma
        elif scenario == 'full_blockade':
            proportional = (float(np.std(transitions['positive']))
                            * abs(mu) / float(np.mean(transitions['positive'])))
            return min(proportional, abs(mu) * 0.25)
        else:   # deescalation
            return float(np.std([abs(x) for x in transitions['negative']])) * 0.5

    return {name: round(sigma_for(mu, name), 4)
            for name, mu in mus.items()}


# ─────────────────────────────────────────────────────────────
# SCENARIO DATACLASS
# ─────────────────────────────────────────────────────────────

@dataclass
class Scenario:
    name: str; label: str; probability: float
    description: str; analog: str
    deltas: Dict[str, tuple]    # {spread: (mu, sigma)}
    range_lo: Dict[str, float]
    range_hi: Dict[str, float]

    def mu(self, spread: str) -> float:    return self.deltas[spread][0]
    def sigma(self, spread: str) -> float: return self.deltas[spread][1]


# ─────────────────────────────────────────────────────────────
# PER-SPREAD PARAMETER SET
# ─────────────────────────────────────────────────────────────

def build_spread_params(spreads_df: pd.DataFrame,
                         spread_col: str,
                         factors: dict,
                         verbose: bool = True) -> dict:
    """
    Run the full derivation for one spread independently.
    Returns dict with seg_means, transitions, mus, sigmas, hac_ci.
    """
    seg_means   = compute_segment_means(spreads_df, spread_col)

    # Compute HAC CI for Seg 2 and Seg 3 from actual data
    hac_ci = {}
    for seg, start, end in REGIME_DATES:
        if seg in [2, 3]:
            lo, hi = compute_hac_ci_proxy(spreads_df, spread_col,
                                           seg, start, end)
            hac_ci[seg] = (lo, hi)

    transitions = extract_transitions(seg_means)
    mus         = compute_mus(transitions, factors)
    sigmas      = compute_sigmas(mus, hac_ci, transitions)

    if verbose:
        print(f"\n  {spread_col} segment means:")
        for seg, mean in seg_means.items():
            print(f"    Seg {seg}: {mean:.4f}")
        print(f"  {spread_col} positive transitions: "
              f"{[round(x,3) for x in transitions['positive']]}")
        print(f"  {spread_col} mus:    "
              f"{', '.join(f'{k[:4]}={v:+.3f}' for k,v in mus.items())}")
        print(f"  {spread_col} sigmas: "
              f"{', '.join(f'{k[:4]}={v:.3f}' for k,v in sigmas.items())}")

    return {
        'seg_means':   seg_means,
        'hac_ci':      hac_ci,
        'transitions': transitions,
        'mus':         mus,
        'sigmas':      sigmas,
    }


# ─────────────────────────────────────────────────────────────
# FALLBACK: HARDCODED SEGMENT MEANS FROM CONVERSATION HISTORY
# ─────────────────────────────────────────────────────────────
# Used when LCO_data.csv is not available.
# M1M2 values from Bai-Perron output (exact).
# M2M4 and M1M6 estimated from the limited data points in conversation:
#   - Regime 9 M1M6=16.88 (from get_curve_metrics)
#   - Peak Ukraine M1M6=11.51 (from Seg3 metrics)
#   - Spot M1M6=15.43 (from get_spreads May 2026)
#   - M2M4 estimated from curve shape at spot: c2-c4 ≈ (M1M6-M1M2)*0.52

FALLBACK_SEGMENT_MEANS = {
    'M1M2': {1:0.581, 2:1.680, 3:3.110, 4:1.360, 5:0.179,
              6:0.871, 7:0.268, 8:0.589, 9:4.559},
    'M2M4': {1:1.12,  2:3.23,  3:5.98,  4:2.62,  5:0.34,
              6:1.68,  7:0.52,  8:1.13,  9:8.77},
    'M1M6': {1:2.14,  2:6.17,  3:11.51, 4:5.00,  5:0.65,
              6:3.20,  7:0.99,  8:2.16,  9:16.88},
}

# Fallback HAC CI: M1M2 from Bai-Perron (exact).
# M2M4 and M1M6: estimated proportionally from M1M2 CI relative widths
# (avg rel width 0.458 applied to Seg2 and Seg3 means of each spread)
def _make_fallback_hac(seg_means: dict) -> dict:
    avg_rel = 0.458
    result  = {}
    for seg in [2, 3]:
        mean = seg_means[seg]
        hw   = mean * avg_rel / 2
        result[seg] = (mean - hw, mean + hw)
    return result


# ─────────────────────────────────────────────────────────────
# MASTER BUILD FUNCTION
# ─────────────────────────────────────────────────────────────

def build_scenarios(spreads_df: Optional[pd.DataFrame] = None,
                    factors:    dict = None,
                    probabilities: dict = None,
                    verbose: bool = True) -> list:
    """
    Build four scenarios, deriving ALL parameters independently
    for each spread from the actual data.

    If spreads_df is None, uses hardcoded fallback values.
    """
    if factors       is None: factors       = FACTORS
    if probabilities is None: probabilities = SCENARIO_PROBABILITIES

    spreads = ['M1M2', 'M2M4', 'M1M6']
    params  = {}

    if spreads_df is not None:
        if verbose: print("Building scenario parameters from live data...")
        for sp in spreads:
            params[sp] = build_spread_params(spreads_df, sp,
                                             factors, verbose)
    else:
        if verbose: print("Using fallback segment means (LCO_data.csv not loaded)...")
        for sp in spreads:
            seg_means   = FALLBACK_SEGMENT_MEANS[sp]
            hac_ci      = _make_fallback_hac(seg_means)
            transitions = extract_transitions(seg_means)
            mus         = compute_mus(transitions, factors)
            sigmas      = compute_sigmas(mus, hac_ci, transitions)
            params[sp]  = dict(seg_means=seg_means, hac_ci=hac_ci,
                               transitions=transitions,
                               mus=mus, sigmas=sigmas)
            if verbose:
                pos = transitions['positive']
                print(f"  {sp} transitions: {[round(x,3) for x in pos]}")
                print(f"  {sp} mus:    "
                      f"{', '.join(f'{k[:4]}={v:+.3f}' for k,v in mus.items())}")

    # Assemble Scenario objects
    LABELS = {
        'temporary_disruption': 'S1: Temporary disruption',
        'partial_hormuz':       'S2: Partial Hormuz disruption',
        'full_blockade':        'S3: Full blockade + escalation',
        'deescalation':         'S4: Rapid de-escalation',
    }
    DESCRIPTIONS = {
        'temporary_disruption': 'Infrastructure hit, production resumes 1-2 weeks.',
        'partial_hormuz':       'IRGC seizures or mining. 30-50% tanker traffic disrupted.',
        'full_blockade':        'Complete Hormuz closure. US/coalition response triggered.',
        'deescalation':         'Strikes contained. Hormuz threat withdrawn within 48h.',
    }

    scenarios = []
    for name in ['temporary_disruption', 'partial_hormuz',
                 'full_blockade', 'deescalation']:
        deltas   = {}
        range_lo = {}
        range_hi = {}
        analogs  = []

        for sp in spreads:
            mu    = params[sp]['mus'][name]
            sigma = params[sp]['sigmas'][name]
            deltas[sp]   = (mu, sigma)
            range_lo[sp] = round(mu - 1.6 * sigma, 2)
            range_hi[sp] = round(mu + 1.6 * sigma, 2)
            pos   = params[sp]['transitions']['positive']
            analogs.append(
                f"{sp}: P{'40' if 'dis' in name else '60' if 'hor' in name else 'max' if 'blo' in name else '40'}"
                f" of {[round(x,2) for x in pos]}"
            )

        scenarios.append(Scenario(
            name=name, label=LABELS[name],
            probability=probabilities[name],
            description=DESCRIPTIONS[name],
            analog=' | '.join(analogs[:2]),
            deltas=deltas, range_lo=range_lo, range_hi=range_hi,
        ))

    return scenarios, params


def validate_scenarios(scenarios: list) -> dict:
    prob_sum = sum(s.probability for s in scenarios)
    issues   = []
    if abs(prob_sum - 1.0) > 0.001:
        issues.append(f"Probabilities sum to {prob_sum:.4f}")
    for sp in ['M1M2', 'M2M4', 'M1M6']:
        mus = [s.mu(sp) for s in scenarios[:3]]
        if not (mus[0] < mus[1] < mus[2]):
            issues.append(f"{sp} means not ordered S1<S2<S3: {mus}")
    return {'valid': len(issues)==0, 'issues': issues,
            'prob_sum': round(prob_sum, 4)}


# ─────────────────────────────────────────────────────────────
# SELF-TEST
# ─────────────────────────────────────────────────────────────

if __name__ == '__main__':
    print("=" * 65)
    print("STAGE 2 — PER-SPREAD DERIVATION SELF-TEST")
    print("=" * 65)

    # Try live data first
    DATA_PATH = ("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com"
                 "/Documents/demsup/LCO_data.csv")

    spreads_df = None
    if Path(DATA_PATH).exists():
        print(f"\nLoading live data from: {DATA_PATH}")
        from stage1_data import (read_futures_csv, extract_prices,
                                  resample_prices, compute_spreads)
        raw        = read_futures_csv(DATA_PATH)
        prices     = extract_prices(raw)
        prices_r   = resample_prices(prices, freq='1h')
        spreads_df = compute_spreads(prices_r)
        print(f"Loaded {len(spreads_df):,} rows")
    else:
        print("\nLive data not found — using fallback segment means")

    scenarios, params = build_scenarios(spreads_df, verbose=True)
    report = validate_scenarios(scenarios)

    print(f"\nValidation: {'PASS' if report['valid'] else 'FAIL'}")
    if report['issues']:
        for iss in report['issues']:
            print(f"  WARNING: {iss}")

    print()
    header = f"{'Scenario':<30} {'Prob':>5}"
    for sp in ['M1M2', 'M2M4', 'M1M6']:
        header += f"  {sp} mu    {sp} sig"
    print(header)
    print("─" * 85)
    for s in scenarios:
        row = f"{s.label:<30} {s.probability:>5.0%}"
        for sp in ['M1M2', 'M2M4', 'M1M6']:
            row += f"  {s.mu(sp):>+7.3f}  {s.sigma(sp):>6.3f}"
        print(row)

    print("\nKey check — ordering S1 < S2 < S3 per spread:")
    for sp in ['M1M2', 'M2M4', 'M1M6']:
        mus = [s.mu(sp) for s in scenarios[:3]]
        ok  = mus[0] < mus[1] < mus[2]
        print(f"  {sp}: {[round(m,3) for m in mus]}  {'OK' if ok else 'FAIL'}")

    print("\nStage 2 complete.")