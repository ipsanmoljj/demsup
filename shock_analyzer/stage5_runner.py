"""
STAGE 5 — MASTER RUNNER
========================
Ties all four stages together into one clean results object.
This is what the plotting and deck stages will consume.

Single entry point: run_analysis(filepath=None)
Returns: AnalysisResults containing everything needed for plots and slides.
"""

import numpy as np
import pandas as pd
from dataclasses import dataclass, field
from typing import Dict, Optional

from stage1_data        import load_brent_data, FALLBACK_BASELINE, FALLBACK_CONTEXT
from stage2_scenarios   import build_scenarios, validate_scenarios
from stage3_distributions import compute_all_distributions, SpreadDistribution
from stage4_calibration import (build_historical_evidence,
                                 BRENT_REGIME_SEGMENTS,
                                 probability_sensitivity)


# ─────────────────────────────────────────────────────────────
# 5A  RESULTS CONTAINER
# ─────────────────────────────────────────────────────────────

@dataclass
class AnalysisResults:
    """
    Everything the plotting and deck layers need in one object.
    """
    # Inputs
    scenarios:        list
    baseline:         dict
    context:          dict
    data_source:      str

    # Outputs
    distributions:    Dict[str, SpreadDistribution]

    # Calibration
    historical:       list
    regime_segments:  list
    sensitivity:      dict

    # Quick-access summary table
    summary_table:    pd.DataFrame

    def print_summary(self):
        print(self.summary_table.to_string(index=False))


# ─────────────────────────────────────────────────────────────
# 5B  SUMMARY TABLE BUILDER
# ─────────────────────────────────────────────────────────────

def build_summary_table(distributions: Dict[str, SpreadDistribution],
                         baseline: dict) -> pd.DataFrame:
    """
    Build the headline results table.

    Columns:
      Spread | Baseline | E[delta] | Total EV | 50% lo | 50% hi | 90% lo | 90% hi
    """
    rows = []
    for sp, dist in distributions.items():
        rows.append({
            "Spread":          sp,
            "Baseline ($/bbl)": dist.baseline,
            "E[delta]":         round(dist.delta_ev, 2),
            "Total EV":         round(dist.total_ev, 2),
            "50% range lo":     round(dist.delta_p25, 2),
            "50% range hi":     round(dist.delta_p75, 2),
            "90% range lo":     round(dist.delta_p5,  2),
            "90% range hi":     round(dist.delta_p95, 2),
        })
    return pd.DataFrame(rows)


# ─────────────────────────────────────────────────────────────
# 5C  MASTER RUN FUNCTION
# ─────────────────────────────────────────────────────────────

def run_analysis(filepath:  Optional[str] = None,
                 n_sim:     int           = 100_000,
                 verbose:   bool          = True) -> AnalysisResults:
    """
    Single entry point for the full pipeline.

    Stage 1 → Data (live or fallback)
    Stage 2 → Scenarios
    Stage 3 → Distributions
    Stage 4 → Calibration
    Stage 5 → Pack into AnalysisResults
    """
    HEADLINE = ("Israel launches strikes on Iranian energy infrastructure. "
                "Iran threatens closure of the Strait of Hormuz.")

    if verbose:
        print("=" * 65)
        print("BRENT CALENDAR SPREAD SHOCK ANALYZER")
        print("=" * 65)
        print(f"Headline: {HEADLINE[:80]}...")
        print()

    # ── Stage 1: Data ──────────────────────────────────────────
    if verbose: print("Stage 1: Loading data...")
    spreads, baseline, context, data_source = load_brent_data(
        filepath=filepath, verbose=verbose
    )

    # ── Stage 2: Scenarios (per-spread derivation) ───────────────
    if verbose: print('\nStage 2: Building scenarios (per-spread derivation)...')
    scenarios, spread_params = build_scenarios(
        spreads_df=spreads, verbose=verbose)
    report = validate_scenarios(scenarios)
    if not report['valid']:
        raise ValueError(f"Scenario validation failed: {report['issues']}")
    if verbose:
        for s in scenarios:
            print(f"  {s.label:<35} p={s.probability:.0%}  "
                  f"M1M2: mu={s.mu('M1M2'):+.2f} sig={s.sigma('M1M2'):.2f}  "
                  f"M1M6: mu={s.mu('M1M6'):+.2f} sig={s.sigma('M1M6'):.2f}")

    # ── Stage 3: Distributions ────────────────────────────────
    if verbose: print(f"\nStage 3: Running distributions (n_sim={n_sim:,})...")
    distributions = compute_all_distributions(scenarios, baseline, n_sim=n_sim)

    # ── Stage 4: Calibration ──────────────────────────────────
    if verbose: print("\nStage 4: Loading calibration evidence...")
    historical = build_historical_evidence()
    segments   = BRENT_REGIME_SEGMENTS
    sens_m1m2  = probability_sensitivity(scenarios, "M1M2")
    sens_m1m6  = probability_sensitivity(scenarios, "M1M6")

    # ── Stage 5: Pack results ─────────────────────────────────
    summary = build_summary_table(distributions, baseline)

    results = AnalysisResults(
        scenarios       = scenarios,
        baseline        = baseline,
        context         = context,
        data_source     = data_source,
        distributions   = distributions,
        historical      = historical,
        regime_segments = segments,
        sensitivity     = {"M1M2": sens_m1m2, "M1M6": sens_m1m6},
        summary_table   = summary,
    )

    # ── Final summary ─────────────────────────────────────────
    if verbose:
        print()
        print("=" * 65)
        print("RESULTS SUMMARY")
        print("=" * 65)
        print(f"\nRegime context: {context['regime_label']}")
        print(f"M1-M2 percentile rank: {context['pct_rank_M1M2']}% "
              f"(top {100-context['pct_rank_M1M2']:.1f}% of all history)")
        print()
        print(f"{'Spread':<8} {'Baseline':>10} {'E[Δ]':>8} "
              f"{'50% lo':>8} {'50% hi':>8} {'90% lo':>8} {'90% hi':>8} "
              f"{'Total EV':>10}")
        print("─" * 75)
        for sp, dist in distributions.items():
            print(f"{sp:<8} ${dist.baseline:>9.2f}  "
                  f"{dist.delta_ev:>+7.2f}  "
                  f"{dist.delta_p25:>+7.2f}  "
                  f"{dist.delta_p75:>+7.2f}  "
                  f"{dist.delta_p5:>+7.2f}  "
                  f"{dist.delta_p95:>+7.2f}  "
                  f"${dist.total_ev:>9.2f}")

        print()
        print("SENSITIVITY (which probability assumption matters most for M1M2):")
        for name, s in results.sensitivity["M1M2"]["sensitivities"].items():
            print(f"  +5% to {name:<28} → E[Δ] {s['delta_ev']:+.3f}  "
                  f"(${s['per_1pct']:+.3f} per 1%)")

        print()
        print("KEY TAKEAWAY:")
        m1m6 = distributions["M1M6"]
        m1m2 = distributions["M1M2"]
        print(f"  M1-M6 total EV = ${m1m6.total_ev:.1f}/bbl  "
              f"(90% range: ${m1m6.total_p5:.1f}–${m1m6.total_p95:.1f})")
        print(f"  M1-M2 total EV = ${m1m2.total_ev:.1f}/bbl  "
              f"(90% range: ${m1m2.total_p5:.1f}–${m1m2.total_p95:.1f})")
        print(f"  S3 drives {results.sensitivity['M1M2']['sensitivities']['full_blockade']['per_1pct']:+.2f}/bbl "
              f"per 1% probability shift — the dominant tail risk")

    return results


# ─────────────────────────────────────────────────────────────
# SELF-TEST
# ─────────────────────────────────────────────────────────────

if __name__ == "__main__":

    # Run the full pipeline
    results = run_analysis(
        filepath = "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup/LCO_data.csv",
        n_sim    = 100_000,
        verbose  = True,
    )

    print()
    print("─" * 65)
    print("FULL SUMMARY TABLE:")
    print()
    results.print_summary()

    print()
    print("─" * 65)
    print("OBJECTS AVAILABLE FOR PLOTTING:")
    print(f"  results.scenarios         — {len(results.scenarios)} scenario objects")
    print(f"  results.distributions     — dict with keys: {list(results.distributions.keys())}")
    print(f"  results.distributions['M1M2'].samples — {len(results.distributions['M1M2'].samples):,} MC draws")
    print(f"  results.historical        — {len(results.historical)} calibration records")
    print(f"  results.regime_segments   — {len(results.regime_segments)} Brent regime periods")
    print(f"  results.sensitivity       — sensitivity to probability assumptions")
    print()
    print("Stage 5 complete. Pipeline ready for plots and deck.")