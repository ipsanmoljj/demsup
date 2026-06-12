"""
STAGE 3 — DISTRIBUTION ENGINE
==============================
Takes scenario parameters and produces the full probability distribution
for each spread.

Three outputs per spread:
  1. Expected value  (weighted mean of scenario means)
  2. 50% range       (P25 to P75 from Monte Carlo)
  3. 90% range       (P5  to P95 from Monte Carlo)

Method:
  - Mixture of 4 normals: f(x) = Σ pᵢ · N(μᵢ, σᵢ²)
  - Expected value computed analytically
  - Confidence intervals from Monte Carlo (100,000 draws)
  - Final spread = baseline + shock delta
"""

import numpy as np
import pandas as pd
from dataclasses import dataclass
from typing import Dict, List


# ─────────────────────────────────────────────────────────────
# 3A  RESULT DATACLASS
# ─────────────────────────────────────────────────────────────

@dataclass
class SpreadDistribution:
    """
    Full distribution result for one spread.

    delta_*  : the shock change (what news adds)
    total_*  : baseline + delta (actual market level post-shock)
    """
    spread:           str

    # Delta (shock) statistics
    delta_ev:         float    # expected value of the shock
    delta_variance:   float    # mixture variance of the shock
    delta_std:        float    # standard deviation of the shock
    delta_p5:         float    # 5th percentile
    delta_p25:        float    # 25th percentile
    delta_p50:        float    # median
    delta_p75:        float    # 75th percentile
    delta_p95:        float    # 95th percentile

    # Total = baseline + delta
    baseline:         float
    total_ev:         float
    total_p5:         float
    total_p25:        float
    total_p50:        float
    total_p75:        float
    total_p95:        float

    # Monte Carlo samples (for plotting)
    samples:          np.ndarray

    # Scenario contributions
    scenario_contributions: Dict[str, float]   # scenario name → contribution to EV

    def range_50(self) -> tuple:
        return (round(self.delta_p25, 2), round(self.delta_p75, 2))

    def range_90(self) -> tuple:
        return (round(self.delta_p5, 2), round(self.delta_p95, 2))

    def summary(self) -> str:
        return (f"{self.spread}: E[Δ]={self.delta_ev:+.2f}  "
                f"50%=[{self.delta_p25:+.2f},{self.delta_p75:+.2f}]  "
                f"90%=[{self.delta_p5:+.2f},{self.delta_p95:+.2f}]  "
                f"Total EV=${self.total_ev:.2f}")


# ─────────────────────────────────────────────────────────────
# 3B  ANALYTICAL EXPECTED VALUE AND VARIANCE
# ─────────────────────────────────────────────────────────────

def analytical_moments(scenarios: list, spread: str) -> tuple:
    """
    Compute the expected value and variance of the mixture analytically.

    For a mixture of normals:
      E[X]   = Σ pᵢ μᵢ
      Var[X] = Σ pᵢ (σᵢ² + μᵢ²) − E[X]²

    This is exact — no approximation. The MC simulation is used only
    for percentile estimates, not for the expected value.

    Returns: (expected_value, variance, std_dev)
    """
    probs  = np.array([s.probability for s in scenarios])
    probs  = probs / probs.sum()            # normalise to be safe

    mus    = np.array([s.mu(spread)    for s in scenarios])
    sigmas = np.array([s.sigma(spread) for s in scenarios])

    ev       = float(np.dot(probs, mus))
    variance = float(np.dot(probs, sigmas**2 + mus**2) - ev**2)
    std      = float(np.sqrt(max(variance, 0.0)))   # guard against float error

    return ev, variance, std


# ─────────────────────────────────────────────────────────────
# 3C  MONTE CARLO SIMULATION
# ─────────────────────────────────────────────────────────────

def monte_carlo(scenarios: list,
                spread:    str,
                n_sim:     int = 100_000,
                seed:      int = 42) -> np.ndarray:
    """
    Draw n_sim samples from the mixture distribution.

    Algorithm:
      1. For each draw, sample a scenario with probability pᵢ
      2. Then draw from N(μᵢ, σᵢ) for that scenario

    The resulting array of samples IS the mixture distribution.
    Percentiles of this array give us the confidence intervals.

    Seed is fixed so results are reproducible.
    """
    rng    = np.random.default_rng(seed)

    probs  = np.array([s.probability for s in scenarios])
    probs  = probs / probs.sum()

    mus    = np.array([s.mu(spread)    for s in scenarios])
    sigmas = np.array([s.sigma(spread) for s in scenarios])

    # Step 1: assign each draw to a scenario
    scenario_indices = rng.choice(len(scenarios), size=n_sim, p=probs)

    # Step 2: draw from the assigned scenario's normal distribution
    std_normals = rng.standard_normal(n_sim)
    samples     = mus[scenario_indices] + sigmas[scenario_indices] * std_normals

    return samples


# ─────────────────────────────────────────────────────────────
# 3D  SCENARIO CONTRIBUTION BREAKDOWN
# ─────────────────────────────────────────────────────────────

def scenario_contributions(scenarios: list,
                            spread:    str) -> Dict[str, float]:
    """
    How much does each scenario contribute to the expected value?

    Contribution of scenario i = pᵢ × μᵢ

    These sum to E[X]. They show which scenario is driving the expected value.
    """
    probs = np.array([s.probability for s in scenarios])
    probs = probs / probs.sum()
    mus   = np.array([s.mu(spread)  for s in scenarios])

    return {
        s.name: round(float(p * mu), 4)
        for s, p, mu in zip(scenarios, probs, mus)
    }


# ─────────────────────────────────────────────────────────────
# 3E  MASTER COMPUTE FUNCTION
# ─────────────────────────────────────────────────────────────

def compute_distribution(scenarios: list,
                          spread:    str,
                          baseline:  float,
                          n_sim:     int = 100_000,
                          seed:      int = 42) -> SpreadDistribution:
    """
    Full computation pipeline for one spread.

    1. Analytical moments (exact EV and variance)
    2. Monte Carlo for percentile estimates
    3. Add baseline to all delta statistics
    4. Package into SpreadDistribution

    baseline: the current market level before the shock
    """
    # Analytical
    ev, var, std = analytical_moments(scenarios, spread)

    # Monte Carlo
    samples = monte_carlo(scenarios, spread, n_sim=n_sim, seed=seed)

    # Percentiles of the delta
    p5, p25, p50, p75, p95 = np.percentile(samples, [5, 25, 50, 75, 95])

    # Contributions
    contribs = scenario_contributions(scenarios, spread)

    return SpreadDistribution(
        spread    = spread,

        delta_ev        = round(ev,   4),
        delta_variance  = round(var,  4),
        delta_std       = round(std,  4),
        delta_p5        = round(float(p5),  4),
        delta_p25       = round(float(p25), 4),
        delta_p50       = round(float(p50), 4),
        delta_p75       = round(float(p75), 4),
        delta_p95       = round(float(p95), 4),

        baseline  = round(baseline, 4),
        total_ev  = round(baseline + ev,   4),
        total_p5  = round(baseline + float(p5),  4),
        total_p25 = round(baseline + float(p25), 4),
        total_p50 = round(baseline + float(p50), 4),
        total_p75 = round(baseline + float(p75), 4),
        total_p95 = round(baseline + float(p95), 4),

        samples                 = samples,
        scenario_contributions  = contribs,
    )


# ─────────────────────────────────────────────────────────────
# 3F  RUN ALL THREE SPREADS
# ─────────────────────────────────────────────────────────────

def compute_all_distributions(scenarios: list,
                               baseline:  dict,
                               n_sim:     int = 100_000) -> Dict[str, SpreadDistribution]:
    """
    Compute distributions for M1M2, M2M4, M1M6 in one call.

    Returns a dict keyed by spread name.
    """
    spreads = ["M1M2", "M2M4", "M1M6"]
    results = {}

    for sp in spreads:
        results[sp] = compute_distribution(
            scenarios = scenarios,
            spread    = sp,
            baseline  = baseline[sp],
            n_sim     = n_sim,
            seed      = 42,
        )

    return results


# ─────────────────────────────────────────────────────────────
# SELF-TEST
# ─────────────────────────────────────────────────────────────

if __name__ == "__main__":

    from stage2_scenarios import build_scenarios
    from stage1_data import FALLBACK_BASELINE

    print("=" * 65)
    print("STAGE 3 — DISTRIBUTION ENGINE SELF-TEST")
    print("=" * 65)

    scenarios = build_scenarios()
    baseline  = FALLBACK_BASELINE

    results   = compute_all_distributions(scenarios, baseline, n_sim=100_000)

    print()
    print(f"{'Spread':<8} {'Baseline':>10} {'E[Δ]':>8} {'50% range (delta)':>22} "
          f"{'90% range (delta)':>22} {'Total EV':>10}")
    print("─" * 85)

    for sp, dist in results.items():
        lo50, hi50 = dist.range_50()
        lo90, hi90 = dist.range_90()
        print(f"{sp:<8} ${dist.baseline:>9.2f}  "
              f"{dist.delta_ev:>+7.2f}  "
              f"[{lo50:+.2f}, {hi50:+.2f}]{'':<8}"
              f"[{lo90:+.2f}, {hi90:+.2f}]{'':<8}"
              f"${dist.total_ev:>9.2f}")

    print()
    print("─" * 65)
    print("SCENARIO CONTRIBUTIONS TO E[Δ] (M1M2):")
    m1m2 = results["M1M2"]
    for name, contrib in m1m2.scenario_contributions.items():
        pct = contrib / m1m2.delta_ev * 100
        print(f"  {name:<30} ${contrib:>5.2f}  ({pct:.0f}% of EV)")

    print()
    print("─" * 65)
    print("ANALYTICAL vs MONTE CARLO CHECK (M1M2):")
    mc_mean = results["M1M2"].samples.mean()
    mc_std  = results["M1M2"].samples.std()
    print(f"  Analytical E[Δ]:   {results['M1M2'].delta_ev:.4f}")
    print(f"  Monte Carlo mean:  {mc_mean:.4f}  (should match within ~0.02)")
    print(f"  Analytical std:    {results['M1M2'].delta_std:.4f}")
    print(f"  Monte Carlo std:   {mc_std:.4f}  (should match within ~0.05)")

    # Tolerance check
    assert abs(mc_mean - results["M1M2"].delta_ev) < 0.05, "MC mean too far from analytical EV"
    assert abs(mc_std  - results["M1M2"].delta_std) < 0.10, "MC std too far from analytical std"

    print()
    print("─" * 65)
    print("TAIL BEHAVIOUR CHECK (M1M6):")
    m1m6 = results["M1M6"]
    s3_mu = 32.50   # S3 M1M6 mu
    extreme_pct = (m1m6.samples > s3_mu * 0.8).mean() * 100
    print(f"  P95 of M1M6 delta: {m1m6.delta_p95:.2f}")
    print(f"  % draws above {s3_mu*0.8:.0f}: {extreme_pct:.1f}%  "
          f"(driven by S3 at 15% weight)")
    print(f"  Total M1M6 EV:     ${m1m6.total_ev:.2f}/bbl  "
          f"(baseline ${m1m6.baseline:.2f} + delta ${m1m6.delta_ev:.2f})")

    print()
    print("Stage 3 complete. All checks passed.")
