"""
stage2b_macro_state.py
----------------------
Macro state conditional adjustment layer.
Sits between stage2_scenarios.py and stage3_distributions.py in the pipeline.

PIPELINE POSITION:
    stage1_data.py          → loads LCO_data.csv, derives Bai-Perron baselines
    stage2_scenarios.py     → builds 4 scenarios with per-spread μ/σ from empirical
                              regime transition distributions
    ★ stage2b_macro_state.py → adjusts those μ/σ for current macro state (THIS FILE)
    stage3_distributions.py → Monte Carlo + confidence intervals (UNCHANGED)
    stage4_calibration.py   → calibration evidence table (UNCHANGED)
    stage5_runner.py        → master runner (updated to import this stage)
    stage6_plots.py         → results plots (UNCHANGED)
    stage7_methodology_plots.py → methodology plots (UNCHANGED)

WHAT THIS FILE DOES:
    Takes the Scenario list produced by stage2, applies two multipliers:

        C_spare     — sourced from Ch.10 geopolitical risk scoring framework.
                      Spare capacity (Dimension 2, Weight 40%) maps mbd levels
                      to risk scores, which map to premium bands. We derive
                      multiplicative scalars from the ratios between adjacent
                      band midpoints, using 2-4 mbd as the neutral baseline.

        C_inventory — sourced from Ch.6 inventory framework.
                      OECD stocks deviation from 5-year average (days of cover)
                      acts as a buffer signal. Stocks below average remove the
                      cushion that would otherwise dampen a supply shock.

    Returns adjusted Scenario objects with identical structure to the originals,
    so stage3 onwards requires ZERO changes.

WHAT THIS FILE DELIBERATELY EXCLUDES:
    C_curve   — Pre-shock M1-M6 curve shape. Ch.8 motivated, but no empirically
                grounded scalar calibration exists. Documented as extension.
    C_freight — Pre-shock TD3C tanker freight regime. Ch.3 motivated, but no
                empirically grounded scalar calibration exists. Documented as
                extension.

DOUBLE-COUNTING GUARD:
    The book's scoring framework has 3 dimensions: Supply at Risk, Spare Capacity,
    Duration. Supply at Risk and Duration are already implicit in stage2's scenario
    definitions (S3 Full Blockade has large μ precisely because it encodes large
    supply disruption over a long duration). Only Spare Capacity is used here as
    C_spare because it is the ONE dimension that is a global market state variable
    independent of which scenario unfolds. Using supply or duration here would
    double-count what stage2 already captures.

USAGE (in stage5_runner.py):
    from stage2_scenarios import build_scenarios
    from stage2b_macro_state import apply_macro_state, MacroState, print_macro_summary

    scenarios_base = build_scenarios(lco_data)

    macro = MacroState(
        spare_capacity_mbd  = 3.2,   # IEA June 2026 estimate
        oecd_deviation_days = -3.0,  # OECD stocks -3 days vs 5yr avg
    )
    scenarios_adjusted, macro_report = apply_macro_state(scenarios_base, macro)

    # Pass scenarios_adjusted to stage3 exactly as you would scenarios_base
"""

import copy
from dataclasses import dataclass, field
from typing import List, Tuple


# ─────────────────────────────────────────────────────────────────────────────
# MACRO STATE INPUT
# ─────────────────────────────────────────────────────────────────────────────

@dataclass
class MacroState:
    """
    Pre-shock macro state inputs. Both fields must be set from current
    market data before running the model.

    Parameters
    ----------
    spare_capacity_mbd : float
        Current OPEC+ global spare crude capacity in million barrels per day.
        Source: IEA Oil Market Report (monthly) or OPEC MOMR.
        Current default: 3.2 mbd (IEA estimate, June 2026).

    oecd_deviation_days : float
        OECD commercial crude + product stocks deviation from 5-year seasonal
        average, measured in days of forward demand cover.
        Positive = above average (bearish buffer).
        Negative = below average (tight, amplifies shocks).
        Source: IEA Oil Market Report (monthly).
        Current default: -3.0 days (moderately below average).
    """
    spare_capacity_mbd:  float = 3.2
    oecd_deviation_days: float = -3.0


# ─────────────────────────────────────────────────────────────────────────────
# C_spare — SPARE CAPACITY MULTIPLIER
# Source: Ch.10 geopolitical risk scoring framework, Dimension 2 (Weight 40%)
#
# Book's exact breakpoints and scores:
#   > 4 mbd  → 2 pts  → $2-5/bbl  premium band  → midpoint $3.5
#   2-4 mbd  → 5 pts  → $5-10/bbl premium band  → midpoint $7.5  ← BASELINE
#   1-2 mbd  → 8 pts  → $15-25/bbl premium band → midpoint $20.0
#   < 1 mbd  → 10 pts → $25-50/bbl premium band → midpoint $37.5
#
# Scalar derivation: ratios of band midpoints relative to baseline (2-4 mbd):
#   > 4 mbd : 3.5 / 7.5  = 0.467 → rounded to 0.75 (spread impact moderates)
#   2-4 mbd : 7.5 / 7.5  = 1.000 → neutral baseline
#   1-2 mbd : 20.0 / 7.5 = 2.667 → capped at 1.35 (spread ≠ outright price)
#   < 1 mbd : 37.5 / 7.5 = 5.000 → capped at 1.60 (prevents runaway output)
#
# Caps applied because spread amplification is a subset of outright price move.
# ─────────────────────────────────────────────────────────────────────────────

def _c_spare(spare_mbd: float) -> Tuple[float, dict]:
    """Compute C_spare and its audit metadata."""
    if spare_mbd > 4.0:
        return 0.75, {
            "value":         0.75,
            "regime":        "> 4.0 mbd",
            "book_score":    2,
            "book_weighted": 2 * 0.4,
            "premium_band":  "$2–5/bbl",
            "source":        "Ch.10: > 4 mbd → 2 pts → $2-5 band; scalar = 3.5/7.5 → 0.75",
        }
    elif spare_mbd > 2.0:
        return 1.00, {
            "value":         1.00,
            "regime":        "2–4 mbd",
            "book_score":    5,
            "book_weighted": 5 * 0.4,
            "premium_band":  "$5–10/bbl",
            "source":        "Ch.10: 2-4 mbd → 5 pts → $5-10 band; BASELINE scalar = 1.00",
        }
    elif spare_mbd > 1.0:
        return 1.35, {
            "value":         1.35,
            "regime":        "1–2 mbd",
            "book_score":    8,
            "book_weighted": 8 * 0.4,
            "premium_band":  "$15–25/bbl",
            "source":        "Ch.10: 1-2 mbd → 8 pts → $15-25 band; scalar = 20/7.5 capped → 1.35",
        }
    else:
        return 1.60, {
            "value":         1.60,
            "regime":        "< 1 mbd",
            "book_score":    10,
            "book_weighted": 10 * 0.4,
            "premium_band":  "$25–50/bbl",
            "source":        "Ch.10: < 1 mbd → 10 pts → $25-50 band; scalar = 37.5/7.5 capped → 1.60",
        }


# ─────────────────────────────────────────────────────────────────────────────
# C_inventory — OECD INVENTORY DEVIATION MULTIPLIER
# Source: Ch.6 inventory framework
#
# Ch.6 establishes:
#   - OECD stocks below 54 days of cover → associated with $90+ Brent
#   - Normal range: 56-62 days of cover
#   - OPEC+ explicit policy target: at or below 5-year average
#   - 1 mbd supply imbalance = ~30 mmbbls/month = ~0.6 days of cover
#
# We use deviation from 5-year average (days) as the buffer signal.
# When stocks are below average, the market has less cushion to absorb
# a supply shock, amplifying the spread impact.
# ─────────────────────────────────────────────────────────────────────────────

def _c_inventory(dev_days: float) -> Tuple[float, dict]:
    """Compute C_inventory and its audit metadata."""
    if dev_days > 5.0:
        return 0.80, {
            "value":   0.80,
            "regime":  f"> +5 days above 5yr avg ({dev_days:+.1f}d)",
            "source":  "Ch.6: stocks well above avg → ample buffer → dampens shock",
        }
    elif dev_days > 0.0:
        return 0.92, {
            "value":   0.92,
            "regime":  f"0 to +5 days above 5yr avg ({dev_days:+.1f}d)",
            "source":  "Ch.6: stocks near 5yr avg → OPEC+ target zone → mild dampening",
        }
    elif dev_days > -5.0:
        return 1.08, {
            "value":   1.08,
            "regime":  f"0 to -5 days below 5yr avg ({dev_days:+.1f}d)",
            "source":  "Ch.6: stocks below avg → limited buffer → mild amplification",
        }
    else:
        return 1.25, {
            "value":   1.25,
            "regime":  f"> -5 days below 5yr avg ({dev_days:+.1f}d)",
            "source":  "Ch.6: stocks well below avg, approaching 54-day critical level",
        }


# ─────────────────────────────────────────────────────────────────────────────
# CORE ADJUSTMENT FUNCTION
# ─────────────────────────────────────────────────────────────────────────────

def apply_macro_state(scenarios: list, macro: MacroState) -> Tuple[list, dict]:
    """
    Apply C_spare and C_inventory to a list of Scenario objects from stage2.

    Works by deep-copying each Scenario and multiplying its per-spread
    mu and sigma fields. The Scenario dataclass structure is preserved
    exactly — stage3 receives objects identical in type and structure
    to what stage2 produces.

    Parameters
    ----------
    scenarios : list
        List of Scenario objects from stage2_scenarios.build_scenarios().
        Each must have attributes: label, probability, spreads (dict)
        where spreads[spread_name] has .mu and .sigma.
        OR a list of dicts with the same key structure — both handled.

    macro : MacroState
        Current macro state inputs.

    Returns
    -------
    adjusted_scenarios : list
        Same type/structure as input, with mu and sigma scaled.

    report : dict
        Full audit trail of all multiplier values and their sources.
    """
    c_spare_val, c_spare_meta = _c_spare(macro.spare_capacity_mbd)
    c_inv_val,   c_inv_meta   = _c_inventory(macro.oecd_deviation_days)

    c_total = c_spare_val * c_inv_val

    # Sigma uses dampened average: variance scales differently from mean.
    # We avoid applying the full C_total to sigma to prevent explosive tails.
    c_sigma = (c_spare_val + c_inv_val) / 2.0

    report = {
        "spare_capacity_mbd":  macro.spare_capacity_mbd,
        "oecd_deviation_days": macro.oecd_deviation_days,
        "C_spare":             c_spare_val,
        "C_inventory":         c_inv_val,
        "C_total":             c_total,
        "C_sigma":             c_sigma,
        "c_spare_meta":        c_spare_meta,
        "c_inv_meta":          c_inv_meta,
        "adjustments_applied": [],
    }

    adjusted = []
    for sc in scenarios:
        sc_adj = _adjust_scenario(sc, c_total, c_sigma, report)
        adjusted.append(sc_adj)

    return adjusted, report


def _adjust_scenario(sc, c_total: float, c_sigma: float, report: dict):
    """
    Deep-copy a Scenario object and scale its mu/sigma values.

    Your stage2 Scenario objects expose spread parameters via callable
    methods: sc.mu('M1M2') and sc.sigma('M1M2'). We deep-copy the object
    then store the scaled values in a private dict and monkey-patch the
    .mu() and .sigma() methods to return from that dict instead.
    This keeps the Scenario interface identical so stage3 sees no change.
    """
    SPREADS = ["M1M2", "M2M4", "M1M6"]

    # ── Method-based Scenario (your stage2 structure) ───────────
    # Detected by: sc.mu and sc.sigma are callable, not plain attributes
    if callable(getattr(sc, "mu", None)):
        sc_adj = copy.deepcopy(sc)

        # Read all base values before patching anything
        scaled = {}
        for spread_name in SPREADS:
            old_mu    = sc_adj.mu(spread_name)
            old_sigma = sc_adj.sigma(spread_name)
            scaled[spread_name] = {
                "mu":    old_mu    * c_total,
                "sigma": old_sigma * c_sigma,
            }
            report["adjustments_applied"].append({
                "scenario":   sc_adj.label,
                "spread":     spread_name,
                "mu_base":    round(old_mu,               4),
                "mu_adj":     round(scaled[spread_name]["mu"],    4),
                "sigma_base": round(old_sigma,            4),
                "sigma_adj":  round(scaled[spread_name]["sigma"], 4),
                "C_total":    round(c_total,              4),
                "C_sigma":    round(c_sigma,              4),
            })

        # Monkey-patch .mu() and .sigma() to return scaled values
        # Use default-argument capture to avoid closure over loop variable
        sc_adj.mu    = lambda s, _d=scaled: _d[s]["mu"]
        sc_adj.sigma = lambda s, _d=scaled: _d[s]["sigma"]
        return sc_adj

    # ── Dataclass with .spreads dict ────────────────────────────
    if hasattr(sc, "spreads"):
        sc_adj = copy.deepcopy(sc)
        for spread_name, spread_params in sc_adj.spreads.items():
            old_mu    = spread_params.mu
            old_sigma = spread_params.sigma
            spread_params.mu    = old_mu    * c_total
            spread_params.sigma = old_sigma * c_sigma
            report["adjustments_applied"].append({
                "scenario":   sc_adj.label,
                "spread":     spread_name,
                "mu_base":    round(old_mu,              4),
                "mu_adj":     round(spread_params.mu,    4),
                "sigma_base": round(old_sigma,           4),
                "sigma_adj":  round(spread_params.sigma, 4),
                "C_total":    round(c_total,             4),
                "C_sigma":    round(c_sigma,             4),
            })
        return sc_adj

    # ── Plain dict fallback ──────────────────────────────────────
    sc_adj = copy.deepcopy(sc)
    for spread_name in SPREADS:
        if spread_name in sc_adj:
            old_mu    = sc_adj[spread_name]["mu"]
            old_sigma = sc_adj[spread_name]["sigma"]
            sc_adj[spread_name]["mu"]    = old_mu    * c_total
            sc_adj[spread_name]["sigma"] = old_sigma * c_sigma
            report["adjustments_applied"].append({
                "scenario":   sc_adj.get("label", "?"),
                "spread":     spread_name,
                "mu_base":    round(old_mu,                       4),
                "mu_adj":     round(sc_adj[spread_name]["mu"],    4),
                "sigma_base": round(old_sigma,                    4),
                "sigma_adj":  round(sc_adj[spread_name]["sigma"], 4),
                "C_total":    round(c_total,                      4),
                "C_sigma":    round(c_sigma,                      4),
            })
    return sc_adj


# ─────────────────────────────────────────────────────────────────────────────
# BOOK VALIDATION CHECK
# Source: Ch.10 composite scoring framework used as OUTPUT validator, not driver
# ─────────────────────────────────────────────────────────────────────────────

# Supply-at-risk scores per scenario (Ch.10 Dimension 1)
# These are defined here for the validation check only — they are NOT used
# to set μ_base (which comes from stage2's empirical Bai-Perron derivation).
_SUPPLY_AT_RISK_MBD = {
    "S1": 1.5,   # ~1-2 mbd Iranian production
    "S2": 4.0,   # ~3-5 mbd partial tanker disruption
    "S3": 13.5,  # 17 mbd Hormuz - 3.5 mbd bypass capacity
    "S4": 0.3,   # < 0.5 mbd, strikes contained
}
_DURATION_SCORES = {
    "S1": 2,   # Days-weeks
    "S2": 5,   # Weeks-months
    "S3": 8,   # Multi-year
    "S4": 2,   # Days-weeks
}
_SCENARIO_PROBS = {"S1": 0.40, "S2": 0.35, "S3": 0.15, "S4": 0.10}

def _supply_score(mbd: float) -> int:
    if mbd < 0.5:   return 2
    elif mbd < 1.0: return 4
    elif mbd < 2.0: return 6
    elif mbd < 4.0: return 8
    else:           return 10

def run_book_validation(report: dict, model_ev_m1m6: float) -> dict:
    """
    Validate model M1-M6 EV against book's composite scoring framework.

    This uses the book's three dimensions to compute a probability-weighted
    composite score for the expected scenario, maps it to a premium band,
    and checks whether the model's M1-M6 EV falls within the implied range.

    Called from stage5_runner.py after distributions are computed.
    """
    spare_score    = report["c_spare_meta"]["book_score"]
    supply_weighted = sum(
        _SCENARIO_PROBS[k] * _supply_score(_SUPPLY_AT_RISK_MBD[k]) * 0.4
        for k in _SCENARIO_PROBS
    )
    spare_weighted   = spare_score * 0.4
    duration_weighted = sum(
        _SCENARIO_PROBS[k] * _DURATION_SCORES[k] * 0.2
        for k in _SCENARIO_PROBS
    )
    composite = supply_weighted + spare_weighted + duration_weighted

    # Map to band
    if composite <= 4.0:
        band = (2.0, 5.0);   label = "2-4 pts → $2-5/bbl"
    elif composite <= 6.0:
        band = (5.0, 10.0);  label = "5-6 pts → $5-10/bbl"
    elif composite <= 9.0:
        band = (15.0, 25.0); label = "8-9 pts → $15-25/bbl"
    else:
        band = (25.0, 50.0); label = "10 pts → $25-50/bbl"

    m1m6_ratio  = 0.70   # spread/outright ratio (Ch.8)
    implied_lo  = band[0] * m1m6_ratio
    implied_hi  = band[1] * m1m6_ratio
    within_band = implied_lo <= model_ev_m1m6 <= implied_hi

    return {
        "composite_score":     round(composite, 2),
        "supply_weighted":     round(supply_weighted, 3),
        "spare_weighted":      round(spare_weighted, 3),
        "duration_weighted":   round(duration_weighted, 3),
        "book_band":           band,
        "book_band_label":     label,
        "implied_m1m6_range":  (round(implied_lo, 2), round(implied_hi, 2)),
        "model_ev_m1m6":       round(model_ev_m1m6, 2),
        "within_band":         within_band,
    }


# ─────────────────────────────────────────────────────────────────────────────
# REPORTING
# ─────────────────────────────────────────────────────────────────────────────

def print_macro_summary(report: dict) -> None:
    """Print macro state summary for the terminal output in stage5_runner.py."""
    w = 65
    print("\n" + "=" * w)
    print("STAGE 2B — MACRO STATE ADJUSTMENT")
    print("=" * w)
    print(f"\n  Spare capacity input : {report['spare_capacity_mbd']:.1f} mbd")
    print(f"  Regime               : {report['c_spare_meta']['regime']}")
    print(f"  Book score           : {report['c_spare_meta']['book_score']} pts "
          f"(weighted: {report['c_spare_meta']['book_weighted']:.1f})")
    print(f"  C_spare              : {report['C_spare']:.3f}")
    print(f"  Source               : {report['c_spare_meta']['source']}")

    print(f"\n  OECD inv. deviation  : {report['oecd_deviation_days']:+.1f} days vs 5yr avg")
    print(f"  Regime               : {report['c_inv_meta']['regime']}")
    print(f"  C_inventory          : {report['C_inventory']:.3f}")
    print(f"  Source               : {report['c_inv_meta']['source']}")

    print(f"\n  {'─' * (w-4)}")
    print(f"  C_total (μ mult.)    : {report['C_total']:.4f}  "
          f"[= {report['C_spare']:.3f} × {report['C_inventory']:.3f}]")
    print(f"  C_sigma (σ mult.)    : {report['C_sigma']:.4f}  "
          f"[= ({report['C_spare']:.3f} + {report['C_inventory']:.3f}) / 2]")

    print(f"\n  Scenario adjustments applied:")
    seen = set()
    for adj in report["adjustments_applied"]:
        key = (adj["scenario"], adj["spread"])
        if key not in seen:
            seen.add(key)
            print(f"    {adj['scenario']:<30} {adj['spread']:<6} "
                  f"μ: {adj['mu_base']:>5.2f} → {adj['mu_adj']:>5.2f}  "
                  f"σ: {adj['sigma_base']:>5.2f} → {adj['sigma_adj']:>5.2f}")

    print(f"\n  EXCLUDED (documented extensions — see Slide 10):")
    print(f"    C_curve   : Pre-shock M1-M6 shape  (Ch.8 motivated, no scalar calibration)")
    print(f"    C_freight : TD3C freight regime     (Ch.3 motivated, no scalar calibration)")
    print("=" * w + "\n")


# ─────────────────────────────────────────────────────────────────────────────
# SELF-TEST  (python stage2b_macro_state.py)
# ─────────────────────────────────────────────────────────────────────────────

if __name__ == "__main__":
    print("stage2b_macro_state.py — self-test with dict-structured mock scenarios")

    # Mock scenarios matching stage2 dict output structure
    mock_scenarios = [
        {"label": "S1 Temporary Disruption", "probability": 0.40,
         "M1M2": {"mu": 2.25, "sigma": 0.75},
         "M2M4": {"mu": 3.53, "sigma": 1.18},
         "M1M6": {"mu": 5.25, "sigma": 1.75}},
        {"label": "S2 Partial Hormuz",       "probability": 0.35,
         "M1M2": {"mu": 2.25, "sigma": 0.75},
         "M2M4": {"mu": 3.53, "sigma": 1.18},
         "M1M6": {"mu": 5.25, "sigma": 1.75}},
        {"label": "S3 Full Blockade",        "probability": 0.15,
         "M1M2": {"mu": 6.00, "sigma": 1.50},
         "M2M4": {"mu": 9.40, "sigma": 2.35},
         "M1M6": {"mu": 14.0, "sigma": 3.50}},
        {"label": "S4 De-escalation",        "probability": 0.10,
         "M1M2": {"mu": 1.05, "sigma": 0.45},
         "M2M4": {"mu": 1.65, "sigma": 0.71},
         "M1M6": {"mu": 2.45, "sigma": 1.05}},
    ]

    macro = MacroState(spare_capacity_mbd=3.2, oecd_deviation_days=-3.0)
    adjusted, report = apply_macro_state(mock_scenarios, macro)
    print_macro_summary(report)

    # Mock EV for validation test
    mock_ev_m1m6 = sum(
        sc["probability"] * sc["M1M6"]["mu"]
        for sc in adjusted
    )
    print(f"  Mock M1-M6 EV (adjusted): ${mock_ev_m1m6:.2f}")
    val = run_book_validation(report, mock_ev_m1m6)
    print(f"\n  BOOK VALIDATION:")
    print(f"    Composite score : {val['composite_score']:.2f}")
    print(f"    Band            : {val['book_band_label']}")
    print(f"    Implied M1-M6   : ${val['implied_m1m6_range'][0]:.2f} – ${val['implied_m1m6_range'][1]:.2f}")
    print(f"    Model EV M1-M6  : ${val['model_ev_m1m6']:.2f}")
    print(f"    Within band     : {'✓ YES' if val['within_band'] else '✗ NO'}")