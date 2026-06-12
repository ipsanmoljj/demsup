"""
STAGE 7 — METHODOLOGY PLOTS
=============================
Six charts showing exactly how parameters are derived.

Plot M1 — Regime timeline:       9 regime means as a time series with transitions marked
Plot M2 — Transition deltas:     The 8 observed deltas, split positive/negative
Plot M3 — Delta derivation:      How percentiles of positive transitions map to S1-S3
Plot M4 — Sigma derivation:      HAC CI widths → proportional sigma → S3 cap
Plot M5 — Curve ratios:          The 3 M1M6/M1M2 observations used to derive scaling
Plot M6 — Full parameter map:    One diagram showing every input → every output
"""

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.gridspec import GridSpec
from matplotlib.patches import FancyArrowPatch
from pathlib import Path

from stage2_scenarios import (
    FALLBACK_SEGMENT_MEANS, FACTORS, SCENARIO_PROBABILITIES,
    extract_transitions, compute_mus,
)

# Convenience aliases so rest of file is unchanged
SEGMENT_MEANS_M1M2 = FALLBACK_SEGMENT_MEANS["M1M2"]

def compute_m1m2_mus(transitions, factors):
    return compute_mus(transitions, factors)

def compute_spread_ratios(curve_ratios, factors):
    """Kept for compatibility — returns ratio dict from fallback data."""
    m1m2_segs = FALLBACK_SEGMENT_MEANS["M1M2"]
    m1m6_segs = FALLBACK_SEGMENT_MEANS["M1M6"]
    obs = [(m1m2_segs[s], m1m6_segs[s]) for s in [9, 8, 3]]
    import numpy as np
    level_ratio = float(np.mean([m6/m2 for m2, m6 in obs]))
    return {
        "m1m6_prompt":           round(level_ratio, 4),
        "m1m6_hormuz":           round(level_ratio * factors["m1m6_hormuz_premium"] if "m1m6_hormuz_premium" in factors else level_ratio * 1.3, 4),
        "m2m4":                  1.92,
        "m1m6_level_ratio_mean": round(level_ratio, 4),
        "m1m6_observations":     [round(m6/m2, 3) for m2, m6 in obs],
    }

CURVE_RATIOS = {
    "m1m6_to_m1m2_observations": [
        (FALLBACK_SEGMENT_MEANS["M1M2"][9], FALLBACK_SEGMENT_MEANS["M1M6"][9]),
        (3.35, 15.43),
        (FALLBACK_SEGMENT_MEANS["M1M2"][3], FALLBACK_SEGMENT_MEANS["M1M6"][3]),
    ],
    "m2m4_to_m1m2": 1.92,
}

C = {
    "bg":     "#0D1117", "panel": "#161B22", "border": "#30363D",
    "text":   "#E6EDF3", "muted": "#8B949E", "ev":     "#FFFFFF",
    "s1":     "#378ADD", "s2":    "#EF9F27", "s3":     "#E24B4A",
    "s4":     "#1D9E75", "seg9":  "#E24B4A", "pos":    "#1D9E75",
    "neg":    "#E24B4A", "ci":    "#7F77DD",
}
SC_COLORS = [C["s1"], C["s2"], C["s3"], C["s4"]]

def apply_style():
    plt.rcParams.update({
        "figure.facecolor": C["bg"],  "axes.facecolor":  C["panel"],
        "axes.edgecolor":   C["border"], "axes.labelcolor": C["text"],
        "axes.titlecolor":  C["text"],   "text.color":      C["text"],
        "xtick.color":      C["muted"],  "ytick.color":     C["muted"],
        "grid.color":       C["border"], "grid.linewidth":  0.4,
        "font.family":      "sans-serif","font.size":       10,
        "axes.titlesize":   11,          "axes.labelsize":  9,
        "xtick.labelsize":  8,           "ytick.labelsize": 8,
        "legend.fontsize":  8,           "legend.facecolor":C["panel"],
        "legend.edgecolor": C["border"], "figure.dpi":      150,
        "savefig.facecolor":C["bg"],     "savefig.bbox":    "tight",
        "savefig.pad_inches": 0.15,
    })

def ax_clean(ax, grid_axis="y"):
    for sp in ax.spines.values():
        sp.set_edgecolor(C["border"]); sp.set_linewidth(0.6)
    ax.tick_params(colors=C["muted"], length=3)
    if grid_axis: ax.grid(True, axis=grid_axis, alpha=0.25)


# ─────────────────────────────────────────────────────────────
# PLOT M1 — REGIME TIMELINE AS LINE CHART
# ─────────────────────────────────────────────────────────────

def plot_regime_timeline(output_path):
    """
    M1M2 mean per regime shown as a step-line through time.
    Each transition delta annotated. Positive = green, negative = red.
    """
    apply_style()
    fig, ax = plt.subplots(figsize=(13, 5))

    segs = SEGMENT_MEANS_M1M2
    seg_labels = {
        1:"Pre-Ukraine", 2:"Ukraine uncert.", 3:"Peak Ukraine",
        4:"Demand destr.", 5:"OPEC+ equil.", 6:"Saudi cuts",
        7:"Gaza fade", 8:"Long stable", 9:"Regime 9 (now)"
    }
    # Approximate midpoints on a 0-100 scale for x-axis
    x_pos = [5, 16, 24, 31, 42, 53, 60, 72, 92]
    means  = [segs[i] for i in range(1,10)]

    # Step line
    ax.step(x_pos, means, where="mid", color=C["muted"], linewidth=1.0,
            alpha=0.4, zorder=2)

    # Dots at each segment
    for i, (x, m, seg) in enumerate(zip(x_pos, means, range(1,10))):
        col = C["seg9"] if seg == 9 else C["s1"]
        ax.scatter(x, m, color=col, s=70, zorder=5, alpha=0.95)
        ax.text(x, m + 0.12, f"${m:.3f}", ha="center", fontsize=7.5,
                color=col)
        # Regime label below
        ax.text(x, -0.35, seg_labels[seg], ha="center", fontsize=6.5,
                color=C["muted"], rotation=30)

    # Transition arrows and delta labels
    transitions = extract_transitions(SEGMENT_MEANS_M1M2)
    all_t = []
    seg_list = sorted(segs.keys())
    for i in range(1, len(seg_list)):
        delta = segs[seg_list[i]] - segs[seg_list[i-1]]
        all_t.append((x_pos[i-1], x_pos[i],
                      segs[seg_list[i-1]], segs[seg_list[i]], delta))

    for x0, x1, y0, y1, delta in all_t:
        col   = C["pos"] if delta > 0 else C["neg"]
        xmid  = (x0 + x1) / 2
        ymid  = (y0 + y1) / 2
        ax.annotate("", xy=(x1, y1), xytext=(x0, y0),
                    arrowprops=dict(arrowstyle="->", color=col,
                                    lw=1.0, alpha=0.7))
        sign = "+" if delta > 0 else ""
        ax.text(xmid, ymid + 0.15, f"{sign}{delta:.3f}",
                ha="center", fontsize=7, color=col, fontweight="bold")

    # Highlight which transitions become inputs
    ax.axhline(segs[2], color=C["s1"], linestyle=":", linewidth=0.8, alpha=0.6)
    ax.axhline(segs[3], color=C["s2"], linestyle=":", linewidth=0.8, alpha=0.6)
    ax.text(98, segs[2] + 0.05, "S1 anchor", color=C["s1"], fontsize=7.5, ha="right")
    ax.text(98, segs[3] + 0.05, "S2 anchor", color=C["s2"], fontsize=7.5, ha="right")

    ax.set_xlim(0, 100)
    ax.set_ylim(-0.6, 5.2)
    ax.set_xticks([])
    ax.set_ylabel("Brent M1−M2 mean ($/bbl)", fontsize=9)
    ax.set_title("Regime timeline: 9 Bai-Perron segments with transition deltas  "
                 "|  green = positive  red = negative", fontsize=10, pad=10)

    legend = [
        mpatches.Patch(color=C["pos"], alpha=0.8, label="Positive transition → input to S1/S2/S3 δ derivation"),
        mpatches.Patch(color=C["neg"], alpha=0.8, label="Negative transition → input to S4 δ derivation"),
        mpatches.Patch(color=C["seg9"], alpha=0.8, label="Regime 9 — current baseline"),
    ]
    ax.legend(handles=legend, loc="upper left", fontsize=8)
    ax_clean(ax, grid_axis="y")
    fig.tight_layout()
    fig.savefig(output_path)
    plt.close(fig)
    print(f"  Saved: {output_path}")


# ─────────────────────────────────────────────────────────────
# PLOT M2 — TRANSITION DELTA DISTRIBUTION
# ─────────────────────────────────────────────────────────────

def plot_transition_deltas(output_path):
    """
    Bar chart of all 8 transitions.
    Shows which percentiles of the positive distribution map to S1, S2, S3.
    """
    apply_style()
    fig, (ax_bar, ax_pct) = plt.subplots(1, 2, figsize=(13, 5),
                                          gridspec_kw={"width_ratios":[1,1]})

    segs = SEGMENT_MEANS_M1M2
    seg_list = sorted(segs.keys())
    labels, deltas, colors = [], [], []
    for i in range(1, len(seg_list)):
        d = segs[seg_list[i]] - segs[seg_list[i-1]]
        labels.append(f"Seg{seg_list[i-1]}→{seg_list[i]}")
        deltas.append(d)
        colors.append(C["pos"] if d > 0 else C["neg"])

    # Bar chart of all transitions
    bars = ax_bar.bar(range(len(deltas)), deltas, color=colors, alpha=0.85,
                       width=0.6)
    for bar, val, lbl in zip(bars, deltas, labels):
        yoff = 0.05 if val > 0 else -0.15
        ax_bar.text(bar.get_x() + bar.get_width()/2, val + yoff,
                    f"{val:+.3f}", ha="center", fontsize=8,
                    color=C["text"])
    ax_bar.set_xticks(range(len(labels)))
    ax_bar.set_xticklabels(labels, rotation=35, ha="right", fontsize=7.5)
    ax_bar.axhline(0, color=C["border"], linewidth=0.8)
    ax_bar.set_ylabel("Transition delta ($/bbl)", fontsize=9)
    ax_bar.set_title("All 8 observed regime transition deltas", fontsize=10)
    ax_clean(ax_bar)

    # Positive transitions with percentile markers
    pos = sorted([d for d in deltas if d > 0])
    mus = compute_m1m2_mus(extract_transitions(segs), FACTORS)

    ax_pct.barh(range(len(pos)), pos, color=C["pos"], alpha=0.7, height=0.5)
    for i, v in enumerate(pos):
        ax_pct.text(v + 0.03, i, f"{v:.3f}", va="center", fontsize=8.5,
                    color=C["pos"])

    # Percentile lines
    p40 = float(np.percentile(pos, 40))
    p60 = float(np.percentile(pos, 60))

    ax_pct.axvline(p40, color=C["s1"], linewidth=1.5, linestyle="--")
    ax_pct.axvline(p60, color=C["s2"], linewidth=1.5, linestyle="--")
    ax_pct.axvline(max(pos), color=C["s3"], linewidth=1.5, linestyle="--")

    # Place annotation boxes above the axis (y > n_bars) to avoid bar overlap
    n = len(pos)
    ax_pct.set_ylim(-0.5, n + 2.2)
    ax_pct.annotate(
        f"P40={p40:.3f}  x1.2  ->  S1 d={mus['temporary_disruption']:.3f}",
        xy=(p40, n - 0.5), xytext=(p40 * 0.5, n + 0.5),
        ha='left', va='bottom', fontsize=7.5, color=C['s1'],
        bbox=dict(facecolor=C['panel'], edgecolor=C['s1'],
                  boxstyle='round,pad=0.25', linewidth=0.7),
        arrowprops=dict(arrowstyle='->', color=C['s1'], lw=0.8))
    ax_pct.annotate(
        f"P60={p60:.3f}  x1.8  ->  S2 d={mus['partial_hormuz']:.3f}",
        xy=(p60, n - 0.5), xytext=(p60 * 0.5, n + 1.2),
        ha='left', va='bottom', fontsize=7.5, color=C['s2'],
        bbox=dict(facecolor=C['panel'], edgecolor=C['s2'],
                  boxstyle='round,pad=0.25', linewidth=0.7),
        arrowprops=dict(arrowstyle='->', color=C['s2'], lw=0.8))
    ax_pct.annotate(
        f"max={max(pos):.3f}  x2.79  ->  S3 d={mus['full_blockade']:.3f}",
        xy=(max(pos), n - 0.5), xytext=(max(pos) * 0.45, n + 1.9),
        ha='left', va='bottom', fontsize=7.5, color=C['s3'],
        bbox=dict(facecolor=C['panel'], edgecolor=C['s3'],
                  boxstyle='round,pad=0.25', linewidth=0.7),
        arrowprops=dict(arrowstyle='->', color=C['s3'], lw=0.8))

    ax_pct.set_yticks(range(len(pos)))
    ax_pct.set_yticklabels([f"Rank {i+1}" for i in range(len(pos))], fontsize=8)
    ax_pct.set_xlabel("Positive transition magnitude ($/bbl)", fontsize=9)
    ax_pct.set_title("Positive transitions: percentile → scenario delta derivation",
                     fontsize=10)
    ax_clean(ax_pct, grid_axis="x")

    fig.suptitle("Transition delta distribution — the empirical input to all δ values",
                 fontsize=11, y=1.01)
    fig.tight_layout()
    fig.savefig(output_path)
    plt.close(fig)
    print(f"  Saved: {output_path}")


# ─────────────────────────────────────────────────────────────
# PLOT M3 — SIGMA DERIVATION STEP BY STEP
# ─────────────────────────────────────────────────────────────

def plot_sigma_derivation(output_path):
    """
    Shows: HAC CIs for Seg2 and Seg3 → relative width → sigma for each scenario.
    Also shows the S3 cap.
    """
    apply_style()
    fig, axes = plt.subplots(1, 3, figsize=(14, 5))

    # ── Panel 1: HAC confidence intervals ──
    ax = axes[0]
    seg_data = [
        {"seg": 2, "mean": 1.680, "lo": 1.34, "hi": 2.08,
         "label": "Seg 2\n(Ukraine uncert.)", "color": C["s1"]},
        {"seg": 3, "mean": 3.110, "lo": 2.37, "hi": 3.85,
         "label": "Seg 3\n(Peak Ukraine)",    "color": C["s2"]},
    ]
    for i, sd in enumerate(seg_data):
        ax.barh(i, sd["hi"] - sd["lo"], left=sd["lo"],
                color=sd["color"], alpha=0.35, height=0.5)
        ax.plot(sd["mean"], i, "|", color=sd["color"],
                markersize=18, markeredgewidth=2.5)
        ax.text(sd["lo"] - 0.02, i, f"{sd['lo']:.2f}",
                va="center", ha="right", fontsize=8, color=sd["color"])
        ax.text(sd["hi"] + 0.02, i, f"{sd['hi']:.2f}",
                va="center", ha="left", fontsize=8, color=sd["color"])
        ax.text(sd["mean"], i + 0.32,
                f"mean={sd['mean']:.3f}\nwidth={sd['hi']-sd['lo']:.3f}\nrel={( sd['hi']-sd['lo'])/sd['mean']:.3f}",
                ha="center", va="bottom", fontsize=7, color=sd["color"])

    ax.set_yticks([0, 1])
    ax.set_yticklabels([d["label"] for d in seg_data], fontsize=9)
    ax.set_xlabel("M1−M2 ($/bbl)", fontsize=9)
    ax.set_title("HAC 95% CI\nfrom Bai-Perron output", fontsize=9)
    ax_clean(ax, grid_axis="x")

    # Relative widths
    rel_widths = [(sd["hi"]-sd["lo"])/sd["mean"] for sd in seg_data]
    avg_rel = float(np.mean(rel_widths))

    # ── Panel 2: Relative width → sigma formula ──
    ax2 = axes[1]
    ax2.axis("off")

    formula_lines = [
        ("INPUT", f"Seg2 rel width = {rel_widths[0]:.3f}", C["s1"]),
        ("INPUT", f"Seg3 rel width = {rel_widths[1]:.3f}", C["s2"]),
        ("STEP",  f"Average rel width = {avg_rel:.3f}", C["muted"]),
        ("STEP",  "CI→sigma factor = 0.408", C["muted"]),
        ("      ", "(converts 95% CI half-width", C["muted"]),
        ("      ", " to ±1.6σ coverage)", C["muted"]),
        ("FORMULA","σ = |δ| × 0.458 × 0.408", C["ci"]),
        ("      ", "for S1 and S2 only", C["ci"]),
        ("S3 CAP","proportional blows up", C["s3"]),
        ("      ", "→ cap at |δ| × 0.25", C["s3"]),
    ]

    y = 0.95
    for tag, line, col in formula_lines:
        if tag in ("INPUT","STEP","FORMULA","S3 CAP"):
            ax2.text(0.05, y, tag, fontsize=7, color=C["muted"],
                     fontweight="bold", transform=ax2.transAxes)
        ax2.text(0.30, y, line, fontsize=8.5, color=col,
                 transform=ax2.transAxes)
        y -= 0.09

    ax2.set_title("Sigma formula derivation", fontsize=9)

    # ── Panel 3: Final sigma values ──
    ax3 = axes[2]
    mus_dict = compute_m1m2_mus(extract_transitions(SEGMENT_MEANS_M1M2), FACTORS)
    scenarios_data = [
        ("S1", mus_dict["temporary_disruption"],
         abs(mus_dict["temporary_disruption"]) * avg_rel * 0.408, C["s1"]),
        ("S2", mus_dict["partial_hormuz"],
         abs(mus_dict["partial_hormuz"]) * avg_rel * 0.408, C["s2"]),
        ("S3", mus_dict["full_blockade"],
         mus_dict["full_blockade"] * 0.25, C["s3"]),
        ("S4", mus_dict["deescalation"],
         float(np.std([1.750, 1.181, 0.603])) * 0.5, C["s4"]),
    ]

    y_pos = range(len(scenarios_data))
    for i, (name, mu, sigma, col) in enumerate(scenarios_data):
        # Show mu as range centre, sigma as half-width
        ax3.barh(i, 2 * sigma, left=mu - sigma,
                 color=col, alpha=0.5, height=0.55)
        ax3.plot(mu, i, "|", color=col, markersize=14, markeredgewidth=2.5)
        ax3.text(mu + sigma + 0.1, i,
                 f"δ={mu:+.3f}  σ={sigma:.3f}",
                 va="center", fontsize=8, color=col)

    ax3.set_yticks(list(y_pos))
    ax3.set_yticklabels(["S1","S2","S3","S4"], fontsize=10)
    ax3.axvline(0, color=C["muted"], linewidth=0.7, linestyle="--", alpha=0.5)
    ax3.set_xlabel("M1−M2 delta ($/bbl)", fontsize=9)
    ax3.set_title("Final δ ± σ per scenario\n(bar = ±1σ range)", fontsize=9)
    ax3.invert_yaxis()
    ax_clean(ax3, grid_axis="x")

    fig.suptitle("Sigma derivation: HAC CI widths → proportional uncertainty → scenario σ",
                 fontsize=11, y=1.02)
    fig.tight_layout()
    fig.savefig(output_path)
    plt.close(fig)
    print(f"  Saved: {output_path}")


# ─────────────────────────────────────────────────────────────
# PLOT M4 — CURVE RATIO DERIVATION
# ─────────────────────────────────────────────────────────────

def plot_curve_ratios(output_path):
    """
    Shows the 3 observed M1M6/M1M2 data points and how they produce
    the scaling ratios used for M1M6 and M2M4.
    """
    apply_style()
    fig, (ax_obs, ax_ratio) = plt.subplots(1, 2, figsize=(12, 5))

    obs = CURVE_RATIOS["m1m6_to_m1m2_observations"]
    labels_obs = [
        "Regime 9 mean\n(demsup get_curve_metrics)",
        "May 22 2026 spot\n(demsup get_spreads output)",
        "Peak Ukraine Seg3\n(demsup Seg3 metrics)",
    ]
    colors_obs = [C["seg9"], C["s1"], C["s2"]]
    ratios_obs = [m6/m2 for m2, m6 in obs]
    mean_ratio = float(np.mean(ratios_obs))

    # ── Scatter: M1M2 vs M1M6 ──
    m2_vals = [x[0] for x in obs]
    m6_vals = [x[1] for x in obs]
    # Staggered offsets to prevent label collision — 3 points cluster tightly
    offsets = [
        (0.15,  1.2),   # Regime 9 — annotate upper right
        (-1.8, -2.5),   # May 22 spot — annotate left
        (-1.8, -2.5),   # Peak Ukraine — annotate lower left
    ]
    for i, (m2, m6, lbl, col, (dx, dy)) in enumerate(
            zip(m2_vals, m6_vals, labels_obs, colors_obs, offsets)):
        ax_obs.scatter(m2, m6, color=col, s=120, zorder=5)
        ax_obs.annotate(
            f"{lbl}\n({m2:.2f}, {m6:.2f})  ratio={m6/m2:.3f}",
            xy=(m2, m6), xytext=(m2 + dx, m6 + dy),
            fontsize=7, color=col,
            bbox=dict(facecolor=C['panel'], edgecolor=col,
                      boxstyle='round,pad=0.2', linewidth=0.5, alpha=0.9),
            arrowprops=dict(arrowstyle='->', color=col, lw=0.7))

    # Regression line through origin (forced)
    x_line = np.linspace(0, 5.5, 100)
    ax_obs.plot(x_line, mean_ratio * x_line,
                color=C["muted"], linewidth=1.0, linestyle="--", alpha=0.7,
                label=f"Mean ratio = {mean_ratio:.3f}")
    ax_obs.set_xlabel("M1−M2 level ($/bbl)", fontsize=9)
    ax_obs.set_ylabel("M1−M6 level ($/bbl)", fontsize=9)
    ax_obs.set_title("3 observed M1M6/M1M2 data points\n(all sourced from demsup outputs)",
                     fontsize=9)
    ax_obs.legend(fontsize=8)
    ax_clean(ax_obs)

    # ── Bar: how ratios are used ──
    ratio_data = [
        ("M1M6\nS1, S4\n(prompt)", mean_ratio, C["s1"],
         f"level ratio\n{mean_ratio:.3f}"),
        ("M1M6\nS2, S3\n(Hormuz)", mean_ratio * 1.3, C["s3"],
         f"level × 1.3\n= {mean_ratio*1.3:.3f}"),
        ("M2M4\nall scenarios", CURVE_RATIOS["m2m4_to_m1m2"], C["s2"],
         f"curve interp.\n{CURVE_RATIOS['m2m4_to_m1m2']:.3f}"),
    ]

    for i, (lbl, ratio, col, note) in enumerate(ratio_data):
        bar = ax_ratio.bar(i, ratio, color=col, alpha=0.8, width=0.55)
        ax_ratio.text(i, ratio + 0.05, f"×{ratio:.3f}",
                      ha="center", fontsize=9, fontweight="bold", color=col)
        ax_ratio.text(i, ratio / 2, note,
                      ha="center", va="center", fontsize=7.5,
                      color=C["bg"])

    ax_ratio.set_xticks(range(len(ratio_data)))
    ax_ratio.set_xticklabels([d[0] for d in ratio_data], fontsize=9)
    ax_ratio.set_ylabel("Multiplier applied to M1M2 δ and σ", fontsize=9)
    ax_ratio.set_title("How M1M2 delta scales to M1M6 and M2M4\n"
                       "Hormuz scenarios get ×1.3 duration premium on M1M6",
                       fontsize=9)
    ax_clean(ax_ratio)

    fig.suptitle("Spread ratio derivation: 3 data points → scaling factors for M1M6 and M2M4",
                 fontsize=11, y=1.02)
    fig.tight_layout()
    fig.savefig(output_path)
    plt.close(fig)
    print(f"  Saved: {output_path}")


# ─────────────────────────────────────────────────────────────
# PLOT M5 — S3 SCALING LOGIC
# ─────────────────────────────────────────────────────────────

def plot_s3_scaling(output_path):
    """
    Shows how S3 delta is derived: max_transition × Hormuz_scale.
    Visualises the scaling chain and the sigma cap.
    """
    apply_style()
    fig, (ax_chain, ax_cap) = plt.subplots(1, 2, figsize=(13, 5))

    # ── Scaling chain ──
    ax_chain.axis("off")
    steps = [
        ("START", "Max observed\ntransition", "+3.970", C["muted"]),
        ("×", "Hormuz disruption\n13.5 mbd / 3.5 mbd implied", "= ×3.857", C["s3"]),
        ("÷", "Spare capacity\ndampener (1.8)", "÷1.8 = ×2.143", C["s2"]),
        ("×", "Week-1 immediacy\nfactor (1.3)", "×1.3 = ×2.786", C["s1"]),
        ("CAP", "Max scale capped\nat 3.0 (conservative)", "min(2.786,3.0)", C["muted"]),
        ("=", "Final S3 delta\n3.970 × 2.786", "= +11.059", C["s3"]),
    ]

    y = 0.92
    for tag, desc, result, col in steps:
        # Tag box
        ax_chain.text(0.05, y, tag, fontsize=10, color=C["bg"],
                      fontweight="bold", transform=ax_chain.transAxes,
                      bbox=dict(facecolor=col, boxstyle="round,pad=0.3"))
        ax_chain.text(0.20, y, desc, fontsize=8.5, color=C["text"],
                      transform=ax_chain.transAxes, va="center")
        ax_chain.text(0.72, y, result, fontsize=9, color=col,
                      fontweight="bold", transform=ax_chain.transAxes,
                      va="center")
        y -= 0.14

    ax_chain.set_title("S3 delta derivation chain\nmax_transition × Hormuz scale",
                       fontsize=10)

    # ── Sigma cap comparison ──
    categories = ["S3 proportional\nformula (broken)",
                  "S3 with\n25% cap (used)"]
    s3_mu = 11.059
    proportional_sigma = 1.344 * (s3_mu / float(np.mean([0.321,0.692,1.099,1.430,3.970])))
    capped_sigma       = s3_mu * 0.25
    values = [proportional_sigma, capped_sigma]
    colors_bar = [C["neg"], C["s3"]]

    bars = ax_cap.bar(range(2), values, color=colors_bar, alpha=0.85, width=0.5)
    for bar, val, lbl in zip(bars, values, ["9.89 (86% of δ)\n→ P95 would be +30+", 
                                              "2.77 (25% of δ)\n→ P95 = +15.5"]):
        ax_cap.text(bar.get_x() + bar.get_width()/2,
                    val + 0.15, f"σ = {val:.2f}", ha="center",
                    fontsize=9, fontweight="bold",
                    color=colors_bar[0] if val == values[0] else colors_bar[1])
        ax_cap.text(bar.get_x() + bar.get_width()/2,
                    val / 2, lbl, ha="center", va="center",
                    fontsize=7.5, color=C["bg"])

    ax_cap.set_xticks([0, 1])
    ax_cap.set_xticklabels(categories, fontsize=9)
    ax_cap.set_ylabel("σ value ($/bbl)", fontsize=9)
    ax_cap.set_title("S3 sigma: proportional formula vs 25% cap\n"
                     "Cap justified: kurtosis=39, zero historical analogs",
                     fontsize=9)
    ax_clean(ax_cap)

    fig.suptitle("S3 (full blockade) parameter derivation",
                 fontsize=11, y=1.02)
    fig.tight_layout()
    fig.savefig(output_path)
    plt.close(fig)
    print(f"  Saved: {output_path}")


# ─────────────────────────────────────────────────────────────
# PLOT M6 — FULL PARAMETER MAP
# ─────────────────────────────────────────────────────────────

def plot_parameter_map(output_path):
    """
    Single summary diagram: data source → computation → output parameter.
    """
    apply_style()
    fig, ax = plt.subplots(figsize=(14, 7))
    ax.set_xlim(0, 14); ax.set_ylim(0, 7)
    ax.axis("off")

    def box(ax, x, y, w, h, text, col, fontsize=8):
        rect = mpatches.FancyBboxPatch((x, y), w, h,
                                        boxstyle="round,pad=0.1",
                                        facecolor=col, edgecolor=C["border"],
                                        linewidth=0.8, alpha=0.85)
        ax.add_patch(rect)
        ax.text(x + w/2, y + h/2, text, ha="center", va="center",
                fontsize=fontsize, color=C["bg"] if col != C["panel"] else C["text"],
                fontweight="bold", wrap=True)

    def arrow(ax, x0, y0, x1, y1, col=C["muted"]):
        ax.annotate("", xy=(x1, y1), xytext=(x0, y0),
                    arrowprops=dict(arrowstyle="->", color=col,
                                    lw=1.0, connectionstyle="arc3,rad=0.0"))

    # Column headers
    for x, lbl, col in [(0.3, "DATA SOURCE\n(demsup output)", C["ci"]),
                          (4.5, "COMPUTATION\n(Stage 2)", C["s2"]),
                          (9.2, "PARAMETER\nOUTPUT", C["s3"])]:
        ax.text(x + 1.2, 6.6, lbl, ha="center", fontsize=9,
                color=col, fontweight="bold")

    # DATA SOURCES
    box(ax, 0.2, 5.2, 2.8, 0.7, "9 regime segment means\n(Bai-Perron on LCO_data.csv)",
        C["ci"], 7.5)
    box(ax, 0.2, 4.0, 2.8, 0.7,
        "HAC 95% CI: Seg2=[1.34,2.08]\nSeg3=[2.37,3.85]",
        C["ci"], 7.5)
    box(ax, 0.2, 2.8, 2.8, 0.7,
        "3 M1M6/M1M2 observations\n3.70, 4.61, 3.70",
        C["ci"], 7.5)
    box(ax, 0.2, 1.6, 2.8, 0.7,
        "Diagnostics: kurtosis=39\nvariance ratio=37,723",
        C["ci"], 7.5)
    box(ax, 0.2, 0.4, 2.8, 0.7,
        "Regime 9 current level\nM1M2=$3.85  M1M6=$17.26",
        C["ci"], 7.5)

    # COMPUTATIONS
    box(ax, 4.0, 5.2, 3.0, 0.7,
        "8 transition deltas → 5 positive\nPct40×1.2, Pct60×1.8, Max×2.79",
        C["s2"], 7.5)
    box(ax, 4.0, 4.0, 3.0, 0.7,
        "avg_rel_width=0.458\nσ = |δ| × 0.458 × 0.408",
        C["s2"], 7.5)
    box(ax, 4.0, 2.8, 3.0, 0.7,
        "Mean ratio=4.003\n×1.3 Hormuz premium for S2,S3",
        C["s2"], 7.5)
    box(ax, 4.0, 1.6, 3.0, 0.7,
        "S3 cap = |δ|×0.25\n(proportional formula unusable)",
        C["s2"], 7.5)
    box(ax, 4.0, 0.4, 3.0, 0.7,
        "Added to all delta outputs\nto get total spread levels",
        C["s2"], 7.5)

    # OUTPUTS
    box(ax, 8.7, 5.2, 2.0, 0.7,
        f"S1 δ=+1.12  S2 δ=+2.22\nS3 δ=+11.06  S4 δ=−0.37",
        C["s3"], 7.5)
    box(ax, 8.7, 4.0, 2.0, 0.7,
        "S1 σ=0.21  S2 σ=0.41\nS3 σ=2.77  S4 σ=0.23",
        C["s3"], 7.5)
    box(ax, 8.7, 2.8, 2.0, 0.7,
        "M1M6: ×4.0 (prompt)\n       ×5.2 (Hormuz)\nM2M4: ×1.92 (all)",
        C["s3"], 7.5)
    box(ax, 8.7, 1.6, 2.0, 0.7,
        "S3 σ capped\nat 2.77 not 9.89",
        C["s3"], 7.5)
    box(ax, 8.7, 0.4, 2.0, 0.7,
        "M1M2 EV=$7.41\nM1M6 EV=$31.20",
        C["s3"], 7.5)

    # Arrows
    for y in [5.55, 4.35, 3.15, 1.95, 0.75]:
        arrow(ax, 3.0, y, 4.0, y, C["muted"])
        arrow(ax, 7.0, y, 8.7, y, C["muted"])

    ax.set_title("Full parameter map: every data source → every output parameter",
                 fontsize=11, pad=10, color=C["text"])

    fig.savefig(output_path)
    plt.close(fig)
    print(f"  Saved: {output_path}")


# ─────────────────────────────────────────────────────────────
# MASTER RUNNER
# ─────────────────────────────────────────────────────────────

def run_methodology_plots(output_dir="plots/methodology"):
    out = Path(output_dir)
    out.mkdir(parents=True, exist_ok=True)

    print("Generating methodology plots...")
    plot_regime_timeline(str(out / "M1_regime_timeline.png"))
    plot_transition_deltas(str(out / "M2_transition_deltas.png"))
    plot_sigma_derivation(str(out / "M3_sigma_derivation.png"))
    plot_curve_ratios(str(out / "M4_curve_ratios.png"))
    plot_s3_scaling(str(out / "M5_s3_scaling.png"))
    plot_parameter_map(str(out / "M6_parameter_map.png"))
    print(f"\nAll methodology plots saved to: {output_dir}/")


if __name__ == "__main__":
    run_methodology_plots()