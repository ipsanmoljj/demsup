"""
STAGE 6 — PLOTS
================
Eight charts, each answering one specific question.

Plot 1 — Regime history:        Where are we now in historical context?
Plot 2 — Scenario tree:         What are the four paths and their weights?
Plot 3 — M1M2 distribution:     What does the M1M2 shock distribution look like?
Plot 4 — M2M4 distribution:     Same for M2M4.
Plot 5 — M1M6 distribution:     Same for M1M6 (primary Hormuz barometer).
Plot 6 — Scenario contributions: Which scenario drives expected value most?
Plot 7 — Confidence bands:      50% vs 90% bands across all three spreads.
Plot 8 — Sensitivity:           How does EV shift if probabilities are wrong?
"""

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.gridspec import GridSpec
from pathlib import Path

# ── colour palette ────────────────────────────────────────────
C = {
    "bg":      "#0D1117",   # dark background
    "panel":   "#161B22",   # panel background
    "border":  "#30363D",   # border / axis
    "text":    "#E6EDF3",   # primary text
    "muted":   "#8B949E",   # secondary text
    "s1":      "#378ADD",   # S1 blue
    "s2":      "#EF9F27",   # S2 amber
    "s3":      "#E24B4A",   # S3 red
    "s4":      "#1D9E75",   # S4 green
    "ev":      "#FFFFFF",   # expected value line
    "band50":  "#378ADD",   # 50% CI fill
    "band90":  "#378ADD",   # 90% CI fill
    "regime9": "#E24B4A",   # current regime highlight
}
SC_COLORS = [C["s1"], C["s2"], C["s3"], C["s4"]]

SPREAD_LABELS = {"M1M2": "M1−M2", "M2M4": "M2−M4", "M1M6": "M1−M6"}
SPREAD_ORDER  = ["M1M2", "M2M4", "M1M6"]


# ─────────────────────────────────────────────────────────────
# BASE STYLE
# ─────────────────────────────────────────────────────────────

def apply_style():
    plt.rcParams.update({
        "figure.facecolor":     C["bg"],
        "axes.facecolor":       C["panel"],
        "axes.edgecolor":       C["border"],
        "axes.labelcolor":      C["text"],
        "axes.titlecolor":      C["text"],
        "text.color":           C["text"],
        "xtick.color":          C["muted"],
        "ytick.color":          C["muted"],
        "grid.color":           C["border"],
        "grid.linewidth":       0.4,
        "grid.alpha":           0.6,
        "font.family":          "sans-serif",
        "font.size":            10,
        "axes.titlesize":       11,
        "axes.labelsize":       9,
        "xtick.labelsize":      8,
        "ytick.labelsize":      8,
        "legend.fontsize":      8,
        "legend.facecolor":     C["panel"],
        "legend.edgecolor":     C["border"],
        "figure.dpi":           150,
        "savefig.facecolor":    C["bg"],
        "savefig.bbox":         "tight",
        "savefig.pad_inches":   0.15,
    })


def ax_spine_style(ax):
    for spine in ax.spines.values():
        spine.set_edgecolor(C["border"])
        spine.set_linewidth(0.6)
    ax.tick_params(colors=C["muted"], length=3)
    ax.grid(True, axis="y", alpha=0.3)


# ─────────────────────────────────────────────────────────────
# PLOT 1 — REGIME HISTORY
# ─────────────────────────────────────────────────────────────

def plot_regime_history(regime_segments: list,
                         baseline: dict,
                         output_path: str) -> None:
    """
    Bar chart of Brent M1M2 segment means across all 9 regimes.
    Highlights Regime 9 as the current extreme baseline.
    Annotates the historical anchors used for S1/S2/S3 calibration.
    """
    apply_style()
    fig, ax = plt.subplots(figsize=(11, 5))

    segs    = regime_segments
    labels  = [f"Seg {s['seg']}\n{s['start'][:7]}" for s in segs]
    means   = [s["mean_M1M2"] for s in segs]
    colors  = [C["regime9"] if s["seg"] == 9 else C["s1"] for s in segs]

    bars = ax.bar(range(len(segs)), means, color=colors,
                  alpha=0.85, width=0.65, zorder=3)

    # CI error bars for Seg 2 and 3
    for seg in segs:
        if seg.get("ci_lo") and seg.get("ci_hi"):
            i = seg["seg"] - 1
            ax.errorbar(i, seg["mean_M1M2"],
                        yerr=[[seg["mean_M1M2"] - seg["ci_lo"]],
                               [seg["ci_hi"] - seg["mean_M1M2"]]],
                        fmt="none", color=C["ev"], capsize=4,
                        linewidth=1.2, zorder=5)

    # Horizontal reference lines for scenario anchors
    ax.axhline(y=segs[1]["mean_M1M2"], color=C["s1"],
               linestyle="--", linewidth=0.8, alpha=0.7, zorder=2)
    ax.axhline(y=segs[2]["mean_M1M2"], color=C["s2"],
               linestyle="--", linewidth=0.8, alpha=0.7, zorder=2)

    # Current baseline marker
    ax.axhline(y=baseline["M1M2"], color=C["regime9"],
               linestyle="-", linewidth=1.2, alpha=0.9, zorder=2)

    # Annotations
    ax.annotate(f"S1 anchor\n${segs[1]['mean_M1M2']:.2f}",
                xy=(1, segs[1]["mean_M1M2"]), xytext=(1.5, segs[1]["mean_M1M2"] + 0.35),
                color=C["s1"], fontsize=7.5,
                arrowprops=dict(arrowstyle="-", color=C["s1"], lw=0.7))
    ax.annotate(f"S2 anchor\n${segs[2]['mean_M1M2']:.2f}",
                xy=(2, segs[2]["mean_M1M2"]), xytext=(2.5, segs[2]["mean_M1M2"] + 0.35),
                color=C["s2"], fontsize=7.5,
                arrowprops=dict(arrowstyle="-", color=C["s2"], lw=0.7))
    ax.annotate(f"Current baseline\n${baseline['M1M2']:.2f}",
                xy=(8, baseline["M1M2"]), xytext=(7.0, baseline["M1M2"] + 0.4),
                color=C["regime9"], fontsize=7.5,
                arrowprops=dict(arrowstyle="-", color=C["regime9"], lw=0.7))

    # Regime labels inside bars
    for i, (seg, mean) in enumerate(zip(segs, means)):
        ax.text(i, mean / 2 if mean > 0.3 else mean + 0.05,
                seg["label"].split("—")[0].strip()[:14],
                ha="center", va="center", fontsize=6.5,
                color=C["bg"] if seg["seg"] == 9 else C["muted"],
                rotation=90 if len(seg["label"]) > 12 else 0)

    ax.set_xticks(range(len(segs)))
    ax.set_xticklabels(labels, fontsize=7)
    ax.set_ylabel("Brent M1−M2 mean ($/bbl)", fontsize=9)
    ax.set_title("Brent M1−M2: all 9 regime segment means  |  calibration anchors for scenario parameters",
                 fontsize=10, pad=10)
    ax_spine_style(ax)

    legend_elements = [
        mpatches.Patch(color=C["s1"],      alpha=0.85, label="Historical regimes"),
        mpatches.Patch(color=C["regime9"], alpha=0.85, label="Regime 9 — current baseline ($4.56)"),
        plt.Line2D([0],[0], color=C["s1"],  linestyle="--", linewidth=0.8, label=f"S1 anchor (${segs[1]['mean_M1M2']:.2f})"),
        plt.Line2D([0],[0], color=C["s2"],  linestyle="--", linewidth=0.8, label=f"S2 anchor (${segs[2]['mean_M1M2']:.2f})"),
    ]
    ax.legend(handles=legend_elements, loc="upper left", framealpha=0.8)

    fig.tight_layout()
    fig.savefig(output_path)
    plt.close(fig)
    print(f"  Saved: {output_path}")


# ─────────────────────────────────────────────────────────────
# PLOT 2 — SCENARIO TREE
# ─────────────────────────────────────────────────────────────

def plot_scenario_tree(scenarios: list, baseline: dict,
                        output_path: str) -> None:
    """
    Horizontal bar chart showing scenario probabilities and M1M2 delta ranges.
    """
    apply_style()
    fig, (ax_prob, ax_range) = plt.subplots(1, 2, figsize=(12, 4.5),
                                              gridspec_kw={"width_ratios": [1, 2]})

    labels = [s.label.replace(": ", ":\n") for s in scenarios]
    probs  = [s.probability * 100 for s in scenarios]
    mus    = [s.mu("M1M2") for s in scenarios]
    los    = [s.range_lo["M1M2"] for s in scenarios]
    his    = [s.range_hi["M1M2"] for s in scenarios]

    y_pos = range(len(scenarios))

    # ── Probability bars ──
    for i, (y, prob, sc) in enumerate(zip(y_pos, probs, scenarios)):
        ax_prob.barh(y, prob, color=SC_COLORS[i], alpha=0.85, height=0.55)
        ax_prob.text(prob + 0.5, y, f"{prob:.0f}%",
                     va="center", ha="left", fontsize=9,
                     color=SC_COLORS[i], fontweight="bold")

    ax_prob.set_yticks(list(y_pos))
    ax_prob.set_yticklabels(labels, fontsize=8.5)
    ax_prob.set_xlabel("Probability (%)", fontsize=9)
    ax_prob.set_title("Scenario probability", fontsize=10)
    ax_prob.set_xlim(0, 55)
    ax_prob.invert_yaxis()
    ax_spine_style(ax_prob)

    # ── Range bars with mu markers ──
    for i, (y, mu, lo, hi, sc) in enumerate(zip(y_pos, mus, los, his, scenarios)):
        # Range bar
        ax_range.barh(y, hi - max(lo, -2), left=max(lo, -2),
                      color=SC_COLORS[i], alpha=0.25, height=0.55)
        # IQR-style inner bar (±sigma from mu)
        sig = sc.sigma("M1M2")
        ax_range.barh(y, 2 * sig, left=mu - sig,
                      color=SC_COLORS[i], alpha=0.55, height=0.55)
        # Mu marker
        ax_range.plot(mu, y, "|", color=SC_COLORS[i], markersize=14,
                      markeredgewidth=2.5)
        ax_range.text(hi + 0.15, y, f"μ={mu:+.2f}", va="center",
                      ha="left", fontsize=8, color=SC_COLORS[i])

    # Baseline reference line
    ax_range.axvline(0, color=C["muted"], linewidth=0.8, linestyle="--", alpha=0.6)
    ax_range.set_yticks(list(y_pos))
    ax_range.set_yticklabels([""] * len(scenarios))
    ax_range.set_xlabel("M1−M2 shock delta ($/bbl)", fontsize=9)
    ax_range.set_title(f"M1−M2 shock delta ranges  |  baseline = ${baseline['M1M2']:.2f}/bbl",
                        fontsize=10)
    ax_range.invert_yaxis()
    ax_spine_style(ax_range)

    # Legend
    legend_elements = [mpatches.Patch(color=SC_COLORS[i], alpha=0.85, label=s.label)
                        for i, s in enumerate(scenarios)]
    ax_range.legend(handles=legend_elements, loc="lower right", fontsize=8)

    fig.suptitle("Scenario tree: four physically distinct resolution paths",
                  fontsize=11, y=1.01)
    fig.tight_layout()
    fig.savefig(output_path)
    plt.close(fig)
    print(f"  Saved: {output_path}")


# ─────────────────────────────────────────────────────────────
# HELPER: distribution PDF for a mixture
# ─────────────────────────────────────────────────────────────

def _mixture_pdf(x_vals, scenarios, spread):
    from scipy.stats import norm
    probs = np.array([s.probability for s in scenarios])
    probs = probs / probs.sum()
    pdf   = np.zeros_like(x_vals, dtype=float)
    for s, p in zip(scenarios, probs):
        pdf += p * norm.pdf(x_vals, s.mu(spread), s.sigma(spread))
    return pdf


# ─────────────────────────────────────────────────────────────
# PLOT 3/4/5 — DISTRIBUTION FOR ONE SPREAD
# ─────────────────────────────────────────────────────────────

def plot_distribution(dist, scenarios: list, spread: str,
                       baseline: float, output_path: str) -> None:
    """
    Full distribution plot for one spread.
    Shows: mixture PDF, individual scenario PDFs, CI bands, EV marker, contribution bars.
    """
    from scipy.stats import norm

    apply_style()
    fig = plt.figure(figsize=(12, 6))
    gs  = GridSpec(1, 3, figure=fig, width_ratios=[3, 0.05, 1], wspace=0.04)
    ax_pdf  = fig.add_subplot(gs[0])
    ax_bar  = fig.add_subplot(gs[2])

    probs  = np.array([s.probability for s in scenarios])
    probs  = probs / probs.sum()
    mus    = [s.mu(spread)    for s in scenarios]
    sigmas = [s.sigma(spread) for s in scenarios]

    # X range: cover all scenario distributions
    x_lo  = min(mus) - 3 * max(sigmas)
    x_hi  = max(mus) + 3 * max(sigmas)
    x_hi  = min(x_hi, max(mus) * 1.4 + 2)   # cap for readability
    x_vals = np.linspace(x_lo, x_hi, 600)
    mix    = _mixture_pdf(x_vals, scenarios, spread)

    # CI bands as background spans — visible even where PDF is near-zero.
    # fill_between(pdf) fails for this mixture because pdf≈0 between
    # the S1/S2 cluster and S3 peak, making the 90% band invisible and
    # giving the false impression that EV sits outside the CI.
    ax_pdf.axvspan(dist.delta_p5,  dist.delta_p95,
                   color=C["band90"], alpha=0.12, zorder=1, label="90% CI")
    ax_pdf.axvspan(dist.delta_p25, dist.delta_p75,
                   color=C["band50"], alpha=0.28, zorder=2, label="50% CI")

    # Component scenario PDFs (thin, coloured)
    for i, (s, p, mu, sigma) in enumerate(zip(scenarios, probs, mus, sigmas)):
        comp = p * norm.pdf(x_vals, mu, sigma)
        ax_pdf.plot(x_vals, comp, color=SC_COLORS[i],
                    linewidth=1.0, alpha=0.55, linestyle="--",
                    label=f"{s.label.split(':')[0]} (p={p:.0%})")

    # Mixture PDF
    ax_pdf.plot(x_vals, mix, color=C["ev"], linewidth=2.0, zorder=5,
                label="Mixture PDF")

    # EV and percentile verticals
    ax_pdf.axvline(dist.delta_ev,  color=C["ev"],    linewidth=1.5,
                   linestyle="-",  alpha=0.9, zorder=6)
    ax_pdf.axvline(dist.delta_p25, color=C["band50"], linewidth=0.8,
                   linestyle=":",  alpha=0.7, zorder=4)
    ax_pdf.axvline(dist.delta_p75, color=C["band50"], linewidth=0.8,
                   linestyle=":",  alpha=0.7, zorder=4)
    ax_pdf.axvline(0, color=C["muted"], linewidth=0.7,
                   linestyle="--", alpha=0.5, zorder=3)

    # Annotations
    y_top = mix.max()
    ax_pdf.annotate(f"E[Δ] = {dist.delta_ev:+.2f}",
                    xy=(dist.delta_ev, y_top * 0.92),
                    xytext=(dist.delta_ev + (x_hi - x_lo) * 0.04, y_top * 0.92),
                    color=C["ev"], fontsize=8,
                    arrowprops=dict(arrowstyle="-", color=C["ev"], lw=0.6))

    ax_pdf.set_xlabel(f"{SPREAD_LABELS[spread]} shock delta  ($/bbl)", fontsize=9)
    ax_pdf.set_ylabel("Probability density", fontsize=9)
    ax_pdf.set_title(
        f"{SPREAD_LABELS[spread]}  |  "
        f"E[Δ] = {dist.delta_ev:+.2f}   "
        f"50%: [{dist.delta_p25:+.2f}, {dist.delta_p75:+.2f}]   "
        f"90%: [{dist.delta_p5:+.2f}, {dist.delta_p95:+.2f}]   "
        f"Total EV = ${dist.total_ev:.2f}",
        fontsize=9.5, pad=8)
    ax_pdf.legend(loc="upper right", fontsize=7.5, framealpha=0.8)
    ax_spine_style(ax_pdf)

    # ── Contribution bar chart ──
    contribs = [dist.scenario_contributions[s.name] for s in scenarios]
    y_pos    = range(len(scenarios))

    for i, (y, contrib) in enumerate(zip(y_pos, contribs)):
        ax_bar.barh(y, contrib, color=SC_COLORS[i], alpha=0.85, height=0.6)
        ax_bar.text(contrib + 0.02, y, f"${contrib:.2f}",
                    va="center", ha="left", fontsize=8, color=SC_COLORS[i])

    ax_bar.set_yticks(list(y_pos))
    ax_bar.set_yticklabels([s.label.split(":")[0] for s in scenarios], fontsize=8)
    ax_bar.set_xlabel("Contribution to E[Δ]", fontsize=8)
    ax_bar.set_title("Scenario\ncontributions", fontsize=8.5)
    ax_bar.invert_yaxis()
    ax_spine_style(ax_bar)

    # Total baseline label
    fig.text(0.02, 0.02,
             f"Baseline: ${baseline:.2f}/bbl  |  "
             f"Total EV post-shock: ${dist.total_ev:.2f}/bbl  |  "
             f"σ mixture = ${dist.delta_std:.2f}",
             fontsize=7.5, color=C["muted"])

    fig.savefig(output_path)
    plt.close(fig)
    print(f"  Saved: {output_path}")


# ─────────────────────────────────────────────────────────────
# PLOT 6 — SCENARIO CONTRIBUTION COMPARISON
# ─────────────────────────────────────────────────────────────

def plot_contributions(distributions: dict, scenarios: list,
                        output_path: str) -> None:
    """
    Grouped bar chart: for each spread, show how much each scenario
    contributes to the expected value.
    """
    apply_style()
    fig, ax = plt.subplots(figsize=(10, 5))

    spreads   = SPREAD_ORDER
    n_spreads = len(spreads)
    n_sc      = len(scenarios)
    bar_w     = 0.18
    x_base    = np.arange(n_spreads)

    for i, (sc, color) in enumerate(zip(scenarios, SC_COLORS)):
        offsets = x_base + (i - n_sc / 2 + 0.5) * bar_w
        contribs = [distributions[sp].scenario_contributions[sc.name]
                    for sp in spreads]
        bars = ax.bar(offsets, contribs, width=bar_w * 0.9,
                      color=color, alpha=0.85, label=sc.label)
        for bar, val in zip(bars, contribs):
            if abs(val) > 0.05:
                ax.text(bar.get_x() + bar.get_width() / 2,
                        bar.get_height() + 0.03,
                        f"{val:.2f}", ha="center", va="bottom",
                        fontsize=7, color=color)

    # EV total markers
    for j, sp in enumerate(spreads):
        ev = distributions[sp].delta_ev
        ax.plot(j, ev, "D", color=C["ev"], markersize=8, zorder=6)
        ax.text(j + 0.01, ev + 0.12, f"E[Δ]={ev:.2f}",
                ha="center", va="bottom", fontsize=8, color=C["ev"])

    ax.axhline(0, color=C["border"], linewidth=0.8)
    ax.set_xticks(x_base)
    ax.set_xticklabels([SPREAD_LABELS[sp] for sp in spreads], fontsize=10)
    ax.set_ylabel("Contribution to E[Δ]  ($/bbl)", fontsize=9)
    ax.set_title("Scenario contributions to expected spread shock  |  "
                 "diamond = total E[Δ]", fontsize=10, pad=8)
    ax.legend(fontsize=8, framealpha=0.8, loc="upper left")
    ax_spine_style(ax)

    fig.tight_layout()
    fig.savefig(output_path)
    plt.close(fig)
    print(f"  Saved: {output_path}")


# ─────────────────────────────────────────────────────────────
# PLOT 7 — CONFIDENCE BANDS COMPARISON
# ─────────────────────────────────────────────────────────────

def plot_confidence_bands(distributions: dict, baseline: dict,
                           output_path: str) -> None:
    """
    Horizontal confidence interval chart: three spreads on one axis.
    Shows 50% (darker) and 90% (lighter) CI bands side by side.
    Both DELTA and TOTAL levels shown.
    """
    apply_style()
    fig, (ax_delta, ax_total) = plt.subplots(1, 2, figsize=(13, 5))

    spreads     = SPREAD_ORDER
    y_pos       = np.arange(len(spreads))
    spread_cols = [C["s1"], C["s2"], C["s4"]]

    for ax, mode in [(ax_delta, "delta"), (ax_total, "total")]:
        for i, (sp, color) in enumerate(zip(spreads, spread_cols)):
            d = distributions[sp]

            if mode == "delta":
                p5, p25, ev, p75, p95 = (d.delta_p5, d.delta_p25,
                                          d.delta_ev, d.delta_p75, d.delta_p95)
            else:
                p5, p25, ev = d.total_p5, d.total_p25, d.total_ev
                p75, p95    = d.total_p75, d.total_p95

            y = y_pos[i]
            # 90% band
            ax.barh(y, p95 - p5, left=p5,
                    color=color, alpha=0.18, height=0.55)
            # 50% band
            ax.barh(y, p75 - p25, left=p25,
                    color=color, alpha=0.55, height=0.55)
            # EV marker
            ax.plot(ev, y, "|", color=C["ev"],
                    markersize=16, markeredgewidth=2.5)
            # Labels
            ax.text(p5 - 0.2, y, f"{p5:+.1f}", va="center", ha="right",
                    fontsize=8, color=C["muted"])
            ax.text(p95 + 0.2, y, f"{p95:+.1f}", va="center", ha="left",
                    fontsize=8, color=C["muted"])
            ax.text(ev, y + 0.32, f"E={ev:+.2f}", va="bottom", ha="center",
                    fontsize=8, color=C["ev"])

        ax.set_yticks(y_pos)
        ax.set_yticklabels([SPREAD_LABELS[sp] for sp in spreads], fontsize=10)
        ax_spine_style(ax)
        ax.grid(True, axis="x", alpha=0.25)

    # Delta-specific formatting
    ax_delta.axvline(0, color=C["muted"], linewidth=0.8, linestyle="--", alpha=0.5)
    ax_delta.set_xlabel("Shock delta ($/bbl)", fontsize=9)
    ax_delta.set_title("Shock delta distributions  |  dark = 50%  light = 90%",
                        fontsize=9.5, pad=8)

    # Total-specific formatting
    for i, sp in enumerate(spreads):
        ax_total.axvline(baseline[sp], color=spread_cols[i],
                          linewidth=0.8, linestyle=":", alpha=0.5)
    ax_total.set_xlabel("Total spread level post-shock ($/bbl)", fontsize=9)
    ax_total.set_title("Total spread level  |  dotted = current baseline",
                        fontsize=9.5, pad=8)

    legend_elements = [
        mpatches.Patch(color=C["s1"], alpha=0.55, label="50% confidence"),
        mpatches.Patch(color=C["s1"], alpha=0.18, label="90% confidence"),
        plt.Line2D([0],[0], color=C["ev"], linewidth=2, marker="|",
                   markersize=12, label="Expected value"),
    ]
    ax_delta.legend(handles=legend_elements, loc="lower right", fontsize=8)

    fig.suptitle("Confidence bands: 50% and 90% ranges across all three spreads",
                  fontsize=11, y=1.01)
    fig.tight_layout()
    fig.savefig(output_path)
    plt.close(fig)
    print(f"  Saved: {output_path}")


# ─────────────────────────────────────────────────────────────
# PLOT 8 — SENSITIVITY ANALYSIS
# ─────────────────────────────────────────────────────────────

def plot_sensitivity(sensitivity: dict, output_path: str) -> None:
    """
    Bar chart: for M1M2 and M1M6, show how much E[Δ] changes
    per 1% shift in each scenario's probability.
    """
    apply_style()
    fig, axes = plt.subplots(1, 2, figsize=(11, 4.5))

    for ax, sp_key in zip(axes, ["M1M2", "M1M6"]):
        sens  = sensitivity[sp_key]
        names = list(sens["sensitivities"].keys())
        vals  = [sens["sensitivities"][n]["per_1pct"] for n in names]
        base_ev = sens["base_ev"]

        colors = [SC_COLORS[i] for i in range(len(names))]
        bar_colors = [c if v > 0 else C["muted"] for c, v in zip(colors, vals)]

        bars = ax.bar(range(len(names)), vals, color=bar_colors, alpha=0.85, width=0.6)

        for bar, val in zip(bars, vals):
            ax.text(bar.get_x() + bar.get_width() / 2,
                    val + (0.04 if val > 0 else -0.12),
                    f"{val:+.3f}", ha="center",
                    va="bottom" if val > 0 else "top",
                    fontsize=8.5, color=C["text"])

        ax.axhline(0, color=C["border"], linewidth=0.8)
        ax.set_xticks(range(len(names)))
        ax.set_xticklabels(["S1", "S2", "S3", "S4"], fontsize=10)
        ax.set_ylabel("ΔE[Δ] per 1% probability shift  ($/bbl)", fontsize=9)
        ax.set_title(
            f"{SPREAD_LABELS[sp_key]}  |  base E[Δ] = ${base_ev:.3f}",
            fontsize=9.5, pad=8)
        ax_spine_style(ax)

    fig.suptitle("Sensitivity: how much does E[Δ] change if each probability shifts by 1%?",
                  fontsize=11, y=1.02)

    legend_elements = [mpatches.Patch(color=SC_COLORS[i], alpha=0.85,
                                       label=f"S{i+1}")
                        for i in range(4)]
    axes[1].legend(handles=legend_elements, loc="upper left", fontsize=9)

    fig.tight_layout()
    fig.savefig(output_path)
    plt.close(fig)
    print(f"  Saved: {output_path}")


# ─────────────────────────────────────────────────────────────
# MASTER PLOT RUNNER
# ─────────────────────────────────────────────────────────────

def run_all_plots(results, output_dir: str = "plots") -> dict:
    """
    Generate all 8 plots and save to output_dir.
    Returns a dict of plot name → file path.
    """
    out = Path(output_dir)
    out.mkdir(exist_ok=True)

    paths = {}

    print("Generating plots...")

    # Plot 1: Regime history
    p = str(out / "01_regime_history.png")
    plot_regime_history(results.regime_segments, results.baseline, p)
    paths["regime_history"] = p

    # Plot 2: Scenario tree
    p = str(out / "02_scenario_tree.png")
    plot_scenario_tree(results.scenarios, results.baseline, p)
    paths["scenario_tree"] = p

    # Plots 3/4/5: Distribution per spread
    for sp in SPREAD_ORDER:
        p = str(out / f"0{SPREAD_ORDER.index(sp)+3}_dist_{sp}.png")
        plot_distribution(results.distributions[sp], results.scenarios,
                          sp, results.baseline[sp], p)
        paths[f"dist_{sp}"] = p

    # Plot 6: Contributions
    p = str(out / "06_contributions.png")
    plot_contributions(results.distributions, results.scenarios, p)
    paths["contributions"] = p

    # Plot 7: Confidence bands
    p = str(out / "07_confidence_bands.png")
    plot_confidence_bands(results.distributions, results.baseline, p)
    paths["confidence_bands"] = p

    # Plot 8: Sensitivity
    p = str(out / "08_sensitivity.png")
    plot_sensitivity(results.sensitivity, p)
    paths["sensitivity"] = p

    print(f"\nAll plots saved to: {output_dir}/")
    return paths


# ─────────────────────────────────────────────────────────────
# SELF-TEST
# ─────────────────────────────────────────────────────────────

if __name__ == "__main__":
    from stage5_runner import run_analysis

    print("=" * 60)
    print("STAGE 6 — PLOTS SELF-TEST")
    print("=" * 60)

    results = run_analysis(verbose=False)
    paths   = run_all_plots(results, output_dir="plots")

    print()
    print("Generated:")
    for name, path in paths.items():
        size_kb = Path(path).stat().st_size // 1024
        print(f"  {name:<22} → {path}  ({size_kb} KB)")