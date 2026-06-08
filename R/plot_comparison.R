# R/plot_comparison.R
# --------------------
# Plot WTI and Brent M1M2 breaks on identical x-axis for comparison.

plot_comparison <- function(results_wti, results_lco,
                             save_path = "output/CL_LCO_comparison.png") {

  # ── Common x-axis range (union of both datasets) ────────────────────────
  wti_times  <- as.POSIXct(results_wti$data$timestamp)
  lco_times  <- as.POSIXct(results_lco$data$timestamp)
  x_min      <- min(c(wti_times, lco_times))
  x_max      <- max(c(wti_times, lco_times))
  x_ticks    <- seq(as.POSIXct("2021-01-01"), as.POSIXct("2027-01-01"),
                    by = "3 months")

  # ── Common y-axis range per product (don't force same — different scales) 
  wti_m1m2   <- results_wti$data$M1M2
  lco_m1m2   <- results_lco$data$M1M2
  wti_ylim   <- c(min(wti_m1m2, na.rm=TRUE) - 0.3,
                  max(wti_m1m2, na.rm=TRUE) + 0.5)
  lco_ylim   <- c(min(lco_m1m2, na.rm=TRUE) - 0.3,
                  max(lco_m1m2, na.rm=TRUE) + 0.5)

  wti_breaks <- results_wti$consensus$high_confidence
  lco_breaks <- results_lco$consensus$high_confidence

  # ── Regime shading colours ────────────────────────────────────────────────
  regime_col <- c(
    "deep_backwardation" = "#FAECE7",
    "mild_backwardation" = "#EAF3DE",
    "flat"               = "#F1EFE8",
    "mild_contango"      = "#E6F1FB",
    "deep_contango"      = "#EEEDFE",
    "transitional"       = "#FAEEDA"
  )

  .add_regime_bands <- function(regime_labels, ylim) {
    bands <- regime_labels[!is.na(regime_id),
                            .(start = min(as.POSIXct(timestamp)),
                              end   = max(as.POSIXct(timestamp)),
                              curve_regime = first(curve_regime)),
                            by = regime_id]
    for (i in seq_len(nrow(bands))) {
      col <- regime_col[bands$curve_regime[i]]
      if (is.na(col)) col <- "#F1EFE8"
      rect(bands$start[i], ylim[1], bands$end[i], ylim[2],
           col = adjustcolor(col, alpha.f = 0.4), border = NA)
    }
  }

  .add_break_lines <- function(breaks, ylim, label_y_frac = 0.88) {
    for (i in seq_along(breaks)) {
      abline(v = as.POSIXct(breaks[i]), col = "#E24B4A",
             lwd = 1.2, lty = "solid")
      text(as.POSIXct(breaks[i]),
           ylim[1] + (ylim[2] - ylim[1]) * label_y_frac,
           paste0("#", i, "\n", format(breaks[i], "%b %y")),
           col = "#E24B4A", cex = 0.55, pos = 4, offset = 0.2)
    }
  }

  # ── Open PNG device ───────────────────────────────────────────────────────
  png(save_path, width = 1600, height = 1000, res = 130)
  par(mfrow = c(2, 1),
      mar   = c(2, 4.5, 3, 2),
      oma   = c(3, 0, 2, 0),
      bg    = "white")

  # ── Panel 1: WTI ─────────────────────────────────────────────────────────
  plot(wti_times, wti_m1m2,
       type = "n",
       xlim = c(x_min, x_max),
       ylim = wti_ylim,
       xlab = "", ylab = "M1M2 ($/bbl)",
       main = "WTI CL — M1M2 spread",
       xaxt = "n", las = 1, cex.main = 1.1, cex.axis = 0.8)

  .add_regime_bands(results_wti$regime_labels, wti_ylim)
  lines(wti_times, wti_m1m2, col = "#185FA5", lwd = 0.7)
  abline(h = 0, lty = 2, col = "gray50", lwd = 0.5)
  .add_break_lines(wti_breaks, wti_ylim)
  axis.POSIXct(1, at = x_ticks, format = "%b %Y", cex.axis = 0.75, las = 2)
  grid(nx = NA, ny = NULL, col = "gray90", lty = 1)

  # ── Panel 2: Brent ────────────────────────────────────────────────────────
  plot(lco_times, lco_m1m2,
       type = "n",
       xlim = c(x_min, x_max),
       ylim = lco_ylim,
       xlab = "", ylab = "M1M2 ($/bbl)",
       main = "Brent LCO — M1M2 spread",
       xaxt = "n", las = 1, cex.main = 1.1, cex.axis = 0.8)

  .add_regime_bands(results_lco$regime_labels, lco_ylim)
  lines(lco_times, lco_m1m2, col = "#0F6E56", lwd = 0.7)
  abline(h = 0, lty = 2, col = "gray50", lwd = 0.5)
  .add_break_lines(lco_breaks, lco_ylim)
  axis.POSIXct(1, at = x_ticks, format = "%b %Y", cex.axis = 0.75, las = 2)
  grid(nx = NA, ny = NULL, col = "gray90", lty = 1)

  # ── Shared x-axis label + legend ─────────────────────────────────────────
  mtext("Structural breaks (red) numbered by order of occurrence per product",
        side = 1, outer = TRUE, cex = 0.8, line = 1.5)
  mtext("WTI vs Brent — M1M2 spread structural breaks (identical timeline)",
        side = 3, outer = TRUE, cex = 1.0, line = 0.5, font = 2)

  # ── Regime legend ─────────────────────────────────────────────────────────
  par(fig = c(0, 1, 0, 1), oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
  plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
  legend("bottomright",
         legend = c("Deep backwardation", "Mild backwardation",
                    "Flat", "Mild contango", "Deep contango"),
         fill   = adjustcolor(c("#FAECE7","#EAF3DE","#F1EFE8",
                                "#E6F1FB","#EEEDFE"), alpha.f = 0.6),
         border = NA, cex = 0.7, bty = "n", ncol = 5)

  dev.off()
  cat("Saved:", save_path, "\n")
}