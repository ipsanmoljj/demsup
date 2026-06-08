# R/plot_all_products.R
# ---------------------
# 5-panel chart with all products on identical timeline.
# Converts HO and Gasoil to $/bbl equivalent for visual comparison.
# wtcl_lco treated as location spread (separate panel title/colour).

plot_all_products <- function(results, results_lco, results_ho,
                               results_lgo, results_wtlco,
                               save_path = "output/all_products_breaks.png") {

  # ── Conversion factors to $/bbl ───────────────────────────────────────────
  conversions <- list(
    WTI       = list(res = results,       label = "WTI CL ($/bbl)",
                     col = "#185FA5", mult = 1.0),
    Brent     = list(res = results_lco,   label = "Brent LCO ($/bbl)",
                     col = "#0F6E56", mult = 1.0),
    HO        = list(res = results_ho,    label = "Heating Oil HO ($/bbl equiv, ×42)",
                     col = "#8B4513", mult = 42.0),
    Gasoil    = list(res = results_lgo,   label = "Gasoil LGO ($/bbl equiv, ÷7.45)",
                     col = "#6B2D8B", mult = 1/7.45),
    WTI_Brent = list(res = results_wtlco, label = "WTI-Brent location spread ($/bbl)",
                     col = "#B8860B", mult = 1.0)
  )

  x_min   <- as.POSIXct("2021-01-01")
  x_max   <- as.POSIXct("2026-07-01")
  x_ticks <- seq(as.POSIXct("2021-01-01"), as.POSIXct("2027-01-01"), by = "6 months")

  regime_col <- c(
    "deep_backwardation" = "#FAECE7",
    "mild_backwardation" = "#EAF3DE",
    "flat"               = "#F5F5F0",
    "mild_contango"      = "#E6F1FB",
    "deep_contango"      = "#EEEDFE",
    "transitional"       = "#FAEEDA"
  )

  png(save_path, width = 1800, height = 2400, res = 130)
  par(mfrow = c(5, 1),
      mar   = c(1.5, 5.5, 2.5, 2),
      oma   = c(5, 0, 4, 0),
      bg    = "white")

  for (nm in names(conversions)) {
    p      <- conversions[[nm]]
    d      <- p$res$data
    times  <- as.POSIXct(d$timestamp)
    m1m2   <- d$M1M2 * p$mult        # convert to $/bbl
    breaks <- p$res$consensus$high_confidence

    # y-axis: trim extreme outliers for display (keep 1st-99th percentile)
    y_lo  <- quantile(m1m2, 0.005, na.rm = TRUE)
    y_hi  <- quantile(m1m2, 0.995, na.rm = TRUE) * 1.08
    ylim  <- c(y_lo - abs(y_lo)*0.05, y_hi)

    plot(times, m1m2,
         type = "n", xlim = c(x_min, x_max), ylim = ylim,
         xlab = "", ylab = "$/bbl equiv",
         main = p$label,
         xaxt = "n", las = 1, cex.main = 0.95, cex.axis = 0.75,
         cex.lab = 0.8)

    # Regime shading
    rl <- p$res$regime_labels
    if (!is.null(rl) && nrow(rl) > 0) {
      bands <- rl[!is.na(regime_id),
                   .(start = min(as.POSIXct(timestamp)),
                     end   = max(as.POSIXct(timestamp)),
                     curve_regime = first(curve_regime)),
                   by = regime_id]
      for (i in seq_len(nrow(bands))) {
        col <- regime_col[bands$curve_regime[i]]
        if (is.na(col)) col <- "#F5F5F0"
        rect(bands$start[i], ylim[1], bands$end[i], ylim[2],
             col = adjustcolor(col, alpha.f = 0.35), border = NA)
      }
    }

    lines(times, m1m2, col = p$col, lwd = 0.65)
    abline(h = 0, lty = 2, col = "gray55", lwd = 0.5)

    # Break lines + labels
    for (i in seq_along(breaks)) {
      abline(v = as.POSIXct(breaks[i]), col = "#E24B4A", lwd = 0.9)
      text(as.POSIXct(breaks[i]),
           ylim[1] + (ylim[2] - ylim[1]) * 0.84,
           paste0("#", i, "\n", format(breaks[i], "%b%y")),
           col = "#E24B4A", cex = 0.48, pos = 4, offset = 0.15)
    }

    axis.POSIXct(1, at = x_ticks, format = "%b %Y",
                 cex.axis = 0.68, las = 2)
    grid(nx = NA, ny = NULL, col = "gray93", lty = 1)
  }

  # ── Titles and legend ─────────────────────────────────────────────────────
  mtext("All products — M1M2 spread structural breaks (identical timeline, normalised to $/bbl)",
        side = 3, outer = TRUE, cex = 1.05, font = 2, line = 2)
  mtext("HO × 42 | LGO ÷ 7.45 | Red lines = structural breaks | Shading = curve regime",
        side = 3, outer = TRUE, cex = 0.78, line = 0.4)

  # Regime colour legend at bottom
  par(fig = c(0.05, 0.95, 0, 0.035), oma = c(0,0,0,0),
      mar = c(0,0,0,0), new = TRUE)
  plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
  legend("center",
         legend = c("Deep backwardation", "Mild backwardation",
                    "Flat", "Mild contango", "Deep contango", "Transitional"),
         fill   = adjustcolor(c("#FAECE7","#EAF3DE","#F5F5F0",
                                "#E6F1FB","#EEEDFE","#FAEEDA"), alpha.f = 0.6),
         border = NA, cex = 0.72, bty = "n", ncol = 6, x.intersp = 0.5)

  dev.off()
  cat("Saved:", save_path, "\n")
}

# ── Wrapper that saves both full-size and GitHub-viewable small version ───────
save_all_products_both <- function(results, results_lco, results_ho,
                                    results_lgo, results_wtlco) {
  # Full resolution (local use)
  plot_all_products(results, results_lco, results_ho, results_lgo, results_wtlco,
                    save_path = "output/all_products_breaks.png")

  # Small version for GitHub (under 1MB, renders in browser)
  small_path <- "output/all_products_breaks_github.png"
  png(small_path, width = 1000, height = 1300, res = 85)

  par(mfrow = c(5, 1),
      mar   = c(1.5, 4, 2, 1.5),
      oma   = c(4, 0, 3, 0),
      bg    = "white")

  conversions <- list(
    list(res = results,       label = "WTI CL ($/bbl)",            col = "#185FA5", mult = 1.0),
    list(res = results_lco,   label = "Brent LCO ($/bbl)",         col = "#0F6E56", mult = 1.0),
    list(res = results_ho,    label = "Heating Oil HO ($/bbl ×42)", col = "#8B4513", mult = 42.0),
    list(res = results_lgo,   label = "Gasoil LGO ($/bbl ÷7.45)",  col = "#6B2D8B", mult = 1/7.45),
    list(res = results_wtlco, label = "WTI-Brent spread ($/bbl)",  col = "#B8860B", mult = 1.0)
  )

  x_min   <- as.POSIXct("2021-01-01")
  x_max   <- as.POSIXct("2026-07-01")
  x_ticks <- seq(as.POSIXct("2021-01-01"), as.POSIXct("2027-01-01"), by = "1 year")

  regime_col <- c(
    "deep_backwardation" = "#FAECE7", "mild_backwardation" = "#EAF3DE",
    "flat" = "#F5F5F0", "mild_contango" = "#E6F1FB",
    "deep_contango" = "#EEEDFE", "transitional" = "#FAEEDA"
  )

  for (p in conversions) {
    d      <- p$res$data
    times  <- as.POSIXct(d$timestamp)
    m1m2   <- d$M1M2 * p$mult
    breaks <- p$res$consensus$high_confidence
    y_lo   <- quantile(m1m2, 0.005, na.rm = TRUE)
    y_hi   <- quantile(m1m2, 0.995, na.rm = TRUE) * 1.08
    ylim   <- c(y_lo - abs(y_lo)*0.05, y_hi)

    plot(times, m1m2, type = "n", xlim = c(x_min, x_max), ylim = ylim,
         xlab = "", ylab = "$/bbl", main = p$label,
         xaxt = "n", las = 1, cex.main = 0.8, cex.axis = 0.65)

    rl <- p$res$regime_labels
    if (!is.null(rl) && nrow(rl) > 0) {
      bands <- rl[!is.na(regime_id),
                   .(start = min(as.POSIXct(timestamp)),
                     end   = max(as.POSIXct(timestamp)),
                     curve_regime = first(curve_regime)),
                   by = regime_id]
      for (i in seq_len(nrow(bands))) {
        col <- regime_col[bands$curve_regime[i]]
        if (is.na(col)) col <- "#F5F5F0"
        rect(bands$start[i], ylim[1], bands$end[i], ylim[2],
             col = adjustcolor(col, alpha.f = 0.35), border = NA)
      }
    }

    lines(times, m1m2, col = p$col, lwd = 0.6)
    abline(h = 0, lty = 2, col = "gray60", lwd = 0.4)
    abline(v = as.POSIXct(breaks), col = "#E24B4A", lwd = 0.8)
    axis.POSIXct(1, at = x_ticks, format = "%Y", cex.axis = 0.65, las = 1)
    grid(nx = NA, ny = NULL, col = "gray93", lty = 1)
  }

  mtext("All products — M1M2 structural breaks ($/bbl normalised)",
        side = 3, outer = TRUE, cex = 0.9, font = 2, line = 1.5)
  mtext("Red = break dates | Shading = curve regime",
        side = 1, outer = TRUE, cex = 0.7, line = 2)

  dev.off()
  cat("Saved:", small_path, "\n")
  invisible(list(full = "output/all_products_breaks.png", small = small_path))
}