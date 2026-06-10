# R/window_selector.R
# --------------------
# Selects the optimal rolling window size for the level z-score used in
# regime classification, using BIC (Bayesian Information Criterion).
#
# For each candidate window size, the M1M2 series is divided into regime
# segments based on the z-score labels. The window that minimises BIC
# (lowest within-regime variance, penalised for complexity) is selected.
#
# Usage:
#   source("R/window_selector.R")
#
#   # Run after run_parallel_models() so model_signals.rds exists
#   result <- select_level_z_window(product = "CL")
#   print(result$bic_table)
#   cat("Optimal window:", result$optimal_window, "days\n")
#
#   # Run for all products and compare
#   all_windows <- select_window_all_products(c("CL","LCO","HO","LGO"))
#   print(all_windows)

library(data.table)
library(zoo)

# ── Candidate window sizes (trading days) ─────────────────────────────────────
# Standard grid used for all products:
# 21=1mo, 42=2mo, 63=1qtr, 84=4mo, 126=6mo, 168=8mo, 252=1yr
CANDIDATE_WINDOWS <- c(21, 42, 63, 84, 126, 168, 252)

# Extended grid for products where BIC is still declining at 252 days
# (LCO, HO, LGO) — adds 315=15mo and 378=18mo
# NOTE: 378-day warm-up consumes ~1.5 years of data — only use when n > 800
CANDIDATE_WINDOWS_EXTENDED <- c(21, 42, 63, 84, 126, 168, 252, 315, 378)

# Z-score thresholds for tier assignment (same as classifier)
LEVEL_Z_HIGH <-  0.5
LEVEL_Z_LOW  <- -0.5

# ── BIC calculation for a given window ───────────────────────────────────────

.compute_bic <- function(y, window_size) {
  n <- length(y)

  # Rolling mean and sd
  roll_mean <- zoo::rollmean(y, window_size, fill = NA, align = "right")
  roll_sd   <- zoo::rollapply(y, window_size, sd, fill = NA, align = "right")

  # Fill early NAs with expanding window
  for (i in which(is.na(roll_mean))) {
    roll_mean[i] <- mean(y[1:i], na.rm = TRUE)
    roll_sd[i]   <- if (i > 1) sd(y[1:i], na.rm = TRUE) else 0
  }
  roll_sd <- pmax(roll_sd, 1e-6)

  # Level z-score
  lz <- (y - roll_mean) / roll_sd

  # Assign tier labels
  tier <- ifelse(lz >= LEVEL_Z_HIGH, "high",
          ifelse(lz <= LEVEL_Z_LOW,  "low", "mid"))

  # Identify contiguous regime segments (runs of same tier)
  tier_rle    <- rle(tier)
  n_segments  <- length(tier_rle$lengths)

  # Reconstruct segment index per bar
  seg_idx <- rep(seq_len(n_segments), tier_rle$lengths)

  # Within-segment RSS: sum of (y - segment_mean)^2
  seg_means <- tapply(y, seg_idx, mean, na.rm = TRUE)
  fitted    <- seg_means[seg_idx]
  residuals <- y - fitted
  rss       <- sum(residuals^2, na.rm = TRUE)

  # Number of free parameters:
  # Each segment has 2 parameters (mean + variance) → k = 2 * n_segments
  k <- 2 * n_segments

  # BIC = n * log(RSS/n) + k * log(n)
  bic <- n * log(rss / n) + k * log(n)

  # Also compute F-statistic (between / within variance ratio)
  grand_mean    <- mean(y, na.rm = TRUE)
  ss_between    <- sum(tier_rle$lengths * (seg_means - grand_mean)^2, na.rm = TRUE)
  ss_within     <- rss
  df_between    <- n_segments - 1
  df_within     <- n - n_segments
  f_stat        <- if (df_within > 0 && ss_within > 0)
                     (ss_between / df_between) / (ss_within / df_within)
                   else NA_real_

  # Mean within-segment variance (lower = more homogeneous regimes)
  mean_within_var <- mean(tapply(y, seg_idx, var, na.rm = TRUE), na.rm = TRUE)

  list(
    window          = window_size,
    bic             = round(bic, 2),
    n_segments      = n_segments,
    rss             = round(rss, 4),
    f_stat          = round(f_stat, 3),
    mean_within_var = round(mean_within_var, 6)
  )
}

# ── Main window selection function ────────────────────────────────────────────

select_level_z_window <- function(product    = "CL",
                                   output_dir = "output",
                                   windows    = CANDIDATE_WINDOWS,
                                   plot       = TRUE) {

  cat("\n", strrep("=", 60), "\n")
  cat("WINDOW SELECTOR —", product, "\n")
  cat(strrep("=", 60), "\n\n")

  # Load signals (only need M1M2)
  signals_path <- file.path(output_dir, "model_signals.rds")
  if (!file.exists(signals_path)) {
    stop("model_signals.rds not found in ", output_dir,
         "\nRun run_parallel_models() first.")
  }

  signals <- readRDS(signals_path)
  y       <- as.numeric(signals$M1M2)
  y       <- y[!is.na(y)]
  n       <- length(y)

  cat("Series length:", n, "bars\n")
  cat("M1M2 range:   ", round(min(y), 3), "to", round(max(y), 3), "\n\n")
  cat("Testing", length(windows), "window sizes:", paste(windows, collapse = ", "), "\n\n")

  # ── Run BIC for each window ───────────────────────────────────────────────
  results <- lapply(windows, function(w) .compute_bic(y, w))
  bic_dt  <- rbindlist(results)

  # ── Rank by BIC (lower is better) ───────────────────────────────────────
  bic_dt[, bic_rank  := rank(bic)]
  bic_dt[, delta_bic := round(bic - min(bic), 2)]

  # ── Elbow detection via second derivative ────────────────────────────────
  # Raw minimum BIC can be misleading when BIC is monotonically declining
  # (no true minimum found within the grid = possible overfitting with large windows).
  # The elbow = point of maximum curvature = maximum second derivative of BIC curve.
  # This is the genuine trade-off point between fit improvement and complexity penalty.
  bic_ordered <- bic_dt[order(window)]
  n_w <- nrow(bic_ordered)

  if (n_w >= 3) {
    # Second derivative: BIC[i+1] - 2*BIC[i] + BIC[i-1]
    bic_vals_ord <- bic_ordered$bic
    second_deriv <- c(NA,
                      diff(bic_vals_ord, differences = 2),
                      NA)
    bic_dt[order(window), second_deriv := second_deriv]

    # Elbow = window with maximum second derivative (sharpest bend)
    elbow_idx    <- which.max(second_deriv[!is.na(second_deriv)])
    # Adjust index to skip the leading NA
    elbow_window <- bic_ordered$window[elbow_idx + 1]

    # Check if BIC is monotonically declining (no true minimum)
    bic_diffs        <- diff(bic_vals_ord)
    is_monotone      <- all(bic_diffs < 0)

    # Selection rule:
    #   - If BIC has a genuine minimum (not at the boundary) → use which.min
    #   - If BIC is monotonically declining → use elbow (avoids overfitting)
    raw_min_window   <- bic_dt[which.min(bic), window]
    raw_min_at_edge  <- raw_min_window == max(bic_dt$window)

    if (is_monotone || raw_min_at_edge) {
      optimal_window <- elbow_window
      selection_method <- "ELBOW (BIC monotone — raw minimum avoided to prevent overfitting)"
    } else {
      optimal_window <- raw_min_window
      selection_method <- "MINIMUM BIC (genuine interior minimum found)"
    }
  } else {
    optimal_window   <- bic_dt[which.min(bic), window]
    selection_method <- "MINIMUM BIC (insufficient points for elbow detection)"
    bic_dt[, second_deriv := NA_real_]
  }

  bic_dt[, selected := ifelse(window == optimal_window, "<<< OPTIMAL", "")]

  cat("--- BIC RESULTS ---\n\n")
  print(bic_dt[order(window), .(window, bic, delta_bic, second_deriv,
                                 n_segments, f_stat, selected)])

  cat("\nSelection method:", selection_method, "\n")
  cat("Optimal window:  ", optimal_window, "days (",
      round(optimal_window / 21, 1), "months )\n")

  # ── Plot BIC curve ────────────────────────────────────────────────────────
  if (plot) {
    plot_path <- file.path(output_dir,
                           paste0("window_selection_", product, ".png"))
    png(plot_path, width = 1600, height = 600, res = 120)
    par(mfrow = c(1, 3), mar = c(4, 4.5, 3, 2), bg = "white")

    bic_vals <- bic_dt[order(window), bic]
    w_vals   <- bic_dt[order(window), window]
    sd_vals  <- bic_dt[order(window), second_deriv]

    # Panel 1: BIC by window — colour raw minimum blue, elbow red
    raw_min_w <- bic_dt[which.min(bic), window]
    pt_col <- ifelse(w_vals == optimal_window, "#C0392B",
              ifelse(w_vals == raw_min_w,       "#185FA5", "gray50"))
    pt_cex <- ifelse(w_vals %in% c(optimal_window, raw_min_w), 1.6, 0.9)

    plot(w_vals, bic_vals, type = "b", pch = 19,
         col = pt_col, cex = pt_cex,
         lwd = 1.5, main = paste0(product, " — BIC by window size"),
         xlab = "Window (days)", ylab = "BIC (lower = better)",
         xaxt = "n", las = 1, cex.main = 0.9)
    axis(1, at = w_vals,
         labels = paste0(w_vals, "\n(", round(w_vals/21,1), "mo)"),
         cex.axis = 0.65)
    abline(v = optimal_window, col = "#C0392B", lty = 2, lwd = 1.2)
    if (raw_min_w != optimal_window)
      abline(v = raw_min_w, col = "#185FA5", lty = 3, lwd = 0.8)
    text(optimal_window, max(bic_vals),
         paste0("Elbow:\n", optimal_window, "d"),
         col = "#C0392B", cex = 0.65, adj = c(-0.1, 1))
    if (raw_min_w != optimal_window)
      text(raw_min_w, min(bic_vals),
           paste0("Min:\n", raw_min_w, "d"),
           col = "#185FA5", cex = 0.65, adj = c(-0.1, 0))

    # Panel 2: n_segments and F-stat by window
    par(mar = c(4, 4.5, 3, 4))
    seg_vals <- bic_dt[order(window), n_segments]
    plot(w_vals, seg_vals, type = "b", pch = 19, col = "#0F6E56",
         lwd = 1.5, main = paste0(product, " — Segments & F-stat by window"),
         xlab = "Window (days)", ylab = "N segments", xaxt = "n",
         las = 1, cex.main = 0.9)
    axis(1, at = w_vals,
         labels = paste0(w_vals, "\n(", round(w_vals/21,1), "mo)"),
         cex.axis = 0.7)

    # F-stat on right axis
    f_vals <- bic_dt[order(window), f_stat]
    par(new = TRUE)
    plot(w_vals, f_vals, type = "b", pch = 17, col = "#E67E22",
         axes = FALSE, xlab = "", ylab = "", lwd = 1.5)
    axis(4, col = "#E67E22", col.axis = "#E67E22", las = 1, cex.axis = 0.8)
    mtext("F-statistic", side = 4, line = 2.5, col = "#E67E22", cex = 0.8)
    legend("topright", legend = c("N segments", "F-statistic"),
           col = c("#0F6E56", "#E67E22"), pch = c(19, 17),
           lwd = 1.5, cex = 0.7, bty = "n")

    # Panel 3: Second derivative of BIC (elbow detection)
    sd_plot <- ifelse(is.na(sd_vals), 0, sd_vals)
    bar_col  <- ifelse(w_vals == optimal_window, "#C0392B", "gray60")
    barplot(sd_plot, names.arg = paste0(w_vals, "d"),
            col = bar_col, border = NA,
            main = paste0(product, " — BIC 2nd derivative (elbow)"),
            xlab = "Window", ylab = "2nd derivative (higher = sharper bend)",
            las = 2, cex.names = 0.65, cex.main = 0.9)
    abline(h = 0, col = "gray40", lwd = 0.5)
    text(which(w_vals == optimal_window) * 1.2 - 0.5,
         max(sd_plot, na.rm = TRUE) * 0.9,
         paste0("Elbow\n", optimal_window, "d"),
         col = "#C0392B", cex = 0.65)

    dev.off()
    cat("Plot saved:", plot_path, "\n")
  }

  list(
    product          = product,
    bic_table        = bic_dt[order(window)],
    optimal_window   = optimal_window,
    selection_method = selection_method
  )
}

# ── Run for all products and summarise ────────────────────────────────────────

select_window_all_products <- function(products   = c("CL", "LCO", "HO", "LGO"),
                                        output_dir = "output") {

  cat("\n", strrep("=", 60), "\n")
  cat("WINDOW SELECTION — ALL PRODUCTS\n")
  cat(strrep("=", 60), "\n\n")
  cat("NOTE: Run run_parallel_models() for each product before calling this.\n")
  cat("      model_signals.rds must reflect the correct product's data.\n\n")

  # This function assumes you have already run each product through
  # run_parallel_models() and saved the results. Since model_signals.rds
  # is overwritten per product, you need to either:
  #   (a) run select_level_z_window() immediately after each run_parallel_models()
  #   (b) save per-product signals separately (recommended for full pipeline)

  results <- lapply(products, function(p) {
    res <- tryCatch(
      select_level_z_window(product = p, output_dir = output_dir, plot = TRUE),
      error = function(e) {
        cat("  ERROR for", p, ":", conditionMessage(e), "\n")
        NULL
      }
    )
    if (is.null(res)) return(NULL)
    data.table(product = p, optimal_window = res$optimal_window)
  })

  results <- results[!sapply(results, is.null)]

  if (length(results) > 0) {
    summary_dt <- rbindlist(results)
    cat("\n--- OPTIMAL WINDOWS BY PRODUCT ---\n\n")
    print(summary_dt)
    return(summary_dt)
  }

  invisible(NULL)
}

# ═════════════════════════════════════════════════════════════════════════════
# THRESHOLD SELECTOR — finds optimal quantile thresholds for level tier
# assignment and deep regime detection via BIC grid search
# ═════════════════════════════════════════════════════════════════════════════
#
# Instead of fixed z-score thresholds (e.g. ±0.5, ±1.5), this function
# finds the empirical quantile thresholds that best separate the M1M2
# distribution into homogeneous regime tiers for each product.
#
# Output thresholds:
#   q_low, q_high       : quantiles for low/mid/high tier split
#   q_deep_low          : quantile below which = Deep-Contango
#   q_deep_high         : quantile above which = Deep-Backwardation
#
# Usage:
#   thresh <- select_thresholds(product = "CL", window = 168)
#   print(thresh$threshold_table)
#   cat("High threshold:", thresh$q_high, "\n")
#   cat("Deep-high threshold:", thresh$q_deep_high, "\n")

# ── Candidate threshold quantile grids ───────────────────────────────────────

# Normal tier split candidates (low/mid/high)
TIER_QUANTILE_GRID <- list(
  c(0.20, 0.80),
  c(0.25, 0.75),
  c(0.33, 0.67),
  c(0.40, 0.60),
  c(0.15, 0.85)
)

# Deep tier candidates (extreme low/high within the high/low tiers)
DEEP_QUANTILE_GRID <- c(0.05, 0.10, 0.15)

# ── BIC for a given threshold combination ────────────────────────────────────

.compute_bic_thresholds <- function(y, lz, q_low, q_high,
                                     q_deep_low, q_deep_high) {
  n <- length(y)

  # Compute threshold values from quantiles of the z-score distribution
  z_low       <- quantile(lz, q_low,       na.rm = TRUE)
  z_high      <- quantile(lz, q_high,      na.rm = TRUE)
  z_deep_low  <- quantile(lz, q_deep_low,  na.rm = TRUE)
  z_deep_high <- quantile(lz, q_deep_high, na.rm = TRUE)

  # Assign 5-tier labels
  tier <- ifelse(lz >= z_deep_high, "deep_high",
          ifelse(lz >= z_high,      "high",
          ifelse(lz <= z_deep_low,  "deep_low",
          ifelse(lz <= z_low,       "low", "mid"))))

  # Contiguous segments
  tier_rle   <- rle(tier)
  n_segments <- length(tier_rle$lengths)
  seg_idx    <- rep(seq_len(n_segments), tier_rle$lengths)

  # Within-segment RSS
  seg_means  <- tapply(y, seg_idx, mean, na.rm = TRUE)
  fitted     <- seg_means[seg_idx]
  rss        <- sum((y - fitted)^2, na.rm = TRUE)

  # BIC
  k   <- 2 * n_segments
  bic <- n * log(rss / n) + k * log(n)

  # F-statistic
  grand_mean <- mean(y, na.rm = TRUE)
  ss_between <- sum(tier_rle$lengths * (seg_means - grand_mean)^2, na.rm = TRUE)
  df_between <- n_segments - 1
  df_within  <- n - n_segments
  f_stat     <- if (df_within > 0 && rss > 0)
                  (ss_between / df_between) / (rss / df_within)
                else NA_real_

  list(
    q_low        = q_low,
    q_high       = q_high,
    q_deep_low   = q_deep_low,
    q_deep_high  = q_deep_high,
    z_low        = round(z_low, 4),
    z_high       = round(z_high, 4),
    z_deep_low   = round(z_deep_low, 4),
    z_deep_high  = round(z_deep_high, 4),
    bic          = round(bic, 2),
    n_segments   = n_segments,
    f_stat       = round(f_stat, 3)
  )
}

# ── Main threshold selection function ────────────────────────────────────────

select_thresholds <- function(product    = "CL",
                               output_dir = "output",
                               window     = NULL,
                               plot       = TRUE) {

  cat("\n", strrep("=", 60), "\n")
  cat("THRESHOLD SELECTOR —", product, "\n")
  cat(strrep("=", 60), "\n\n")

  # Load signals
  signals_path <- file.path(output_dir, "model_signals.rds")
  if (!file.exists(signals_path)) {
    stop("model_signals.rds not found. Run run_parallel_models() first.")
  }
  signals <- readRDS(signals_path)
  y       <- as.numeric(signals$M1M2)
  y       <- y[!is.na(y)]
  n       <- length(y)

  # ── Get or auto-select window ─────────────────────────────────────────────
  if (is.null(window)) {
    if (exists("select_level_z_window", mode = "function")) {
      cat("  Auto-selecting window via BIC first...\n")
      w_res  <- select_level_z_window(product    = product,
                                       output_dir = output_dir,
                                       windows    = CANDIDATE_WINDOWS_EXTENDED,
                                       plot       = FALSE)
      window <- w_res$optimal_window
      cat("  Using window:", window, "days\n\n")
    } else {
      window <- 126
      cat("  window_selector not available — using 126-day default\n\n")
    }
  }

  # ── Compute level z-score with chosen window ──────────────────────────────
  roll_mean <- zoo::rollmean(y, window, fill = NA, align = "right")
  roll_sd   <- zoo::rollapply(y, window, sd, fill = NA, align = "right")
  for (i in which(is.na(roll_mean))) {
    roll_mean[i] <- mean(y[1:i], na.rm = TRUE)
    roll_sd[i]   <- if (i > 1) sd(y[1:i], na.rm = TRUE) else 0
  }
  roll_sd <- pmax(roll_sd, 1e-6)
  lz      <- (y - roll_mean) / roll_sd

  cat("Level z-score distribution:\n")
  cat("  Mean:", round(mean(lz, na.rm=TRUE), 3),
      "| SD:", round(sd(lz, na.rm=TRUE), 3),
      "| Skew:", round(mean((lz - mean(lz,na.rm=TRUE))^3,na.rm=TRUE) /
                       sd(lz,na.rm=TRUE)^3, 3),
      "| Kurt:", round(mean((lz - mean(lz,na.rm=TRUE))^4,na.rm=TRUE) /
                       sd(lz,na.rm=TRUE)^4, 3), "\n\n")

  # ── Grid search over all threshold combinations ───────────────────────────
  cat("Testing", length(TIER_QUANTILE_GRID), "x",
      length(DEEP_QUANTILE_GRID), "=",
      length(TIER_QUANTILE_GRID) * length(DEEP_QUANTILE_GRID),
      "threshold combinations...\n\n")

  results <- list()
  for (tq in TIER_QUANTILE_GRID) {
    for (dq in DEEP_QUANTILE_GRID) {
      # Deep quantiles must be more extreme than tier quantiles
      if (dq >= tq[1]) next   # deep_low must be below q_low
      res <- .compute_bic_thresholds(y, lz,
               q_low       = tq[1],
               q_high      = tq[2],
               q_deep_low  = dq,
               q_deep_high = 1 - dq)
      results <- c(results, list(res))
    }
  }

  thresh_dt <- rbindlist(results)
  thresh_dt[, delta_bic := round(bic - min(bic), 2)]
  thresh_dt[, selected  := ifelse(bic == min(bic), "<<< OPTIMAL", "")]

  # ── Elbow detection (same logic as window selector) ───────────────────────
  thresh_ordered <- thresh_dt[order(bic)]
  raw_min_bic    <- thresh_dt[which.min(bic)]

  cat("--- THRESHOLD BIC RESULTS (top 10) ---\n\n")
  print(thresh_dt[order(bic)][1:min(10, .N),
        .(q_low, q_high, q_deep_low, q_deep_high,
          z_low, z_high, z_deep_low, z_deep_high,
          bic, delta_bic, n_segments, f_stat, selected)])

  # Extract optimal thresholds
  opt <- thresh_dt[which.min(bic)]

  cat("\n--- OPTIMAL THRESHOLDS:", product, "---\n\n")
  cat("  Tier split    : q_low =", opt$q_low, "| q_high =", opt$q_high, "\n")
  cat("  Deep tier     : q_deep =", opt$q_deep_low,
      "/ q_deep_high =", opt$q_deep_high, "\n")
  cat("  Z-score values:\n")
  cat("    Deep-Contango  below z =", opt$z_deep_low,  "\n")
  cat("    Low tier       below z =", opt$z_low,        "\n")
  cat("    High tier      above z =", opt$z_high,       "\n")
  cat("    Deep-Backw.    above z =", opt$z_deep_high,  "\n\n")

  # ── Plot threshold BIC surface ────────────────────────────────────────────
  if (plot) {
    plot_path <- file.path(output_dir,
                           paste0("threshold_selection_", product, ".png"))
    png(plot_path, width = 1400, height = 600, res = 120)
    par(mfrow = c(1, 2), mar = c(4, 4.5, 3, 2), bg = "white")

    # Panel 1: BIC by tier quantile pair (coloured by deep quantile)
    deep_cols <- c("0.05" = "#C0392B", "0.1" = "#185FA5", "0.15" = "#0F6E56")
    plot(NULL,
         xlim = range(thresh_dt$q_low),
         ylim = range(thresh_dt$bic),
         main = paste0(product, " — Threshold BIC by tier split"),
         xlab = "Low quantile (high = 1 - low)",
         ylab = "BIC (lower = better)", las = 1, cex.main = 0.9)
    for (dq in DEEP_QUANTILE_GRID) {
      sub <- thresh_dt[q_deep_low == dq][order(q_low)]
      if (nrow(sub) == 0) next
      lines(sub$q_low, sub$bic,
            col = deep_cols[as.character(dq)], lwd = 1.5)
      points(sub$q_low, sub$bic,
             col = deep_cols[as.character(dq)], pch = 19, cex = 0.9)
    }
    # Mark optimal
    points(opt$q_low, opt$bic, col = "#C0392B", pch = 8, cex = 2, lwd = 2)
    legend("topright",
           legend = paste0("deep q=", DEEP_QUANTILE_GRID),
           col    = unname(deep_cols),
           lwd = 1.5, pch = 19, cex = 0.7, bty = "n")

    # Panel 2: z-score distribution with optimal thresholds overlaid
    hist(lz, breaks = 50, col = "gray85", border = "white",
         main = paste0(product, " — z-score distribution & thresholds"),
         xlab = "Level z-score", ylab = "Frequency",
         las = 1, cex.main = 0.9)
    abline(v = c(opt$z_deep_low, opt$z_low, opt$z_high, opt$z_deep_high),
           col = c("#2C3E50", "#1ABC9C", "#E67E22", "#C0392B"),
           lwd = c(1.5, 1.2, 1.2, 1.5), lty = c(2, 2, 2, 2))
    legend("topright",
           legend = c(
             paste0("Deep-Cont z=", round(opt$z_deep_low, 2)),
             paste0("Low z=",       round(opt$z_low, 2)),
             paste0("High z=",      round(opt$z_high, 2)),
             paste0("Deep-Back z=", round(opt$z_deep_high, 2))
           ),
           col = c("#2C3E50", "#1ABC9C", "#E67E22", "#C0392B"),
           lwd = 1.5, lty = 2, cex = 0.7, bty = "n")

    dev.off()
    cat("Plot saved:", plot_path, "\n")
  }

  list(
    product      = product,
    window       = window,
    q_low        = opt$q_low,
    q_high       = opt$q_high,
    q_deep_low   = opt$q_deep_low,
    q_deep_high  = opt$q_deep_high,
    z_low        = opt$z_low,
    z_high       = opt$z_high,
    z_deep_low   = opt$z_deep_low,
    z_deep_high  = opt$z_deep_high,
    threshold_table = thresh_dt[order(bic)]
  )
}

# ═════════════════════════════════════════════════════════════════════════════
# LOOKBACK LAG SELECTOR — finds optimal lag between current bar and
# the start of the baseline window via BIC
# ═════════════════════════════════════════════════════════════════════════════
#
# The lag ensures z-scores are computed against a pre-current-regime baseline.
# A lag of 63 days means: "compare today against what the market looked like
# 3 months ago through (3 + window) months ago". This prevents a sustained
# crisis from inflating its own baseline mean.
#
# Usage:
#   lag_result <- select_lookback_lag(product = "LGO", window = 252)
#   cat("Optimal lag:", lag_result$optimal_lag, "days\n")

CANDIDATE_LAGS <- c(0, 21, 42, 63, 84, 126)
# 0   = no lag (current rolling, original behaviour)
# 21  = 1 month lag
# 42  = 2 month lag
# 63  = 3 month lag (default)
# 84  = 4 month lag
# 126 = 6 month lag

# ── BIC for a given lag ───────────────────────────────────────────────────────

.compute_bic_lag <- function(y, window_size, lag,
                              z_high = 0.5, z_low = -0.5) {
  n         <- length(y)
  roll_mean <- rep(NA_real_, n)
  roll_sd   <- rep(NA_real_, n)

  if (lag == 0) {
    roll_mean <- as.numeric(zoo::rollmean(y, window_size, fill = NA, align = "right"))
    roll_sd   <- as.numeric(zoo::rollapply(y, window_size, sd, fill = NA, align = "right"))
  } else {
    y_lagged  <- c(rep(NA_real_, lag), y[1:(n - lag)])
    roll_mean <- as.numeric(zoo::rollmean(y_lagged, window_size, fill = NA, align = "right"))
    roll_sd   <- as.numeric(zoo::rollapply(y_lagged, window_size, sd, fill = NA, align = "right"))
  }
  for (i in which(is.na(roll_mean))) {
    avail     <- if (lag == 0) y[1:i] else y[1:max(1, i - lag)]
    roll_mean[i] <- mean(avail, na.rm = TRUE)
    roll_sd[i]   <- if (length(avail) > 1) sd(avail, na.rm = TRUE) else 0
  }

  roll_sd <- pmax(roll_sd, 1e-6)
  lz      <- (y - roll_mean) / roll_sd

  tier <- ifelse(lz >= z_high, "high",
          ifelse(lz <= z_low,  "low", "mid"))

  tier_rle   <- rle(tier)
  n_segments <- length(tier_rle$lengths)
  seg_idx    <- rep(seq_len(n_segments), tier_rle$lengths)
  seg_means  <- tapply(y, seg_idx, mean, na.rm = TRUE)
  fitted     <- seg_means[seg_idx]
  rss        <- sum((y - fitted)^2, na.rm = TRUE)
  k          <- 2 * n_segments
  bic        <- n * log(rss / n) + k * log(n)

  # F-statistic
  grand_mean <- mean(y, na.rm = TRUE)
  ss_between <- sum(tier_rle$lengths * (seg_means - grand_mean)^2, na.rm = TRUE)
  df_between <- n_segments - 1
  df_within  <- n - n_segments
  f_stat     <- if (df_within > 0 && rss > 0)
                  (ss_between / df_between) / (rss / df_within)
                else NA_real_

  # Also compute mean absolute z-score during known break periods
  # (higher = baseline is correctly distant from the current regime)
  mean_abs_z <- mean(abs(lz), na.rm = TRUE)

  list(
    lag            = lag,
    bic            = round(bic, 2),
    n_segments     = n_segments,
    f_stat         = round(f_stat, 3),
    mean_abs_z     = round(mean_abs_z, 4)
  )
}

# ── Main lag selection function ───────────────────────────────────────────────

select_lookback_lag <- function(product    = "CL",
                                 output_dir = "output",
                                 window     = NULL,
                                 lags       = CANDIDATE_LAGS,
                                 plot       = TRUE) {

  cat("\n", strrep("=", 60), "\n")
  cat("LOOKBACK LAG SELECTOR —", product, "\n")
  cat(strrep("=", 60), "\n\n")

  signals_path <- file.path(output_dir, "model_signals.rds")
  if (!file.exists(signals_path))
    stop("model_signals.rds not found. Run run_parallel_models() first.")

  signals <- readRDS(signals_path)
  y       <- as.numeric(signals$M1M2)
  y       <- y[!is.na(y)]
  n       <- length(y)

  # Get or auto-select window
  if (is.null(window)) {
    if (exists("select_level_z_window", mode = "function")) {
      w_res  <- select_level_z_window(product = product,
                                       output_dir = output_dir,
                                       windows = CANDIDATE_WINDOWS_EXTENDED,
                                       plot = FALSE)
      window <- w_res$optimal_window
    } else {
      window <- 126
    }
  }
  cat("Using window:", window, "days\n")
  cat("Testing", length(lags), "lag values:", paste(lags, collapse = ", "), "\n\n")

  # Run BIC for each lag
  results <- lapply(lags, function(lag) .compute_bic_lag(y, window, lag))
  lag_dt  <- rbindlist(results)
  lag_dt[, delta_bic := round(bic - min(bic), 2)]

  # Elbow detection (same logic as window selector)
  lag_ordered  <- lag_dt[order(lag)]
  bic_vals_ord <- lag_ordered$bic
  n_l          <- nrow(lag_ordered)

  if (n_l >= 3) {
    second_deriv <- c(NA, diff(bic_vals_ord, differences = 2), NA)
    lag_dt[order(lag), second_deriv := second_deriv]

    bic_diffs       <- diff(bic_vals_ord)
    is_monotone     <- all(bic_diffs < 0)
    raw_min_lag     <- lag_dt[which.min(bic), lag]
    raw_min_at_edge <- raw_min_lag == max(lag_dt$lag)

    if (is_monotone || raw_min_at_edge) {
      elbow_idx    <- which.max(second_deriv[!is.na(second_deriv)])
      optimal_lag  <- lag_ordered$lag[elbow_idx + 1]
      sel_method   <- "ELBOW (BIC monotone)"
    } else {
      optimal_lag  <- raw_min_lag
      sel_method   <- "MINIMUM BIC"
    }
  } else {
    optimal_lag  <- lag_dt[which.min(bic), lag]
    sel_method   <- "MINIMUM BIC"
    lag_dt[, second_deriv := NA_real_]
  }

  lag_dt[, selected := ifelse(lag == optimal_lag, "<<< OPTIMAL", "")]

  cat("--- LAG BIC RESULTS ---\n\n")
  print(lag_dt[order(lag), .(lag, bic, delta_bic, second_deriv,
                               n_segments, f_stat, mean_abs_z, selected)])

  cat("\nSelection method:", sel_method, "\n")
  cat("Optimal lag:     ", optimal_lag, "days (",
      round(optimal_lag / 21, 1), "months )\n")

  # Plot
  if (plot) {
    plot_path <- file.path(output_dir,
                           paste0("lag_selection_", product, ".png"))
    png(plot_path, width = 1200, height = 500, res = 120)
    par(mfrow = c(1, 2), mar = c(4, 4.5, 3, 2), bg = "white")

    l_vals   <- lag_dt[order(lag), lag]
    bic_vals <- lag_dt[order(lag), bic]

    plot(l_vals, bic_vals, type = "b", pch = 19,
         col = ifelse(l_vals == optimal_lag, "#C0392B", "#185FA5"),
         cex = ifelse(l_vals == optimal_lag, 1.6, 0.9),
         lwd = 1.5,
         main = paste0(product, " — BIC by lookback lag"),
         xlab = "Lag (days)", ylab = "BIC (lower = better)",
         xaxt = "n", las = 1, cex.main = 0.9)
    axis(1, at = l_vals,
         labels = paste0(l_vals, "d\n(", round(l_vals/21,1), "mo)"),
         cex.axis = 0.7)
    abline(v = optimal_lag, col = "#C0392B", lty = 2, lwd = 1.2)
    text(optimal_lag, max(bic_vals),
         paste0("Optimal:\n", optimal_lag, "d"),
         col = "#C0392B", cex = 0.7, adj = c(-0.1, 1))

    # F-stat and mean_abs_z
    f_vals <- lag_dt[order(lag), f_stat]
    z_vals <- lag_dt[order(lag), mean_abs_z]
    plot(l_vals, f_vals, type = "b", pch = 19, col = "#0F6E56",
         lwd = 1.5,
         main = paste0(product, " — F-stat & mean |z| by lag"),
         xlab = "Lag (days)", ylab = "F-statistic",
         xaxt = "n", las = 1, cex.main = 0.9)
    axis(1, at = l_vals,
         labels = paste0(l_vals, "d\n(", round(l_vals/21,1), "mo)"),
         cex.axis = 0.7)
    par(new = TRUE)
    plot(l_vals, z_vals, type = "b", pch = 17, col = "#E67E22",
         axes = FALSE, xlab = "", ylab = "", lwd = 1.5)
    axis(4, col = "#E67E22", col.axis = "#E67E22", las = 1, cex.axis = 0.8)
    mtext("Mean |z-score|", side = 4, line = 2.5, col = "#E67E22", cex = 0.8)
    legend("topright",
           legend = c("F-statistic", "Mean |z|"),
           col = c("#0F6E56", "#E67E22"), pch = c(19, 17),
           lwd = 1.5, cex = 0.7, bty = "n")
    abline(v = optimal_lag, col = "#C0392B", lty = 2, lwd = 0.8)

    dev.off()
    cat("Plot saved:", plot_path, "\n")
  }

  list(
    product          = product,
    window           = window,
    lag_table        = lag_dt[order(lag)],
    optimal_lag      = optimal_lag,
    selection_method = sel_method
  )
}