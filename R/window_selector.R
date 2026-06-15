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
#   result <- select_level_z_window(product = "CL")
#   print(result$bic_table)
#   cat("Optimal window:", result$optimal_window, "days\n")
#
#   all_windows <- select_window_all_products(c("CL","LCO","HO","LGO"))
#   print(all_windows)

library(data.table)
library(zoo)

# ── Candidate window sizes (trading days) ─────────────────────────────────────
CANDIDATE_WINDOWS          <- c(21, 42, 63, 84, 126, 168, 252)
CANDIDATE_WINDOWS_EXTENDED <- c(21, 42, 63, 84, 126, 168, 252, 315, 378)

# Z-score thresholds for tier assignment (same as classifier)
LEVEL_Z_HIGH <-  0.5
LEVEL_Z_LOW  <- -0.5

# ── Candidate lags (trading days) ─────────────────────────────────────────────
# Minimum lag is 63 days (3 months).
# Rationale: with a 252-day window, a lag of 21 days allows crisis prices
# to contaminate the baseline window within 21 bars of a regime shift.
# At lag=63, the baseline window stays entirely pre-crisis for the first
# 63 days of any sustained shock — sufficient for the LGO 2022 case.
# Lags below 63 are excluded as structurally inadequate.
CANDIDATE_LAGS <- c(63, 84, 126)

# ── BIC calculation for a given window ───────────────────────────────────────

.compute_bic <- function(y, window_size) {
  n <- length(y)

  roll_mean <- zoo::rollmean(y, window_size, fill = NA, align = "right")
  roll_sd   <- zoo::rollapply(y, window_size, sd, fill = NA, align = "right")

  for (i in which(is.na(roll_mean))) {
    roll_mean[i] <- mean(y[1:i], na.rm = TRUE)
    roll_sd[i]   <- if (i > 1) sd(y[1:i], na.rm = TRUE) else 0
  }
  roll_sd <- pmax(roll_sd, 1e-6)

  lz <- (y - roll_mean) / roll_sd

  tier <- ifelse(lz >= LEVEL_Z_HIGH, "high",
          ifelse(lz <= LEVEL_Z_LOW,  "low", "mid"))

  tier_rle   <- rle(tier)
  n_segments <- length(tier_rle$lengths)
  seg_idx    <- rep(seq_len(n_segments), tier_rle$lengths)
  seg_means  <- tapply(y, seg_idx, mean, na.rm = TRUE)
  fitted     <- seg_means[seg_idx]
  rss        <- sum((y - fitted)^2, na.rm = TRUE)
  k          <- 2 * n_segments
  bic        <- n * log(rss / n) + k * log(n)

  grand_mean      <- mean(y, na.rm = TRUE)
  ss_between      <- sum(tier_rle$lengths * (seg_means - grand_mean)^2, na.rm = TRUE)
  df_between      <- n_segments - 1
  df_within       <- n - n_segments
  f_stat          <- if (df_within > 0 && rss > 0)
                       (ss_between / df_between) / (rss / df_within)
                     else NA_real_
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

  signals_path <- file.path(output_dir, "model_signals.rds")
  if (!file.exists(signals_path))
    stop("model_signals.rds not found in ", output_dir,
         "\nRun run_parallel_models() first.")

  signals <- readRDS(signals_path)
  y       <- as.numeric(signals$M1M2)
  y       <- y[!is.na(y)]
  n       <- length(y)

  cat("Series length:", n, "bars\n")
  cat("M1M2 range:   ", round(min(y), 3), "to", round(max(y), 3), "\n\n")
  cat("Testing", length(windows), "window sizes:", paste(windows, collapse = ", "), "\n\n")

  results <- lapply(windows, function(w) .compute_bic(y, w))
  bic_dt  <- rbindlist(results)

  bic_dt[, bic_rank  := rank(bic)]
  bic_dt[, delta_bic := round(bic - min(bic), 2)]

  bic_ordered  <- bic_dt[order(window)]
  bic_vals_ord <- bic_ordered$bic
  n_w          <- nrow(bic_ordered)

  if (n_w >= 3) {
    second_deriv <- c(NA, diff(bic_vals_ord, differences = 2), NA)
    bic_dt[order(window), second_deriv := second_deriv]

    elbow_idx    <- which.max(second_deriv[!is.na(second_deriv)])
    elbow_window <- bic_ordered$window[elbow_idx + 1]

    bic_diffs       <- diff(bic_vals_ord)
    is_monotone     <- all(bic_diffs < 0)
    raw_min_window  <- bic_dt[which.min(bic), window]
    raw_min_at_edge <- raw_min_window == max(bic_dt$window)

    if (is_monotone || raw_min_at_edge) {
      optimal_window   <- elbow_window
      selection_method <- "ELBOW (BIC monotone — raw minimum avoided)"
    } else {
      optimal_window   <- raw_min_window
      selection_method <- "MINIMUM BIC (genuine interior minimum found)"
    }
  } else {
    optimal_window   <- bic_dt[which.min(bic), window]
    selection_method <- "MINIMUM BIC (insufficient points for elbow)"
    bic_dt[, second_deriv := NA_real_]
  }

  bic_dt[, selected := ifelse(window == optimal_window, "<<< OPTIMAL", "")]

  cat("--- BIC RESULTS ---\n\n")
  print(bic_dt[order(window), .(window, bic, delta_bic, second_deriv,
                                 n_segments, f_stat, selected)])

  cat("\nSelection method:", selection_method, "\n")
  cat("Optimal window:  ", optimal_window, "days (",
      round(optimal_window / 21, 1), "months )\n")

  if (plot) {
    plot_path <- file.path(output_dir,
                           paste0("window_selection_", product, ".png"))
    png(plot_path, width = 1600, height = 600, res = 120)
    par(mfrow = c(1, 3), mar = c(4, 4.5, 3, 2), bg = "white")

    bic_vals <- bic_dt[order(window), bic]
    w_vals   <- bic_dt[order(window), window]
    sd_vals  <- bic_dt[order(window), second_deriv]
    raw_min_w <- bic_dt[which.min(bic), window]

    pt_col <- ifelse(w_vals == optimal_window, "#C0392B",
              ifelse(w_vals == raw_min_w,       "#185FA5", "gray50"))
    pt_cex <- ifelse(w_vals %in% c(optimal_window, raw_min_w), 1.6, 0.9)

    plot(w_vals, bic_vals, type = "b", pch = 19,
         col = pt_col, cex = pt_cex, lwd = 1.5,
         main = paste0(product, " — BIC by window size"),
         xlab = "Window (days)", ylab = "BIC (lower = better)",
         xaxt = "n", las = 1, cex.main = 0.9)
    axis(1, at = w_vals,
         labels = paste0(w_vals, "\n(", round(w_vals/21, 1), "mo)"),
         cex.axis = 0.65)
    abline(v = optimal_window, col = "#C0392B", lty = 2, lwd = 1.2)
    if (raw_min_w != optimal_window)
      abline(v = raw_min_w, col = "#185FA5", lty = 3, lwd = 0.8)
    text(optimal_window, max(bic_vals),
         paste0("Elbow:\n", optimal_window, "d"),
         col = "#C0392B", cex = 0.65, adj = c(-0.1, 1))

    par(mar = c(4, 4.5, 3, 4))
    seg_vals <- bic_dt[order(window), n_segments]
    f_vals   <- bic_dt[order(window), f_stat]
    plot(w_vals, seg_vals, type = "b", pch = 19, col = "#0F6E56",
         lwd = 1.5, main = paste0(product, " — Segments & F-stat by window"),
         xlab = "Window (days)", ylab = "N segments",
         xaxt = "n", las = 1, cex.main = 0.9)
    axis(1, at = w_vals,
         labels = paste0(w_vals, "\n(", round(w_vals/21, 1), "mo)"),
         cex.axis = 0.7)
    par(new = TRUE)
    plot(w_vals, f_vals, type = "b", pch = 17, col = "#E67E22",
         axes = FALSE, xlab = "", ylab = "", lwd = 1.5)
    axis(4, col = "#E67E22", col.axis = "#E67E22", las = 1, cex.axis = 0.8)
    mtext("F-statistic", side = 4, line = 2.5, col = "#E67E22", cex = 0.8)
    legend("topright", legend = c("N segments", "F-statistic"),
           col = c("#0F6E56", "#E67E22"), pch = c(19, 17),
           lwd = 1.5, cex = 0.7, bty = "n")

    sd_plot <- ifelse(is.na(sd_vals), 0, sd_vals)
    bar_col  <- ifelse(w_vals == optimal_window, "#C0392B", "gray60")
    barplot(sd_plot, names.arg = paste0(w_vals, "d"),
            col = bar_col, border = NA,
            main = paste0(product, " — BIC 2nd derivative (elbow)"),
            xlab = "Window", ylab = "2nd derivative",
            las = 2, cex.names = 0.65, cex.main = 0.9)
    abline(h = 0, col = "gray40", lwd = 0.5)

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

# ── Run for all products ──────────────────────────────────────────────────────

select_window_all_products <- function(products   = c("CL", "LCO", "HO", "LGO"),
                                        output_dir = "output") {

  cat("\n", strrep("=", 60), "\n")
  cat("WINDOW SELECTION — ALL PRODUCTS\n")
  cat(strrep("=", 60), "\n\n")

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
# THRESHOLD SELECTOR
# ═════════════════════════════════════════════════════════════════════════════

TIER_QUANTILE_GRID <- list(
  c(0.20, 0.80), c(0.25, 0.75), c(0.33, 0.67),
  c(0.40, 0.60), c(0.15, 0.85)
)
DEEP_QUANTILE_GRID <- c(0.05, 0.10, 0.15)

.compute_bic_thresholds <- function(y, lz, q_low, q_high,
                                     q_deep_low, q_deep_high) {
  n           <- length(y)
  z_low       <- quantile(lz, q_low,       na.rm = TRUE)
  z_high      <- quantile(lz, q_high,      na.rm = TRUE)
  z_deep_low  <- quantile(lz, q_deep_low,  na.rm = TRUE)
  z_deep_high <- quantile(lz, q_deep_high, na.rm = TRUE)

  tier <- ifelse(lz >= z_deep_high, "deep_high",
          ifelse(lz >= z_high,      "high",
          ifelse(lz <= z_deep_low,  "deep_low",
          ifelse(lz <= z_low,       "low", "mid"))))

  tier_rle   <- rle(tier)
  n_segments <- length(tier_rle$lengths)
  seg_idx    <- rep(seq_len(n_segments), tier_rle$lengths)
  seg_means  <- tapply(y, seg_idx, mean, na.rm = TRUE)
  fitted     <- seg_means[seg_idx]
  rss        <- sum((y - fitted)^2, na.rm = TRUE)
  k          <- 2 * n_segments
  bic        <- n * log(rss / n) + k * log(n)

  grand_mean <- mean(y, na.rm = TRUE)
  ss_between <- sum(tier_rle$lengths * (seg_means - grand_mean)^2, na.rm = TRUE)
  df_between <- n_segments - 1
  df_within  <- n - n_segments
  f_stat     <- if (df_within > 0 && rss > 0)
                  (ss_between / df_between) / (rss / df_within)
                else NA_real_

  list(
    q_low = q_low, q_high = q_high,
    q_deep_low = q_deep_low, q_deep_high = q_deep_high,
    z_low = round(z_low, 4), z_high = round(z_high, 4),
    z_deep_low = round(z_deep_low, 4), z_deep_high = round(z_deep_high, 4),
    bic = round(bic, 2), n_segments = n_segments, f_stat = round(f_stat, 3)
  )
}

select_thresholds <- function(product    = "CL",
                               output_dir = "output",
                               window     = NULL,
                               plot       = TRUE) {

  cat("\n", strrep("=", 60), "\n")
  cat("THRESHOLD SELECTOR —", product, "\n")
  cat(strrep("=", 60), "\n\n")

  signals_path <- file.path(output_dir, "model_signals.rds")
  if (!file.exists(signals_path))
    stop("model_signals.rds not found. Run run_parallel_models() first.")

  signals <- readRDS(signals_path)
  y       <- as.numeric(signals$M1M2)
  y       <- y[!is.na(y)]

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

  roll_mean <- zoo::rollmean(y, window, fill = NA, align = "right")
  roll_sd   <- zoo::rollapply(y, window, sd, fill = NA, align = "right")
  for (i in which(is.na(roll_mean))) {
    roll_mean[i] <- mean(y[1:i], na.rm = TRUE)
    roll_sd[i]   <- if (i > 1) sd(y[1:i], na.rm = TRUE) else 0
  }
  roll_sd <- pmax(roll_sd, 1e-6)
  lz      <- (y - roll_mean) / roll_sd

  cat("Level z-score distribution:\n")
  cat("  Mean:", round(mean(lz, na.rm = TRUE), 3),
      "| SD:", round(sd(lz, na.rm = TRUE), 3),
      "| Skew:", round(mean((lz - mean(lz, na.rm = TRUE))^3, na.rm = TRUE) /
                       sd(lz, na.rm = TRUE)^3, 3),
      "| Kurt:", round(mean((lz - mean(lz, na.rm = TRUE))^4, na.rm = TRUE) /
                       sd(lz, na.rm = TRUE)^4, 3), "\n\n")

  cat("Testing", length(TIER_QUANTILE_GRID), "x",
      length(DEEP_QUANTILE_GRID), "=",
      length(TIER_QUANTILE_GRID) * length(DEEP_QUANTILE_GRID),
      "threshold combinations...\n\n")

  results <- list()
  for (tq in TIER_QUANTILE_GRID) {
    for (dq in DEEP_QUANTILE_GRID) {
      if (dq >= tq[1]) next
      res <- .compute_bic_thresholds(y, lz,
               q_low = tq[1], q_high = tq[2],
               q_deep_low = dq, q_deep_high = 1 - dq)
      results <- c(results, list(res))
    }
  }

  thresh_dt <- rbindlist(results)
  thresh_dt[, delta_bic := round(bic - min(bic), 2)]
  thresh_dt[, selected  := ifelse(bic == min(bic), "<<< OPTIMAL", "")]
  opt <- thresh_dt[which.min(bic)]

  cat("--- THRESHOLD BIC RESULTS (top 10) ---\n\n")
  print(thresh_dt[order(bic)][1:min(10, .N),
        .(q_low, q_high, q_deep_low, q_deep_high,
          z_low, z_high, z_deep_low, z_deep_high,
          bic, delta_bic, n_segments, f_stat, selected)])

  cat("\n--- OPTIMAL THRESHOLDS:", product, "---\n\n")
  cat("  Tier split : q_low =", opt$q_low, "| q_high =", opt$q_high, "\n")
  cat("  Deep tier  : q_deep =", opt$q_deep_low,
      "/ q_deep_high =", opt$q_deep_high, "\n")
  cat("  Z-score values:\n")
  cat("    Deep-Contango  below z =", opt$z_deep_low,  "\n")
  cat("    Low tier       below z =", opt$z_low,        "\n")
  cat("    High tier      above z =", opt$z_high,       "\n")
  cat("    Deep-Backw.    above z =", opt$z_deep_high,  "\n\n")

  if (plot) {
    plot_path <- file.path(output_dir,
                           paste0("threshold_selection_", product, ".png"))
    png(plot_path, width = 1400, height = 600, res = 120)
    par(mfrow = c(1, 2), mar = c(4, 4.5, 3, 2), bg = "white")

    deep_cols <- c("0.05" = "#C0392B", "0.1" = "#185FA5", "0.15" = "#0F6E56")
    plot(NULL,
         xlim = range(thresh_dt$q_low), ylim = range(thresh_dt$bic),
         main = paste0(product, " — Threshold BIC by tier split"),
         xlab = "Low quantile (high = 1 - low)",
         ylab = "BIC (lower = better)", las = 1, cex.main = 0.9)
    for (dq in DEEP_QUANTILE_GRID) {
      sub <- thresh_dt[q_deep_low == dq][order(q_low)]
      if (nrow(sub) == 0) next
      lines(sub$q_low, sub$bic, col = deep_cols[as.character(dq)], lwd = 1.5)
      points(sub$q_low, sub$bic, col = deep_cols[as.character(dq)],
             pch = 19, cex = 0.9)
    }
    points(opt$q_low, opt$bic, col = "#C0392B", pch = 8, cex = 2, lwd = 2)
    legend("topright",
           legend = paste0("deep q=", DEEP_QUANTILE_GRID),
           col = unname(deep_cols), lwd = 1.5, pch = 19, cex = 0.7, bty = "n")

    hist(lz, breaks = 50, col = "gray85", border = "white",
         main = paste0(product, " — z-score distribution & thresholds"),
         xlab = "Level z-score", ylab = "Frequency",
         las = 1, cex.main = 0.9)
    abline(v = c(opt$z_deep_low, opt$z_low, opt$z_high, opt$z_deep_high),
           col = c("#2C3E50", "#1ABC9C", "#E67E22", "#C0392B"),
           lwd = c(1.5, 1.2, 1.2, 1.5), lty = c(2, 2, 2, 2))
    legend("topright",
           legend = c(
             paste0("Deep-Cont z=", round(opt$z_deep_low,  2)),
             paste0("Low z=",       round(opt$z_low,        2)),
             paste0("High z=",      round(opt$z_high,       2)),
             paste0("Deep-Back z=", round(opt$z_deep_high,  2))
           ),
           col = c("#2C3E50", "#1ABC9C", "#E67E22", "#C0392B"),
           lwd = 1.5, lty = 2, cex = 0.7, bty = "n")

    dev.off()
    cat("Plot saved:", plot_path, "\n")
  }

  list(
    product = product, window = window,
    q_low = opt$q_low, q_high = opt$q_high,
    q_deep_low = opt$q_deep_low, q_deep_high = opt$q_deep_high,
    z_low = opt$z_low, z_high = opt$z_high,
    z_deep_low = opt$z_deep_low, z_deep_high = opt$z_deep_high,
    threshold_table = thresh_dt[order(bic)]
  )
}

# ═════════════════════════════════════════════════════════════════════════════
# LOOKBACK LAG SELECTOR
# ═════════════════════════════════════════════════════════════════════════════
#
# Selects the optimal lag between the current bar and the start of the
# baseline window, using F-statistic maximisation on the Z-SCORE series.
#
# THREE DESIGN DECISIONS:
#
# 1. MINIMUM LAG = 63 DAYS
#    With a 252-day window, lag=21 allows crisis prices into the baseline
#    within 21 bars of a regime shift. At lag=63, the window stays entirely
#    pre-crisis for the first 63 days of any sustained shock. Shorter lags
#    are excluded as structurally inadequate for multi-month energy crises.
#
# 2. F-STATISTIC ON Z-SCORES (not raw M1M2)
#    The F-statistic measures between-regime separation / within-regime
#    homogeneity. Computing it on raw M1M2 rewards tighter price clustering
#    regardless of whether the z-score correctly flags crisis periods.
#    Computing it on lz (the z-score series) directly measures what we care
#    about: do crisis bars produce consistently large z-scores, and do normal
#    bars produce consistently small z-scores?
#
# 3. GLOBAL_SD FLOOR (not 1e-6)
#    Early bars (i < lag) have 0 or 1 observations in the baseline window,
#    producing sd = 0. A 1e-6 floor causes z-scores in the millions for
#    M1M2 values of O(5-50). Using 0.1 * global_sd keeps early-bar z-scores
#    in a sensible range. These bars are excluded by the warm-up mask in
#    the classifier anyway, so this is purely numerical stability.

.compute_bic_lag <- function(y, window_size, lag,
                              z_high = 0.5, z_low = -0.5) {
  n <- length(y)

  # ── Global sd floor — prevents z blow-up on early bars ───────────────────
  global_sd <- sd(y, na.rm = TRUE)
  sd_floor  <- 0.1 * global_sd

  # ── Rolling mean and sd on lagged series ─────────────────────────────────
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

  # Fill early NAs with expanding window on the lagged series
  for (i in which(is.na(roll_mean))) {
    avail        <- if (lag == 0) y[1:i] else y[1:max(1L, i - lag)]
    roll_mean[i] <- mean(avail, na.rm = TRUE)
    roll_sd[i]   <- if (length(avail) > 1L) sd(avail, na.rm = TRUE) else 0
  }

  # Apply global_sd floor
  roll_sd <- pmax(roll_sd, sd_floor)

  # Z-score series
  lz <- (y - roll_mean) / roll_sd

  # Tier assignment on z-scores
  tier <- ifelse(lz >= z_high, "high",
          ifelse(lz <= z_low,  "low", "mid"))

  tier_rle   <- rle(tier)
  n_segments <- length(tier_rle$lengths)
  seg_idx    <- rep(seq_len(n_segments), tier_rle$lengths)

  # ── BIC on raw M1M2 (kept for reference / diagnostics) ───────────────────
  seg_means_y <- tapply(y, seg_idx, mean, na.rm = TRUE)
  fitted_y    <- seg_means_y[seg_idx]
  rss_y       <- sum((y - fitted_y)^2, na.rm = TRUE)
  k           <- 2 * n_segments
  bic         <- n * log(rss_y / n) + k * log(n)

  # ── F-STATISTIC ON Z-SCORES (the actual selection criterion) ─────────────
  # Measures: are crisis z-scores consistently large (between-regime high)?
  #           are normal-period z-scores consistently small (within-regime low)?
  # A longer lag pushes the baseline pre-crisis → larger crisis z-scores →
  # better between-regime separation on lz → higher F on lz.
  seg_means_z <- tapply(lz, seg_idx, mean, na.rm = TRUE)
  fitted_z    <- seg_means_z[seg_idx]
  rss_z       <- sum((lz - fitted_z)^2, na.rm = TRUE)
  grand_z     <- mean(lz, na.rm = TRUE)
  lengths     <- as.numeric(tier_rle$lengths)
  ss_between  <- sum(lengths * (seg_means_z - grand_z)^2, na.rm = TRUE)
  df_between  <- n_segments - 1
  df_within   <- n - n_segments
  f_stat      <- if (df_within > 0 && rss_z > 0)
                   (ss_between / df_between) / (rss_z / df_within)
                 else NA_real_

  mean_abs_z  <- mean(abs(lz), na.rm = TRUE)

  list(
    lag        = lag,
    bic        = round(bic, 2),
    n_segments = n_segments,
    f_stat     = round(f_stat, 3),
    mean_abs_z = round(mean_abs_z, 4)
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

  if (is.null(window)) {
    if (exists("select_level_z_window", mode = "function")) {
      w_res  <- select_level_z_window(product    = product,
                                       output_dir = output_dir,
                                       windows    = CANDIDATE_WINDOWS_EXTENDED,
                                       plot       = FALSE)
      window <- w_res$optimal_window
    } else {
      window <- 126
    }
  }

  cat("Using window:", window, "days\n")
  cat("Candidate lags:", paste(lags, collapse = ", "),
      "(minimum 63 days enforced)\n\n")

  if (min(lags) < 63) {
    warning("Lags below 63 days excluded — insufficient separation for ",
            "252-day window against multi-month energy crises.")
    lags <- lags[lags >= 63]
  }

  results <- lapply(lags, function(lag) .compute_bic_lag(y, window, lag))
  lag_dt  <- rbindlist(results)
  lag_dt[, delta_bic := round(bic - min(bic), 2)]

  # Second derivative of BIC (reference only — not used for selection)
  lag_ordered  <- lag_dt[order(lag)]
  bic_vals_ord <- lag_ordered$bic
  if (nrow(lag_ordered) >= 3) {
    second_deriv <- c(NA, diff(bic_vals_ord, differences = 2), NA)
    lag_dt[order(lag), second_deriv := second_deriv]
  } else {
    lag_dt[, second_deriv := NA_real_]
  }

  # ── PRIMARY SELECTION: F-STATISTIC ON Z-SCORES ───────────────────────────
  optimal_lag <- lag_dt[which.max(f_stat), lag]
  sel_method  <- "MAXIMUM F-STATISTIC on z-score series (between-regime separation)"

  lag_dt[, selected := ifelse(lag == optimal_lag, "<<< OPTIMAL", "")]

  cat("--- LAG RESULTS ---\n\n")
  print(lag_dt[order(lag), .(lag, bic, delta_bic, n_segments,
                               f_stat, mean_abs_z, selected)])

  cat("\nSelection method:", sel_method, "\n")
  cat("Optimal lag:     ", optimal_lag, "days (",
      round(optimal_lag / 21, 1), "months )\n")

  # Sanity check
  max_maz <- lag_dt[lag == optimal_lag, mean_abs_z]
  if (!is.na(max_maz) && max_maz > 50) {
    warning(sprintf(
      "mean_abs_z at optimal lag = %.1f — sd floor may still be too small.", max_maz
    ))
  }

  if (plot) {
    plot_path <- file.path(output_dir,
                           paste0("lag_selection_", product, ".png"))
    png(plot_path, width = 1400, height = 500, res = 120)
    par(mfrow = c(1, 3), mar = c(4, 4.5, 3, 2), bg = "white")

    l_vals   <- lag_dt[order(lag), lag]
    bic_vals <- lag_dt[order(lag), bic]
    f_vals   <- lag_dt[order(lag), f_stat]
    z_vals   <- lag_dt[order(lag), mean_abs_z]

    # Panel 1: BIC by lag (reference only)
    plot(l_vals, bic_vals, type = "b", pch = 19, col = "gray50",
         cex = 0.9, lwd = 1.5,
         main = paste0(product, " — BIC by lag (reference only)"),
         xlab = "Lag (days)", ylab = "BIC",
         xaxt = "n", las = 1, cex.main = 0.9)
    axis(1, at = l_vals,
         labels = paste0(l_vals, "d\n(", round(l_vals/21, 1), "mo)"),
         cex.axis = 0.7)
    mtext("F-statistic on z-scores used for selection",
          side = 3, line = 0, cex = 0.6, col = "gray50")

    # Panel 2: F-statistic on z-scores (selection criterion)
    plot(l_vals, f_vals, type = "b", pch = 19,
         col = ifelse(l_vals == optimal_lag, "#C0392B", "#0F6E56"),
         cex = ifelse(l_vals == optimal_lag, 1.8, 0.9),
         lwd = 1.5,
         main = paste0(product, " — F-stat on z-scores (SELECTION CRITERION)"),
         xlab = "Lag (days)", ylab = "F-statistic on lz (higher = better)",
         xaxt = "n", las = 1, cex.main = 0.9)
    axis(1, at = l_vals,
         labels = paste0(l_vals, "d\n(", round(l_vals/21, 1), "mo)"),
         cex.axis = 0.7)
    abline(v = optimal_lag, col = "#C0392B", lty = 2, lwd = 1.2)
    text(optimal_lag, min(f_vals),
         paste0("Optimal:\n", optimal_lag, "d"),
         col = "#C0392B", cex = 0.7, adj = c(-0.1, 0))

    # Panel 3: Mean |z| by lag (diagnostic — should increase monotonically)
    plot(l_vals, z_vals, type = "b", pch = 19,
         col = ifelse(l_vals == optimal_lag, "#C0392B", "#E67E22"),
         cex = ifelse(l_vals == optimal_lag, 1.8, 0.9),
         lwd = 1.5,
         main = paste0(product, " — Mean |z-score| by lag (diagnostic)"),
         xlab = "Lag (days)", ylab = "Mean |z-score|",
         xaxt = "n", las = 1, cex.main = 0.9)
    axis(1, at = l_vals,
         labels = paste0(l_vals, "d\n(", round(l_vals/21, 1), "mo)"),
         cex.axis = 0.7)
    mtext("Should increase monotonically — confirms sd fix is working",
          side = 3, line = 0, cex = 0.6, col = "gray50")
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