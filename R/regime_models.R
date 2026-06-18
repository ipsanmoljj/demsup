# R/regime_models.R
# -----------------
# Parallel regime classification using 4 independent methods:
#   A: Bai-Perron structural breaks (already computed)
#   B: Kalman filter time-varying mean + deviations
#   C: Markov switching (2 or 3 states)
#   D: ARIMA residual z-scores
#
# Output: consensus matrix showing which models agree at each break date
#
# Usage:
#   source("R/futures_reader.R")
#   source("R/structural_breaks.R")
#   source("R/regime_models.R")
#   ff      <- read_futures_csv("CL_data.csv")
#   results <- run_break_detection(ff, resample_to = "1 day")
#   models  <- run_parallel_models(results$data, results$consensus$high_confidence)
#   print(models$consensus_matrix)

library(data.table)
library(zoo)

# ── Install required packages if missing ──────────────────────────────────────
.install_if_missing <- function(pkgs) {
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    cat("Installing:", paste(missing, collapse = ", "), "\n")
    install.packages(missing, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
}
.install_if_missing(c("KFAS", "MSwM", "forecast", "tseries"))

# ── Main entry point ──────────────────────────────────────────────────────────

run_parallel_models <- function(data,
                                bp_breaks,
                                series    = "M1M2",
                                n_states  = 3,
                                window_days = 21,
                                product   = "CL",
                                output_dir = "output") {
  # data       : data.table from run_break_detection()$data
  # bp_breaks  : high-confidence break dates from Bai-Perron
  # series     : which column to model (default M1M2)
  # n_states   : number of Markov states (2 or 3)
  # window_days: tolerance window for consensus matching (days)
  # product    : product code (CL/LCO/HO/LGO) — used ONLY to namespace the
  #              saved .rds files so different products don't silently
  #              overwrite each other's model outputs. Confirmed bug (2026-06-17):
  #              before this fix, "output/model_kf.rds" etc. were the same five
  #              filenames for every product, so calling this for LCO right after
  #              CL would have classify_regimes("LCO") silently read back CL's
  #              models. See output_dir default in classify_regimes() — the two
  #              MUST agree on the same per-product directory.
  # output_dir : base directory; actual save path is output_dir/product/

  cat("\n", strrep("=", 60), "\n")
  cat("PARALLEL REGIME MODELS —", series, "(", product, ")\n")
  cat(strrep("=", 60), "\n\n")

  y    <- as.numeric(data[[series]])
  ts_  <- as.Date(data$timestamp)
  valid <- !is.na(y)
  y_c  <- y[valid]
  ts_c <- ts_[valid]
  n    <- length(y_c)

  # ── Model B: Kalman filter ─────────────────────────────────────────────────
  cat("Model B: Kalman filter (time-varying mean)...\n")
  kf_result <- .run_kalman(y_c, ts_c)

  # ── Model C: Markov switching ──────────────────────────────────────────────
  cat("Model C: Markov switching (", n_states, "states)...\n")
  ms_result <- .run_markov(y_c, ts_c, n_states)

  # ── Model D: ARIMA residuals ───────────────────────────────────────────────
  cat("Model D: ARIMA residual z-scores...\n")
  ar_result <- .run_arima(y_c, ts_c)

  # ── Combine all signals into one table ────────────────────────────────────
  signals <- .combine_signals(ts_c, y_c, bp_breaks, kf_result, ms_result, ar_result)

  # ── Consensus matrix ───────────────────────────────────────────────────────
  consensus <- .build_consensus_matrix(bp_breaks, signals, window_days)

  cat("\n--- CONSENSUS MATRIX ---\n\n")
  print(consensus)

  # ── Persist model outputs for downstream classifier ─────────────────────
  # Namespaced by product — e.g. output/CL/model_kf.rds, output/LCO/model_kf.rds —
  # so different products' model fits never collide on disk.
  product_dir <- file.path(output_dir, product)
  dir.create(product_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(kf_result,  file.path(product_dir, "model_kf.rds"))
  saveRDS(ms_result,  file.path(product_dir, "model_ms.rds"))
  saveRDS(ar_result,  file.path(product_dir, "model_arima.rds"))
  saveRDS(bp_breaks,  file.path(product_dir, "model_bp_breaks.rds"))
  saveRDS(signals,    file.path(product_dir, "model_signals.rds"))
  cat("\nModel outputs saved to", product_dir, "\n")
  
  list(
    signals          = signals,
    consensus_matrix = consensus,
    kf               = kf_result,
    ms               = ms_result,
    arima            = ar_result,
    bp_breaks        = bp_breaks,
    product          = product
  )
}

# ── Model B: Kalman Filter ────────────────────────────────────────────────────

.run_kalman <- function(y, dates) {
  library(KFAS)

  # Local level model: y_t = mu_t + eps_t, mu_t = mu_{t-1} + eta_t
  # Signal variance (eps) vs state variance (eta) ratio controls smoothness
  model <- SSModel(y ~ SSMtrend(1, Q = list(NA)),
                   H = matrix(NA))

  # Estimate variances by maximum likelihood
  fit <- tryCatch(
    fitSSM(model, inits = c(log(var(y, na.rm=TRUE) * 0.1),
                             log(var(y, na.rm=TRUE))),
           method = "L-BFGS-B"),
    error = function(e) {
      cat("  KF fit warning:", conditionMessage(e), "— using default variances\n")
      list(model = SSModel(y ~ SSMtrend(1, Q = list(var(y)*0.05)),
                           H = matrix(var(y)*0.5)))
    }
  )

  smoothed <- KFS(fit$model)
  mu_hat   <- as.numeric(smoothed$alphahat[, "level"])
  deviation <- y - mu_hat
  dev_sd   <- sd(deviation, na.rm = TRUE)
  dev_z    <- deviation / dev_sd

  # Detect breaks: points where |z| > 2 sustained for > 5 bars
  kf_regime <- ifelse(dev_z > 1.5,  "backwardation",
               ifelse(dev_z < -1.5, "contango", "flat"))

  # Find sustained regime shifts (transition dates)
  regime_rle  <- rle(kf_regime)
  cum_lengths <- cumsum(regime_rle$lengths)
  # Only keep transitions after segments of >= 5 bars
  sustained   <- regime_rle$lengths >= 5
  trans_idx   <- cum_lengths[sustained & c(FALSE, head(sustained, -1))]
  kf_breaks   <- dates[trans_idx[trans_idx <= length(dates)]]

  cat("  KF: found", length(kf_breaks), "sustained regime transitions\n")

  data.table(
    date      = dates,
    kf_mean   = mu_hat,
    kf_dev    = deviation,
    kf_z      = dev_z,
    kf_regime = kf_regime
  )
}

# ── Model C: Markov Switching ─────────────────────────────────────────────────

.run_markov <- function(y, dates, n_states = 3) {
  library(MSwM)

  # ── Normalise y before fitting ───────────────────────────────────────────
  # MSwM numerical optimisation is sensitive to scale.
  # LGO ($/mt range -70 to +170) and LCO cause singular covariance matrices
  # when passed raw. Standardise to mean=0, sd=1 before fitting, then
  # rescale state means back to original units for interpretability.
  y_mean <- mean(y, na.rm = TRUE)
  y_sd   <- sd(y,   na.rm = TRUE)
  if (is.na(y_sd) || y_sd < 1e-8) y_sd <- 1
  y_scaled <- (y - y_mean) / y_sd

  ms_dt <- data.frame(y = y_scaled, t = seq_along(y_scaled))

  # Try requested n_states first, fall back to k=2 if singular
  ms_fit <- NULL
  for (k_try in unique(c(n_states, 2L))) {
    ms_fit <- tryCatch({
      lm_base <- lm(y_scaled ~ 1, data = ms_dt)
      msmFit(lm_base, k = k_try, sw = c(TRUE, TRUE),
             control = list(parallel = FALSE, maxiter = 500, tol = 1e-6))
    }, error = function(e) {
      cat("  MS: k=", k_try, "failed —", conditionMessage(e), "\n")
      NULL
    })
    if (!is.null(ms_fit)) {
      if (k_try != n_states)
        cat("  MS: fell back to k=", k_try, "states\n")
      n_states <- k_try
      break
    }
  }

  if (is.null(ms_fit)) {
    return(data.table(date = dates,
                      ms_state     = NA_integer_,
                      ms_prob_s1   = NA_real_,
                      ms_prob_s2   = NA_real_,
                      ms_prob_s3   = NA_real_,
                      ms_regime    = NA_character_))
  }

  # Extract smoothed state probabilities
  probs     <- ms_fit@Fit@smoProb[-1, , drop = FALSE]
  states    <- apply(probs, 1, which.max)

  # Label states by their mean (lowest mean = contango, highest = backwardation)
  # Rescale state means back to original units
  state_means <- ms_fit@Coef[, "(Intercept)"] * y_sd + y_mean
  state_order <- order(state_means)
  labels      <- character(n_states)
  if (n_states == 2) {
    labels[state_order[1]] <- "contango_flat"
    labels[state_order[2]] <- "backwardation"
  } else {
    labels[state_order[1]] <- "contango"
    labels[state_order[2]] <- "flat"
    labels[state_order[3]] <- "backwardation"
  }

  ms_regime <- labels[states]

  # Find transition dates
  transitions <- which(diff(states) != 0) + 1
  ms_breaks   <- dates[transitions]
  # Filter: only keep transitions that persist for >= 5 days
  persist_idx <- transitions[sapply(transitions, function(i) {
    end_i <- min(i + 4, length(states))
    length(unique(states[i:end_i])) == 1
  })]
  ms_breaks_filtered <- dates[persist_idx]

  cat("  MS:", n_states, "states fitted |",
      length(ms_breaks_filtered), "sustained transitions\n")
  cat("  State means:", round(sort(state_means), 3), "\n")

  result <- data.table(date = dates, ms_state = states, ms_regime = ms_regime)
  for (j in seq_len(ncol(probs))) {
    result[, paste0("ms_prob_s", j) := probs[, j]]
  }
  result
}

# ── Model D: ARIMA residuals ──────────────────────────────────────────────────

.run_arima <- function(y, dates) {
  library(forecast)

  # Auto-select ARIMA order
  fit <- tryCatch(
    auto.arima(y, seasonal = FALSE, stepwise = TRUE,
               approximation = TRUE, max.p = 3, max.q = 3),
    error = function(e) {
      cat("  ARIMA: auto failed, using ARIMA(1,1,1)\n")
      arima(y, order = c(1,1,1))
    }
  )

  cat("  ARIMA order:", paste(fit$arma[c(1,6,2)], collapse=","), "\n")

  resid     <- as.numeric(residuals(fit))
  roll_sd   <- zoo::rollapply(resid, 63, sd, fill = NA, align = "right")
  roll_sd   <- ifelse(is.na(roll_sd), sd(resid, na.rm=TRUE), roll_sd)
  arima_z   <- resid / roll_sd

  # Rolling z-score of y itself (mean reversion signal)
  roll_mean <- zoo::rollmean(y, 63, fill = NA, align = "right")
  roll_sd2  <- zoo::rollapply(y, 63, sd, fill = NA, align = "right")
  level_z   <- (y - roll_mean) / roll_sd2

  arima_regime <- ifelse(level_z > 1.5,  "backwardation",
                  ifelse(level_z < -1.5, "contango", "flat"))

  # Find sustained z-score exceedances as regime transitions
  exceed     <- abs(level_z) > 1.5
  exceed_rle <- rle(exceed & !is.na(exceed))
  trans_ends <- cumsum(exceed_rle$lengths)
  sustained  <- exceed_rle$values & exceed_rle$lengths >= 5
  ar_breaks  <- dates[trans_ends[sustained]]

  cat("  ARIMA: found", length(ar_breaks), "sustained exceedances\n")

  data.table(
    date         = dates,
    arima_resid  = resid,
    arima_z      = arima_z,
    level_z      = level_z,
    arima_regime = arima_regime
  )
}

# ── Combine all signals ───────────────────────────────────────────────────────

.combine_signals <- function(dates, y, bp_breaks, kf, ms, arima) {
  base <- data.table(date = dates, M1M2 = y)

  # BP regime label per date
  bp_regime <- rep(NA_character_, length(dates))
  boundaries <- c(as.Date("2000-01-01"), sort(bp_breaks), as.Date("2100-01-01"))
  for (i in seq_len(length(boundaries) - 1)) {
    idx <- dates > boundaries[i] & dates <= boundaries[i+1]
    seg_mean <- mean(y[idx], na.rm = TRUE)
    bp_regime[idx] <- ifelse(seg_mean > 1.5, "backwardation",
                      ifelse(seg_mean < -0.5, "contango", "flat"))
  }

  result <- cbind(base,
                  data.table(bp_regime = bp_regime),
                  kf[, .(kf_mean, kf_z, kf_regime)],
                  ms[, .(ms_state, ms_regime)],
                  arima[, .(level_z, arima_regime)])

  result[, n_agree := rowSums(cbind(
    bp_regime    == "backwardation",
    kf_regime    == "backwardation",
    ms_regime    == "backwardation",
    arima_regime == "backwardation"
  ), na.rm = TRUE)]

  result
}

# ── Consensus matrix ──────────────────────────────────────────────────────────

.build_consensus_matrix <- function(bp_breaks, signals, window_days = 21) {

  rows <- lapply(seq_along(bp_breaks), function(i) {
    bd  <- bp_breaks[i]
    win <- signals[date >= (bd - window_days) & date <= (bd + window_days)]

    if (nrow(win) == 0) return(NULL)

    # Before vs after regime for each model
    before <- signals[date >= (bd - window_days*2) & date < bd]
    after  <- signals[date >  bd & date <= (bd + window_days*2)]

    if (nrow(before) == 0 || nrow(after) == 0) return(NULL)

    # Did each model show a regime change at this break?
    bp_change <- TRUE   # by definition (this IS a BP break)

    kf_change <- abs(mean(after$kf_z, na.rm=TRUE) -
                     mean(before$kf_z, na.rm=TRUE)) > 0.75

    ms_change <- !is.na(after$ms_state[1]) &&
                 (names(sort(table(after$ms_regime), decreasing=TRUE)[1]) !=
                  names(sort(table(before$ms_regime), decreasing=TRUE)[1]))

    ar_change <- abs(mean(after$level_z, na.rm=TRUE) -
                     mean(before$level_z, na.rm=TRUE)) > 0.75

    # Direction of change
    m1m2_before <- mean(before$M1M2, na.rm=TRUE)
    m1m2_after  <- mean(after$M1M2,  na.rm=TRUE)
    direction   <- ifelse(m1m2_after > m1m2_before, "↑ tighter", "↓ looser")
    magnitude   <- round(m1m2_after - m1m2_before, 3)

    n_models <- sum(c(bp_change, kf_change, ms_change, ar_change))

    confidence <- ifelse(n_models == 4, "VERY HIGH",
                  ifelse(n_models == 3, "HIGH",
                  ifelse(n_models == 2, "MEDIUM", "LOW")))

    data.table(
      break_date  = format(bd),
      BP          = ifelse(bp_change, "YES", "---"),
      KF          = ifelse(kf_change, "YES", "---"),
      MS          = ifelse(ms_change, "YES", "---"),
      ARIMA       = ifelse(ar_change, "YES", "---"),
      n_models    = n_models,
      confidence  = confidence,
      direction   = direction,
      delta_M1M2  = magnitude
    )
  })

  rows <- rows[!sapply(rows, is.null)]
  if (length(rows) == 0) return(data.table())

  result <- rbindlist(rows)

  # Add regime label column
  result[, regime_after := sapply(as.Date(break_date), function(bd) {
    seg <- signals[date > bd & date <= (bd + 60)]
    if (nrow(seg) == 0) return("unknown")
    m <- mean(seg$M1M2, na.rm=TRUE)
    ifelse(m > 1.5, "deep_backwardation",
    ifelse(m > 0.4, "mild_backwardation",
    ifelse(m < -1.5, "deep_contango",
    ifelse(m < -0.2, "mild_contango", "flat"))))
  })]

  result
}

# ── Plot all model signals ────────────────────────────────────────────────────

plot_model_signals <- function(models_result,
                                save_path = "output/regime_model_signals.png") {
  signals <- models_result$signals
  breaks  <- models_result$bp_breaks
  cm      <- models_result$consensus_matrix

  png(save_path, width = 1600, height = 1400, res = 120)
  par(mfrow = c(4, 1), mar = c(2, 4.5, 2.5, 2), oma = c(3, 0, 3, 0), bg = "white")

  times <- as.POSIXct(signals$date)
  bp_v  <- as.POSIXct(breaks)

  # Panel 1: Raw M1M2 + BP breaks
  plot(times, signals$M1M2, type = "l", col = "#185FA5", lwd = 0.7,
       main = "Model A — Bai-Perron: raw M1M2 + break dates",
       xlab = "", ylab = "M1M2 ($/bbl)", xaxt = "n", las = 1, cex.main = 0.9)
  abline(v = bp_v, col = "#E24B4A", lwd = 1.0)
  abline(h = 0, lty = 2, col = "gray60", lwd = 0.4)
  axis.POSIXct(1, at = seq(min(times), max(times), by = "6 months"),
               format = "%b %Y", cex.axis = 0.7, las = 2)

  # Panel 2: Kalman filter mean + deviation z-score
  par(mar = c(2, 4.5, 2.5, 2))
  plot(times, signals$kf_z, type = "l", col = "#0F6E56", lwd = 0.7,
       main = "Model B — Kalman Filter: deviation from time-varying mean (z-score)",
       xlab = "", ylab = "Z-score", xaxt = "n", las = 1, cex.main = 0.9)
  abline(h = c(-1.5, 0, 1.5), lty = c(2,1,2), col = c("gray50","gray70","gray50"), lwd = 0.5)
  abline(v = bp_v, col = "#E24B4A", lwd = 0.8, lty = 2)
  polygon(c(times, rev(times)),
          c(pmax(signals$kf_z, 0), rep(0, nrow(signals))),
          col = adjustcolor("#E24B4A", 0.15), border = NA)
  polygon(c(times, rev(times)),
          c(pmin(signals$kf_z, 0), rep(0, nrow(signals))),
          col = adjustcolor("#185FA5", 0.15), border = NA)
  axis.POSIXct(1, at = seq(min(times), max(times), by = "6 months"),
               format = "%b %Y", cex.axis = 0.7, las = 2)

  # Panel 3: Markov switching state
  if (!all(is.na(signals$ms_state))) {
    state_cols <- c("#E6F1FB", "#EAF3DE", "#FAECE7")
    plot(times, signals$ms_state, type = "s", col = "#6B2D8B", lwd = 1.0,
         main = "Model C — Markov Switching: state classification",
         xlab = "", ylab = "State", xaxt = "n", las = 1,
         ylim = c(0.5, max(signals$ms_state, na.rm=TRUE) + 0.5),
         yaxt = "n", cex.main = 0.9)
    axis(2, at = unique(na.omit(signals$ms_state)), cex.axis = 0.8)
    abline(v = bp_v, col = "#E24B4A", lwd = 0.8, lty = 2)
  } else {
    plot(1, type = "n", main = "Model C — Markov Switching: not available",
         xlab = "", ylab = "", xaxt = "n")
    text(1, 1, "MSwM fitting failed — try install.packages('MSwM')", cex = 0.8)
  }
  axis.POSIXct(1, at = seq(min(times), max(times), by = "6 months"),
               format = "%b %Y", cex.axis = 0.7, las = 2)

  # Panel 4: ARIMA level z-score
  plot(times, signals$level_z, type = "l", col = "#B8860B", lwd = 0.7,
       main = "Model D — ARIMA: rolling level z-score (deviation from 63-day mean)",
       xlab = "", ylab = "Z-score", xaxt = "n", las = 1, cex.main = 0.9)
  abline(h = c(-1.5, 0, 1.5), lty = c(2,1,2), col = c("gray50","gray70","gray50"), lwd = 0.5)
  abline(v = bp_v, col = "#E24B4A", lwd = 0.8, lty = 2)
  polygon(c(times, rev(times)),
          c(pmax(signals$level_z, 0, na.rm=TRUE), rep(0, nrow(signals))),
          col = adjustcolor("#E24B4A", 0.15), border = NA)
  axis.POSIXct(1, at = seq(min(times), max(times), by = "6 months"),
               format = "%b %Y", cex.axis = 0.7, las = 2)

  mtext("Parallel regime models — WTI M1M2 spread",
        side = 3, outer = TRUE, cex = 1.0, font = 2, line = 1.5)
  mtext("Dashed red = Bai-Perron break dates (reference)",
        side = 1, outer = TRUE, cex = 0.75, line = 1.5)

  dev.off()
  cat("Saved:", save_path, "\n")
}