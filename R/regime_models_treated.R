# R/regime_models_treated.R
# -----------------
# Parallel regime classification — 4 models with diagnostic-driven adjustments.
#
# Adjustments applied based on diagnostics:
#   A: Bai-Perron    — Newey-West HAC standard errors (non-Gaussian, fat tails)
#   B: Kalman Filter — Winsorise outliers + time-varying H matrix (variance ratio=37723)
#   C: Markov Switch — Switching variance sw=TRUE (already correct), verify state separation
#   D: ARIMA         — Fit WITHIN each BP regime separately (ACF1=0.95, structural breaks)
#
# Usage:
#   source("R/regime_models_treated.R")
#   models <- run_parallel_models(results$data, results$consensus$high_confidence)
#   print(models$consensus_matrix)

library(data.table)
library(zoo)

.install_if_missing <- function(pkgs) {
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    cat("Installing:", paste(missing, collapse = ", "), "\n")
    install.packages(missing, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
}
.install_if_missing(c("KFAS", "MSwM", "forecast", "tseries", "sandwich", "lmtest"))

# ═══════════════════════════════════════════════════════════════════════════════
# MAIN ENTRY POINT
# ═══════════════════════════════════════════════════════════════════════════════

run_parallel_models <- function(data,
                                 bp_breaks,
                                 series      = "M1M2",
                                 n_states    = 3,
                                 window_days = 21) {

  cat("\n", strrep("═", 65), "\n")
  cat("  PARALLEL REGIME MODELS —", series, "\n")
  cat(strrep("═", 65), "\n\n")

  y     <- as.numeric(data[[series]])
  ts_   <- as.Date(data$timestamp)
  valid <- !is.na(y)
  y_c   <- y[valid]
  ts_c  <- ts_[valid]

  # ── Model A: Bai-Perron with HAC ──────────────────────────────────────────
  cat("Model A: Bai-Perron (HAC standard errors)...\n")
  ma <- .model_A_baiperron(y_c, ts_c, bp_breaks)

  # ── Model B: Kalman Filter with winsorisation + time-varying H ────────────
  cat("Model B: Kalman Filter (winsorised + time-varying H)...\n")
  mb <- .model_B_kalman(y_c, ts_c)

  # ── Model C: Markov Switching with switching variance ─────────────────────
  cat("Model C: Markov Switching (switching variance, k=", n_states, ")...\n")
  mc <- .model_C_markov(y_c, ts_c, n_states)

  # ── Model D: ARIMA within each BP regime ──────────────────────────────────
  cat("Model D: ARIMA (fitted within each BP regime)...\n")
  md <- .model_D_arima_within_regime(y_c, ts_c, bp_breaks)

  # ── Combine signals ────────────────────────────────────────────────────────
  signals <- .combine_signals(ts_c, y_c, bp_breaks, mb, mc, md)

  # ── Consensus matrix ───────────────────────────────────────────────────────
  consensus <- .build_consensus_matrix(bp_breaks, signals, window_days)

  cat("\n\n", strrep("═", 65), "\n")
  cat("  CONSENSUS MATRIX\n")
  cat(strrep("═", 65), "\n\n")
  print(consensus)

  # Save outputs
  dir.create("output", showWarnings = FALSE)
  fwrite(consensus, "output/consensus_matrix_CL.csv")
  fwrite(signals,   "output/model_signals_CL.csv")
  cat("\nSaved: consensus_matrix_CL.csv, model_signals_CL.csv\n")

  invisible(list(
    signals          = signals,
    consensus_matrix = consensus,
    model_A          = ma,
    model_B          = mb,
    model_C          = mc,
    model_D          = md,
    bp_breaks        = bp_breaks
  ))
}

# ═══════════════════════════════════════════════════════════════════════════════
# MODEL A — Bai-Perron with Newey-West HAC
# Adjustment: HAC SEs correct for fat tails (kurtosis=39) and autocorrelation
# ═══════════════════════════════════════════════════════════════════════════════

.model_A_baiperron <- function(y_c, ts_c, bp_breaks) {
  library(sandwich); library(lmtest)

  boundaries <- c(as.Date("2000-01-01"), sort(bp_breaks), as.Date("2100-01-01"))
  n_seg      <- length(boundaries) - 1

  seg_stats <- lapply(seq_len(n_seg), function(i) {
    idx   <- ts_c > boundaries[i] & ts_c <= boundaries[i+1]
    y_s   <- y_c[idx]
    if (length(y_s) < 5) return(NULL)

    # OLS mean with Newey-West HAC standard errors
    # HAC corrects for autocorrelation AND heteroskedasticity in SEs
    lm_fit  <- lm(y_s ~ 1)
    hac_se  <- tryCatch({
      sqrt(diag(NeweyWest(lm_fit, lag = floor(length(y_s)^(1/3)),
                          prewhite = FALSE)))
    }, error = function(e) sqrt(diag(vcov(lm_fit))))

    list(
      segment   = i,
      start     = min(ts_c[idx]),
      end       = max(ts_c[idx]),
      n         = length(y_s),
      mean      = mean(y_s),
      sd        = sd(y_s),
      hac_se    = hac_se,
      ci_lo     = mean(y_s) - 1.96 * hac_se,
      ci_hi     = mean(y_s) + 1.96 * hac_se
    )
  })

  seg_dt <- rbindlist(Filter(Negate(is.null), seg_stats))

  cat("  HAC SEs computed for", nrow(seg_dt), "segments\n")
  cat("  Segment means (HAC 95% CI):\n")
  for (i in seq_len(nrow(seg_dt))) {
    cat(sprintf("    Seg %d [%s–%s]: mean=%.3f  CI=[%.3f, %.3f]\n",
                seg_dt$segment[i],
                format(seg_dt$start[i]), format(seg_dt$end[i]),
                seg_dt$mean[i], seg_dt$ci_lo[i], seg_dt$ci_hi[i]))
  }

  # BP regime label per date
  regime <- rep(NA_character_, length(ts_c))
  for (i in seq_len(n_seg)) {
    idx <- ts_c > boundaries[i] & ts_c <= boundaries[i+1]
    m   <- if (nrow(seg_dt) >= i) seg_dt$mean[i] else mean(y_c[idx])
    regime[idx] <- ifelse(m > 1.5,  "backwardation",
                   ifelse(m < -0.5, "contango", "flat"))
  }

  data.table(date=ts_c, bp_regime=regime, segment_stats=list(seg_dt))
}

# ═══════════════════════════════════════════════════════════════════════════════
# MODEL B — Kalman Filter with winsorisation + time-varying H
# Adjustment: winsorise at 1st/99th percentile (kurtosis=39, ratio=37723)
#             use regime-specific H variance (time-varying noise)
# ═══════════════════════════════════════════════════════════════════════════════

.model_B_kalman <- function(y_c, ts_c) {
  library(KFAS)

  # Step 1: Winsorise at 1st/99th percentile
  # Rationale: kurtosis=39 means extreme values dominate MLE variance estimates
  # Winsorising clips the distribution without removing data
  lo  <- quantile(y_c, 0.01)
  hi  <- quantile(y_c, 0.99)
  y_w <- pmin(pmax(y_c, lo), hi)
  n_clipped <- sum(y_c < lo | y_c > hi)
  cat("  Winsorised", n_clipped, "observations at [",
      round(lo,3), ",", round(hi,3), "]\n")

  # Step 2: Time-varying H (observation variance)
  # Rationale: variance ratio=37723 — one constant H cannot fit all periods
  # Use rolling 63-day variance as the H schedule
  roll_var <- zoo::rollapply(y_w, 63, var, fill=NA, align="right")
  # Fill leading NAs with first valid value
  first_valid <- roll_var[!is.na(roll_var)][1]
  roll_var[is.na(roll_var)] <- first_valid
  # Floor at minimum variance to avoid numerical issues
  roll_var <- pmax(roll_var, 1e-6)

  # Step 3: Fit local level model with time-varying H
  H_array <- array(roll_var, dim=c(1,1,length(y_w)))

  model <- tryCatch(
    SSModel(y_w ~ SSMtrend(1, Q=list(matrix(NA))),
            H = H_array),
    error = function(e) {
      cat("  Time-varying H failed — falling back to estimated constant H\n")
      SSModel(y_w ~ SSMtrend(1, Q=list(matrix(NA))), H=matrix(NA))
    }
  )

  fit <- tryCatch(
    fitSSM(model,
           inits   = log(var(y_w) * 0.1),
           method  = "L-BFGS-B"),
    error = function(e) {
      cat("  MLE failed:", conditionMessage(e), "\n")
      list(model = model)
    }
  )

  smoothed  <- KFS(fit$model)
  mu_hat    <- as.numeric(smoothed$alphahat[, "level"])
  deviation <- y_c - mu_hat     # use original (not winsorised) for deviation
  dev_sd    <- sd(deviation, na.rm=TRUE)
  dev_z     <- deviation / dev_sd

  kf_regime <- ifelse(dev_z > 1.5,  "backwardation",
               ifelse(dev_z < -1.5, "contango", "flat"))

  cat("  KF fitted | Signal variance (Q):",
      round(fit$model$Q[1,1,1], 6), "\n")

  data.table(
    date      = ts_c,
    kf_mean   = mu_hat,
    kf_dev    = deviation,
    kf_z      = dev_z,
    kf_regime = kf_regime
  )
}

# ═══════════════════════════════════════════════════════════════════════════════
# MODEL C — Markov Switching with switching variance
# Adjustment: sw=c(TRUE,TRUE) already handles heteroskedasticity
#             verify state separation post-fit
# ═══════════════════════════════════════════════════════════════════════════════

.model_C_markov <- function(y_c, ts_c, n_states = 3) {
  library(MSwM)

  ms_fit <- tryCatch({
    lm_base <- lm(y_c ~ 1)
    msmFit(lm_base, k = n_states,
           sw      = c(TRUE, TRUE),   # switching mean AND variance
           control = list(parallel=FALSE, maxiter=500, tol=1e-6))
  }, error = function(e) {
    cat("  MS fit failed:", conditionMessage(e), "\n")
    NULL
  })

  if (is.null(ms_fit)) {
    return(data.table(date=ts_c, ms_state=NA_integer_,
                      ms_regime=NA_character_,
                      ms_prob_back=NA_real_))
  }

  probs      <- ms_fit@Fit@smoProb[-1, , drop=FALSE]
  states     <- apply(probs, 1, which.max)
  state_means <- ms_fit@Coef[, "(Intercept)"]
  state_sds   <- ms_fit@std
  
  # Order states by mean
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

  # Verify state separation (key check given volatility clustering)
  cat("  State means  :", paste(round(sort(state_means), 3), collapse=" | "), "\n")
  cat("  State SDs    :", paste(round(state_sds[order(state_means)], 3), collapse=" | "), "\n")

  # Separation ratio: distance between adjacent means relative to pooled SD
  sorted_means <- sort(state_means)
  if (n_states >= 2) {
    sep_ratio <- diff(sorted_means) / mean(state_sds)
    cat("  State separation ratios:", paste(round(sep_ratio, 2), collapse=" | "),
        ifelse(all(sep_ratio > 0.5), "— WELL SEPARATED ✓", "— OVERLAP WARNING ✗"), "\n")
  }

  ms_regime  <- labels[states]
  prob_back  <- if (n_states==2) probs[,state_order[2]] else probs[,state_order[3]]

  data.table(
    date         = ts_c,
    ms_state     = states,
    ms_regime    = ms_regime,
    ms_prob_back = prob_back
  )
}

# ═══════════════════════════════════════════════════════════════════════════════
# MODEL D — ARIMA fitted within each BP regime
# Adjustment: ACF(1)=0.95 and structural breaks — ARIMA across full sample invalid
#             Each regime gets its own ARIMA; rolling 63-day z-score for comparison
# ═══════════════════════════════════════════════════════════════════════════════

.model_D_arima_within_regime <- function(y_c, ts_c, bp_breaks) {
  library(forecast)

  boundaries <- c(as.Date("2000-01-01"), sort(bp_breaks), as.Date("2100-01-01"))
  n_seg      <- length(boundaries) - 1

  # Output containers
  arima_z      <- rep(NA_real_, length(y_c))
  arima_regime <- rep(NA_character_, length(y_c))
  arima_orders <- list()

  for (i in seq_len(n_seg)) {
    idx   <- which(ts_c > boundaries[i] & ts_c <= boundaries[i+1])
    y_s   <- y_c[idx]

    if (length(y_s) < 20) {
      cat("  Regime", i, ": too few obs (", length(y_s), ") — skipping\n")
      next
    }

    # Fit ARIMA within this regime only
    fit <- tryCatch(
      auto.arima(y_s, seasonal=FALSE, stepwise=TRUE,
                 approximation=TRUE, max.p=3, max.q=3),
      error=function(e) {
        cat("  Regime", i, "ARIMA failed — using AR(1)\n")
        tryCatch(arima(y_s, order=c(1,0,0)), error=function(e2) NULL)
      }
    )

    if (is.null(fit)) next

    order_str <- paste(fit$arma[c(1,6,2)], collapse=",")
    arima_orders[[i]] <- order_str

    resid_s   <- as.numeric(residuals(fit))
    resid_sd  <- sd(resid_s, na.rm=TRUE)

    # Rolling 63-day z-score of level (for cross-regime comparison)
    # This is stationary within each regime and comparable across regimes
    roll_mean <- zoo::rollmean(y_s, pmin(63, floor(length(y_s)/2)),
                                fill=NA, align="right")
    roll_sd   <- zoo::rollapply(y_s, pmin(63, floor(length(y_s)/2)),
                                 sd, fill=NA, align="right")
    roll_sd   <- ifelse(is.na(roll_sd) | roll_sd < 1e-6,
                        sd(y_s, na.rm=TRUE), roll_sd)
    level_z   <- (y_s - roll_mean) / roll_sd

    arima_z[idx]      <- level_z
    arima_regime[idx] <- ifelse(level_z > 1.5,  "backwardation",
                         ifelse(level_z < -1.5, "contango", "flat"))

    cat(sprintf("  Regime %d [%s–%s]: ARIMA(%s)  n=%d\n",
                i, format(boundaries[i+1]-as.integer(boundaries[i+1]-boundaries[i])),
                format(boundaries[i+1]), order_str, length(y_s)))
  }

  data.table(
    date         = ts_c,
    arima_z      = arima_z,
    arima_regime = arima_regime
  )
}

# ═══════════════════════════════════════════════════════════════════════════════
# COMBINE SIGNALS
# ═══════════════════════════════════════════════════════════════════════════════

.combine_signals <- function(ts_c, y_c, bp_breaks, kf, ms, arima) {

  boundaries <- c(as.Date("2000-01-01"), sort(bp_breaks), as.Date("2100-01-01"))
  bp_regime  <- rep(NA_character_, length(ts_c))
  for (i in seq_len(length(boundaries)-1)) {
    idx <- ts_c > boundaries[i] & ts_c <= boundaries[i+1]
    m   <- mean(y_c[idx], na.rm=TRUE)
    bp_regime[idx] <- ifelse(m > 1.5,  "backwardation",
                      ifelse(m < -0.5, "contango", "flat"))
  }

  result <- data.table(
    date         = ts_c,
    M1M2         = y_c,
    bp_regime    = bp_regime,
    kf_z         = kf$kf_z,
    kf_regime    = kf$kf_regime,
    ms_state     = ms$ms_state,
    ms_regime    = ms$ms_regime,
    ms_prob_back = ms$ms_prob_back,
    arima_z      = arima$arima_z,
    arima_regime = arima$arima_regime
  )

  # Consensus: how many models say "backwardation" at each bar
  result[, n_models_back := rowSums(cbind(
    bp_regime    == "backwardation",
    kf_regime    == "backwardation",
    ms_regime    == "backwardation",
    arima_regime == "backwardation"
  ), na.rm=TRUE)]

  result[, consensus_regime := ifelse(n_models_back >= 3, "backwardation",
                               ifelse(n_models_back == 0, "contango_flat",
                               "transitional"))]
  result
}

# ═══════════════════════════════════════════════════════════════════════════════
# CONSENSUS MATRIX
# ═══════════════════════════════════════════════════════════════════════════════

.build_consensus_matrix <- function(bp_breaks, signals, window_days=21) {

  rows <- lapply(seq_along(bp_breaks), function(i) {
    bd     <- bp_breaks[i]
    before <- signals[date >= (bd - window_days*2) & date <  bd]
    after  <- signals[date >  bd & date <= (bd + window_days*2)]
    if (nrow(before) < 5 || nrow(after) < 5) return(NULL)

    # BP: always YES (this is a BP break by definition)
    bp_chg <- TRUE

    # KF: z-score shift > 0.75 SD
    kf_chg <- !all(is.na(after$kf_z)) && !all(is.na(before$kf_z)) &&
               abs(mean(after$kf_z, na.rm=TRUE) -
                   mean(before$kf_z, na.rm=TRUE)) > 0.75

    # MS: dominant state changes
    ms_chg <- !all(is.na(after$ms_regime)) &&
               !all(is.na(before$ms_regime)) &&
              (names(sort(table(after$ms_regime),  decreasing=TRUE)[1]) !=
               names(sort(table(before$ms_regime), decreasing=TRUE)[1]))

    # ARIMA: within-regime z-score shift > 0.75
    ar_chg <- !all(is.na(after$arima_z)) && !all(is.na(before$arima_z)) &&
               abs(mean(after$arima_z, na.rm=TRUE) -
                   mean(before$arima_z, na.rm=TRUE)) > 0.75

    n_models   <- sum(c(bp_chg, kf_chg, ms_chg, ar_chg))
    confidence <- ifelse(n_models==4,"VERY HIGH",
                  ifelse(n_models==3,"HIGH",
                  ifelse(n_models==2,"MEDIUM","LOW")))

    m_before <- mean(before$M1M2, na.rm=TRUE)
    m_after  <- mean(after$M1M2,  na.rm=TRUE)
    delta    <- round(m_after - m_before, 3)

    regime_after <- local({
      m <- mean(after$M1M2, na.rm=TRUE)
      ifelse(m > 1.5,  "deep_backwardation",
      ifelse(m > 0.4,  "mild_backwardation",
      ifelse(m < -1.5, "deep_contango",
      ifelse(m < -0.2, "mild_contango", "flat"))))
    })

    # MS probability of backwardation after break
    ms_prob <- if (!all(is.na(after$ms_prob_back)))
                 round(mean(after$ms_prob_back, na.rm=TRUE), 3)
               else NA_real_

    data.table(
      break_date   = format(bd),
      BP           = ifelse(bp_chg, "YES", "---"),
      KF           = ifelse(kf_chg, "YES", "---"),
      MS           = ifelse(ms_chg, "YES", "---"),
      ARIMA        = ifelse(ar_chg, "YES", "---"),
      n_models     = n_models,
      confidence   = confidence,
      direction    = ifelse(delta > 0, "tighter", "looser"),
      delta_M1M2   = delta,
      MS_prob_back = ms_prob,
      regime_after = regime_after
    )
  })

  rbindlist(Filter(Negate(is.null), rows))
}

# ═══════════════════════════════════════════════════════════════════════════════
# PLOT ALL MODEL SIGNALS
# ═══════════════════════════════════════════════════════════════════════════════

plot_model_signals <- function(models_result,
                                save_path="output/regime_model_signals.png") {
  sig    <- models_result$signals
  breaks <- models_result$bp_breaks
  times  <- as.POSIXct(sig$date)
  bp_v   <- as.POSIXct(breaks)
  x_ax   <- function() axis.POSIXct(1,
               at=seq(min(times),max(times),by="6 months"),
               format="%b %Y", cex.axis=0.72, las=2)

  png(save_path, width=1600, height=1500, res=120)
  par(mfrow=c(4,1), mar=c(2,5,2.5,2), oma=c(3,0,3,0), bg="white")

  # Panel 1 — Raw M1M2 + BP breaks
  plot(times, sig$M1M2, type="l", col="#185FA5", lwd=0.7,
       main="Model A — Bai-Perron: M1M2 + break dates (HAC SEs)",
       xlab="", ylab="M1M2 ($/bbl)", xaxt="n", las=1)
  abline(v=bp_v, col="#E24B4A", lwd=1.0)
  abline(h=0, lty=2, col="gray60", lwd=0.4)
  x_ax()

  # Panel 2 — KF z-score (winsorised + time-varying H)
  plot(times, sig$kf_z, type="l", col="#0F6E56", lwd=0.7,
       main="Model B — Kalman Filter: deviation z-score (winsorised, time-varying H)",
       xlab="", ylab="Z-score", xaxt="n", las=1)
  abline(h=c(-1.5,0,1.5), lty=c(2,1,2), col=c("gray50","gray70","gray50"), lwd=0.5)
  abline(v=bp_v, col="#E24B4A", lwd=0.8, lty=2)
  polygon(c(times,rev(times)), c(pmax(sig$kf_z,0,na.rm=TRUE), rep(0,nrow(sig))),
          col=adjustcolor("#E24B4A",0.12), border=NA)
  polygon(c(times,rev(times)), c(pmin(sig$kf_z,0,na.rm=TRUE), rep(0,nrow(sig))),
          col=adjustcolor("#185FA5",0.12), border=NA)
  x_ax()

  # Panel 3 — Markov switching state + backwardation probability
  if (!all(is.na(sig$ms_state))) {
    par(mar=c(2,5,2.5,5))
    plot(times, sig$ms_state, type="s", col="#6B2D8B", lwd=1.0,
         main="Model C — Markov Switching: state + P(backwardation) (switching variance)",
         xlab="", ylab="State", xaxt="n", las=1,
         ylim=c(0.5, max(sig$ms_state,na.rm=TRUE)+0.5), yaxt="n")
    axis(2, at=sort(unique(na.omit(sig$ms_state))), cex.axis=0.8)
    abline(v=bp_v, col="#E24B4A", lwd=0.8, lty=2)
    # Overlay backwardation probability on right axis
    par(new=TRUE)
    plot(times, sig$ms_prob_back, type="l", col=adjustcolor("#E24B4A",0.5),
         lwd=0.8, axes=FALSE, xlab="", ylab="", ylim=c(0,1))
    axis(4, at=seq(0,1,0.25), labels=paste0(seq(0,100,25),"%"), cex.axis=0.72)
    mtext("P(back)", side=4, line=3, cex=0.7)
    x_ax()
    par(mar=c(2,5,2.5,2))
  } else {
    plot(1, type="n", main="Model C — Markov Switching: unavailable", xlab="", ylab="")
    x_ax()
  }

  # Panel 4 — ARIMA within-regime z-score
  plot(times, sig$arima_z, type="l", col="#B8860B", lwd=0.7,
       main="Model D — ARIMA: within-regime rolling z-score (separate fit per regime)",
       xlab="", ylab="Z-score", xaxt="n", las=1)
  abline(h=c(-1.5,0,1.5), lty=c(2,1,2), col=c("gray50","gray70","gray50"), lwd=0.5)
  abline(v=bp_v, col="#E24B4A", lwd=0.8, lty=2)
  polygon(c(times,rev(times)), c(pmax(sig$arima_z,0,na.rm=TRUE), rep(0,nrow(sig))),
          col=adjustcolor("#E24B4A",0.12), border=NA)
  x_ax()

  mtext("Parallel regime models — WTI M1M2 (diagnostic adjustments applied)",
        side=3, outer=TRUE, cex=1.0, font=2, line=1.5)
  mtext("Dashed red = Bai-Perron break dates (reference)",
        side=1, outer=TRUE, cex=0.75, line=1.5)

  dev.off()
  cat("Saved:", save_path, "\n")
}
