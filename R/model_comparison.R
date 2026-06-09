# R/model_comparison.R
# --------------------
# Compares model performance before and after diagnostic adjustments.
#
# Metrics:
#   1. Within-regime variance (lower = more stable regimes)
#   2. Between-regime variance (higher = more separated regimes)
#   3. Signal-to-noise ratio (between/within)
#   4. Detection lag vs BP breaks
#   5. Regime persistence (avg days per regime)
#   6. Spurious transitions (reversals within 5 days)
#   7. ARIMA within-regime AIC (before: full sample, after: per regime)
#   8. KF innovation residual normality (before: constant H, after: time-varying H)
#
# Usage:
#   source("R/model_comparison.R")
#   comparison <- compare_models(results$data,
#                                results$consensus$high_confidence,
#                                models_old, models_new)
#   print(comparison$scorecard)

library(data.table)
library(zoo)

.install_if_missing <- function(pkgs) {
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0)
    install.packages(missing, repos="https://cloud.r-project.org", quiet=TRUE)
}
.install_if_missing(c("KFAS","forecast","moments","nortest"))

# ═══════════════════════════════════════════════════════════════════════════════
# MAIN ENTRY POINT
# ═══════════════════════════════════════════════════════════════════════════════

compare_models <- function(data, bp_breaks,
                            models_old  = NULL,
                            models_new  = NULL,
                            series      = "M1M2",
                            save_path   = "output/model_comparison.png") {

  cat("\n", strrep("═", 65), "\n")
  cat("  MODEL PERFORMANCE COMPARISON\n")
  cat(strrep("═", 65), "\n\n")

  y    <- as.numeric(data[[series]])
  ts_  <- as.Date(data$timestamp)
  valid <- !is.na(y)
  y_c  <- y[valid]; ts_c <- ts_[valid]

  # ── Build "before" baseline models (unadjusted) ───────────────────────────
  cat("Building BEFORE (unadjusted) baseline...\n")
  before <- .build_before_models(y_c, ts_c, bp_breaks)

  # ── Use provided "after" models or build them ─────────────────────────────
  if (!is.null(models_new)) {
    cat("Using provided AFTER (adjusted) models...\n")
    after_signals <- models_new$signals
  } else {
    cat("Building AFTER (adjusted) models...\n")
    source("R/regime_models.R")
    models_new    <- run_parallel_models(data, bp_breaks, series)
    after_signals <- models_new$signals
  }

  # ── Compute all metrics ───────────────────────────────────────────────────
  cat("\nComputing performance metrics...\n\n")

  metrics <- list(
    kf    = .compare_kf(y_c, ts_c, before$kf_before, after_signals),
    arima = .compare_arima(y_c, ts_c, bp_breaks),
    ms    = .compare_ms(before$ms_before, after_signals, bp_breaks),
    regime = .compare_regime_quality(y_c, ts_c, bp_breaks,
                                      before$bp_regime, after_signals$bp_regime)
  )

  # ── Build scorecard ───────────────────────────────────────────────────────
  scorecard <- .build_scorecard(metrics)

  cat("\n", strrep("═", 65), "\n")
  cat("  SCORECARD: BEFORE vs AFTER ADJUSTMENTS\n")
  cat(strrep("═", 65), "\n\n")
  print(scorecard)

  # ── Plot ──────────────────────────────────────────────────────────────────
  .plot_comparison(y_c, ts_c, bp_breaks, before, after_signals,
                    metrics, save_path)

  dir.create("output", showWarnings=FALSE)
  fwrite(scorecard, "output/model_comparison_scorecard.csv")
  cat("\nSaved: model_comparison_scorecard.csv\n")

  invisible(list(scorecard=scorecard, metrics=metrics,
                  before=before, after=after_signals))
}

# ═══════════════════════════════════════════════════════════════════════════════
# BUILD UNADJUSTED BASELINE MODELS
# ═══════════════════════════════════════════════════════════════════════════════

.build_before_models <- function(y_c, ts_c, bp_breaks) {
  library(KFAS); library(forecast)

  # KF BEFORE: constant H, no winsorisation
  # Use fixed variances to avoid NA/Inf issues with extreme raw data
  cat("  KF (constant H, no winsorisation)...\n")
  y_var <- var(y_c, na.rm=TRUE)
  # Cap initial variance at reasonable value to avoid SSModel NA error
  h_init <- min(y_var, 100)
  q_init <- min(y_var * 0.1, 10)
  model_before <- tryCatch(
    SSModel(y_c ~ SSMtrend(1, Q=list(matrix(q_init))),
            H=matrix(h_init)),
    error = function(e) {
      cat("  KF-before SSModel failed — using simplified version\n")
      SSModel(y_c ~ SSMtrend(1, Q=list(matrix(0.1))), H=matrix(1.0))
    }
  )
  fit_before <- tryCatch(
    fitSSM(model_before,
           inits  = c(log(q_init), log(h_init)),
           method = "L-BFGS-B"),
    error = function(e) list(model=model_before)
  )
  kfs_before   <- KFS(fit_before$model)
  mu_before    <- as.numeric(kfs_before$alphahat[,"level"])
  innov_before <- tryCatch(as.numeric(kfs_before$v[,1]),
                            error=function(e) rep(NA_real_, length(y_c)))

  kf_before <- data.table(
    date       = ts_c,
    kf_mean    = mu_before,
    kf_dev     = y_c - mu_before,
    kf_z       = (y_c - mu_before) / sd(y_c - mu_before, na.rm=TRUE),
    innovation = innov_before
  )

  # ARIMA BEFORE: single fit across full sample
  cat("  ARIMA (full sample, no regime split)...\n")
  fit_full <- tryCatch(
    auto.arima(y_c, seasonal=FALSE, stepwise=TRUE, approximation=TRUE,
               max.p=3, max.q=3),
    error=function(e) arima(y_c, order=c(1,0,0))
  )
  resid_full  <- as.numeric(residuals(fit_full))
  roll_mean   <- zoo::rollmean(y_c, 63, fill=NA, align="right")
  roll_sd     <- zoo::rollapply(y_c, 63, sd, fill=NA, align="right")
  roll_sd     <- ifelse(is.na(roll_sd)|roll_sd<1e-6, sd(y_c), roll_sd)
  arima_z_full <- (y_c - roll_mean) / roll_sd

  arima_before <- data.table(
    date        = ts_c,
    arima_z     = arima_z_full,
    arima_resid = resid_full,
    aic_full    = AIC(fit_full),
    order       = paste(fit_full$arma[c(1,6,2)], collapse=",")
  )

  # MS BEFORE: 3 states, mean-only switching (no variance switching)
  cat("  MS (mean-only switching, no variance switching)...\n")
  ms_before_regime <- rep(NA_character_, length(y_c))
  ms_before_prob   <- rep(NA_real_,      length(y_c))
  tryCatch({
    library(MSwM)
    lm_base  <- lm(y_c ~ 1)
    ms_fit   <- msmFit(lm_base, k=3, sw=c(TRUE, FALSE),  # mean only, NOT variance
                       control=list(parallel=FALSE, maxiter=300, tol=1e-5))
    probs    <- ms_fit@Fit@smoProb[-1,, drop=FALSE]
    states   <- apply(probs, 1, which.max)
    means    <- ms_fit@Coef[,"(Intercept)"]
    ord      <- order(means)
    labels   <- c("contango","flat","backwardation")[match(1:3, ord)]
    ms_before_regime <- labels[states]
    ms_before_prob   <- probs[, ord[3]]
    cat("  MS-before state means:", paste(round(sort(means),3), collapse=" | "), "\n")
  }, error=function(e) cat("  MS-before failed:", conditionMessage(e), "\n"))

  # BP regime labels
  boundaries <- c(as.Date("2000-01-01"), sort(bp_breaks), as.Date("2100-01-01"))
  bp_regime  <- rep(NA_character_, length(y_c))
  for (i in seq_len(length(boundaries)-1)) {
    idx <- ts_c > boundaries[i] & ts_c <= boundaries[i+1]
    m   <- mean(y_c[idx], na.rm=TRUE)
    bp_regime[idx] <- ifelse(m>1.5,"backwardation",ifelse(m<-0.5,"contango","flat"))
  }

  list(kf_before    = kf_before,
       arima_before = arima_before,
       ms_before    = data.table(date=ts_c,
                                  ms_regime=ms_before_regime,
                                  ms_prob_back=ms_before_prob),
       bp_regime    = bp_regime)
}

# ═══════════════════════════════════════════════════════════════════════════════
# METRIC 1: KF COMPARISON
# ═══════════════════════════════════════════════════════════════════════════════

.compare_kf <- function(y_c, ts_c, kf_before, after_signals) {
  library(moments); library(nortest)

  # Innovation residuals: after model needs re-extraction
  # Use deviation z-score as proxy (already computed)
  dev_before <- kf_before$kf_dev
  dev_after  <- after_signals$kf_z * sd(after_signals$kf_z, na.rm=TRUE)
  if ("kf_dev" %in% names(after_signals)) dev_after <- after_signals$kf_dev

  # 1. Normality of innovation residuals (higher p = better)
  innov_b <- kf_before$innovation[!is.na(kf_before$innovation)]
  jb_before <- tryCatch(moments::jarque.test(innov_b)$p.value, error=function(e) NA)
  jb_after  <- tryCatch(moments::jarque.test(dev_after[!is.na(dev_after)])$p.value,
                         error=function(e) NA)

  # 2. Residual variance (lower = better fit)
  var_before <- var(dev_before, na.rm=TRUE)
  var_after  <- var(dev_after,  na.rm=TRUE)

  # 3. Variance ratio of residuals across regimes (lower = more homoskedastic)
  roll_var_b <- zoo::rollapply(dev_before, 63, var, fill=NA, align="right")
  roll_var_a <- zoo::rollapply(dev_after,  63, var, fill=NA, align="right")
  vr_before  <- max(roll_var_b,na.rm=TRUE) / max(min(roll_var_b,na.rm=TRUE),1e-10)
  vr_after   <- max(roll_var_a,na.rm=TRUE) / max(min(roll_var_a,na.rm=TRUE),1e-10)

  cat("KF Comparison:\n")
  cat("  Innovation residual normality (JB p-value):\n")
  cat("    Before:", round(jb_before,4), "| After:", round(jb_after,4),
      "→", ifelse(!is.na(jb_after) && jb_after > jb_before, "IMPROVED ↑", "no change"), "\n")
  cat("  Residual variance:\n")
  cat("    Before:", round(var_before,4), "| After:", round(var_after,4),
      "→", ifelse(!is.na(var_after) && var_after < var_before, "IMPROVED ↑", "no change"), "\n")
  cat("  Rolling variance ratio:\n")
  cat("    Before:", round(vr_before,1), "| After:", round(vr_after,1),
      "→", ifelse(!is.na(vr_after) && vr_after < vr_before, "IMPROVED ↑", "no change"), "\n")

  list(jb_before=jb_before, jb_after=jb_after,
       var_before=var_before, var_after=var_after,
       vr_before=vr_before, vr_after=vr_after)
}

# ═══════════════════════════════════════════════════════════════════════════════
# METRIC 2: ARIMA COMPARISON
# ═══════════════════════════════════════════════════════════════════════════════

.compare_arima <- function(y_c, ts_c, bp_breaks) {
  library(forecast)

  boundaries <- c(as.Date("2000-01-01"), sort(bp_breaks), as.Date("2100-01-01"))
  n_seg      <- length(boundaries) - 1

  # Full-sample ARIMA AIC (before)
  fit_full  <- tryCatch(
    auto.arima(y_c, seasonal=FALSE, stepwise=TRUE, approximation=TRUE,
               max.p=3, max.q=3),
    error=function(e) arima(y_c, order=c(1,0,0))
  )
  aic_full  <- AIC(fit_full)
  resid_full <- as.numeric(residuals(fit_full))
  lb_full   <- tryCatch(
    Box.test(resid_full, lag=10, type="Ljung-Box")$p.value,
    error=function(e) NA)

  # Per-regime ARIMA AIC (after)
  aic_per_regime <- sapply(seq_len(n_seg), function(i) {
    idx <- which(ts_c > boundaries[i] & ts_c <= boundaries[i+1])
    y_s <- y_c[idx]
    if (length(y_s) < 20) return(NA)
    fit <- tryCatch(
      auto.arima(y_s, seasonal=FALSE, stepwise=TRUE, approximation=TRUE,
                 max.p=3, max.q=3),
      error=function(e) tryCatch(arima(y_s, order=c(1,0,0)),
                                  error=function(e2) NULL))
    if (is.null(fit)) return(NA)
    # Scale AIC by length for fair comparison
    AIC(fit) / length(y_s)
  })

  aic_full_scaled  <- aic_full / length(y_c)
  aic_regime_mean  <- mean(aic_per_regime, na.rm=TRUE)

  # Ljung-Box on within-regime residuals
  lb_within <- sapply(seq_len(n_seg), function(i) {
    idx <- which(ts_c > boundaries[i] & ts_c <= boundaries[i+1])
    y_s <- y_c[idx]
    if (length(y_s) < 20) return(NA)
    fit <- tryCatch(
      auto.arima(y_s, seasonal=FALSE, stepwise=TRUE, approximation=TRUE),
      error=function(e) NULL)
    if (is.null(fit)) return(NA)
    tryCatch(Box.test(residuals(fit), lag=5, type="Ljung-Box")$p.value,
             error=function(e) NA)
  })

  cat("\nARIMA Comparison:\n")
  cat("  AIC/n (full sample)     :", round(aic_full_scaled, 4), "\n")
  cat("  AIC/n (per regime mean) :", round(aic_regime_mean, 4),
      "→", ifelse(aic_regime_mean < aic_full_scaled, "IMPROVED ↑", "no change"), "\n")
  cat("  Ljung-Box p (full)      :", round(lb_full, 4),
      ifelse(!is.na(lb_full) && lb_full < 0.05, "(autocorrelated ✗)", "(OK ✓)"), "\n")
  cat("  Ljung-Box p (within-reg):", round(mean(lb_within, na.rm=TRUE), 4),
      ifelse(mean(lb_within,na.rm=TRUE) > lb_full, "IMPROVED ↑", "no change"), "\n")

  list(aic_full_scaled=aic_full_scaled, aic_regime_mean=aic_regime_mean,
       lb_full=lb_full, lb_within_mean=mean(lb_within,na.rm=TRUE),
       aic_per_regime=aic_per_regime)
}

# ═══════════════════════════════════════════════════════════════════════════════
# METRIC 3: MARKOV SWITCHING COMPARISON
# ═══════════════════════════════════════════════════════════════════════════════

.compare_ms <- function(ms_before, after_signals, bp_breaks) {

  # State persistence: avg run length before state change
  .avg_persistence <- function(regime_vec) {
    r   <- rle(regime_vec[!is.na(regime_vec)])
    mean(r$lengths)
  }

  # Spurious transitions: transitions that reverse within 5 bars
  .spurious_rate <- function(regime_vec) {
    rv    <- regime_vec[!is.na(regime_vec)]
    trans <- which(diff(as.integer(factor(rv))) != 0)
    if (length(trans) == 0) return(0)
    spurious <- sum(sapply(trans, function(i) {
      end_i <- min(i+5, length(rv))
      rv[end_i] == rv[max(1,i-1)]  # reverts back
    }))
    spurious / length(trans)
  }

  # Agreement with BP breaks
  .bp_agreement <- function(regime_vec, ts_c, bp_breaks, window=21) {
    if (all(is.na(regime_vec))) return(NA)
    agreed <- sapply(bp_breaks, function(bd) {
      before_idx <- which(ts_c >= (bd-window) & ts_c < bd)
      after_idx  <- which(ts_c >  bd & ts_c <= (bd+window))
      if (length(before_idx)<3 || length(after_idx)<3) return(NA)
      dom_before <- names(sort(table(regime_vec[before_idx]),decreasing=TRUE)[1])
      dom_after  <- names(sort(table(regime_vec[after_idx]), decreasing=TRUE)[1])
      !is.na(dom_before) && !is.na(dom_after) && dom_before != dom_after
    })
    mean(agreed, na.rm=TRUE)
  }

  ts_c <- ms_before$date
  b_reg <- ms_before$ms_regime
  a_reg <- after_signals$ms_regime

  persist_before <- .avg_persistence(b_reg)
  persist_after  <- .avg_persistence(a_reg)
  spurious_before <- .spurious_rate(b_reg)
  spurious_after  <- .spurious_rate(a_reg)
  agree_before   <- .bp_agreement(b_reg, ts_c, bp_breaks)
  agree_after    <- .bp_agreement(a_reg, ts_c, bp_breaks)

  cat("\nMarkov Switching Comparison:\n")
  cat("  Avg regime persistence (days):\n")
  cat("    Before:", round(persist_before,1), "| After:", round(persist_after,1),
      "→", ifelse(persist_after > persist_before, "IMPROVED ↑", "no change"), "\n")
  cat("  Spurious transition rate:\n")
  cat("    Before:", round(spurious_before,3), "| After:", round(spurious_after,3),
      "→", ifelse(spurious_after < spurious_before, "IMPROVED ↑", "no change"), "\n")
  cat("  BP break agreement rate:\n")
  cat("    Before:", round(agree_before,3), "| After:", round(agree_after,3),
      "→", ifelse(!is.na(agree_after) && agree_after > agree_before,
                  "IMPROVED ↑", "no change"), "\n")

  list(persist_before=persist_before, persist_after=persist_after,
       spurious_before=spurious_before, spurious_after=spurious_after,
       agree_before=agree_before, agree_after=agree_after)
}

# ═══════════════════════════════════════════════════════════════════════════════
# METRIC 4: REGIME QUALITY
# ═══════════════════════════════════════════════════════════════════════════════

.compare_regime_quality <- function(y_c, ts_c, bp_breaks,
                                     bp_regime_before, bp_regime_after) {

  .snr <- function(y, regime) {
    # Signal-to-noise = between-regime variance / within-regime variance
    regime_means <- tapply(y, regime, mean, na.rm=TRUE)
    grand_mean   <- mean(y, na.rm=TRUE)
    between_var  <- mean((regime_means - grand_mean)^2)
    within_var   <- mean(tapply(y, regime, var, na.rm=TRUE), na.rm=TRUE)
    list(between=between_var, within=within_var,
         snr=between_var/max(within_var,1e-10))
  }

  snr_b <- .snr(y_c, bp_regime_before)
  snr_a <- .snr(y_c, bp_regime_after)

  # Regime means separation (Mahalanobis-like distance)
  means_b <- tapply(y_c, bp_regime_before, mean, na.rm=TRUE)
  means_a <- tapply(y_c, bp_regime_after,  mean, na.rm=TRUE)
  pooled_sd_b <- sqrt(mean(tapply(y_c, bp_regime_before, var, na.rm=TRUE), na.rm=TRUE))
  pooled_sd_a <- sqrt(mean(tapply(y_c, bp_regime_after,  var, na.rm=TRUE), na.rm=TRUE))

  sep_b <- if (length(means_b)>1) diff(range(means_b))/pooled_sd_b else NA
  sep_a <- if (length(means_a)>1) diff(range(means_a))/pooled_sd_a else NA

  cat("\nRegime Quality Comparison:\n")
  cat("  Signal-to-noise ratio (between/within variance):\n")
  cat("    Before:", round(snr_b$snr,3), "| After:", round(snr_a$snr,3),
      "→", ifelse(!is.na(snr_a$snr) && snr_a$snr > snr_b$snr,
                  "IMPROVED ↑", "no change"), "\n")
  cat("  Regime separation (range of means / pooled SD):\n")
  cat("    Before:", round(sep_b,3), "| After:", round(sep_a,3),
      "→", ifelse(!is.na(sep_a) && sep_a > sep_b, "IMPROVED ↑", "no change"), "\n")
  cat("  Within-regime variance:\n")
  cat("    Before:", round(snr_b$within,4), "| After:", round(snr_a$within,4),
      "→", ifelse(!is.na(snr_a$within) && snr_a$within < snr_b$within,
                  "IMPROVED ↑", "no change"), "\n")

  list(snr_before=snr_b$snr, snr_after=snr_a$snr,
       sep_before=sep_b, sep_after=sep_a,
       within_before=snr_b$within, within_after=snr_a$within)
}

# ═══════════════════════════════════════════════════════════════════════════════
# SCORECARD
# ═══════════════════════════════════════════════════════════════════════════════

.build_scorecard <- function(m) {
  .fmt <- function(before, after, higher_better=TRUE) {
    if (is.na(before) || is.na(after)) return(data.table(
      before="N/A", after="N/A", change="N/A", verdict="N/A"))
    pct <- round((after - before)/abs(before)*100, 1)
    improved <- if (higher_better) after > before else after < before
    data.table(
      before  = round(before, 4),
      after   = round(after,  4),
      change  = paste0(ifelse(pct>0,"+",""), pct, "%"),
      verdict = ifelse(improved, "IMPROVED", "NO CHANGE")
    )
  }

  rows <- rbindlist(list(
    cbind(data.table(model="KF", metric="Innovation normality (JB p-value)",
                     better_if="higher"),
          .fmt(m$kf$jb_before,       m$kf$jb_after,       TRUE)),
    cbind(data.table(model="KF", metric="Residual variance",
                     better_if="lower"),
          .fmt(m$kf$var_before,      m$kf$var_after,       FALSE)),
    cbind(data.table(model="KF", metric="Rolling variance ratio",
                     better_if="lower"),
          .fmt(m$kf$vr_before,       m$kf$vr_after,        FALSE)),
    cbind(data.table(model="ARIMA", metric="AIC per observation",
                     better_if="lower"),
          .fmt(m$arima$aic_full_scaled, m$arima$aic_regime_mean, FALSE)),
    cbind(data.table(model="ARIMA", metric="Ljung-Box p (residuals)",
                     better_if="higher"),
          .fmt(m$arima$lb_full,      m$arima$lb_within_mean, TRUE)),
    cbind(data.table(model="MS", metric="Regime persistence (days)",
                     better_if="higher"),
          .fmt(m$ms$persist_before,  m$ms$persist_after,   TRUE)),
    cbind(data.table(model="MS", metric="Spurious transition rate",
                     better_if="lower"),
          .fmt(m$ms$spurious_before, m$ms$spurious_after,  FALSE)),
    cbind(data.table(model="MS", metric="BP break agreement",
                     better_if="higher"),
          .fmt(m$ms$agree_before,    m$ms$agree_after,     TRUE)),
    cbind(data.table(model="ALL", metric="Signal-to-noise ratio",
                     better_if="higher"),
          .fmt(m$regime$snr_before,  m$regime$snr_after,   TRUE)),
    cbind(data.table(model="ALL", metric="Regime separation",
                     better_if="higher"),
          .fmt(m$regime$sep_before,  m$regime$sep_after,   TRUE)),
    cbind(data.table(model="ALL", metric="Within-regime variance",
                     better_if="lower"),
          .fmt(m$regime$within_before, m$regime$within_after, FALSE))
  ))

  # Overall score
  n_improved <- sum(rows$verdict == "IMPROVED", na.rm=TRUE)
  n_total    <- sum(rows$verdict != "N/A")
  cat("\nOverall:", n_improved, "/", n_total,
      "metrics improved after adjustments\n")

  rows
}

# ═══════════════════════════════════════════════════════════════════════════════
# COMPARISON PLOT
# ═══════════════════════════════════════════════════════════════════════════════

.plot_comparison <- function(y_c, ts_c, bp_breaks,
                               before, after_signals,
                               metrics, save_path) {

  times <- as.POSIXct(ts_c)
  bp_v  <- as.POSIXct(bp_breaks)
  x_ax  <- function() axis.POSIXct(1,
              at=seq(min(times),max(times),by="6 months"),
              format="%b %Y", cex.axis=0.65, las=2)

  png(save_path, width=1700, height=1800, res=115)
  par(mfrow=c(4,2), mar=c(2,4.5,2.5,1.5),
      oma=c(3,0,4,0), bg="white")

  # Row 1: KF before vs after
  plot(times, before$kf_before$kf_z, type="l", col="#0F6E56", lwd=0.6,
       main="KF BEFORE: constant H, raw data",
       xlab="", ylab="KF z-score", xaxt="n", las=1)
  abline(h=c(-1.5,0,1.5), lty=c(2,1,2), col="gray70", lwd=0.4)
  abline(v=bp_v, col="#E24B4A", lwd=0.8, lty=2)
  x_ax()

  plot(times, after_signals$kf_z, type="l", col="#0F6E56", lwd=0.6,
       main="KF AFTER: time-varying H, winsorised",
       xlab="", ylab="KF z-score", xaxt="n", las=1)
  abline(h=c(-1.5,0,1.5), lty=c(2,1,2), col="gray70", lwd=0.4)
  abline(v=bp_v, col="#E24B4A", lwd=0.8, lty=2)
  x_ax()

  # Row 2: ARIMA z-score before vs after
  arima_b <- before$arima_before$arima_z
  plot(times, arima_b, type="l", col="#B8860B", lwd=0.6,
       main="ARIMA BEFORE: full-sample rolling z-score",
       xlab="", ylab="Z-score", xaxt="n", las=1)
  abline(h=c(-1.5,0,1.5), lty=c(2,1,2), col="gray70", lwd=0.4)
  abline(v=bp_v, col="#E24B4A", lwd=0.8, lty=2)
  x_ax()

  plot(times, after_signals$arima_z, type="l", col="#B8860B", lwd=0.6,
       main="ARIMA AFTER: within-regime z-score",
       xlab="", ylab="Z-score", xaxt="n", las=1)
  abline(h=c(-1.5,0,1.5), lty=c(2,1,2), col="gray70", lwd=0.4)
  abline(v=bp_v, col="#E24B4A", lwd=0.8, lty=2)
  x_ax()

  # Row 3: MS regime before vs after
  ms_cols <- c("contango"="#E6F1FB","flat"="#F5F5F0","backwardation"="#FAECE7",
                "contango_flat"="#E6F1FB")

  .plot_ms_regime <- function(regime_vec, prob_vec, title) {
    plot(times, y_c, type="l", col="gray80", lwd=0.4,
         main=title, xlab="", ylab="M1M2", xaxt="n", las=1)
    # shade regimes
    rle_r <- rle(regime_vec)
    ends   <- cumsum(rle_r$lengths)
    starts <- c(1, head(ends,-1)+1)
    for (i in seq_along(rle_r$values)) {
      if (is.na(rle_r$values[i])) next
      col <- ms_cols[rle_r$values[i]]
      if (is.na(col)) col <- "#F5F5F0"
      rect(times[starts[i]], min(y_c,na.rm=TRUE),
           times[ends[i]],   max(y_c,na.rm=TRUE),
           col=adjustcolor(col,0.4), border=NA)
    }
    lines(times, y_c, col=adjustcolor("#185FA5",0.6), lwd=0.5)
    abline(v=bp_v, col="#E24B4A", lwd=0.8, lty=2)
    abline(h=0, col="gray60", lty=2, lwd=0.4)
    x_ax()
  }

  .plot_ms_regime(before$ms_before$ms_regime, before$ms_before$ms_prob_back,
                  "MS BEFORE: mean-only switching")
  .plot_ms_regime(after_signals$ms_regime, after_signals$ms_prob_back,
                  "MS AFTER: mean + variance switching")

  # Row 4: Scorecard bar chart
  sc   <- c(
    "KF: innov\nnormality" = metrics$kf$jb_after / max(metrics$kf$jb_before, 1e-10),
    "KF: var\nratio"       = metrics$kf$vr_before / max(metrics$kf$vr_after, 1e-10),
    "ARIMA: AIC\nper obs"  = metrics$arima$aic_full_scaled /
                               max(metrics$arima$aic_regime_mean, 1e-10),
    "ARIMA: LB\np-value"   = metrics$arima$lb_within_mean /
                               max(metrics$arima$lb_full, 1e-10),
    "MS: persist\ndays"    = metrics$ms$persist_after /
                               max(metrics$ms$persist_before, 1e-10),
    "MS: spurious\nrate"   = metrics$ms$spurious_before /
                               max(metrics$ms$spurious_after, 1e-10),
    "ALL: SNR"             = metrics$regime$snr_after /
                               max(metrics$regime$snr_before, 1e-10)
  )
  sc <- pmin(sc, 5)  # cap at 5x for display
  bar_col <- ifelse(sc >= 1, "#0F6E56", "#E24B4A")

  barplot(sc, names.arg=names(sc), col=bar_col,
          main="Improvement ratios (after/before, >1 = improved)",
          ylab="Ratio", las=2, cex.names=0.65, cex.axis=0.75,
          ylim=c(0, max(sc)*1.15))
  abline(h=1, col="#E24B4A", lty=2, lwd=1.5)
  legend("topright", c("Improved (>1×)","Not improved (<1×)"),
         fill=c("#0F6E56","#E24B4A"), bty="n", cex=0.75)

  # Summary text panel
  n_imp <- sum(unlist(lapply(metrics, function(m)
    sapply(m, function(v) is.numeric(v)))), na.rm=TRUE)
  plot(0, 0, type="n", axes=FALSE, xlab="", ylab="",
       main="Adjustment summary")
  text(0, 0.6, "A: Bai-Perron → HAC standard errors",    cex=0.8, adj=0.5)
  text(0, 0.3, "B: Kalman → winsorise + time-varying H",  cex=0.8, adj=0.5)
  text(0, 0.0, "C: Markov → switching variance (sw=TRUE)", cex=0.8, adj=0.5)
  text(0,-0.3, "D: ARIMA → within-regime fitting",        cex=0.8, adj=0.5)

  mtext("Model performance: BEFORE vs AFTER diagnostic adjustments — WTI M1M2",
        side=3, outer=TRUE, cex=1.05, font=2, line=2)
  mtext("Dashed red = Bai-Perron break dates | Green shading = backwardation | Blue = contango",
        side=3, outer=TRUE, cex=0.78, line=0.5)

  dev.off()
  cat("Saved:", save_path, "\n")
}
