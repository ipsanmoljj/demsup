# R/model_diagnostics.R
# ----------------------
# Tests all model assumptions before fitting.
# Each test saves its own PNG. Summary matrix shows pass/fail per model.
#
# Usage:
#   source("R/futures_reader.R")
#   source("R/structural_breaks.R")
#   source("R/model_diagnostics.R")
#   ff      <- read_futures_csv("CL_data.csv")
#   results <- run_break_detection(ff, resample_to = "1 day")
#   diag    <- run_diagnostics(results$data, results$consensus$high_confidence)

library(data.table)
library(zoo)

.install_if_missing <- function(pkgs) {
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    cat("Installing:", paste(missing, collapse = ", "), "\n")
    install.packages(missing, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
}
.install_if_missing(c("tseries", "FinTS", "strucchange", "moments", "nortest"))

# ═══════════════════════════════════════════════════════════════════════════════
# MAIN ENTRY POINT
# ═══════════════════════════════════════════════════════════════════════════════

run_diagnostics <- function(data,
                             bp_breaks  = NULL,
                             series     = "M1M2",
                             out_dir    = "output/diagnostics") {

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  y    <- as.numeric(data[[series]])
  ts_  <- as.Date(data$timestamp)
  valid <- !is.na(y)
  y_c  <- y[valid]
  ts_c <- ts_[valid]

  cat("\n", strrep("═", 65), "\n")
  cat("  ASSUMPTION DIAGNOSTICS —", series, "\n")
  cat(strrep("═", 65), "\n")

  # Run all tests — each saves its own plot
  r_quality  <- .test_quality(y_c, ts_c, series, out_dir)
  r_station  <- .test_stationarity(y_c, ts_c, series, out_dir)
  r_autocorr <- .test_autocorrelation(y_c, ts_c, series, out_dir)
  r_normal   <- .test_normality(y_c, series, out_dir)
  r_arch     <- .test_arch(y_c, ts_c, series, out_dir)
  r_stable   <- .test_stability(y_c, ts_c, series, out_dir)

  if (!is.null(bp_breaks) && length(bp_breaks) > 0)
    r_regime <- .test_within_regime(y_c, ts_c, bp_breaks, series, out_dir)
  else
    r_regime <- NULL

  # Build and print summary
  summary <- .build_summary(r_quality, r_station, r_autocorr,
                              r_normal, r_arch, r_stable)

  cat("\n\n", strrep("═", 65), "\n")
  cat("  ASSUMPTION MATRIX — which model assumptions are met\n")
  cat(strrep("═", 65), "\n\n")
  print(summary$matrix)

  cat("\n\n", strrep("═", 65), "\n")
  cat("  ADJUSTMENT REQUIREMENTS PER MODEL\n")
  cat(strrep("═", 65), "\n\n")
  print(summary$adjustments)

  # Save summary CSVs
  fwrite(summary$matrix,      file.path(out_dir, "assumption_matrix.csv"))
  fwrite(summary$adjustments, file.path(out_dir, "model_adjustments.csv"))
  cat("\nSaved: assumption_matrix.csv and model_adjustments.csv →", out_dir, "\n")

  invisible(list(
    quality    = r_quality,
    stationarity = r_station,
    autocorr   = r_autocorr,
    normality  = r_normal,
    arch       = r_arch,
    stability  = r_stable,
    within_regime = r_regime,
    summary    = summary
  ))
}

# ═══════════════════════════════════════════════════════════════════════════════
# TEST 1 — DATA QUALITY
# ═══════════════════════════════════════════════════════════════════════════════

.test_quality <- function(y_c, ts_c, series, out_dir) {
  cat("\n── TEST 1: DATA QUALITY ─────────────────────────────────\n")

  z          <- (y_c - mean(y_c)) / sd(y_c)
  n_outliers <- sum(abs(z) > 4)
  pct_chg    <- abs(diff(y_c) / replace(y_c[-length(y_c)], y_c[-length(y_c)] == 0, NA))
  n_spikes   <- sum(pct_chg > 0.10, na.rm = TRUE)
  date_gaps  <- as.integer(diff(ts_c))
  n_gaps     <- sum(date_gaps > 5, na.rm = TRUE)
  skew       <- moments::skewness(y_c)
  kurt       <- moments::kurtosis(y_c) - 3

  cat("  N observations  :", length(y_c), "\n")
  cat("  Outliers |z|>4  :", n_outliers, "\n")
  cat("  Roll spikes>10% :", n_spikes, "\n")
  cat("  Gaps > 5 days   :", n_gaps, "(weekends/holidays)\n")
  cat("  Skewness        :", round(skew, 3), "\n")
  cat("  Excess kurtosis :", round(kurt,  3), "(normal=0)\n")

  # Plot
  png(file.path(out_dir, "01_data_quality.png"), width=1400, height=500, res=110)
  par(mfrow=c(1,3), mar=c(4,4,3,1), oma=c(0,0,2,0), bg="white")

  times <- as.POSIXct(ts_c)
  plot(times, y_c, type="l", col="#185FA5", lwd=0.6,
       main="Time series + outliers", xlab="", ylab=series, xaxt="n")
  axis.POSIXct(1, at=seq(min(times), max(times), by="1 year"),
               format="%Y", cex.axis=0.75)
  points(times[abs(z)>4], y_c[abs(z)>4], col="#E24B4A", pch=16, cex=1.2)
  legend("topleft", c("Series","Outliers (|z|>4)"),
         col=c("#185FA5","#E24B4A"), lty=c(1,NA), pch=c(NA,16), cex=0.7, bty="n")

  hist(y_c, breaks=60, col="#D4E9F7", border="white",
       main=paste0("Distribution (skew=",round(skew,2),
                   ", kurt=",round(kurt,2),")"),
       xlab=series, freq=FALSE)
  curve(dnorm(x, mean(y_c), sd(y_c)), add=TRUE, col="#E24B4A", lwd=1.5)

  qqnorm(y_c, main="QQ Plot", pch=16, cex=0.3,
         col=adjustcolor("#185FA5", 0.4))
  qqline(y_c, col="#E24B4A", lwd=1.5)

  mtext(paste("Data Quality —", series), side=3, outer=TRUE, font=2, cex=1.0)
  dev.off()
  cat("  Saved: 01_data_quality.png\n")

  list(n=length(y_c), n_outliers=n_outliers, n_spikes=n_spikes,
       skewness=skew, excess_kurtosis=kurt,
       pass_outliers = n_outliers < 20,
       pass_spikes   = n_spikes < 50)
}

# ═══════════════════════════════════════════════════════════════════════════════
# TEST 2 — STATIONARITY
# ═══════════════════════════════════════════════════════════════════════════════

.test_stationarity <- function(y_c, ts_c, series, out_dir) {
  cat("\n── TEST 2: STATIONARITY ─────────────────────────────────\n")
  library(tseries)

  adf_p  <- tryCatch(adf.test(y_c,  alternative="stationary")$p.value, error=function(e) NA)
  kpss_p <- tryCatch(kpss.test(y_c)$p.value,                           error=function(e) NA)
  pp_p   <- tryCatch(pp.test(y_c)$p.value,                             error=function(e) NA)

  adf_pass  <- !is.na(adf_p)  && adf_p  < 0.05
  kpss_pass <- !is.na(kpss_p) && kpss_p > 0.05
  pp_pass   <- !is.na(pp_p)   && pp_p   < 0.05
  stationary <- sum(c(adf_pass, kpss_pass, pp_pass), na.rm=TRUE) >= 2

  # First difference
  dy      <- diff(y_c)
  adf_d_p <- tryCatch(adf.test(dy, alternative="stationary")$p.value, error=function(e) NA)

  cat("  ADF   (H0:unit root,  p<0.05=stationary) p =", round(adf_p,4),
      "→", ifelse(adf_pass,  "PASS ✓","FAIL ✗"), "\n")
  cat("  KPSS  (H0:stationary, p>0.05=stationary) p =", round(kpss_p,4),
      "→", ifelse(kpss_pass, "PASS ✓","FAIL ✗"), "\n")
  cat("  PP    (H0:unit root,  p<0.05=stationary) p =", round(pp_p,4),
      "→", ifelse(pp_pass,   "PASS ✓","FAIL ✗"), "\n")
  cat("  Verdict:", ifelse(stationary, "STATIONARY", "NON-STATIONARY"), "\n")
  cat("  1st diff ADF p =", round(adf_d_p,4),
      ifelse(!is.na(adf_d_p) && adf_d_p<0.05, "(stationary after diff)",""), "\n")

  # Rolling mean plot
  roll_mean <- zoo::rollmean(y_c, 63, fill=NA, align="right")
  roll_sd   <- zoo::rollapply(y_c, 63, sd, fill=NA, align="right")
  times     <- as.POSIXct(ts_c)

  png(file.path(out_dir, "02_stationarity.png"), width=1400, height=600, res=110)
  par(mfrow=c(1,2), mar=c(4,4,3,1), oma=c(0,0,2,0), bg="white")

  plot(times, y_c, type="l", col=adjustcolor("#185FA5",0.4), lwd=0.5,
       main="Rolling mean (non-stationarity check)", xlab="", ylab=series, xaxt="n")
  lines(times, roll_mean, col="#E24B4A", lwd=1.5)
  axis.POSIXct(1, at=seq(min(times),max(times),by="1 year"), format="%Y", cex.axis=0.75)
  legend("topleft", c("Series","63-day mean"), col=c("#185FA5","#E24B4A"),
         lty=1, cex=0.75, bty="n")

  plot(times, roll_sd, type="l", col="#8B4513", lwd=1.0,
       main="Rolling SD (variance stationarity)", xlab="", ylab="SD", xaxt="n")
  abline(h=mean(roll_sd, na.rm=TRUE), col="#E24B4A", lty=2)
  axis.POSIXct(1, at=seq(min(times),max(times),by="1 year"), format="%Y", cex.axis=0.75)

  mtext(paste("Stationarity Tests —", series,
              "| ADF:", round(adf_p,3),
              "| KPSS:", round(kpss_p,3),
              "| Verdict:", ifelse(stationary,"STATIONARY","NON-STATIONARY")),
        side=3, outer=TRUE, font=2, cex=0.9)
  dev.off()
  cat("  Saved: 02_stationarity.png\n")

  list(adf_p=adf_p, kpss_p=kpss_p, pp_p=pp_p,
       stationary=stationary, adf_diff_p=adf_d_p, pass=stationary)
}

# ═══════════════════════════════════════════════════════════════════════════════
# TEST 3 — AUTOCORRELATION
# ═══════════════════════════════════════════════════════════════════════════════

.test_autocorrelation <- function(y_c, ts_c, series, out_dir) {
  cat("\n── TEST 3: AUTOCORRELATION ──────────────────────────────\n")

  lags   <- c(1, 5, 10, 21, 63)
  lb_p   <- sapply(lags, function(l)
    tryCatch(Box.test(y_c, lag=l, type="Ljung-Box")$p.value, error=function(e) NA))

  acf1      <- acf(y_c, lag.max=1, plot=FALSE)$acf[2]
  half_life <- -log(2) / log(abs(acf1))

  cat("  Ljung-Box test (H0: no autocorrelation):\n")
  for (i in seq_along(lags))
    cat(sprintf("    Lag %2d: p = %.4f → %s\n", lags[i], lb_p[i],
                ifelse(!is.na(lb_p[i]) && lb_p[i] < 0.05,
                       "AUTOCORRELATED", "no autocorr")))
  cat("  ACF(1)         :", round(acf1, 4), "\n")
  cat("  Half-life      :", round(half_life, 1), "bars\n")

  png(file.path(out_dir, "03_autocorrelation.png"), width=1400, height=600, res=110)
  par(mfrow=c(1,3), mar=c(4,4,3,1), oma=c(0,0,2,0), bg="white")

  acf(y_c,  lag.max=40, main="ACF",  col="#185FA5", lwd=1.2)
  pacf(y_c, lag.max=40, main="PACF", col="#0F6E56", lwd=1.2)

  # Ljung-Box p-values across lags
  lb_lags <- 1:30
  lb_all  <- sapply(lb_lags, function(l)
    tryCatch(Box.test(y_c, lag=l, type="Ljung-Box")$p.value, error=function(e) NA))
  plot(lb_lags, lb_all, type="b", pch=16, cex=0.7,
       col=ifelse(lb_all < 0.05, "#E24B4A", "#185FA5"),
       main="Ljung-Box p-values by lag",
       xlab="Lag", ylab="p-value", ylim=c(0,1))
  abline(h=0.05, col="#E24B4A", lty=2)
  legend("topright", c("p<0.05 (autocorr)","p≥0.05"),
         col=c("#E24B4A","#185FA5"), pch=16, cex=0.75, bty="n")

  mtext(paste("Autocorrelation —", series,
              "| ACF(1):", round(acf1,3),
              "| Half-life:", round(half_life,1), "bars"),
        side=3, outer=TRUE, font=2, cex=0.9)
  dev.off()
  cat("  Saved: 03_autocorrelation.png\n")

  list(lb_p=lb_p, acf1=acf1, half_life=half_life,
       high_persistence = abs(acf1) > 0.5,
       pass = TRUE)   # autocorrelation is expected — not a pass/fail
}

# ═══════════════════════════════════════════════════════════════════════════════
# TEST 4 — NORMALITY
# ═══════════════════════════════════════════════════════════════════════════════

.test_normality <- function(y_c, series, out_dir) {
  cat("\n── TEST 4: NORMALITY ────────────────────────────────────\n")
  library(moments); library(nortest)

  jb_p <- tryCatch(jarque.test(y_c)$p.value,    error=function(e) NA)
  ad_p <- tryCatch(ad.test(y_c)$p.value,         error=function(e) NA)
  sw_sample <- if (length(y_c) > 5000) sample(y_c, 5000) else y_c
  sw_p <- tryCatch(shapiro.test(sw_sample)$p.value, error=function(e) NA)

  skew <- skewness(y_c)
  kurt <- kurtosis(y_c) - 3

  cat("  Jarque-Bera      p =", round(jb_p, 6),
      "→", ifelse(!is.na(jb_p) && jb_p > 0.05, "NORMAL ✓", "NON-NORMAL ✗"), "\n")
  cat("  Anderson-Darling p =", round(ad_p, 6),
      "→", ifelse(!is.na(ad_p) && ad_p > 0.05, "NORMAL ✓", "NON-NORMAL ✗"), "\n")
  cat("  Shapiro-Wilk     p =", round(sw_p, 6),
      "→", ifelse(!is.na(sw_p) && sw_p > 0.05, "NORMAL ✓", "NON-NORMAL ✗"), "\n")
  cat("  Skewness         :", round(skew, 4), "\n")
  cat("  Excess kurtosis  :", round(kurt, 4), "(fat tails if > 1)\n")

  png(file.path(out_dir, "04_normality.png"), width=1400, height=500, res=110)
  par(mfrow=c(1,3), mar=c(4,4,3,1), oma=c(0,0,2,0), bg="white")

  hist(y_c, breaks=80, col="#D4E9F7", border="white",
       main=paste0("Histogram (skew=",round(skew,2),
                   ", exkurt=",round(kurt,2),")"),
       xlab=series, freq=FALSE)
  curve(dnorm(x, mean(y_c), sd(y_c)), add=TRUE, col="#E24B4A", lwd=2)

  qqnorm(y_c, main="QQ Plot vs Normal", pch=16, cex=0.25,
         col=adjustcolor("#185FA5", 0.4))
  qqline(y_c, col="#E24B4A", lwd=1.5)

  # Empirical CDF vs normal CDF
  plot(ecdf(y_c), main="ECDF vs Normal CDF", xlab=series, ylab="CDF",
       col="#185FA5", lwd=1.0)
  curve(pnorm(x, mean(y_c), sd(y_c)), add=TRUE, col="#E24B4A", lwd=1.5)
  legend("topleft", c("Empirical","Normal"),
         col=c("#185FA5","#E24B4A"), lty=1, cex=0.75, bty="n")

  mtext(paste("Normality Tests —", series,
              "| JB p:", format(round(jb_p,4), scientific=FALSE),
              "| AD p:", format(round(ad_p,4), scientific=FALSE)),
        side=3, outer=TRUE, font=2, cex=0.9)
  dev.off()
  cat("  Saved: 04_normality.png\n")

  list(jb_p=jb_p, ad_p=ad_p, sw_p=sw_p, skewness=skew, excess_kurtosis=kurt,
       pass = !is.na(jb_p) && jb_p > 0.05)
}

# ═══════════════════════════════════════════════════════════════════════════════
# TEST 5 — ARCH / HETEROSKEDASTICITY
# ═══════════════════════════════════════════════════════════════════════════════

.test_arch <- function(y_c, ts_c, series, out_dir) {
  cat("\n── TEST 5: ARCH EFFECTS (HETEROSKEDASTICITY) ────────────\n")
  library(FinTS)

  lags   <- c(5, 10, 21)
  arch_p <- sapply(lags, function(l)
    tryCatch(ArchTest(y_c, lags=l)$p.value, error=function(e) NA))

  roll_var  <- zoo::rollapply(y_c, 21, var, fill=NA, align="right")
  var_ratio <- max(roll_var, na.rm=TRUE) / max(min(roll_var, na.rm=TRUE), 1e-10)
  arch_present <- any(!is.na(arch_p) & arch_p < 0.05)

  cat("  ARCH-LM test (H0: no ARCH effects):\n")
  for (i in seq_along(lags))
    cat(sprintf("    Lag %2d: p = %.4f → %s\n", lags[i], arch_p[i],
                ifelse(!is.na(arch_p[i]) && arch_p[i] < 0.05,
                       "ARCH PRESENT ✗", "No ARCH ✓")))
  cat("  Max/min rolling variance ratio:", round(var_ratio, 1), "\n")
  cat("  Verdict:", ifelse(arch_present, "VOLATILITY CLUSTERING PRESENT", "HOMOSKEDASTIC"), "\n")

  times <- as.POSIXct(ts_c)
  png(file.path(out_dir, "05_arch_effects.png"), width=1400, height=600, res=110)
  par(mfrow=c(1,3), mar=c(4,4,3,1), oma=c(0,0,2,0), bg="white")

  plot(times, roll_var, type="l", col="#8B4513", lwd=0.8,
       main="Rolling 21-day variance", xlab="", ylab="Variance", xaxt="n")
  abline(h=mean(roll_var, na.rm=TRUE), col="#E24B4A", lty=2)
  axis.POSIXct(1, at=seq(min(times),max(times),by="1 year"), format="%Y", cex.axis=0.75)

  plot(times, y_c^2, type="l", col="#6B2D8B", lwd=0.5,
       main="Squared series (clustering = ARCH)", xlab="", ylab=paste0(series,"²"), xaxt="n")
  axis.POSIXct(1, at=seq(min(times),max(times),by="1 year"), format="%Y", cex.axis=0.75)

  acf(y_c^2, lag.max=30, main="ACF of squared series",
      col="#6B2D8B", lwd=1.2)

  mtext(paste("ARCH Effects —", series,
              "| Variance ratio:", round(var_ratio,1),
              "| ARCH:", ifelse(arch_present,"PRESENT","ABSENT")),
        side=3, outer=TRUE, font=2, cex=0.9)
  dev.off()
  cat("  Saved: 05_arch_effects.png\n")

  list(arch_p=arch_p, var_ratio=var_ratio, arch_present=arch_present,
       pass = !arch_present)
}

# ═══════════════════════════════════════════════════════════════════════════════
# TEST 6 — STRUCTURAL STABILITY
# ═══════════════════════════════════════════════════════════════════════════════

.test_stability <- function(y_c, ts_c, series, out_dir) {
  cat("\n── TEST 6: STRUCTURAL STABILITY ─────────────────────────\n")
  library(strucchange)

  cusum_p <- tryCatch({
    ef <- efp(y_c ~ 1, type="OLS-CUSUM")
    list(p=sctest(ef)$p.value, ef=ef)
  }, error=function(e) list(p=NA, ef=NULL))

  mosum_p <- tryCatch({
    ef2 <- efp(y_c ~ 1, type="OLS-MOSUM", h=0.15)
    list(p=sctest(ef2)$p.value, ef=ef2)
  }, error=function(e) list(p=NA, ef=NULL))

  unstable <- (!is.na(cusum_p$p) && cusum_p$p < 0.05) ||
              (!is.na(mosum_p$p) && mosum_p$p < 0.05)

  cat("  CUSUM test (H0: stable) p =", round(cusum_p$p, 4),
      "→", ifelse(!is.na(cusum_p$p) && cusum_p$p < 0.05,
                  "UNSTABLE ✗","STABLE ✓"), "\n")
  cat("  MOSUM test (H0: stable) p =", round(mosum_p$p, 4),
      "→", ifelse(!is.na(mosum_p$p) && mosum_p$p < 0.05,
                  "UNSTABLE ✗","STABLE ✓"), "\n")
  cat("  Verdict:", ifelse(unstable, "STRUCTURAL BREAKS PRESENT (expected)", "STABLE"), "\n")

  png(file.path(out_dir, "06_structural_stability.png"), width=1400, height=600, res=110)
  par(mfrow=c(1,2), mar=c(4,4,3,1), oma=c(0,0,2,0), bg="white")

  if (!is.null(cusum_p$ef)) {
    plot(cusum_p$ef, main="CUSUM test", col="#185FA5",
         boundary.col="#E24B4A", boundary.lty=2)
  } else {
    plot(1, type="n", main="CUSUM (unavailable)", xlab="", ylab="")
  }

  if (!is.null(mosum_p$ef)) {
    plot(mosum_p$ef, main="MOSUM test", col="#0F6E56",
         boundary.col="#E24B4A", boundary.lty=2)
  } else {
    plot(1, type="n", main="MOSUM (unavailable)", xlab="", ylab="")
  }

  mtext(paste("Structural Stability —", series,
              "| CUSUM p:", round(cusum_p$p,4),
              "| MOSUM p:", round(mosum_p$p,4)),
        side=3, outer=TRUE, font=2, cex=0.9)
  dev.off()
  cat("  Saved: 06_structural_stability.png\n")

  list(cusum_p=cusum_p$p, mosum_p=mosum_p$p, unstable=unstable,
       pass = !unstable)
}

# ═══════════════════════════════════════════════════════════════════════════════
# TEST 7 — WITHIN-REGIME DIAGNOSTICS
# ═══════════════════════════════════════════════════════════════════════════════

.test_within_regime <- function(y_c, ts_c, bp_breaks, series, out_dir) {
  cat("\n── TEST 7: WITHIN-REGIME DIAGNOSTICS ───────────────────\n")
  library(tseries)

  boundaries <- c(as.Date("2000-01-01"), sort(bp_breaks), as.Date("2100-01-01"))
  n_reg <- length(boundaries) - 1

  cat(sprintf("  %-4s %-11s %-11s %5s %7s %6s %8s %8s\n",
              "Reg","Start","End","N","Mean","SD","ADF_p","LB5_p"))
  cat("  ", strrep("-", 65), "\n")

  stats <- lapply(seq_len(n_reg), function(i) {
    idx   <- ts_c > boundaries[i] & ts_c <= boundaries[i+1]
    y_s   <- y_c[idx]; ts_s <- ts_c[idx]
    if (length(y_s) < 10) return(NULL)
    adf_p <- tryCatch(adf.test(y_s, alternative="stationary")$p.value, error=function(e) NA)
    lb_p  <- tryCatch(Box.test(y_s, lag=5, type="Ljung-Box")$p.value,  error=function(e) NA)
    cat(sprintf("  %-4d %-11s %-11s %5d %7.3f %6.3f %8s %8s\n",
                i, format(min(ts_s)), format(max(ts_s)), length(y_s),
                mean(y_s), sd(y_s),
                ifelse(!is.na(adf_p), paste0(round(adf_p,3), ifelse(adf_p<0.05," ✓"," ✗")), "NA"),
                ifelse(!is.na(lb_p),  paste0(round(lb_p,3),  ifelse(lb_p<0.05," ✗"," ✓")), "NA")))
    list(n=length(y_s), mean=mean(y_s), sd=sd(y_s), adf_p=adf_p, lb_p=lb_p,
         stationary=!is.na(adf_p) && adf_p < 0.05)
  })

  sds <- sapply(Filter(Negate(is.null), stats), `[[`, "sd")
  cat("\n  SD ratio across regimes:", round(max(sds)/min(sds), 2),
      ifelse(max(sds)/min(sds) > 3, "— HIGH heteroskedasticity", "— OK"), "\n")

  # Plot within-regime distributions
  png(file.path(out_dir, "07_within_regime.png"), width=1600, height=500, res=110)
  par(mar=c(4,4,3,1), oma=c(0,0,2,0), bg="white")

  cols <- colorRampPalette(c("#185FA5","#0F6E56","#E24B4A","#8B4513",
                              "#6B2D8B","#B8860B","#4B0082","#2F4F4F","#8B0000"))(n_reg)
  times <- as.POSIXct(ts_c)

  plot(times, y_c, type="l", col="gray80", lwd=0.4,
       main="Within-regime distributions", xlab="", ylab=series, xaxt="n")
  axis.POSIXct(1, at=seq(min(times),max(times),by="1 year"), format="%Y", cex.axis=0.75)

  for (i in seq_len(n_reg)) {
    idx <- ts_c > boundaries[i] & ts_c <= boundaries[i+1]
    if (sum(idx) < 5) next
    lines(times[idx], y_c[idx], col=cols[i], lwd=0.7)
    seg_mean <- mean(y_c[idx], na.rm=TRUE)
    abline(h=seg_mean, col=adjustcolor(cols[i], 0.5), lty=2, lwd=0.8)
  }

  mtext(paste("Within-Regime Analysis —", series, "| Regimes:", n_reg),
        side=3, outer=TRUE, font=2, cex=1.0)
  dev.off()
  cat("  Saved: 07_within_regime.png\n")

  stats
}

# ═══════════════════════════════════════════════════════════════════════════════
# SUMMARY MATRIX
# ═══════════════════════════════════════════════════════════════════════════════

.build_summary <- function(r_q, r_s, r_a, r_n, r_ar, r_st) {

  # ── Assumption matrix: rows = assumptions, cols = models ──────────────────
  fmt <- function(pass, note="") {
    if (is.na(pass)) return("N/A")
    if (isTRUE(pass))  return(paste0("MET ✓",  ifelse(nchar(note)>0, paste0(" (",note,")"), "")))
    if (isFALSE(pass)) return(paste0("FAIL ✗", ifelse(nchar(note)>0, paste0(" (",note,")"), "")))
    "NOTE"
  }

  # Each cell: is assumption met for that model?
  stat_note   <- ifelse(r_s$pass, "", "use d=1")
  arch_note   <- ifelse(r_ar$pass, "", "switch var")
  norm_note   <- ifelse(r_n$pass, "", "HAC SEs")
  stable_note <- "expected"

  mat <- data.table(
    Assumption = c(
      "Stationarity",
      "No autocorrelation",
      "Gaussian errors",
      "Homoskedasticity",
      "Structural stability",
      "No outliers"
    ),
    `Model A\nBai-Perron` = c(
      fmt(TRUE,  "HAC robust"),
      fmt(TRUE,  "HAC robust"),
      fmt(r_n$pass, norm_note),
      fmt(TRUE,  "HAC robust"),
      fmt(FALSE, stable_note),
      fmt(r_q$pass_outliers)
    ),
    `Model B\nKalman Filter` = c(
      fmt(TRUE,  "handles drift"),
      fmt(TRUE,  "state equation"),
      fmt(r_n$pass, norm_note),
      fmt(!r_ar$arch_present, arch_note),
      fmt(TRUE,  "time-varying"),
      fmt(r_q$pass_outliers, "winsorise")
    ),
    `Model C\nMarkov Switch` = c(
      fmt(TRUE,  "by design"),
      fmt(TRUE,  "within-state"),
      fmt(r_n$pass, "within-state"),
      fmt(!r_ar$arch_present, arch_note),
      fmt(TRUE,  "by design"),
      fmt(r_q$pass_outliers)
    ),
    `Model D\nARIMA` = c(
      fmt(r_s$pass, stat_note),
      fmt(FALSE, "fit within regime"),
      fmt(r_n$pass, norm_note),
      fmt(!r_ar$arch_present, "rolling SD"),
      fmt(FALSE, "within-regime only"),
      fmt(r_q$pass_outliers)
    )
  )

  # ── Adjustments table: what to change per model ────────────────────────────
  adj <- data.table(
    Model = c("A: Bai-Perron", "B: Kalman Filter",
              "C: Markov Switch", "D: ARIMA"),
    Status = c(
      ifelse(r_n$pass && r_q$pass_outliers, "PROCEED AS-IS", "PROCEED WITH HAC"),
      ifelse(r_n$pass && r_ar$pass,         "PROCEED AS-IS", "PROCEED WITH CAUTION"),
      ifelse(r_q$pass_outliers,             "PROCEED AS-IS", "PROCEED — CHECK STATES"),
      "FIT WITHIN EACH REGIME SEPARATELY"
    ),
    Required_adjustment = c(
      ifelse(!r_n$pass,
             "Newey-West HAC standard errors (non-Gaussian errors)",
             "None required"),
      paste0(
        ifelse(!r_n$pass, "Winsorise outliers before MLE; ", ""),
        ifelse(r_ar$arch_present, "Consider time-varying H matrix; ", ""),
        ifelse(!r_n$pass || r_ar$arch_present, "Check innovation residuals", "None required")
      ),
      paste0(
        ifelse(r_ar$arch_present, "Switching variance enabled (sw=TRUE) — OK; ", ""),
        ifelse(!r_n$pass, "Check within-state residual normality; ", ""),
        "Verify state means are well-separated"
      ),
      paste0(
        ifelse(!r_s$pass, "Use d=1 (non-stationary series); ", ""),
        "Fit separate ARIMA per BP regime; ",
        "Use rolling 63-day z-score for cross-regime comparison"
      )
    ),
    Key_concern = c(
      ifelse(!r_n$pass,
             paste0("Fat tails (excess kurt=", round(r_n$excess_kurtosis,1),
                    ") inflate type-I error"),
             "None"),
      ifelse(r_ar$arch_present,
             paste0("Variance ratio=", round(r_ar$var_ratio,1),
                    " — KF H may be misspecified in volatile periods"),
             ifelse(!r_n$pass,
                    "Non-Gaussian → MLE biased; outliers distort smoothed state",
                    "None")),
      ifelse(!r_q$pass_outliers,
             paste0(r_q$n_outliers, " outliers may create spurious states"),
             ifelse(r_ar$arch_present,
                    "Volatility clustering may cause state misclassification",
                    "None")),
      paste0("ACF(1)=", round(r_a$acf1, 2),
             " — high persistence; structural breaks violate stationarity assumption")
    )
  )

  list(matrix=mat, adjustments=adj)
}
