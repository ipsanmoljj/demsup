# R/cftc_cl_pipeline.R
# ─────────────────────────────────────────────────────────────────────────────
# BUILD WTI WEEKLY PRICE SERIES FROM CL_DATA.CSV, THEN REDO ALL CFTC ANALYSIS
#
# Note: cl_data.csv was Excel-exported; file has 1,048,576 rows (Excel row cap)
#       covering 2021-01-04 to 2024-02-01 (158 weekly obs after merge).
#
# Aggregation:
#   Intraday → daily  :  TWAP (mean of all 1-min bars per calendar day, UTC)
#   Daily    → weekly :  TWAP of last trading day in each ISO week
#   Alignment         :  join CFTC release_date ↔ weekly on ISO week
#                        (robust to market holiday Fridays)
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table); library(readxl)
  library(lmtest); library(sandwich)
  library(strucchange); library(segmented); library(MSwM)
  library(nortest); library(car)
})

setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
dir.create("output/cftc", showWarnings=FALSE)

# ═══════════════════════════════════════════════════════════════════════════════
# PART A: BUILD WEEKLY WTI PRICE FROM CL_DATA.CSV
# ═══════════════════════════════════════════════════════════════════════════════
cat("Reading cl_data.csv...\n")
cl_raw <- fread("cl_data.csv", skip=1, header=TRUE,
                select=c("timestamp","c1||contract","c1||weighted_mid"))
setnames(cl_raw, c("timestamp","contract","mid"))
cl_raw[, mid := as.numeric(mid)]
cl_raw <- cl_raw[!is.na(mid)]
cat(sprintf("  Rows read: %s\n", format(nrow(cl_raw), big.mark=",")))

# Unix epoch seconds → UTC date
cl_raw[, ts_utc := as.POSIXct(as.numeric(timestamp), origin="1970-01-01", tz="UTC")]
cl_raw[, date   := as.Date(ts_utc, tz="UTC")]
cat(sprintf("  Date range: %s to %s\n", min(cl_raw$date), max(cl_raw$date)))
cat(sprintf("  M1 price range: $%.2f to $%.2f\n",
            min(cl_raw$mid), max(cl_raw$mid)))

# Daily TWAP: time-weighted average = simple mean of all intraday 1-min bars
daily_twap <- cl_raw[, .(
  twap     = mean(mid, na.rm=TRUE),
  n_bars   = .N,
  contract = last(contract)
), keyby=date]
cat(sprintf("  Daily TWAP rows: %d\n", nrow(daily_twap)))

# Weekly: last trading day of each ISO week → TWAP of that day
daily_twap[, iso_week := format(date, "%G-W%V")]
weekly <- daily_twap[, .(
  friday      = max(date),           # last trading day of the ISO week
  twap_weekly = last(twap),          # its TWAP = weekly close
  n_days      = .N
), keyby=iso_week]
weekly[, friday := as.Date(friday)]
weekly <- weekly[order(iso_week)]
cat(sprintf("  Weekly rows: %d  (%s to %s)\n",
            nrow(weekly), min(weekly$friday), max(weekly$friday)))

# Forward returns on the ordered weekly series (leads computed BEFORE merge)
weekly[, price_1w := shift(twap_weekly, -1L, type="lead")]
weekly[, price_2w := shift(twap_weekly, -2L, type="lead")]
weekly[, price_4w := shift(twap_weekly, -4L, type="lead")]
weekly[, ret_1w   := (price_1w - twap_weekly) / twap_weekly]
weekly[, ret_2w   := (price_2w - twap_weekly) / twap_weekly]
weekly[, ret_4w   := (price_4w - twap_weekly) / twap_weekly]
fwrite(weekly[, .(iso_week, friday, twap_weekly, n_days, ret_1w, ret_2w, ret_4w)],
       "output/cftc/cl_weekly_twap.csv")
cat("  Saved: output/cftc/cl_weekly_twap.csv\n")

# ═══════════════════════════════════════════════════════════════════════════════
# PART B: LOAD CFTC + MERGE ON ISO WEEK
# ═══════════════════════════════════════════════════════════════════════════════
cat("\nLoading CFTC data...\n")
cftc_raw <- as.data.table(read_excel("CFTC 2016-2026 CL.xlsx", sheet="Sheet1"))
cftc_raw[, release_date := as.Date(releasedate)]
cftc_raw[, pos_date     := as.Date(date)]
cftc_raw[, net_pos      := as.numeric(actual)]
cftc <- unique(cftc_raw[!is.na(net_pos)][order(release_date)], by="release_date")
cftc[, iso_week := format(release_date, "%G-W%V")]
cat(sprintf("  CFTC: %d rows  %s to %s\n", nrow(cftc),
            min(cftc$release_date), max(cftc$release_date)))

# Join on ISO week: robust to holiday Fridays where market closed ≠ CFTC release
mf <- merge(
  cftc[, .(release_date, pos_date, net_pos, iso_week)],
  weekly[, .(iso_week, friday, twap_weekly, ret_1w, ret_2w, ret_4w)],
  by="iso_week", all.x=FALSE
)
mf <- mf[!is.na(twap_weekly)][order(release_date)]
cat(sprintf("  Merged panel: %d rows  %s to %s\n",
            nrow(mf), min(mf$release_date), max(mf$release_date)))

# Positioning metrics
zsc <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)

# Rolling 52-week z-score; falls back to full available window when n < 52
roll_z52 <- function(x, w=52) {
  n <- length(x); z <- rep(NA_real_, n)
  for (i in seq_along(x)) {
    avail <- max(2L, min(w, i))      # need at least 2 obs for sd
    win   <- x[(i - avail + 1L):i]
    if (sum(!is.na(win)) >= 2L)
      z[i] <- (x[i] - mean(win, na.rm=TRUE)) / sd(win, na.rm=TRUE)
  }; z
}

mf[, net_pos_chg := net_pos - shift(net_pos, 1L)]
mf[, pos_pct     := frank(net_pos, ties.method="average") / .N]
mf[, pos_z       := zsc(net_pos)]
mf[, pos_z52     := roll_z52(net_pos)]

mf[, regime := fcase(
  pos_pct >= 0.90, "Extreme Long  (>90th)",
  pos_pct <= 0.10, "Extreme Short (<10th)",
  pos_pct >= 0.60, "Long  (60-90th)",
  pos_pct <= 0.40, "Short (10-40th)",
  default          = "Neutral (40-60th)"
)]

fwrite(mf, "output/cftc/cftc_cl_merged.csv")
cat("  Saved: output/cftc/cftc_cl_merged.csv\n")

cat("\n=== POSITIONING SUMMARY ===\n")
cat(sprintf("  Total obs: %d   |   with ret_4w: %d\n", nrow(mf), sum(!is.na(mf$ret_4w))))
cat(sprintf("  Extreme Long (>90th):  %d obs\n", sum(mf$pos_pct >= 0.90)))
cat(sprintf("  Extreme Short (<10th): %d obs\n", sum(mf$pos_pct <= 0.10)))
cat(sprintf("  net_pos range: %d to %d contracts\n", min(mf$net_pos), max(mf$net_pos)))
cat(sprintf("  WTI TWAP range: $%.2f to $%.2f\n",
            min(mf$twap_weekly, na.rm=TRUE), max(mf$twap_weekly, na.rm=TRUE)))

cat("\n=== REGIME RETURNS ===\n")
regime_stats <- mf[!is.na(ret_4w), .(
  n         = .N,
  avg_1w    = round(mean(ret_1w, na.rm=TRUE)*100, 2),
  avg_2w    = round(mean(ret_2w, na.rm=TRUE)*100, 2),
  avg_4w    = round(mean(ret_4w, na.rm=TRUE)*100, 2),
  med_4w    = round(median(ret_4w, na.rm=TRUE)*100, 2),
  hit_up_4w = round(mean(ret_4w > 0, na.rm=TRUE)*100, 1)
), keyby=regime]
print(as.data.frame(regime_stats))
fwrite(regime_stats, "output/cftc/cftc_cl_regime_returns.csv")

# ═══════════════════════════════════════════════════════════════════════════════
# PART C: ATTACH MACRO FACTORS + BASE MODELS
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n=== BASE MODELS ===\n")
fac <- fread("output/factors_extended.csv")
fac[, date     := as.Date(date, origin="1970-01-01")]
fac[, iso_week := format(date, "%G-W%V")]
use_cols <- c("dxy_4wk_chg","sofr","td3c_z52","sin_ann","cos_ann")
use_cols <- use_cols[use_cols %in% names(fac)]
fac_wk <- fac[, lapply(.SD, function(x) last(x[!is.na(x)])), by=iso_week, .SDcols=use_cols]
# Join factors to merged panel on iso_week
mf <- merge(mf, fac_wk, by="iso_week", all.x=TRUE)

mf[, mm_chg_z := zsc(net_pos_chg)]
mf[, dxy_z    := if ("dxy_4wk_chg" %in% names(mf)) zsc(dxy_4wk_chg) else NA_real_]
mf[, sofr_z   := if ("sofr"         %in% names(mf)) zsc(sofr)         else NA_real_]
mf[, td3c_z   := if ("td3c_z52"    %in% names(mf)) td3c_z52          else NA_real_]

sub1 <- mf[!is.na(ret_4w) & !is.na(pos_z)]
sub3_vars <- c("ret_4w","pos_z","dxy_z","sofr_z","td3c_z","sin_ann","cos_ann")
sub3_vars_ok <- sub3_vars[sub3_vars %in% names(mf)]
sub3 <- mf[complete.cases(mf[, ..sub3_vars_ok]) & !is.na(ret_4w)]

cat(sprintf("  sub1 (CFTC only): n=%d\n  sub3 (CFTC+Macro): n=%d\n", nrow(sub1), nrow(sub3)))

m1 <- lm(ret_4w ~ pos_z, data=sub1)
cat(sprintf("Model 1: n=%d  R2=%.4f  pos_z coef=%+.4f  p=%.3f\n",
            nobs(m1), summary(m1)$r.squared,
            coef(summary(m1))["pos_z",1], coef(summary(m1))["pos_z",4]))

macro_rhs <- paste(intersect(c("dxy_z","sofr_z","td3c_z","sin_ann","cos_ann"), names(sub3)),
                   collapse=" + ")
m3_form <- as.formula(paste("ret_4w ~ pos_z +", macro_rhs))
m3 <- if (nrow(sub3) > 10 && macro_rhs != "") {
  m <- lm(m3_form, data=sub3)
  cat(sprintf("Model 3: n=%d  R2=%.4f  pos_z coef=%+.4f  p=%.3f\n",
              nobs(m), summary(m)$r.squared,
              coef(summary(m))["pos_z",1], coef(summary(m))["pos_z",4]))
  m
} else { cat("  Model 3 skipped (insufficient macro data)\n"); NULL }

# ═══════════════════════════════════════════════════════════════════════════════
# PART D: OLS DIAGNOSTICS + PLOTS
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("PART D: OLS DIAGNOSTIC TESTS\n")
cat("══════════════════════════════════════════════════\n")

run_diag <- function(mod, label) {
  res <- residuals(mod); p <- length(coef(mod))
  sw  <- shapiro.test(res)
  ad  <- ad.test(res)
  bp  <- bptest(mod)
  dw  <- dwtest(mod, alternative="two.sided")
  lb4 <- Box.test(res, lag=4, type="Ljung-Box")
  cat(sprintf("\n[%s]\n", label))
  cat(sprintf("  SW normality: W=%.4f p=%.4f  %s\n", sw$statistic, sw$p.value,
      ifelse(sw$p.value<0.05,"NON-NORMAL","OK")))
  cat(sprintf("  AD normality: A=%.4f p=%.4f  %s\n", ad$statistic, ad$p.value,
      ifelse(ad$p.value<0.05,"NON-NORMAL","OK")))
  cat(sprintf("  Skew/Kurt:    %.3f / %.3f\n",
      mean((res-mean(res))^3)/sd(res)^3, mean((res-mean(res))^4)/sd(res)^4))
  cat(sprintf("  Breusch-Pagan:  LM=%.3f p=%.4f  %s\n", bp$statistic, bp$p.value,
      ifelse(bp$p.value<0.05,"HETEROSKED","OK")))
  cat(sprintf("  Durbin-Watson:  D=%.4f p=%.4f  %s\n", dw$statistic, dw$p.value,
      ifelse(dw$p.value<0.05,"AUTOCORR","OK")))
  cat(sprintf("  Ljung-Box(4):   Q=%.3f p=%.4f  %s\n", lb4$statistic, lb4$p.value,
      ifelse(lb4$p.value<0.05,"SERIAL CORR","OK")))
  if (p > 2) {
    vf <- tryCatch(vif(mod), error=function(e) NULL)
    if (!is.null(vf)) cat(sprintf("  VIF max: %.2f (%s)\n", max(vf), names(which.max(vf))))
  }
  invisible(list(sw=sw, ad=ad, bp=bp, dw=dw, lb=lb4))
}

run_diag(m1, "Model 1 (CFTC only)")
if (!is.null(m3)) run_diag(m3, "Model 3 (CFTC+Macro)")

diag_plots <- function(mod, label, fname) {
  res  <- residuals(mod); fit <- fitted(mod); sres <- rstudent(mod)
  png(fname, width=1400, height=1100, res=130)
  par(mfrow=c(2,2), mar=c(4,4,3,2), oma=c(0,0,3,0))
  plot(fit, res, pch=16, cex=0.7, col=rgb(0.2,0.4,0.8,0.5),
       xlab="Fitted", ylab="Residuals", main="Residuals vs Fitted")
  abline(h=0, col="red", lty=2, lwd=1.5)
  lines(lowess(fit, res), col="darkred", lwd=2)
  big <- which(abs(res) > 2.5*sd(res))
  if (length(big)) text(fit[big], res[big], labels=big, cex=0.5, col="red", pos=3)

  qqnorm(sres, pch=16, cex=0.7, col=rgb(0.2,0.4,0.8,0.5), main="Normal Q-Q (Studentised)")
  qqline(sres, col="red", lwd=2)

  plot(fit, sqrt(abs(sres)), pch=16, cex=0.7, col=rgb(0.2,0.6,0.3,0.5),
       xlab="Fitted", ylab=expression(sqrt("|Stud. Res|")), main="Scale-Location")
  lines(lowess(fit, sqrt(abs(sres))), col="darkgreen", lwd=2)
  abline(h=1, col="red", lty=2)

  acf(res, lag.max=20, main="ACF of Residuals", col=rgb(0.2,0.4,0.8,0.7), lwd=2)
  mtext(paste("OLS Diagnostics:", label), outer=TRUE, cex=1.1, font=2)
  dev.off()
  cat(sprintf("  Saved: %s\n", fname))
}

diag_plots(m1, "Model 1 (CL TWAP, CFTC only)",  "output/cftc/cl_diag_m1_before.png")
if (!is.null(m3)) diag_plots(m3, "Model 3 (CL TWAP, CFTC+Macro)", "output/cftc/cl_diag_m3_before.png")

# Outlier detection
detect_outliers <- function(mod, data, label) {
  n <- nobs(mod); p <- length(coef(mod))
  cook <- cooks.distance(mod)
  hat  <- hatvalues(mod)
  sres <- rstudent(mod)
  nf   <- (cook > 4/n) + (hat > 2*(p+1)/n) + (abs(sres) > 2.5)
  extreme <- which(nf >= 2)
  cat(sprintf("\n[Outliers: %s]  thresholds: Cook>%.4f | Lev>%.4f | |Stud|>2.5\n",
              label, 4/n, 2*(p+1)/n))
  cat(sprintf("  Flagged (>=2 criteria): %d\n", length(extreme)))
  if (length(extreme)) {
    od <- data.table(
      idx=extreme, release_date=data$release_date[extreme],
      net_pos=data$net_pos[extreme], pos_z=round(data$pos_z[extreme],3),
      ret_4w=round(data$ret_4w[extreme]*100,2),
      cook_d=round(cook[extreme],5), stud_r=round(sres[extreme],3)
    )
    print(as.data.frame(od))
  }
  extreme
}

out1 <- detect_outliers(m1, sub1, "Model 1")
out3 <- if (!is.null(m3)) detect_outliers(m3, sub3, "Model 3") else integer(0)

# Outlier Cook's D plot
png("output/cftc/cl_diag_outliers.png", width=1400, height=550, res=120)
par(mfrow=c(1,3), mar=c(5,5,3,2))
n1 <- nobs(m1); cook1 <- cooks.distance(m1)
top1 <- sort(cook1, decreasing=TRUE)[1:min(30,n1)]
barplot(top1, col=ifelse(top1>4/n1,"#D04040","#6090C0"),
        main="Cook's D — Model 1 (top 30)", ylab="Cook's D")
abline(h=4/n1, col="red", lty=2)
legend("topright", sprintf("4/n = %.4f",4/n1), lty=2, col="red", cex=0.8)

if (!is.null(m3)) {
  n3 <- nobs(m3); cook3 <- cooks.distance(m3)
  top3 <- sort(cook3, decreasing=TRUE)[1:min(30,n3)]
  barplot(top3, col=ifelse(top3>4/n3,"#D04040","#6090C0"),
          main="Cook's D — Model 3 (top 30)", ylab="Cook's D")
  abline(h=4/n3, col="red", lty=2)
  hat3 <- hatvalues(m3); sres3 <- rstudent(m3)
  plot(hat3, sres3, pch=21, cex=pmax(0.5, sqrt(cook3)*4),
       bg=ifelse(abs(sres3)>2.5,"#D04040",rgb(0.2,0.4,0.8,0.5)),
       xlab="Leverage", ylab="Studentised Residual",
       main="Leverage vs Residual — Model 3\n(bubble = Cook's D)")
  abline(h=c(-2.5,2.5), lty=2, col="red")
  abline(v=2*(length(coef(m3))+1)/n3, lty=2, col="orange")
} else {
  plot.new(); plot.new()
}
dev.off()
cat("  Saved: output/cftc/cl_diag_outliers.png\n")

# Refit after outlier removal
sub1_c <- if (length(out1)) sub1[-out1] else sub1
sub3_c <- if (!is.null(m3) && length(out3)) sub3[-out3] else sub3

m1c <- lm(ret_4w ~ pos_z, data=sub1_c)
m3c <- if (!is.null(m3)) lm(m3_form, data=sub3_c) else NULL

cat(sprintf("\nAfter outlier removal:\n"))
cat(sprintf("  M1: n=%d (-%d)  R2=%.4f→%.4f  pos_z p: %.3f→%.3f\n",
    nobs(m1c), length(out1), summary(m1)$r.squared, summary(m1c)$r.squared,
    coef(summary(m1))["pos_z",4], coef(summary(m1c))["pos_z",4]))
if (!is.null(m3c))
  cat(sprintf("  M3: n=%d (-%d)  R2=%.4f→%.4f  pos_z p: %.3f→%.3f\n",
      nobs(m3c), length(out3), summary(m3)$r.squared, summary(m3c)$r.squared,
      coef(summary(m3))["pos_z",4], coef(summary(m3c))["pos_z",4]))

diag_plots(m1c, "Model 1 CLEANED", "output/cftc/cl_diag_m1_after.png")
if (!is.null(m3c)) diag_plots(m3c, "Model 3 CLEANED", "output/cftc/cl_diag_m3_after.png")

# Before/after comparison table
comp <- data.table(
  model      = c("M1 (CFTC only)", "M3 (CFTC+Macro)"),
  n_before   = c(nobs(m1), if (!is.null(m3)) nobs(m3) else NA),
  n_after    = c(nobs(m1c), if (!is.null(m3c)) nobs(m3c) else NA),
  r2_before  = round(c(summary(m1)$r.squared, if (!is.null(m3)) summary(m3)$r.squared else NA),4),
  r2_after   = round(c(summary(m1c)$r.squared, if (!is.null(m3c)) summary(m3c)$r.squared else NA),4),
  pz_before  = round(c(coef(summary(m1))["pos_z",4], if (!is.null(m3)) coef(summary(m3))["pos_z",4] else NA),4),
  pz_after   = round(c(coef(summary(m1c))["pos_z",4], if (!is.null(m3c)) coef(summary(m3c))["pos_z",4] else NA),4)
)
cat("\n=== BEFORE/AFTER OUTLIER REMOVAL ===\n"); print(as.data.frame(comp))
fwrite(comp, "output/cftc/cl_model_comparison.csv")

# ═══════════════════════════════════════════════════════════════════════════════
# PART E: SUB-PERIOD STABILITY
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("PART E: SUB-PERIOD STABILITY (2021-2024 data range)\n")
cat("══════════════════════════════════════════════════\n")

periods <- list(
  "2021-2022 (COVID recovery + energy spike)" = c(as.Date("2021-01-01"), as.Date("2022-12-31")),
  "2023-2024 (post-energy-spike normalisation)"= c(as.Date("2023-01-01"), as.Date("2024-12-31")),
  "Full overlap (2021-2024)"                   = c(as.Date("2021-01-01"), as.Date("2024-12-31"))
)

period_res <- rbindlist(lapply(names(periods), function(pnm) {
  d1 <- periods[[pnm]][1]; d2 <- periods[[pnm]][2]
  s  <- sub1_c[release_date >= d1 & release_date <= d2]
  if (nrow(s) < 15) {
    cat(sprintf("  [%-44s] n=%d — too few, skipped\n", pnm, nrow(s))); return(NULL)
  }
  m  <- lm(ret_4w ~ pos_z, data=s); sm <- summary(m)
  lag_nw <- max(1, floor(nrow(s)^(1/3)))   # Andrews optimal lag
  nw <- coeftest(m, vcov=NeweyWest(m, lag=lag_nw, prewhite=FALSE))
  cat(sprintf("  [%-44s] n=%3d  coef=%+.4f  pOLS=%.3f  pNW=%.3f  R2=%.4f\n",
              pnm, nrow(s), coef(sm)["pos_z",1], coef(sm)["pos_z",4], nw["pos_z",4], sm$r.squared))
  data.table(period=pnm, n=nrow(s), nw_lag=lag_nw,
             coef=round(coef(sm)["pos_z",1],5),
             pOLS=round(coef(sm)["pos_z",4],4), pNW=round(nw["pos_z",4],4),
             r2=round(sm$r.squared,4), avg_4w=round(mean(s$ret_4w,na.rm=TRUE)*100,2))
}), fill=TRUE)
if (nrow(period_res)) fwrite(period_res, "output/cftc/cl_subperiod.csv")

# Sub-period scatter plots
avail_periods <- names(periods)[sapply(names(periods), function(pnm) {
  d1 <- periods[[pnm]][1]; d2 <- periods[[pnm]][2]
  nrow(sub1_c[release_date >= d1 & release_date <= d2]) >= 10
})]
ncols <- length(avail_periods)
if (ncols > 0) {
  png("output/cftc/cl_subperiod_scatter.png", width=500*ncols, height=500, res=110)
  par(mfrow=c(1,ncols), mar=c(4,4,3,1))
  cols <- c("#3060A0","#C03030","#30A060","#A06020")
  for (i in seq_along(avail_periods)) {
    pnm <- avail_periods[i]
    d1 <- periods[[pnm]][1]; d2 <- periods[[pnm]][2]
    s  <- sub1_c[release_date >= d1 & release_date <= d2]
    m_p <- lm(ret_4w ~ pos_z, data=s)
    plot(s$pos_z, s$ret_4w*100, pch=16, cex=0.7, col=adjustcolor(cols[i],0.5),
         xlab="pos_z", ylab="4W return (%)", main=pnm)
    abline(m_p, col=cols[i], lwd=2); abline(h=0, v=0, lty=3, col="grey60")
    legend("topleft", sprintf("coef=%+.3f\np=%.3f  R2=%.3f",
           coef(m_p)[2], summary(m_p)$coef[2,4], summary(m_p)$r.squared),
           bty="n", cex=0.8)
  }
  dev.off(); cat("  Saved: output/cftc/cl_subperiod_scatter.png\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART F: NEWEY-WEST HAC STANDARD ERRORS
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("PART F: NEWEY-WEST HAC STANDARD ERRORS\n")
cat("══════════════════════════════════════════════════\n")

models_for_hac <- list(list(m1c,"M1 CFTC only"))
if (!is.null(m3c)) {
  m4c <- lm(as.formula(paste("ret_4w ~", macro_rhs)), data=sub3_c)
  models_for_hac <- c(models_for_hac, list(list(m3c,"M3 CFTC+Macro"), list(m4c,"M4 Macro only")))
}

hac_table <- rbindlist(lapply(models_for_hac, function(x) {
  mod <- x[[1]]; lbl <- x[[2]]
  ols <- coef(summary(mod))
  nw  <- coeftest(mod, vcov=NeweyWest(mod, lag=3, prewhite=FALSE))
  cat(sprintf("\n--- %s (n=%d  R2=%.4f) ---\n", lbl, nobs(mod), summary(mod)$r.squared))
  for (v in rownames(ols)) {
    ols_s <- ifelse(ols[v,4]<0.01,"***",ifelse(ols[v,4]<0.05,"**",ifelse(ols[v,4]<0.10,"*","")))
    nw_s  <- ifelse(nw[v,4] <0.01,"***",ifelse(nw[v,4] <0.05,"**",ifelse(nw[v,4] <0.10,"*","")))
    cat(sprintf("  %-20s OLS: %+.4f (p=%.3f%s) | NW: %+.4f (p=%.3f%s)\n",
                v, ols[v,1], ols[v,4], ols_s, nw[v,1], nw[v,4], nw_s))
  }
  data.table(model=lbl, n=nobs(mod), r2=round(summary(mod)$r.squared,4),
             variable=rownames(ols),
             coef_ols=round(ols[,1],6), pval_ols=round(ols[,4],4),
             coef_nw=round(nw[,1],6),   pval_nw=round(nw[,4],4))
}), fill=TRUE)
fwrite(hac_table, "output/cftc/cl_hac_results.csv")

if (length(models_for_hac) >= 3) {
  cat(sprintf("\nMarginal R2 from CFTC (M3-M4): +%.4f\n",
              summary(m3c)$r.squared - summary(m4c)$r.squared))
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART G: THRESHOLD REGRESSION
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("PART G: THRESHOLD REGRESSION\n")
cat("══════════════════════════════════════════════════\n")

chow <- function(data, thresh_col, thresh_val, label) {
  tryCatch({
    mf_i <- lm(as.formula(paste0("ret_4w ~ pos_z * I(", thresh_col, ">", thresh_val, ")")), data=data)
    mf_r <- lm(ret_4w ~ pos_z, data=data)
    ft   <- anova(mf_r, mf_i)
    cat(sprintf("  Chow @ %s > %.2f  (%s): F=%.3f  p=%.4f  %s\n",
                thresh_col, thresh_val, label, ft$F[2], ft$`Pr(>F)`[2],
                ifelse(ft$`Pr(>F)`[2]<0.05,"*** BREAK","no break")))
  }, error=function(e) cat(sprintf("  Chow @ %s: %s\n", label, e$message)))
}
chow(sub1_c, "pos_pct", 0.90, "extreme long  (>90th)")
chow(sub1_c, "pos_pct", 0.10, "extreme short (<10th)")
chow(sub1_c, "pos_pct", 0.50, "median")

cat("\n  Segmented regression:\n")
tryCatch({
  seg <- segmented(lm(ret_4w ~ pos_z, data=sub1_c),
                   seg.Z=~pos_z, psi=list(pos_z=c(-1,1)))
  sl  <- slope(seg)$pos_z
  cat(sprintf("  Breakpoints: %s\n", paste(round(seg$psi[,2],3), collapse=", ")))
  for (i in seq_len(nrow(sl)))
    cat(sprintf("  Segment %d slope: %+.4f  CI: [%.4f, %.4f]\n",
                i, sl[i,1], sl[i,3], sl[i,4]))
  cat(sprintf("  R2: %.4f → %.4f\n",
              summary(lm(ret_4w~pos_z,data=sub1_c))$r.squared,
              summary(seg)$r.squared))
}, error=function(e) cat(sprintf("  Segmented: %s\n", e$message)))

cat("\n  Regressions by positioning regime:\n")
regime_list <- list(
  list(sub1_c,                                   "Full cleaned"),
  list(sub1_c[pos_pct>=0.90|pos_pct<=0.10],      "Extremes (<10 or >90)"),
  list(sub1_c[pos_pct>0.10&pos_pct<0.90],         "Non-extreme (10-90)"),
  list(sub1_c[pos_pct>=0.90],                     "Extreme Long (>90)"),
  list(sub1_c[pos_pct<=0.10],                     "Extreme Short (<10)")
)
thresh_rows <- rbindlist(lapply(regime_list, function(x) {
  s <- x[[1]]; lbl <- x[[2]]
  if (nrow(s) < 10) { cat(sprintf("  [%-25s] n=%d — skipped\n", lbl, nrow(s))); return(NULL) }
  m  <- lm(ret_4w ~ pos_z, data=s)
  nw <- tryCatch(coeftest(m, vcov=NeweyWest(m, lag=min(3,floor(nrow(s)/4)), prewhite=FALSE)),
                 error=function(e) coef(summary(m)))
  cat(sprintf("  [%-25s] n=%3d  coef=%+.4f  pOLS=%.3f  pNW=%.3f  R2=%.4f  avg4w=%+.2f%%\n",
              lbl, nrow(s), coef(m)[2], coef(summary(m))[2,4], nw[2,4],
              summary(m)$r.squared, mean(s$ret_4w,na.rm=TRUE)*100))
  data.table(subset=lbl, n=nrow(s),
             coef_ols=round(coef(m)[2],5), pOLS=round(coef(summary(m))[2,4],4),
             pNW=round(nw[2,4],4), r2=round(summary(m)$r.squared,4),
             avg4w=round(mean(s$ret_4w,na.rm=TRUE)*100,2))
}), fill=TRUE)
if (nrow(thresh_rows)) fwrite(thresh_rows, "output/cftc/cl_threshold_results.csv")

# Threshold plots
png("output/cftc/cl_threshold.png", width=1400, height=550, res=120)
par(mfrow=c(1,3), mar=c(4,4,3,1))
plot(sub1_c$pos_z, sub1_c$ret_4w*100, pch=16, cex=0.6,
     col=ifelse(sub1_c$pos_pct>=0.90,"#D04040",
         ifelse(sub1_c$pos_pct<=0.10,"#3060A0","grey60")),
     xlab="pos_z", ylab="4W return (%)",
     main="Return vs pos_z (red=ext.long, blue=ext.short)")
abline(lm(ret_4w*100~pos_z,data=sub1_c), col="black", lwd=2)
abline(v=quantile(sub1_c$pos_z,c(0.10,0.90)), lty=2, col=c("#3060A0","#D04040"))
abline(h=0, lty=3, col="grey50")

bdat <- list(
  "Ext Short\n<10"= sub1_c[pos_pct<=0.10, ret_4w*100],
  "Normal\n10-90" = sub1_c[pos_pct>0.10&pos_pct<0.90, ret_4w*100],
  "Ext Long\n>90" = sub1_c[pos_pct>=0.90, ret_4w*100]
)
boxplot(bdat, col=c("#3060A0","grey80","#D04040"),
        main="4W Return by Regime", ylab="4W return (%)")
abline(h=0, lty=2, col="red")

sub1_ord <- sub1_c[order(release_date)]
w_size <- min(26, floor(nrow(sub1_ord)/3))
rcor <- rep(NA_real_, nrow(sub1_ord))
for (i in seq(w_size, nrow(sub1_ord))) {
  win <- sub1_ord[(i-w_size+1):i]
  ok  <- !is.na(win$pos_z) & !is.na(win$ret_4w)
  if (sum(ok) > 6) rcor[i] <- cor(win$pos_z[ok], win$ret_4w[ok])
}
plot(sub1_ord$release_date, rcor, type="l", col="#3060A0", lwd=1.5,
     xlab="", ylab=sprintf("Rolling %d-week correlation", w_size),
     main=sprintf("Rolling %d-week Corr: pos_z vs ret_4w", w_size))
abline(h=0, lty=2, col="red"); abline(h=c(-0.2,0.2), lty=3, col="grey60")
dev.off(); cat("  Saved: output/cftc/cl_threshold.png\n")

# ═══════════════════════════════════════════════════════════════════════════════
# PART H: MARKOV-SWITCHING REGRESSION (2-state)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("PART H: MARKOV-SWITCHING REGRESSION (2-state)\n")
cat("══════════════════════════════════════════════════\n")

tryCatch({
  ms_data  <- sub1_c[order(release_date)]
  base_lm  <- lm(ret_4w ~ pos_z, data=ms_data)
  ms_mod   <- msmFit(base_lm, k=2, sw=c(TRUE,TRUE,TRUE),
                     control=list(maxiter=500, tol=1e-6))
  cat("\nMarkov-Switching: 2-state model fitted\n")
  print(summary(ms_mod))

  tp <- ms_mod@transMat
  cat("\nTransition matrix:\n")
  cat(sprintf("  P(S1->S1)=%.3f  P(S1->S2)=%.3f  (expected dur S1: %.1f wks)\n",
              tp[1,1], tp[1,2], 1/(1-tp[1,1])))
  cat(sprintf("  P(S2->S1)=%.3f  P(S2->S2)=%.3f  (expected dur S2: %.1f wks)\n",
              tp[2,1], tp[2,2], 1/(1-tp[2,2])))

  cf <- ms_mod@Coef
  cat("\nState coefficients:\n")
  for (st in 1:2)
    cat(sprintf("  State %d: intercept=%+.4f  pos_z=%+.4f\n", st, cf[st,1], cf[st,2]))

  spr    <- ms_mod@Fit@smoProb[,1]
  ms_out <- data.table(
    release_date=ms_data$release_date, twap=ms_data$twap_weekly,
    pos_z=ms_data$pos_z, ret_4w=ms_data$ret_4w,
    prob_state1=round(spr,4), state=as.integer(ifelse(spr>0.5,1L,2L))
  )
  fwrite(ms_out, "output/cftc/cl_ms_states.csv")
  cat("\nState statistics:\n")
  for (st in 1:2) {
    s <- ms_out[state==st]
    cat(sprintf("  State %d (n=%d): avg_ret4w=%+.2f%%  sd=%.2f%%  hit_up=%.0f%%\n",
                st, nrow(s), mean(s$ret_4w,na.rm=TRUE)*100,
                sd(s$ret_4w,na.rm=TRUE)*100, mean(s$ret_4w>0,na.rm=TRUE)*100))
  }

  png("output/cftc/cl_markov.png", width=1400, height=700, res=120)
  par(mfrow=c(2,1), mar=c(2,4,2,2), oma=c(2,0,3,0))
  plot(ms_out$release_date, spr, type="l", col="#3060A0", lwd=1.5,
       ylim=c(0,1), xlab="", ylab="P(State 1)", main="Smoothed State 1 Probability")
  abline(h=0.5, lty=2, col="red")
  polygon(c(ms_out$release_date, rev(ms_out$release_date)),
          c(spr, rep(0, length(spr))), col=rgb(0.2,0.4,0.8,0.2), border=NA)
  plot(ms_out$release_date, ms_out$twap, type="l", col="grey40",
       xlab="", ylab="WTI TWAP ($/bbl)", main="WTI Price with Regime Overlay")
  points(ms_out$release_date[ms_out$state==1], ms_out$twap[ms_out$state==1],
         pch=16, cex=0.5, col="#3060A0")
  points(ms_out$release_date[ms_out$state==2], ms_out$twap[ms_out$state==2],
         pch=16, cex=0.5, col="#D04040")
  legend("topright", c("State 1","State 2"), col=c("#3060A0","#D04040"), pch=16, cex=0.8)
  mtext("Markov-Switching: CFTC pos_z vs WTI 4W return (CL TWAP data)",
        outer=TRUE, font=2, cex=1.0)
  dev.off(); cat("  Saved: output/cftc/cl_markov.png\n")

}, error=function(e) cat(sprintf("  Markov-Switching error: %s\n", e$message)))

# ═══════════════════════════════════════════════════════════════════════════════
# FINAL SUMMARY
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("FINAL SUMMARY\n")
cat("══════════════════════════════════════════════════\n")
cat(sprintf("Price source  : cl_data.csv  (c1||weighted_mid, 1-min bars, UTC)\n"))
cat(sprintf("Aggregation   : Daily TWAP (mean of 1-min bars) -> ISO-week close\n"))
cat(sprintf("Alignment     : ISO-week join (robust to holiday Fridays)\n"))
cat(sprintf("Date range    : %s to %s  (file truncated at Excel 1M row cap)\n",
            min(mf$release_date), max(mf$release_date)))
cat(sprintf("Weekly obs    : %d total  |  %d with ret_4w  |  %d after outlier removal\n",
            nrow(mf), sum(!is.na(mf$ret_4w)), nrow(sub1_c)))
cat(sprintf("Extreme Long  : %d obs (>90th pct)\n", sum(sub1_c$pos_pct>=0.90)))
cat(sprintf("Extreme Short : %d obs (<10th pct)\n", sum(sub1_c$pos_pct<=0.10)))
cat("\nOutputs in output/cftc/:\n")
cat("  cl_weekly_twap.csv, cftc_cl_merged.csv, cftc_cl_regime_returns.csv\n")
cat("  cl_model_comparison.csv, cl_subperiod.csv, cl_hac_results.csv\n")
cat("  cl_threshold_results.csv, cl_ms_states.csv\n")
cat("  cl_diag_m1_before/after.png, cl_diag_m3_before/after.png\n")
cat("  cl_diag_outliers.png, cl_subperiod_scatter.png\n")
cat("  cl_threshold.png, cl_markov.png\n")
