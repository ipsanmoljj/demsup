# R/cftc_diagnostics.R
# ─────────────────────────────────────────────────────────────────────────────
# OLS Diagnostic Tests + Outlier Removal + Model Refit
#
# Tests applied to each model:
#   1. Normality of residuals  — Shapiro-Wilk + Anderson-Darling + QQ plot
#   2. Homoskedasticity        — Breusch-Pagan test + Residuals vs Fitted
#   3. Autocorrelation         — Durbin-Watson test + ACF plot of residuals
#   4. Multicollinearity       — VIF (Model 3 only; skipped for univariate)
#
# Outlier detection:
#   - Cook's Distance > 4/n  (influential observations)
#   - Studentised residuals  |r| > 2.5  (response outliers)
#   - Leverage (hat values)  > 2*(p+1)/n  (predictor space outliers)
#   Flag if 2+ criteria triggered; remove and refit.
#
# OUTPUTS  (output/cftc/)
#   diag_model1_before.png  — 4-panel diagnostic for Model 1 (pre-clean)
#   diag_model3_before.png  — 4-panel diagnostic for Model 3 (pre-clean)
#   diag_outliers.png       — Cook's distance + leverage chart
#   diag_model1_after.png   — diagnostics after outlier removal
#   diag_model3_after.png
#   cftc_outliers.csv       — flagged observations with reason
#   cftc_model_comparison.csv — coefficient table before vs after
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table); library(lmtest); library(car); library(nortest)
})

setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
dir.create("output/cftc", showWarnings=FALSE)

# ── 1. Reload merged data and refit models ────────────────────────────────────
mf <- fread("output/cftc/cftc_wti_merged.csv")
mf[, release_date := as.Date(release_date)]

zsc <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)

# Re-attach macro factors (not saved in the merged CSV)
fac <- fread("output/factors_extended.csv")
fac[, date := as.Date(date, origin="1970-01-01")]
fac[, week_fri := as.Date(cut(date, "week")) + 4L]
want <- c("dxy_4wk_chg","sofr","td3c_z52","bdi_z52","gasoil_crack_dev","sin_ann","cos_ann")
use  <- want[want %in% names(fac)]
fac_wk <- fac[, lapply(.SD, function(x) last(x[!is.na(x)])), by=week_fri, .SDcols=use]
setnames(fac_wk, "week_fri", "release_date")
mf <- merge(mf, fac_wk, by="release_date", all.x=TRUE)

mf[, mm_chg_z := zsc(net_pos_chg)]
mf[, dxy_z    := zsc(dxy_4wk_chg)]
mf[, sofr_z   := zsc(sofr)]
mf[, td3c_z   := td3c_z52]

# Model 1: CFTC level only, full history
sub1 <- mf[!is.na(ret_4w) & !is.na(pos_z)]
m1   <- lm(ret_4w ~ pos_z, data=sub1)

# Model 3: CFTC + macro, 2021+ only
sub3 <- mf[!is.na(ret_4w) & !is.na(pos_z) & !is.na(td3c_z) & !is.na(dxy_z) & !is.na(sofr_z)]
m3   <- lm(ret_4w ~ pos_z + dxy_z + sofr_z + td3c_z + sin_ann + cos_ann, data=sub3)

cat(sprintf("Model 1: n=%d  R2=%.4f\n", nobs(m1), summary(m1)$r.squared))
cat(sprintf("Model 3: n=%d  R2=%.4f\n", nobs(m3), summary(m3)$r.squared))

# ── 2. Diagnostic test runner ────────────────────────────────────────────────
run_diagnostics <- function(mod, label) {
  cat(sprintf("\n╔══════════════════════════════════════╗\n"))
  cat(sprintf("║  DIAGNOSTICS: %-22s║\n", label))
  cat(sprintf("╚══════════════════════════════════════╝\n"))
  res <- residuals(mod)
  fit <- fitted(mod)
  n   <- length(res)
  p   <- length(coef(mod))

  # ── Test 1: Normality ──────────────────────────────────────────────────────
  cat("\n[1] NORMALITY OF RESIDUALS\n")
  sw  <- shapiro.test(res)
  ad  <- ad.test(res)    # Anderson-Darling (more powerful for fat tails)
  cat(sprintf("    Shapiro-Wilk:      W=%.4f   p=%.4f  %s\n",
              sw$statistic, sw$p.value, ifelse(sw$p.value<0.05,"*** NON-NORMAL","  pass")))
  cat(sprintf("    Anderson-Darling:  A=%.4f   p=%.4f  %s\n",
              ad$statistic, ad$p.value, ifelse(ad$p.value<0.05,"*** NON-NORMAL","  pass")))
  cat(sprintf("    Skewness:  %.3f   Kurtosis: %.3f  (normal = 0, 3)\n",
              mean((res-mean(res))^3)/sd(res)^3,
              mean((res-mean(res))^4)/sd(res)^4))

  # ── Test 2: Homoskedasticity ───────────────────────────────────────────────
  cat("\n[2] HOMOSKEDASTICITY (constant residual variance)\n")
  bp <- bptest(mod)   # Breusch-Pagan
  cat(sprintf("    Breusch-Pagan:  LM=%.4f  df=%d  p=%.4f  %s\n",
              bp$statistic, bp$parameter, bp$p.value,
              ifelse(bp$p.value<0.05,"*** HETEROSKEDASTIC","  pass")))

  # ── Test 3: Autocorrelation ────────────────────────────────────────────────
  cat("\n[3] AUTOCORRELATION OF RESIDUALS\n")
  dw <- dwtest(mod, alternative="two.sided")
  cat(sprintf("    Durbin-Watson:  DW=%.4f  p=%.4f  %s\n",
              dw$statistic, dw$p.value,
              ifelse(dw$p.value<0.05,"*** AUTOCORRELATED","  pass")))
  # Ljung-Box at lag 4 (one month) and lag 12 (one quarter)
  lb4  <- Box.test(res, lag=4,  type="Ljung-Box")
  lb12 <- Box.test(res, lag=12, type="Ljung-Box")
  cat(sprintf("    Ljung-Box lag-4:  Q=%.3f  p=%.4f  %s\n",
              lb4$statistic, lb4$p.value, ifelse(lb4$p.value<0.05,"*** serial corr","  pass")))
  cat(sprintf("    Ljung-Box lag-12: Q=%.3f  p=%.4f  %s\n",
              lb12$statistic, lb12$p.value, ifelse(lb12$p.value<0.05,"*** serial corr","  pass")))

  # ── Test 4: Multicollinearity (only for multi-predictor models) ────────────
  cat("\n[4] MULTICOLLINEARITY (VIF)\n")
  if (p > 2) {
    vf <- vif(mod)
    for (nm in names(vf))
      cat(sprintf("    %-20s  VIF=%.2f  %s\n", nm, vf[nm],
                  ifelse(vf[nm]>5,"*** HIGH", ifelse(vf[nm]>2.5,"  moderate","  OK"))))
  } else {
    cat("    Skipped (univariate model — VIF not applicable)\n")
  }

  invisible(list(sw=sw, ad=ad, bp=bp, dw=dw, lb4=lb4, lb12=lb12))
}

# ── 3. Diagnostic PLOTS (4-panel) ────────────────────────────────────────────
diag_plots <- function(mod, label, fname) {
  res  <- residuals(mod)
  fit  <- fitted(mod)
  sres <- rstudent(mod)
  n    <- length(res)

  png(fname, width=1400, height=1100, res=130)
  par(mfrow=c(2,2), mar=c(4,4,3,2), oma=c(0,0,3,0))

  # Panel 1: Residuals vs Fitted
  plot(fit, res, pch=16, cex=0.6, col=rgb(0.2,0.4,0.8,0.5),
       xlab="Fitted values", ylab="Residuals",
       main="Residuals vs Fitted")
  abline(h=0, col="red", lwd=1.5, lty=2)
  lines(lowess(fit, res), col="darkred", lwd=2)
  # flag large residuals
  big <- which(abs(res) > 2*sd(res))
  if (length(big)) text(fit[big], res[big], labels=big, cex=0.5, col="red", pos=3)

  # Panel 2: Normal QQ plot
  qqnorm(sres, pch=16, cex=0.6, col=rgb(0.2,0.4,0.8,0.5),
         main="Normal Q-Q (Studentised Residuals)")
  qqline(sres, col="red", lwd=2)
  # annotate outliers
  qq <- qqnorm(sres, plot.it=FALSE)
  far <- which(abs(qq$y - qq$x) > 0.5)
  if (length(far)) text(qq$x[far], qq$y[far], labels=far, cex=0.5, col="red", pos=4)

  # Panel 3: Scale-Location (sqrt(|res|) vs fitted) — tests homoskedasticity
  plot(fit, sqrt(abs(sres)), pch=16, cex=0.6, col=rgb(0.2,0.6,0.3,0.5),
       xlab="Fitted values", ylab=expression(sqrt("|Studentised residuals|")),
       main="Scale-Location (Homoskedasticity)")
  lines(lowess(fit, sqrt(abs(sres))), col="darkgreen", lwd=2)
  abline(h=1, col="red", lty=2)

  # Panel 4: ACF of residuals — tests autocorrelation
  acf(res, lag.max=20, main="ACF of Residuals (Autocorrelation)",
      col=rgb(0.2,0.4,0.8,0.7), lwd=2)

  mtext(paste("OLS Diagnostics:", label), outer=TRUE, cex=1.1, font=2)
  dev.off()
  cat(sprintf("  Saved: %s\n", fname))
}

# ── 4. Run diagnostics BEFORE cleaning ───────────────────────────────────────
cat("\n─── BEFORE OUTLIER REMOVAL ───\n")
run_diagnostics(m1, "Model 1 (CFTC only, n=527)")
run_diagnostics(m3, "Model 3 (CFTC+Macro, n=111)")

cat("\nGenerating diagnostic plots (before)...\n")
diag_plots(m1, "Model 1 — CFTC only (full history)", "output/cftc/diag_model1_before.png")
diag_plots(m3, "Model 3 — CFTC + Macro (2021+)",     "output/cftc/diag_model3_before.png")

# ── 5. Outlier detection ──────────────────────────────────────────────────────
detect_outliers <- function(mod, data, label) {
  cat(sprintf("\n[OUTLIERS: %s]\n", label))
  n     <- nobs(mod)
  p     <- length(coef(mod))
  cook  <- cooks.distance(mod)
  hat   <- hatvalues(mod)
  sres  <- rstudent(mod)

  # Thresholds
  cook_thr <- 4 / n                    # Cook's distance
  hat_thr  <- 2 * (p + 1) / n         # leverage
  sres_thr <- 2.5                      # studentised residuals

  flag_cook <- cook > cook_thr
  flag_hat  <- hat  > hat_thr
  flag_sres <- abs(sres) > sres_thr

  # Flag if 2+ criteria
  n_flags <- as.integer(flag_cook) + as.integer(flag_hat) + as.integer(flag_sres)
  extreme <- which(n_flags >= 2)

  cat(sprintf("  Thresholds:  Cook>%.4f  |  Leverage>%.4f  |  |Rstud|>%.1f\n",
              cook_thr, hat_thr, sres_thr))
  cat(sprintf("  Cook flagged: %d   Leverage: %d   |Rstud|: %d\n",
              sum(flag_cook), sum(flag_hat), sum(flag_sres)))
  cat(sprintf("  Multi-flagged (2+ criteria): %d observations\n", length(extreme)))

  if (length(extreme)) {
    out_dt <- data[extreme, .(release_date, net_pos, pos_z, ret_4w)]
    out_dt[, cook_d  := round(cook[extreme], 4)]
    out_dt[, leverage:= round(hat[extreme],  4)]
    out_dt[, stud_res:= round(sres[extreme], 3)]
    out_dt[, flags   := n_flags[extreme]]
    out_dt[, reason  := paste(
      ifelse(flag_cook[extreme], "Cook", ""),
      ifelse(flag_hat[extreme],  "Lev",  ""),
      ifelse(flag_sres[extreme], "Stud", "")
    )]
    print(as.data.frame(out_dt))
    return(extreme)
  }
  integer(0)
}

out1 <- detect_outliers(m1, sub1, "Model 1")
out3 <- detect_outliers(m3, sub3, "Model 3")

# ── 6. Outlier plot ───────────────────────────────────────────────────────────
png("output/cftc/diag_outliers.png", width=1400, height=600, res=130)
par(mfrow=c(1,3), mar=c(5,5,3,2))

# Cook's distance — Model 1
cook1 <- cooks.distance(m1)
n1 <- nobs(m1)
barplot(sort(cook1, decreasing=TRUE)[1:30], col=ifelse(sort(cook1,dec=TRUE)[1:30]>4/n1,"#D04040","#6090C0"),
        main="Cook's Distance — Model 1\n(top 30 observations)",
        ylab="Cook's D", xlab="Obs rank", cex.names=0.6)
abline(h=4/n1, col="red", lty=2, lwd=1.5)
legend("topright", c(sprintf("Threshold 4/n=%.4f",4/n1)), lty=2, col="red", cex=0.8)

# Cook's distance — Model 3
cook3 <- cooks.distance(m3)
n3 <- nobs(m3)
barplot(sort(cook3, decreasing=TRUE)[1:30], col=ifelse(sort(cook3,dec=TRUE)[1:30]>4/n3,"#D04040","#6090C0"),
        main="Cook's Distance — Model 3\n(top 30 observations)",
        ylab="Cook's D", xlab="Obs rank", cex.names=0.6)
abline(h=4/n3, col="red", lty=2, lwd=1.5)
legend("topright", c(sprintf("Threshold 4/n=%.4f",4/n3)), lty=2, col="red", cex=0.8)

# Leverage vs Studentised residuals bubble chart (Model 3)
hat3  <- hatvalues(m3)
sres3 <- rstudent(m3)
cook3_sz <- sqrt(cook3) * 4
col3 <- ifelse(abs(sres3) > 2.5 | hat3 > 2*(6+1)/n3, "#D04040", rgb(0.2,0.4,0.8,0.5))
plot(hat3, sres3, pch=21, bg=col3, cex=pmax(0.5, cook3_sz),
     xlab="Leverage (hat value)", ylab="Studentised Residual",
     main="Leverage vs Residual — Model 3\n(bubble = Cook's D)")
abline(h=c(-2.5,2.5), lty=2, col="red")
abline(v=2*(length(coef(m3))+1)/n3, lty=2, col="orange")
abline(h=0, col="grey50")
dev.off()
cat("  Saved: output/cftc/diag_outliers.png\n")

# ── 7. Save outlier records ───────────────────────────────────────────────────
cook1_v  <- cooks.distance(m1); hat1_v <- hatvalues(m1); sres1_v <- rstudent(m1)
cook3_v  <- cooks.distance(m3); hat3_v <- hatvalues(m3); sres3_v <- rstudent(m3)

mk_out_dt <- function(mod, data, cook_v, hat_v, sres_v, model_name) {
  n <- nobs(mod); p <- length(coef(mod))
  flag_c <- cook_v > 4/n
  flag_h <- hat_v  > 2*(p+1)/n
  flag_s <- abs(sres_v) > 2.5
  nf <- as.integer(flag_c) + as.integer(flag_h) + as.integer(flag_s)
  idx <- which(nf >= 1)
  if (!length(idx)) return(data.table())
  data[idx, .(model=model_name, release_date, net_pos=round(net_pos), pos_pct=round(pos_pct,3),
              ret_4w=round(ret_4w*100,2), cook_d=round(cook_v[idx],5),
              leverage=round(hat_v[idx],4), stud_res=round(sres_v[idx],3),
              n_flags=nf[idx],
              flags=mapply(function(c,h,s) paste(c(if(c)"Cook",if(h)"Lev",if(s)"Stud"),collapse="+"),
                           flag_c[idx], flag_h[idx], flag_s[idx]))]
}
out_all <- rbindlist(list(
  mk_out_dt(m1, sub1, cook1_v, hat1_v, sres1_v, "M1_CFTC_only"),
  mk_out_dt(m3, sub3, cook3_v, hat3_v, sres3_v, "M3_CFTC_Macro")
), fill=TRUE)
fwrite(out_all, "output/cftc/cftc_outliers.csv")
cat(sprintf("  Outlier records saved: %d rows\n", nrow(out_all)))

# ── 8. Remove outliers and refit ──────────────────────────────────────────────
cat("\n─── AFTER OUTLIER REMOVAL ───\n")

# Strict removal: 2+ flags
sub1_clean <- if (length(out1)) sub1[-out1] else sub1
sub3_clean <- if (length(out3)) sub3[-out3] else sub3

m1_c <- lm(ret_4w ~ pos_z, data=sub1_clean)
m3_c <- lm(ret_4w ~ pos_z + dxy_z + sofr_z + td3c_z + sin_ann + cos_ann, data=sub3_clean)

cat(sprintf("Model 1 clean: n=%d (removed %d)  R2=%.4f  (was %.4f)\n",
            nobs(m1_c), length(out1), summary(m1_c)$r.squared, summary(m1)$r.squared))
cat(sprintf("Model 3 clean: n=%d (removed %d)  R2=%.4f  (was %.4f)\n",
            nobs(m3_c), length(out3), summary(m3_c)$r.squared, summary(m3)$r.squared))

run_diagnostics(m1_c, "Model 1 CLEANED")
run_diagnostics(m3_c, "Model 3 CLEANED")

diag_plots(m1_c, "Model 1 CLEANED — CFTC only", "output/cftc/diag_model1_after.png")
diag_plots(m3_c, "Model 3 CLEANED — CFTC + Macro", "output/cftc/diag_model3_after.png")

# ── 9. Coefficient comparison table ──────────────────────────────────────────
cat("\n═══ COEFFICIENT COMPARISON: Before vs After ═══\n")
compare_coef <- function(m_before, m_after, model_name) {
  cfb <- coef(summary(m_before)); cfa <- coef(summary(m_after))
  all_rows <- union(rownames(cfb), rownames(cfa))
  dt <- data.table(model=model_name, variable=all_rows)
  dt[, coef_before := cfb[variable, 1]]
  dt[, pval_before := cfb[variable, 4]]
  dt[, coef_after  := cfa[variable, 1]]
  dt[, pval_after  := cfa[variable, 4]]
  dt[, coef_change := coef_after - coef_before]
  dt
}
comp <- rbindlist(list(
  compare_coef(m1, m1_c, "M1_CFTC_only"),
  compare_coef(m3, m3_c, "M3_CFTC_Macro")
))
# print to console
for (mod_nm in unique(comp$model)) {
  cat(sprintf("\n[%s]\n", mod_nm))
  cat(sprintf("  %-20s  %10s  %6s  %10s  %6s  %10s\n",
              "Variable","Coef(before)","p","Coef(after)","p","Change"))
  sub <- comp[model==mod_nm]
  for (i in seq_len(nrow(sub))) {
    r <- sub[i]
    cat(sprintf("  %-20s  %+10.5f  %.3f  %+10.5f  %.3f  %+10.5f\n",
                r$variable, r$coef_before, r$pval_before,
                r$coef_after, r$pval_after, r$coef_change))
  }
}
comp[, coef_before := round(coef_before,6)][, coef_after := round(coef_after,6)]
comp[, pval_before := round(pval_before,4)][, pval_after := round(pval_after,4)]
fwrite(comp, "output/cftc/cftc_model_comparison.csv")

cat("\n═══ SUMMARY ═══\n")
cat(sprintf("Model 1:  R2 %.4f -> %.4f  |  n %d -> %d  |  removed %d obs\n",
            summary(m1)$r.squared, summary(m1_c)$r.squared,
            nobs(m1), nobs(m1_c), length(out1)))
cat(sprintf("Model 3:  R2 %.4f -> %.4f  |  n %d -> %d  |  removed %d obs\n",
            summary(m3)$r.squared, summary(m3_c)$r.squared,
            nobs(m3), nobs(m3_c), length(out3)))
cat("\nAll outputs in output/cftc/\n")
