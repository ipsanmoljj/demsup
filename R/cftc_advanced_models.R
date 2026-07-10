# R/cftc_advanced_models.R
# ─────────────────────────────────────────────────────────────────────────────
# Step 1 : Sub-period stability  (2016-19 | 2020-22 | 2023-26)
# Step 2 : Newey-West HAC standard errors  (fix autocorrelation bias)
# Step 3 : Threshold regression  (Chow test + segmented / TAR)
# Step 4 : Markov-switching regression  (2-state: bull/bear regime)
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table); library(lmtest); library(sandwich)
  library(strucchange); library(segmented); library(MSwM)
})

setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
dir.create("output/cftc", showWarnings=FALSE)

zsc <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)

# ── Reload data ───────────────────────────────────────────────────────────────
mf <- fread("output/cftc/cftc_wti_merged.csv")
mf[, release_date := as.Date(release_date)]
mf[, year := year(release_date)]

fac <- fread("output/factors_extended.csv")
fac[, date := as.Date(date, origin="1970-01-01")]
fac[, week_fri := as.Date(cut(date, "week")) + 4L]
use_cols <- c("dxy_4wk_chg","sofr","td3c_z52","sin_ann","cos_ann")[
  c("dxy_4wk_chg","sofr","td3c_z52","sin_ann","cos_ann") %in% names(fac)]
fac_wk <- fac[, lapply(.SD, function(x) last(x[!is.na(x)])), by=week_fri, .SDcols=use_cols]
setnames(fac_wk, "week_fri", "release_date")
mf <- merge(mf, fac_wk, by="release_date", all.x=TRUE)

mf[, dxy_z   := zsc(dxy_4wk_chg)]
mf[, sofr_z  := zsc(sofr)]
mf[, td3c_z  := td3c_z52]
mf[, mm_chg_z:= zsc(net_pos_chg)]

# Clean datasets (outliers already identified)
sub1 <- mf[!is.na(ret_4w) & !is.na(pos_z)]
sub3 <- mf[!is.na(ret_4w) & !is.na(pos_z) & !is.na(td3c_z) & !is.na(dxy_z) & !is.na(sofr_z)]

# COVID outliers (10 obs, Mar-Jun 2020)
covid_dates <- as.Date(c("2020-03-13","2020-03-20","2020-03-27","2020-04-03",
                          "2020-04-17","2020-04-24","2020-05-15","2020-05-22",
                          "2020-05-29","2020-06-05"))
# Model 3 outliers (4 obs)
m3_out_dates <- as.Date(c("2026-03-13","2026-03-27","2026-06-12","2026-06-19"))

sub1_c <- sub1[!release_date %in% covid_dates]
sub3_c <- sub3[!release_date %in% m3_out_dates]

cat(sprintf("Base datasets: sub1 n=%d  sub1_clean n=%d  sub3 n=%d  sub3_clean n=%d\n",
            nrow(sub1), nrow(sub1_c), nrow(sub3), nrow(sub3_c)))

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 1: Sub-period stability
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("STEP 1: SUB-PERIOD STABILITY ANALYSIS\n")
cat("══════════════════════════════════════════════════\n")

periods <- list(
  "2016-2019 (Pre-COVID normal)"     = c(as.Date("2016-01-01"), as.Date("2019-12-31")),
  "2020-2022 (COVID + energy shock)" = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
  "2023-2026 (Post-shock)"           = c(as.Date("2023-01-01"), as.Date("2026-12-31"))
)

period_results <- list()

for (pnm in names(periods)) {
  d1 <- periods[[pnm]][1]; d2 <- periods[[pnm]][2]
  s  <- sub1[release_date >= d1 & release_date <= d2 & !release_date %in% covid_dates]
  if (nrow(s) < 20) { cat(sprintf("\n[%s]  n=%d — too few obs, skip\n", pnm, nrow(s))); next }

  m  <- lm(ret_4w ~ pos_z, data=s)
  sm <- summary(m)
  cf <- coef(sm)

  # Newey-West corrected for this sub-period
  nw <- coeftest(m, vcov=NeweyWest(m, lag=3, prewhite=FALSE))

  cat(sprintf("\n[%s]  n=%d\n", pnm, nrow(s)))
  cat(sprintf("  OLS:  pos_z coef=%+.4f  p=%.3f  R2=%.4f\n",
              cf["pos_z",1], cf["pos_z",4], sm$r.squared))
  cat(sprintf("  NW:   pos_z coef=%+.4f  p=%.3f  (HAC corrected)\n",
              nw["pos_z",1], nw["pos_z",4]))

  period_results[[pnm]] <- data.table(
    period=pnm, n=nrow(s),
    coef_ols=round(cf["pos_z",1],5), pval_ols=round(cf["pos_z",4],4),
    coef_nw =round(nw["pos_z",1],5), pval_nw =round(nw["pos_z",4],4),
    r2      =round(sm$r.squared,4),
    avg_ret_4w_pct = round(mean(s$ret_4w, na.rm=TRUE)*100, 2)
  )
}

# Full period row
m_full <- lm(ret_4w ~ pos_z, data=sub1_c)
sm_full <- summary(m_full)
nw_full <- coeftest(m_full, vcov=NeweyWest(m_full, lag=3, prewhite=FALSE))
period_results[["Full 2016-2026 (cleaned)"]] <- data.table(
  period="Full 2016-2026 (cleaned)", n=nrow(sub1_c),
  coef_ols=round(coef(sm_full)["pos_z",1],5), pval_ols=round(coef(sm_full)["pos_z",4],4),
  coef_nw =round(nw_full["pos_z",1],5),        pval_nw =round(nw_full["pos_z",4],4),
  r2      =round(sm_full$r.squared,4),
  avg_ret_4w_pct=round(mean(sub1_c$ret_4w, na.rm=TRUE)*100, 2)
)

period_dt <- rbindlist(period_results)
fwrite(period_dt, "output/cftc/cftc_subperiod_results.csv")
cat("\nSub-period summary:\n"); print(as.data.frame(period_dt))

# ── Sub-period plot ───────────────────────────────────────────────────────────
png("output/cftc/subperiod_scatter.png", width=1400, height=500, res=120)
par(mfrow=c(1,3), mar=c(4,4,3,1))
period_cols <- c("#3060A0","#C03030","#30A060")
for (i in seq_along(periods)) {
  pnm <- names(periods)[i]
  d1  <- periods[[pnm]][1]; d2 <- periods[[pnm]][2]
  s   <- sub1[release_date >= d1 & release_date <= d2 & !release_date %in% covid_dates]
  if (nrow(s) < 5) next
  m_p <- lm(ret_4w ~ pos_z, data=s)
  plot(s$pos_z, s$ret_4w*100, pch=16, cex=0.7,
       col=adjustcolor(period_cols[i], 0.5),
       xlab="pos_z (CFTC net position z-score)",
       ylab="4-week return (%)",
       main=pnm)
  abline(m_p, col=period_cols[i], lwd=2)
  abline(h=0, v=0, lty=3, col="grey60")
  legend("topleft", sprintf("coef=%+.3f\np=%.3f  R2=%.3f",
         coef(m_p)[2], summary(m_p)$coefficients[2,4], summary(m_p)$r.squared),
         bty="n", cex=0.8)
}
dev.off()
cat("  Saved: output/cftc/subperiod_scatter.png\n")

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 2: Newey-West HAC standard errors on all 4 models
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("STEP 2: NEWEY-WEST HAC CORRECTED STANDARD ERRORS\n")
cat("══════════════════════════════════════════════════\n")
cat("(Lag=3 chosen: 4-week overlapping returns on weekly data => 3 overlapping obs)\n\n")

m1c <- lm(ret_4w ~ pos_z, data=sub1_c)
m2c <- lm(ret_4w ~ pos_z + mm_chg_z, data=sub1_c[!is.na(mm_chg_z)])
m3c <- lm(ret_4w ~ pos_z + dxy_z + sofr_z + td3c_z + sin_ann + cos_ann, data=sub3_c)
m4c <- lm(ret_4w ~ dxy_z + sofr_z + td3c_z + sin_ann + cos_ann, data=sub3_c)

print_hac <- function(mod, label) {
  ols <- coef(summary(mod))
  nw  <- coeftest(mod, vcov=NeweyWest(mod, lag=3, prewhite=FALSE))
  cat(sprintf("\n--- %s (n=%d, R2=%.4f) ---\n", label, nobs(mod), summary(mod)$r.squared))
  cat(sprintf("  %-22s  %10s %7s  |  %10s %7s  | CHANGE\n",
              "Variable","OLS coef","OLS p","NW coef","NW p"))
  cat(sprintf("  %s\n", strrep("-",75)))
  for (v in rownames(ols)) {
    ols_sig <- ifelse(ols[v,4]<0.01,"***",ifelse(ols[v,4]<0.05,"** ",ifelse(ols[v,4]<0.10,"*  ","   ")))
    nw_sig  <- ifelse(nw[v,4] <0.01,"***",ifelse(nw[v,4] <0.05,"** ",ifelse(nw[v,4] <0.10,"*  ","   ")))
    cat(sprintf("  %-22s  %+10.5f %5.3f%s  |  %+10.5f %5.3f%s\n",
                v, ols[v,1], ols[v,4], ols_sig, nw[v,1], nw[v,4], nw_sig))
  }
}
print_hac(m1c, "Model 1: CFTC only (cleaned)")
print_hac(m2c, "Model 2: CFTC + weekly change")
print_hac(m3c, "Model 3: CFTC + Macro (cleaned)")
print_hac(m4c, "Model 4: Macro only")

# Save HAC results
hac_rows <- rbindlist(lapply(list(m1c,m2c,m3c,m4c), function(mod) {
  ols <- coef(summary(mod))
  nw  <- coeftest(mod, vcov=NeweyWest(mod, lag=3, prewhite=FALSE))
  data.table(n=nobs(mod), r2=round(summary(mod)$r.squared,4),
             variable=rownames(ols),
             coef_ols=round(ols[,1],6), pval_ols=round(ols[,4],4),
             coef_nw =round(nw[,1], 6), pval_nw =round(nw[,4], 4))
}), fill=TRUE)
fwrite(hac_rows, "output/cftc/cftc_hac_results.csv")

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 3: Threshold Regression
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("STEP 3: THRESHOLD REGRESSION\n")
cat("══════════════════════════════════════════════════\n")

# 3a. Chow test: does the relationship change at the 90th / 10th pct boundary?
cat("\n[3a] Chow Test at percentile thresholds\n")
chow_test <- function(data, threshold_col, threshold_val, y="ret_4w", x="pos_z", label="") {
  d1 <- data[get(threshold_col) <= threshold_val]
  d2 <- data[get(threshold_col) >  threshold_val]
  if (nrow(d1) < 15 || nrow(d2) < 15) return(NULL)
  m_full  <- lm(as.formula(paste(y, "~", x)), data=data)
  m_split <- lm(as.formula(paste(y, "~", x, "* I(", threshold_col, ">", threshold_val, ")")), data=data)
  ft <- anova(m_full, m_split)
  cat(sprintf("  Chow @ %s=%.3f (%s):  F=%.3f  p=%.4f  %s\n",
              threshold_col, threshold_val, label,
              ft$F[2], ft$`Pr(>F)`[2],
              ifelse(ft$`Pr(>F)`[2]<0.05,"*** BREAK DETECTED","  no break")))
  list(F=ft$F[2], p=ft$`Pr(>F)`[2])
}

chow_test(sub1_c, "pos_pct", 0.90, label="extreme long boundary")
chow_test(sub1_c, "pos_pct", 0.10, label="extreme short boundary")
chow_test(sub1_c, "pos_pct", 0.50, label="median")
# Also test at arbitrary z-score thresholds
sub1_c[, pos_z_abs := abs(pos_z)]
chow_test(sub1_c, "pos_z_abs", 1.5, label="|z|>1.5 extreme")

# 3b. Segmented regression (piecewise linear with estimated breakpoint)
cat("\n[3b] Segmented / Piecewise Linear Regression (estimated breakpoint in pos_z)\n")
base_m <- lm(ret_4w ~ pos_z, data=sub1_c)
tryCatch({
  seg <- segmented(base_m, seg.Z=~pos_z, psi=list(pos_z=c(-1, 1)))
  cat(sprintf("  Breakpoints estimated at pos_z = %s\n",
              paste(round(seg$psi[,2], 3), collapse=", ")))
  cat(sprintf("  Slopes:\n"))
  sl <- slope(seg)$pos_z
  for (i in seq_len(nrow(sl)))
    cat(sprintf("    Segment %d: slope=%+.4f (95%%CI: %+.4f, %+.4f)\n",
                i, sl[i,1], sl[i,3], sl[i,4]))
  cat(sprintf("  R2 improvement: %.4f -> %.4f\n",
              summary(base_m)$r.squared, summary(seg)$r.squared))
}, error=function(e) cat(sprintf("  Segmented failed: %s\n", e$message)))

# 3c. Manual threshold: separate regressions for extreme vs non-extreme
cat("\n[3c] Separate regressions: extreme vs non-extreme positioning\n")
sub_ext  <- sub1_c[pos_pct >= 0.90 | pos_pct <= 0.10]
sub_norm <- sub1_c[pos_pct > 0.10 & pos_pct < 0.90]
sub_xlong<- sub1_c[pos_pct >= 0.90]
sub_xshort<-sub1_c[pos_pct <= 0.10]

fit_report <- function(data, label) {
  if (nrow(data) < 10) { cat(sprintf("  [%s] n=%d -- too few\n", label, nrow(data))); return() }
  m  <- lm(ret_4w ~ pos_z, data=data)
  nw <- coeftest(m, vcov=NeweyWest(m, lag=3, prewhite=FALSE))
  cat(sprintf("  [%-30s] n=%3d  coef=%+.4f  pOLS=%.3f  pNW=%.3f  R2=%.4f  avg4w=%+.2f%%\n",
              label, nrow(data),
              coef(m)[2], coef(summary(m))[2,4], nw[2,4],
              summary(m)$r.squared,
              mean(data$ret_4w, na.rm=TRUE)*100))
}
fit_report(sub1_c,    "All (cleaned)")
fit_report(sub_ext,   "Extremes only (<10 or >90 pct)")
fit_report(sub_norm,  "Non-extreme (10-90 pct)")
fit_report(sub_xlong, "Extreme Long (>90 pct)")
fit_report(sub_xshort,"Extreme Short (<10 pct)")

# Save threshold results
thr_rows <- rbindlist(lapply(
  list(list(sub1_c,"All cleaned"), list(sub_ext,"Extremes"),
       list(sub_norm,"Non-extreme"), list(sub_xlong,"Extreme Long"),
       list(sub_xshort,"Extreme Short")),
  function(x) {
    if (nrow(x[[1]]) < 10) return(NULL)
    m  <- lm(ret_4w ~ pos_z, data=x[[1]])
    nw <- coeftest(m, vcov=NeweyWest(m, lag=3, prewhite=FALSE))
    data.table(subset=x[[2]], n=nrow(x[[1]]),
               coef_ols=round(coef(m)[2],5), pval_ols=round(coef(summary(m))[2,4],4),
               coef_nw=round(nw[2,1],5),     pval_nw=round(nw[2,4],4),
               r2=round(summary(m)$r.squared,4),
               avg_ret_4w=round(mean(x[[1]]$ret_4w,na.rm=TRUE)*100,2))
  }), fill=TRUE)
fwrite(thr_rows, "output/cftc/cftc_threshold_results.csv")

# 3d. Threshold plot
png("output/cftc/threshold_regression.png", width=1400, height=550, res=120)
par(mfrow=c(1,3), mar=c(4,4,3,1))

# Left: full scatter with segmented line
plot(sub1_c$pos_z, sub1_c$ret_4w*100, pch=16, cex=0.6,
     col=ifelse(sub1_c$pos_pct>=0.90,"#D04040",
         ifelse(sub1_c$pos_pct<=0.10,"#3060A0","grey60")),
     xlab="pos_z", ylab="4-week return (%)",
     main="Return vs CFTC pos_z\n(red=extreme long, blue=extreme short)")
abline(lm(ret_4w*100 ~ pos_z, data=sub1_c), col="black", lwd=2)
abline(v=quantile(sub1_c$pos_z, c(0.10,0.90)), lty=2, col=c("#3060A0","#D04040"))
abline(h=0, lty=3, col="grey50")
legend("topleft", c("Ext. Long (>90)","Ext. Short (<10)","Normal"),
       col=c("#D04040","#3060A0","grey60"), pch=16, cex=0.7)

# Middle: boxplot of 4W returns by extreme/normal
bdat <- list(
  "Ext Short\n(<10pct)" = sub_xshort$ret_4w*100,
  "Normal\n(10-90pct)" = sub_norm$ret_4w*100,
  "Ext Long\n(>90pct)" = sub_xlong$ret_4w*100
)
boxplot(bdat, col=c("#3060A0","grey80","#D04040"),
        main="4-Week Return Distribution\nby Positioning Regime",
        ylab="4-week return (%)", las=1)
abline(h=0, lty=2, col="red")

# Right: rolling 52-week correlation
roll_cor <- function(x, y, w=52) {
  n <- length(x); r <- rep(NA,n)
  for (i in seq(w,n)) {
    xw <- x[(i-w+1):i]; yw <- y[(i-w+1):i]
    if (sum(!is.na(xw) & !is.na(yw)) > 10)
      r[i] <- cor(xw, yw, use="complete")
  }
  r
}
sub1_ord <- sub1_c[order(release_date)]
rcor <- roll_cor(sub1_ord$pos_z, sub1_ord$ret_4w)
plot(sub1_ord$release_date, rcor, type="l", col="#3060A0", lwd=1.5,
     xlab="", ylab="Rolling 52-week correlation",
     main="Rolling Correlation: pos_z vs ret_4w\n(52-week window)")
abline(h=0, lty=2, col="red")
abline(h=c(-0.2,0.2), lty=3, col="grey60")
# shade periods
rect(as.Date("2020-01-01"), -1, as.Date("2022-12-31"), 1,
     col=rgb(1,0.8,0.8,0.3), border=NA)
text(as.Date("2021-01-01"), min(rcor,na.rm=TRUE)*0.9, "COVID\n+Energy", cex=0.7, col="red")
dev.off()
cat("  Saved: output/cftc/threshold_regression.png\n")

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 4: Markov-Switching Regression (2-state)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("STEP 4: MARKOV-SWITCHING REGRESSION (2-state)\n")
cat("══════════════════════════════════════════════════\n")

# MSwM fits a 2-state MS model where both intercept and coefficient can switch
# States interpreted as: high-volatility (crisis) vs low-volatility (normal)
tryCatch({
  ms_data <- sub1_c[!is.na(ret_4w) & !is.na(pos_z)][order(release_date)]
  base_lm  <- lm(ret_4w ~ pos_z, data=ms_data)
  ms_mod   <- msmFit(base_lm, k=2, sw=c(TRUE,TRUE,TRUE),
                     control=list(maxiter=500, tol=1e-6))

  cat("\nMarkov-Switching Model Summary:\n")
  sm_ms <- summary(ms_mod)

  # Extract transition probabilities
  tp <- ms_mod@transMat
  cat(sprintf("  Transition matrix:\n"))
  cat(sprintf("    P(stay in S1 | S1) = %.3f   P(move to S2 | S1) = %.3f\n", tp[1,1], tp[1,2]))
  cat(sprintf("    P(move to S1 | S2) = %.3f   P(stay in S2 | S2) = %.3f\n", tp[2,1], tp[2,2]))

  # Implied regime durations (expected weeks in each state)
  dur1 <- 1 / (1 - tp[1,1])
  dur2 <- 1 / (1 - tp[2,2])
  cat(sprintf("  Expected duration: State 1 = %.1f weeks,  State 2 = %.1f weeks\n", dur1, dur2))

  # State-specific coefficients
  coef_ms <- ms_mod@Coef
  se_ms   <- ms_mod@seCoef
  cat("\n  State-specific coefficients:\n")
  for (st in 1:2) {
    cat(sprintf("  State %d:  intercept=%+.4f  pos_z=%+.4f  sigma=%.4f\n",
                st, coef_ms[st,1], coef_ms[st,2], ms_mod@sigma[st]))
  }

  # Smoothed state probabilities
  smooth_probs <- ms_mod@Fit@smoProb[,1]  # P(state 1)
  ms_dates     <- ms_data$release_date

  # Save state classification
  ms_out <- data.table(
    release_date = ms_dates,
    ret_4w       = ms_data$ret_4w,
    pos_z        = ms_data$pos_z,
    prob_state1  = round(smooth_probs, 4),
    state        = ifelse(smooth_probs > 0.5, 1L, 2L)
  )
  fwrite(ms_out, "output/cftc/cftc_ms_states.csv")

  # MS plot
  png("output/cftc/markov_switching.png", width=1400, height=700, res=120)
  par(mfrow=c(2,1), mar=c(2,4,2,2), oma=c(2,0,3,0))

  # Top: smoothed probability of State 1
  plot(ms_dates, smooth_probs, type="l", col="#3060A0", lwd=1.5,
       ylim=c(0,1), xlab="", ylab="P(State 1)",
       main="Smoothed State 1 Probability")
  abline(h=0.5, lty=2, col="red")
  polygon(c(ms_dates, rev(ms_dates)),
          c(smooth_probs, rep(0, length(smooth_probs))),
          col=rgb(0.2,0.4,0.8,0.2), border=NA)

  # Bottom: WTI price coloured by state
  plot(ms_data$release_date, ms_data$price, type="l", col="grey50",
       xlab="", ylab="WTI Price ($/bbl)", main="WTI Price with Regime Overlay")
  state1_idx <- which(smooth_probs > 0.5)
  if (length(state1_idx))
    points(ms_dates[state1_idx], ms_data$price[state1_idx],
           pch=16, cex=0.4, col="#3060A0")
  state2_idx <- which(smooth_probs <= 0.5)
  if (length(state2_idx))
    points(ms_dates[state2_idx], ms_data$price[state2_idx],
           pch=16, cex=0.4, col="#D04040")
  legend("topright", c("State 1","State 2"), col=c("#3060A0","#D04040"), pch=16, cex=0.8)
  mtext("Markov-Switching Regression (2-state): CFTC vs WTI 4-week return",
        outer=TRUE, cex=1.0, font=2)
  dev.off()
  cat("  Saved: output/cftc/markov_switching.png\n")

  # State-level return analysis
  cat("\n  Returns by identified state:\n")
  for (st in 1:2) {
    s_sub <- ms_out[state==st]
    cat(sprintf("  State %d (n=%d):  avg_ret_4w=%+.2f%%  sd=%.2f%%  hit_up=%.1f%%\n",
                st, nrow(s_sub),
                mean(s_sub$ret_4w,na.rm=TRUE)*100,
                sd(s_sub$ret_4w,na.rm=TRUE)*100,
                mean(s_sub$ret_4w>0,na.rm=TRUE)*100))
  }

}, error=function(e) {
  cat(sprintf("  Markov-switching failed: %s\n", e$message))
  cat("  (Common cause: local optimum / convergence issue — interpret with caution)\n")
})

# ═══════════════════════════════════════════════════════════════════════════════
# FINAL SUMMARY TABLE
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("FINAL MODEL COMPARISON TABLE\n")
cat("══════════════════════════════════════════════════\n")
cat(sprintf("%-40s  %5s  %8s  %6s  %8s  %6s  %6s\n",
            "Model / Subset","n","coef_ols","pOLS","coef_NW","pNW","R2"))
cat(strrep("-",90), "\n")

final_rows <- rbind(
  period_dt[, .(label=period, n, coef_ols, pval_ols, coef_nw, pval_nw, r2)],
  thr_rows[, .(label=paste("Threshold:",subset), n, coef_ols, pval_ols, coef_nw, pval_nw, r2)]
)
for (i in seq_len(nrow(final_rows))) {
  r <- final_rows[i]
  cat(sprintf("%-40s  %5d  %+8.4f  %6.3f  %+8.4f  %6.3f  %6.4f\n",
              substr(r$label,1,40), r$n, r$coef_ols, r$pval_ols, r$coef_nw, r$pval_nw, r$r2))
}

cat("\nOutputs saved to output/cftc/\n")
cat("  cftc_subperiod_results.csv\n  cftc_hac_results.csv\n")
cat("  cftc_threshold_results.csv\n  cftc_ms_states.csv\n")
cat("  subperiod_scatter.png\n  threshold_regression.png\n  markov_switching.png\n")
