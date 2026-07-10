# R/cftc_yahoo_ms.R
# ─────────────────────────────────────────────────────────────────────────────
# CFTC ANALYSIS ON YAHOO FINANCE WTI WEEKLY DATA (2016-2026, 531 obs)
# Key addition: Markov-switching with FILTERED probabilities (no look-ahead)
#   - Filtered:  P(S_t | y_1,...,y_t)  — forward pass only  [VALID OOS signal]
#   - Smoothed:  P(S_t | y_1,...,y_T)  — uses future data   [in-sample only]
#   - R² gap between the two = magnitude of look-ahead bias in smoother
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table); library(readxl)
  library(lmtest); library(sandwich)
  library(segmented); library(MSwM)
  library(nortest); library(car)
})

setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
dir.create("output/cftc", showWarnings=FALSE)

zsc <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)

roll_z52 <- function(x, w=52) {
  n <- length(x); z <- rep(NA_real_, n)
  for (i in seq_along(x)) {
    avail <- max(2L, min(w, i))
    win   <- x[(i - avail + 1L):i]
    if (sum(!is.na(win)) >= 2L)
      z[i] <- (x[i] - mean(win, na.rm=TRUE)) / sd(win, na.rm=TRUE)
  }; z
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART A: LOAD + MERGE
# ═══════════════════════════════════════════════════════════════════════════════
cat("Loading Yahoo WTI weekly + CFTC...\n")
wti  <- fread("output/wti_weekly.csv")
wti[, date := as.Date(date)]

cftc_raw <- as.data.table(read_excel("CFTC 2016-2026 CL.xlsx", sheet="Sheet1"))
cftc_raw[, release_date := as.Date(releasedate)]
cftc_raw[, net_pos      := as.numeric(actual)]
cftc <- unique(cftc_raw[!is.na(net_pos)][order(release_date)], by="release_date")

mf <- merge(cftc[, .(release_date, net_pos)],
            wti[, .(release_date=date, wti_close)],
            by="release_date", all.x=FALSE)
mf <- mf[!is.na(wti_close)][order(release_date)]
cat(sprintf("  Merged: %d rows  %s to %s\n", nrow(mf),
            min(mf$release_date), max(mf$release_date)))

# Forward prices & returns
mf[, price_1w := shift(wti_close, -1L, type="lead")]
mf[, price_2w := shift(wti_close, -2L, type="lead")]
mf[, price_4w := shift(wti_close, -4L, type="lead")]
mf[, ret_1w   := (price_1w - wti_close) / wti_close]
mf[, ret_2w   := (price_2w - wti_close) / wti_close]
mf[, ret_4w   := (price_4w - wti_close) / wti_close]

# Positioning metrics
mf[, net_pos_chg := net_pos - shift(net_pos, 1L)]
mf[, pos_pct     := frank(net_pos, ties.method="average") / .N]
mf[, pos_z       := zsc(net_pos)]
mf[, pos_z52     := roll_z52(net_pos)]
mf[, mm_chg_z    := zsc(net_pos_chg)]

mf[, regime := fcase(
  pos_pct >= 0.90, "Extreme Long  (>90th)",
  pos_pct <= 0.10, "Extreme Short (<10th)",
  pos_pct >= 0.60, "Long  (60-90th)",
  pos_pct <= 0.40, "Short (10-40th)",
  default          = "Neutral (40-60th)"
)]

cat(sprintf("  Extreme Long:  %d  |  Extreme Short: %d\n",
            sum(mf$pos_pct >= 0.90), sum(mf$pos_pct <= 0.10)))
cat(sprintf("  WTI range: $%.2f to $%.2f\n",
            min(mf$wti_close), max(mf$wti_close)))

# ── Regime return table ───────────────────────────────────────────────────────
cat("\n=== REGIME RETURNS (Yahoo WTI) ===\n")
regime_stats <- mf[!is.na(ret_4w), .(
  n         = .N,
  avg_1w    = round(mean(ret_1w, na.rm=TRUE)*100, 2),
  avg_2w    = round(mean(ret_2w, na.rm=TRUE)*100, 2),
  avg_4w    = round(mean(ret_4w, na.rm=TRUE)*100, 2),
  med_4w    = round(median(ret_4w, na.rm=TRUE)*100, 2),
  hit_up_4w = round(mean(ret_4w > 0, na.rm=TRUE)*100, 1)
), keyby=regime]
print(as.data.frame(regime_stats))
fwrite(regime_stats, "output/cftc/yahoo_regime_returns.csv")

# ═══════════════════════════════════════════════════════════════════════════════
# PART B: BASE OLS MODELS
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n=== BASE OLS MODELS ===\n")
sub1 <- mf[!is.na(ret_4w) & !is.na(pos_z)]
m1   <- lm(ret_4w ~ pos_z, data=sub1)
m2   <- lm(ret_4w ~ pos_z + mm_chg_z, data=sub1[!is.na(mm_chg_z)])
cat(sprintf("Model 1 (pos_z only):         n=%d  R2=%.4f  pos_z coef=%+.4f  p=%.4f\n",
            nobs(m1), summary(m1)$r.squared,
            coef(summary(m1))["pos_z",1], coef(summary(m1))["pos_z",4]))
cat(sprintf("Model 2 (pos_z + chg_z):      n=%d  R2=%.4f  pos_z coef=%+.4f  p=%.4f\n",
            nobs(m2), summary(m2)$r.squared,
            coef(summary(m2))["pos_z",1], coef(summary(m2))["pos_z",4]))

# ═══════════════════════════════════════════════════════════════════════════════
# PART C: OLS DIAGNOSTICS + OUTLIER REMOVAL
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("PART C: OLS DIAGNOSTICS\n")
cat("══════════════════════════════════════════════════\n")

res <- residuals(m1)
sw  <- shapiro.test(res); ad <- ad.test(res)
bp  <- bptest(m1); dw <- dwtest(m1, alternative="two.sided")
lb  <- Box.test(res, lag=4, type="Ljung-Box")
cat(sprintf("  SW normality:    W=%.4f  p=%.4f  %s\n", sw$statistic, sw$p.value, ifelse(sw$p.value<0.05,"NON-NORMAL","OK")))
cat(sprintf("  AD normality:    A=%.4f  p=%.4f  %s\n", ad$statistic, ad$p.value, ifelse(ad$p.value<0.05,"NON-NORMAL","OK")))
cat(sprintf("  Skew/Kurt:       %.3f / %.3f\n",
    mean((res-mean(res))^3)/sd(res)^3, mean((res-mean(res))^4)/sd(res)^4))
cat(sprintf("  Breusch-Pagan:   LM=%.3f  p=%.4f  %s\n", bp$statistic, bp$p.value, ifelse(bp$p.value<0.05,"HETEROSKED","OK")))
cat(sprintf("  Durbin-Watson:   D=%.4f  p=%.4f  %s\n", dw$statistic, dw$p.value, ifelse(dw$p.value<0.05,"AUTOCORR (structural)","OK")))
cat(sprintf("  Ljung-Box(4):    Q=%.3f  p=%.4f  %s\n", lb$statistic, lb$p.value, ifelse(lb$p.value<0.05,"SERIAL CORR","OK")))

# Outlier detection
n1 <- nobs(m1); p1 <- length(coef(m1))
cook <- cooks.distance(m1); hat <- hatvalues(m1); sres <- rstudent(m1)
nf   <- (cook > 4/n1) + (hat > 2*(p1+1)/n1) + (abs(sres) > 2.5)
out1 <- which(nf >= 2)
cat(sprintf("\nOutliers flagged (>=2 criteria): %d\n", length(out1)))
if (length(out1)) {
  od <- data.table(release_date=sub1$release_date[out1], net_pos=sub1$net_pos[out1],
                   pos_z=round(sub1$pos_z[out1],3), ret_4w=round(sub1$ret_4w[out1]*100,2),
                   cook_d=round(cook[out1],4), stud_r=round(sres[out1],3))
  print(as.data.frame(od))
}

sub1_c <- if (length(out1)) sub1[-out1] else sub1
m1c    <- lm(ret_4w ~ pos_z, data=sub1_c)
cat(sprintf("\nAfter removal: n=%d (-%d)  R2=%.4f→%.4f  pos_z p: %.4f→%.4f\n",
    nobs(m1c), length(out1), summary(m1)$r.squared, summary(m1c)$r.squared,
    coef(summary(m1))["pos_z",4], coef(summary(m1c))["pos_z",4]))

# Diagnostic plots
png("output/cftc/yahoo_diag_m1.png", width=1400, height=1100, res=130)
par(mfrow=c(2,2), mar=c(4,4,3,2), oma=c(0,0,3,0))
res_c <- residuals(m1c); fit_c <- fitted(m1c); sr_c <- rstudent(m1c)
plot(fit_c, res_c, pch=16, cex=0.6, col=rgb(0.2,0.4,0.8,0.5),
     xlab="Fitted", ylab="Residuals", main="Residuals vs Fitted")
abline(h=0, col="red", lty=2); lines(lowess(fit_c, res_c), col="darkred", lwd=2)
big <- which(abs(res_c) > 2.5*sd(res_c))
if (length(big)) text(fit_c[big], res_c[big], labels=big, cex=0.5, col="red", pos=3)
qqnorm(sr_c, pch=16, cex=0.6, col=rgb(0.2,0.4,0.8,0.5), main="Normal Q-Q (Studentised)")
qqline(sr_c, col="red", lwd=2)
plot(fit_c, sqrt(abs(sr_c)), pch=16, cex=0.6, col=rgb(0.2,0.6,0.3,0.5),
     xlab="Fitted", ylab=expression(sqrt("|Stud. Res|")), main="Scale-Location")
lines(lowess(fit_c, sqrt(abs(sr_c))), col="darkgreen", lwd=2)
acf(res_c, lag.max=20, main="ACF of Residuals", col=rgb(0.2,0.4,0.8,0.7), lwd=2)
mtext("OLS Diagnostics — Yahoo WTI (Model 1, cleaned)", outer=TRUE, cex=1.1, font=2)
dev.off(); cat("  Saved: output/cftc/yahoo_diag_m1.png\n")

# ═══════════════════════════════════════════════════════════════════════════════
# PART D: SUB-PERIOD STABILITY
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("PART D: SUB-PERIOD STABILITY\n")
cat("══════════════════════════════════════════════════\n")

periods <- list(
  "2016-2019 (pre-COVID)"                    = c(as.Date("2016-01-01"), as.Date("2019-12-31")),
  "2020-2022 (COVID + energy spike)"          = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
  "2023-2026 (post-normalisation)"            = c(as.Date("2023-01-01"), as.Date("2026-12-31")),
  "Full (2016-2026)"                          = c(as.Date("2016-01-01"), as.Date("2026-12-31"))
)

period_res <- rbindlist(lapply(names(periods), function(pnm) {
  d1 <- periods[[pnm]][1]; d2 <- periods[[pnm]][2]
  s  <- sub1_c[release_date >= d1 & release_date <= d2]
  if (nrow(s) < 20) { cat(sprintf("  [%-42s] n=%d — skipped\n", pnm, nrow(s))); return(NULL) }
  m  <- lm(ret_4w ~ pos_z, data=s); sm <- summary(m)
  lag_nw <- max(1, floor(nrow(s)^(1/3)))
  nw <- coeftest(m, vcov=NeweyWest(m, lag=lag_nw, prewhite=FALSE))
  cat(sprintf("  [%-42s] n=%3d  coef=%+.4f  pOLS=%.3f  pNW=%.3f  R2=%.4f  avg4w=%+.2f%%\n",
              pnm, nrow(s), coef(sm)["pos_z",1], coef(sm)["pos_z",4],
              nw["pos_z",4], sm$r.squared, mean(s$ret_4w,na.rm=TRUE)*100))
  data.table(period=pnm, n=nrow(s), nw_lag=lag_nw,
             coef=round(coef(sm)["pos_z",1],5),
             pOLS=round(coef(sm)["pos_z",4],4), pNW=round(nw["pos_z",4],4),
             r2=round(sm$r.squared,4), avg_4w=round(mean(s$ret_4w,na.rm=TRUE)*100,2))
}), fill=TRUE)
if (nrow(period_res)) fwrite(period_res, "output/cftc/yahoo_subperiod.csv")

# Sub-period scatter
avail_p <- names(periods)[sapply(names(periods), function(pnm) {
  d1<-periods[[pnm]][1]; d2<-periods[[pnm]][2]
  nrow(sub1_c[release_date>=d1 & release_date<=d2])>=20
})]
if (length(avail_p)) {
  nc <- length(avail_p)
  png("output/cftc/yahoo_subperiod_scatter.png", width=400*nc, height=420, res=100)
  par(mfrow=c(1,nc), mar=c(4,4,3,1))
  cols <- c("#3060A0","#C03030","#30A060","#A06020")
  for (i in seq_along(avail_p)) {
    pnm <- avail_p[i]; d1<-periods[[pnm]][1]; d2<-periods[[pnm]][2]
    s <- sub1_c[release_date>=d1 & release_date<=d2]
    mp <- lm(ret_4w ~ pos_z, data=s)
    plot(s$pos_z, s$ret_4w*100, pch=16, cex=0.6, col=adjustcolor(cols[i],0.5),
         xlab="pos_z", ylab="4W return (%)", main=pnm, cex.main=0.75)
    abline(mp, col=cols[i], lwd=2); abline(h=0, v=0, lty=3, col="grey60")
    legend("topleft", sprintf("b=%+.3f\np=%.3f\nR2=%.3f",
           coef(mp)[2], summary(mp)$coef[2,4], summary(mp)$r.squared), bty="n", cex=0.75)
  }
  dev.off(); cat("  Saved: output/cftc/yahoo_subperiod_scatter.png\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART E: NEWEY-WEST HAC
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("PART E: NEWEY-WEST HAC (lag=3)\n")
cat("══════════════════════════════════════════════════\n")
nw_full <- coeftest(m1c, vcov=NeweyWest(m1c, lag=3, prewhite=FALSE))
ols_sm  <- coef(summary(m1c))
cat(sprintf("  %-20s  OLS coef=%+.5f  p=%.4f  |  NW p=%.4f\n",
            "pos_z", ols_sm["pos_z",1], ols_sm["pos_z",4], nw_full["pos_z",4]))

# ═══════════════════════════════════════════════════════════════════════════════
# PART F: THRESHOLD REGRESSION
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("PART F: THRESHOLD REGRESSION\n")
cat("══════════════════════════════════════════════════\n")

chow_test <- function(data, col, val, lbl) {
  tryCatch({
    f <- as.formula(paste0("ret_4w ~ pos_z * I(", col, ">", val, ")"))
    ft <- anova(lm(ret_4w ~ pos_z, data=data), lm(f, data=data))
    cat(sprintf("  Chow @ %-22s F=%.3f  p=%.4f  %s\n", lbl,
                ft$F[2], ft$`Pr(>F)`[2], ifelse(ft$`Pr(>F)`[2]<0.05,"*** BREAK","no break")))
  }, error=function(e) cat(sprintf("  Chow @ %s: error\n", lbl)))
}
chow_test(sub1_c, "pos_pct", 0.90, "extreme long  (>90th):")
chow_test(sub1_c, "pos_pct", 0.10, "extreme short (<10th):")
chow_test(sub1_c, "pos_pct", 0.50, "median:")

cat("\n  Regime-conditional regressions:\n")
thresh_rows <- rbindlist(lapply(list(
  list(sub1_c,                                  "Full cleaned"),
  list(sub1_c[pos_pct>=0.90|pos_pct<=0.10],     "Extremes (<10 or >90)"),
  list(sub1_c[pos_pct>0.10&pos_pct<0.90],        "Non-extreme (10-90)"),
  list(sub1_c[pos_pct>=0.90],                    "Extreme Long (>90)"),
  list(sub1_c[pos_pct<=0.10],                    "Extreme Short (<10)")
), function(x) {
  s <- x[[1]]; lbl <- x[[2]]
  if (nrow(s) < 15) return(NULL)
  m <- lm(ret_4w ~ pos_z, data=s)
  nw <- tryCatch(coeftest(m, vcov=NeweyWest(m, lag=min(3,floor(nrow(s)/4)), prewhite=FALSE)),
                 error=function(e) coef(summary(m)))
  cat(sprintf("  [%-26s] n=%3d  coef=%+.4f  pOLS=%.3f  pNW=%.3f  R2=%.4f  avg4w=%+.2f%%\n",
              lbl, nrow(s), coef(m)[2], coef(summary(m))[2,4],
              nw[2,4], summary(m)$r.squared, mean(s$ret_4w,na.rm=TRUE)*100))
  data.table(subset=lbl, n=nrow(s), coef_ols=round(coef(m)[2],5),
             pOLS=round(coef(summary(m))[2,4],4), pNW=round(nw[2,4],4),
             r2=round(summary(m)$r.squared,4), avg4w=round(mean(s$ret_4w,na.rm=TRUE)*100,2))
}), fill=TRUE)
if (nrow(thresh_rows)) fwrite(thresh_rows, "output/cftc/yahoo_threshold_results.csv")

# ═══════════════════════════════════════════════════════════════════════════════
# PART G: MARKOV-SWITCHING — FILTERED vs SMOOTHED
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("PART G: MARKOV-SWITCHING  (filtered vs smoothed)\n")
cat("══════════════════════════════════════════════════\n")

ms_data <- sub1_c[order(release_date)]
base_lm <- lm(ret_4w ~ pos_z, data=ms_data)

tryCatch({
  ms_mod <- msmFit(base_lm, k=2, sw=c(TRUE,TRUE,TRUE),
                   control=list(maxiter=500, tol=1e-6))
  cat("  MS model fitted\n")

  # ── Extract filtered AND smoothed probabilities ─────────────────────────────
  # MSwM: filtProb is n×k (one row per obs); smoProb is (n+1)×k (includes t=0 initial)
  filt <- ms_mod@Fit@filtProb               # P(S_t | y_1,...,y_t) — forward pass only
  smo  <- ms_mod@Fit@smoProb[-1, , drop=FALSE]  # P(S_t | y_1,...,y_T) — drop t=0 row

  cat(sprintf("  filtProb dim (after align): %d x %d\n", nrow(filt), ncol(filt)))
  cat(sprintf("  smoProb  dim (after align): %d x %d\n", nrow(smo),  ncol(smo)))

  # Identify bull state by highest intercept in the coefficient matrix
  cf       <- ms_mod@Coef
  bull_col <- which.max(cf[, 1])   # state with highest intercept = bull regime
  bear_col <- which.min(cf[, 1])
  cat(sprintf("  Bull state column: %d (intercept=%+.4f)\n", bull_col, cf[bull_col,1]))
  cat(sprintf("  Bear state column: %d (intercept=%+.4f)\n", bear_col, cf[bear_col,1]))

  # Probability of being in bull state
  p_bull_filt <- filt[, bull_col]
  p_bull_smo  <- smo[,  bull_col]

  # Correlation between filtered and smoothed
  r_fs <- cor(p_bull_filt, p_bull_smo, use="complete")
  cat(sprintf("  Corr(filtered, smoothed): %.4f\n", r_fs))

  # ── State classification ────────────────────────────────────────────────────
  state_filt <- as.integer(ifelse(p_bull_filt > 0.5, bull_col, bear_col))
  state_smo  <- as.integer(ifelse(p_bull_smo  > 0.5, bull_col, bear_col))
  agree_pct  <- mean(state_filt == state_smo) * 100
  cat(sprintf("  State agreement (filt vs smo): %.1f%% of obs\n", agree_pct))

  # ── Regime statistics ────────────────────────────────────────────────────────
  cat("\n  STATE STATISTICS:\n")
  cat(sprintf("  %-12s  %-12s  %5s  %10s  %8s  %8s\n",
              "Prob type","State","n","avg_ret4w","sd_ret4w","pct_up"))
  for (prob_type in c("Filtered","Smoothed")) {
    st_vec <- if (prob_type=="Filtered") state_filt else state_smo
    for (st in sort(unique(st_vec))) {
      idx <- which(st_vec == st)
      lbl <- if (st == bull_col) "Bull" else "Bear"
      cat(sprintf("  %-12s  %-12s  %5d  %+9.2f%%  %7.2f%%  %7.0f%%\n",
                  prob_type, paste0("S",st,"(",lbl,")"), length(idx),
                  mean(ms_data$ret_4w[idx],na.rm=TRUE)*100,
                  sd(ms_data$ret_4w[idx],na.rm=TRUE)*100,
                  mean(ms_data$ret_4w[idx]>0,na.rm=TRUE)*100))
    }
  }

  # ── Regression: ret_4w ~ regime signal (filtered vs smoothed) ──────────────
  cat("\n  REGRESSION ON REGIME SIGNAL:\n")

  # Approach 1: Binary regime (0/1)
  ms_data[, state_filt := state_filt]
  ms_data[, state_smo  := state_smo]
  ms_data[, bull_filt  := as.integer(state_filt == bull_col)]
  ms_data[, bull_smo   := as.integer(state_smo  == bull_col)]
  ms_data[, p_bull_f   := p_bull_filt]
  ms_data[, p_bull_s   := p_bull_smo]

  reg_results <- rbindlist(list(
    {m<-lm(ret_4w~pos_z,           data=ms_data); data.table(model="OLS (no regime)",      n=nobs(m), r2=round(summary(m)$r.squared,4), pz=round(coef(summary(m))["pos_z",4],4), signal="—")},
    {m<-lm(ret_4w~bull_filt,       data=ms_data); data.table(model="Filtered binary",      n=nobs(m), r2=round(summary(m)$r.squared,4), pz=round(coef(summary(m))["bull_filt",4],4), signal="filtProb>0.5")},
    {m<-lm(ret_4w~bull_smo,        data=ms_data); data.table(model="Smoothed binary",      n=nobs(m), r2=round(summary(m)$r.squared,4), pz=round(coef(summary(m))["bull_smo",4],4), signal="smoProb>0.5")},
    {m<-lm(ret_4w~p_bull_f,        data=ms_data); data.table(model="Filtered continuous",  n=nobs(m), r2=round(summary(m)$r.squared,4), pz=round(coef(summary(m))["p_bull_f",4],4), signal="filtProb (0-1)")},
    {m<-lm(ret_4w~p_bull_s,        data=ms_data); data.table(model="Smoothed continuous",  n=nobs(m), r2=round(summary(m)$r.squared,4), pz=round(coef(summary(m))["p_bull_s",4],4), signal="smoProb (0-1)")},
    {m<-lm(ret_4w~bull_filt+pos_z, data=ms_data); data.table(model="Filtered + pos_z",    n=nobs(m), r2=round(summary(m)$r.squared,4), pz=round(coef(summary(m))["pos_z",4],4), signal="filtProb>0.5 + pos_z")},
    {m<-lm(ret_4w~bull_smo+pos_z,  data=ms_data); data.table(model="Smoothed + pos_z",    n=nobs(m), r2=round(summary(m)$r.squared,4), pz=round(coef(summary(m))["pos_z",4],4), signal="smoProb>0.5 + pos_z")}
  ), fill=TRUE)

  cat(sprintf("\n  %-28s  %5s  %8s  %8s  %s\n", "Model","n","R2","p(signal)","Signal"))
  for (i in seq_len(nrow(reg_results))) {
    cat(sprintf("  %-28s  %5d  %8.4f  %8.4f  %s\n",
                reg_results$model[i], reg_results$n[i],
                reg_results$r2[i], reg_results$pz[i], reg_results$signal[i]))
  }

  r2_filt_bin  <- reg_results[model=="Filtered binary",  r2]
  r2_smo_bin   <- reg_results[model=="Smoothed binary",  r2]
  r2_filt_cont <- reg_results[model=="Filtered continuous", r2]
  r2_smo_cont  <- reg_results[model=="Smoothed continuous", r2]
  cat(sprintf("\n  Look-ahead bias (binary):     smoothed R2 %.4f vs filtered R2 %.4f  (gap=%+.4f)\n",
              r2_smo_bin, r2_filt_bin, r2_filt_bin - r2_smo_bin))
  cat(sprintf("  Look-ahead bias (continuous): smoothed R2 %.4f vs filtered R2 %.4f  (gap=%+.4f)\n",
              r2_smo_cont, r2_filt_cont, r2_filt_cont - r2_smo_cont))
  fwrite(reg_results, "output/cftc/yahoo_ms_regime_regression.csv")

  # ── Transition matrix + state coefficients ──────────────────────────────────
  tp <- ms_mod@transMat; cf <- ms_mod@Coef
  cat(sprintf("\n  Transition matrix:\n"))
  cat(sprintf("  P(S1->S1)=%.3f  P(S1->S2)=%.3f  (dur S1: %.1f wks)\n",
              tp[1,1], tp[1,2], 1/(1-tp[1,1])))
  cat(sprintf("  P(S2->S1)=%.3f  P(S2->S2)=%.3f  (dur S2: %.1f wks)\n",
              tp[2,1], tp[2,2], 1/(1-tp[2,2])))
  cat(sprintf("  State 1: intercept=%+.4f  pos_z=%+.4f\n", cf[1,1], cf[1,2]))
  cat(sprintf("  State 2: intercept=%+.4f  pos_z=%+.4f\n", cf[2,1], cf[2,2]))
  tryCatch({
    sm_summary <- summary(ms_mod)
    cat(sprintf("  logLik=%.4f  AIC=%.2f  BIC=%.2f\n",
                sm_summary@logLike, sm_summary@AIC, sm_summary@BIC))
  }, error=function(e) cat("  (model fit stats unavailable)\n"))

  # ── Simple trading strategy on filtered signal ───────────────────────────────
  strat <- ms_data[!is.na(ret_4w), .(
    release_date, ret_4w, bull_filt,
    strat_ret = ifelse(bull_filt == 1L, ret_4w, -ret_4w)   # long bull, short bear
  )]
  cat(sprintf("\n  FILTERED-SIGNAL TRADING STRATEGY (long bull / short bear):\n"))
  cat(sprintf("  Avg strategy return: %+.2f%%  (vs buy&hold: %+.2f%%)\n",
              mean(strat$strat_ret,na.rm=TRUE)*100,
              mean(strat$ret_4w,   na.rm=TRUE)*100))
  cat(sprintf("  Strategy SR (annualised, 52W): %.3f\n",
              mean(strat$strat_ret,na.rm=TRUE)/sd(strat$strat_ret,na.rm=TRUE)*sqrt(13)))
  cat(sprintf("  Hit rate: %.1f%%\n", mean(strat$strat_ret>0,na.rm=TRUE)*100))

  # ── Save state data ─────────────────────────────────────────────────────────
  ms_out <- data.table(
    release_date=ms_data$release_date, wti=ms_data$wti_close,
    net_pos=ms_data$net_pos, pos_z=round(ms_data$pos_z,4),
    ret_4w=round(ms_data$ret_4w,6),
    p_bull_filtered=round(p_bull_filt,4), p_bull_smoothed=round(p_bull_smo,4),
    state_filtered=state_filt, state_smoothed=state_smo
  )
  fwrite(ms_out, "output/cftc/yahoo_ms_states.csv")

  # ── PLOTS ───────────────────────────────────────────────────────────────────
  # Plot 1: Filtered vs Smoothed probability over time
  png("output/cftc/yahoo_ms_filt_vs_smo.png", width=1400, height=800, res=120)
  par(mfrow=c(3,1), mar=c(2,4,2,2), oma=c(2,0,3,0))

  plot(ms_out$release_date, ms_out$p_bull_smoothed,
       type="l", col="#C03030", lwd=1.5, ylim=c(0,1),
       xlab="", ylab="P(Bull state)", main="Smoothed P(Bull) — uses future data")
  lines(ms_out$release_date, ms_out$p_bull_filtered, col="#3060A0", lwd=1.5)
  abline(h=0.5, lty=2, col="grey40")
  legend("topright", c("Smoothed (look-ahead)","Filtered (real-time)"),
         col=c("#C03030","#3060A0"), lwd=2, bty="n", cex=0.8)

  # Difference: smoothed - filtered
  diff_pf <- ms_out$p_bull_smoothed - ms_out$p_bull_filtered
  plot(ms_out$release_date, diff_pf,
       type="l", col="#8030A0", lwd=1.2, ylim=range(diff_pf, na.rm=TRUE),
       xlab="", ylab="Smoothed - Filtered",
       main=sprintf("Look-ahead gap (cor=%.3f, avg abs diff=%.3f)", r_fs, mean(abs(diff_pf),na.rm=TRUE)))
  abline(h=0, lty=2, col="grey40")
  polygon(c(ms_out$release_date, rev(ms_out$release_date)),
          c(pmax(diff_pf,0), rep(0,nrow(ms_out))), col=rgb(0.8,0.2,0.6,0.2), border=NA)
  polygon(c(ms_out$release_date, rev(ms_out$release_date)),
          c(pmin(diff_pf,0), rep(0,nrow(ms_out))), col=rgb(0.2,0.4,0.8,0.2), border=NA)

  # WTI price with filtered state overlay
  plot(ms_out$release_date, ms_out$wti,
       type="l", col="grey40", lwd=1,
       xlab="", ylab="WTI ($/bbl)", main="WTI with Filtered State Overlay")
  points(ms_out$release_date[ms_out$state_filtered==bull_col],
         ms_out$wti[ms_out$state_filtered==bull_col],
         pch=16, cex=0.5, col="#3060A0")
  points(ms_out$release_date[ms_out$state_filtered==bear_col],
         ms_out$wti[ms_out$state_filtered==bear_col],
         pch=16, cex=0.5, col="#C03030")
  legend("topright", c("Bull (filtered)","Bear (filtered)"),
         col=c("#3060A0","#C03030"), pch=16, bty="n", cex=0.8)
  mtext("Markov-Switching: Filtered vs Smoothed State Probabilities — Yahoo WTI 2016-2026",
        outer=TRUE, font=2, cex=0.95)
  dev.off(); cat("  Saved: output/cftc/yahoo_ms_filt_vs_smo.png\n")

  # Plot 2: R2 comparison bar chart + scatter filtered vs smoothed prob
  png("output/cftc/yahoo_ms_r2_comparison.png", width=1100, height=520, res=120)
  par(mfrow=c(1,2), mar=c(6,5,3,2))
  # Bar: R2 comparison
  r2_vals <- c(
    "OLS only"         = reg_results[model=="OLS (no regime)",       r2],
    "Filt binary"      = reg_results[model=="Filtered binary",       r2],
    "Smo binary"       = reg_results[model=="Smoothed binary",       r2],
    "Filt continuous"  = reg_results[model=="Filtered continuous",   r2],
    "Smo continuous"   = reg_results[model=="Smoothed continuous",   r2]
  )
  bcols <- c("grey60","#3060A0","#C03030","#5090C0","#E05050")
  bp_out <- barplot(r2_vals*100, col=bcols, las=2, cex.names=0.75,
                    ylab="R² (%)", main="R² by Regime Signal Type\n(filtered vs smoothed)")
  text(bp_out, r2_vals*100 + 0.05, sprintf("%.2f%%", r2_vals*100), cex=0.7, adj=0)
  abline(h=seq(0,max(r2_vals)*100,by=0.5), lty=3, col="grey85")
  # Scatter: filtered vs smoothed probability
  plot(ms_out$p_bull_filtered, ms_out$p_bull_smoothed,
       pch=16, cex=0.5, col=rgb(0.2,0.4,0.8,0.4),
       xlab="Filtered P(Bull)", ylab="Smoothed P(Bull)",
       main=sprintf("Filtered vs Smoothed prob\ncor = %.4f", r_fs))
  abline(0,1, col="red", lty=2); abline(h=0.5, v=0.5, lty=3, col="grey60")
  dev.off(); cat("  Saved: output/cftc/yahoo_ms_r2_comparison.png\n")

  # Plot 3: Cumulative strategy return
  strat_all <- ms_out[!is.na(ret_4w), .(
    release_date,
    bh_ret   = ret_4w,
    filt_ret = ifelse(state_filtered == bull_col, ret_4w, -ret_4w),
    smo_ret  = ifelse(state_smoothed == bull_col, ret_4w, -ret_4w)
  )]
  strat_all[, bh_cum   := cumprod(1 + bh_ret)   - 1]
  strat_all[, filt_cum := cumprod(1 + filt_ret) - 1]
  strat_all[, smo_cum  := cumprod(1 + smo_ret)  - 1]
  png("output/cftc/yahoo_ms_cumret.png", width=1100, height=500, res=120)
  par(mar=c(4,5,3,2))
  ylims <- range(c(strat_all$bh_cum, strat_all$filt_cum, strat_all$smo_cum)*100, na.rm=TRUE)
  plot(strat_all$release_date, strat_all$filt_cum*100,
       type="l", col="#3060A0", lwd=2, ylim=ylims,
       xlab="", ylab="Cumulative return (%)",
       main="Cumulative 4W Return: Strategy vs Buy & Hold\n(long bull / short bear, 4W non-overlapping)")
  lines(strat_all$release_date, strat_all$smo_cum*100, col="#C03030", lwd=2, lty=2)
  lines(strat_all$release_date, strat_all$bh_cum*100,  col="grey40",  lwd=1.5, lty=3)
  abline(h=0, col="grey70")
  legend("topleft", c("Filtered signal (OOS-valid)","Smoothed signal (biased)","Buy & Hold"),
         col=c("#3060A0","#C03030","grey40"), lwd=c(2,2,1.5), lty=c(1,2,3), bty="n", cex=0.85)
  dev.off(); cat("  Saved: output/cftc/yahoo_ms_cumret.png\n")

}, error=function(e) cat(sprintf("  MS model error: %s\n", e$message)))

# ═══════════════════════════════════════════════════════════════════════════════
# FINAL SUMMARY TABLE
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════\n")
cat("FINAL SUMMARY\n")
cat("══════════════════════════════════════════════════\n")
cat(sprintf("Price source  : output/wti_weekly.csv  (Yahoo Finance CL=F weekly close)\n"))
cat(sprintf("Date range    : %s to %s  (%d obs)\n",
            min(mf$release_date), max(mf$release_date), nrow(mf)))
cat(sprintf("After outlier : %d obs (-%d)\n", nrow(sub1_c), length(out1)))
cat(sprintf("Extreme Long  : %d obs (>90th pct)  |  Extreme Short: %d obs (<10th pct)\n",
            sum(sub1_c$pos_pct>=0.90), sum(sub1_c$pos_pct<=0.10)))
cat("\nOutputs in output/cftc/:\n")
cat("  yahoo_regime_returns.csv      yahoo_subperiod.csv\n")
cat("  yahoo_threshold_results.csv   yahoo_ms_regime_regression.csv\n")
cat("  yahoo_ms_states.csv\n")
cat("  yahoo_diag_m1.png             yahoo_subperiod_scatter.png\n")
cat("  yahoo_ms_filt_vs_smo.png      yahoo_ms_r2_comparison.png\n")
cat("  yahoo_ms_cumret.png\n")
