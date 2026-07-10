# R/cftc_analysis.R
# ─────────────────────────────────────────────────────────────────────────────
# CFTC Managed Money Positioning vs WTI Crude Oil
#
# TASKS:
#   1. Net position vs price relationship (correlation, lead-lag)
#   2. Extreme positioning events (top/bottom 10% and |z|>1.5)
#      => subsequent 1W, 2W, 4W price performance
#   3. Multi-variable regression: does CFTC add value beyond macro factors?
#
# OUTPUTS  (all to output/cftc/)
#   cftc_wti_merged.csv        — master merged dataset
#   cftc_extremes.csv          — event table with forward returns
#   cftc_regime_returns.csv    — mean/median/hit-rate by positioning regime
#   cftc_regression.csv        — OLS coefficient table
#   Plots: positions_vs_price, forward_returns, regression_coefs
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table); library(readxl)
})

setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
dir.create("output/cftc", showWarnings=FALSE)

# ── 1. Load data ─────────────────────────────────────────────────────────────
cat("Loading CFTC data...\n")
cftc_raw <- as.data.table(read_excel("CFTC 2016-2026 CL.xlsx", sheet="Sheet1"))
cftc_raw[, date        := as.Date(date)]
cftc_raw[, releasedate := as.Date(releasedate)]
cftc_raw[, net_pos     := as.numeric(actual)]
# keep unique release dates (some duplicates in raw feed)
cftc <- unique(cftc_raw[!is.na(net_pos)][order(-releasedate)], by="releasedate")
cat(sprintf("  CFTC: %d rows  %s to %s\n", nrow(cftc),
            min(cftc$releasedate), max(cftc$releasedate)))

cat("Loading WTI weekly prices...\n")
wti <- fread("output/wti_weekly.csv")
wti[, date := as.Date(date)]
cat(sprintf("  WTI: %d rows  %s to %s  price %.2f-%.2f\n", nrow(wti),
            min(wti$date), max(wti$date), min(wti$wti_close), max(wti$wti_close)))

# ── 2. Merge on CFTC release date ↔ WTI Friday close ────────────────────────
# CFTC releasedate = Friday; WTI weekly = Friday close => direct join
m <- merge(cftc[, .(release_date=releasedate, pos_date=date, net_pos)],
           wti[, .(release_date=date, price=wti_close)],
           by="release_date", all.x=TRUE)
m <- m[!is.na(price)][order(release_date)]

# Forward prices: +1W, +2W, +4W
m[, price_1w := shift(price, -1L, type="lead")]
m[, price_2w := shift(price, -2L, type="lead")]
m[, price_4w := shift(price, -4L, type="lead")]

# Returns
m[, ret_1w := (price_1w - price) / price]
m[, ret_2w := (price_2w - price) / price]
m[, ret_4w := (price_4w - price) / price]

# Price change (level)
m[, dp_1w := price_1w - price]
m[, dp_2w := price_2w - price]
m[, dp_4w := price_4w - price]

# ── 3. Positioning metrics ────────────────────────────────────────────────────
# Weekly change in net position
m[, net_pos_chg := net_pos - shift(net_pos, 1L)]

# Full-period percentile rank
m[, pos_pct := frank(net_pos, ties.method="average") / .N]

# Z-score (full history)
m[, pos_z   := (net_pos - mean(net_pos, na.rm=TRUE)) / sd(net_pos, na.rm=TRUE)]

# Rolling 52-week z-score (comparable to the EIA model's approach)
roll_z <- function(x, w=52) {
  n <- length(x); z <- rep(NA_real_, n)
  for (i in seq(w, n)) {
    win <- x[(i-w+1):i]
    z[i] <- (x[i] - mean(win, na.rm=TRUE)) / sd(win, na.rm=TRUE)
  }
  z
}
m[, pos_z52 := roll_z(net_pos, 52)]

# Regime labels
m[, regime := fcase(
  pos_pct >= 0.90, "Extreme Long  (>90th pct)",
  pos_pct <= 0.10, "Extreme Short (<10th pct)",
  pos_pct >= 0.60, "Long  (60-90th)",
  pos_pct <= 0.40, "Short (10-40th)",
  default          = "Neutral (40-60th)"
)]

cat(sprintf("  Merged panel: %d rows  %s to %s\n", nrow(m),
            min(m$release_date), max(m$release_date)))
fwrite(m, "output/cftc/cftc_wti_merged.csv")

# ── 4. Correlation & Lead-Lag ─────────────────────────────────────────────────
cat("\n=== CORRELATION: Net Position vs Price ===\n")
cat(sprintf("  Contemporaneous corr (net_pos, price):  %.3f\n",
            cor(m$net_pos, m$price, use="complete")))
cat(sprintf("  net_pos leads price 1W:                 %.3f\n",
            cor(m$net_pos, m$price_1w, use="complete")))
cat(sprintf("  net_pos leads price 2W:                 %.3f\n",
            cor(m$net_pos, m$price_2w, use="complete")))
cat(sprintf("  net_pos leads price 4W:                 %.3f\n",
            cor(m$net_pos, m$price_4w, use="complete")))
cat(sprintf("  net_pos_chg leads ret_1w:               %.3f\n",
            cor(m$net_pos_chg, m$ret_1w, use="complete")))
cat(sprintf("  net_pos_chg leads ret_4w:               %.3f\n",
            cor(m$net_pos_chg, m$ret_4w, use="complete")))

# ── 5. Extreme positioning analysis ──────────────────────────────────────────
cat("\n=== EXTREME POSITIONING ANALYSIS ===\n")

regime_stats <- m[!is.na(ret_4w), .(
  n        = .N,
  pct_long = round(mean(net_pos, na.rm=TRUE)),
  avg_1w   = round(mean(ret_1w, na.rm=TRUE)*100, 2),
  avg_2w   = round(mean(ret_2w, na.rm=TRUE)*100, 2),
  avg_4w   = round(mean(ret_4w, na.rm=TRUE)*100, 2),
  med_1w   = round(median(ret_1w, na.rm=TRUE)*100, 2),
  med_4w   = round(median(ret_4w, na.rm=TRUE)*100, 2),
  hit_up_1w = round(mean(ret_1w > 0, na.rm=TRUE)*100, 1),
  hit_up_4w = round(mean(ret_4w > 0, na.rm=TRUE)*100, 1),
  avg_dp_4w  = round(mean(dp_4w, na.rm=TRUE), 2)
), keyby=regime]

cat("\nReturn statistics by positioning regime:\n")
print(regime_stats)
fwrite(regime_stats, "output/cftc/cftc_regime_returns.csv")

# ── 6. Event table ────────────────────────────────────────────────────────────
extremes <- m[pos_pct >= 0.90 | pos_pct <= 0.10]
extremes[, signal := ifelse(pos_pct >= 0.90, "CROWDED_LONG", "CROWDED_SHORT")]
extremes_out <- extremes[, .(release_date, signal, net_pos, pos_pct=round(pos_pct,3),
                              pos_z=round(pos_z,2), price,
                              ret_1w=round(ret_1w*100,2), ret_2w=round(ret_2w*100,2),
                              ret_4w=round(ret_4w*100,2))]
fwrite(extremes_out, "output/cftc/cftc_extremes.csv")
cat(sprintf("\nExtreme events: %d long, %d short\n",
            sum(extremes$signal=="CROWDED_LONG"), sum(extremes$signal=="CROWDED_SHORT")))

# ── 7. Add macro factors (2021+) ──────────────────────────────────────────────
cat("\nLoading factors for multi-variable regression...\n")
fac <- fread("output/factors_extended.csv")
fac[, date := as.Date(date, origin="1970-01-01")]
# Match factors to CFTC release date: use same-week Friday
# factors are daily; take the Friday row (or last available that week)
fac[, week_fri := as.Date(cut(date, "week")) + 4L]  # Mon + 4 = Friday
avail_cols <- names(fac)
want_cols  <- c("dxy","dxy_4wk_chg","sofr","td3c_z52","bdi_z52",
                "gasoil_crack_dev","hdd_dev_5yr","cdd_us_ne","cftc_mm_net_chg",
                "crude_prod_chg","crude_net_exports_kbd",
                "sin_ann","cos_ann","driving_season","heating_season")
use_cols <- want_cols[want_cols %in% avail_cols]
fac_wk <- fac[, lapply(.SD, function(x) last(x[!is.na(x)])), by=week_fri,
               .SDcols=use_cols]
setnames(fac_wk, "week_fri", "release_date")

mf <- merge(m, fac_wk, by="release_date", all.x=TRUE)
cat(sprintf("  Factor-merged rows: %d (factor coverage from ~2021)\n",
            sum(!is.na(mf$td3c_z52))))

# ── 8. Regression: CFTC alone vs CFTC + macro ────────────────────────────────
cat("\n=== REGRESSION: CFTC vs Price Returns (4-week horizon) ===\n")

# z-score the macro vars for comparability
zsc <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
# z-score from raw CFTC data (available full history)
mf[, mm_chg_z := zsc(net_pos_chg)]

# Add macro z-scores using whichever columns merged in
add_z <- function(dt, col_in, col_out) {
  if (col_in %in% names(dt) && sum(!is.na(dt[[col_in]])) > 5) {
    dt[, (col_out) := zsc(get(col_in))]
  } else {
    dt[, (col_out) := NA_real_]
  }
}
add_z(mf, "dxy_4wk_chg", "dxy_z")
add_z(mf, "sofr",         "sofr_z")
add_z(mf, "td3c_z52",     "td3c_z")
add_z(mf, "bdi_z52",      "bdi_z")
add_z(mf, "gasoil_crack_dev", "crack_z")

# Report how many non-NA rows each macro var has
macro_vars <- c("dxy_z","sofr_z","td3c_z","bdi_z")
for (v in macro_vars) cat(sprintf("  %s non-NA rows: %d\n", v, sum(!is.na(mf[[v]]))))

# Model 1: CFTC level only (full history)
sub1 <- mf[!is.na(ret_4w) & !is.na(pos_z)]
m1 <- lm(ret_4w ~ pos_z, data=sub1)

# Model 2: CFTC level + weekly change (full history)
sub2 <- mf[!is.na(ret_4w) & !is.na(pos_z) & !is.na(mm_chg_z)]
m2 <- lm(ret_4w ~ pos_z + mm_chg_z, data=sub2)

# Model 3: CFTC + macro (2021+ only; use available macro vars)
macro_avail <- macro_vars[sapply(macro_vars, function(v) sum(!is.na(mf[[v]])) > 20)]
cat(sprintf("  Macro vars available for Model 3: %s\n", paste(macro_avail, collapse=", ")))
if (length(macro_avail) == 0) {
  cat("  No macro data -- Model 3 same as Model 1\n")
  m3 <- m1; m4 <- m1
} else {
  sub3 <- mf[!is.na(ret_4w) & !is.na(pos_z) &
             Reduce("&", lapply(macro_avail, function(v) !is.na(mf[[v]])))]
  cat(sprintf("  Model 3/4 training rows: %d\n", nrow(sub3)))
  fml3 <- as.formula(paste("ret_4w ~ pos_z +", paste(c(macro_avail,"sin_ann","cos_ann"), collapse=" + ")))
  fml4 <- as.formula(paste("ret_4w ~",          paste(c(macro_avail,"sin_ann","cos_ann"), collapse=" + ")))
  m3 <- lm(fml3, data=sub3)
  m4 <- lm(fml4, data=sub3)
}

print_model <- function(mod, name) {
  s <- summary(mod)
  cf <- coef(s)
  cat(sprintf("\n--- %s  |  R2=%.3f  |  n=%d ---\n", name, s$r.squared, nobs(mod)))
  for (i in seq_len(nrow(cf))) {
    stars <- ifelse(cf[i,4]<0.01,"***", ifelse(cf[i,4]<0.05,"**", ifelse(cf[i,4]<0.10,"*","")))
    cat(sprintf("  %-28s  %+.4f  (p=%.3f) %s\n", rownames(cf)[i], cf[i,1], cf[i,4], stars))
  }
}
print_model(m1, "Model 1: ret_4w ~ pos_z  [CFTC level only, full history]")
print_model(m2, "Model 2: ret_4w ~ pos_z + mm_chg_z  [CFTC level+change]")
print_model(m3, "Model 3: ret_4w ~ pos_z + macro  [CFTC + macro, 2021+]")
print_model(m4, "Model 4: ret_4w ~ macro only  [benchmark, 2021+]")

cat(sprintf("\nMarginal R2 from CFTC (M3 - M4): %.4f (= +%.1f%%)\n",
            summary(m3)$r.squared - summary(m4)$r.squared,
            (summary(m3)$r.squared - summary(m4)$r.squared)*100))

# Save regression table
reg_rows <- rbindlist(lapply(list(m1,m2,m3,m4), function(mod) {
  s <- summary(mod); cf <- coef(s)
  data.table(model=deparse(formula(mod)),
             r2=round(s$r.squared,4), n=nobs(mod),
             feature=rownames(cf), coef=round(cf[,1],5), pval=round(cf[,4],4))
}), fill=TRUE)
fwrite(reg_rows, "output/cftc/cftc_regression.csv")

# ── 9. Plots ──────────────────────────────────────────────────────────────────
cat("\nGenerating plots...\n")

# Plot 1: Net positions vs price (dual axis)
m_plot <- m[!is.na(price)]
png("output/cftc/positions_vs_price.png", width=1400, height=700, res=120)
par(mar=c(4,5,3,5))
plot(m_plot$release_date, m_plot$net_pos/1000, type="l", col="#3060A0", lwd=1.5,
     xlab="", ylab="Net Positions (000 contracts)", main="CFTC Managed Money Net Positions vs WTI Price",
     ylim=c(min(m_plot$net_pos/1000)*1.1, max(m_plot$net_pos/1000)*1.1))
abline(h=quantile(m_plot$net_pos/1000, 0.10), col="#D04040", lty=2, lwd=1)
abline(h=quantile(m_plot$net_pos/1000, 0.90), col="#40A040", lty=2, lwd=1)
abline(h=0, col="grey50", lty=3)
par(new=TRUE)
plot(m_plot$release_date, m_plot$price, type="l", col="#D08020", lwd=1.5,
     axes=FALSE, xlab="", ylab="")
axis(4, col.axis="#D08020"); mtext("WTI Price ($/bbl)", side=4, line=3, col="#D08020")
legend("topright", c("Net positions (L)","WTI price (R)","90th pct","10th pct"),
       col=c("#3060A0","#D08020","#40A040","#D04040"), lty=c(1,1,2,2), lwd=1.5, cex=0.8)
dev.off()

# Plot 2: Forward returns by regime (boxplot)
sub_box <- m[!is.na(ret_4w), .(regime, ret_1w=ret_1w*100, ret_2w=ret_2w*100, ret_4w=ret_4w*100)]
regime_order <- c("Extreme Short (<10th pct)","Short (10-40th)","Neutral (40-60th)",
                  "Long  (60-90th)","Extreme Long  (>90th pct)")
sub_box[, regime := factor(regime, levels=regime_order)]

png("output/cftc/forward_returns_by_regime.png", width=1200, height=700, res=120)
par(mar=c(7,5,3,2), mfrow=c(1,2))
# 1W returns
r_list <- lapply(regime_order, function(r) sub_box[regime==r, ret_1w])
bp1 <- boxplot(r_list, names=c("Ext.Short","Short","Neutral","Long","Ext.Long"),
               col=c("#D04040","#E08080","#A0A0A0","#80C080","#40A040"),
               main="1-Week Return by CFTC Regime (%)", ylab="Return (%)",
               las=2, cex.axis=0.8)
abline(h=0, lty=2, col="grey50")
# 4W returns
r_list4 <- lapply(regime_order, function(r) sub_box[regime==r, ret_4w])
bp4 <- boxplot(r_list4, names=c("Ext.Short","Short","Neutral","Long","Ext.Long"),
               col=c("#D04040","#E08080","#A0A0A0","#80C080","#40A040"),
               main="4-Week Return by CFTC Regime (%)", ylab="Return (%)",
               las=2, cex.axis=0.8)
abline(h=0, lty=2, col="grey50")
dev.off()

# Plot 3: Regression coefficients (model 3)
cf3 <- as.data.frame(coef(summary(m3)))
cf3 <- cf3[rownames(cf3) != "(Intercept)",]
cf3$feature <- rownames(cf3)
cf3 <- cf3[order(abs(cf3$Estimate), decreasing=TRUE),]

png("output/cftc/regression_coefficients.png", width=900, height=600, res=120)
par(mar=c(5,12,3,2))
cols <- ifelse(cf3$Estimate > 0, "#40A040", "#D04040")
barplot(cf3$Estimate, names.arg=cf3$feature, horiz=TRUE, las=2, col=cols,
        main="Regression Coefficients: 4-week WTI Return\n(CFTC + Macro, 2021+)",
        xlab="Coefficient (return %)", cex.names=0.8)
abline(v=0, lwd=1.5)
dev.off()

cat("\n=== DONE ===\n")
cat("Outputs saved to output/cftc/\n")
cat("  cftc_wti_merged.csv\n")
cat("  cftc_extremes.csv\n")
cat("  cftc_regime_returns.csv\n")
cat("  cftc_regression.csv\n")
cat("  positions_vs_price.png\n")
cat("  forward_returns_by_regime.png\n")
cat("  regression_coefficients.png\n")
