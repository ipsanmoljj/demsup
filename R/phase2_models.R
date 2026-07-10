# R/phase2_models.R
# ─────────────────────────────────────────────────────────────────────────────
# CFTC Phase 2 — Expiry Week Spread Modelling and Diagnostics Pipeline
#
# TASKS:
#   1. Fit Stage 1 OLS: roll_compression ~ pos_z_full (Full, High-Vol, Low-Vol)
#   2. Fit Stage 2 OLS: roll_reversion ~ pos_z_full + roll_compression (Full, High-Vol, Low-Vol)
#   3. Fit Interaction models:
#      - Stage 1: roll_compression ~ pos_z_full * is_hv
#      - Stage 2: roll_reversion ~ pos_z_full * is_hv + roll_compression
#   4. Fit Logit models: glm(I(roll_compression < 0) ~ pos_z_full, family=binomial)
#   5. Compute Newey-West HAC standard errors (lag=2, prewhite=FALSE, adjust=FALSE) for OLS
#      and standard GLM standard errors for Logit.
#   6. Run OLS diagnostics (normality, heteroskedasticity, autocorrelation, VIF)
#   7. Detect outliers and refit clean models to verify coefficient stability
#   8. Save outputs (CSVs + diagnostic plots) to output/cftc_phase2/
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table)
  library(lmtest)
  library(sandwich)
  library(nortest)
  library(car)
})

setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
dir.create("output/cftc_phase2", showWarnings=FALSE, recursive=TRUE)

cat("=== CFTC Phase 2 Modeling & Diagnostics Pipeline ===\n")
cat("Started:", format(Sys.time()), "\n\n")

# ── Load and prepare data ──────────────────────────────────────────────────────
df <- fread("output/cftc_phase2/phase2_dataset.csv")

# Create binary volatility regime indicator (1 for High-Vol, 0 for Low-Vol)
df[, is_hv := as.numeric(markov_state == 1)]

# Print basic stats
n_total <- nrow(df)
n_spread <- sum(!is.na(df$roll_compression))
cat(sprintf("Total expiry events: %d\nEvents with spread data: %d\n", n_total, n_spread))
cat(sprintf("High-Vol (Markov=1) events with spread: %d\n", sum(df$markov_state == 1 & !is.na(df$roll_compression))))
cat(sprintf("Low-Vol (Markov=2) events with spread: %d\n\n", sum(df$markov_state == 2 & !is.na(df$roll_compression))))

# ═══════════════════════════════════════════════════════════════════════════════
# PART 1: FIT MODELS & REPLICATE RESULTS
# ═══════════════════════════════════════════════════════════════════════════════
cat("--- Fitting models and replicating CSV results ---\n")

# Helper to run coeftest with matching Newey-West settings
run_nw_test <- function(m) {
  coeftest(m, vcov = NeweyWest(m, lag = 2, prewhite = FALSE, adjust = FALSE))
}

# 1A. Stage 1 (roll_compression ~ pos_z_full)
m1_full <- lm(roll_compression ~ pos_z_full, data = df)
m1_hv   <- lm(roll_compression ~ pos_z_full, data = df[markov_state == 1])
m1_lv   <- lm(roll_compression ~ pos_z_full, data = df[markov_state == 2])

nw1_full <- run_nw_test(m1_full)
nw1_hv   <- run_nw_test(m1_hv)
nw1_lv   <- run_nw_test(m1_lv)

# 1B. Stage 2 (roll_reversion ~ pos_z_full + roll_compression)
m2_full <- lm(roll_reversion ~ pos_z_full + roll_compression, data = df)
m2_hv   <- lm(roll_reversion ~ pos_z_full + roll_compression, data = df[markov_state == 1])
m2_lv   <- lm(roll_reversion ~ pos_z_full + roll_compression, data = df[markov_state == 2])

nw2_full <- run_nw_test(m2_full)
nw2_hv   <- run_nw_test(m2_hv)
nw2_lv   <- run_nw_test(m2_lv)

# 1C. Interaction models
m1_int <- lm(roll_compression ~ pos_z_full * is_hv, data = df)
nw1_int <- run_nw_test(m1_int)

m2_int <- lm(roll_reversion ~ pos_z_full * is_hv + roll_compression, data = df)
nw2_int <- run_nw_test(m2_int)

# 1D. Logit models (predicting spread compression occurrence, using standard binomial GLM)
logit_full <- glm(I(roll_compression < 0) ~ pos_z_full, family = binomial, data = df)
logit_hv   <- glm(I(roll_compression < 0) ~ pos_z_full, family = binomial, data = df[markov_state == 1])
logit_lv   <- glm(I(roll_compression < 0) ~ pos_z_full, family = binomial, data = df[markov_state == 2])

# Standard Wald test p-values (no NW correction for GLM, matching the CSV)
nw_logit_full <- coeftest(logit_full)
nw_logit_hv   <- coeftest(logit_hv)
nw_logit_lv   <- coeftest(logit_lv)

# ── Save model_results.csv ────────────────────────────────────────────────────
model_res <- data.table(
  stage = c(1, 1, 1, 2, 2, 2),
  group = c("Full", "High-Vol", "Low-Vol", "Full", "High-Vol", "Low-Vol"),
  coef  = rep("pos_z_full", 6),
  b     = c(coef(m1_full)["pos_z_full"], coef(m1_hv)["pos_z_full"], coef(m1_lv)["pos_z_full"],
            coef(m2_full)["pos_z_full"], coef(m2_hv)["pos_z_full"], coef(m2_lv)["pos_z_full"]),
  p     = c(nw1_full["pos_z_full", 4], nw1_hv["pos_z_full", 4], nw1_lv["pos_z_full", 4],
            nw2_full["pos_z_full", 4], nw2_hv["pos_z_full", 4], nw2_lv["pos_z_full", 4]),
  r2    = c(summary(m1_full)$r.squared, summary(m1_hv)$r.squared, summary(m1_lv)$r.squared,
            summary(m2_full)$r.squared, summary(m2_hv)$r.squared, summary(m2_lv)$r.squared)
)
fwrite(model_res, "output/cftc_phase2/model_results.csv")
cat("Saved: output/cftc_phase2/model_results.csv\n")

# ── Save model_results_full.csv ───────────────────────────────────────────────
model_res_full <- data.table(
  test   = c("S1_Full", "S1_HV", "S1_LV", "S2_Full", "S2_HV", "S2_LV",
             "Int_S1_LVbase", "Int_S1_HVdiff", "Int_S2_LVbase", "Int_S2_HVdiff",
             "Logit_Full", "Logit_HV", "Logit_LV"),
  beta   = c(coef(m1_full)["pos_z_full"], coef(m1_hv)["pos_z_full"], coef(m1_lv)["pos_z_full"],
             coef(m2_full)["pos_z_full"], coef(m2_hv)["pos_z_full"], coef(m2_lv)["pos_z_full"],
             coef(m1_int)["pos_z_full"], coef(m1_int)["pos_z_full:is_hv"],
             coef(m2_int)["pos_z_full"], coef(m2_int)["pos_z_full:is_hv"],
             coef(logit_full)["pos_z_full"], coef(logit_hv)["pos_z_full"], coef(logit_lv)["pos_z_full"]),
  p_nw   = c(nw1_full["pos_z_full", 4], nw1_hv["pos_z_full", 4], nw1_lv["pos_z_full", 4],
             nw2_full["pos_z_full", 4], nw2_hv["pos_z_full", 4], nw2_lv["pos_z_full", 4],
             nw1_int["pos_z_full", 4], nw1_int["pos_z_full:is_hv", 4],
             nw2_int["pos_z_full", 4], nw2_int["pos_z_full:is_hv", 4],
             nw_logit_full["pos_z_full", 4], nw_logit_hv["pos_z_full", 4], nw_logit_lv["pos_z_full", 4])
)
fwrite(model_res_full, "output/cftc_phase2/model_results_full.csv")
cat("Saved: output/cftc_phase2/model_results_full.csv\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# PART 2: OLS DIAGNOSTIC TESTS
# ═══════════════════════════════════════════════════════════════════════════════
cat("--- Running OLS Diagnostics ---\n")

run_diagnostics <- function(mod, label) {
  cat(sprintf("\n[Diagnostics: %s]\n", label))
  res <- residuals(mod)
  
  # Shapiro-Wilk & Anderson-Darling normality tests
  sw <- shapiro.test(res)
  ad <- ad.test(res)
  cat(sprintf("  Normality: SW W=%.4f (p=%.4f) | AD A=%.4f (p=%.4f) | %s\n",
              sw$statistic, sw$p.value, ad$statistic, ad$p.value,
              ifelse(ad$p.value < 0.05, "NON-NORMAL residuals", "PASS normality")))
              
  # Breusch-Pagan homoskedasticity test
  bp <- bptest(mod)
  cat(sprintf("  Heteroskedasticity: BP LM=%.4f (p=%.4f) | %s\n",
              bp$statistic, bp$p.value,
              ifelse(bp$p.value < 0.05, "HETEROSKEDASTIC residuals", "PASS homoskedasticity")))
              
  # Durbin-Watson & Ljung-Box autocorrelation tests
  dw <- dwtest(mod, alternative="two.sided")
  lb4 <- Box.test(res, lag=4, type="Ljung-Box")
  cat(sprintf("  Autocorrelation: DW=%.4f (p=%.4f) | Ljung-Box(4) Q=%.3f (p=%.4f) | %s\n",
              dw$statistic, dw$p.value, lb4$statistic, lb4$p.value,
              ifelse(dw$p.value < 0.05 || lb4$p.value < 0.05, "AUTOCORRELATED residuals", "PASS autocorrelation")))
              
  # Multicollinearity (VIF)
  p <- length(coef(mod))
  if (p > 2) {
    vf <- vif(mod)
    cat(sprintf("  Multicollinearity: Max VIF = %.2f (%s)\n", max(vf), names(which.max(vf))))
  }
}

run_diagnostics(m1_full, "Stage 1 Full OLS")
run_diagnostics(m2_full, "Stage 2 Full OLS")

# ── Generate Diagnostic Plots ─────────────────────────────────────────────────
diag_plots <- function(mod, label, fname) {
  res  <- residuals(mod)
  fit  <- fitted(mod)
  sres <- rstudent(mod)
  
  png(fname, width=1400, height=1100, res=130)
  par(mfrow=c(2,2), mar=c(4,4,3,2), oma=c(0,0,3,0))
  
  # Residuals vs Fitted
  plot(fit, res, pch=16, cex=0.6, col=rgb(0.2,0.4,0.8,0.5),
       xlab="Fitted values", ylab="Residuals", main="Residuals vs Fitted")
  abline(h=0, col="red", lwd=1.5, lty=2)
  lines(lowess(fit, res), col="darkred", lwd=2)
  
  # Normal QQ
  qqnorm(sres, pch=16, cex=0.6, col=rgb(0.2,0.4,0.8,0.5),
         main="Normal Q-Q (Studentised Residuals)")
  qqline(sres, col="red", lwd=2)
  
  # Scale-Location
  plot(fit, sqrt(abs(sres)), pch=16, cex=0.6, col=rgb(0.2,0.6,0.3,0.5),
       xlab="Fitted values", ylab=expression(sqrt("|Studentised residuals|")),
       main="Scale-Location (Homoskedasticity)")
  lines(lowess(fit, sqrt(abs(sres))), col="darkgreen", lwd=2)
  abline(h=1, col="red", lty=2)
  
  # ACF of residuals
  acf(res, lag.max=15, main="ACF of Residuals (Autocorrelation)",
      col=rgb(0.2,0.4,0.8,0.7), lwd=2)
  
  mtext(paste("OLS Diagnostics:", label), outer=TRUE, cex=1.1, font=2)
  dev.off()
}

diag_plots(m1_full, "Stage 1 Full OLS", "output/cftc_phase2/diag_stage1_before.png")
diag_plots(m2_full, "Stage 2 Full OLS", "output/cftc_phase2/diag_stage2_before.png")
cat("\nSaved OLS diagnostics plots.\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# PART 3: OUTLIER DETECTION & CLEANED MODELS REFIT
# ═══════════════════════════════════════════════════════════════════════════════
cat("--- Running Outlier Detection ---\n")

detect_outliers <- function(mod, data, label) {
  n <- nobs(mod)
  p <- length(coef(mod))
  cook <- cooks.distance(mod)
  hat <- hatvalues(mod)
  sres <- rstudent(mod)
  
  # Threshold definitions
  cook_thr <- 4 / n
  hat_thr  <- 2 * (p + 1) / n
  sres_thr <- 2.5
  
  flag_cook <- cook > cook_thr
  flag_hat  <- hat > hat_thr
  flag_sres <- abs(sres) > sres_thr
  
  n_flags <- as.integer(flag_cook) + as.integer(flag_hat) + as.integer(flag_sres)
  extreme <- which(n_flags >= 2)
  
  cat(sprintf("[Outliers: %s] n=%d. Cook>%.4f, Leverage>%.4f, |Rstud|>2.5\n",
              label, n, cook_thr, hat_thr, sres_thr))
  cat(sprintf("  Cook's D flagged: %d | Leverage: %d | Residuals: %d\n",
              sum(flag_cook), sum(flag_hat), sum(flag_sres)))
  cat(sprintf("  Multi-flagged (2+ criteria): %d observations\n", length(extreme)))
  
  if (length(extreme)) {
    out_dt <- data[extreme, .(delivery_month, expiry_date, pos_z_full, roll_compression, roll_reversion)]
    out_dt[, model := label]
    out_dt[, cook_d := round(cook[extreme], 4)]
    out_dt[, leverage := round(hat[extreme], 4)]
    out_dt[, stud_res := round(sres[extreme], 3)]
    out_dt[, flags := n_flags[extreme]]
    out_dt[, reason := paste0(
      ifelse(flag_cook[extreme], "Cook ", ""),
      ifelse(flag_hat[extreme], "Lev ", ""),
      ifelse(flag_sres[extreme], "Stud", "")
    )]
    print(as.data.frame(out_dt))
    return(list(indices = extreme, table = out_dt))
  }
  return(list(indices = integer(0), table = data.table()))
}

out1 <- detect_outliers(m1_full, df[!is.na(roll_compression)], "Stage 1 Full OLS")
out2 <- detect_outliers(m2_full, df[!is.na(roll_reversion)], "Stage 2 Full OLS")

# Combine and save outlier table
outliers_all <- rbindlist(list(out1$table, out2$table), fill=TRUE)
fwrite(outliers_all, "output/cftc_phase2/expiry_outliers.csv")
cat("Saved: output/cftc_phase2/expiry_outliers.csv\n\n")

# Refit after outlier removal (multi-flagged 2+)
df_s1_clean <- df[!is.na(roll_compression)]
if (length(out1$indices)) df_s1_clean <- df_s1_clean[-out1$indices]

df_s2_clean <- df[!is.na(roll_reversion)]
if (length(out2$indices)) df_s2_clean <- df_s2_clean[-out2$indices]

m1_full_c <- lm(roll_compression ~ pos_z_full, data = df_s1_clean)
m2_full_c <- lm(roll_reversion ~ pos_z_full + roll_compression, data = df_s2_clean)

nw1_full_c <- run_nw_test(m1_full_c)
nw2_full_c <- run_nw_test(m2_full_c)

# Save Clean diagnostic plots
diag_plots(m1_full_c, "Stage 1 Clean OLS", "output/cftc_phase2/diag_stage1_after.png")
diag_plots(m2_full_c, "Stage 2 Clean OLS", "output/cftc_phase2/diag_stage2_after.png")
cat("Saved Clean OLS diagnostics plots.\n\n")

# ── Save OLS vs Clean Model Comparison ────────────────────────────────────────
compare_coef <- function(m_before, nw_before, m_after, nw_after, model_name) {
  cfb <- coef(summary(m_before))
  cfa <- coef(summary(m_after))
  all_rows <- union(rownames(cfb), rownames(cfa))
  dt <- data.table(model = model_name, variable = all_rows)
  dt[, coef_before := cfb[variable, 1]]
  dt[, pval_before_ols := cfb[variable, 4]]
  dt[, pval_before_nw  := nw_before[variable, 4]]
  dt[, coef_after := cfa[variable, 1]]
  dt[, pval_after_ols := cfa[variable, 4]]
  dt[, pval_after_nw  := nw_after[variable, 4]]
  dt[, coef_change := coef_after - coef_before]
  dt
}

comp_tbl <- rbindlist(list(
  compare_coef(m1_full, nw1_full, m1_full_c, nw1_full_c, "Stage 1 (roll_compression)"),
  compare_coef(m2_full, nw2_full, m2_full_c, nw2_full_c, "Stage 2 (roll_reversion)")
))

fwrite(comp_tbl, "output/cftc_phase2/model_comparison.csv")
cat("Saved: output/cftc_phase2/model_comparison.csv\n")

cat("\n=== Pipeline Execution Completed successfully ===\n")
