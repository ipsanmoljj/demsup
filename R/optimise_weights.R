# R/optimise_weights.R
# Derive regime × instrument weights via regularised regression (glmnet).
#
# Framing:
#   Response  y_t = sig_t * ret_20d_t  (signed benchmark return for day t)
#   Predictors X_t = [pnl_inst1, ..., pnl_instK]  (each instrument's daily P&L)
#   Constraint: lower.limits = 0  (non-negative weights only)
#   CV: grouped folds by year  (respects time-series structure)
#
# Runs for CL (7 instruments, CL regimes) and LCO (6 instruments, LCO regimes).
# Saves weight tables to strategy_live/final data/phase3c/:
#   cl_weights_enet.csv   (alpha=0.5 elastic net, + ridge + lasso variants)
#   lco_weights_enet.csv

suppressPackageStartupMessages({
  library(data.table); library(zoo); library(glmnet)
})

REPO  <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup"
OUTD  <- file.path(REPO, "strategy_live/final data/phase2")
TENT  <- file.path(REPO, "strategy_live/final data/tent_data")
P3LCO <- file.path(REPO, "strategy_live/final data/phase3_lco")
SAVE  <- file.path(REPO, "strategy_live/final data/phase3c")
dir.create(SAVE, showWarnings = FALSE)

# ── Load & merge data ─────────────────────────────────────────────────────────
oos     <- fread(file.path(OUTD, "oos_signals_v2.csv"))
curve   <- fread(file.path(TENT, "cl_curve_daily.csv"))
lco     <- fread(file.path(TENT, "lco_curve_daily.csv"))
lco_reg <- fread(file.path(REPO, "output/LCO/regime_labels_LCO.csv"))

for (dt in list(oos, curve, lco, lco_reg)) dt[, date := as.Date(date)]
setorder(oos, date)

oos <- merge(oos, curve, by = "date", all.x = TRUE)
oos <- merge(oos, lco_reg[, .(date, lco_regime = regime_label)], by = "date", all.x = TRUE)

lco_spread_cols <- c("m1m2","m1m3","m1m6","m1m12","m2_fly","m3_fly")
setnames(lco, lco_spread_cols, paste0("lco_", c("m1m2","m1m3","m1m6","m1m12","m2fly","m3fly")))
oos <- merge(oos, lco[, .(date, lco_m1 = m1, lco_m1m2, lco_m1m3,
                           lco_m1m6, lco_m1m12, lco_m2fly, lco_m3fly)],
             by = "date", all.x = TRUE)

oos[, wti_spot := as.numeric(wti_spot)]
oos[, m1_px    := zoo::na.locf(wti_spot,  na.rm = FALSE)]
oos[, lco_m1   := zoo::na.locf(lco_m1,   na.rm = FALSE)]
oos[, sig      := sig_ens]
oos[, year     := as.integer(format(date, "%Y"))]

# ── Forward returns ───────────────────────────────────────────────────────────
spread_ret <- function(spread, m1, h = 20)
  (shift(spread, -h, type = "lead") - spread) / pmax(m1, 10)

oos[, ret_out   := ret_20d]
oos[, ret_m1m2  := spread_ret(m1m2,   m1_px)]
oos[, ret_m1m3  := spread_ret(m1m3,   m1_px)]
oos[, ret_m1m6  := spread_ret(m1m6,   m1_px)]
oos[, ret_m1m12 := spread_ret(m1m12,  m1_px)]
oos[, ret_m2fly := spread_ret(m2_fly, m1_px)]
oos[, ret_m3fly := spread_ret(m3_fly, m1_px)]

oos[, ret_lco_m1m2  := spread_ret(lco_m1m2,  lco_m1)]
oos[, ret_lco_m1m3  := spread_ret(lco_m1m3,  lco_m1)]
oos[, ret_lco_m1m6  := spread_ret(lco_m1m6,  lco_m1)]
oos[, ret_lco_m1m12 := spread_ret(lco_m1m12, lco_m1)]
oos[, ret_lco_m2fly := spread_ret(lco_m2fly, lco_m1)]
oos[, ret_lco_m3fly := spread_ret(lco_m3fly, lco_m1)]

# ── Positions ─────────────────────────────────────────────────────────────────
REG_THR <- list("Deep-Backwardation"=0.04,"Easing-Backwardation"=0.04,
                "Stable-Depressed"=0.04,"default"=0.10)
LCO_THR <- c(REG_THR, list("Stable-Elevated"=0.04))
get_thr  <- function(r,thr) { v <- thr[[r]]; if (is.null(v)) thr$default else v }

oos[, cl_thr  := sapply(cl_regime,  get_thr, thr = REG_THR)]
oos[, lco_thr := sapply(fifelse(is.na(lco_regime),"default",lco_regime),
                        get_thr, thr = LCO_THR)]

M2FLY_FLIP <- c("Backwardation-Deficit"=TRUE,"Deep-Backwardation"=FALSE,
                 "Easing-Backwardation"=FALSE,"Contango-Surplus"=TRUE,
                 "Deep-Contango"=TRUE,"Easing-Contango"=TRUE,
                 "Stable-Depressed"=FALSE,"Stable-Elevated"=TRUE,
                 "Transition-Tightening"=FALSE,"Transition-Loosening"=TRUE)

make_pos <- function(sig, thr, flip=FALSE) {
  r <- fifelse(sig > thr, 1L, fifelse(sig < -thr, -1L, 0L))
  if (flip) r * -1L else r
}

oos[, raw_cl  := fifelse(sig > cl_thr,  1L, fifelse(sig < -cl_thr,  -1L, 0L))]
oos[, raw_lco := fifelse(sig > lco_thr, 1L, fifelse(sig < -lco_thr, -1L, 0L))]

# CL positions
oos[, pos_out   := raw_cl]
oos[, pos_m1m2  := raw_cl]
oos[, pos_m1m3  := raw_cl]
oos[, pos_m1m6  := raw_cl]
oos[, pos_m1m12 := raw_cl]
oos[, pos_m3fly := raw_cl * -1L]
oos[, cl_m2flip := M2FLY_FLIP[cl_regime]]
oos[is.na(cl_m2flip), cl_m2flip := TRUE]
oos[, pos_m2fly := fifelse(cl_m2flip, raw_cl * -1L, raw_cl)]

# LCO positions
oos[, pos_lco_m1m2  := raw_lco]
oos[, pos_lco_m1m3  := raw_lco]
oos[, pos_lco_m1m6  := raw_lco]
oos[, pos_lco_m1m12 := raw_lco]
oos[, pos_lco_m3fly := raw_lco * -1L]
oos[, lco_m2flip := M2FLY_FLIP[fifelse(is.na(lco_regime),"Stable-Elevated",lco_regime)]]
oos[is.na(lco_m2flip), lco_m2flip := TRUE]
oos[, pos_lco_m2fly := fifelse(lco_m2flip, raw_lco * -1L, raw_lco)]

# ── Regularised weight optimisation ──────────────────────────────────────────
# For each regime: regress signed benchmark return on instrument P&Ls
# y = sig * ret_20d  |  X = [pos_i * ret_i]  |  lower.limits = 0
#
# Three models: ridge (alpha=0), lasso (alpha=1), enet (alpha=0.5)
# CV grouped by year to respect time ordering.

# Ridge-regularised mean-variance optimisation:
#   w* = (Σ + λI)^{-1} μ   (μ = mean P&L per instrument, Σ = covariance)
# λ chosen by leave-one-year-out CV maximising out-of-sample Sharpe.
# Non-negative constraint applied after solving; weights normalised to [0,1].

mvo_sharpe <- function(X, lambda) {
  mu  <- colMeans(X)
  Sig <- cov(X) + lambda * diag(ncol(X))
  w   <- tryCatch(solve(Sig, mu), error = function(e) rep(NA_real_, ncol(X)))
  w
}

cv_lambda <- function(X, years, lambdas = 10^seq(-4, 1, length.out = 30)) {
  uniq_y <- sort(unique(years))
  if (length(uniq_y) < 2) return(lambdas[ceiling(length(lambdas)/2)])
  sharpes <- sapply(lambdas, function(lam) {
    sh <- sapply(uniq_y, function(yr) {
      train <- X[years != yr, , drop=FALSE]
      test  <- X[years == yr, , drop=FALSE]
      if (nrow(train) < 5 || nrow(test) < 2) return(NA_real_)
      w <- mvo_sharpe(train, lam)
      if (any(is.na(w))) return(NA_real_)
      pnl <- test %*% pmax(w, 0)
      if (sd(pnl) == 0) return(NA_real_)
      mean(pnl) / sd(pnl) * sqrt(252)
    })
    mean(sh, na.rm = TRUE)
  })
  lambdas[which.max(sharpes)]
}

fit_weights <- function(sub, pos_cols, ret_cols, inst_names, min_obs = 15) {
  pnl_list <- mapply(function(p, r) sub[[p]] * sub[[r]], pos_cols, ret_cols,
                     SIMPLIFY = FALSE)
  X <- do.call(cbind, pnl_list)
  colnames(X) <- inst_names

  ok <- complete.cases(X) & (rowSums(abs(X)) > 0)
  X  <- X[ok, , drop = FALSE]
  n  <- nrow(X)
  if (n < min_obs || ncol(X) == 0) return(NULL)

  yrs  <- sub$year[ok]
  lam  <- cv_lambda(X, yrs)

  w_ridge <- pmax(mvo_sharpe(X, lam), 0)
  # Lasso-like sparsity: zero out weights below 10% of max
  w_enet  <- ifelse(w_ridge < 0.1 * max(w_ridge, na.rm=TRUE), 0, w_ridge)

  norm_w <- function(v) { mx <- max(v, na.rm=TRUE); if (mx>0) round(v/mx,3) else v }

  list(
    n_obs  = n,
    lambda = round(lam, 5),
    ridge  = norm_w(w_ridge),
    enet   = norm_w(w_enet)
  )
}

build_weight_table <- function(dt, regime_col, pos_cols, ret_cols, inst_names) {
  regimes <- sort(unique(dt[[regime_col]]))
  regimes <- regimes[!is.na(regimes) & regimes != "Warm-Up"]
  rows <- list()
  for (reg in regimes) {
    sub <- dt[get(regime_col) == reg & !is.na(get(regime_col))]
    cat(sprintf("  %-28s  n=%d\n", reg, nrow(sub)))
    res <- fit_weights(sub, pos_cols, ret_cols, inst_names)
    if (is.null(res)) { cat("    → skipped\n"); next }
    r <- data.table(regime = reg, n_obs = res$n_obs, lambda = res$lambda)
    for (nm in c("ridge","enet")) {
      v <- res[[nm]]
      for (i in seq_along(inst_names))
        r[, paste0("w_", nm, "_", inst_names[i]) := v[i]]
    }
    rows[[length(rows)+1]] <- r
  }
  rbindlist(rows, fill = TRUE)
}

# ── CL ────────────────────────────────────────────────────────────────────────
cat("\n=== CL weight optimisation ===\n")
cl_pos  <- c("pos_out","pos_m1m2","pos_m1m3","pos_m1m6","pos_m1m12","pos_m2fly","pos_m3fly")
cl_ret  <- c("ret_out","ret_m1m2","ret_m1m3","ret_m1m6","ret_m1m12","ret_m2fly","ret_m3fly")
cl_inst <- c("out","m1m2","m1m3","m1m6","m1m12","m2fly","m3fly")

cl_wt <- build_weight_table(
  oos[!is.na(cl_regime) & cl_regime != "Warm-Up" & !is.na(ret_out)],
  "cl_regime", cl_pos, cl_ret, cl_inst
)

cat("\nCL MVO weights (ridge | enet-sparse):\n")
ridge_cols <- grep("^w_ridge_", names(cl_wt), value=TRUE)
enet_cols  <- grep("^w_enet_",  names(cl_wt), value=TRUE)
print(cl_wt[, c("regime","n_obs","lambda",enet_cols), with=FALSE])

fwrite(cl_wt, file.path(SAVE, "cl_weights_enet.csv"))
cat("Saved cl_weights_enet.csv\n")

# ── LCO ───────────────────────────────────────────────────────────────────────
cat("\n=== LCO weight optimisation ===\n")
lco_pos  <- c("pos_lco_m1m2","pos_lco_m1m3","pos_lco_m1m6",
              "pos_lco_m1m12","pos_lco_m2fly","pos_lco_m3fly")
lco_ret  <- c("ret_lco_m1m2","ret_lco_m1m3","ret_lco_m1m6",
              "ret_lco_m1m12","ret_lco_m2fly","ret_lco_m3fly")
lco_inst <- c("m1m2","m1m3","m1m6","m1m12","m2fly","m3fly")

lco_wt <- build_weight_table(
  oos[!is.na(lco_regime) & lco_regime != "Warm-Up" & !is.na(ret_out)],
  "lco_regime", lco_pos, lco_ret, lco_inst
)

cat("\nLCO MVO weights (enet-sparse):\n")
enet_cols_lco <- grep("^w_enet_", names(lco_wt), value=TRUE)
print(lco_wt[, c("regime","n_obs","lambda",enet_cols_lco), with=FALSE])

fwrite(lco_wt, file.path(SAVE, "lco_weights_enet.csv"))
cat("Saved lco_weights_enet.csv\n")

cat("\n=== DONE ===\n")
cat("Ridge (MVO) + sparse-enet weights saved per regime × instrument.\n")
cat("Use w_enet_* columns in phase3c_strategy.R (ridge fallback for zero rows).\n")
