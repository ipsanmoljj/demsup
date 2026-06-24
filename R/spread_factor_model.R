# R/spread_factor_model.R
# ─────────────────────────────────────────────────────────────────────────────
# TRAIN / TEST SPLIT DESIGN
#   Events are sorted by date per product. The first TRAIN_FRAC fraction of
#   events (default 70%) becomes the training set; the remaining 30% is the
#   held-out test set. This gives ~80 test events spread across multiple
#   regimes — far more robust than a single calendar-window OOS.
#
#   All model tiers (old and new) are fit on the SAME training partition and
#   evaluated on the SAME test partition, so the comparison is apples-to-apples.
#
# TIER FAMILIES
#   OLD model tiers  (replicate inventory_shock_model.R):
#     old_t1_baseline   — surprise_z only
#     old_t2_physical   — EIA physical components + Cushing interaction
#     old_t2_freight    — EIA + freight (TD3C) + interactions
#     old_t3_full       — all 25 variables from original model
#
#   NEW model tiers  (structural factors as standalone regressors):
#     sfm_t1_eia_only       — surprise_z only (same as old_t1, for parity)
#     sfm_t2_eia_full       — all 6 EIA product components
#     sfm_t3_structural     — structural factors, NO EIA (control test)
#     sfm_t4_combined       — full model: structural + EIA + interactions (~32 vars)
#
# OUTPUTS
#   output/sfm_results.csv      — in-sample metrics + coefficients per model
#   output/sfm_test.csv         — event-level test-set predictions
#   output/sfm_report.csv       — test-set summary (hit rate, RMSE, MAE)
#   output/sfm_comparison.csv   — old vs new side-by-side per spread
#   output/sfm_models.rds       — model objects
# ─────────────────────────────────────────────────────────────────────────────


# ── Packages ──────────────────────────────────────────────────────────────────
.ensure_sfm <- function() {
  req <- c("data.table", "lubridate", "zoo", "glmnet")
  mis <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
  if (length(mis)) install.packages(mis, repos = "https://cloud.r-project.org", quiet = TRUE)
}
.ensure_sfm()
suppressPackageStartupMessages({
  library(data.table); library(lubridate); library(zoo); library(glmnet)
})


# ── Constants ─────────────────────────────────────────────────────────────────
SFM_TRAIN_FRAC     <- 0.70      # 70% train, 30% test
SFM_EVENT_WINDOW   <- 2L        # days after EIA release to measure spread change
SFM_MIN_OBS        <- 15L       # minimum observations to fit any model
SFM_MIN_SURPRISE_Z <- 0.4       # filter to significant EIA surprises only
SFM_OLS_MIN_N      <- 30L       # minimum n for OLS (otherwise ridge/lasso)
SFM_OLS_DOF_RATIO  <- 10L       # n / p must exceed this for OLS
SFM_SIG_THRESH     <- 0.5
SFM_TRIM_QUANTILE  <- 0.05
SFM_UNIT_CONV      <- list(CL = 1.0, LCO = 1.0, HO = 42.0, LGO = 1 / 7.45)
SFM_PRODUCTS       <- c("CL", "LCO", "HO", "LGO")


# ── All tier definitions — OLD and NEW in one table ───────────────────────────
SFM_TIERS <- list(

  # ── OLD MODEL TIERS (replicate inventory_shock_model.R) ──────────────────

  old_t1_baseline = c(
    "surprise_z"
  ),

  old_t2_physical = c(
    "surprise_z", "cushing_stocks_chg_z", "refinery_util_dev",
    "crude_prod_chg_z", "crude_net_exports_z",
    "sx_cushing", "sx_util"
  ),

  old_t2_freight = c(
    "surprise_z", "cushing_stocks_chg_z", "refinery_util_dev",
    "crude_net_exports_z", "td3c_z52", "td3c_wow_ws_z",
    "sx_cushing", "sx_td3c"
  ),

  old_t3_full = c(
    "surprise_z", "cushing_stocks_chg_z", "refinery_util_dev",
    "crude_prod_chg_z", "crude_net_exports_z",
    "gasoline_stocks_chg_z", "distillate_stocks_chg_z",
    "rig_chg_wow_z", "td3c_z52", "td3c_wow_ws_z", "td3c_storage_cost_z",
    "bdi_z52", "hdd_dev_5yr_z", "cdd_us_ne", "cftc_mm_net_chg_z",
    "driving_season", "heating_season", "turnaround_season",
    "sin_ann", "cos_ann",
    "sx_cushing", "sx_util", "sx_td3c", "sx_cftc", "sx_5yr_dev"
  ),

  # ── NEW MODEL TIERS (structural factors as standalone regressors) ─────────

  # Exact same as old_t1 — included to confirm parity
  sfm_t1_eia_only = c(
    "surprise_z"
  ),

  # All 6 EIA product components in one regression (no structural controls)
  sfm_t2_eia_full = c(
    "surprise_z", "cushing_stocks_chg_z",
    "gasoline_stocks_chg_z", "distillate_stocks_chg_z",
    "crude_prod_chg_z", "crude_net_exports_z"
  ),

  # Structural factors ONLY — no EIA. Shows what macro/freight/positioning
  # explains on a Wednesday independently of the inventory print.
  sfm_t3_structural = c(
    "crude_stocks_5yr_dev_z", "cushing_stocks_5yr_dev_z",
    "cftc_net_mm_zscore", "cftc_mm_net_chg_z",
    "td3c_z52", "td3c_wow_ws_z", "bdi_z52",
    "dxy_z", "dxy_4wk_chg_z", "sofr_z",
    "opec_prod_z", "rig_chg_wow_z", "refinery_util_dev",
    "hdd_dev_5yr_z", "cdd_us_ne", "gasoil_crack_dev_z",
    "sin_ann", "cos_ann",
    "driving_season", "heating_season", "turnaround_season"
  ),

  # Full combined — structural controls + EIA components + interaction amplifiers
  # surprise_z coefficient = pure EIA effect after controlling for everything else
  sfm_t4_combined = c(
    # EIA inventory
    "surprise_z", "cushing_stocks_chg_z",
    "gasoline_stocks_chg_z", "distillate_stocks_chg_z",
    "crude_prod_chg_z", "crude_net_exports_z",
    # Structural (standalone — key difference vs old model)
    "crude_stocks_5yr_dev_z", "cushing_stocks_5yr_dev_z",
    "cftc_net_mm_zscore", "cftc_mm_net_chg_z",
    "td3c_z52", "td3c_wow_ws_z", "bdi_z52",
    "dxy_z", "dxy_4wk_chg_z", "sofr_z",
    "opec_prod_z", "rig_chg_wow_z", "refinery_util_dev",
    "hdd_dev_5yr_z", "cdd_us_ne", "gasoil_crack_dev_z",
    "sin_ann", "cos_ann",
    "driving_season", "heating_season", "turnaround_season",
    # EIA x structural interaction amplifiers
    "sx_cushing", "sx_td3c", "sx_cftc", "sx_5yr_dev", "sx_util"
  )
)


# ── Helpers ───────────────────────────────────────────────────────────────────
.sfm_root <- function() {
  path <- getwd()
  for (i in seq_len(10)) {
    if (file.exists(file.path(path, ".git"))) return(path)
    parent <- dirname(path)
    if (parent == path) break
    path <- parent
  }
  getwd()
}

.sfm_zscore <- function(x, train_mask) {
  mu <- mean(x[train_mask], na.rm = TRUE)
  sg <- sd(x[train_mask], na.rm = TRUE)
  if (!is.finite(sg) || sg < 1e-10) return(rep(0, length(x)))
  (x - mu) / sg
}

.to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))


# ── Data loaders ──────────────────────────────────────────────────────────────
.sfm_load_factors <- function(root) {
  for (nm in c("factors_extended.csv", "factors_combined.csv")) {
    p <- file.path(root, "output", nm)
    if (!file.exists(p)) next
    dt <- fread(p)
    dt[, date := as.Date(date)]
    # Columns stored as character that should be numeric
    char_cols <- c("td3c_z52", "td3c_wow_ws", "td3c_yoy_ws", "td3c_storage_cost_bbl_mo",
                   "bdi", "bdi_4wk_chg", "bdi_z52", "rig_count",
                   "opec_spare_cap_mbd", "cftc_mm_net_chg", "cftc_prod_short", "cftc_swap_net")
    for (col in intersect(char_cols, names(dt)))
      set(dt, j = col, value = .to_num(dt[[col]]))
    message("  Factor file: ", nm, " (", nrow(dt), " rows, ", ncol(dt), " cols)")
    return(dt)
  }
  stop("No factor file found. Run R/factor_loader_extended.R first.")
}

.sfm_load_spreads <- function(product, root) {
  fname <- file.path(root, paste0(product, "_data.csv"))
  if (!file.exists(fname)) stop("Cannot find ", fname)
  raw  <- fread(fname, skip = 1L, header = TRUE)
  cols <- colnames(raw)
  get_mid <- function(cn) {
    idx <- grep(paste0("^", cn, "\\|\\|weighted_mid$"), cols)
    if (!length(idx)) return(NULL)
    as.numeric(raw[[cols[idx]]])
  }
  c1 <- get_mid("c1"); c2 <- get_mid("c2")
  c3 <- get_mid("c3"); c6 <- get_mid("c6")
  if (is.null(c1) || is.null(c2)) stop("c1/c2 not found in ", fname)
  u <- SFM_UNIT_CONV[[product]]
  dt <- data.table(
    date = as.Date(raw[["timestamp"]]),
    m1 = as.numeric(c1), m2 = as.numeric(c2),
    m3 = if (!is.null(c3)) as.numeric(c3) else NA_real_,
    m6 = if (!is.null(c6)) as.numeric(c6) else NA_real_
  )
  dt <- dt[order(date)][, .SD[.N], by = date]
  dt[, `:=`(
    m1m2   = (m1 - m2) * u,
    m2m3   = if (any(!is.na(m3))) (m2 - m3) * u else NA_real_,
    m1m6   = if (any(!is.na(m6))) (m1 - m6) * u else NA_real_,
    fly123 = if (any(!is.na(m3))) (m1 - 2 * m2 + m3) * u else NA_real_,
    fly136 = if (any(!is.na(m3)) && any(!is.na(m6))) (m1 - 2 * m3 + m6) * u else NA_real_
  )]
  dt[, c("m1", "m2", "m3", "m6") := NULL]
  dt
}

.sfm_load_regime <- function(product, root) {
  for (nm in paste0(c("regime_labels_", "signal_", "signals_", "classifier_"), product, ".csv")) {
    p <- file.path(root, "output", nm)
    if (!file.exists(p)) next
    dt <- fread(p)
    dt[, date := as.Date(date)]
    rc <- intersect(c("regime_label", "regime", "label"), names(dt))[1]
    if (!is.na(rc)) return(dt[, .(date, regime = get(rc))])
  }
  stop("No regime file for ", product)
}


# ── Event panel builder ───────────────────────────────────────────────────────
.sfm_build_panel <- function(spreads, factors, regimes, train_mask_fn) {
  fac <- copy(factors)[order(date)]

  # Derive WoW changes from levels where missing
  deriv <- function(chg, lvl) {
    if (!chg %in% names(fac) && lvl %in% names(fac))
      fac[, (chg) := get(lvl) - shift(get(lvl), 1L)]
  }
  deriv("crude_stocks_chg",      "crude_stocks_kb")
  deriv("gasoline_stocks_chg",   "gasoline_stocks_kb")
  deriv("distillate_stocks_chg", "distillate_stocks_kb")
  deriv("cushing_stocks_chg",    "cushing_stocks_kb")
  deriv("crude_prod_chg",        "crude_prod_kbd")
  if ("rig_count" %in% names(fac))
    set(fac, j = "rig_count", value = .to_num(fac[["rig_count"]]))
  deriv("rig_chg_wow", "rig_count")

  # Wednesday EIA events only
  inv <- fac[weekdays(date) == "Wednesday" & !is.na(crude_stocks_chg)]
  if (!nrow(inv)) {
    inv <- fac[weekdays(date) == "Wednesday"]
    message("  WARN: no crude_stocks_chg — using all Wednesdays")
  }
  inv <- inv[order(date)]
  inv <- merge(inv, regimes, by = "date", all.x = TRUE)

  # Training mask passed in by caller (based on row index, not calendar date)
  tr <- train_mask_fn(inv$date)

  # Surprise proxy fallback
  surprise_raw <- "crude_stocks_surprise"
  if (!surprise_raw %in% names(inv) || all(is.na(inv[[surprise_raw]]))) {
    surprise_raw <- "crude_stocks_chg"
    message("  NOTE: using crude_stocks_chg as surprise proxy")
  }

  # Z-score raw columns using TRAINING rows only to avoid look-ahead
  raw_z_map <- list(
    surprise_z               = surprise_raw,
    cushing_stocks_chg_z     = "cushing_stocks_chg",
    crude_prod_chg_z         = "crude_prod_chg",
    gasoline_stocks_chg_z    = "gasoline_stocks_chg",
    distillate_stocks_chg_z  = "distillate_stocks_chg",
    crude_net_exports_z      = "crude_net_exports_kbd",
    rig_chg_wow_z            = "rig_chg_wow",
    hdd_dev_5yr_z            = "hdd_dev_5yr",
    cftc_mm_net_chg_z        = "cftc_mm_net_chg",
    td3c_wow_ws_z            = "td3c_wow_ws",
    td3c_storage_cost_z      = "td3c_storage_cost_bbl_mo",
    dxy_z                    = "dxy",
    dxy_4wk_chg_z            = "dxy_4wk_chg",
    sofr_z                   = "sofr",
    opec_prod_z              = "opec_prod_mbd",
    crude_stocks_5yr_dev_z   = "crude_stocks_5yr_dev",
    cushing_stocks_5yr_dev_z = "cushing_stocks_5yr_dev",
    gasoil_crack_dev_z       = "gasoil_crack_dev"
  )

  for (z_col in names(raw_z_map)) {
    raw_col <- raw_z_map[[z_col]]
    if (raw_col %in% names(inv))
      inv[, (z_col) := .sfm_zscore(get(raw_col), tr)]
    else
      inv[, (z_col) := 0]
  }

  # Pre-normalised columns — use as-is, set to 0 if absent
  passthrough <- c("td3c_z52", "bdi_z52", "cftc_net_mm_zscore", "refinery_util_dev",
                   "sin_ann", "cos_ann", "driving_season", "heating_season",
                   "turnaround_season", "cdd_us_ne")
  for (col in passthrough)
    if (!col %in% names(inv)) inv[, (col) := 0]
  if (!"refinery_util_dev" %in% names(inv)) inv[, refinery_util_dev := 0]

  # Interaction terms
  inv[, sx_cushing := surprise_z * cushing_stocks_chg_z]
  inv[, sx_util    := surprise_z * refinery_util_dev]
  inv[, sx_td3c    := surprise_z * td3c_z52]
  inv[, sx_cftc    := surprise_z * cftc_net_mm_zscore]
  inv[, sx_5yr_dev := surprise_z * crude_stocks_5yr_dev_z]
  for (ic in c("sx_cushing", "sx_util", "sx_td3c", "sx_cftc", "sx_5yr_dev"))
    inv[!is.finite(get(ic)), (ic) := 0]

  # Compute 2-day spread changes around each EIA event
  tgts  <- c("m1m2", "m2m3", "m1m6", "fly123", "fly136")
  sp_dt <- spreads[, c("date", tgts), with = FALSE]

  rbindlist(lapply(seq_len(nrow(inv)), function(i) {
    rd      <- inv$date[i]
    idx_pre <- which(sp_dt$date <= rd)
    idx_pos <- which(sp_dt$date > rd)
    if (!length(idx_pre) || length(idx_pos) < SFM_EVENT_WINDOW) return(NULL)
    pre <- sp_dt[tail(idx_pre, 1)]
    pst <- sp_dt[idx_pos[SFM_EVENT_WINDOW]]
    row <- copy(inv[i])
    for (tgt in tgts) row[, paste0("d_", tgt) := pst[[tgt]] - pre[[tgt]]]
    row
  }), fill = TRUE)
}


# ── Fitting ───────────────────────────────────────────────────────────────────
.sfm_fit <- function(sub, xcols, y_col, filter_sig = TRUE, use_ols = FALSE, trim_y = FALSE) {
  if (!y_col %in% names(sub)) return(NULL)
  if (filter_sig && "surprise_z" %in% names(sub))
    sub <- sub[abs(surprise_z) >= SFM_MIN_SURPRISE_Z]
  y   <- sub[[y_col]]
  Xdt <- sub[, xcols, with = FALSE]
  for (col in names(Xdt)) set(Xdt, which(!is.finite(Xdt[[col]])), col, 0)
  X    <- as.matrix(Xdt)
  keep <- is.finite(y) & apply(is.finite(X), 1, all)
  if (sum(keep) < SFM_MIN_OBS) return(NULL)
  Xk <- X[keep, , drop = FALSE]; yk <- y[keep]
  n <- nrow(Xk); p <- ncol(Xk)

  ols_ok <- use_ols && n >= SFM_OLS_MIN_N && p <= floor(n / SFM_OLS_DOF_RATIO)

  if (ols_ok) {
    if (trim_y) {
      ql <- quantile(yk, SFM_TRIM_QUANTILE); qh <- quantile(yk, 1 - SFM_TRIM_QUANTILE)
      tr <- yk >= ql & yk <= qh
      if (sum(tr) >= SFM_MIN_OBS) { Xk <- Xk[tr, , drop = FALSE]; yk <- yk[tr]; n <- nrow(Xk) }
    }
    fit  <- tryCatch(lm.fit(cbind(1, Xk), yk), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    yh   <- as.numeric(cbind(1, Xk) %*% fit$coefficients)
    sst  <- sum((yk - mean(yk))^2); ssr <- sum((yk - yh)^2)
    r2   <- if (sst > 0) 1 - ssr / sst else NA_real_
    r2a  <- if (!is.na(r2) && n > p + 1) 1 - (1 - r2) * (n - 1) / (n - p - 1) else NA_real_
    sig  <- if ("surprise_z" %in% colnames(Xk)) abs(Xk[, "surprise_z"]) > SFM_SIG_THRESH else rep(TRUE, n)
    coefs <- fit$coefficients
    cr   <- as.data.table(t(coefs))
    setnames(cr, paste0("coef_", c("intercept", colnames(Xk))))
    return(list(n_events = n, r2_insample = round(r2, 4), r2_adj = round(r2a, 4),
                rmse_train = round(sqrt(mean((yk - yh)^2)), 4),
                hit_train = round(mean(sign(yh) == sign(yk), na.rm = TRUE), 4),
                n_sig_train = sum(sig), coef_row = cr, estimator = "OLS"))
  }

  alpha <- if (n >= 40) 1 else 0
  nf    <- min(5, max(3, floor(n / 4)))
  cvf   <- tryCatch(cv.glmnet(Xk, yk, alpha = alpha, nfolds = nf), error = function(e) NULL)
  if (is.null(cvf)) return(NULL)
  fit  <- glmnet(Xk, yk, alpha = alpha, lambda = cvf$lambda.min)
  yh   <- as.numeric(predict(fit, Xk))
  sst  <- sum((yk - mean(yk))^2); ssr <- sum((yk - yh)^2)
  r2   <- if (sst > 0) 1 - ssr / sst else NA_real_
  r2cv <- tryCatch(1 - min(cvf$cvm) / mean((yk - mean(yk))^2), error = function(e) NA_real_)
  sig  <- if ("surprise_z" %in% colnames(Xk)) abs(Xk[, "surprise_z"]) > SFM_SIG_THRESH else rep(TRUE, n)
  coefs <- as.numeric(coef(fit))
  cr   <- as.data.table(t(coefs))
  setnames(cr, paste0("coef_", c("intercept", colnames(Xk))))
  list(n_events = n, r2_insample = round(r2, 4), r2_adj = round(r2cv, 4),
       rmse_train = round(sqrt(mean((yk - yh)^2)), 4),
       hit_train = round(mean(sign(yh) == sign(yk), na.rm = TRUE), 4),
       n_sig_train = sum(sig), coef_row = cr,
       estimator = if (alpha == 0) "Ridge" else "Lasso")
}


# ── Fit all tiers × regimes for a product's training set ─────────────────────
.sfm_fit_all <- function(events_train, product) {
  tgts    <- c("m1m2", "m2m3", "m1m6", "fly123", "fly136")
  regimes <- c(as.list(unique(na.omit(events_train$regime))), list("ALL_REGIMES"))

  rbindlist(lapply(regimes, function(reg) {
    sub <- if (identical(reg, "ALL_REGIMES")) events_train
           else events_train[regime == reg]
    if (nrow(sub) < SFM_MIN_OBS) return(NULL)

    rbindlist(lapply(names(SFM_TIERS), function(tnm) {
      xcols <- SFM_TIERS[[tnm]]
      xcols <- xcols[xcols %in% names(sub)]
      if (!length(xcols)) return(NULL)

      rbindlist(lapply(tgts, function(tgt) {
        res <- .sfm_fit(sub, xcols, paste0("d_", tgt),
                        filter_sig = TRUE, use_ols = TRUE, trim_y = FALSE)
        if (is.null(res)) return(NULL)
        cbind(data.table(product = product, regime = as.character(reg),
                         tier = tnm, spread_target = tgt,
                         estimator = res$estimator,
                         n_train = res$n_events,
                         r2_insample = res$r2_insample,
                         r2_adj = res$r2_adj,
                         rmse_train = res$rmse_train,
                         hit_train = res$hit_train,
                         n_sig_train = res$n_sig_train),
              res$coef_row)
      }), fill = TRUE)
    }), fill = TRUE)
  }), fill = TRUE)
}


# ── Test-set evaluation ───────────────────────────────────────────────────────
.sfm_evaluate_test <- function(events_test, models, product) {
  tgts <- c("m1m2", "m2m3", "m1m6", "fly123", "fly136")
  if (!nrow(events_test)) return(data.table())

  rbindlist(lapply(seq_len(nrow(events_test)), function(i) {
    ev  <- events_test[i]
    if (abs(ev$surprise_z) < SFM_MIN_SURPRISE_Z) return(NULL)
    reg <- if (is.na(ev$regime)) "ALL_REGIMES" else ev$regime

    rbindlist(lapply(tgts, function(tgt) {
      yact <- ev[[paste0("d_", tgt)]]
      if (!is.finite(yact)) return(NULL)

      rbindlist(lapply(names(SFM_TIERS), function(tnm) {
        xcols <- SFM_TIERS[[tnm]]
        xcols <- xcols[xcols %in% names(ev)]
        # Prefer regime-specific model, fall back to ALL_REGIMES
        mod <- models[product == product & spread_target == tgt &
                        tier == tnm & regime == reg]
        if (!nrow(mod))
          mod <- models[product == product & spread_target == tgt &
                          tier == tnm & regime == "ALL_REGIMES"]
        if (!nrow(mod)) return(NULL)
        fv <- sapply(xcols, function(col) {
          v <- ev[[col]]; if (is.finite(v)) v else 0
        })
        cc <- paste0("coef_", xcols)
        cc <- cc[cc %in% names(mod)]
        cv <- as.numeric(mod[1, cc, with = FALSE])
        ic <- if ("coef_intercept" %in% names(mod)) as.numeric(mod$coef_intercept[1]) else 0
        yp <- ic + sum(fv[sub("coef_", "", cc)] * cv, na.rm = TRUE)
        data.table(product = product, date = ev$date, regime = reg,
                   tier = tnm, spread_target = tgt,
                   surprise_z = round(ev$surprise_z, 3),
                   y_actual = round(yact, 4), y_pred = round(yp, 4),
                   correct_sign = (sign(yp) == sign(yact)),
                   error = round(yp - yact, 4),
                   abs_error = round(abs(yp - yact), 4))
      }), fill = TRUE)
    }), fill = TRUE)
  }), fill = TRUE)
}


# ── Summarise test results ────────────────────────────────────────────────────
.sfm_summarise_test <- function(test_dt) {
  if (!nrow(test_dt)) return(data.table())
  rbindlist(list(
    test_dt[, .(
      n_test    = .N,
      hit_rate  = round(mean(correct_sign, na.rm = TRUE), 4),
      rmse      = round(sqrt(mean(error^2, na.rm = TRUE)), 4),
      mae       = round(mean(abs_error, na.rm = TRUE), 4)
    ), by = .(product, tier, spread_target, regime)],
    test_dt[, .(
      regime = "ALL_REGIMES",
      n_test    = .N,
      hit_rate  = round(mean(correct_sign, na.rm = TRUE), 4),
      rmse      = round(sqrt(mean(error^2, na.rm = TRUE)), 4),
      mae       = round(mean(abs_error, na.rm = TRUE), 4)
    ), by = .(product, tier, spread_target)]
  ), fill = TRUE)
}


# ── Build side-by-side comparison: best OLD tier vs best NEW tier ─────────────
.sfm_build_comparison <- function(summary_dt) {
  all_reg <- summary_dt[regime == "ALL_REGIMES"]
  old_tiers <- c("old_t1_baseline", "old_t2_physical", "old_t2_freight", "old_t3_full")
  new_tiers <- c("sfm_t1_eia_only", "sfm_t2_eia_full", "sfm_t3_structural", "sfm_t4_combined")

  old <- all_reg[tier %in% old_tiers]
  new <- all_reg[tier %in% new_tiers]

  # Best old tier per cell (highest hit rate, break ties by RMSE)
  old_best <- old[order(-hit_rate, rmse)][, .SD[1], by = .(product, spread_target)]
  old_best <- old_best[, .(product, spread_target,
                            old_best_tier = tier,
                            old_n = n_test,
                            old_hit = hit_rate,
                            old_rmse = rmse,
                            old_mae = mae)]

  # Best new tier per cell
  new_best <- new[order(-hit_rate, rmse)][, .SD[1], by = .(product, spread_target)]
  new_best <- new_best[, .(product, spread_target,
                            new_best_tier = tier,
                            new_n = n_test,
                            new_hit = hit_rate,
                            new_rmse = rmse,
                            new_mae = mae)]

  comp <- merge(old_best, new_best, by = c("product", "spread_target"), all = TRUE)
  comp[, delta_hit  := round(new_hit  - old_hit,  4)]
  comp[, delta_rmse := round(new_rmse - old_rmse, 4)]
  comp
}


# ══ MAIN RUNNER ═══════════════════════════════════════════════════════════════
run_spread_factor_model <- function(root = NULL, train_frac = SFM_TRAIN_FRAC) {
  root <- if (!is.null(root)) root else .sfm_root()
  odir <- file.path(root, "output")
  if (!dir.exists(odir)) dir.create(odir, recursive = TRUE)

  message("══ Spread Factor Model — Train/Test Split ═══════════════════════")
  message("Root       : ", root)
  message("Train frac : ", sprintf("%.0f%% train / %.0f%% test", train_frac * 100, (1 - train_frac) * 100))
  message("Tiers      : old (4) + new (4) = 8 total")
  message("Products   : ", paste(SFM_PRODUCTS, collapse = " | "))

  message("\n[1] Loading factors...")
  factors <- .sfm_load_factors(root)

  all_models <- list(); all_test <- list(); split_info <- list()

  for (prod in SFM_PRODUCTS) {
    message("\n── ", prod, " ──────────────────────────────────────────────────")

    spreads <- tryCatch(.sfm_load_spreads(prod, root),
                        error = function(e) { message("  SKIP: ", e$message); NULL })
    if (is.null(spreads)) next

    regimes <- tryCatch(.sfm_load_regime(prod, root),
                        error = function(e) { message("  SKIP: ", e$message); NULL })
    if (is.null(regimes)) next

    message("  Building event panel...")

    # Determine train cutoff after we know how many events exist
    # First pass to get event dates
    fac_tmp <- copy(factors)[order(date)]
    ev_dates <- fac_tmp[weekdays(date) == "Wednesday" & !is.na(crude_stocks_chg), date]
    if (!length(ev_dates)) ev_dates <- fac_tmp[weekdays(date) == "Wednesday", date]
    n_ev      <- length(ev_dates)
    n_train   <- floor(n_ev * train_frac)
    cutoff    <- ev_dates[n_train]
    message(sprintf("  Events total=%d  train=%d (before %s)  test=%d (from %s)",
                    n_ev, n_train, cutoff, n_ev - n_train, ev_dates[n_train + 1]))

    train_mask_fn <- function(dates) dates <= cutoff

    events <- tryCatch(
      .sfm_build_panel(spreads, factors, regimes, train_mask_fn),
      error = function(e) { message("  SKIP: ", e$message); NULL }
    )
    if (is.null(events) || !nrow(events)) { message("  No events built."); next }

    train <- events[date <= cutoff]
    test  <- events[date >  cutoff]
    n_sig_test <- nrow(test[abs(surprise_z) >= SFM_MIN_SURPRISE_Z])
    message(sprintf("  Panel: train=%d  test=%d  (sig test: %d)",
                    nrow(train), nrow(test), n_sig_test))

    split_info[[prod]] <- list(cutoff = cutoff, n_train = nrow(train),
                               n_test = nrow(test), n_sig_test = n_sig_test)

    message("  Fitting models on training set...")
    models <- tryCatch(.sfm_fit_all(train, prod),
                       error = function(e) { message("  ERR fit: ", e$message); NULL })
    if (is.null(models) || !nrow(models)) next
    all_models[[prod]] <- models

    message("  Evaluating on test set...")
    test_ev <- tryCatch(.sfm_evaluate_test(test, models, prod),
                        error = function(e) { message("  ERR eval: ", e$message); data.table() })
    if (nrow(test_ev)) {
      all_test[[prod]] <- test_ev
      # Quick summary: best new tier for m1m2
      q <- test_ev[tier == "sfm_t4_combined" & spread_target == "m1m2"]
      if (nrow(q))
        message(sprintf("  sfm_t4 m1m2 test: hit=%.1f%%  RMSE=%.3f  n=%d",
                        mean(q$correct_sign, na.rm = TRUE) * 100,
                        sqrt(mean(q$error^2, na.rm = TRUE)), nrow(q)))
      q2 <- test_ev[tier == "old_t3_full" & spread_target == "m1m2"]
      if (nrow(q2))
        message(sprintf("  old_t3 m1m2 test: hit=%.1f%%  RMSE=%.3f  n=%d",
                        mean(q2$correct_sign, na.rm = TRUE) * 100,
                        sqrt(mean(q2$error^2, na.rm = TRUE)), nrow(q2)))
    }
  }

  # ── Save ─────────────────────────────────────────────────────────────────────
  message("\n── Saving outputs ───────────────────────────────────────────────")
  models_dt <- rbindlist(all_models, fill = TRUE)
  test_dt   <- rbindlist(all_test,   fill = TRUE)
  summary   <- .sfm_summarise_test(test_dt)
  comp      <- .sfm_build_comparison(summary)

  fwrite(models_dt, file.path(odir, "sfm_results.csv"))
  fwrite(test_dt,   file.path(odir, "sfm_test.csv"))
  fwrite(summary,   file.path(odir, "sfm_report.csv"))
  fwrite(comp,      file.path(odir, "sfm_comparison.csv"))
  saveRDS(list(models = models_dt, test = test_dt,
               summary = summary, comparison = comp,
               split_info = split_info),
          file.path(odir, "sfm_models.rds"))
  message("  Saved: sfm_results.csv, sfm_test.csv, sfm_report.csv,")
  message("         sfm_comparison.csv, sfm_models.rds")

  # ── Print comparison table ────────────────────────────────────────────────
  cat("\n══ TEST-SET COMPARISON: Best OLD tier vs Best NEW tier ══════════\n")
  cat(sprintf("  %-6s %-8s  %-22s %6s %6s  %-22s %6s %6s  %7s %7s\n",
              "Prod", "Spread",
              "Old best tier", "Hit%", "RMSE",
              "New best tier", "Hit%", "RMSE",
              "ΔHit%", "ΔRMSE"))
  cat("  ", strrep("─", 110), "\n")

  spreads_show <- c("m1m2", "m2m3", "m1m6", "fly123", "fly136")
  setorder(comp, product, spread_target)
  for (j in seq_len(nrow(comp))) {
    r <- comp[j]
    if (!r$spread_target %in% spreads_show) next
    oh <- if (!is.na(r$old_hit))  sprintf("%5.1f%%", r$old_hit  * 100) else "  n/a"
    nh <- if (!is.na(r$new_hit))  sprintf("%5.1f%%", r$new_hit  * 100) else "  n/a"
    or <- if (!is.na(r$old_rmse)) sprintf("%6.3f",  r$old_rmse)        else "   n/a"
    nr <- if (!is.na(r$new_rmse)) sprintf("%6.3f",  r$new_rmse)        else "   n/a"
    dh <- if (!is.na(r$delta_hit))  sprintf("%+6.1f%%", r$delta_hit  * 100) else "    n/a"
    dr <- if (!is.na(r$delta_rmse)) sprintf("%+7.3f",   r$delta_rmse)       else "     n/a"
    cat(sprintf("  %-6s %-8s  %-22s %6s %6s  %-22s %6s %6s  %7s %7s\n",
                r$product, r$spread_target,
                r$old_best_tier, oh, or,
                r$new_best_tier, nh, nr,
                dh, dr))
  }
  cat("  ", strrep("─", 110), "\n\n")

  # ── Print full hit-rate grid ──────────────────────────────────────────────
  cat("══ FULL HIT RATE GRID — ALL_REGIMES, sig surprises ≥ 0.4σ ══════\n")
  all_reg_sum <- summary[regime == "ALL_REGIMES"]
  tier_order  <- c("old_t1_baseline", "old_t2_physical", "old_t2_freight", "old_t3_full",
                   "sfm_t1_eia_only", "sfm_t2_eia_full", "sfm_t3_structural", "sfm_t4_combined")
  cat(sprintf("  %-6s %-8s", "Prod", "Spread"))
  for (tn in tier_order) cat(sprintf("  %-9s", substr(tn, 1, 9)))
  cat("\n  ", strrep("─", 100), "\n")
  for (prod in SFM_PRODUCTS) {
    for (spr in spreads_show) {
      sub <- all_reg_sum[product == prod & spread_target == spr]
      cat(sprintf("  %-6s %-8s", prod, spr))
      for (tn in tier_order) {
        row <- sub[tier == tn]
        val <- if (nrow(row)) sprintf("%7.1f%%", row$hit_rate[1] * 100) else "    n/a"
        cat(sprintf("  %-9s", val))
      }
      cat("\n")
    }
  }
  cat("  ", strrep("─", 100), "\n\n")

  # ── RMSE grid ────────────────────────────────────────────────────────────
  cat("══ FULL RMSE GRID ($/bbl) ═══════════════════════════════════════\n")
  cat(sprintf("  %-6s %-8s", "Prod", "Spread"))
  for (tn in tier_order) cat(sprintf("  %-9s", substr(tn, 1, 9)))
  cat("\n  ", strrep("─", 100), "\n")
  for (prod in SFM_PRODUCTS) {
    for (spr in spreads_show) {
      sub <- all_reg_sum[product == prod & spread_target == spr]
      cat(sprintf("  %-6s %-8s", prod, spr))
      for (tn in tier_order) {
        row <- sub[tier == tn]
        val <- if (nrow(row)) sprintf("%7.3f", row$rmse[1]) else "    n/a"
        cat(sprintf("  %-9s", val))
      }
      cat("\n")
    }
  }
  cat("  ", strrep("─", 100), "\n\n")

  invisible(list(models = models_dt, test = test_dt,
                 summary = summary, comparison = comp))
}

# results <- run_spread_factor_model()
