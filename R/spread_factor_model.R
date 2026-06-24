# R/spread_factor_model.R
# ─────────────────────────────────────────────────────────────────────────────
# ESTIMATOR FAMILIES
#   Linear  — Ridge / Lasso (glmnet) or OLS with DOF guard
#   Non-linear — Random Forest (randomForest) and XGBoost (xgboost)
#
#   Tree models train on ALL events (no sig-surprise filter) so the model
#   learns the significance threshold endogenously. Linear models keep the
#   filter (|surprise_z| >= 0.4) for consistency with original design.
#   Test evaluation is filtered to |surprise_z| >= 0.4 for ALL estimators.
#
# TIER STRUCTURE
#   Old linear : old_t1_baseline, old_t2_physical, old_t2_freight, old_t3_full
#   New linear : sfm_t1_eia_only, sfm_t2_eia_full, sfm_t3_structural, sfm_t4_combined
#   Non-linear : old_t3_rf, old_t3_xgb, sfm_t4_rf, sfm_t4_xgb
#                (same features as their linear counterparts)
#
# TRAIN / TEST SPLIT
#   Events sorted by date; first TRAIN_FRAC fraction = train, rest = test.
#   Z-score normalisers computed on training rows only (no look-ahead).
#
# OUTPUTS
#   output/sfm_results.csv   — in-sample metrics
#   output/sfm_test.csv      — event-level test predictions
#   output/sfm_report.csv    — test-set summary per tier/spread/regime
#   output/sfm_comparison.csv— best old vs best new per spread
#   output/sfm_models.rds    — model objects + metrics
# ─────────────────────────────────────────────────────────────────────────────


# ── Packages ──────────────────────────────────────────────────────────────────
.ensure_sfm <- function() {
  req <- c("data.table", "lubridate", "glmnet", "randomForest", "xgboost")
  mis <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
  if (length(mis)) install.packages(mis, repos = "https://cloud.r-project.org", quiet = TRUE)
}
.ensure_sfm()
suppressPackageStartupMessages({
  library(data.table); library(lubridate)
  library(glmnet); library(randomForest); library(xgboost)
})


# ── Constants ─────────────────────────────────────────────────────────────────
SFM_TRAIN_FRAC      <- 0.70
SFM_EVENT_WINDOW    <- 2L
SFM_MIN_OBS_LINEAR  <- 15L   # minimum events to fit linear model
SFM_MIN_OBS_TREE    <- 40L   # minimum events to fit tree model
SFM_MIN_SURPRISE_Z  <- 0.4   # test-set evaluation filter
SFM_OLS_MIN_N       <- 30L
SFM_OLS_DOF_RATIO   <- 10L
SFM_TRIM_QUANTILE   <- 0.05
SFM_UNIT_CONV       <- list(CL = 1.0, LCO = 1.0, HO = 42.0, LGO = 1 / 7.45)
SFM_PRODUCTS        <- c("CL", "LCO", "HO", "LGO")

# XGBoost defaults
XGB_MAX_DEPTH   <- 3
XGB_ETA         <- 0.05
XGB_NROUNDS_MAX <- 300
XGB_EARLY_STOP  <- 25
XGB_NFOLD       <- 5
XGB_SUBSAMPLE   <- 0.8
XGB_COLSAMPLE   <- 0.8
XGB_MIN_CHILD   <- 3


# ── Tier feature definitions ──────────────────────────────────────────────────
# Each tier defines a character vector of column names from the event panel.
# Tree tiers reference the same features as their linear counterparts via
# SFM_TREE_FEATURES — the fitting function selects the right set.

SFM_TIERS_LINEAR <- list(
  old_t1_baseline  = c("surprise_z"),
  old_t2_physical  = c("surprise_z", "cushing_stocks_chg_z", "refinery_util_dev",
                        "crude_prod_chg_z", "crude_net_exports_z",
                        "sx_cushing", "sx_util"),
  old_t2_freight   = c("surprise_z", "cushing_stocks_chg_z", "refinery_util_dev",
                        "crude_net_exports_z", "td3c_z52", "td3c_wow_ws_z",
                        "sx_cushing", "sx_td3c"),
  old_t3_full      = c("surprise_z", "cushing_stocks_chg_z", "refinery_util_dev",
                        "crude_prod_chg_z", "crude_net_exports_z",
                        "gasoline_stocks_chg_z", "distillate_stocks_chg_z",
                        "rig_chg_wow_z", "td3c_z52", "td3c_wow_ws_z",
                        "td3c_storage_cost_z", "bdi_z52",
                        "hdd_dev_5yr_z", "cdd_us_ne", "cftc_mm_net_chg_z",
                        "driving_season", "heating_season", "turnaround_season",
                        "sin_ann", "cos_ann",
                        "sx_cushing", "sx_util", "sx_td3c", "sx_cftc", "sx_5yr_dev"),
  sfm_t1_eia_only  = c("surprise_z"),
  sfm_t2_eia_full  = c("surprise_z", "cushing_stocks_chg_z",
                        "gasoline_stocks_chg_z", "distillate_stocks_chg_z",
                        "crude_prod_chg_z", "crude_net_exports_z"),
  sfm_t3_structural = c("crude_stocks_5yr_dev_z", "cushing_stocks_5yr_dev_z",
                         "cftc_net_mm_zscore", "cftc_mm_net_chg_z",
                         "td3c_z52", "td3c_wow_ws_z", "bdi_z52",
                         "dxy_z", "dxy_4wk_chg_z", "sofr_z",
                         "opec_prod_z", "rig_chg_wow_z", "refinery_util_dev",
                         "hdd_dev_5yr_z", "cdd_us_ne", "gasoil_crack_dev_z",
                         "sin_ann", "cos_ann",
                         "driving_season", "heating_season", "turnaround_season"),
  sfm_t4_combined  = c("surprise_z", "cushing_stocks_chg_z",
                        "gasoline_stocks_chg_z", "distillate_stocks_chg_z",
                        "crude_prod_chg_z", "crude_net_exports_z",
                        "crude_stocks_5yr_dev_z", "cushing_stocks_5yr_dev_z",
                        "cftc_net_mm_zscore", "cftc_mm_net_chg_z",
                        "td3c_z52", "td3c_wow_ws_z", "bdi_z52",
                        "dxy_z", "dxy_4wk_chg_z", "sofr_z",
                        "opec_prod_z", "rig_chg_wow_z", "refinery_util_dev",
                        "hdd_dev_5yr_z", "cdd_us_ne", "gasoil_crack_dev_z",
                        "sin_ann", "cos_ann",
                        "driving_season", "heating_season", "turnaround_season",
                        "sx_cushing", "sx_td3c", "sx_cftc", "sx_5yr_dev", "sx_util")
)

# Tree tiers use same features as their linear counterparts
SFM_TIERS_TREE <- list(
  old_t3_rf  = SFM_TIERS_LINEAR[["old_t3_full"]],
  old_t3_xgb = SFM_TIERS_LINEAR[["old_t3_full"]],
  sfm_t4_rf  = SFM_TIERS_LINEAR[["sfm_t4_combined"]],
  sfm_t4_xgb = SFM_TIERS_LINEAR[["sfm_t4_combined"]]
)

SFM_TIERS <- c(SFM_TIERS_LINEAR, SFM_TIERS_TREE)


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
  sg <- sd(x[train_mask],   na.rm = TRUE)
  if (!is.finite(sg) || sg < 1e-10) return(rep(0, length(x)))
  (x - mu) / sg
}

.to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

.is_tree <- function(tnm) grepl("_rf$|_xgb$", tnm)


# ── Data loaders ──────────────────────────────────────────────────────────────
.sfm_load_factors <- function(root) {
  for (nm in c("factors_extended.csv", "factors_combined.csv")) {
    p <- file.path(root, "output", nm)
    if (!file.exists(p)) next
    dt <- fread(p)
    dt[, date := as.Date(date)]
    char_cols <- c("td3c_z52", "td3c_wow_ws", "td3c_yoy_ws",
                   "td3c_storage_cost_bbl_mo", "bdi", "bdi_4wk_chg", "bdi_z52",
                   "rig_count", "opec_spare_cap_mbd",
                   "cftc_mm_net_chg", "cftc_prod_short", "cftc_swap_net")
    for (col in intersect(char_cols, names(dt)))
      set(dt, j = col, value = .to_num(dt[[col]]))
    message("  Factors: ", nm, " (", nrow(dt), " rows, ", ncol(dt), " cols)")
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
  u  <- SFM_UNIT_CONV[[product]]
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
    dt <- fread(p); dt[, date := as.Date(date)]
    rc <- intersect(c("regime_label", "regime", "label"), names(dt))[1]
    if (!is.na(rc)) return(dt[, .(date, regime = get(rc))])
  }
  stop("No regime file for ", product)
}


# ── Event panel builder ───────────────────────────────────────────────────────
.sfm_build_panel <- function(spreads, factors, regimes, train_cutoff) {
  fac <- copy(factors)[order(date)]

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

  inv <- fac[weekdays(date) == "Wednesday" & !is.na(crude_stocks_chg)]
  if (!nrow(inv)) inv <- fac[weekdays(date) == "Wednesday"]
  inv <- inv[order(date)]
  inv <- merge(inv, regimes, by = "date", all.x = TRUE)

  tr <- inv$date <= train_cutoff

  # Surprise proxy
  surprise_raw <- if ("crude_stocks_surprise" %in% names(inv) &&
                      !all(is.na(inv$crude_stocks_surprise)))
                    "crude_stocks_surprise" else "crude_stocks_chg"

  # Z-score all raw columns using training rows only
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
  for (zc in names(raw_z_map)) {
    rc <- raw_z_map[[zc]]
    if (rc %in% names(inv)) inv[, (zc) := .sfm_zscore(get(rc), tr)]
    else                     inv[, (zc) := 0]
  }

  passthrough <- c("td3c_z52", "bdi_z52", "cftc_net_mm_zscore",
                   "refinery_util_dev", "sin_ann", "cos_ann",
                   "driving_season", "heating_season", "turnaround_season", "cdd_us_ne")
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

  # Compute 2-day spread changes
  tgts  <- c("m1m2", "m2m3", "m1m6", "fly123", "fly136")
  sp_dt <- spreads[, c("date", tgts), with = FALSE]

  rbindlist(lapply(seq_len(nrow(inv)), function(i) {
    rd      <- inv$date[i]
    idx_pre <- which(sp_dt$date <= rd)
    idx_pos <- which(sp_dt$date >  rd)
    if (!length(idx_pre) || length(idx_pos) < SFM_EVENT_WINDOW) return(NULL)
    pre <- sp_dt[tail(idx_pre, 1)]
    pst <- sp_dt[idx_pos[SFM_EVENT_WINDOW]]
    row <- copy(inv[i])
    for (tgt in tgts) row[, paste0("d_", tgt) := pst[[tgt]] - pre[[tgt]]]
    row
  }), fill = TRUE)
}


# ── Model fitting ─────────────────────────────────────────────────────────────

# Returns list(type, obj, xcols) or NULL on failure
.fit_linear <- function(Xk, yk, xcols) {
  n <- nrow(Xk); p <- ncol(Xk)
  ols_ok <- n >= SFM_OLS_MIN_N && p <= floor(n / SFM_OLS_DOF_RATIO)
  if (ols_ok) {
    fit <- tryCatch(lm.fit(cbind(1, Xk), yk), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    list(type = "lm", obj = fit, xcols = xcols)
  } else {
    alpha <- if (n >= 40) 1 else 0
    nf    <- min(5, max(3, floor(n / 4)))
    cvf   <- tryCatch(cv.glmnet(Xk, yk, alpha = alpha, nfolds = nf), error = function(e) NULL)
    if (is.null(cvf)) return(NULL)
    fit <- glmnet(Xk, yk, alpha = alpha, lambda = cvf$lambda.min)
    list(type = if (alpha == 0) "ridge" else "lasso",
         obj = fit, xcols = xcols, alpha = alpha)
  }
}

.fit_rf <- function(Xk, yk, xcols) {
  df <- as.data.frame(Xk)
  mt <- max(1L, floor(sqrt(ncol(Xk))))
  ns <- max(3L, floor(nrow(Xk) / 20L))
  fit <- tryCatch(
    randomForest(x = df, y = yk, ntree = 500L, mtry = mt, nodesize = ns),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  list(type = "rf", obj = fit, xcols = xcols)
}

.fit_xgb <- function(Xk, yk, xcols) {
  dtrain <- xgb.DMatrix(Xk, label = yk)
  params <- list(
    booster          = "gbtree",
    objective        = "reg:squarederror",
    max_depth        = XGB_MAX_DEPTH,
    eta              = XGB_ETA,
    subsample        = XGB_SUBSAMPLE,
    colsample_bytree = XGB_COLSAMPLE,
    min_child_weight = XGB_MIN_CHILD,
    verbosity        = 0
  )
  cv <- tryCatch(
    xgb.cv(params, dtrain, nrounds = XGB_NROUNDS_MAX,
            nfold = min(XGB_NFOLD, floor(nrow(Xk) / 8)),
            early_stopping_rounds = XGB_EARLY_STOP,
            verbose = FALSE, showsd = FALSE),
    error = function(e) NULL
  )
  if (is.null(cv)) return(NULL)
  best_n <- max(1, cv$best_iteration)
  fit <- tryCatch(
    xgb.train(params, dtrain, nrounds = best_n, verbose = 0),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  list(type = "xgb", obj = fit, xcols = xcols, nrounds = best_n)
}

# Predict from a stored model object given a feature matrix (one or more rows)
.sfm_predict <- function(m, X_new) {
  X_new <- as.matrix(X_new)
  tryCatch({
    if (m$type == "lm") {
      as.numeric(cbind(1, X_new) %*% m$obj$coefficients)
    } else if (m$type %in% c("ridge", "lasso")) {
      as.numeric(predict(m$obj, X_new))
    } else if (m$type == "rf") {
      as.numeric(predict(m$obj, as.data.frame(X_new)))
    } else if (m$type == "xgb") {
      as.numeric(predict(m$obj, xgb.DMatrix(X_new)))
    } else NA_real_
  }, error = function(e) rep(NA_real_, nrow(X_new)))
}

# In-sample stats given a fitted model
.insample_stats <- function(m, Xk, yk) {
  yh  <- .sfm_predict(m, Xk)
  ok  <- is.finite(yh) & is.finite(yk)
  if (sum(ok) < 3) return(list(r2 = NA, rmse = NA, hit = NA))
  yh  <- yh[ok]; yk2 <- yk[ok]
  sst <- sum((yk2 - mean(yk2))^2)
  ssr <- sum((yk2 - yh)^2)
  list(
    r2   = if (sst > 0) round(1 - ssr / sst, 4) else NA_real_,
    rmse = round(sqrt(mean((yk2 - yh)^2)), 4),
    hit  = round(mean(sign(yh) == sign(yk2)), 4)
  )
}


# ── Fit all tiers × regimes ───────────────────────────────────────────────────
# model_store: environment keyed by "product||tier||spread||regime"
.sfm_fit_all <- function(events_train, product, model_store) {
  tgts    <- c("m1m2", "m2m3", "m1m6", "fly123", "fly136")
  regimes <- c(as.list(unique(na.omit(events_train$regime))), list("ALL_REGIMES"))
  rows    <- list()

  for (reg in regimes) {
    sub_all <- if (identical(reg, "ALL_REGIMES")) events_train
               else events_train[regime == reg]

    for (tnm in names(SFM_TIERS)) {
      is_tree   <- .is_tree(tnm)
      min_obs   <- if (is_tree) SFM_MIN_OBS_TREE else SFM_MIN_OBS_LINEAR
      xcols_raw <- SFM_TIERS[[tnm]]

      # Linear: filter to significant surprises; Trees: all events
      sub <- if (!is_tree && "surprise_z" %in% names(sub_all))
               sub_all[abs(surprise_z) >= SFM_MIN_SURPRISE_Z]
             else sub_all

      if (nrow(sub) < min_obs) next

      xcols <- xcols_raw[xcols_raw %in% names(sub)]
      if (!length(xcols)) next

      for (tgt in tgts) {
        y_col <- paste0("d_", tgt)
        if (!y_col %in% names(sub)) next
        y   <- sub[[y_col]]
        Xdt <- sub[, xcols, with = FALSE]
        for (col in names(Xdt)) set(Xdt, which(!is.finite(Xdt[[col]])), col, 0)
        X    <- as.matrix(Xdt)
        keep <- is.finite(y) & apply(is.finite(X), 1, all)
        if (sum(keep) < min_obs) next
        Xk <- X[keep, , drop = FALSE]; yk <- y[keep]

        m <- if (is_tree && grepl("_rf$", tnm))   .fit_rf(Xk, yk, xcols)
             else if (is_tree && grepl("_xgb$", tnm)) .fit_xgb(Xk, yk, xcols)
             else .fit_linear(Xk, yk, xcols)
        if (is.null(m)) next

        key <- paste(product, tnm, tgt, as.character(reg), sep = "||")
        assign(key, m, envir = model_store)

        st <- .insample_stats(m, Xk, yk)
        rows[[length(rows) + 1]] <- data.table(
          product      = product,
          regime       = as.character(reg),
          tier         = tnm,
          spread_target = tgt,
          estimator    = m$type,
          n_train      = nrow(Xk),
          r2_insample  = st$r2,
          rmse_train   = st$rmse,
          hit_train    = st$hit
        )
      }
    }
  }
  rbindlist(rows, fill = TRUE)
}


# ── Test-set evaluation ───────────────────────────────────────────────────────
.sfm_evaluate_test <- function(events_test, model_store, product) {
  tgts <- c("m1m2", "m2m3", "m1m6", "fly123", "fly136")
  test_sig <- events_test[abs(surprise_z) >= SFM_MIN_SURPRISE_Z]
  if (!nrow(test_sig)) return(data.table())

  rbindlist(lapply(seq_len(nrow(test_sig)), function(i) {
    ev  <- test_sig[i]
    reg <- if (is.na(ev$regime)) "ALL_REGIMES" else ev$regime

    rbindlist(lapply(tgts, function(tgt) {
      yact <- ev[[paste0("d_", tgt)]]
      if (!is.finite(yact)) return(NULL)

      rbindlist(lapply(names(SFM_TIERS), function(tnm) {
        xcols <- SFM_TIERS[[tnm]]
        xcols <- xcols[xcols %in% names(ev)]
        if (!length(xcols)) return(NULL)

        # Prefer regime-specific model; fall back to ALL_REGIMES
        key1 <- paste(product, tnm, tgt, reg,            sep = "||")
        key2 <- paste(product, tnm, tgt, "ALL_REGIMES",  sep = "||")
        m <- if (exists(key1, envir = model_store, inherits = FALSE))
               get(key1, envir = model_store, inherits = FALSE)
             else if (exists(key2, envir = model_store, inherits = FALSE))
               get(key2, envir = model_store, inherits = FALSE)
             else return(NULL)

        Xrow <- matrix(sapply(xcols, function(col) {
          v <- ev[[col]]; if (is.finite(v)) v else 0
        }), nrow = 1, dimnames = list(NULL, xcols))

        yp <- .sfm_predict(m, Xrow)
        if (!is.finite(yp)) return(NULL)

        data.table(
          product       = product,
          date          = ev$date,
          regime        = reg,
          tier          = tnm,
          spread_target = tgt,
          estimator     = m$type,
          surprise_z    = round(ev$surprise_z, 3),
          y_actual      = round(yact, 4),
          y_pred        = round(yp, 4),
          correct_sign  = (sign(yp) == sign(yact)),
          error         = round(yp - yact, 4),
          abs_error     = round(abs(yp - yact), 4)
        )
      }), fill = TRUE)
    }), fill = TRUE)
  }), fill = TRUE)
}


# ── Summary & comparison ──────────────────────────────────────────────────────
.sfm_summarise <- function(test_dt) {
  if (!nrow(test_dt)) return(data.table())
  rbindlist(list(
    test_dt[, .(n_test   = .N,
                hit_rate = round(mean(correct_sign, na.rm = TRUE), 4),
                rmse     = round(sqrt(mean(error^2, na.rm = TRUE)), 4),
                mae      = round(mean(abs_error,    na.rm = TRUE), 4)),
            by = .(product, tier, spread_target, regime)],
    test_dt[, .(regime   = "ALL_REGIMES",
                n_test   = .N,
                hit_rate = round(mean(correct_sign, na.rm = TRUE), 4),
                rmse     = round(sqrt(mean(error^2, na.rm = TRUE)), 4),
                mae      = round(mean(abs_error,    na.rm = TRUE), 4)),
            by = .(product, tier, spread_target)]
  ), fill = TRUE)
}

.sfm_comparison <- function(summary_dt) {
  ar  <- summary_dt[regime == "ALL_REGIMES"]
  old <- ar[grepl("^old_", tier)][order(-hit_rate, rmse)][, .SD[1], by = .(product, spread_target)]
  new <- ar[grepl("^sfm_", tier)][order(-hit_rate, rmse)][, .SD[1], by = .(product, spread_target)]
  old <- old[, .(product, spread_target, old_tier = tier, old_n = n_test,
                 old_hit = hit_rate, old_rmse = rmse, old_mae = mae)]
  new <- new[, .(product, spread_target, new_tier = tier, new_n = n_test,
                 new_hit = hit_rate, new_rmse = rmse, new_mae = mae)]
  comp <- merge(old, new, by = c("product", "spread_target"), all = TRUE)
  comp[, `:=`(delta_hit  = round(new_hit  - old_hit,  4),
              delta_rmse = round(new_rmse - old_rmse, 4))]
  comp
}


# ══ MAIN RUNNER ═══════════════════════════════════════════════════════════════
run_spread_factor_model <- function(root = NULL, train_frac = SFM_TRAIN_FRAC) {
  root <- if (!is.null(root)) root else .sfm_root()
  odir <- file.path(root, "output")
  if (!dir.exists(odir)) dir.create(odir, recursive = TRUE)

  message("══ Spread Factor Model — Linear + RF + XGBoost ══════════════════")
  message("Train frac : ", sprintf("%.0f%% / %.0f%%", train_frac*100, (1-train_frac)*100))
  message("Tiers      : ", length(SFM_TIERS_LINEAR), " linear  +  ",
          length(SFM_TIERS_TREE), " tree (RF/XGB)")

  message("\n[1] Loading factors...")
  factors <- .sfm_load_factors(root)

  model_store  <- new.env(parent = emptyenv())
  all_metrics  <- list()
  all_test     <- list()

  for (prod in SFM_PRODUCTS) {
    message("\n── ", prod, " ──────────────────────────────────────────────────")

    spreads <- tryCatch(.sfm_load_spreads(prod, root),
                        error = function(e) { message("  SKIP: ", e$message); NULL })
    if (is.null(spreads)) next

    regimes <- tryCatch(.sfm_load_regime(prod, root),
                        error = function(e) { message("  SKIP: ", e$message); NULL })
    if (is.null(regimes)) next

    # Determine train cutoff by event count
    ev_dates  <- factors[weekdays(date) == "Wednesday" & !is.na(crude_stocks_chg), date]
    if (!length(ev_dates)) ev_dates <- factors[weekdays(date) == "Wednesday", date]
    ev_dates  <- sort(unique(ev_dates))
    n_ev      <- length(ev_dates)
    n_tr      <- floor(n_ev * train_frac)
    cutoff    <- ev_dates[n_tr]
    message(sprintf("  Cutoff: %s  (train=%d  test=%d)", cutoff, n_tr, n_ev - n_tr))

    message("  Building event panel...")
    events <- tryCatch(
      .sfm_build_panel(spreads, factors, regimes, cutoff),
      error = function(e) { message("  SKIP: ", e$message); NULL }
    )
    if (is.null(events) || !nrow(events)) { message("  No events."); next }

    train <- events[date <= cutoff]
    test  <- events[date >  cutoff]
    message(sprintf("  Panel: train=%d  test=%d  (sig test: %d)",
                    nrow(train), nrow(test),
                    nrow(test[abs(surprise_z) >= SFM_MIN_SURPRISE_Z])))

    message("  Fitting linear models...")
    metrics <- tryCatch(
      .sfm_fit_all(train, prod, model_store),
      error = function(e) { message("  ERR fit: ", e$message); NULL }
    )
    if (!is.null(metrics)) all_metrics[[prod]] <- metrics

    message("  Evaluating on test set...")
    test_ev <- tryCatch(
      .sfm_evaluate_test(test, model_store, prod),
      error = function(e) { message("  ERR eval: ", e$message); data.table() }
    )
    if (nrow(test_ev)) {
      all_test[[prod]] <- test_ev
      for (tnm in c("old_t3_full", "old_t3_xgb", "sfm_t4_combined", "sfm_t4_xgb")) {
        q <- test_ev[tier == tnm & spread_target == "m1m2"]
        if (nrow(q))
          message(sprintf("    %-18s m1m2: hit=%5.1f%%  RMSE=%.3f  n=%d",
                          tnm,
                          mean(q$correct_sign, na.rm = TRUE) * 100,
                          sqrt(mean(q$error^2, na.rm = TRUE)),
                          nrow(q)))
      }
    }
  }

  # ── Save ─────────────────────────────────────────────────────────────────────
  message("\n── Saving outputs ───────────────────────────────────────────────")
  metrics_dt <- rbindlist(all_metrics, fill = TRUE)
  test_dt    <- rbindlist(all_test,    fill = TRUE)
  summary    <- .sfm_summarise(test_dt)
  comp       <- .sfm_comparison(summary)

  fwrite(metrics_dt, file.path(odir, "sfm_results.csv"))
  fwrite(test_dt,    file.path(odir, "sfm_test.csv"))
  fwrite(summary,    file.path(odir, "sfm_report.csv"))
  fwrite(comp,       file.path(odir, "sfm_comparison.csv"))
  saveRDS(list(metrics = metrics_dt, test = test_dt,
               summary = summary, comparison = comp),
          file.path(odir, "sfm_models.rds"))
  message("  Saved: sfm_results.csv, sfm_test.csv, sfm_report.csv,")
  message("         sfm_comparison.csv, sfm_models.rds")

  # ── Print hit rate grid ──────────────────────────────────────────────────
  tier_order <- c("old_t1_baseline", "old_t2_freight", "old_t3_full",
                  "old_t3_rf", "old_t3_xgb",
                  "sfm_t4_combined", "sfm_t4_rf", "sfm_t4_xgb")
  tier_lbl   <- c("old_t1", "old_t2fr", "old_t3", "old_t3RF", "old_t3XGB",
                  "sfm_t4", "sfm_t4RF", "sfm_t4XGB")
  spreads_show <- c("m1m2", "m2m3", "m1m6", "fly123", "fly136")
  ar <- summary[regime == "ALL_REGIMES"]

  cat("\n══ TEST HIT RATE GRID — ALL_REGIMES (%)\n")
  cat(sprintf("  %-6s %-8s", "Prod", "Spread"))
  for (lb in tier_lbl) cat(sprintf("  %9s", lb))
  cat("\n  ", strrep("─", 100), "\n")
  for (prod in SFM_PRODUCTS) {
    for (spr in spreads_show) {
      sub <- ar[product == prod & spread_target == spr]
      cat(sprintf("  %-6s %-8s", prod, spr))
      for (tn in tier_order) {
        r <- sub[tier == tn]
        cat(sprintf("  %8s", if (nrow(r)) sprintf("%6.1f%%", r$hit_rate[1]*100) else "    n/a"))
      }
      cat("\n")
    }
  }
  cat("  ", strrep("─", 100), "\n")

  # ── Print RMSE grid ──────────────────────────────────────────────────────
  cat("\n══ TEST RMSE GRID — ALL_REGIMES ($/bbl)\n")
  cat(sprintf("  %-6s %-8s", "Prod", "Spread"))
  for (lb in tier_lbl) cat(sprintf("  %9s", lb))
  cat("\n  ", strrep("─", 100), "\n")
  for (prod in SFM_PRODUCTS) {
    for (spr in spreads_show) {
      sub <- ar[product == prod & spread_target == spr]
      cat(sprintf("  %-6s %-8s", prod, spr))
      for (tn in tier_order) {
        r <- sub[tier == tn]
        cat(sprintf("  %8s", if (nrow(r)) sprintf("%6.3f", r$rmse[1]) else "    n/a"))
      }
      cat("\n")
    }
  }
  cat("  ", strrep("─", 100), "\n")

  # ── Best-tier comparison ─────────────────────────────────────────────────
  cat("\n══ BEST OLD vs BEST NEW (incl. trees) ═══════════════════════════\n")
  cat(sprintf("  %-6s %-8s  %-18s %6s %6s  %-18s %6s %6s  %7s\n",
              "Prod", "Spread", "Best old tier", "Hit%", "RMSE",
              "Best new tier", "Hit%", "RMSE", "ΔHit%"))
  cat("  ", strrep("─", 96), "\n")
  setorder(comp, product, spread_target)
  for (j in seq_len(nrow(comp))) {
    r <- comp[j]
    if (!r$spread_target %in% spreads_show) next
    cat(sprintf("  %-6s %-8s  %-18s %5.1f%% %6.3f  %-18s %5.1f%% %6.3f  %+6.1f%%\n",
                r$product, r$spread_target,
                r$old_tier, r$old_hit*100, r$old_rmse,
                r$new_tier, r$new_hit*100, r$new_rmse,
                r$delta_hit*100))
  }
  cat("  ", strrep("─", 96), "\n\n")

  invisible(list(metrics = metrics_dt, test = test_dt,
                 summary = summary, comparison = comp))
}

# results <- run_spread_factor_model()
