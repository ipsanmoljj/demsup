# R/inventory_shock_model.R  (v4 — multi-estimator + OLS degree-of-freedom guard)
# ─────────────────────────────────────────────────────────────────────────────
# Runs FOUR estimator variants side-by-side and prints a cumulative comparison:
#
#   v1_ridge_all    — original: ridge/lasso, all events pooled (baseline)
#   v2_ridge_sig    — ridge/lasso, filtered to |surprise_z| >= MIN_SURPRISE_Z
#   v3_ols_sig      — OLS on significant events, ridge fallback for small-n
#                     OLS only allowed when n_features <= floor(n / 10)
#   v4_ols_sig_trim — OLS + 5% tail trim on y, significant events only
#                     Same DOF guard as v3
#
# All four variants run the same four tiers:
#   tier1_baseline | tier2_physical | tier2_freight | tier3_full
# across all products and regimes.
#
# Final table compares OOS hit% and RMSE for m1m2 (ALL_REGIMES) across variants.
#
# OUTPUTS (per variant)
#   output/ism_results_{variant}.csv
#   output/ism_oos_{variant}.csv
#   output/ism_report_{variant}.csv
#   output/ism_{variant}.rds
#   output/ism_variant_comparison.csv   <- single cross-variant table
# ─────────────────────────────────────────────────────────────────────────────


# ── Package bootstrap ─────────────────────────────────────────────────────────
.ensure_packages_shock <- function() {
  required <- c("data.table","lubridate","zoo","glmnet")
  missing  <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse=", "))
    install.packages(missing, repos="https://cloud.r-project.org", quiet=TRUE)
  }
}
.ensure_packages_shock()

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(zoo)
  library(glmnet)
})


# ── Global constants ──────────────────────────────────────────────────────────
PRODUCTS          <- c("CL","LCO","HO","LGO")
OUTPUT_DIR        <- "output"
OOS_START         <- as.Date("2026-03-01")
OOS_END           <- as.Date("2026-06-12")
EVENT_WINDOW      <- 2L       # trading days after release to measure spread change
MIN_OBS           <- 15L      # minimum events to attempt fitting
ALPHA_RIDGE       <- 0        # glmnet alpha for pure ridge
ALPHA_LASSO       <- 1        # glmnet alpha for pure lasso
SIG_THRESH        <- 0.5      # |surprise_z| threshold for hit_sig display column
MIN_SURPRISE_Z    <- 0.4      # event filter for v2/v3/v4
OLS_MIN_N         <- 30L      # minimum n to even consider OLS
OLS_DOF_RATIO     <- 10L      # OLS only when n >= n_features * OLS_DOF_RATIO
TRIM_QUANTILE     <- 0.05     # tail trim fraction for v4

UNIT_CONV <- list(CL=1.0, LCO=1.0, HO=42.0, LGO=1/7.45)


# ── Estimator variant definitions ─────────────────────────────────────────────
ESTIMATOR_VARIANTS <- list(
  v1_ridge_all    = list(filter_sig=FALSE, use_ols=FALSE, trim_y=FALSE,
                          label="Ridge/Lasso — all events (original)"),
  v2_ridge_sig    = list(filter_sig=TRUE,  use_ols=FALSE, trim_y=FALSE,
                          label="Ridge/Lasso — sig events only"),
  v3_ols_sig      = list(filter_sig=TRUE,  use_ols=TRUE,  trim_y=FALSE,
                          label="OLS (DOF-guarded) — sig events"),
  v4_ols_sig_trim = list(filter_sig=TRUE,  use_ols=TRUE,  trim_y=TRUE,
                          label="OLS + tail trim (DOF-guarded) — sig events")
)


# ── Factor tier definitions ───────────────────────────────────────────────────
FACTOR_TIERS <- list(
  tier1_baseline = c(
    "surprise_z"
  ),
  tier2_physical = c(
    "surprise_z",
    "cushing_stocks_chg_z",
    "refinery_util_dev",
    "crude_prod_chg_z",
    "crude_net_exports_z",
    "sx_cushing",
    "sx_util"
  ),
  tier2_freight = c(
    "surprise_z",
    "cushing_stocks_chg_z",
    "refinery_util_dev",
    "crude_net_exports_z",
    "td3c_z52",
    "td3c_wow_ws_z",
    "sx_cushing",
    "sx_td3c"
  ),
  tier3_full = c(
    "surprise_z",
    "cushing_stocks_chg_z",
    "refinery_util_dev",
    "crude_prod_chg_z",
    "crude_net_exports_z",
    "gasoline_stocks_chg_z",
    "distillate_stocks_chg_z",
    "rig_chg_wow_z",
    "td3c_z52",
    "td3c_wow_ws_z",
    "td3c_storage_cost_z",
    "bdi_z52",
    "hdd_dev_5yr_z",
    "cdd_us_ne",
    "cftc_mm_net_chg_z",
    "driving_season",
    "heating_season",
    "turnaround_season",
    "sin_ann",
    "cos_ann",
    "sx_cushing",
    "sx_util",
    "sx_td3c",
    "sx_cftc",
    "sx_5yr_dev"
  )
)


# ── Helpers ───────────────────────────────────────────────────────────────────
.find_repo_root <- function() {
  path <- getwd()
  for (i in seq_len(10)) {
    if (file.exists(file.path(path, ".git"))) return(path)
    parent <- dirname(path)
    if (parent == path) break
    path <- parent
  }
  getwd()
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

.train_zscore <- function(x, train_mask) {
  mu <- mean(x[train_mask], na.rm=TRUE)
  s  <- sd(x[train_mask],   na.rm=TRUE)
  if (!is.finite(s) || s < 1e-10) return(rep(0, length(x)))
  (x - mu) / s
}


# ── Data loaders ──────────────────────────────────────────────────────────────
load_spread_targets <- function(product, root) {
  fname <- file.path(root, paste0(product, "_data.csv"))
  if (!file.exists(fname)) stop("Cannot find ", fname)

  raw      <- fread(fname, skip=1L, header=TRUE)
  all_cols <- colnames(raw)

  get_mid <- function(cn) {
    idx <- which(all_cols == paste0(cn, "||weighted_mid"))
    if (length(idx) == 0)
      idx <- grep(paste0("^", cn, "\\|\\|weighted_mid$"), all_cols)
    if (length(idx) == 0) return(NULL)
    as.numeric(raw[[all_cols[idx]]])
  }

  c1 <- get_mid("c1"); c2 <- get_mid("c2")
  c3 <- get_mid("c3"); c6 <- get_mid("c6")
  if (is.null(c1) || is.null(c2)) stop("c1/c2 not found in ", fname)

  dt <- data.table(
    date = as.Date(raw[["timestamp"]]),
    m1   = as.numeric(c1),
    m2   = as.numeric(c2),
    m3   = if (!is.null(c3)) as.numeric(c3) else NA_real_,
    m6   = if (!is.null(c6)) as.numeric(c6) else NA_real_
  )
  dt <- dt[order(date)][, .SD[.N], by=date]

  u <- UNIT_CONV[[product]]
  dt[, `:=`(
    m1m2   = (m1 - m2) * u,
    m2m3   = if (any(!is.na(m3))) (m2 - m3) * u  else NA_real_,
    m1m6   = if (any(!is.na(m6))) (m1 - m6) * u  else NA_real_,
    fly123 = if (any(!is.na(m3))) (m1 - 2*m2 + m3) * u else NA_real_,
    fly136 = if (any(!is.na(m3)) && any(!is.na(m6))) (m1 - 2*m3 + m6) * u else NA_real_
  )]
  dt[, c("m1","m2","m3","m6") := NULL]
  dt
}

load_factors <- function(root) {
  for (fname in c("factors_extended.csv", "factors_combined.csv")) {
    path <- file.path(root, OUTPUT_DIR, fname)
    if (file.exists(path)) {
      message("  Factor file: ", fname)
      dt <- fread(path)
      dt[, date := as.Date(date)]
      return(dt)
    }
  }
  stop("No factor panel found. Run R/factor_loader_extended.R first.")
}

load_regime_labels <- function(product, root) {
  candidates <- c(
    paste0("regime_labels_", product, ".csv"),
    paste0("signal_",        product, ".csv"),
    paste0("signals_",       product, ".csv"),
    paste0("classifier_",    product, ".csv")
  )
  for (fname in candidates) {
    path <- file.path(root, OUTPUT_DIR, fname)
    if (file.exists(path)) {
      dt   <- fread(path)
      dt[, date := as.Date(date)]
      rcol <- intersect(c("regime_label","regime","label"), names(dt))[1]
      if (!is.na(rcol)) {
        message("  Regime file: ", fname, "  (column: ", rcol, ")")
        return(dt[, .(date, regime = get(rcol))])
      }
    }
  }
  stop("No regime labels found for ", product,
       "\n  Looked in: ", file.path(root, OUTPUT_DIR),
       "\n  Run R/regime_classifier.R first.")
}


# ── Event panel builder ───────────────────────────────────────────────────────
build_event_panel <- function(spreads, factors, regimes, event_window) {
  fac_ord <- copy(factors)[order(date)]

  # Derive change columns if only level columns exist
  derive_chg <- function(dt, chg_col, lvl_col) {
    if (!chg_col %in% names(dt) && lvl_col %in% names(dt))
      dt[, (chg_col) := get(lvl_col) - shift(get(lvl_col), 1)]
  }
  derive_chg(fac_ord, "crude_stocks_chg",      "crude_stocks_kb")
  derive_chg(fac_ord, "gasoline_stocks_chg",   "gasoline_stocks_kb")
  derive_chg(fac_ord, "distillate_stocks_chg", "distillate_stocks_kb")
  derive_chg(fac_ord, "cushing_stocks_chg",    "cushing_stocks_kb")
  derive_chg(fac_ord, "crude_prod_chg",        "crude_prod_kbd")

  if ("crude_net_exports_kbd" %in% names(fac_ord) &&
      !"crude_net_exports_z"  %in% names(fac_ord))
    fac_ord[, crude_net_exports_z_raw := crude_net_exports_kbd]

  inv <- fac_ord[weekdays(date) == "Wednesday" & !is.na(crude_stocks_chg)]
  if (nrow(inv) == 0) {
    inv <- fac_ord[weekdays(date) == "Wednesday"]
    if (nrow(inv) == 0) stop("No Wednesday observations found")
    message("  WARN: crude_stocks_chg missing — using all Wednesdays")
  }

  inv <- merge(inv, regimes, by="date", all.x=TRUE)
  train_mask <- inv$date < OOS_START

  # Z-score raw columns on training window only
  raw_to_z <- list(
    cushing_stocks_chg_z    = "cushing_stocks_chg",
    crude_prod_chg_z        = "crude_prod_chg",
    gasoline_stocks_chg_z   = "gasoline_stocks_chg",
    distillate_stocks_chg_z = "distillate_stocks_chg",
    crude_net_exports_z     = "crude_net_exports_kbd",
    rig_chg_wow_z           = "rig_chg_wow",
    hdd_dev_5yr_z           = "hdd_dev_5yr",
    cftc_mm_net_chg_z       = "cftc_mm_net_chg",
    td3c_wow_ws_z           = "td3c_wow_ws",
    td3c_storage_cost_z     = "td3c_storage_cost_bbl_mo"
  )
  for (z_col in names(raw_to_z)) {
    raw_col <- raw_to_z[[z_col]]
    if (raw_col %in% names(inv))
      inv[, (z_col) := .train_zscore(get(raw_col), train_mask)]
    else
      inv[, (z_col) := 0]
  }

  # Ensure always-present columns
  for (col in c("td3c_z52","bdi_z52"))
    if (!col %in% names(inv)) inv[, (col) := 0]

  # Surprise proxy
  if ("crude_stocks_surprise" %in% names(inv)) {
    inv[, surprise_z := .train_zscore(crude_stocks_surprise, train_mask)]
  } else {
    inv[, surprise_z := .train_zscore(crude_stocks_chg, train_mask)]
    message("  NOTE: using crude_stocks_chg as surprise proxy (no consensus column)")
  }

  # Seasonal / weather dummies
  for (col in c("sin_ann","cos_ann","driving_season","heating_season",
                "turnaround_season","cdd_us_ne"))
    if (!col %in% names(inv)) inv[, (col) := 0]

  if (!"refinery_util_dev" %in% names(inv)) inv[, refinery_util_dev := 0]

  # Interaction terms
  inv[, sx_cushing := surprise_z * cushing_stocks_chg_z]
  inv[, sx_util    := surprise_z * refinery_util_dev]
  inv[, sx_td3c    := surprise_z * td3c_z52]
  inv[, sx_cftc    := surprise_z * cftc_net_mm_zscore]

  if ("crude_stocks_5yr_dev" %in% names(inv)) {
    inv[, crude_stocks_5yr_dev_z := .train_zscore(crude_stocks_5yr_dev, train_mask)]
    inv[, sx_5yr_dev := surprise_z * crude_stocks_5yr_dev_z]
  } else {
    inv[, sx_5yr_dev := 0]
  }

  for (icol in c("sx_cushing","sx_util","sx_td3c","sx_cftc","sx_5yr_dev"))
    inv[!is.finite(get(icol)), (icol) := 0]

  # Compute spread changes around each event
  spread_targets <- c("m1m2","m2m3","m1m6","fly123","fly136")
  spread_dt      <- spreads[, c("date", spread_targets), with=FALSE]

  rbindlist(lapply(seq_len(nrow(inv)), function(i) {
    rel_date <- inv$date[i]
    idx_pre  <- which(spread_dt$date <= rel_date)
    idx_post <- which(spread_dt$date >  rel_date)
    if (length(idx_pre) == 0 || length(idx_post) < event_window) return(NULL)
    pre  <- spread_dt[tail(idx_pre, 1)]
    post <- spread_dt[idx_post[event_window]]
    row  <- copy(inv[i])
    for (tgt in spread_targets)
      row[, paste0("d_", tgt) := post[[tgt]] - pre[[tgt]]]
    row
  }), fill=TRUE)
}


# ── Core fitting function — variant-aware with DOF guard ─────────────────────
fit_single_model <- function(sub, tier_cols, y_col, variant, n_min=MIN_OBS) {
  if (!y_col %in% names(sub)) return(NULL)

  # Event filter for sig variants
  if (variant$filter_sig && "surprise_z" %in% names(sub))
    sub <- sub[abs(surprise_z) >= MIN_SURPRISE_Z]

  y    <- sub[[y_col]]
  X_dt <- sub[, tier_cols, with=FALSE]
  for (col in names(X_dt))
    set(X_dt, which(!is.finite(X_dt[[col]])), col, 0)
  X    <- as.matrix(X_dt)
  keep <- is.finite(y) & apply(is.finite(X), 1, all)
  if (sum(keep) < n_min) return(NULL)

  Xk <- X[keep, , drop=FALSE]; yk <- y[keep]; n <- nrow(Xk); p <- ncol(Xk)

  # DOF guard: OLS only when well-identified
  ols_allowed <- variant$use_ols &&
                 n >= OLS_MIN_N  &&
                 p <= floor(n / OLS_DOF_RATIO)

  # ── OLS path ────────────────────────────────────────────────────────────────
  if (ols_allowed) {

    if (variant$trim_y) {
      q_lo <- quantile(yk, TRIM_QUANTILE)
      q_hi <- quantile(yk, 1 - TRIM_QUANTILE)
      trim <- yk >= q_lo & yk <= q_hi
      if (sum(trim) >= n_min) {
        Xk <- Xk[trim, , drop=FALSE]; yk <- yk[trim]; n <- nrow(Xk)
      }
    }

    fit_lm <- tryCatch(lm.fit(cbind(1, Xk), yk), error=function(e) NULL)
    if (is.null(fit_lm)) return(NULL)

    y_hat  <- as.numeric(cbind(1, Xk) %*% fit_lm$coefficients)
    ss_r   <- sum((yk - y_hat)^2)
    ss_t   <- sum((yk - mean(yk))^2)
    r2_is  <- if (ss_t > 0) 1 - ss_r / ss_t else NA_real_
    r2_adj <- if (!is.na(r2_is) && n > p + 1)
                1 - (1 - r2_is) * (n - 1) / (n - p - 1)
              else NA_real_
    rmse    <- sqrt(mean((yk - y_hat)^2))
    hit     <- mean(sign(y_hat) == sign(yk), na.rm=TRUE)
    sig     <- if ("surprise_z" %in% colnames(Xk))
                 abs(Xk[, "surprise_z"]) > SIG_THRESH
               else rep(TRUE, n)
    hit_sig <- if (sum(sig) >= 5)
                 mean(sign(y_hat[sig]) == sign(yk[sig]), na.rm=TRUE)
               else NA_real_

    coefs    <- fit_lm$coefficients
    coef_row <- as.data.table(t(coefs))
    setnames(coef_row, paste0("coef_", c("intercept", colnames(Xk))))

    return(list(
      n_events    = n,
      r2_insample = round(r2_is,  4),
      r2_cv       = round(r2_adj, 4),   # adj-R² in r2_cv slot for consistent display
      rmse_bbl    = round(rmse,   4),
      hit_rate    = round(hit,    4),
      hit_sig     = round(hit_sig,4),
      n_sig       = sum(sig),
      coef_row    = coef_row,
      alpha_used  = NA_real_,
      estimator   = "OLS"
    ))
  }

  # ── Ridge / lasso path (default + OLS fallback when DOF guard fires) ────────
  alpha_use <- if (n >= 40) ALPHA_LASSO else ALPHA_RIDGE
  nfolds    <- min(5, max(3, floor(n / 4)))

  cv_fit <- tryCatch(
    cv.glmnet(Xk, yk, alpha=alpha_use, nfolds=nfolds),
    error=function(e) NULL
  )
  if (is.null(cv_fit)) return(NULL)

  fit    <- glmnet(Xk, yk, alpha=alpha_use, lambda=cv_fit$lambda.min)
  y_hat  <- as.numeric(predict(fit, Xk))
  ss_r   <- sum((yk - y_hat)^2)
  ss_t   <- sum((yk - mean(yk))^2)
  r2_is  <- if (ss_t > 0) 1 - ss_r / ss_t else NA_real_
  r2_cv  <- tryCatch(
    1 - min(cv_fit$cvm) / mean((yk - mean(yk))^2),
    error=function(e) NA_real_
  )
  rmse   <- sqrt(mean((yk - y_hat)^2))
  hit    <- mean(sign(y_hat) == sign(yk), na.rm=TRUE)
  sig    <- if ("surprise_z" %in% colnames(Xk))
              abs(Xk[, "surprise_z"]) > SIG_THRESH
            else rep(TRUE, n)
  hit_sig <- if (sum(sig) >= 5)
               mean(sign(y_hat[sig]) == sign(yk[sig]), na.rm=TRUE)
             else NA_real_

  coefs    <- as.numeric(coef(fit))
  coef_row <- as.data.table(t(coefs))
  setnames(coef_row, paste0("coef_", c("intercept", colnames(Xk))))

  list(
    n_events    = n,
    r2_insample = round(r2_is, 4),
    r2_cv       = round(r2_cv, 4),
    rmse_bbl    = round(rmse,  4),
    hit_rate    = round(hit,   4),
    hit_sig     = round(hit_sig, 4),
    n_sig       = sum(sig),
    coef_row    = coef_row,
    alpha_used  = alpha_use,
    estimator   = if (alpha_use == 0) "Ridge" else "Lasso"
  )
}


# ── Fit all tiers × regimes for one variant ───────────────────────────────────
fit_models_all_tiers <- function(events_train, product, variant) {
  spread_targets <- c("m1m2","m2m3","m1m6","fly123","fly136")
  regimes        <- unique(events_train$regime)
  regimes        <- regimes[!is.na(regimes)]
  regime_list    <- c(as.list(regimes), list("ALL_REGIMES"))

  rbindlist(lapply(regime_list, function(reg) {
    sub <- if (identical(reg, "ALL_REGIMES")) events_train
           else events_train[regime == reg]
    if (nrow(sub) < MIN_OBS) return(NULL)

    rbindlist(lapply(names(FACTOR_TIERS), function(tier_name) {
      tier_cols <- FACTOR_TIERS[[tier_name]]
      tier_cols <- tier_cols[tier_cols %in% names(sub)]
      if (length(tier_cols) == 0) return(NULL)

      rbindlist(lapply(spread_targets, function(tgt) {
        result <- fit_single_model(sub, tier_cols, paste0("d_", tgt), variant)
        if (is.null(result)) return(NULL)
        cbind(
          data.table(
            product     = product,
            regime      = as.character(reg),
            tier        = tier_name,
            spread_target = tgt,
            estimator   = result$estimator,
            n_events    = result$n_events,
            r2_insample = result$r2_insample,
            r2_cv       = result$r2_cv,
            rmse_bbl    = result$rmse_bbl,
            hit_rate    = result$hit_rate,
            hit_sig     = result$hit_sig,
            n_sig       = result$n_sig,
            alpha_used  = result$alpha_used
          ),
          result$coef_row
        )
      }), fill=TRUE)
    }), fill=TRUE)
  }), fill=TRUE)
}


# ── OOS evaluation — variant-aware ────────────────────────────────────────────
evaluate_oos <- function(events_all, models, product, variant) {
  oos <- events_all[date >= OOS_START & date <= OOS_END]
  if (nrow(oos) == 0) return(data.table())

  spread_targets <- c("m1m2","m2m3","m1m6","fly123","fly136")

  rbindlist(lapply(seq_len(nrow(oos)), function(i) {
    ev <- oos[i]

    # Skip below-threshold events for sig-filtered variants
    if (variant$filter_sig && "surprise_z" %in% names(ev) &&
        abs(ev$surprise_z) < MIN_SURPRISE_Z) return(NULL)

    reg <- if (is.na(ev$regime)) "ALL_REGIMES" else ev$regime

    rbindlist(lapply(spread_targets, function(tgt) {
      y_col <- paste0("d_", tgt)
      if (!y_col %in% names(ev)) return(NULL)
      y_act <- ev[[y_col]]
      if (!is.finite(y_act)) return(NULL)

      rbindlist(lapply(names(FACTOR_TIERS), function(tier_name) {
        tier_cols <- FACTOR_TIERS[[tier_name]]
        tier_cols <- tier_cols[tier_cols %in% names(ev)]
        if (length(tier_cols) == 0) return(NULL)

        # Look up regime-specific model, fall back to ALL_REGIMES
        # prod_key avoids data.table comparing column 'product' to itself
        prod_key <- product
        mod <- models[product == prod_key & spread_target == tgt &
                        tier == tier_name & regime == reg]
        if (nrow(mod) == 0)
          mod <- models[product == prod_key & spread_target == tgt &
                          tier == tier_name & regime == "ALL_REGIMES"]
        if (nrow(mod) == 0) return(NULL)

        feat_vals     <- as.numeric(ev[, tier_cols, with=FALSE])
        feat_vals[!is.finite(feat_vals)] <- 0
        coef_cols     <- intersect(paste0("coef_", tier_cols), names(mod))
        coef_vals     <- as.numeric(mod[1, coef_cols, with=FALSE])
        intercept_val <- if ("coef_intercept" %in% names(mod))
                           as.numeric(mod[["coef_intercept"]][1]) else 0
        y_pred <- intercept_val + sum(feat_vals * coef_vals, na.rm=TRUE)

        data.table(
          product       = product,
          date          = ev$date,
          regime        = reg,
          tier          = tier_name,
          spread_target = tgt,
          surprise_z    = round(ev$surprise_z, 3),
          y_actual      = round(y_act,  4),
          y_pred        = round(y_pred, 4),
          correct_sign  = (sign(y_pred) == sign(y_act)),
          abs_error     = round(abs(y_pred - y_act), 4),
          error         = round(y_pred - y_act, 4)
        )
      }), fill=TRUE)
    }), fill=TRUE)
  }), fill=TRUE)
}


# ── OOS summariser ────────────────────────────────────────────────────────────
summarise_oos <- function(oos) {
  if (nrow(oos) == 0) return(data.table())
  per_regime <- oos[, .(
    n_events = .N,
    hit_rate = round(mean(correct_sign, na.rm=TRUE), 4),
    rmse     = round(sqrt(mean(error^2, na.rm=TRUE)), 4),
    mae      = round(mean(abs_error,    na.rm=TRUE), 4)
  ), by=.(product, tier, spread_target, regime)]

  all_reg <- oos[, .(
    regime   = "ALL_REGIMES",
    n_events = .N,
    hit_rate = round(mean(correct_sign, na.rm=TRUE), 4),
    rmse     = round(sqrt(mean(error^2, na.rm=TRUE)), 4),
    mae      = round(mean(abs_error,    na.rm=TRUE), 4)
  ), by=.(product, tier, spread_target)]

  rbindlist(list(per_regime, all_reg), fill=TRUE)
}


# ── Report builder ────────────────────────────────────────────────────────────
build_report <- function(models, oos_summary) {
  if (nrow(models) == 0) return(data.table())
  cols <- intersect(
    c("product","regime","tier","spread_target","estimator","n_events",
      "r2_cv","r2_insample","rmse_bbl","hit_rate","hit_sig","n_sig"),
    names(models)
  )
  base <- models[, cols, with=FALSE]
  if (nrow(oos_summary) > 0) {
    oos_col <- oos_summary[, .(product, tier, spread_target, regime,
                                oos_hit_rate = hit_rate,
                                oos_n        = n_events,
                                oos_rmse     = rmse)]
    base <- merge(base, oos_col,
                  by=c("product","tier","spread_target","regime"), all.x=TRUE)
  }
  base[order(product, spread_target, tier, regime)]
}


# ── Main runner ───────────────────────────────────────────────────────────────
run_inventory_shock_model <- function(data_dir=NULL) {
  root <- if (!is.null(data_dir)) data_dir else .find_repo_root()
  if (!dir.exists(file.path(root, OUTPUT_DIR)))
    dir.create(file.path(root, OUTPUT_DIR), recursive=TRUE)

  message("══ Inventory Shock Model v4 (multi-estimator + DOF guard) ══════")
  message("Repo root    : ", root)
  message("OOS window   : ", OOS_START, " to ", OOS_END)
  message("Variants     : ", paste(names(ESTIMATOR_VARIANTS), collapse=" | "))
  message("OLS DOF rule : n_features <= floor(n / ", OLS_DOF_RATIO, ")")

  message("\n[1] Loading factors...")
  factors <- load_factors(root)
  message("  Rows: ", nrow(factors),
          " | ", min(factors$date), " to ", max(factors$date))

  all_results <- lapply(names(ESTIMATOR_VARIANTS), function(v)
    list(models=list(), oos=list(), events=list()))
  names(all_results) <- names(ESTIMATOR_VARIANTS)

  for (product in PRODUCTS) {
    message("\n── ", product,
            " ──────────────────────────────────────────────")

    spreads <- tryCatch(
      load_spread_targets(product, root),
      error=function(e) { message("  SKIP spreads: ", e$message); NULL }
    )
    if (is.null(spreads)) next

    regimes <- tryCatch(
      load_regime_labels(product, root),
      error=function(e) { message("  SKIP regimes: ", e$message); NULL }
    )
    if (is.null(regimes)) next

    message("  Building event panel...")
    events <- tryCatch(
      build_event_panel(spreads, factors, regimes, EVENT_WINDOW),
      error=function(e) { message("  SKIP events: ", e$message); NULL }
    )
    if (is.null(events) || nrow(events) == 0) { message("  No events."); next }

    n_tr  <- nrow(events[date < OOS_START])
    n_oo  <- nrow(events[date >= OOS_START & date <= OOS_END])
    n_sig <- nrow(events[date >= OOS_START & date <= OOS_END &
                           abs(surprise_z) >= MIN_SURPRISE_Z])
    message(sprintf("  Events: train=%d  OOS=%d  (sig OOS: %d)", n_tr, n_oo, n_sig))

    for (vname in names(ESTIMATOR_VARIANTS)) {
      v <- ESTIMATOR_VARIANTS[[vname]]
      message(sprintf("  [%s] fitting...", vname))

      models <- tryCatch(
        fit_models_all_tiers(events[date < OOS_START], product, v),
        error=function(e) { message("    ERR fit: ", e$message); NULL }
      )
      if (is.null(models) || nrow(models) == 0) next
      all_results[[vname]]$models[[product]] <- models

      # Print per-variant estimator usage summary
      if ("estimator" %in% names(models)) {
        est_tbl <- models[regime=="ALL_REGIMES" & spread_target=="m1m2",
                          .N, by=.(tier, estimator)]
        for (k in seq_len(nrow(est_tbl)))
          message(sprintf("    %s / %s → %s",
                          est_tbl$tier[k], est_tbl$estimator[k],
                          ifelse(est_tbl$estimator[k]=="OLS",
                                 "OLS (DOF OK)", "Ridge/Lasso (shrinkage)")))
      }

      oos <- tryCatch(
        evaluate_oos(events, models, product, v),
        error=function(e) { message("    ERR oos: ", e$message); data.table() }
      )
      if (nrow(oos) > 0) {
        all_results[[vname]]$oos[[product]]    <- oos
        all_results[[vname]]$events[[product]] <- events
        oos3 <- oos[spread_target == "m1m2" & tier == "tier3_full"]
        if (nrow(oos3) > 0) {
          hit  <- mean(oos3$correct_sign, na.rm=TRUE)
          rmse <- sqrt(mean(oos3$error^2, na.rm=TRUE))
          message(sprintf("    tier3 m1m2 OOS: hit=%.1f%%  RMSE=%.3f  n=%d",
                          hit * 100, rmse, nrow(oos3)))
        }
      }
    }
  }

  # ── Consolidate and save ────────────────────────────────────────────────────
  message("\n── Saving outputs ──────────────────────────────────────────────")

  saved <- list()
  for (vname in names(ESTIMATOR_VARIANTS)) {
    models_dt <- rbindlist(all_results[[vname]]$models, fill=TRUE)
    oos_dt    <- rbindlist(all_results[[vname]]$oos,    fill=TRUE)
    oos_sum   <- summarise_oos(oos_dt)
    report    <- build_report(models_dt, oos_sum)

    fwrite(models_dt, file.path(root, OUTPUT_DIR, paste0("ism_results_", vname, ".csv")))
    fwrite(oos_dt,    file.path(root, OUTPUT_DIR, paste0("ism_oos_",     vname, ".csv")))
    fwrite(report,    file.path(root, OUTPUT_DIR, paste0("ism_report_",  vname, ".csv")))
    saveRDS(
      list(models=models_dt, oos=oos_dt, summary=oos_sum, report=report),
      file.path(root, OUTPUT_DIR, paste0("ism_", vname, ".rds"))
    )
    saved[[vname]] <- list(models=models_dt, oos=oos_dt,
                           summary=oos_sum, report=report)
    message("  Saved: ism_*_", vname, ".csv + .rds")
  }

  # ── Cumulative comparison table ─────────────────────────────────────────────
  cat("\n")
  cat("══ CUMULATIVE ESTIMATOR COMPARISON — m1m2, ALL_REGIMES ═════════\n")
  cat(sprintf("  %-20s %-6s %-22s %-8s  %4s  %5s  %6s  %7s\n",
              "Variant","Prod","Tier","Estimtr","N","Hit%","AdjR2","RMSE"))
  cat("  ", strrep("─", 80), "\n")

  comp_rows <- rbindlist(lapply(names(ESTIMATOR_VARIANTS), function(vname) {
    sm <- saved[[vname]]$summary
    if (nrow(sm) == 0) return(NULL)
    sm_m1 <- sm[spread_target == "m1m2" & regime == "ALL_REGIMES"]
    if (nrow(sm_m1) == 0) return(NULL)

    mod      <- saved[[vname]]$models
    if (nrow(mod) == 0) return(NULL)
    r2_look  <- mod[regime == "ALL_REGIMES" & spread_target == "m1m2",
                    .(product, tier, r2_cv,
                      estimator = if ("estimator" %in% names(mod))
                                    estimator else NA_character_)]

    merge(
      sm_m1[, .(product, tier, n_oos=n_events, hit_rate, rmse)],
      r2_look, by=c("product","tier"), all.x=TRUE
    )[, variant := vname]
  }), fill=TRUE)

  if (nrow(comp_rows) > 0) {
    comp_rows <- comp_rows[order(product, tier, variant)]
    prev_prod <- ""; prev_tier <- ""
    for (j in seq_len(nrow(comp_rows))) {
      r        <- comp_rows[j]
      sep_prod <- r$product != prev_prod
      if (sep_prod && j > 1) cat("  ", strrep("·", 80), "\n")
      prev_prod <- r$product; prev_tier <- r$tier
      r2_str   <- if (is.na(r$r2_cv)) "    —  " else
                    formatC(r$r2_cv, 4, format="f")
      est_str  <- if (is.null(r$estimator) || is.na(r$estimator)) "—"
                  else as.character(r$estimator)
      cat(sprintf("  %-20s %-6s %-22s %-8s  %4d  %4.1f%%  %s  %7.4f\n",
                  r$variant, r$product, r$tier, est_str,
                  r$n_oos, r$hit_rate * 100, r2_str, r$rmse))
    }
  }
  cat("  ", strrep("─", 80), "\n\n")

  fwrite(comp_rows, file.path(root, OUTPUT_DIR, "ism_variant_comparison.csv"))
  message("  Saved: ism_variant_comparison.csv")
  message("══ Complete ════════════════════════════════════════════════")
  invisible(saved)
}


# ── Convenience reload ────────────────────────────────────────────────────────
reload_results <- function(variant="v2_ridge_sig", root=NULL) {
  if (is.null(root)) root <- .find_repo_root()
  rds <- file.path(root, OUTPUT_DIR, paste0("ism_", variant, ".rds"))
  if (!file.exists(rds))
    stop("File not found: ", rds, "\nRun run_inventory_shock_model() first.")
  readRDS(rds)
}

# results <- run_inventory_shock_model()