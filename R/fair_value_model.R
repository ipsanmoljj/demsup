# R/fair_value_model.R
# --------------------
# Regime-conditional elastic net fair value model for M1M2 spreads.
#
# DATA SPLIT (strictly enforced — no leakage):
#   Training   : warm-up end → Dec 2023  (coefficients estimated here only)
#   Validation : Jan 2024 → Jun 2024     (signal threshold tuned here only)
#   Test       : Jul 2024 → May 2026     (opened once, final performance only)
#
# MODEL:
#   One elastic net per regime class, fitted on training data only.
#   Coefficients frozen after training; applied forward to validation + test.
#   Signal threshold (|dev_z| cutoff) tuned on validation IC, not test.
#
# Outputs:
#   output/fair_value_{PRODUCT}.csv         — full panel with predictions
#   output/model_coefficients_{PRODUCT}.csv — elastic net coefficients
#   output/backtest_{PRODUCT}.csv           — test-window trade log
#
# Usage:
#   source("R/fair_value_model.R")
#   result <- run_fair_value_model(product = "CL")
#   print(result$gof_train)     # in-sample goodness of fit
#   print(result$gof_val)       # validation window performance
#   print(result$gof_test)      # test window performance (final)
#   print(result$signal_table)  # all signals generated

suppressPackageStartupMessages({
  library(data.table)
  library(glmnet)
  library(zoo)
})

# ── Split boundaries (never change these after model development begins) ───────
TRAIN_END <- as.Date("2023-12-31")   # training window end
VAL_END   <- as.Date("2024-06-30")   # validation window end
# Test window: VAL_END + 1 → end of data

# ── Model parameters ──────────────────────────────────────────────────────────
MODEL_PARAMS <- list(
  alpha            = 0.5,    # elastic net (0=ridge, 1=lasso, 0.5=balanced)
  n_folds          = 5L,     # time-series block CV folds
  min_regime_obs   = 25L,    # minimum training obs to fit a regime model
  dev_zscore_window = 63L,   # rolling window for deviation z-score (training only)
  # Signal thresholds — candidates evaluated on validation window
  signal_z_candidates = c(1.0, 1.25, 1.5, 1.75, 2.0)
)

# ── Factor columns ─────────────────────────────────────────────────────────────
FACTOR_COLS <- c(
  "sofr", "crude_stocks_kb", "cushing_stocks_kb",
  "distillate_stocks_kb", "brent_spot",
  "crude_stocks_surprise", "refinery_util_pct", "refinery_util_dev",
  "gasoil_crack_proxy", "ho_crack_proxy",
  "cftc_net_mm_zscore", "dxy_4wk_chg",
  "crude_prod_kbd", "gasoline_stocks_kb", "gasoil_crack_dev"
)

MODELLED_REGIMES <- c(
  "Deep-Backwardation", "Backwardation-Deficit", "Easing-Backwardation",
  "Stable-Elevated", "Easing-Contango", "Contango-Surplus", "Deep-Contango"
)

# ── Time-series block CV ───────────────────────────────────────────────────────
.ts_block_cv <- function(n, k = 5L) {
  block_size <- floor(n / (k + 1L))
  if (block_size < 5L) return(list())
  lapply(seq_len(k), function(i) {
    train_end <- i * block_size
    val_start <- train_end + 1L
    val_end   <- min(val_start + block_size - 1L, n)
    if (val_start > n) return(NULL)
    list(train = seq_len(train_end), val = seq(val_start, val_end))
  }) |> Filter(Negate(is.null), x = _)
}

# ── Impute NA columns with median ─────────────────────────────────────────────
.impute_median <- function(X) {
  for (j in seq_len(ncol(X))) {
    na_idx <- is.na(X[, j])
    if (any(na_idx) && any(!na_idx))
      X[na_idx, j] <- median(X[!na_idx, j], na.rm = TRUE)
    else if (all(na_idx))
      X[, j] <- 0
  }
  X
}

# ── Fit elastic net on training data ──────────────────────────────────────────
.fit_regime_model <- function(X, y, alpha = 0.5, n_folds = 5L) {
  n <- nrow(X)
  if (n < MODEL_PARAMS$min_regime_obs) return(NULL)
  if (length(unique(y)) < 3) return(NULL)

  # Remove zero-variance columns
  col_var <- apply(X, 2, var, na.rm = TRUE)
  keep    <- col_var > 1e-10
  if (!any(keep)) return(NULL)
  X <- X[, keep, drop = FALSE]

  # Scale X
  X_means <- colMeans(X, na.rm = TRUE)
  X_sds   <- pmax(apply(X, 2, sd, na.rm = TRUE), 1e-8)
  X_sc    <- scale(X, center = X_means, scale = X_sds)
  X_sc[is.nan(X_sc) | is.infinite(X_sc)] <- 0

  # Time-series block CV for lambda selection
  folds <- .ts_block_cv(n, k = min(n_folds, floor(n / 10L)))

  lambda_opt <- NULL
  cv_mse     <- NA_real_

  if (length(folds) >= 2) {
    cv_dt <- rbindlist(lapply(folds, function(fold) {
      if (length(fold$train) < 10 || length(fold$val) < 5) return(NULL)
      fit_cv <- tryCatch(
        glmnet(X_sc[fold$train, , drop=FALSE], y[fold$train],
               alpha = alpha, standardize = FALSE),
        error = function(e) NULL
      )
      if (is.null(fit_cv)) return(NULL)
      pred <- predict(fit_cv, newx = X_sc[fold$val, , drop=FALSE],
                      s = fit_cv$lambda)
      mse  <- colMeans((pred - y[fold$val])^2, na.rm = TRUE)
      data.table(lambda = fit_cv$lambda, mse = mse)
    }), fill = TRUE)

    if (nrow(cv_dt) > 0) {
      lam_summary <- cv_dt[, .(mean_mse = mean(mse, na.rm=TRUE)), by=lambda]
      lambda_opt  <- lam_summary[which.min(mean_mse), lambda]
      cv_mse      <- lam_summary[which.min(mean_mse), mean_mse]
    }
  }

  # Final fit on all training data
  fit_final <- tryCatch(
    glmnet(X_sc, y, alpha = alpha, standardize = FALSE),
    error = function(e) NULL
  )
  if (is.null(fit_final)) return(NULL)
  if (is.null(lambda_opt)) lambda_opt <- fit_final$lambda[ceiling(length(fit_final$lambda)/2)]

  list(
    fit       = fit_final,
    lambda    = lambda_opt,
    X_means   = X_means,
    X_sds     = X_sds,
    col_names = colnames(X),
    n_obs     = n,
    cv_mse    = round(cv_mse, 6)
  )
}

# ── Predict from fitted model ──────────────────────────────────────────────────
.predict_model <- function(model, X_new) {
  if (is.null(model) || nrow(X_new) == 0)
    return(rep(NA_real_, nrow(X_new)))

  cols <- model$col_names
  X_sub <- matrix(0, nrow=nrow(X_new), ncol=length(cols),
                   dimnames=list(NULL, cols))
  avail <- intersect(cols, colnames(X_new))
  if (length(avail) > 0) X_sub[, avail] <- X_new[, avail, drop=FALSE]

  X_sc <- scale(X_sub, center=model$X_means[cols], scale=model$X_sds[cols])
  X_sc[is.nan(X_sc) | is.infinite(X_sc)] <- 0

  as.numeric(predict(model$fit, newx=X_sc, s=model$lambda))
}

# ── Extract coefficients ───────────────────────────────────────────────────────
.extract_coefs <- function(model, regime) {
  if (is.null(model)) return(NULL)
  cm <- coef(model$fit, s=model$lambda)
  data.table(
    regime      = regime,
    variable    = rownames(cm),
    coefficient = as.numeric(cm),
    active      = abs(as.numeric(cm)) > 1e-10,
    n_obs       = model$n_obs,
    cv_mse      = model$cv_mse,
    lambda      = model$lambda
  )[variable != "(Intercept)"]
}

# ── Goodness-of-fit metrics ────────────────────────────────────────────────────
.gof_metrics <- function(dt_window, window_name) {
  cat(sprintf("\n--- GOF: %s ---\n\n", window_name))
  cat(sprintf("  %-25s %6s %7s %7s %7s %7s %7s\n",
              "Regime", "N", "R²", "RMSE", "MAE",
              "IC", "Signals"))
  cat("  ", strrep("-", 75), "\n")

  gof_rows <- list()
  for (regime in MODELLED_REGIMES) {
    sub <- dt_window[model_regime == regime & !is.na(fair_value)]
    if (nrow(sub) < 5) next

    y    <- sub$M1M2
    yhat <- sub$fair_value
    dev  <- sub$deviation

    ss_tot <- sum((y - mean(y))^2, na.rm=TRUE)
    ss_res <- sum((y - yhat)^2,    na.rm=TRUE)
    r2     <- round(1 - ss_res/ss_tot, 3)
    rmse   <- round(sqrt(mean((y-yhat)^2, na.rm=TRUE)), 4)
    mae    <- round(mean(abs(y-yhat),     na.rm=TRUE), 4)
    n_sig  <- sum(sub$signal != "FLAT", na.rm=TRUE)

    # Information coefficient: rank corr between dev_zscore and next-bar M1M2 change
    if (nrow(sub) > 10 && "dev_zscore" %in% names(sub)) {
      next_chg <- c(diff(sub$M1M2), NA)
      ic <- round(cor(sub$dev_zscore, next_chg, use="complete.obs",
                      method="spearman"), 3)
    } else ic <- NA_real_

    cat(sprintf("  %-25s %6d %7.3f %7.4f %7.4f %7.3f %7d\n",
                regime, nrow(sub), r2, rmse, mae,
                ifelse(is.na(ic), 0, ic), n_sig))

    gof_rows[[regime]] <- data.table(
      window=window_name, regime=regime, n=nrow(sub),
      r2=r2, rmse=rmse, mae=mae, ic=ic, n_signals=n_sig
    )
  }
  rbindlist(gof_rows, fill=TRUE)
}

# ── Hit rate and IC by threshold (validation only) ────────────────────────────
.tune_threshold <- function(val_dt) {
  cat("\n--- THRESHOLD TUNING (validation window) ---\n\n")
  cat(sprintf("  %-10s %8s %8s %8s %8s\n",
              "Threshold", "N_signals", "Hit_rate", "Avg_IC", "Sharpe_proxy"))
  cat("  ", strrep("-", 50), "\n")

  best_thresh <- 1.5
  best_ic     <- -Inf

  for (thresh in MODEL_PARAMS$signal_z_candidates) {
    sig_rows <- val_dt[!is.na(dev_zscore) & abs(dev_zscore) > thresh]
    if (nrow(sig_rows) < 5) next

    # Direction: BUY when z < -thresh (spread below fair → expect reversion up)
    sig_rows[, direction := ifelse(dev_zscore < 0, 1L, -1L)]
    # 5-bar forward return
    val_dt_sorted <- val_dt[order(date)]
    sig_rows[, fwd_return := sapply(date, function(d) {
      idx <- which(val_dt_sorted$date == d)
      if (length(idx)==0 || idx+5 > nrow(val_dt_sorted)) return(NA_real_)
      val_dt_sorted$M1M2[idx+5] - val_dt_sorted$M1M2[idx]
    })]

    sig_rows <- sig_rows[!is.na(fwd_return)]
    if (nrow(sig_rows) < 5) next

    pnl       <- sig_rows$direction * sig_rows$fwd_return
    hit_rate  <- round(mean(pnl > 0, na.rm=TRUE), 3)
    ic        <- round(cor(sig_rows$dev_zscore, sig_rows$fwd_return,
                            use="complete.obs", method="spearman"), 3)
    sharpe_p  <- round(mean(pnl, na.rm=TRUE) /
                         (sd(pnl, na.rm=TRUE) + 1e-8), 3)

    cat(sprintf("  %-10.2f %8d %8.3f %8.3f %8.3f\n",
                thresh, nrow(sig_rows), hit_rate,
                ifelse(is.na(ic), 0, ic), sharpe_p))

    if (!is.na(ic) && ic > best_ic) {
      best_ic     <- ic
      best_thresh <- thresh
    }
  }

  cat(sprintf("\n  Selected threshold: %.2f  (best validation IC: %.3f)\n",
              best_thresh, best_ic))
  best_thresh
}

# ═════════════════════════════════════════════════════════════════════════════
# MAIN FUNCTION
# ═════════════════════════════════════════════════════════════════════════════

run_fair_value_model <- function(product      = "CL",
                                  output_dir   = "output",
                                  factors_path = "output/factors_combined.csv",
                                  verbose      = TRUE) {

  cat("\n", strrep("=", 60), "\n")
  cat("FAIR VALUE MODEL —", product, "\n")
  cat(strrep("=", 60), "\n")
  cat(sprintf("  Train:      start → %s\n", format(TRAIN_END)))
  cat(sprintf("  Validation: %s → %s\n",
              format(TRAIN_END + 1), format(VAL_END)))
  cat(sprintf("  Test:       %s → end of data\n\n",
              format(VAL_END + 1)))

  # ── Load data ──────────────────────────────────────────────────────────────
  labels_path <- file.path(output_dir, paste0("regime_labels_", product, ".csv"))
  if (!file.exists(labels_path))
    stop("Run classify_regimes('", product, "') first.")
  if (!file.exists(factors_path))
    stop("Run load_all_factors() first.")

  labels  <- fread(labels_path)
  factors <- fread(factors_path)
  labels[,  date := as.Date(date)]
  factors[, date := as.Date(date)]

  dt <- merge(
    labels[, .(date, M1M2, regime_label, in_warmup, confidence_score,
                level_z_126)],
    factors,
    by = "date", all.x = TRUE
  )
  setorder(dt, date)

  # Window flags
  dt[, window := fcase(
    in_warmup == TRUE,               "warmup",
    date <= TRAIN_END,               "train",
    date <= VAL_END,                 "validation",
    default =                        "test"
  )]

  avail_factors <- intersect(FACTOR_COLS, names(dt))
  cat("  Factors available:", length(avail_factors), "\n")

  # Bar counts
  cat(sprintf("  Window sizes — train: %d  val: %d  test: %d\n",
              sum(dt$window=="train" & dt$regime_label %in% MODELLED_REGIMES),
              sum(dt$window=="validation" & dt$regime_label %in% MODELLED_REGIMES),
              sum(dt$window=="test" & dt$regime_label %in% MODELLED_REGIMES)))

  # ── Output columns ─────────────────────────────────────────────────────────
  dt[, fair_value   := NA_real_]
  dt[, deviation    := NA_real_]
  dt[, dev_zscore   := NA_real_]
  dt[, signal       := "FLAT"]
  dt[, model_regime := NA_character_]

  # ══════════════════════════════════════════════════════════════════════════
  # STEP 1: FIT MODELS ON TRAINING DATA ONLY
  # ══════════════════════════════════════════════════════════════════════════
  cat("\n--- STEP 1: Training elastic nets (train window only) ---\n\n")

  train_dt <- dt[window == "train" & regime_label %in% MODELLED_REGIMES]
  regime_models <- list()
  all_coefs     <- list()

  for (regime in MODELLED_REGIMES) {
    regime_train <- train_dt[regime_label == regime]
    n_reg        <- nrow(regime_train)
    cat(sprintf("  %-25s  n=%d  ", regime, n_reg))

    if (n_reg < MODEL_PARAMS$min_regime_obs) {
      cat("SKIP (insufficient obs)\n")
      regime_models[[regime]] <- NULL
      next
    }

    X_raw <- as.matrix(regime_train[, ..avail_factors])
    X_raw <- .impute_median(X_raw)
    y_raw     <- regime_train$M1M2
    y_mean_r  <- mean(y_raw, na.rm = TRUE)
    y_raw     <- y_raw - y_mean_r
    regime_models[[paste0(regime, "_ymean")]] <- y_mean_r

    model_fit <- tryCatch(
      .fit_regime_model(X_raw, y_raw,
                         alpha   = MODEL_PARAMS$alpha,
                         n_folds = MODEL_PARAMS$n_folds),
      error = function(e) { cat("ERROR:", conditionMessage(e)); NULL }
    )

    regime_models[[regime]] <- model_fit

    if (!is.null(model_fit)) {
      n_active <- sum(abs(coef(model_fit$fit, s=model_fit$lambda)[-1]) > 1e-10)
      cat(sprintf("lambda=%.4f  active_vars=%d  cv_mse=%.4f\n",
                  model_fit$lambda, n_active,
                  ifelse(is.na(model_fit$cv_mse), 0, model_fit$cv_mse)))
      coef_dt <- .extract_coefs(model_fit, regime)
      if (!is.null(coef_dt)) all_coefs[[regime]] <- coef_dt
    } else {
      cat("FAILED\n")
    }
  }

  # ══════════════════════════════════════════════════════════════════════════
  # STEP 2: GENERATE PREDICTIONS ON ALL WINDOWS
  # ══════════════════════════════════════════════════════════════════════════
  cat("\n--- STEP 2: Generating fair value predictions ---\n")

  # Apply models to train + validation + test
  pred_idx <- which(dt$regime_label %in% MODELLED_REGIMES & !dt$in_warmup)

  for (i in pred_idx) {
    regime <- dt$regime_label[i]
    model  <- regime_models[[regime]]
    if (is.null(model)) next

    X_bar <- as.matrix(dt[i, ..avail_factors])
    X_bar <- .impute_median(X_bar)

    fv_dev <- tryCatch(.predict_model(model, X_bar), error=function(e) NA_real_)
    regime_idx_prior <- which(dt$model_regime == regime &
                                dt$window == "train" &
                                seq_len(nrow(dt)) < i)
    if (length(regime_idx_prior) >= 10) {
      roll_n     <- min(63L, length(regime_idx_prior))
      regime_lvl <- mean(tail(dt$M1M2[regime_idx_prior], roll_n), na.rm=TRUE)
    } else {
      regime_lvl <- regime_models[[paste0(regime, "_ymean")]]
      if (is.null(regime_lvl)) regime_lvl <- 0
    }
    fv <- fv_dev + regime_lvl
    dt[i, `:=`(fair_value = fv, deviation = M1M2 - fv, model_regime = regime)]
  }

  # ── Deviation z-score (rolling std fitted on training data only) ───────────
  # Compute rolling std of deviation using TRAIN window only
  # Then apply same std to validation and test (no future info)
  cat("  Computing deviation z-scores (train-based rolling std)...\n")

  for (regime in MODELLED_REGIMES) {
    # Training deviations
    train_idx <- which(dt$window == "train" & dt$model_regime == regime &
                         !is.na(dt$deviation))
    if (length(train_idx) < 10) next

    train_devs <- dt$deviation[train_idx]
    # Rolling std on training
    train_roll_std <- zoo::rollapply(train_devs,
                                      MODEL_PARAMS$dev_zscore_window,
                                      sd, fill=NA, align="right", partial=TRUE)
    train_roll_std <- pmax(train_roll_std, 1e-6)
    dt[train_idx, dev_zscore := train_devs / train_roll_std]

    # For validation + test: use the final training rolling std (last value)
    final_train_std <- tail(train_roll_std[!is.na(train_roll_std)], 1)
    if (length(final_train_std) == 0) next

    fwd_idx <- which(dt$window %in% c("validation", "test") &
                       dt$model_regime == regime & !is.na(dt$deviation))
    if (length(fwd_idx) > 0)
      dt[fwd_idx, dev_zscore := deviation / final_train_std]
  }

  # ══════════════════════════════════════════════════════════════════════════
  # STEP 3: TUNE SIGNAL THRESHOLD ON VALIDATION WINDOW ONLY
  # ══════════════════════════════════════════════════════════════════════════
  cat("\n--- STEP 3: Threshold tuning (validation window only) ---\n")

  val_dt    <- dt[window == "validation" & !is.na(dev_zscore)]
  best_thresh <- if (nrow(val_dt) >= 10) {
    .tune_threshold(val_dt)
  } else {
    cat("  Insufficient validation bars — using default threshold 1.5\n")
    1.5
  }

  # ══════════════════════════════════════════════════════════════════════════
  # STEP 4: APPLY FINAL SIGNALS USING TUNED THRESHOLD
  # ══════════════════════════════════════════════════════════════════════════
  dt[!is.na(dev_zscore), signal := "FLAT"]
  dt[!is.na(dev_zscore) & dev_zscore < -best_thresh, signal := "BUY_SPREAD"]
  dt[!is.na(dev_zscore) & dev_zscore >  best_thresh, signal := "SELL_SPREAD"]
  # No signals during training (would be in-sample — not a real signal)
  dt[window == "train", signal := "FLAT"]

  # ══════════════════════════════════════════════════════════════════════════
  # STEP 5: GOODNESS OF FIT — ALL THREE WINDOWS
  # ══════════════════════════════════════════════════════════════════════════
  cat("\n", strrep("=", 60), "\n")
  cat("GOODNESS OF FIT —", product, "\n")
  cat(strrep("=", 60), "\n")

  gof_train <- .gof_metrics(dt[window == "train"], "TRAINING")
  gof_val   <- .gof_metrics(dt[window == "validation"], "VALIDATION")
  gof_test  <- .gof_metrics(dt[window == "test"], "TEST (final)")

  # ── Current signal (last bar) ──────────────────────────────────────────────
  last <- tail(dt[!is.na(fair_value)], 1)
  if (nrow(last) > 0) {
    cat(sprintf("\n--- CURRENT SIGNAL (%s) ---\n", format(last$date)))
    cat(sprintf("  Product:     %s\n",   product))
    cat(sprintf("  Regime:      %s\n",   last$regime_label))
    cat(sprintf("  Actual M1M2: %.4f\n", last$M1M2))
    cat(sprintf("  Fair value:  %.4f\n", last$fair_value))
    cat(sprintf("  Deviation:   %+.4f\n",last$deviation))
    cat(sprintf("  Dev z-score: %+.3f\n",last$dev_zscore))
    cat(sprintf("  Signal:      %s\n",   last$signal))
    cat(sprintf("  Window:      %s\n",   last$window))
  }

  # ── Save ───────────────────────────────────────────────────────────────────
  out_cols <- c("date", "M1M2", "regime_label", "window", "in_warmup",
                "fair_value", "deviation", "dev_zscore", "signal",
                "model_regime", "confidence_score")
  out_cols <- intersect(out_cols, names(dt))

  fwrite(dt[, ..out_cols],
         file.path(output_dir, paste0("fair_value_", product, ".csv")))

  if (length(all_coefs) > 0) {
    coef_all <- rbindlist(all_coefs, fill=TRUE)
    fwrite(coef_all,
           file.path(output_dir,
                     paste0("model_coefficients_", product, ".csv")))
  }

  cat("\nSaved:\n")
  cat("  output/fair_value_", product, ".csv\n", sep="")
  if (length(all_coefs) > 0)
    cat("  output/model_coefficients_", product, ".csv\n", sep="")

  list(
    product      = product,
    fair_value   = dt[, ..out_cols],
    gof_train    = gof_train,
    gof_val      = gof_val,
    gof_test     = gof_test,
    signal_thresh = best_thresh,
    signal_table = dt[window %in% c("validation","test") & signal != "FLAT",
                       .(date, window, M1M2, fair_value, deviation,
                         dev_zscore, signal, regime_label)],
    coefficients = if (length(all_coefs)>0) rbindlist(all_coefs, fill=TRUE)
                   else NULL
  )
}

# ── Run all products ───────────────────────────────────────────────────────────
run_fair_value_all <- function(products = c("CL","LCO","HO","LGO"),
                                output_dir = "output",
                                factors_path = "output/factors_combined.csv") {
  results <- lapply(products, function(p) {
    tryCatch(
      run_fair_value_model(p, output_dir, factors_path),
      error = function(e) { cat("ERROR", p, ":", conditionMessage(e), "\n"); NULL }
    )
  })
  names(results) <- products

  cat("\n", strrep("=", 60), "\n")
  cat("CROSS-PRODUCT SUMMARY — TEST WINDOW\n")
  cat(strrep("=", 60), "\n\n")
  cat(sprintf("  %-8s %-25s %7s %7s %7s %8s\n",
              "Product","Regime","R²","RMSE","IC","Signals"))
  cat("  ", strrep("-", 65), "\n")

  for (p in products) {
    r <- results[[p]]
    if (is.null(r) || is.null(r$gof_test) || nrow(r$gof_test)==0) next
    best <- r$gof_test[which.max(n)]
    cat(sprintf("  %-8s %-25s %7.3f %7.4f %7.3f %8d\n",
                p, substr(best$regime,1,25),
                best$r2, best$rmse,
                ifelse(is.na(best$ic),0,best$ic),
                best$n_signals))
  }
  invisible(results)
}
