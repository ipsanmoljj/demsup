# plumber.R — exposes demsup's regime classifier as a tiny HTTP API.
#
# Place this file at the root of the demsup repo, alongside the R/ folder.
# Run with:
#   Rscript -e "plumber::pr_run(plumber::pr('plumber.R'), port = 8001)"
#
# ── IMPORTANT — what this file does and does NOT do ─────────────────────────
# This file does NOT run the full demsup pipeline (read_futures_csv ->
# run_break_detection -> run_parallel_models). That upstream model-fitting step
# needs your raw price CSVs and is specific to however you load CL/LCO/HO/LGO
# data — nothing in this repo handout specifies that loading code, so it isn't
# duplicated here. You must run that step yourself (in an R console, or your
# own refresh script) BEFORE this API can serve a product, and again whenever
# you want fresh model fits. This file only does the read-back-and-classify
# step (classify_regimes()) plus cross-product consensus, then serves it
# over HTTP for the dashboard's demsup_fetcher.py to poll.
#
# Required per-product directory layout (matches the 2026-06-17 fix to
# run_parallel_models()/classify_regimes() in regime_models.R / regime_classifier.R):
#   output/CL/model_kf.rds, model_ms.rds, model_arima.rds, model_bp_breaks.rds, model_signals.rds
#   output/LCO/...  output/HO/...  output/LGO/...
# These get created by calling, once per product, BEFORE starting plumber
# (or before hitting /refresh — see below):
#   ff      <- read_futures_csv("CL_data.csv")
#   results <- run_break_detection(ff, resample_to = "1 day")
#   models  <- run_parallel_models(results$data, results$consensus$high_confidence,
#                                   product = "CL")
#
# Contract (matches backend/fetchers/demsup_fetcher.py in energy-dashboard):
#   GET /regime?product=CL
#   -> {
#        "product":          "CL",
#        "date":             "2026-06-17",
#        "regime_label":     "Deep-Backwardation",
#        "confidence_score": 0.84,
#        "level_z_126":      -2.31,
#        "consensus_scope":  "GLOBAL"
#      }
#
# If a product's output_dir doesn't exist yet (model not fit), or classification
# fails for any reason, this returns a 4xx/5xx — the dashboard fetcher treats any
# non-200 or malformed body as INSUFFICIENT_DATA and will NOT fabricate a regime.

library(plumber)
library(jsonlite)
library(data.table)

source("R/futures_reader.R")
source("R/structural_breaks.R")
source("R/regime_models.R")
source("R/regime_models_treated.R")
source("R/window_selector.R")
source("R/regime_classifier.R")
source("R/signal_engine.R")

VALID_PRODUCTS <- c("CL", "LCO", "HO", "LGO")
OUTPUT_BASE    <- "output"

# Cache the FULL multi-product classification (all 4 products + consensus) in
# memory, refreshed on a timer. Re-running classify_regimes() for all 4 products
# plus build_cross_product_consensus() on every single HTTP request would be
# slow and pointless if the underlying .rds model files haven't changed —
# these are daily-bar regimes, not intraday, so a 30-min cache is generous.
.cache <- new.env()
.cache$data       <- NULL
.cache$cached_at  <- as.POSIXct(0)
.cache_ttl_secs    <- 30 * 60

# Build (or rebuild) the full cross-product classification. Returns a list:
#   per_product[[product]] -> classify_regimes() result for that product
#   consensus               -> build_cross_product_consensus() result, or NULL
#                               if fewer than 2 products classified successfully
#                               (consensus needs multiple products to mean anything)
.classify_all <- function() {
  per_product <- list()
  errors      <- list()

  for (p in VALID_PRODUCTS) {
    out_dir <- file.path(OUTPUT_BASE, p)
    if (!dir.exists(out_dir)) {
      errors[[p]] <- paste0(
        "output dir '", out_dir, "' missing — run run_parallel_models(..., product = '", p, "') first"
      )
      next
    }
    res <- tryCatch(
      classify_regimes(product = p, output_dir = out_dir),
      error = function(e) {
        errors[[p]] <<- conditionMessage(e)
        NULL
      }
    )
    if (!is.null(res)) per_product[[p]] <- res
  }

  if (length(errors) > 0) {
    for (p in names(errors)) {
      cat("  [demsup plumber] WARNING —", p, ":", errors[[p]], "\n")
    }
  }

  # build_cross_product_consensus() needs all_labels_list — a NAMED list of
  # classify_regimes() results — and only really means something with 2+
  # products successfully classified. With only 1 (or 0), skip it rather than
  # erroring; /regime will just return consensus_scope = NA for that case.
  consensus <- NULL
  if (length(per_product) >= 2) {
    consensus <- tryCatch(
      build_cross_product_consensus(per_product, output_dir = OUTPUT_BASE),
      error = function(e) {
        cat("  [demsup plumber] WARNING — consensus build failed:", conditionMessage(e), "\n")
        NULL
      }
    )
  }

  list(per_product = per_product, consensus = consensus, errors = errors)
}

.get_cached <- function(force_refresh = FALSE) {
  now <- Sys.time()
  stale <- is.null(.cache$data) || (now - .cache$cached_at) > .cache_ttl_secs
  if (force_refresh || stale) {
    .cache$data      <- .classify_all()
    .cache$cached_at <- now
  }
  .cache$data
}

# Extract one product's latest-row summary from a cached classify_all() result.
.product_summary <- function(cached, product) {
  if (product %in% names(cached$errors)) {
    stop(cached$errors[[product]])
  }
  result <- cached$per_product[[product]]
  if (is.null(result)) {
    stop(paste0("no classification result available for '", product, "'"))
  }

  labels <- result$labels
  if (is.null(labels) || nrow(labels) == 0) {
    stop(paste0("classify_regimes('", product, "') returned no labels"))
  }

  active <- labels[in_warmup == FALSE]
  latest <- if (nrow(active) > 0) active[order(-date)][1] else labels[order(-date)][1]

  # consensus_scope: build_cross_product_consensus() returns $consensus, a
  # data.table keyed by `date` with a `regime_scope` column (GLOBAL/BROAD/
  # LOCAL/DIVERGENT) and a `consensus_label` column — confirmed against the
  # real function body in regime_classifier.R on 2026-06-17. It is NOT
  # per-product; it's one scope value per date across all classified products.
  scope <- NA_character_
  if (!is.null(cached$consensus) && !is.null(cached$consensus$consensus)) {
    cdt <- cached$consensus$consensus
    crow <- cdt[date == latest$date]
    if (nrow(crow) > 0) scope <- crow$regime_scope[1]
  }

  list(
    product          = product,
    date             = as.character(latest$date),
    regime_label     = latest$regime_label,
    confidence_score = if (is.na(latest$confidence_score)) NA else round(latest$confidence_score, 4),
    level_z_126      = if (is.na(latest$level_z_126)) NA else round(latest$level_z_126, 4),
    consensus_scope  = scope
  )
}

#* @apiTitle demsup regime API
#* @apiDescription Internal bridge between demsup's R regime classifier and the
#*   energy-dashboard Python/React project. Not intended for public exposure —
#*   run on localhost or behind the same private network as the dashboard backend.

#* Return the latest curve-structure regime for one product
#* @param product:character One of CL, LCO, HO, LGO
#* @serializer unboxedJSON
#* @get /regime
function(product = "", res) {
  product <- toupper(trimws(product))

  if (!(product %in% VALID_PRODUCTS)) {
    res$status <- 400
    return(list(
      error = paste0(
        "Unknown product '", product, "'. Valid: ",
        paste(VALID_PRODUCTS, collapse = ", ")
      )
    ))
  }

  result <- tryCatch(
    .product_summary(.get_cached(), product),
    error = function(e) {
      res$status <- 503
      list(error = paste0("'", product, "' unavailable: ", conditionMessage(e)))
    }
  )

  result
}

#* Force a cache refresh — re-reads whatever .rds files currently exist under
#* output/<product>/ and re-runs classification + consensus. Call this after
#* re-running run_parallel_models() for any product so /regime reflects the
#* new fit without waiting for the 30-min cache to expire on its own.
#* @serializer unboxedJSON
#* @post /refresh
function(res) {
  cached <- .get_cached(force_refresh = TRUE)
  list(
    refreshed_at = as.character(Sys.time()),
    products_ok  = names(cached$per_product),
    products_failed = if (length(cached$errors) > 0) names(cached$errors) else list(),
    has_consensus = !is.null(cached$consensus)
  )
}

#* Return the FULL daily regime history for one product — date, regime_label,
#* level_z_126, confidence_score, M1M2, for every non-warmup day. Used for
#* historical regime timeline / z-score charting on the dashboard, as opposed
#* to /regime which only returns the latest row.
#* @param product:character One of CL, LCO, HO, LGO
#* @serializer unboxedJSON
#* @get /regime-history
function(product = "", res) {
  product <- toupper(trimws(product))

  if (!(product %in% VALID_PRODUCTS)) {
    res$status <- 400
    return(list(
      error = paste0(
        "Unknown product '", product, "'. Valid: ",
        paste(VALID_PRODUCTS, collapse = ", ")
      )
    ))
  }

  cached <- .get_cached()
  if (product %in% names(cached$errors)) {
    res$status <- 503
    return(list(error = cached$errors[[product]]))
  }
  result <- cached$per_product[[product]]
  if (is.null(result) || is.null(result$labels)) {
    res$status <- 503
    return(list(error = paste0("no classification result available for '", product, "'")))
  }

  labels <- result$labels[in_warmup == FALSE]
  if (nrow(labels) == 0) {
    res$status <- 503
    return(list(error = paste0("'", product, "' has no non-warmup history yet")))
  }
  setorder(labels, date)

  list(
    product = product,
    history = lapply(seq_len(nrow(labels)), function(i) {
      row <- labels[i]
      list(
        date              = as.character(row$date),
        regime_label      = row$regime_label,
        regime_id         = row$regime_id,
        level_z_126       = if (is.na(row$level_z_126)) NA else round(row$level_z_126, 4),
        confidence_score  = if (is.na(row$confidence_score)) NA else round(row$confidence_score, 4),
        m1m2              = round(row$M1M2, 4)
      )
    })
  )
}

#* TEMPORARY DEBUG — confirms whether window_selector.R's functions are
#* actually loaded in plumber's live execution environment, vs. just present
#* in the plumber.R source file. Added 2026-06-18 to diagnose why
#* classify_regimes() keeps falling back to default window/lag even after
#* restarts. Safe to remove once the root cause is found.
#* @serializer unboxedJSON
#* @get /debug/env-check
function() {
  # Actually call classify_regimes() right now, fresh (bypassing the 30-min
  # cache entirely), and report what window/lag it actually chose — this is
  # the only way to know for certain which code path it took, since every
  # exists() check has passed and yet the cached /regime-history results
  # still show fallback-like behavior (high segment counts). If this field
  # shows level_z_window != 126, the live call IS auto-selecting correctly
  # and the problem must be in something else (e.g. the cache holding an
  # old pre-fix result). If it still shows 126, the bug is inside
  # classify_regimes() itself or how it's being invoked here, despite every
  # exists() check passing.
  live_call_result <- tryCatch({
    r <- classify_regimes(product = "CL", output_dir = file.path(OUTPUT_BASE, "CL"))
    list(
      level_z_window_used = r$level_z_window,
      lookback_lag_used   = r$lookback_lag,
      n_unique_regimes_in_labels = length(unique(r$labels$regime_label)),
      first_few_regime_labels    = head(r$labels[in_warmup == FALSE]$regime_label, 10)
    )
  }, error = function(e) list(error = conditionMessage(e)))

  list(
    select_level_z_window_exists = exists("select_level_z_window", mode = "function"),
    select_lookback_lag_exists   = exists("select_lookback_lag",   mode = "function"),
    select_thresholds_exists     = exists("select_thresholds",     mode = "function"),
    classify_regimes_env_is_global = identical(environment(classify_regimes), globalenv()),
    working_directory             = getwd(),
    window_selector_file_exists   = file.exists("R/window_selector.R"),
    window_selector_file_mtime    = if (file.exists("R/window_selector.R")) as.character(file.info("R/window_selector.R")$mtime) else NA,
    all_global_functions_matching_select = ls(envir = globalenv())[grepl("^select_", ls(envir = globalenv()))],
    live_call_result = live_call_result
  )
}

#* Health check
#* @serializer unboxedJSON
#* @get /health
function() {
  list(status = "ok", products = VALID_PRODUCTS)
}

# ═════════════════════════════════════════════════════════════════════════
# SIGNAL ENGINE — Layer A daily trade signal (R/signal_engine.R)
# ═════════════════════════════════════════════════════════════════════════
#
# run_signal_engine() depends on output/<product>/regime_labels_<product>.csv
# (with a fallback to the old shared output/regime_labels_<product>.csv —
# see signal_engine.R's labels_path fix, 2026-06-18). That file is written by
# classify_regimes() — so run /refresh (or call classify_regimes() yourself)
# before expecting fresh signal_engine results to reflect a new regime fit.
#
# run_signal_engine() is itself the validated function — this cache just
# avoids re-running it (and its full historical trade simulation across all
# four products) on every single HTTP request. It returns:
#   $live_signal[[product]] -> today's signal (BUY/SELL/FLAT) + why
#   $summary                -> data.table from signal_summary.csv-equivalent
#                               test-window stats (hit rate, EV, P&L, etc.)

.signal_cache <- new.env()
.signal_cache$data      <- NULL
.signal_cache$cached_at <- as.POSIXct(0)
.signal_cache_ttl_secs   <- 30 * 60

.run_signal_engine_safe <- function() {
  tryCatch(
    run_signal_engine(products = VALID_PRODUCTS, output_dir = OUTPUT_BASE, verbose = FALSE),
    error = function(e) {
      cat("  [demsup plumber] WARNING — run_signal_engine failed:", conditionMessage(e), "\n")
      NULL
    }
  )
}

.get_signal_cached <- function(force_refresh = FALSE) {
  now <- Sys.time()
  stale <- is.null(.signal_cache$data) || (now - .signal_cache$cached_at) > .signal_cache_ttl_secs
  if (force_refresh || stale) {
    .signal_cache$data      <- .run_signal_engine_safe()
    .signal_cache$cached_at <- now
  }
  .signal_cache$data
}

#* Return today's live trade signal for one product, plus its validated
#* test-window performance stats (hit rate, EV, total P&L, max drawdown).
#* @param product_code:character One of CL, LCO, HO, LGO
#* @serializer unboxedJSON
#* @get /signal
function(product_code = "", res) {
  # NOTE: parameter is named product_code, not product, deliberately —
  # signal_summary.csv has a column literally named `product`, and
  # data.table's dt[product == product] would silently always be TRUE
  # (column compared to itself) if the filter variable were also called
  # `product`. This is the exact bug class the handover doc flagged as
  # already having bitten intraday_signal_engine.R once (product == product
  # loop-variable collision) — naming it differently here avoids it
  # structurally rather than relying on remembering not to repeat it.
  product_code <- toupper(trimws(product_code))

  if (!(product_code %in% VALID_PRODUCTS)) {
    res$status <- 400
    return(list(
      error = paste0(
        "Unknown product '", product_code, "'. Valid: ",
        paste(VALID_PRODUCTS, collapse = ", ")
      )
    ))
  }

  # run_signal_engine() returns invisible(all_results) directly — NOT a
  # nested list with $all_results/$summary (confirmed against the real
  # function body, 2026-06-18; don't assume otherwise again). It writes
  # output/signal_summary.csv as a side effect, so read that file back for
  # the summary stats rather than threading them through the return value.
  all_results <- .get_signal_cached()
  if (is.null(all_results)) {
    res$status <- 503
    return(list(error = "run_signal_engine() failed — check regime_labels_<product>.csv exist under output/<product>/"))
  }

  prod_result <- all_results[[product_code]]
  if (is.null(prod_result) || is.null(prod_result$live_signal)) {
    res$status <- 503
    return(list(error = paste0("no live signal available for '", product_code, "' — check output/", product_code, "/regime_labels_", product_code, ".csv exists and has non-warmup rows")))
  }

  summary_row <- NULL
  summary_path <- file.path(OUTPUT_BASE, "signal_summary.csv")
  if (file.exists(summary_path)) {
    sdt  <- fread(summary_path)
    srow <- sdt[product == product_code]
    if (nrow(srow) > 0) summary_row <- as.list(srow[1])
  }

  list(
    live    = prod_result$live_signal,
    summary = summary_row
  )
}

#* Force a fresh run of the signal engine (re-reads regime_labels_<product>.csv
#* and re-simulates the full historical trade backtest for all four products).
#* Call after /refresh has updated the regime classification, so the signal
#* reflects the latest regime fit rather than a stale cached run.
#* @serializer unboxedJSON
#* @post /signal/refresh
function(res) {
  all_results <- .get_signal_cached(force_refresh = TRUE)
  if (is.null(all_results)) {
    res$status <- 503
    return(list(error = "run_signal_engine() failed on refresh"))
  }
  # all_results IS the function's real return value (invisible(all_results)
  # from run_signal_engine()) — not a nested $all_results field. See the
  # /signal endpoint's comment for why this was wrong on the first pass.
  list(
    refreshed_at = as.character(Sys.time()),
    products_ok  = names(all_results)
  )
}