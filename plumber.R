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

#* Health check
#* @serializer unboxedJSON
#* @get /health
function() {
  list(status = "ok", products = VALID_PRODUCTS)
}