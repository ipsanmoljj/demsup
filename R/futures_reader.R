# R/futures_reader.R
# ------------------
# Reads the proprietary futures CSV format.
#
# Format (confirmed from data):
#   Line 1 : #meta:1min||contract_num||field
#   Index  : timestamp (UTC, irregular)
#   Columns: c1||contract, c1||volume, c1||weighted_mid, c2||...  (up to c14)
#
# Usage:
#   source("R/futures_reader.R")
#   ff <- read_futures_csv("path/to/CL_outrights_1min_t.csv")
#   prices <- get_prices(ff, resample_to = "1 hour")
#   spreads <- get_spreads(ff, resample_to = "1 hour")

library(data.table)
library(lubridate)
library(zoo)

SEP <- "||"

# ── Core reader ───────────────────────────────────────────────────────────────

read_futures_csv <- function(path) {
  # Read metadata from first line
  meta_line <- readLines(path, n = 1)
  meta_line <- sub("^#meta:", "", meta_line)
  parts     <- strsplit(meta_line, "\\|\\|")[[1]]
  freq      <- parts[1]
  col_l0    <- ifelse(length(parts) > 1, parts[2], "contract_num")
  col_l1    <- ifelse(length(parts) > 2, parts[3], "field")

  # Read CSV skipping the #meta comment line
  dt <- fread(path, skip = 1, header = TRUE, na.strings = c("", "NA"))

  # First column is timestamp
  setnames(dt, 1, "timestamp")
  dt[, timestamp := with_tz(ymd_hms(timestamp), "UTC")]

  # Parse column names: "c1||weighted_mid" -> contract="c1", field="weighted_mid"
  raw_cols  <- colnames(dt)[-1]   # exclude timestamp
  col_parts <- strsplit(raw_cols, "\\|\\|")
  contract  <- sapply(col_parts, `[`, 1)
  field     <- sapply(col_parts, `[`, 2)

  # Sort by timestamp, remove duplicates
  setorder(dt, timestamp)
  dt <- unique(dt, by = "timestamp")

  # Attach metadata as attributes
  setattr(dt, "source_path", path)
  setattr(dt, "freq",        freq)
  setattr(dt, "col_l0",      col_l0)
  setattr(dt, "col_l1",      col_l1)
  setattr(dt, "contracts",   unique(contract))
  setattr(dt, "fields",      unique(field))
  setattr(dt, "raw_cols",    raw_cols)
  setattr(dt, "col_contract", contract)
  setattr(dt, "col_field",    field)

  class(dt) <- c("FuturesFile", class(dt))
  dt
}

# ── Print method ──────────────────────────────────────────────────────────────

print.FuturesFile <- function(ff) {
  contracts <- attr(ff, "contracts")
  fields    <- attr(ff, "fields")
  cat("FuturesFile\n")
  cat("  Source    :", basename(attr(ff, "source_path")), "\n")
  cat("  Contracts :", paste(contracts, collapse = ", "), "\n")
  cat("  Fields    :", paste(fields, collapse = ", "), "\n")
  cat("  Date range:", format(min(ff$timestamp)), "->", format(max(ff$timestamp)), "\n")
  cat("  Total rows:", format(nrow(ff), big.mark = ","), "(irregular timestamps)\n")
  cat("  Roll events (c1):", sum(diff(as.integer(factor(.get_ticker(ff, "c1")))) != 0), "\n")
}

# ── Internal helper: get ticker series for a contract ────────────────────────

.get_ticker <- function(ff, contract = "c1") {
  col <- paste0(contract, SEP, "contract")
  ff[[col]][!is.na(ff[[col]])]
}

# ── Get prices (weighted_mid) wide table ──────────────────────────────────────

get_prices <- function(ff,
                       contracts  = NULL,
                       start      = NULL,
                       end        = NULL,
                       resample_to = "1 hour",
                       fill_method = "locf") {
  # Default: all contracts
  all_contracts <- attr(ff, "contracts")
  if (is.null(contracts)) contracts <- all_contracts

  # Extract weighted_mid columns
  price_cols <- paste0(contracts, SEP, "weighted_mid")
  keep_cols  <- c("timestamp", price_cols)
  # Only keep cols that exist
  keep_cols  <- keep_cols[keep_cols %in% colnames(ff)]

  dt <- ff[, ..keep_cols]

  # Rename: "c1||weighted_mid" -> "c1"
  old_names <- price_cols[price_cols %in% colnames(dt)]
  new_names <- sub(paste0(SEP, "weighted_mid"), "", old_names)
  setnames(dt, old_names, new_names)

  # Date filter
  if (!is.null(start)) dt <- dt[timestamp >= ymd(start)]
  if (!is.null(end))   dt <- dt[timestamp <= ymd(end)]

  # Resample to regular grid
  if (!is.null(resample_to)) {
    dt <- .resample_prices(dt, resample_to, fill_method)
  }

  dt
}

# ── Resample to regular frequency ────────────────────────────────────────────

.resample_prices <- function(dt, freq_str, fill_method = "locf") {
  # Round timestamps to the desired frequency
  dt_copy <- copy(dt)
  dt_copy[, timestamp := floor_date(timestamp, unit = freq_str)]

  # Take last price in each bar (matches Python .last())
  price_cols <- setdiff(colnames(dt_copy), "timestamp")
  dt_agg <- dt_copy[, lapply(.SD, function(x) tail(x[!is.na(x)], 1)),
                     by = timestamp, .SDcols = price_cols]
  setorder(dt_agg, timestamp)

  # Forward fill NAs (carry last known price)
  if (fill_method == "locf") {
    for (col in price_cols) {
      set(dt_agg, j = col,
          value = na.locf(dt_agg[[col]], na.rm = FALSE))
    }
  }

  dt_agg
}

# ── Get spreads ───────────────────────────────────────────────────────────────

get_spreads <- function(ff,
                        pairs       = NULL,
                        resample_to = "1 hour",
                        fill_method = "locf") {

  # Default spread pairs: near contract - far contract
  if (is.null(pairs)) {
    pairs <- list(
      c("c1", "c2"),
      c("c1", "c3"),
      c("c1", "c6"),
      c("c1", "c12"),
      c("c2", "c3"),
      c("c3", "c6")
    )
  }

  # Labels
  labels <- list(
    "c1_c2"  = "M1M2",
    "c1_c3"  = "M1M3",
    "c1_c6"  = "M1M6",
    "c1_c12" = "M1M12",
    "c2_c3"  = "M2M3",
    "c3_c6"  = "M3M6",
    "c6_c12" = "M6M12"
  )

  # Get all needed contracts
  all_contracts <- unique(unlist(pairs))
  prices <- get_prices(ff, contracts = all_contracts,
                       resample_to = resample_to,
                       fill_method = fill_method)

  result <- data.table(timestamp = prices$timestamp)

  for (pair in pairs) {
    near <- pair[1]; far <- pair[2]
    if (near %in% colnames(prices) && far %in% colnames(prices)) {
      key   <- paste0(near, "_", far)
      label <- ifelse(!is.null(labels[[key]]), labels[[key]], key)
      result[, (label) := prices[[near]] - prices[[far]]]
    }
  }

  result
}

# ── Get curve metrics ─────────────────────────────────────────────────────────

get_curve_metrics <- function(ff, resample_to = "1 hour") {
  prices <- get_prices(ff, resample_to = resample_to)

  result <- data.table(timestamp = prices$timestamp)

  # Slope: M1 - M6 (positive = backwardation, negative = contango)
  if ("c1" %in% colnames(prices) && "c6" %in% colnames(prices))
    result[, slope := prices$c1 - prices$c6]

  # Curvature: M1 - 2*M3 + M6
  if (all(c("c1","c3","c6") %in% colnames(prices)))
    result[, curvature := prices$c1 - 2*prices$c3 + prices$c6]

  # Roll yield: annualised (M1-M2)/M1
  if (all(c("c1","c2") %in% colnames(prices))) {
    result[, roll_yield_ann := (prices$c1 - prices$c2) / prices$c1 * (365/30)]
    result[, contango       := as.integer(prices$c1 < prices$c2)]
  }

  # M1 absolute level
  if ("c1" %in% colnames(prices))
    result[, m1_level := prices$c1]

  result
}

# ── Full term structure bundle ────────────────────────────────────────────────

get_term_structure <- function(ff, resample_to = "1 hour") {
  list(
    prices  = get_prices(ff,        resample_to = resample_to),
    spreads = get_spreads(ff,       resample_to = resample_to),
    curve   = get_curve_metrics(ff, resample_to = resample_to)
  )
}