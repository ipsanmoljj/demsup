# =============================================================================
# build_intraday_csvs.R
# =============================================================================
# Reads raw 1-min event-driven outright futures CSVs (manager's format)
# and produces 15-min M1M2 spread OHLCV CSVs that exactly match the
# SQLite bar schema used in the live system.
#
# Input files:
#   CL_outrights_1min_t.csv      -> CL (WTI)
#   wtcl_lco_outrights_1min.csv  -> LCO (Brent) [also contains CL, use CO cols]
#
# Output files (data/intraday/):
#   CL_m1m2_15min.csv
#   LCO_m1m2_15min.csv
#
# Output schema per file (matches SQLite bars exactly):
#   timestamp  TEXT  bar open time UTC "YYYY-MM-DD HH:MM:SS"
#   open       REAL  first spread tick in bar  (c1 - c2)
#   high       REAL  max  spread tick in bar
#   low        REAL  min  spread tick in bar
#   close      REAL  last spread tick in bar
#   volume     REAL  sum(c1_volume) + sum(c2_volume) in bar
#
# Roll convention: positional — c1 = M1 (front), c2 = M2 (second)
# Bar cadence: 15 minutes
# Timestamps: UTC, bar-open convention (matches SQLite)
# =============================================================================

library(data.table)
library(lubridate)

# Null coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── 0. Configuration ----------------------------------------------------------

DATA_ROOT  <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup"
OUT_DIR    <- file.path(DATA_ROOT, "data", "intraday")
BAR_MINS   <- 15L

# Map product label -> which file to read, and which contract columns to use
PRODUCTS <- list(
  CL = list(
    file       = "CL_outrights_1min_t.csv",
    c1_col     = "c1",    # front month
    c2_col     = "c2",    # second month
    has_volume = TRUE
  ),
  # WTCL_LCO_SPREAD: c1_weighted_mid is already the WTI-Brent inter-market
  # spread (WTCL minus LCO). Output it directly as a single-leg "spread" series.
  # open/high/low/close = c1_weighted_mid OHLCV within each 15-min bar.
  WTCL_LCO_SPREAD = list(
    file       = "wtcl_lco_outrights_1min.csv",
    mode       = "single",   # use c1 directly, no subtraction
    c1_col     = "c1",
    c2_col     = NULL,
    has_volume = FALSE
  ),

  # WTCL_LCO_M1M2: c1 minus c2 = the curve spread of the inter-market spread
  # i.e. front-month WTCL-LCO minus second-month WTCL-LCO.
  # Captures the term structure slope of the Brent-WTI differential.
  WTCL_LCO_M1M2 = list(
    file       = "wtcl_lco_outrights_1min.csv",
    mode       = "spread",   # c1 - c2
    c1_col     = "c1",
    c2_col     = "c2",
    has_volume = FALSE
  )
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
message("Output directory: ", OUT_DIR)

# ── 1. Raw file reader (mirrors manager's Python function) --------------------
# Format: #meta: line, then ||‑separated MultiIndex columns
# Columns are like: c1||contract, c1||volume, c1||weighted_mid, c2||...

.read_raw <- function(path) {
  message("  Reading: ", basename(path))

  # Find the meta line and skip it; next line is the column header
  con   <- file(path, "r")
  lines <- readLines(con, n = 5L)
  close(con)

  # Meta line is the first line starting with #meta:
  skip  <- sum(startsWith(lines, "#meta:"))

  raw <- fread(
    path,
    skip        = skip,
    header      = TRUE,
    sep         = ",",
    na.strings  = c("", "NA", "nan", "NaN", "None"),
    showProgress = FALSE
  )

  # Flatten MultiIndex column names first: "c1||weighted_mid" -> "c1_weighted_mid"
  # Must happen before timestamp parsing so we know the real first column name
  setnames(raw, gsub("\\|\\|", "_", names(raw)))

  # Parse timestamp — first column holds the datetime strings
  ts_col <- names(raw)[1]
  raw[, ts_parsed := lubridate::as_datetime(get(ts_col), tz = "UTC")]
  raw[, (ts_col) := NULL]
  setnames(raw, "ts_parsed", "timestamp")

  # Drop rows with no timestamp
  raw <- raw[!is.na(timestamp)]
  setkey(raw, timestamp)

  message("    Rows: ", format(nrow(raw), big.mark = ","),
          "  Range: ", min(raw$timestamp), " -> ", max(raw$timestamp))
  raw
}

# ── Helper: peek at available contract columns --------------------------------
.peek_columns <- function(path) {
  con   <- file(path, "r")
  lines <- readLines(con, n = 5L)
  close(con)
  skip  <- sum(startsWith(lines, "#meta:"))
  hdr   <- fread(path, skip = skip, nrows = 0L, showProgress = FALSE)
  cat("Columns in", basename(path), ":\n")
  print(names(hdr))
}

# ── 2. Build 15-min OHLCV bars for the M1M2 spread ---------------------------

.build_spread_bars <- function(dt, c1_col, c2_col = NULL, mode = "spread", bar_mins = 15L) {

  mid1 <- paste0(c1_col, "_weighted_mid")
  vol1 <- paste0(c1_col, "_volume")

  if (mode == "spread") {
    mid2 <- paste0(c2_col, "_weighted_mid")
    vol2 <- paste0(c2_col, "_volume")
    needed <- c(mid1, mid2)
  } else {
    # single mode: use c1 directly as the value, no subtraction
    mid2 <- NULL
    vol2 <- NULL
    needed <- mid1
  }

  # Validate required columns exist
  missing <- setdiff(needed, names(dt))
  if (length(missing) > 0) {
    stop("Missing columns: ", paste(missing, collapse = ", "),
         "\nAvailable: ", paste(names(dt), collapse = ", "))
  }

  # Check volume availability
  vol_cols <- c(vol1, vol2)
  has_vol  <- length(vol_cols) > 0 && all(vol_cols %in% names(dt))

  # Compute per-tick value
  if (mode == "spread") {
    dt[, value := get(mid1) - get(mid2)]
  } else {
    dt[, value := get(mid1)]
  }
  if (has_vol) {
    dt[, vol := rowSums(.SD, na.rm = TRUE), .SDcols = vol_cols]
  }

  # Drop ticks where value is NA
  dt <- dt[!is.na(value)]

  # Assign each tick to a 15-min bucket (floor to bar open)
  dt[, bar_open := floor_date(timestamp, unit = paste0(bar_mins, " minutes"))]

  # Aggregate to OHLCV — ordered within each bucket
  if (has_vol) {
    bars <- dt[order(timestamp)][
      ,
      .(
        open   = first(value),
        high   = max(value),
        low    = min(value),
        close  = last(value),
        volume = sum(vol, na.rm = TRUE)
      ),
      by = bar_open
    ]
  } else {
    bars <- dt[order(timestamp)][
      ,
      .(
        open   = first(value),
        high   = max(value),
        low    = min(value),
        close  = last(value),
        volume = NA_real_
      ),
      by = bar_open
    ]
    message("    Note: no volume columns in source — volume set to NA")
  }

  setnames(bars, "bar_open", "timestamp")
  setorder(bars, timestamp)

  # Format timestamp to match SQLite convention: "YYYY-MM-DD HH:MM:SS"
  bars[, timestamp := format(timestamp, "%Y-%m-%d %H:%M:%S")]

  bars
}

# ── 3. Process each product ---------------------------------------------------

for (product in names(PRODUCTS)) {
  cfg  <- PRODUCTS[[product]]
  path <- file.path(DATA_ROOT, cfg$file)

  message("\n=== ", product, " ===")

  if (!file.exists(path)) {
    warning("File not found, skipping: ", path)
    next
  }

  # Read raw
  dt <- .read_raw(path)

  # ── FIRST RUN FOR LCO ONLY ─────────────────────────────────────────────────
  # The wtcl_lco file contains both WTI and Brent contracts. Before building
  # bars, confirm that c1/c2 refer to Brent (CO) and not WTI (CL) contracts.
  #
  # HOW TO USE:
  #   Step 1 — uncomment the two lines below, source the file, read the output
  #   Step 2 — re-comment them, update c1_col/c2_col in PRODUCTS if needed
  #   Step 3 — source again to build the actual bars
  #
  # if (product == "LCO") {
  #   .peek_columns(path)
  #   stop("Column check done — re-comment these two lines and re-run")
  # }
  # ───────────────────────────────────────────────────────────────────────────

  # Build 15-min bars
  bars <- .build_spread_bars(dt, cfg$c1_col, cfg$c2_col,
                             mode     = cfg$mode %||% "spread",
                             bar_mins = BAR_MINS)

  message("  15-min bars produced: ", format(nrow(bars), big.mark = ","))
  message("  Spread range:  ", round(min(as.numeric(bars$open)), 4),
          " to ", round(max(as.numeric(bars$close)), 4))

  # Write output
  out_path <- file.path(OUT_DIR, paste0(product, "_m1m2_15min.csv"))
  fwrite(bars, out_path)
  message("  Written: ", out_path)
}

message("\nDone. Files in: ", OUT_DIR)

# ── 4. Quick validation (run after build) ------------------------------------
# Loads one output file and prints a summary to sanity-check output

.validate_output <- function(product) {
  path <- file.path(OUT_DIR, paste0(product, "_m1m2_15min.csv"))
  if (!file.exists(path)) { message("Not found: ", path); return(invisible()) }

  dt <- fread(path)
  cat("\n--- Validation:", product, "---\n")
  cat("Rows        :", format(nrow(dt), big.mark = ","), "\n")
  cat("Date range  :", as.character(dt$timestamp[1]), "->", as.character(dt$timestamp[nrow(dt)]), "\n")
  cat("Spread open :", round(range(dt$open), 4), "\n")
  cat("Spread close:", round(range(dt$close), 4), "\n")
  cat("Volume range:", round(range(dt$volume), 0), "\n")
  cat("NA check    : open=", sum(is.na(dt$open)),
      " high=", sum(is.na(dt$high)),
      " low=", sum(is.na(dt$low)),
      " close=", sum(is.na(dt$close)),
      " vol=", sum(is.na(dt$volume)), "\n")
  cat("Head:\n"); print(head(dt, 5))
  cat("Tail:\n"); print(tail(dt, 5))
}

# Uncomment to validate after build:
#.validate_output("CL")
#.validate_output("WTCL_LCO_SPREAD")
#.validate_output("WTCL_LCO_M1M2")