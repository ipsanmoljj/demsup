# =============================================================================
# validate_live_feed.R
# =============================================================================
# Reads SQLite bar files from the Lightstreamer feed, constructs M1M2 spread
# bars, and runs them through the intraday signal engine to show what signals
# would have fired on live data.
#
# This is a pipeline smoke test — confirms the live feed → signal path works
# end-to-end. With only a few days of data results are not statistically
# meaningful but execution validity is confirmed.
#
# Identifies M1/M2 automatically by sorting contract tenors chronologically.
# =============================================================================

library(data.table)
library(lubridate)
library(DBI)
library(RSQLite)

# ── 0. Configuration ----------------------------------------------------------

FEED_DIR   <- "I:/Public/Siddharth Raj/lightstreamer_data"
REGIME_DIR <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup/output"
OUT_DIR    <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup/output/intraday"

# CME/ICE month codes in calendar order
MONTH_ORDER <- c(F=1,G=2,H=3,J=4,K=5,M=6,N=7,Q=8,U=9,V=10,X=11,Z=12)

# Signal parameters (must match intraday_signal_engine.R)
Z_WINDOW_LONG  <- 120L
Z_WINDOW_SHORT <-  48L
RSI_PERIOD     <-  14L
ATR_PERIOD     <-  14L
ATR_MULT_STOP  <-   1.25
ATR_MULT_TGT   <-   2.0
BE_FRAC        <-   0.5

SESSION_HOURS <- list(
  CL = list(start = 13L, end = 20L),
  CO = list(start =  8L, end = 16L)
)

REGIME_PARAMS <- data.table(
  regime     = c("Deep-Backwardation","Backwardation-Deficit",
                 "Stable-Elevated","Transition-Tightening",
                 "Contango-Surplus","Easing-Contango",
                 "Deep-Contango","Warm-Up","Unknown"),
  z_thresh   = c(1.0, 2.5, 4.0, 4.0, 4.0, 4.0, 4.0, 99.0, 99.0),
  atr_mult   = c(1.25,1.00,0.75,0.75,0.75,0.75,0.75,  0.0,  0.0),
  confidence = c(1.0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,  0.0,  0.0)
)

# ── 1. Utilities --------------------------------------------------------------

.tenor_key <- function(tenor) {
  # e.g. "N26" -> sortable integer 2607
  month <- MONTH_ORDER[substr(tenor, 1, 1)]
  year  <- as.integer(substr(tenor, 2, 3))
  year * 100L + month
}

.sort_contracts <- function(tables, prefix) {
  tbl <- tables[startsWith(tables, paste0(prefix, "_"))]
  tenors <- sub(paste0("^", prefix, "_"), "", tbl)
  keys   <- sapply(tenors, .tenor_key)
  tbl[order(keys)]
}

.read_table <- function(conn, tbl) {
  tryCatch({
    # Checkpoint WAL into main db so all rows are visible
    tryCatch(dbExecute(conn, "PRAGMA wal_checkpoint(TRUNCATE)"),
             error = function(e) NULL)
    dt <- as.data.table(dbReadTable(conn, tbl))
    dt[, timestamp := as.POSIXct(timestamp, tz = "UTC")]
    setorder(dt, timestamp)
    dt
  }, error = function(e) {
    message("    Could not read table ", tbl, ": ", e$message)
    NULL
  })
}

# ── 2. Build M1M2 spread from a single SQLite file ---------------------------

.build_spread_from_db <- function(db_path, prefix) {
  conn   <- dbConnect(SQLite(), db_path,
                    flags = RSQLite::SQLITE_RWC)
  # Force WAL checkpoint so all data is readable
  tryCatch(dbExecute(conn, "PRAGMA journal_mode=WAL"),  error=function(e) NULL)
  tryCatch(dbExecute(conn, "PRAGMA wal_checkpoint(PASSIVE)"), error=function(e) NULL)
  tables <- dbListTables(conn)

  sorted <- .sort_contracts(tables, prefix)
  if (length(sorted) < 2) {
    dbDisconnect(conn)
    message("  Not enough contracts for ", prefix)
    return(NULL)
  }

  m1_tbl <- sorted[1]
  m2_tbl <- sorted[2]
  message("  ", prefix, " M1=", m1_tbl, "  M2=", m2_tbl)

  m1 <- .read_table(conn, m1_tbl)
  m2 <- .read_table(conn, m2_tbl)
  dbDisconnect(conn)

  if (is.null(m1) || is.null(m2)) return(NULL)

  # Align on common timestamps using character keys
  m1[, ts_key := format(timestamp, "%Y-%m-%d %H:%M:%S")]
  m2[, ts_key := format(timestamp, "%Y-%m-%d %H:%M:%S")]
  common <- intersect(m1$ts_key, m2$ts_key)
  if (length(common) == 0) {
    message("  No overlapping timestamps for ", prefix)
    return(NULL)
  }

  m1c <- m1[ts_key %in% common]
  m2c <- m2[ts_key %in% common]
  setorder(m1c, timestamp)
  setorder(m2c, timestamp)

  spread <- data.table(
    timestamp = m1c$timestamp,
    open      = m1c$open   - m2c$open,
    high      = m1c$high   - m2c$high,
    low       = m1c$low    - m2c$low,
    close     = m1c$close  - m2c$close,
    volume    = m1c$volume + m2c$volume,
    m1        = m1_tbl,
    m2        = m2_tbl
  )
  spread[, timestamp := as.POSIXct(format(timestamp, "%Y-%m-%d %H:%M:%S"),
                                    tz = "UTC")]

  spread
}

# ── 3. Load all SQLite files and stack into one spread series -----------------

.load_feed <- function(feed_dir, prefix) {
  db_files <- list.files(feed_dir, pattern = "\\.db$",
                         full.names = TRUE)
  db_files <- db_files[!grepl("-shm$|-wal$", db_files)]

  if (length(db_files) == 0) {
    message("No .db files found in: ", feed_dir)
    return(NULL)
  }

  message("Found ", length(db_files), " db file(s):")
  message("  ", paste(basename(db_files), collapse = "\n  "))

  all_spreads <- lapply(db_files, function(f) {
    message("\nReading: ", basename(f))
    .build_spread_from_db(f, prefix)
  })

  combined <- rbindlist(Filter(Negate(is.null), all_spreads))
  if (nrow(combined) == 0) return(NULL)

  # Deduplicate using character key to avoid POSIXct timezone comparison issues
  combined[, ts_key := format(timestamp, "%Y-%m-%d %H:%M:%S")]
  combined <- combined[!duplicated(ts_key)]
  combined[, ts_key := NULL]
  setorder(combined, timestamp)

  message("\n", prefix, " combined spread: ",
          format(nrow(combined), big.mark=","), " bars  ",
          min(combined$timestamp), " -> ", max(combined$timestamp))
  message("  Spread range: ",
          round(min(combined$close),4), " to ",
          round(max(combined$close),4))

  combined
}

# ── 4. Signal computation on live bars ---------------------------------------

.rsi <- function(x, n=14L) {
  out <- rep(NA_real_, length(x))
  if (length(x) <= n) return(out)
  dx <- diff(x)
  for (i in seq(n, length(dx))) {
    g <- pmax(dx[(i-n+1):i], 0); l <- pmax(-dx[(i-n+1):i], 0)
    ag <- mean(g, na.rm=TRUE); al <- mean(l, na.rm=TRUE)
    out[i+1] <- if(al==0) 100 else 100 - 100/(1+ag/al)
  }
  out
}

.roll_z <- function(x, n, min_obs=10L) {
  len <- length(x); out <- rep(NA_real_, len)
  for (i in seq_len(len)) {
    start <- max(1L, i - n + 1L)
    if (start > i) next
    w <- x[start:i]; w <- w[!is.na(w)]
    if (length(w) >= min_obs && sd(w) > 0)
      out[i] <- (x[i] - mean(w)) / sd(w)
  }
  out
}

.atr14 <- function(high, low, n=14L) {
  tr  <- high - low
  out <- rep(NA_real_, length(tr))
  for (i in seq_len(length(tr))) {
    start <- max(1L, i - n + 1L)
    if (i - start + 1L < n) next
    out[i] <- mean(tr[start:i], na.rm = TRUE)
  }
  out
}

.compute_signals <- function(dt, has_volume=TRUE) {
  dt <- copy(dt)
  dt[, date    := as.Date(format(timestamp, "%Y-%m-%d", tz="UTC"))]
  dt[, hour_utc:= hour(timestamp)]

  dt[, z_long  := .roll_z(close, Z_WINDOW_LONG)]
  dt[, z_short := .roll_z(close, Z_WINDOW_SHORT)]
  dt[, rsi     := .rsi(close, RSI_PERIOD)]
  dt[, atr14   := .atr14(high, low, ATR_PERIOD)]

  # Session open gap
  sc <- dt[, .(prior_close = last(close)), by=date]
  sc[, prior_close := shift(prior_close, 1L)]
  dt <- merge(dt, sc, by="date", all.x=TRUE)
  dt[, bar_rank := seq_len(.N), by=date]
  dt[bar_rank==1L, open_gap := open - prior_close]
  dt[bar_rank!=1L, open_gap := NA_real_]
  dt[, open_gap := zoo::na.locf(open_gap, na.rm=FALSE), by=date]

  # VWAP
  if (has_volume && !all(is.na(dt$volume))) {
    dt[, vwap    := cumsum(close*volume)/cumsum(volume), by=date]
    dt[, vwap_dev:= (close-vwap)/abs(vwap)]
  } else {
    dt[, vwap:=NA_real_][, vwap_dev:=NA_real_]
  }
  dt
}

# ── 5. Regime join ------------------------------------------------------------

.join_regime_live <- function(dt, regime_file) {
  if (!file.exists(regime_file)) {
    message("  Regime file not found: ", regime_file)
    dt[, regime := "Unknown"]
    return(dt)
  }
  reg      <- fread(regime_file)
  date_col <- intersect(c("date","Date"), names(reg))[1]
  lbl_col  <- intersect(c("regime_label","regime","label","modal_label"), names(reg))[1]
  if (!is.na(date_col) && !is.na(lbl_col)) {
    reg[, join_date := as.Date(get(date_col))]
    # Force UTC date extraction from live feed timestamps
    dt[, join_date := as.Date(format(timestamp, "%Y-%m-%d", tz="UTC"))]
    dt  <- merge(dt, reg[, .(join_date, regime=get(lbl_col))],
                 by="join_date", all.x=TRUE)
    dt[, join_date := NULL]
    message("  Regime join: ", sum(!is.na(dt$regime)), " bars matched")
  } else {
    dt[, regime := "Unknown"]
  }
  dt
}

# ── 6. Signal firing logic ---------------------------------------------------

.fire_signals <- function(dt, product) {
  sess <- SESSION_HOURS[[product]]

  dt <- merge(dt, REGIME_PARAMS, by="regime", all.x=TRUE)
  dt[is.na(z_thresh),   z_thresh   := 99.0]
  dt[is.na(atr_mult),   atr_mult   := 0.0 ]
  dt[is.na(confidence), confidence := 0.0 ]

  dt[, in_session := hour_utc >= sess$start & hour_utc < sess$end]
  dt[, regime_ok  := confidence > 0 & !is.na(regime)]

  dt[, signal_dir := fcase(
    z_long < -z_thresh,  1L,
    z_long >  z_thresh, -1L,
    rep(TRUE, .N),       0L
  )]

  dt[, prev_close := shift(close, 1L)]
  dt[, rev_bar    := fcase(
    signal_dir ==  1L & close > prev_close, TRUE,
    signal_dir == -1L & close < prev_close, TRUE,
    rep(TRUE, .N),                          FALSE
  )]

  dt[, entry_trigger := in_session & regime_ok & signal_dir != 0L & rev_bar]

  # Show all triggered bars
  triggered <- dt[entry_trigger == TRUE, .(
    timestamp, close, z_long, z_short, rsi, open_gap,
    signal_dir, regime, confidence, atr14, z_thresh
  )]

  triggered
}

# ── 7. Main runner ------------------------------------------------------------

run_live_validation <- function() {
  message(strrep("=", 65))
  message("LIVE FEED VALIDATION — Intraday Signal Engine")
  message(strrep("=", 65))

  if (!require(RSQLite, quietly=TRUE)) {
    message("Installing RSQLite...")
    install.packages("RSQLite")
    library(RSQLite)
  }

  results <- list()

  # ── CL ─────────────────────────────────────────────────────────────────────
  message("\n--- CL (WTI M1M2) ---")
  cl_spread <- .load_feed(FEED_DIR, "CL")

  if (!is.null(cl_spread)) {
    cl_spread <- .compute_signals(cl_spread, has_volume=TRUE)
    cl_spread <- .join_regime_live(
      cl_spread,
      file.path(REGIME_DIR, "regime_labels_CL.csv")
    )

    message("\n  Signal summary (z_long, last 10 bars):")
    print(tail(cl_spread[, .(timestamp, close, z_long, z_short,
                              rsi, regime, atr14)], 10))

    triggers <- .fire_signals(cl_spread, "CL")
    message("\n  Entry triggers fired: ", nrow(triggers))
    if (nrow(triggers) > 0) print(triggers)

    results$CL <- list(spread=cl_spread, triggers=triggers)
  }

  # ── CO (Brent) ─────────────────────────────────────────────────────────────
  message("\n--- CO (Brent M1M2) ---")
  co_spread <- .load_feed(FEED_DIR, "CO")

  if (!is.null(co_spread)) {
    co_spread <- .compute_signals(co_spread, has_volume=TRUE)
    co_spread <- .join_regime_live(
      co_spread,
      file.path(REGIME_DIR, "regime_labels_LCO.csv")
    )

    message("\n  Signal summary (z_long, last 10 bars):")
    print(tail(co_spread[, .(timestamp, close, z_long, z_short,
                              rsi, regime, atr14)], 10))

    triggers <- .fire_signals(co_spread, "CO")
    message("\n  Entry triggers fired: ", nrow(triggers))
    if (nrow(triggers) > 0) print(triggers)

    results$CO <- list(spread=co_spread, triggers=triggers)
  }

  # ── Summary ────────────────────────────────────────────────────────────────
  message("\n", strrep("=", 65))
  message("PIPELINE VALIDATION SUMMARY")
  message(strrep("=", 65))

  for (prod in names(results)) {
    sp  <- results[[prod]]$spread
    trg <- results[[prod]]$triggers
    message(sprintf("  %-8s  bars=%-4d  z_long_latest=%-7.3f  triggers=%d",
                    prod,
                    nrow(sp),
                    tail(sp$z_long[!is.na(sp$z_long)], 1),
                    nrow(trg)))
  }

  message("\nDone.")
  invisible(results)
}

# ── Run ----------------------------------------------------------------------
# source("R/validate_live_feed.R")
# results <- run_live_validation()