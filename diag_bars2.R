library(DBI)
library(RSQLite)
library(data.table)
library(lubridate)

root <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup"

# ── 1. Inspect both databases: what date range and contracts exist ─────────────
inspect_db <- function(path) {
  con <- dbConnect(SQLite(), path)
  tbls <- dbListTables(con)
  cat("DB:", basename(path), "  Tables:", length(tbls), "\n")
  cat("Contracts:", paste(sort(tbls), collapse=" "), "\n")
  # date range from first available table
  for (t in head(tbls, 1)) {
    r <- dbGetQuery(con, sprintf(
      "SELECT MIN(timestamp) as mn, MAX(timestamp) as mx FROM \"%s\"", t))
    cat("Date range:", r$mn, "to", r$mx, "\n")
  }
  dbDisconnect(con)
}

db_1min  <- file.path(root, "backtesting/extra/bars_1min_20260624.db")
db_15min <- file.path(root, "backtesting/bars_15min_20260623.db")
db_root  <- file.path(root, "bars_15min_20260612.db")  # root-level

cat("=== Database inventory ===\n")
if (file.exists(db_1min))  inspect_db(db_1min)
if (file.exists(db_15min)) inspect_db(db_15min)
if (file.exists(db_root))  inspect_db(db_root)

# ── 2. Extract CL contract prices around EIA release (June 24 10:30 ET) ───────
# EIA releases at 10:30 AM ET = 14:30 UTC
# Pre-EIA  : 10:25-10:29 ET = 14:25-14:29 UTC
# Post-EIA : 10:30-17:00 ET window (model uses 2-trading-day window)

cat("\n=== CL contracts in 1-min DB ===\n")
con1 <- dbConnect(SQLite(), db_1min)
tbls1 <- sort(dbListTables(con1))

# Month code mapping
month_code <- c(F=1,G=2,H=3,J=4,K=5,M=6,N=7,Q=8,U=9,V=10,X=11,Z=12)

# Identify CL and CO contracts
cl_tbls <- sort(tbls1[startsWith(tbls1, "CL_")])
co_tbls <- sort(tbls1[startsWith(tbls1, "CO_")])
cat("CL contracts:", paste(cl_tbls, collapse=" "), "\n")
cat("CO contracts:", paste(co_tbls, collapse=" "), "\n")

# Load all CL contract prices for June 24
load_contract <- function(con, tbl) {
  d <- as.data.table(dbGetQuery(con, sprintf(
    "SELECT timestamp, close FROM \"%s\" ORDER BY timestamp", tbl)))
  d[, ts := as.POSIXct(timestamp, tz="UTC")]
  d[, contract := tbl]
  d[, .(contract, ts, close)]
}

# Get all CL data
cl_data <- rbindlist(lapply(cl_tbls, function(t) load_contract(con1, t)))
co_data <- rbindlist(lapply(co_tbls, function(t) load_contract(con1, t)))

dbDisconnect(con1)

# ── 3. Identify M1/M2/M3/M6 contracts for June 24, 2026 ──────────────────────
# On Jun 24 2026: July (N26) has expired. Front month = August (Q26)
# M1=Q26, M2=U26, M3=V26, M4=X26, M5=Z26, M6=F27

cl_M <- list(M1="CL_Q26", M2="CL_U26", M3="CL_V26", M4="CL_X26", M5="CL_Z26", M6="CL_F27")
co_M <- list(M1="CO_Q26", M2="CO_U26", M3="CO_V26", M4="CO_X26", M5="CO_Z26", M6="CO_F27")

cat("\n=== Price check at market open (09:30 ET = 13:30 UTC) ===\n")
t_open <- as.POSIXct("2026-06-24 13:30:00", tz="UTC")
for (nm in names(cl_M)) {
  ctr <- cl_M[[nm]]
  row <- cl_data[contract == ctr & ts >= t_open][1]
  if (nrow(row)) cat(sprintf("  CL %s (%s): %.2f\n", nm, ctr, row$close))
}

# ── 4. Pre-EIA window: 10:25-10:29 ET = 14:25-14:29 UTC ─────────────────────
t_pre_start <- as.POSIXct("2026-06-24 14:25:00", tz="UTC")
t_pre_end   <- as.POSIXct("2026-06-24 14:30:00", tz="UTC")

cat("\n=== CL prices just BEFORE EIA (14:25-14:29 UTC = 10:25-10:29 ET) ===\n")
pre_prices <- list()
for (nm in names(cl_M)) {
  ctr <- cl_M[[nm]]
  rows <- cl_data[contract == ctr & ts >= t_pre_start & ts < t_pre_end]
  if (nrow(rows)) {
    pre_prices[[nm]] <- tail(rows$close, 1)
    cat(sprintf("  CL %s (%s): %.2f\n", nm, ctr, pre_prices[[nm]]))
  } else {
    # Find nearest available price
    rows2 <- cl_data[contract == ctr & ts <= t_pre_end]
    if (nrow(rows2)) {
      pre_prices[[nm]] <- tail(rows2$close, 1)
      cat(sprintf("  CL %s (%s): %.2f  [nearest available]\n", nm, ctr, pre_prices[[nm]]))
    }
  }
}

# ── 5. Post-EIA windows ───────────────────────────────────────────────────────
t_post_5m  <- as.POSIXct("2026-06-24 14:35:00", tz="UTC")  # 5 min after
t_post_1h  <- as.POSIXct("2026-06-24 15:30:00", tz="UTC")  # 1 hour after
t_post_eod <- as.POSIXct("2026-06-24 19:30:00", tz="UTC")  # EOD (20:30 ET close... approx)

get_price_at <- function(data_dt, ctr, t) {
  rows <- data_dt[contract == ctr & ts <= t]
  if (nrow(rows)) tail(rows$close, 1) else NA_real_
}

cat("\n=== CL prices AFTER EIA release ===\n")
post_5m  <- sapply(cl_M, function(ctr) get_price_at(cl_data, ctr, t_post_5m))
post_1h  <- sapply(cl_M, function(ctr) get_price_at(cl_data, ctr, t_post_1h))
post_eod <- sapply(cl_M, function(ctr) get_price_at(cl_data, ctr, t_post_eod))

cat("Post  5-min (14:35 UTC):\n")
for (nm in names(cl_M)) if (!is.na(post_5m[nm])) cat(sprintf("  CL %s: %.2f\n", nm, post_5m[nm]))
cat("Post  1-hour (15:30 UTC):\n")
for (nm in names(cl_M)) if (!is.na(post_1h[nm])) cat(sprintf("  CL %s: %.2f\n", nm, post_1h[nm]))
cat("Post EOD (19:30 UTC):\n")
for (nm in names(cl_M)) if (!is.na(post_eod[nm])) cat(sprintf("  CL %s: %.2f\n", nm, post_eod[nm]))

# ── 6. Compute actual CL spreads ──────────────────────────────────────────────
compute_spreads <- function(prices, M) {
  m1 <- prices[M$M1]; m2 <- prices[M$M2]
  m3 <- prices[M$M3]; m6 <- prices[M$M6]
  list(
    m1m2   = m1 - m2,
    m2m3   = m2 - m3,
    m1m6   = m1 - m6,
    fly123 = m1 - 2*m2 + m3,
    fly136 = m1 - 2*m3 + m6
  )
}

pre_vec <- setNames(
  sapply(names(cl_M), function(nm) if (!is.null(pre_prices[[nm]])) pre_prices[[nm]] else NA_real_),
  unlist(cl_M))

cat("\n=== CL Spreads: Pre-EIA vs Post-EIA ===\n")
cat(sprintf("%-12s %8s %8s %8s %8s %8s\n", "Spread", "Pre", "+5min", "+1hr", "EOD", "Change(EOD)"))
pre_spr  <- compute_spreads(pre_vec,  cl_M)
p5_spr   <- compute_spreads(post_5m,  cl_M)
p1h_spr  <- compute_spreads(post_1h,  cl_M)
eod_spr  <- compute_spreads(post_eod, cl_M)
for (spr in c("m1m2","m2m3","m1m6","fly123","fly136")) {
  pre <- pre_spr[[spr]]; p5 <- p5_spr[[spr]]
  p1h <- p1h_spr[[spr]]; eod <- eod_spr[[spr]]
  chg <- if (!is.na(eod) && !is.na(pre)) eod - pre else NA
  cat(sprintf("%-12s %8.4f %8.4f %8.4f %8.4f %8.4f\n",
              spr,
              if(!is.na(pre)) pre else 0,
              if(!is.na(p5)) p5 else 0,
              if(!is.na(p1h)) p1h else 0,
              if(!is.na(eod)) eod else 0,
              if(!is.na(chg)) chg else 0))
}

# ── 7. CO (Brent/LCO) spreads ─────────────────────────────────────────────────
cat("\n=== CO (Brent) contracts available ===\n")
cat(paste(co_tbls, collapse=" "), "\n")

if (all(c("CO_Q26","CO_U26","CO_V26","CO_F27") %in% co_tbls)) {
  co_pre_vec <- setNames(
    sapply(names(co_M), function(nm) get_price_at(co_data, co_M[[nm]], t_pre_end)),
    unlist(co_M))
  co_eod_vec <- setNames(
    sapply(names(co_M), function(nm) get_price_at(co_data, co_M[[nm]], t_post_eod)),
    unlist(co_M))
  co_pre_spr <- compute_spreads(co_pre_vec, co_M)
  co_eod_spr <- compute_spreads(co_eod_vec, co_M)
  cat("\n=== CO Spreads: Pre vs EOD ===\n")
  cat(sprintf("%-12s %8s %8s %8s\n", "Spread", "Pre", "EOD", "Change"))
  for (spr in c("m1m2","m2m3","m1m6")) {
    pre <- co_pre_spr[[spr]]; eod <- co_eod_spr[[spr]]
    cat(sprintf("%-12s %8.4f %8.4f %8.4f\n", spr,
                if(!is.na(pre)) pre else 0,
                if(!is.na(eod)) eod else 0,
                if(!is.na(pre)&&!is.na(eod)) eod-pre else 0))
  }
}

# ── 8. Check 15-min DB for HO and LGO ────────────────────────────────────────
cat("\n=== Checking 15-min DB for HO/LGO ===\n")
con15 <- dbConnect(SQLite(), db_15min)
tbls15 <- dbListTables(con15)
cat("All tables:", paste(sort(tbls15), collapse=" "), "\n")
dbDisconnect(con15)

# ── 9. Save actuals for comparison ────────────────────────────────────────────
actuals <- data.table(
  product = "CL",
  spread  = c("m1m2","m2m3","m1m6","fly123","fly136"),
  pre_price = c(pre_spr$m1m2, pre_spr$m2m3, pre_spr$m1m6, pre_spr$fly123, pre_spr$fly136),
  post_5min  = c(p5_spr$m1m2, p5_spr$m2m3, p5_spr$m1m6, p5_spr$fly123, p5_spr$fly136),
  post_1hr   = c(p1h_spr$m1m2, p1h_spr$m2m3, p1h_spr$m1m6, p1h_spr$fly123, p1h_spr$fly136),
  post_eod   = c(eod_spr$m1m2, eod_spr$m2m3, eod_spr$m1m6, eod_spr$fly123, eod_spr$fly136)
)
actuals[, chg_5min := post_5min - pre_price]
actuals[, chg_1hr  := post_1hr  - pre_price]
actuals[, chg_eod  := post_eod  - pre_price]
fwrite(actuals, file.path(root, "output/live_actuals_CL_20260624.csv"))
cat("\nSaved: output/live_actuals_CL_20260624.csv\n")
print(actuals)
