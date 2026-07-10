# R/build_cftc_panel.R
# Builds a daily CFTC panel for WTI (2020-2026) by:
#   1. Extending cftc_cl_merged.csv (which covers 2021-2024W05) with
#      2024W06+ data from cftc_2024.txt, cftc_2025.txt, cftc_2026.txt
#   2. Computing pos_z (52-week rolling z-score of managed money net position)
#   3. Forward-filling to daily frequency and tagging CFTC regime
#
# CFTC regime overlay logic (applied in phase3c):
#   Extreme Long  (pos_z > +1.5) : MM overcrowded → contrarian, multiply pos by 0.5
#   Mild Long     (pos_z +0.5 to +1.5): MM leaning long → reinforce long, dilute short
#   Neutral       (|pos_z| < 0.5)     : no adjustment
#   Mild Short    (pos_z -0.5 to -1.5): reinforce short, dilute long
#   Extreme Short (pos_z < -1.5)      : MM overcrowded → contrarian, multiply pos by 0.5

suppressPackageStartupMessages({ library(data.table); library(zoo) })

ROOT <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup"
OUT  <- file.path(ROOT, "strategy_live/final data/tent_data/cftc_daily.csv")
CFTC_DIR <- file.path(ROOT, "output")

WTI_PATTERN <- "CRUDE OIL, LIGHT SWEET-WTI"

# ── Helper: parse a raw CFTC annual txt file ──────────────────────────────────
parse_cftc_file <- function(path) {
  cat(sprintf("  Reading %s...\n", basename(path)))
  dt <- fread(path, header = TRUE, fill = TRUE, showProgress = FALSE,
              colClasses = "character")
  # filter for WTI
  wti <- dt[grepl(WTI_PATTERN, Market_and_Exchange_Names, ignore.case = TRUE)]
  if (nrow(wti) == 0) return(NULL)
  date_col <- grep("Report_Date", names(wti), value = TRUE)[1]
  wti[, .(
    report_date   = as.Date(get(date_col)),
    mm_long       = suppressWarnings(as.integer(trimws(M_Money_Positions_Long_All))),
    mm_short      = suppressWarnings(as.integer(trimws(M_Money_Positions_Short_All))),
    open_interest = suppressWarnings(as.integer(trimws(Open_Interest_All)))
  )]
}

# ── 1. Load existing merged data (2021–2024W05) ───────────────────────────────
existing <- fread(file.path(CFTC_DIR, "cftc/cftc_cl_merged.csv"))
existing[, report_date := as.Date(release_date)]
existing[, mm_long  := NA_integer_]  # not stored in merged — will recompute z from net_pos
existing[, mm_short := NA_integer_]
existing_simple <- existing[, .(report_date, net_pos, pos_z)]
existing_simple[, report_date := as.Date(report_date)]
setorder(existing_simple, report_date)
cat(sprintf("Existing CFTC: %d weeks (%s → %s)\n",
            nrow(existing_simple), min(existing_simple$report_date),
            max(existing_simple$report_date)))

# ── 2. Load raw files for 2024-2026 ──────────────────────────────────────────
raw_files <- c(
  file.path(CFTC_DIR, "cftc_2024.txt"),
  file.path(CFTC_DIR, "cftc_2025.txt"),
  file.path(CFTC_DIR, "cftc_2026.txt")
)

new_raw <- rbindlist(lapply(raw_files, parse_cftc_file), use.names = TRUE)
new_raw <- new_raw[!is.na(report_date)]
setorder(new_raw, report_date)
new_raw[, net_pos := mm_long - mm_short]
cat(sprintf("New raw CFTC: %d weeks (%s → %s)\n",
            nrow(new_raw), min(new_raw$report_date), max(new_raw$report_date)))

# Keep only dates after the existing data
cutoff <- max(existing_simple$report_date)
new_extended <- new_raw[report_date > cutoff, .(report_date, net_pos, pos_z = NA_real_)]

# ── 3. Combine and compute rolling z-score ────────────────────────────────────
cftc_all <- rbindlist(list(existing_simple, new_extended), use.names = TRUE, fill = TRUE)
setorder(cftc_all, report_date)
cftc_all <- unique(cftc_all, by = "report_date")  # deduplicate
cat(sprintf("Combined: %d weeks (%s → %s)\n",
            nrow(cftc_all), min(cftc_all$report_date), max(cftc_all$report_date)))

# Recompute 52-week rolling z-score from scratch using net_pos
cftc_all[, pos_z52_calc := {
  n <- .N
  z <- rep(NA_real_, n)
  for (i in 52:n) {
    window <- net_pos[(i-51):i]
    m <- mean(window, na.rm = TRUE); s <- sd(window, na.rm = TRUE)
    if (!is.na(s) && s > 0) z[i] <- (net_pos[i] - m) / s
  }
  z
}]

# Use existing pos_z where available, fill with recalculated where new
cftc_all[is.na(pos_z), pos_z := pos_z52_calc]
cftc_all[, pos_z52_calc := NULL]

# CFTC regime tag
cftc_all[, cftc_regime := fcase(
  pos_z >  1.5, "Extreme Long",
  pos_z >  0.5, "Mild Long",
  pos_z < -1.5, "Extreme Short",
  pos_z < -0.5, "Mild Short",
  default = "Neutral"
)]

# Overlay multiplier: contrarian at extremes, reinforcing in between
# Applied to positions: if MM is extreme, reduce position by 50% (caution)
cftc_all[, cftc_multiplier := fcase(
  pos_z >  1.5, 0.5,   # overcrowded long → be cautious on longs
  pos_z >  0.5, 1.0,   # mild long → neutral
  pos_z < -1.5, 0.5,   # overcrowded short → be cautious on shorts
  pos_z < -0.5, 1.0,   # mild short → neutral
  default = 1.0
)]
# Note: at extremes we multiply the signal-DIRECTION contribution, not just size
# full logic: cftc_dir = sign(-pos_z) when extreme (contrarian signal can reinforce)
# For simplicity here: use multiplier on existing signal strength
cftc_all[, cftc_signal := fcase(
  pos_z >  1.5, -1L,  # overcrowded long → contrarian bearish signal
  pos_z >  0.5,  0L,
  pos_z < -1.5,  1L,  # overcrowded short → contrarian bullish signal
  pos_z < -0.5,  0L,
  default = 0L
)]

cat("\nCFTC regime distribution:\n")
print(cftc_all[, .N, by = cftc_regime])
cat(sprintf("Latest pos_z: %.2f  regime: %s\n",
            cftc_all[.N, pos_z], cftc_all[.N, cftc_regime]))

# ── 4. Forward-fill to daily ──────────────────────────────────────────────────
date_spine <- data.table(date = seq.Date(min(cftc_all$report_date),
                                          as.Date("2026-07-06"), by = "day"))
cftc_all_r <- cftc_all[, .(date = report_date, net_pos, pos_z, cftc_regime,
                             cftc_multiplier, cftc_signal)]
daily <- merge(date_spine, cftc_all_r, by = "date", all.x = TRUE)
setorder(daily, date)

# Forward fill weekly data to every day
for (col in c("net_pos","pos_z","cftc_regime","cftc_multiplier","cftc_signal")) {
  if (is.character(daily[[col]])) {
    daily[, (col) := zoo::na.locf(get(col), na.rm = FALSE)]
  } else {
    daily[, (col) := zoo::na.locf(get(col), na.rm = FALSE)]
  }
}

# Only keep from 2021-01-01 onward (aligns with OOS signal window)
daily <- daily[date >= as.Date("2021-01-01")]

fwrite(daily, OUT)
cat(sprintf("\nWrote %d daily rows to %s\n", nrow(daily), OUT))
cat(sprintf("Date range: %s → %s\n", min(daily$date), max(daily$date)))
