library(data.table)
library(lubridate)
library(zoo)

# Load the pre-built spread CSVs
DATA_ROOT <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup"
REGIME_DIR <- file.path(DATA_ROOT, "output")

source("R/validate_live_feed.R")  # loads all functions

# Read CSVs directly
cl  <- fread(file.path(DATA_ROOT, "data/live/CL_live_m1m2.csv"))
co  <- fread(file.path(DATA_ROOT, "data/live/CO_live_m1m2.csv"))

# Compute signals
cl  <- .compute_signals(cl,  has_volume = TRUE)
co  <- .compute_signals(co,  has_volume = TRUE)

# Join regime labels
cl  <- .join_regime_live(cl,  file.path(REGIME_DIR, "regime_labels_CL.csv"))
co  <- .join_regime_live(co,  file.path(REGIME_DIR, "regime_labels_LCO.csv"))

# Show last 10 bars
cat("=== CL last 10 bars ===\n")
print(tail(cl[, .(timestamp, close, z_long, z_short, rsi, regime, atr14)], 10))

cat("\n=== CO last 10 bars ===\n")
print(tail(co[, .(timestamp, close, z_long, z_short, rsi, regime, atr14)], 10))

# Fire signals
cat("\n=== CL triggers ===\n")
print(.fire_signals(cl, "CL"))

cat("\n=== CO triggers ===\n")
print(.fire_signals(co, "CO"))