setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
library(data.table)
root <- getwd()
for (prod in c("CL","LCO","HO","LGO")) {
  f <- file.path(root, paste0(prod, "_data.csv"))
  if (!file.exists(f)) { cat(prod, "NOT FOUND\n"); next }
  full <- tryCatch(fread(f, skip=1, select=1L), error=function(e) NULL)
  if (!is.null(full)) {
    dates <- suppressWarnings(as.Date(full[[1]]))
    dates <- dates[!is.na(dates)]
    cat(prod, ": rows=", length(dates),
        " range=", format(min(dates)), "to", format(max(dates)), "\n")
  }
}
# Also check factor file
for (fn in c("output/factors_extended.csv","output/factors_combined.csv")) {
  if (!file.exists(fn)) next
  fac <- fread(fn, select="date")
  fac[, date := as.Date(date)]
  cat(fn, ": rows=", nrow(fac), " range=", format(min(fac$date)), "to", format(max(fac$date)), "\n")
}
