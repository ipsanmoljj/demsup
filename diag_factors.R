library(data.table)
setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")

fac <- fread("output/factors_extended.csv")
fac[, date := as.Date(date)]
cat("Rows:", nrow(fac), "  Cols:", ncol(fac), "\n")
cat("Columns:\n")
cat(paste(names(fac), collapse="\n"), "\n")

cat("\n--- key EIA columns (last 5 Wednesday rows) ---\n")
fac <- fac[order(date)]
tr <- fac[weekdays(date)=="Wednesday"]
eia_cols <- grep("crude|cushing|gasoline|distillate|stock|chg|surprise", names(tr), value=TRUE, ignore.case=TRUE)
cat("EIA-related cols:", paste(eia_cols, collapse=", "), "\n")
cat("\nLast 5 Wednesday rows, selected cols:\n")
show_cols <- intersect(c("date", eia_cols), names(tr))
print(tail(tr[, ..show_cols], 5))
