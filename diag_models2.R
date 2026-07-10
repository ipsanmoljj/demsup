setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")

ms <- readRDS("output/sfm_models.rds")
cat("Class:", class(ms), "\n")
cat("Length:", length(ms), "\n")
cat("Names:", paste(names(ms), collapse=", "), "\n")

if (is.list(ms)) {
  for (nm in names(ms)) {
    cat("\n---", nm, "---\n")
    cat("  class:", class(ms[[nm]]), "\n")
    if (is.data.frame(ms[[nm]]) || inherits(ms[[nm]], "data.table")) {
      cat("  dims:", nrow(ms[[nm]]), "x", ncol(ms[[nm]]), "\n")
      cat("  cols:", paste(names(ms[[nm]]), collapse=", "), "\n")
    }
  }
}

# Check for model_ms.rds
if (file.exists("output/model_ms.rds")) {
  cat("\n--- model_ms.rds ---\n")
  ms2 <- readRDS("output/model_ms.rds")
  cat("Class:", class(ms2), "\n")
  if (is.environment(ms2)) {
    keys <- ls(ms2)
    cat("Total env keys:", length(keys), "\n")
    cat("Sample keys:\n", paste(head(keys, 20), collapse="\n"), "\n")
  } else {
    cat("Length:", length(ms2), "\n")
    cat("Names:", paste(head(names(ms2), 20), collapse=", "), "\n")
  }
}
