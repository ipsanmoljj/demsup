[1mdiff --git a/R/signal_engine.R b/R/signal_engine.R[m
[1mindex eb90bc8..bdb964e 100644[m
[1m--- a/R/signal_engine.R[m
[1m+++ b/R/signal_engine.R[m
[36m@@ -268,22 +268,14 @@[m [mrun_signal_engine <- function(products   = c("CL","LCO","HO","LGO"),[m
     cfg    <- PRODUCT_CONFIG[[prod]][m
     thresh <- SIGNAL_THRESHOLDS[prod][m
 [m
[31m-    # classify_regimes() (regime_classifier.R) now defaults to writing[m
[31m-    # regime_labels_<product>.csv under output/<product>/ rather than[m
[31m-    # directly under output/ — fixed 2026-06-17 alongside the cross-product[m
[31m-    # model-file collision bug (see regime_classifier.R's classify_regimes[m
[31m-    # docstring). Check the new per-product path first; fall back to the old[m
[31m-    # shared-output path for anyone running this against pre-fix CSVs that[m
[31m-    # haven't been regenerated yet, so this doesn't silently SKIP a product[m
[31m-    # just because the directory convention moved.[m
[31m-    labels_path_new <- file.path(output_dir, prod,[m
[31m-                                  paste0("regime_labels_", prod, ".csv"))[m
[31m-    labels_path_old <- file.path(output_dir,[m
[31m-                                  paste0("regime_labels_", prod, ".csv"))[m
[31m-    labels_path <- if (file.exists(labels_path_new)) labels_path_new else labels_path_old[m
[31m-[m
[32m+[m[32m    labels_path <- file.path(output_dir, prod,[m
[32m+[m[32m                              paste0("regime_labels_", prod, ".csv"))[m
[32m+[m[32m    if (!file.exists(labels_path)) {[m
[32m+[m[32m      labels_path <- file.path(output_dir,[m
[32m+[m[32m                                paste0("regime_labels_", prod, ".csv"))[m
[32m+[m[32m    }[m
     if (!file.exists(labels_path)) {[m
[31m-      cat("  SKIP: regime labels not found (checked", labels_path_new, "and", labels_path_old, ")\n\n"); next[m
[32m+[m[32m      cat("  SKIP: regime labels not found\n\n"); next[m
     }[m
 [m
     dt <- fread(labels_path)[m
[36m@@ -428,7 +420,6 @@[m [mrun_signal_engine <- function(products   = c("CL","LCO","HO","LGO"),[m
 [m
     # ── Live signal ─────────────────────────────────────────────────────────[m
     last <- tail(dt[!in_warmup & !is.na(level_z_126)], 1)[m
[31m-    live_signal <- NULL[m
     if (nrow(last) > 0) {[m
       cat(sprintf("  LIVE SIGNAL (%s):\n", format(last$date)))[m
       cat(sprintf("    Regime:    %s\n",      last$regime_label))[m
[36m@@ -439,24 +430,6 @@[m [mrun_signal_engine <- function(products   = c("CL","LCO","HO","LGO"),[m
       cat(sprintf("    Vol gate:  %s\n",[m
                   ifelse(last$atr_filter_pass,"PASS","BLOCKED")))[m
       sig <- last$signal[m
[31m-      # Structured version of the same live-signal block, returned to the[m
[31m-      # caller (e.g. plumber's /signal endpoint) instead of only printed —[m
[31m-      # this guarantees a served "live signal" is always identical to what[m
[31m-      # this validated function itself computed, with no separate[m
[31m-      # reimplementation of the gating logic anywhere else.[m
[31m-      live_signal <- list([m
[31m-        product          = prod,[m
[31m-        date             = as.character(last$date),[m
[31m-        regime           = last$regime_label,[m
[31m-        m1m2             = round(last$M1M2, 4),[m
[31m-        level_z          = round(last$level_z_126, 3),[m
[31m-        atr14            = if (is.na(last$atr14)) NA else round(last$atr14, 4),[m
[31m-        vol_gate_pass    = isTRUE(last$atr_filter_pass),[m
[31m-        vol_gate         = atr_gate,[m
[31m-        threshold        = thresh,[m
[31m-        signal           = sig,[m
[31m-        unit             = cfg$unit[m
[31m-      )[m
       if (sig != "FLAT") {[m
         stop_dist <- last$atr14 * cfg$atr_multiplier[m
         hard_stop <- last$M1M2 - ifelse(sig=="BUY",1,-1) * stop_dist[m
[36m@@ -464,20 +437,32 @@[m [mrun_signal_engine <- function(products   = c("CL","LCO","HO","LGO"),[m
         cat(sprintf("    Stop:      %.4f %s (%.1fx ATR)\n",[m
                     stop_dist, cfg$unit, cfg$atr_multiplier))[m
         cat(sprintf("    Hard stop: %.4f %s\n", hard_stop, cfg$unit))[m
[31m-        live_signal$stop_dist <- round(stop_dist, 4)[m
[31m-        live_signal$hard_stop <- round(hard_stop, 4)[m
[31m-        live_signal$atr_multiplier <- cfg$atr_multiplier[m
       } else {[m
         cat(sprintf("    Signal:    FLAT  (z=%+.3f  thresh=%.2f  gate=%s)\n",[m
                     last$level_z_126, thresh,[m
                     ifelse(last$atr_filter_pass,"pass","blocked")))[m
[31m-        live_signal$stop_dist <- NA[m
[31m-        live_signal$hard_stop <- NA[m
[31m-        live_signal$atr_multiplier <- cfg$atr_multiplier[m
       }[m
       cat("\n")[m
     }[m
 [m
[32m+[m[32m    # ── Live signal (structured, for plumber /signal endpoint) ─────────────[m
[32m+[m[32m    live_signal <- if (nrow(last) > 0) {[m
[32m+[m[32m      s     <- as.character(last$signal)[m
[32m+[m[32m      sdist <- if (s != "FLAT") round(as.numeric(last$atr14) * cfg$atr_multiplier, 4) else NA[m
[32m+[m[32m      hstop <- if (s != "FLAT") round(as.numeric(last$M1M2) - ifelse(s=="BUY",1,-1)*sdist, 4) else NA[m
[32m+[m[32m      list([m
[32m+[m[32m        date      = as.character(last$date),[m
[32m+[m[32m        signal    = s,[m
[32m+[m[32m        regime    = as.character(last$regime_label),[m
[32m+[m[32m        m1m2      = round(as.numeric(last$M1M2), 4),[m
[32m+[m[32m        z_score   = round(as.numeric(last$level_z_126), 3),[m
[32m+[m[32m        atr14     = round(ifelse(is.na(last$atr14), 0, as.numeric(last$atr14)), 4),[m
[32m+[m[32m        vol_gate  = ifelse(isTRUE(last$atr_filter_pass), "PASS", "BLOCKED"),[m
[32m+[m[32m        stop_dist = sdist,[m
[32m+[m[32m        hard_stop = hstop[m
[32m+[m[32m      )[m
[32m+[m[32m    } else NULL[m
[32m+[m
     # ── Save ────────────────────────────────────────────────────────────────[m
     fwrite(trades_dt,[m
            file.path(output_dir, paste0("trades_", prod, ".csv")))[m
