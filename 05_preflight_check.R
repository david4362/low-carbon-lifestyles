# 05_preflight_check.R — TRE PRE-FLIGHT CHECK (run BEFORE the long pipeline)
# ---------------------------------------------------------------------------
# Purpose: in ~30 seconds, confirm the data loader produced everything the run
# needs — ESPECIALLY `sampling_frame`, whose absence silently skipped the
# sampling-IPW table in the Jun 2026 run and left STALE numbers in the SI.
#
# HOW TO USE (in the TRE RStudio Console, working dir = the scripts folder):
#   1. Make sure REAL_LOADER_DIR is set in 10_load_data.R (see TRE_RA_RUNBOOK.txt).
#   2. Run:  source("05_preflight_check.R")
#       (or  source("R/05_preflight_check.R") if scripts live under R/)
#   3. Read the verdict at the bottom:
#        - "PRE-FLIGHT: GO"     -> safe to source("master_analysis.R")
#        - "PRE-FLIGHT: NO-GO"  -> fix the listed problem first, then re-run this.
#
# This script LOADS DATA (it sources the foundation files + 10_load_data.R if
# the data objects are not already in memory). It writes nothing and changes no
# analysis state, so it is safe to run on its own.
# ---------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("TRE PRE-FLIGHT CHECK\n")
cat("=============================================================\n")

# --- Locate the scripts folder (mirror master_analysis.R's logic) ----------
.pf_dir <- {
  if      (file.exists("10_load_data.R"))      getwd()
  else if (file.exists("R/10_load_data.R"))    file.path(getwd(), "R")
  else if (file.exists("Code/10_load_data.R")) file.path(getwd(), "Code")
  else                                         NA_character_
}
if (is.na(.pf_dir))
  stop("Could not find 10_load_data.R from the working directory '", getwd(),
       "'. cd to the folder that holds the numbered scripts, then re-run.")

.pf_src <- function(f) {
  p <- file.path(.pf_dir, f)
  if (!file.exists(p)) stop("Missing required script: ", p)
  source(p, keep.source = TRUE)
}

# --- Load data only if it is not already in memory -------------------------
if (!exists("transactions") || !exists("sampling_frame")) {
  cat("\n[1/3] Loading data (sourcing 00_constants.R, 01_utils.R, 10_load_data.R)...\n")
  options(keep.source = TRUE)
  .pf_src("00_constants.R")
  .pf_src("01_utils.R")
  .pf_src("10_load_data.R")
} else {
  cat("\n[1/3] Data objects already in memory; skipping re-load.\n")
}

# --- Confirm we are on the REAL loader, not the local fallback -------------
cat("\n[2/3] Data source:\n")
.pf_real <- isTRUE(get0("USING_REAL_DATA", ifnotfound = NA))
if (is.na(.pf_real)) {
  cat("  ?  USING_REAL_DATA flag not set (older loader?). Cannot auto-confirm.\n")
  cat("     Check the messages above say 'Using real data loader: <path>'.\n")
} else if (.pf_real) {
  cat("  OK real data loader was used (USING_REAL_DATA = TRUE).\n")
} else {
  cat("  X  FELL BACK to local default_filter.RData (USING_REAL_DATA = FALSE).\n")
  cat("     Set REAL_LOADER_DIR in 10_load_data.R (see runbook section 2).\n")
}

# --- Check the objects the pipeline needs ----------------------------------
cat("\n[3/3] Required objects:\n")
.pf_required <- c("users", "survey", "transactions", "monthly_incomes",
                  "selected_aids", "sampling_frame")
.pf_problems <- character(0)

for (nm in .pf_required) {
  if (!exists(nm)) {
    cat(sprintf("  X  %-16s MISSING\n", nm))
    .pf_problems <- c(.pf_problems, paste0("missing object: ", nm))
  } else {
    obj <- get(nm)
    n   <- if (is.data.frame(obj)) nrow(obj) else length(obj)
    cat(sprintf("  OK %-16s present (%s rows/elements)\n", nm, format(n, big.mark = ",")))
  }
}

# --- Deep-check sampling_frame (the object that broke the Jun run) ----------
cat("\nsampling_frame detail (drives the sampling-IPW table):\n")
if (!exists("sampling_frame") || !is.data.frame(sampling_frame)) {
  cat("  X  sampling_frame is absent or not a data.frame.\n")
  cat("     The canonical loader must build it (~80k random-invitation frame).\n")
  .pf_problems <- c(.pf_problems, "sampling_frame absent / not a data.frame")
} else {
  .pf_need_cols <- c("aid", "postort", "age", "gender", "randomsample")
  .pf_has <- .pf_need_cols %in% names(sampling_frame)
  cat(sprintf("  rows: %s\n", format(nrow(sampling_frame), big.mark = ",")))
  for (i in seq_along(.pf_need_cols))
    cat(sprintf("  %s column '%s'\n", if (.pf_has[i]) "OK" else "X ", .pf_need_cols[i]))

  if (!all(.pf_has)) {
    .pf_problems <- c(.pf_problems,
      paste0("sampling_frame missing columns: ",
             paste(.pf_need_cols[!.pf_has], collapse = ", ")))
  }
  if (nrow(sampling_frame) < 50000) {
    cat("  !  WARNING: fewer than 50,000 rows; expected the ~80k invitation frame.\n")
    .pf_problems <- c(.pf_problems,
      sprintf("sampling_frame has only %s rows (expected ~80,000)",
              format(nrow(sampling_frame), big.mark = ",")))
  }
  if (all(.pf_has) && "randomsample" %in% names(sampling_frame)) {
    n_rs <- sum(sampling_frame$randomsample == 1, na.rm = TRUE)
    cat(sprintf("  randomsample == 1: %s rows\n", format(n_rs, big.mark = ",")))
    if (n_rs == 0)
      .pf_problems <- c(.pf_problems, "no rows with randomsample == 1")
  }
}

# --- Verdict ---------------------------------------------------------------
cat("\n-------------------------------------------------------------\n")
if (length(.pf_problems) == 0 && isTRUE(.pf_real)) {
  cat("PRE-FLIGHT: GO\n")
  cat("  All required objects present and sampling_frame looks valid.\n")
  cat("  You may now run:  source(\"master_analysis.R\")\n")
} else {
  cat("PRE-FLIGHT: NO-GO\n")
  if (!isTRUE(.pf_real))
    cat("  - Not confirmed on the real data loader (see [2/3] above).\n")
  for (p in .pf_problems) cat("  - ", p, "\n", sep = "")
  cat("  Fix the above, then re-run source(\"05_preflight_check.R\").\n")
  cat("  (master_analysis.R Step 6b will STOP on a real run if sampling_frame\n")
  cat("   is missing, so do not skip this.)\n")
}
cat("-------------------------------------------------------------\n\n")

suppressWarnings(rm(.pf_dir, .pf_src, .pf_real, .pf_required, .pf_problems,
                    .pf_need_cols, .pf_has))
