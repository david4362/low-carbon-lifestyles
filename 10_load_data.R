# load_data.R — DATA LOADER (sources the canonical TRE loader)
#
# The canonical loader (the script that actually reads the raw .dta data and
# builds `transactions`, `survey`, `users`, `monthly_incomes`, ...) lives in a
# separate folder inside the TRE. This file just points at it and sources it.
#
# =====================================================================
#  >>> ONE THING TO SET (TRE only): REAL_LOADER_DIR <<<
#
#  Set REAL_LOADER_DIR to the FOLDER that contains the real, canonical
#  load_data.R — i.e. the one that reads the raw .dta file.
#
#  As of the June 2026 run that folder is:
#    /safe/data/studie_konsumtion_och_attityder/chalmers/eriksson-code (copy)/eriksson-code
#
#  To find/confirm it, run this one line in the Console — the path it prints
#  marked "REAL" is the folder (drop the trailing /load_data.R):
#    sapply(list.files("/safe/data/studie_konsumtion_och_attityder",
#      pattern="^load_data\\.R$", recursive=TRUE, full.names=TRUE),
#      function(f) if (any(grepl("read_dta|\\.dta", readLines(f, warn=FALSE))))
#                    "REAL" else "redirect")
#
#  This should return TRUE once REAL_LOADER_DIR is set correctly:
#    file.exists(file.path(REAL_LOADER_DIR, "load_data.R"))
# =====================================================================
REAL_LOADER_DIR <- "/safe/data/studie_konsumtion_och_attityder/chalmers/eriksson-code (copy)/eriksson-code"

# An option/env override beats the hard-coded value above (handy if the folder
# ever moves): options(chalmers2.real_loader_dir = "/path/to/folder")  OR
# Sys.setenv(CHALMERS2_REAL_LOADER_DIR = "/path/to/folder").
.ov <- getOption("chalmers2.real_loader_dir", "")
if (!nzchar(.ov)) .ov <- Sys.getenv("CHALMERS2_REAL_LOADER_DIR", "")
if (nzchar(.ov)) REAL_LOADER_DIR <- .ov

.this_dir <- tryCatch(getSrcDirectory(function(){})[1], error = function(e) NA_character_)
if (is.na(.this_dir) || !nzchar(.this_dir)) .this_dir <- getwd()

.is_real_loader <- function(d) {
	if (is.null(d) || length(d) != 1 || is.na(d) || !nzchar(d)) return(FALSE)
	f <- file.path(d, "load_data.R")
	if (!file.exists(f)) return(FALSE)
	# Never source THIS file as if it were the canonical loader.
	normalizePath(f, mustWork = FALSE) != normalizePath(file.path(.this_dir, "load_data.R"), mustWork = FALSE)
}

if (.is_real_loader(REAL_LOADER_DIR)) {
	# --- Real data path (TRE) ----------------------------------------------
	.real <- normalizePath(file.path(REAL_LOADER_DIR, "load_data.R"), mustWork = TRUE)
	message("Using real data loader: ", .real)
	# keep.source = TRUE is ESSENTIAL: the canonical loader uses
	# getSrcDirectory() to locate broad-category.csv next to itself. Without
	# keep.source it resolves to NA and fails with 'NA/broad-category.csv'.
	source(.real, keep.source = TRUE)
	# Ground-truth flag for downstream steps (e.g. Step 6b sampling-IPW) to tell a
	# real TRE run from a mock/fallback run. Real run => missing objects are errors.
	USING_REAL_DATA <- TRUE
	suppressWarnings(rm(.this_dir, .real, .ov, .is_real_loader))
} else {
	# --- Local fallback (no raw data available) ----------------------------
	message("Real data loader not found at REAL_LOADER_DIR:")
	message("   ", REAL_LOADER_DIR)
	message("Falling back to local default_filter.RData (mock/cached data)...")
	local_rdata <- if (file.exists(file.path("R", "default_filter.RData"))) {
		file.path("R", "default_filter.RData")
	} else if (file.exists("default_filter.RData")) {
		"default_filter.RData"
	} else {
		stop("Could not find the real loader at REAL_LOADER_DIR, and no local ",
			 "default_filter.RData fallback exists. In the TRE, edit the ",
			 "REAL_LOADER_DIR line near the top of 10_load_data.R so that ",
			 "file.exists(file.path(REAL_LOADER_DIR, \"load_data.R\")) is TRUE.")
	}
	load(local_rdata)
	# Expect objects: transactions, survey, users, monthly_incomes, selected_months, selected_aids, etc.
	USING_REAL_DATA <- FALSE
	suppressWarnings(rm(.this_dir, .ov, .is_real_loader))

	# Load broad category lookups for local/synthetic data
	if (file.exists("broad-category.csv")) {
		broad_cat <- read.csv("broad-category.csv", stringsAsFactors = FALSE)
	}
	if (file.exists("broad-category-kr.csv")) {
		broad_cat_kr <- read.csv("broad-category-kr.csv", stringsAsFactors = FALSE)
	}
}

# --- Aviation emission-intensity override (applies after both branches) ---
# Keep local mock/fallback runs aligned with the manuscript aviation override.
AVIATION_COMBUSTION_CO2_PER_PKM <- 0.08
AVIATION_TICKET_SEK_PER_PKM <- 0.82
AVIATION_NON_CO2_UPLIFT <- 1.5
AVIATION_UPSTREAM_SHARE <- 0.20
AVIATION_CO2E_PER_SEK <- (
	AVIATION_COMBUSTION_CO2_PER_PKM * AVIATION_NON_CO2_UPLIFT +
	AVIATION_COMBUSTION_CO2_PER_PKM * AVIATION_UPSTREAM_SHARE
) / AVIATION_TICKET_SEK_PER_PKM

if (exists("transactions") && all(c("aviation.kr", "aviation.co2e") %in% names(transactions))) {
	aviation_idx <- !is.na(transactions$aviation.kr)
	transactions$aviation.co2e[aviation_idx] <-
		transactions$aviation.kr[aviation_idx] * AVIATION_CO2E_PER_SEK
}

if (exists("monthly_emissions") && exists("monthly_spending")) {
	aviation_spending <- monthly_spending |>
		dplyr::filter(category == "aviation.kr") |>
		dplyr::transmute(aid, date, aviation_co2e = kr * AVIATION_CO2E_PER_SEK)

	monthly_emissions <- monthly_emissions |>
		dplyr::left_join(aviation_spending, by = c("aid", "date")) |>
		dplyr::mutate(
			co2e = dplyr::if_else(
				category == "aviation.co2e" & !is.na(aviation_co2e),
				aviation_co2e,
				co2e
			)
		) |>
		dplyr::select(-aviation_co2e)
}
