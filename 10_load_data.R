# load_data.R — REDIRECT
# The canonical version lives in chalmers2-shared/.
# Edit that file, not this one.
#
# This redirect exists so that  source("load_data.R")  from this Code/ folder
# continues to work unchanged, even when running non-interactively (Rscript).


# --- PATCH: Allow fallback to local loading if shared dir is missing ---
.this_dir <- tryCatch(getSrcDirectory(function(){})[1], error = function(e) NA_character_)
if (is.na(.this_dir) || !nzchar(.this_dir)) .this_dir <- getwd()
.shared <- normalizePath(file.path(.this_dir, "..", "..", "chalmers2-shared"), mustWork = FALSE)

if (dir.exists(.shared)) {
	# Use shared loader (TRE or local with shared dir)
	options(chalmers2.shared_dir = .shared)
	source(file.path(.shared, "load_data.R"))
	rm(.this_dir, .shared)
} else {
	message("Shared folder not found, using local fallback loader (default_filter.RData)...")
	# Try R/ directory first, then current directory
	local_rdata <- if (file.exists(file.path("R", "default_filter.RData"))) {
		file.path("R", "default_filter.RData")
	} else if (file.exists("default_filter.RData")) {
		"default_filter.RData"
	} else {
		stop("Could not find default_filter.RData in R/ or current directory!")
	}
	load(local_rdata)
	# Expect objects: transactions, survey, users, monthly_incomes, selected_months, selected_aids, etc.
	rm(.this_dir, .shared)
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
