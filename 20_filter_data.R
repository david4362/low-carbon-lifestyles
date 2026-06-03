###############################################################################
# filter_data.R — identify analytical aids and months. Builds:
#   selected_aids, selected_months, monthly_spending, monthly_emissions,
#   filtered, output (dir).
#
# Configuration variables (label, consecutive_months, ...) may be overridden
# by the caller before sourcing this file.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(clock); library(collapse)
})

# Mock-mode short-circuit (preserved from previous version).
skip_filter_recompute <- isTRUE(get0("using_prefiltered_mock", ifnotfound = FALSE)) &&
  all(c("selected_aids","selected_months","monthly_spending","monthly_emissions") %in% ls())
if (!skip_filter_recompute)
  suppressPackageStartupMessages({ library(zoo); library(lubridate) })

# ---- Default configuration (override before sourcing) ----------------------
.cfg_defaults <- list(
  label                          = "no filtering",
  consecutive_months             = CONSECUTIVE_MONTHS_DEFAULT,
  max_cost_uncategorized         = UNCAT_THRESHOLD_DEFAULT,
  use_income_linkage_filter      = FALSE,
  full_month_buffer              = 3,
  max_adults                     = 1,
  covid_start                    = COVID_START_DEFAULT,
  covid_end                      = COVID_END_DEFAULT,
  use_random_sample              = TRUE,
  no_vegans                      = FALSE,
  no_uncategorized               = FALSE,
  no_credit_bills                = FALSE,
  no_separate_houses             = FALSE,
  no_unknown_edu                 = TRUE,
  apply_measurement_error_filters = FALSE,
  max_transfer_like_share        = 0.40,
  max_single_category_share      = 0.85
)
for (.nm in names(.cfg_defaults)) if (!exists(.nm)) assign(.nm, .cfg_defaults[[.nm]])

if (skip_filter_recompute) {
  if (!exists("output"))   output   <- file.path("output", "mock")
  if (!exists("filtered")) filtered <- list()
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  message("Mock mode: using pre-filtered objects from RData; skipping filter_data.R recomputation.")
} else {

  base_output_dir <- if (isTRUE(get0("using_mock_data", ifnotfound = FALSE)))
    get0("output", ifnotfound = file.path("output","mock")) else "output"
  output <- if (nzchar(label)) file.path(base_output_dir, label) else base_output_dir
  dir.create(output, recursive = TRUE, showWarnings = FALSE)

  all_aids <- if (exists("sample_aids")) sample_aids else users$aid
  attributes(all_aids) <- NULL

  # --- Restrict transactions: pre-app-download, non-COVID, full months -----
  selected_transactions <- transactions |>
    left_join(users |> select(aid, `user-created`), by = "aid") |>
    filter(date < `user-created`,
           date < covid_start | covid_end < date) |>
    group_by(aid) |>
    mutate(
      day_after_last     = add_days(max(date),  full_month_buffer),
      month_after_last   = date_build(get_year(day_after_last),  get_month(day_after_last)),
      day_before_first   = add_days(min(date), -full_month_buffer),
      month_before_first = date_build(get_year(day_before_first), get_month(day_before_first))
    ) |>
    filter(date < month_after_last, date > month_before_first) |>
    ungroup() |>
    select(-day_after_last, -month_after_last, -day_before_first, -month_before_first)

  if (no_uncategorized)
    selected_transactions <- filter(selected_transactions, !str_detect(category, "uncategorized"))
  if (no_credit_bills)
    selected_transactions <- filter(selected_transactions, !str_detect(category, "transfer_to_creditcard."))

  # --- Monthly aggregations ------------------------------------------------
  monthly_spending <- selected_transactions |>
    select(aid, date, ends_with(".kr")) |>
    mutate(date = date_build(get_year(date), get_month(date))) |>
    pivot(ids = c("aid","date")) |>
    rename(category = variable, kr = value) |>
    filter(category != "da_credit_.kr") |>
    fgroup_by(aid, date, category) |>
    fsummarise(kr = fsum(kr, na.rm = TRUE)) |>
    # collapse >= 2 returns NA (not 0) for an all-missing group; an absent
    # category in a month means zero spend, so coalesce back to 0 to match the
    # historical behaviour relied on by the month-selection sums below.
    mutate(kr = coalesce(kr, 0)) |>
    left_join(broad_cat_kr, by = "category")

  monthly_emissions <- selected_transactions |>
    select(aid, date, ends_with(".co2e")) |>
    pivot(ids = c("aid","date")) |>
    rename(category = variable, co2e = value) |>
    mutate(
      co2e = if_else(category %in% c("exclude.co2e","savings.co2e","charity.co2e"), 0, co2e),
      date = date_build(get_year(date), get_month(date))
    ) |>
    fgroup_by(aid, date, category) |>
    fsummarise(co2e = fsum(co2e, na.rm = TRUE)) |>
    mutate(co2e = coalesce(co2e, 0)) |>   # all-missing group -> 0 (see monthly_spending)
    left_join(broad_cat, by = "category")

  # --- Selected months: rolling 12-month uncat-share window ---------------
  selected_months <- monthly_spending |>
    arrange(aid, date) |>
    group_by(aid, date) |>
    summarise(
      uncat_kr = sum(kr[category == "uncategorized.kr"], na.rm = TRUE),
      total_kr = sum(kr[!(category %in% non_cost_categories)], na.rm = TRUE),
      .groups  = "drop"
    ) |>
    group_by(aid,
             grp = cumsum(date - lag(date, default = as.Date("1900-01-01")) > 32)) |>
    mutate(
      uncat_sum   = zoo::rollsum(uncat_kr, k = consecutive_months, fill = NA, align = "right"),
      total_sum   = zoo::rollsum(total_kr, k = consecutive_months, fill = NA, align = "right"),
      uncat_share = if_else(total_sum > 0, uncat_sum / total_sum, NA_real_)
    ) |>
    filter(any(!is.na(uncat_share)),
           any(uncat_share <= max_cost_uncategorized, na.rm = TRUE)) |>
    group_by(aid) |>
    mutate(target_month = max(date[uncat_share <= max_cost_uncategorized], na.rm = TRUE)) |>
    filter(date <= target_month,
           date >= (target_month %m-% months(consecutive_months - 1))) |>
    select(aid, date) |>
    ungroup()

  # --- Optional measurement-error month filter ----------------------------
  if (isTRUE(apply_measurement_error_filters)) {
    mq <- monthly_spending |>
      semi_join(selected_months, by = c("aid","date")) |>
      group_by(aid, date) |>
      summarise(
        cost_total    = sum(kr[!(category %in% non_cost_categories)], na.rm = TRUE),
        transfer_like = sum(kr[category %in% transfer_like_categories], na.rm = TRUE),
        max_cost_cat  = if (any(!(category %in% non_cost_categories)))
                          max(kr[!(category %in% non_cost_categories)], na.rm = TRUE) else NA_real_,
        .groups = "drop"
      ) |>
      mutate(
        transfer_like_share = if_else(cost_total > 0, transfer_like / cost_total, NA_real_),
        max_category_share  = if_else(cost_total > 0, max_cost_cat  / cost_total, NA_real_)
      )
    before <- nrow(selected_months)
    selected_months <- selected_months |>
      inner_join(
        filter(mq,
               is.finite(transfer_like_share), is.finite(max_category_share),
               transfer_like_share <= max_transfer_like_share,
               max_category_share  <= max_single_category_share) |>
          select(aid, date),
        by = c("aid","date")
      ) |>
      group_by(aid) |> filter(n() == consecutive_months) |> ungroup()
    cat(sprintf("Measurement-error filter: kept %d/%d months (%.1f%%)\n",
                nrow(selected_months), before,
                if (before > 0) 100*nrow(selected_months)/before else 0))
  }

  # --- Build per-filter aid sets ------------------------------------------
  pull_aids <- function(df) df |> distinct(aid) |> pull(aid)

  valid_aids_consecutivemonths <- monthly_spending |>
    semi_join(selected_months, by = c("aid","date")) |> pull_aids()
  valid_aids_adults <- users |>
    filter(profile.field_profile_household_adults <= max_adults) |> pull_aids()
  if (use_income_linkage_filter)
    valid_aids_income <- monthly_incomes |>
      right_join(selected_months, by = c("aid","date")) |>
      group_by(aid) |> filter(sum(is.na(income)) > 0) |> pull_aids()
  random_sample      <- if (use_random_sample) filter(users, randomsample == 1) |> pull_aids() else all_aids
  not_vegan          <- if (no_vegans)          filter(users, profile.field_food_diet != "vegan") |> pull_aids() else all_aids
  not_seperate_house <- if (no_separate_houses) filter(users, profile.hometype != "Villa/Radhus") |> pull_aids() else all_aids
  not_unknown_edu    <- if (no_unknown_edu)     filter(users, Sun2020Niva != 999) |> pull_aids() else all_aids

  filter_cats <- c(
    "valid_aids_adults", "valid_aids_consecutivemonths",
    if (use_income_linkage_filter) "valid_aids_income",
    "random_sample",
    if (no_vegans)          "not_vegan",
    if (no_separate_houses) "not_seperate_house",
                            "not_unknown_edu",
    "all_aids"
  )

  outersect <- function(x, y) sort(c(setdiff(x, y), setdiff(y, x)))
  filtered      <- lapply(mget(filter_cats), function(x) outersect(x, all_aids))
  selected_aids <- Reduce(intersect, mget(filter_cats))

  rm(selected_transactions, all_aids, valid_aids_adults, valid_aids_consecutivemonths,
     random_sample, not_vegan, not_seperate_house, not_unknown_edu, filter_cats,
     pull_aids, outersect, base_output_dir)
  gc()
}
