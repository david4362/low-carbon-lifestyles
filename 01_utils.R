###############################################################################
# utils.R — reusable helpers. All logic that appeared >1 time in the pipeline.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(sandwich)
  library(lmtest)
  library(zoo)
  library(lubridate)
  library(collapse)
})

# ---------------------------------------------------------------------------
# Source a script from R/ (TRE layout) or current directory transparently.
# ---------------------------------------------------------------------------
source_R <- function(script) {
  candidates <- c(file.path("R", script), script)
  for (p in candidates) if (file.exists(p)) { source(p); return(invisible()) }
  stop(sprintf("Could not find %s in R/ or current directory", script))
}

# ---------------------------------------------------------------------------
# Person-level direct/indirect emissions or spending for the three lifestyles.
# Replaces ~10 hand-written `group_by(aid) |> summarise(direct_no_car = ...)`
# blocks scattered across interactions.R, master_analysis.R, deferred_*, etc.
# ---------------------------------------------------------------------------
aggregate_person_co2e <- function(emissions) {
  emissions |>
    fgroup_by(aid) |>
    fsummarise(
      total              = fsum(co2e),
      direct_no_car      = fsum(co2e * (broad_category == "Car_Public_co2e")),
      indirect_no_car    = fsum(co2e * (broad_category != "Car_Public_co2e")),
      direct_no_flying   = fsum(co2e * (broad_category == "Aviation_LDT_co2e")),
      indirect_no_flying = fsum(co2e * (broad_category != "Aviation_LDT_co2e")),
      direct_no_meat     = fsum(co2e * (category       == "groceries.co2e")),
      indirect_no_meat   = fsum(co2e * (category       != "groceries.co2e"))
    )
}

aggregate_person_kr <- function(spending, drop_non_cost = TRUE) {
  if (drop_non_cost) {
    spending <- spending |> filter(!(category %in% non_cost_categories))
  }
  spending |>
    fgroup_by(aid) |>
    fsummarise(
      total              = fsum(kr),
      direct_no_car      = fsum(kr * (broad_category == "Car_Public_kr")),
      indirect_no_car    = fsum(kr * (broad_category != "Car_Public_kr")),
      direct_no_flying   = fsum(kr * (broad_category == "Aviation_LDT_kr")),
      indirect_no_flying = fsum(kr * (broad_category != "Aviation_LDT_kr")),
      direct_no_meat     = fsum(kr * (category       == "groceries.kr")),
      indirect_no_meat   = fsum(kr * (category       != "groceries.kr"))
    )
}

# ---------------------------------------------------------------------------
# Annual person×category P99 winsorization (scales daily values down so the
# annual person-total equals the P99 cap). Works for emissions or spending.
# ---------------------------------------------------------------------------
winsorize_p99 <- function(df, value_col, target_categories, q = P99_QUANTILE_DEFAULT) {
  target_categories <- intersect(target_categories, unique(df$category))
  if (length(target_categories) == 0) return(list(data = df, thresholds = tibble(), n_capped = 0))

  # Avoid rlang !! unquoting (not supported by collapse's NSE);
  # use a temporary standardized column instead.
  df_tmp <- df
  df_tmp[[".__val__"]] <- df_tmp[[value_col]]

  annual <- df_tmp |>
    filter(category %in% target_categories) |>
    fgroup_by(aid, category) |>
    fsummarise(annual_total = fsum(.__val__))

  thresholds <- annual |>
    fgroup_by(category) |>
    fsummarise(p99 = quantile(annual_total, q))

  scale_factors <- annual |>
    left_join(thresholds, by = "category") |>
    filter(annual_total > p99) |>
    mutate(scale = p99 / annual_total) |>
    select(aid, category, scale)

  if (nrow(scale_factors) > 0) {
    df <- df |>
      left_join(scale_factors, by = c("aid", "category"))
    df[[value_col]] <- ifelse(!is.na(df$scale), df[[value_col]] * df$scale, df[[value_col]])
    df$scale <- NULL
  }
  list(data = df, thresholds = thresholds, n_capped = nrow(scale_factors))
}

# Convenience: apply both co2e and kr P99 caps as in master_analysis.R.
apply_p99_caps <- function(emissions, spending, output_dir = NULL) {
  cats_co2e <- intersect(
    c(grep("_other\\.co2e$", unique(emissions$category), value = TRUE),
      p99_target_suffix_co2e),
    unique(emissions$category)
  )
  cats_kr <- sub("\\.co2e$", ".kr", cats_co2e)

  c_em <- winsorize_p99(emissions, "co2e", cats_co2e)
  c_sp <- winsorize_p99(spending,  "kr",   cats_kr)

  if (!is.null(output_dir)) {
    log <- bind_rows(
      mutate(c_em$thresholds, type = "co2e"),
      mutate(c_sp$thresholds, type = "kr")
    )
    write.csv(log, file.path(output_dir, "p99_cap_thresholds.csv"), row.names = FALSE)
  }

  list(
    emissions = c_em$data, spending = c_sp$data,
    n_capped_co2e = c_em$n_capped, n_capped_kr = c_sp$n_capped,
    target_categories_co2e = cats_co2e
  )
}

# ---------------------------------------------------------------------------
# Credit-card-share exclusion: returns the set of aids whose
# transfer_to_creditcard share of consumption exceeds the threshold.
# ---------------------------------------------------------------------------
compute_cc_excluded_aids <- function(monthly_spending, aids, months,
                                     threshold = CC_SHARE_THRESHOLD_DEFAULT) {
  monthly_spending |>
    filter(aid %in% aids) |>
    semi_join(months, by = c("aid", "date")) |>
    fgroup_by(aid, category) |>
    fsummarise(kr = fsum(kr)) |>
    mutate(
      is_noncost = category %in% non_cost_categories,
      is_cc      = category == "transfer_to_creditcard.kr"
    ) |>
    fgroup_by(aid) |>
    fsummarise(
      consumption = fsum(kr * !is_noncost),
      cc          = fsum(kr * is_cc)
    ) |>
    mutate(share = if_else(consumption > 0, cc / consumption, 0)) |>
    filter(share > threshold) |>
    pull(aid)
}

# ---------------------------------------------------------------------------
# Rolling-12-month uncategorized-share month selection (the logic that lives
# in filter_data.R AND was duplicated in two robustness sections of master).
# ---------------------------------------------------------------------------
select_uncat_months <- function(monthly_spending,
                                threshold      = UNCAT_THRESHOLD_DEFAULT,
                                window         = CONSECUTIVE_MONTHS_DEFAULT,
                                min_consumption = 1000) {
  monthly_spending |>
    arrange(aid, date) |>
    group_by(aid, date) |>
    filter(sum(kr[!(category %in% non_cost_categories)]) > min_consumption) |>
    summarise(
      uncat_kr = sum(kr[category == "uncategorized.kr"]),
      total_kr = sum(kr[!(category %in% non_cost_categories)]),
      .groups  = "drop"
    ) |>
    group_by(aid,
             grp = cumsum(date - lag(date, default = as.Date("1900-01-01")) > 32)) |>
    mutate(
      uncat_sum   = zoo::rollsum(uncat_kr, k = window, fill = NA, align = "right"),
      total_sum   = zoo::rollsum(total_kr, k = window, fill = NA, align = "right"),
      uncat_share = if_else(total_sum > 0, uncat_sum / total_sum, NA_real_)
    ) |>
    filter(any(!is.na(uncat_share)), any(uncat_share <= threshold, na.rm = TRUE)) |>
    group_by(aid) |>
    mutate(target_month = max(date[uncat_share <= threshold], na.rm = TRUE)) |>
    filter(date <= target_month, date >= (target_month %m-% months(window - 1))) |>
    select(aid, date) |>
    ungroup()
}

# ---------------------------------------------------------------------------
# Robust regression: fit lm with optional weights, return tibble of HC3 robust
# coefficients with standardised column names. Optionally include glance stats.
# ---------------------------------------------------------------------------
fit_robust <- function(formula, data, weights = NULL, glance = FALSE) {
  call_args <- list(formula = formula, data = data)
  if (!is.null(weights)) call_args$weights <- weights
  m <- do.call(lm, call_args)
  V <- vcovHC(m, type = "HC3")
  ct <- coeftest(m, vcov. = V)
  out <- as.data.frame(ct[, , drop = FALSE])
  names(out) <- c("estimate", "stderr", "t", "p")
  out$variable <- rownames(out)
  out <- as_tibble(out)
  if (glance) {
    g <- broom::glance(m)
    out$r.squared     <- g$r.squared
    out$adj.r.squared <- g$adj.r.squared
    out$model_n       <- g$nobs
  }
  attr(out, "model") <- m
  attr(out, "vcov")  <- V
  out
}

# Standard interaction-model formula used everywhere:
#   outcome ~ esi * lifestyle + ctrl_vars
make_interaction_formula <- function(outcome, lifestyle, ctrl_vars) {
  rhs <- paste(c(paste("esi *", lifestyle), ctrl_vars), collapse = " + ")
  as.formula(paste(outcome, "~", rhs))
}

ctrl_var_names <- function(control_data) {
  setdiff(names(control_data), "aid")
}

# ---------------------------------------------------------------------------
# Build control_data + ESI for an arbitrary subset of aids. Used both for the
# main analysis and for the green-sample replication. Replaces ~120 lines of
# duplicated logic in stat_vars.R + master_analysis.R Step 12.
# ---------------------------------------------------------------------------
.age_group_table <- function() {
  bind_rows(
    tibble(age = 0:17,    age_group = "<18"),
    tibble(age = 18:29,   age_group = "18-29"),
    tibble(age = 30:44,   age_group = "30-44"),
    tibble(age = 45:65,   age_group = "45-65"),
    tibble(age = 66:120,  age_group = "65+")
  )
}

.set_reference_factor <- function(values, preferred_ref, all_levels) {
  present <- all_levels[all_levels %in% unique(values)]
  ref     <- if (preferred_ref %in% present) preferred_ref else present[[1]]
  factor(values, levels = c(ref, setdiff(all_levels, ref)))
}

build_control_data <- function(users, monthly_incomes, months, aids) {
  # Mean monthly income over the person's selected months. Using the mean
  # rather than the sum makes the control invariant to window length, so the
  # same definition is appropriate in robustness branches that vary the
  # window (e.g. the extended-window deferred-consumption tests).
  income_levels <- monthly_incomes |>
    filter(aid %in% aids) |>
    semi_join(months, by = c("aid", "date")) |>
    group_by(aid, date) |>
    summarise(income = sum(income, na.rm = TRUE), .groups = "drop") |>
    group_by(aid) |>
    summarise(
      monthly_income = if (all(is.na(income))) NA_real_
                       else mean(income, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      income_missing  = is.na(monthly_income),
      income_centered = monthly_income - mean(monthly_income, na.rm = TRUE)
    ) |>
    select(aid, income_centered, income_missing)

  users |>
    filter(aid %in% aids) |>
    mutate(pop_density_log_centered = log(pop_density) - mean(log(pop_density))) |>
    transmute(
      aid, age, sex,
      separate_house = profile.hometype == "Villa/Radhus",
      no_children    = profile.field_profile_household_children == 0,
      pop_density_log_centered,
      major_city     = profile.kommun %in% c("Göteborg", "Malmö", "Stockholm"),
      education = case_when(
        Sun2020Niva < 300 ~ "Grundskola",
        Sun2020Niva < 400 ~ "Gymnasium",
        Sun2020Niva < 500 ~ "Eftergymnasial <2 år",
        Sun2020Niva < 600 ~ "Eftergymnasial >=2 år",
        Sun2020Niva >= 600 ~ "Forskare"
      )
    ) |>
    left_join(income_levels, by = "aid") |>
    left_join(.age_group_table(), by = "age") |>
    select(-age) |>
    mutate(
      income_missing  = as.integer(replace_na(income_missing, TRUE)),
      income_centered = replace_na(income_centered, 0),
      education = .set_reference_factor(
        education, "Grundskola",
        c("Grundskola","Gymnasium","Eftergymnasial <2 år","Eftergymnasial >=2 år","Forskare")
      ),
      sex = .set_reference_factor(sex, "male", c("male", "female")),
      age_group = .set_reference_factor(
        age_group, "45-65", c("45-65", "18-29", "30-44", "65+")
      )
    )
}

# Build ESI scores (factor analysis on 3 survey items; standardised).
# Also retains `items` (raw item frame) and `score_mean`/`score_sd` so that
# scores from any other sample can be projected onto this factor solution
# via `project_esi()` (used for the green-sample replication).
build_esi <- function(survey, aids) {
  s     <- filter(survey, aid %in% aids) |> select(aid, array3_8, array3_9, array3_11)
  items <- s |> select(-aid)
  fa    <- psych::fa(items, rotate = "none")
  list(
    esi        = tibble(aid = s$aid, esi = as.numeric(scale(fa$scores))),
    fa         = fa,
    items      = items,
    score_mean = mean(fa$scores, na.rm = TRUE),
    score_sd   = sd(  fa$scores, na.rm = TRUE),
    alpha      = suppressWarnings(psych::alpha(items, warnings = FALSE))
  )
}

# Project a different sample's survey items onto an existing ESI factor
# solution (`main_esi` is the list returned by `build_esi()` on the main
# analytical sample). Returns a tibble of (aid, esi) on the same scale as
# the main-sample ESI: i.e. standardised using main-sample factor-score
# mean and SD, NOT re-standardised within the projected sample.
project_esi <- function(survey, aids, main_esi) {
  s <- filter(survey, aid %in% aids) |> select(aid, array3_8, array3_9, array3_11)
  raw_scores <- predict(main_esi$fa,
                        data     = s |> select(-aid),
                        old.data = main_esi$items)
  tibble(
    aid = s$aid,
    esi = as.numeric((raw_scores - main_esi$score_mean) / main_esi$score_sd)
  )
}

# Build target_data = lifestyle indicators + esi + control_data.
build_target_data <- function(users, emissions, esi_tbl, control_data) {
  non_flying <- emissions |>
    fgroup_by(aid) |>
    fsummarise(no_flying = fsum(co2e * (category == "aviation.co2e")) == 0)

  lifestyles <- users |>
    transmute(
      aid,
      no_car  = profile.ncars == 0,
      no_meat = profile.field_food_diet != "mixed"
    )

  control_data |>
    left_join(lifestyles, by = "aid") |>
    left_join(esi_tbl,    by = "aid") |>
    left_join(non_flying, by = "aid")
}

# ---------------------------------------------------------------------------
# Run main interaction regressions for the three lifestyles × {total, direct,
# indirect}. Returns a 9-row-per-coef tibble plus the named list of fitted
# lm objects.
# ---------------------------------------------------------------------------
run_interaction_models <- function(person_data, ctrl_vars, value = c("co2e", "kr")) {
  value <- match.arg(value)
  labels <- if (value == "co2e") interaction_model_labels else interaction_model_labels_kr
  models <- vector("list", 9); names(models) <- labels
  rows <- vector("list", 9)
  i <- 0
  for (ls in LIFESTYLES) {
    for (tp in c("total", "direct", "indirect")) {
      i <- i + 1
      dv <- if (tp == "total") "total" else paste0(tp, "_", ls)
      f  <- make_interaction_formula(dv, ls, ctrl_vars)
      rb <- fit_robust(f, person_data, glance = TRUE)
      models[[i]] <- attr(rb, "model")
      rows[[i]]   <- rb |>
        rename(term = variable, std.error = stderr, statistic = t, p.value = p) |>
        mutate(model_name = labels[[i]])
    }
  }
  list(models = models, df = bind_rows(rows))
}

# ---------------------------------------------------------------------------
# Standard filter for "headline" model coefficients: lifestyle main effects
# (no_carTRUE etc.), the ESI main effect, and ESI x lifestyle interactions.
# Centralises the regex used in master_analysis.R / 93_si_figures.R.
# ---------------------------------------------------------------------------
term_is_lifestyle <- function(x) grepl("TRUE$|^esi$|esi:", x)

# ---------------------------------------------------------------------------
# Generic robustness loop: fit `total` + `direct_<lf>` + `indirect_<lf>` for
# each lifestyle in `lifestyles` and return a long tibble. Used in Steps 6b,
# 11 and 12 of master_analysis.R, which had the same skeleton three times.
#
#   data        : person-level frame containing total/direct_*/indirect_*,
#                 esi, the lifestyle indicators, and ctrl_vars.
#   ctrl_vars   : character vector passed to make_interaction_formula().
#   weights     : optional numeric vector (e.g. IPW weights).
#   lifestyles  : which lifestyles to iterate over (default: LIFESTYLES).
#   outcomes    : which outcome types per lifestyle (default total/direct/
#                 indirect). "total" maps to dv "total"; "direct"/"indirect"
#                 to "direct_<lf>" / "indirect_<lf>".
#   extra_cols  : named list of scalars to attach to every output row
#                 (e.g. list(threshold = 0.10)).
# ---------------------------------------------------------------------------
run_lifestyle_models <- function(data, ctrl_vars, weights = NULL,
                                 lifestyles = LIFESTYLES,
                                 outcomes   = c("total", "direct", "indirect"),
                                 extra_cols = NULL) {
  rows <- list()
  N <- nrow(data)
  for (lf in lifestyles) {
    for (tp in outcomes) {
      dv <- if (tp == "total") "total" else paste0(tp, "_", lf)
      rb <- tryCatch(
        fit_robust(make_interaction_formula(dv, lf, ctrl_vars),
                   data, weights = weights),
        error = function(e) NULL
      )
      if (is.null(rb)) next
      rows[[length(rows) + 1L]] <- tibble(
        lifestyle = lf, outcome = dv, N = N,
        variable = rb$variable, estimate = rb$estimate,
        stderr = rb$stderr, t = rb$t, p = rb$p
      )
    }
  }
  out <- bind_rows(rows)
  if (!is.null(extra_cols) && nrow(out) > 0)
    out <- bind_cols(as_tibble(extra_cols)[rep(1, nrow(out)), , drop = FALSE], out)
  out
}
