###############################################################################
# MASTER ANALYSIS SCRIPT (slimmed orchestrator)
# "Low-carbon lifestyles deliver broad climate benefits strengthened by
#  environmental self-identity"
#
# All cross-step helpers live in utils.R; constants in constants.R.
# All outputs land in `output/` (preserved filenames from prior version).
#
# Pipeline:
#   STEP 1   Load + filter + winsorize + stat_vars + interactions + regressions
#            + waterfall + S7
#   STEP 2   Sample-flow table
#   STEP 3   Table 1 / Table 4 (descriptives)
#   STEP 4   ESI diagnostics
#   STEP 5   Distributional KS tests
#   STEP 6   IPW robustness
#   STEP 6b  Sampling IPW (representativeness vs. random-invitation frame)
#   STEP 7   Rebound benchmark
#   STEP 7b  Non-adopter baselines
#   STEP 8   Income-quartile and urban/rural sensitivity (3-way interactions)
#   STEP 9   Uncategorized-threshold sensitivity
#   STEP 9b  P99 winsorization sensitivity ladder
#   STEP 10  Deferred-consumption tests
#   STEP 11  Recategorized transactions
#   STEP 12  Green-sample replication
#   STEP 13  Diet-definition robustness
#   STEP 14  SI residual diagnostics
#   STEP 14b Manuscript figures (interaction_plot, category_decomposition_esi, si_figures)
#   STEP 15  Key numbers + optional sync to manuscript Results/
###############################################################################

# ---- Working dir + packages -----------------------------------------------
script_dir <- tryCatch(getSrcDirectory(function(){})[1], error = function(e) NA_character_)
if (is.na(script_dir) || !nzchar(script_dir)) {
  if      (file.exists("10_load_data.R"))      script_dir <- getwd()
  else if (file.exists("R/10_load_data.R"))    script_dir <- file.path(getwd(), "R")
  else if (file.exists("Code/10_load_data.R")) script_dir <- file.path(getwd(), "Code")
}
if (!is.na(script_dir) && nzchar(script_dir)) setwd(script_dir)

required_packages <- c(
  "dplyr","stringr","purrr","tidyr","readr","haven","readxl","clock",
  "collapse","zoo","lubridate","ggplot2","jtools","interactions",
  "stargazer","broom","tidysdm","gt","boot","sandwich","lmtest",
  "car","gridExtra","psych"
)
missing <- required_packages[!vapply(required_packages, requireNamespace,
                                     quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing) > 0) stop("Missing packages: ", paste(missing, collapse = ", "))
invisible(lapply(required_packages, function(p)
  suppressPackageStartupMessages(suppressWarnings(library(p, character.only = TRUE)))))

cat("====== Starting master analysis ======\n\n")

# ---- Configuration switches -----------------------------------------------
SYNC_RESULTS_TO_MANUSCRIPT_DIR <- TRUE
EXPORT_CSV_TABLES_AS_PNG       <- TRUE
CSV_TABLE_ROWS_PER_PAGE        <- 35
CSV_TABLE_MAX_PAGES            <- 6
ANALYSIS_DATA_MODE             <- "auto"   # "auto" | "mock" | "tre"

if (ANALYSIS_DATA_MODE == "mock") {
  Sys.setenv(USE_MOCK_DATA = "1")
  if (exists("ANALYSIS_MOCK_DATA_FILE") && nzchar(ANALYSIS_MOCK_DATA_FILE))
    Sys.setenv(MOCK_DATA_FILE = ANALYSIS_MOCK_DATA_FILE)
} else if (ANALYSIS_DATA_MODE == "tre") {
  Sys.setenv(USE_MOCK_DATA = "0"); Sys.unsetenv("MOCK_DATA_FILE")
} else {
  Sys.unsetenv("USE_MOCK_DATA")
}
cat("Data mode:", ANALYSIS_DATA_MODE, "\n\n")

# ---- Source foundation files ----------------------------------------------
# (Plain source() — source_R() itself is defined in 01_utils.R)
.src <- function(f) {
  for (p in c(f, file.path("R", f))) if (file.exists(p)) { source(p); return(invisible()) }
  stop("Missing foundation file: ", f)
}
.src("00_constants.R")
.src("01_utils.R")

if (file.exists("02_csv_table_png.R") || file.exists("R/02_csv_table_png.R")) {
  source_R("02_csv_table_png.R")
} else {
  EXPORT_CSV_TABLES_AS_PNG <- FALSE
  message("02_csv_table_png.R not found; skipping PNG table export.")
}

# ===========================================================================
# STEP 1 — Main pipeline
# ===========================================================================
cat("--- [1/7] Loading data ---\n")
source_R("10_load_data.R")
if (!exists("output") || is.null(output) || !nzchar(output)) output <- "output"
dir.create(output, recursive = TRUE, showWarnings = FALSE)
cat(sprintf("  Users: %d | Transactions: %d | Survey: %d | Income records: %d\n",
            nrow(users), nrow(transactions), nrow(survey), nrow(monthly_incomes)))
cat("\n  Sample structure (randomsample values):\n")
print(table(users$randomsample, useNA = "always")); cat("\n")

cat("--- [2/7] Filtering data ---\n")
source_R("20_filter_data.R")
output_dir       <- output
sample_flow_data <- filtered
n_starting       <- nrow(users)
cat(sprintf("  Selected aids: %d | Month-user pairs: %d\n",
            length(selected_aids), nrow(selected_months)))

# Credit-card-share exclusion
cc_excluded_aids <- compute_cc_excluded_aids(monthly_spending, selected_aids,
                                             selected_months, CC_SHARE_THRESHOLD_DEFAULT)
cat(sprintf("  Credit card share > %.0f%%: excluding %d (%.1f%%)\n",
            CC_SHARE_THRESHOLD_DEFAULT * 100, length(cc_excluded_aids),
            100 * length(cc_excluded_aids) / length(selected_aids)))
selected_aids <- setdiff(selected_aids, cc_excluded_aids)
cat(sprintf("  Remaining sample after cc exclusion: %d\n", length(selected_aids)))

cat("--- [3/7] Building statistical variables ---\n")
source_R("30_stat_vars.R")
cat(sprintf("  Control variables: %d | ESI Cronbach's alpha: %.3f\n",
            ncol(control_data) - 1, env_alpha$total$raw_alpha))

# P99 winsorization
.caps <- apply_p99_caps(selected_emissions, selected_spending, output)
selected_emissions <- .caps$emissions
selected_spending  <- .caps$spending
cat(sprintf("  P99 caps applied to %d categories. Capped: %d (co2e), %d (kr)\n",
            length(.caps$target_categories_co2e),
            .caps$n_capped_co2e, .caps$n_capped_kr))

cat("--- [4/7] Running interaction regressions ---\n")
source_R("40_interactions.R")        # writes interaction regressions co2e/kr.csv/.txt
cat("--- [5/7] Running category regressions ---\n")
source_R("50_regressions.R")         # writes category regression co2e/kr.csv
cat("--- [6/7] Creating waterfall plot ---\n")
output <- output_dir; source_R("60_waterfall.R")
cat("--- [6b/7] Creating vertical waterfall plots ---\n")
output <- output_dir; source_R("61_waterfall_vertical.R")
cat("--- [7/7] Creating S7 plots ---\n")
output <- output_dir; source_R("70_S7.R")
output <- output_dir
cat("\n====== Main pipeline complete ======\nResults saved to:", output, "\n\n")

# ===========================================================================
# STEP 2 — Sample-flow table
# ===========================================================================
cat("--- Step 2: Sample Flow ---\n")
filter_labels <- c(
  "valid_aids_adults"            = "Household with >1 adult",
  "valid_aids_consecutivemonths" = "<12 consecutive qualifying months",
  "valid_aids_income"            = "Missing income linkage in selected months",
  "valid_aids_min_income"        = "Insufficient mean income (legacy filter)",
  "random_sample"                = "Not in random sample",
  "not_vegan"                    = "Vegan diet",
  "not_seperate_house"           = "Lives in separate house",
  "not_unknown_edu"              = "Unknown education level",
  "all_aids"                     = "Missing data across sources"
)
flow_order <- names(sample_flow_data)
all_ids <- users$aid; current_ids <- all_ids
flow_table <- data.frame(
  Filter = flow_order,
  N_excluded_step = integer(length(flow_order)),
  N_remaining     = integer(length(flow_order))
)
for (i in seq_along(flow_order)) {
  next_ids <- intersect(current_ids,
                        setdiff(all_ids, sample_flow_data[[flow_order[i]]]))
  flow_table$N_excluded_step[i] <- length(setdiff(current_ids, next_ids))
  flow_table$N_remaining[i]     <- length(next_ids)
  current_ids <- next_ids
}
flow_table$Description <- filter_labels[flow_table$Filter]
cat("  Starting N:", n_starting, "\n"); print(flow_table)
write.csv(flow_table, file.path(output, "sample_flow.csv"), row.names = FALSE)
cat("  Saved: sample_flow.csv\n\n")

# ===========================================================================
# STEP 3 — Table 1 + Table 4 (descriptives)
# ===========================================================================
cat("--- Step 3: Table 1 ---\n")
income_monthly_selected <- monthly_incomes |>
  filter(aid %in% selected_aids) |>
  group_by(aid, date) |>
  summarise(income = sum(income, na.rm = TRUE), .groups = "drop") |>
  semi_join(selected_months, by = c("aid", "date")) |>
  group_by(aid) |>
  summarise(monthly_income_selected_months = mean(income, na.rm = TRUE), .groups = "drop")

person_emissions <- aggregate_person_co2e(selected_emissions) |>
  transmute(aid, total_co2e = total)
person_spending  <- selected_spending  |> filter(!(category %in% non_cost_categories)) |>
  group_by(aid) |> summarise(total_kr = sum(kr, na.rm = TRUE), .groups = "drop")
person_months    <- selected_months    |> group_by(aid) |>
  summarise(n_months = n(), .groups = "drop")

desc_data <- target_data |>
  left_join(person_emissions, by = "aid") |>
  left_join(person_spending,  by = "aid") |>
  left_join(person_months,    by = "aid") |>
  left_join(income_monthly_selected, by = "aid") |>
  mutate(monthly_income = monthly_income_selected_months,
         monthly_co2e   = total_co2e / n_months,
         monthly_kr     = total_kr   / n_months)

cat("  N =", nrow(desc_data),
    "| Months/person:", round(mean(desc_data$n_months), 1),
    "+/-", round(sd(desc_data$n_months), 1), "\n")
for (lf in LIFESTYLES)
  cat(sprintf("  %s: %d (%.1f%%)\n", lf,
              sum(desc_data[[lf]]), 100 * mean(desc_data[[lf]])))

table1_continuous <- data.frame(
  Variable = c("Monthly CO2e (kg)","Monthly spending (SEK)",
               "Monthly income (SEK, SCB selected months)","ESI score","N months"),
  Mean   = c(mean(desc_data$monthly_co2e), mean(desc_data$monthly_kr),
             mean(desc_data$monthly_income_selected_months, na.rm = TRUE),
             mean(desc_data$esi, na.rm = TRUE), mean(desc_data$n_months)),
  SD     = c(sd(desc_data$monthly_co2e), sd(desc_data$monthly_kr),
             sd(desc_data$monthly_income_selected_months, na.rm = TRUE),
             sd(desc_data$esi, na.rm = TRUE), sd(desc_data$n_months)),
  Median = c(median(desc_data$monthly_co2e), median(desc_data$monthly_kr),
             median(desc_data$monthly_income_selected_months, na.rm = TRUE),
             median(desc_data$esi, na.rm = TRUE), median(desc_data$n_months))
)
table1_categorical <- data.frame(
  Variable = c(paste0("Sex: ",  names(table(desc_data$sex))),
               paste0("Age: ",  names(table(desc_data$age_group))),
               paste0("Edu: ",  names(table(desc_data$education))),
               "Car-free","Flight-free","Meat-free"),
  N = c(as.numeric(table(desc_data$sex)),
        as.numeric(table(desc_data$age_group)),
        as.numeric(table(desc_data$education)),
        sum(desc_data$no_car), sum(desc_data$no_flying), sum(desc_data$no_meat)),
  Percent = round(c(
    as.numeric(prop.table(table(desc_data$sex))) * 100,
    as.numeric(prop.table(table(desc_data$age_group))) * 100,
    as.numeric(prop.table(table(desc_data$education))) * 100,
    mean(desc_data$no_car) * 100, mean(desc_data$no_flying) * 100,
    mean(desc_data$no_meat) * 100), 1)
)
write.csv(table1_continuous,  file.path(output, "table1_continuous.csv"),  row.names = FALSE)
write.csv(table1_categorical, file.path(output, "table1_categorical.csv"), row.names = FALSE)

control_descriptives <- desc_data |> summarise(
  N = n(),
  female_share        = mean(sex == "female", na.rm = TRUE),
  age_18_29           = mean(age_group == "18-29", na.rm = TRUE),
  age_30_44           = mean(age_group == "30-44", na.rm = TRUE),
  age_45_65           = mean(age_group == "45-65", na.rm = TRUE),
  age_65_plus         = mean(age_group == "65+",   na.rm = TRUE),
  edu_grundskola      = mean(education == "Grundskola", na.rm = TRUE),
  edu_gymnasium       = mean(education == "Gymnasium",  na.rm = TRUE),
  edu_postsec_lt2     = mean(education == "Eftergymnasial <2 år",  na.rm = TRUE),
  edu_postsec_ge2     = mean(education == "Eftergymnasial >=2 år", na.rm = TRUE),
  edu_forskare        = mean(education == "Forskare",   na.rm = TRUE),
  no_children_share   = mean(no_children, na.rm = TRUE),
  separate_house_share= mean(separate_house, na.rm = TRUE),
  major_city_share    = mean(major_city, na.rm = TRUE),
  pop_density_log_centered_mean = mean(pop_density_log_centered, na.rm = TRUE),
  pop_density_log_centered_sd   = sd(pop_density_log_centered, na.rm = TRUE),
  income_selected_mean = mean(monthly_income_selected_months, na.rm = TRUE),
  income_selected_sd   = sd(monthly_income_selected_months, na.rm = TRUE)
)
write.csv(
  control_descriptives |> pivot_longer(everything(), names_to = "measure", values_to = "value"),
  file.path(output, "table4_controls_descriptives.csv"), row.names = FALSE
)
cat("  Saved: table1_continuous.csv, table1_categorical.csv, table4_controls_descriptives.csv\n\n")

# ===========================================================================
# STEP 4 — ESI diagnostics
# ===========================================================================
cat("--- Step 4: ESI Diagnostics ---\n")
cat(sprintf("  Cronbach's alpha (3 items): %.3f\n", env_alpha$total$raw_alpha))
cat(sprintf("    Drop array3_8:  %.3f\n", env_alpha_no8$total$raw_alpha))
cat(sprintf("    Drop array3_9:  %.3f\n", env_alpha_no9$total$raw_alpha))
cat(sprintf("    Drop array3_11: %.3f\n", env_alpha_no11$total$raw_alpha))
print(env_fa$loadings)
cat(sprintf("  Variance explained: %.1f%%\n\n  ESI by lifestyle:\n",
            env_fa$Vaccounted[2, 1] * 100))
for (lf in LIFESTYLES) {
  ad <- desc_data[desc_data[[lf]] == TRUE,  "esi", drop = TRUE]
  na <- desc_data[desc_data[[lf]] == FALSE, "esi", drop = TRUE]
  ad <- ad[is.finite(ad)]; na <- na[is.finite(na)]
  pv <- if (length(ad) >= 2 && length(na) >= 2)
    tryCatch(t.test(ad, na)$p.value, error = function(e) NA_real_) else NA_real_
  cat(sprintf("    %s: adopters %.2f (n=%d), non-adopters %.2f (n=%d), p=%s\n",
              lf, mean(ad), length(ad), mean(na), length(na),
              if (is.na(pv)) "NA" else sprintf("%.4f", pv)))
}
ggsave(file.path(output, "esi_distribution.png"),
       ggplot(desc_data |> filter(is.finite(esi)), aes(x = esi)) +
         geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
         theme_minimal() + labs(x = "ESI score (standardised)", y = "Count"),
       width = 16, height = 10, units = "cm")
cat("  Saved: esi_distribution.png\n\n")

# ===========================================================================
# STEP 5 — Distributional KS tests
# ===========================================================================
cat("--- Step 5: Distributional Analysis ---\n")
ctrl_vars <- ctrl_var_names(control_data)
person_indirect <- aggregate_person_co2e(selected_emissions) |>
  transmute(aid,
            indirect_car    = indirect_no_car,
            indirect_flying = indirect_no_flying,
            indirect_meat   = indirect_no_meat) |>
  left_join(target_data, by = "aid")

ks_results <- tibble()
for (lf in LIFESTYLES) {
  iv <- paste0("indirect_", sub("no_", "", lf))
  m <- lm(as.formula(paste(iv, "~", paste(ctrl_vars, collapse = "+"))),
          data = person_indirect)
  person_indirect$resid_indirect <- residuals(m)
  ad <- person_indirect$resid_indirect[person_indirect[[lf]] == TRUE]
  na <- person_indirect$resid_indirect[person_indirect[[lf]] == FALSE]
  if (length(ad) > 1 && length(na) > 1) {
    k <- ks.test(ad, na)
    sa <- round(100 * mean(ad > 0), 1); sn <- round(100 * mean(na > 0), 1)
    ks_results <- bind_rows(ks_results, tibble(
      Lifestyle = lf, KS_D = round(k$statistic, 4),
      KS_p = format.pval(k$p.value, digits = 4),
      Mean_adopters = round(mean(ad), 1), Mean_nonadopters = round(mean(na), 1),
      Median_adopters = round(median(ad), 1), Median_nonadopters = round(median(na), 1),
      Share_positive_adopters = sa, Share_positive_nonadopters = sn))
    cat(sprintf("  %s: KS D = %.4f, p = %s | %%positive: adopters=%.1f%%, non-adopters=%.1f%%\n",
                lf, k$statistic, format.pval(k$p.value), sa, sn))
  }
}
write.csv(ks_results, file.path(output, "distributional_ks_tests.csv"), row.names = FALSE)
cat("  Saved: distributional_ks_tests.csv\n\n")

# ===========================================================================
# STEP 6 — IPW robustness
# ===========================================================================
cat("--- Step 6: IPW Robustness ---\n")
ipw_results <- tibble()
for (lf in LIFESTYLES) {
  ps_data <- target_data |> filter(!is.na(esi))
  ps_model <- tryCatch(
    glm(as.formula(paste(lf, "~", paste(ctrl_vars, collapse = "+"))),
        data = ps_data, family = binomial("logit")),
    error = function(e) { cat("  Warning: PS model failed for", lf, "\n"); NULL })
  if (is.null(ps_model)) next
  ps    <- predict(ps_model, type = "response")
  treat <- as.logical(ps_data[[lf]])
  w     <- ifelse(treat, 1 / ps, 1 / (1 - ps))
  w     <- pmin(w, quantile(w, 0.99, na.rm = TRUE)); w <- w / mean(w)

  focal_field <- lifestyle_focal_co2e[[lf]]$field
  focal_value <- lifestyle_focal_co2e[[lf]]$value
  ipw_em <- selected_emissions |> group_by(aid) |>
    summarise(
      total_co2e    = sum(co2e),
      direct_co2e   = sum(co2e[.data[[focal_field]] == focal_value]),
      indirect_co2e = sum(co2e[.data[[focal_field]] != focal_value]),
      .groups = "drop"
    ) |>
    inner_join(ps_data |> mutate(ipw = w), by = "aid")

  for (dv in c("total_co2e", "direct_co2e", "indirect_co2e")) {
    rb <- fit_robust(make_interaction_formula(dv, lf, ctrl_vars), ipw_em, weights = ipw_em$ipw)
    ipw_results <- bind_rows(ipw_results,
      tibble(lifestyle = lf,
             model = if (dv == "total_co2e") "Total (IPW)" else if (dv == "direct_co2e") "Direct (IPW)" else "Indirect (IPW)",
             variable = rb$variable,
             estimate = rb$estimate, stderr = rb$stderr, t = rb$t, p = rb$p))
  }
  cat(sprintf("  %s: treated wt = %.2f, control wt = %.2f\n",
              lf, mean(w[treat]), mean(w[!treat])))
}
write.csv(ipw_results, file.path(output, "ipw_robustness.csv"), row.names = FALSE)
cat("  Saved: ipw_robustness.csv\n\n")

# ===========================================================================
# STEP 6b — Sampling IPW (representativeness vs. 80k random invitation frame)
# ===========================================================================
# The original survey was sent to ~80k individuals drawn at random from the
# Swedish population. About 4k accepted; further filtering yields the ~1.2k
# analytical sample. To probe sampling bias we fit a logistic model on the
# random-invitation frame:
#     Pr(analyzed = 1 | age_group, gender, major_city)
# and weight the analytical sample by 1/Pr(analyzed). Weights are trimmed at
# the 99th percentile and renormalised to mean 1, matching Step 6.
# The random-invitation frame is created in `10_load_data.R` as
# `sampling_frame`, with columns (postort, age, gender, randomsample, aid).
cat("--- Step 6b: Sampling IPW (representativeness) ---\n")

if (!exists("sampling_frame") || !is.data.frame(sampling_frame)) {
  cat("  Skipping: sampling_frame not available (mock mode).\n\n")
} else {

sframe_raw <- as_tibble(sampling_frame)
if ("efid" %in% names(sframe_raw) && !"aid" %in% names(sframe_raw))
  sframe_raw <- sframe_raw |> rename(aid = efid)
needed_cols <- c("aid", "postort", "age", "gender", "randomsample")
missing_cols <- setdiff(needed_cols, names(sframe_raw))
if (length(missing_cols) > 0)
  stop("Sampling frame missing required columns: ",
       paste(missing_cols, collapse = ", "))

sframe <- sframe_raw |>
  mutate(
    gender = haven::as_factor(gender) |> as.character(),
    gender = case_when(
      gender %in% c("man","male","Man","Male") ~ "male",
      gender %in% c("woman","female","kvinna","Woman","Female","Kvinna") ~ "female",
      TRUE ~ NA_character_
    ),
    postort = as.character(haven::as_factor(postort)),
    age     = as.numeric(age),
    analyzed = as.integer(aid %in% selected_aids)
  ) |>
  left_join(.age_group_table(), by = "age") |>
  mutate(
    major_city = postort %in% c("Göteborg", "Malmö", "Stockholm",
                                "GÖTEBORG", "MALMÖ", "STOCKHOLM")
  )

if (!"aid" %in% names(sframe))
  stop("Sampling frame must contain 'aid' to identify analyzed individuals.")

# Attrition diagnostic: how many analyzed individuals does each filter drop?
.step_drops <- function(df, mask, label) {
  n0  <- sum(df$analyzed)
  n1  <- sum(df$analyzed & mask, na.rm = TRUE)
  if (n0 - n1 > 0)
    cat(sprintf("    [filter] %-30s drops %d analyzed (%d -> %d)\n",
                label, n0 - n1, n0, n1))
  df[which(mask), , drop = FALSE]
}
sframe <- .step_drops(sframe, sframe$randomsample == 1 | sframe$analyzed == 1,
                      "randomsample == 1")
sframe <- .step_drops(sframe, !is.na(sframe$age),     "non-NA age")
sframe <- .step_drops(sframe, !is.na(sframe$gender),  "non-NA gender")
sframe <- .step_drops(sframe, !is.na(sframe$postort), "non-NA postort")
sframe <- .step_drops(sframe, !is.na(sframe$age_group) & sframe$age_group != "<18",
                      "age_group >= 18")
sframe <- sframe |>
  mutate(
    gender    = .set_reference_factor(gender, "male", c("male", "female")),
    age_group = .set_reference_factor(age_group, "45-65",
                                      c("45-65", "18-29", "30-44", "65+"))
  )

n_frame    <- nrow(sframe)
n_analyzed <- sum(sframe$analyzed)
n_target   <- length(intersect(sframe_raw$aid, selected_aids))
cat(sprintf("  Random-invitation frame: N = %d | analyzed = %d (%.2f%%)\n",
            n_frame, n_analyzed, 100 * n_analyzed / n_frame))
if (n_analyzed < n_target)
  cat(sprintf("  NOTE: %d of %d analyzed individuals in the sampling frame were dropped above.\n",
              n_target - n_analyzed, n_target))

ps_fit <- glm(analyzed ~ age_group + gender + major_city,
              data = sframe, family = binomial("logit"))
sframe$ps <- predict(ps_fit, type = "response")

# Diagnostics: balance of selection probability across cells
cell_balance <- sframe |>
  group_by(age_group, gender, major_city) |>
  summarise(n_frame = n(), n_analyzed = sum(analyzed),
            raw_pr  = mean(analyzed),
            mean_ps = mean(ps), .groups = "drop") |>
  arrange(desc(n_frame))
write.csv(cell_balance,
          file.path(output, "sampling_ipw_cell_balance.csv"),
          row.names = FALSE)

# Build sampling weights for the analyzed individuals
sw_tbl <- sframe |>
  filter(analyzed == 1) |>
  transmute(aid, ps, sw_raw = 1 / ps)
q99 <- quantile(sw_tbl$sw_raw, 0.99, na.rm = TRUE)
sw_tbl <- sw_tbl |>
  mutate(sw_trimmed = pmin(sw_raw, q99),
         sw         = sw_trimmed / mean(sw_trimmed, na.rm = TRUE))
cat(sprintf("  Weights: min = %.2f, median = %.2f, mean = %.2f, max = %.2f, P99 trim = %.2f\n",
            min(sw_tbl$sw), median(sw_tbl$sw), mean(sw_tbl$sw),
            max(sw_tbl$sw), q99))
cat(sprintf("  Effective sample size (Kish): %.0f / %d\n",
            sum(sw_tbl$sw)^2 / sum(sw_tbl$sw^2), nrow(sw_tbl)))

write.csv(sw_tbl, file.path(output, "sampling_ipw_weights.csv"),
          row.names = FALSE)

# Re-fit the 9 main interaction regressions with sampling weights
sipw_person_co2e <- aggregate_person_co2e(selected_emissions) |>
  inner_join(target_data, by = "aid") |>
  inner_join(sw_tbl |> select(aid, sw), by = "aid")

sipw_results <- run_lifestyle_models(sipw_person_co2e, ctrl_vars,
                                     weights = sipw_person_co2e$sw)
write.csv(sipw_results,
          file.path(output, "robustness_sampling_ipw.csv"),
          row.names = FALSE)

# Headline comparison vs. unweighted main results
unw <- read.csv(file.path(output, "interaction regressions co2e.csv")) |>
  select(model_name, term, estimate_unw = estimate, p_unw = p.value)
outcome_to_model <- list(
  "no_car"    = list(total = "Total emissions Car ownership",
                     direct = "Direct emissions Car ownership",
                     indirect = "Indirect emissions Car ownership"),
  "no_flying" = list(total = "Total emissions Air travel",
                     direct = "Direct emissions Air travel",
                     indirect = "Indirect emissions Air travel"),
  "no_meat"   = list(total = "Total emissions Diet",
                     direct = "Direct emissions Diet",
                     indirect = "Indirect emissions Diet")
)
sipw_compare <- sipw_results |>
  mutate(
    outcome_kind = case_when(outcome == "total" ~ "total",
                             grepl("^direct_",   outcome) ~ "direct",
                             grepl("^indirect_", outcome) ~ "indirect"),
    model_name = vapply(seq_len(n()), function(i)
      outcome_to_model[[lifestyle[i]]][[outcome_kind[i]]], character(1))
  ) |>
  rename(term = variable, estimate_ipw = estimate, p_ipw = p) |>
  inner_join(unw, by = c("model_name", "term")) |>
  filter(term_is_lifestyle(term)) |>
  mutate(
    delta_abs = estimate_ipw - estimate_unw,
    delta_pct = ifelse(abs(estimate_unw) > 0,
                       100 * delta_abs / abs(estimate_unw), NA_real_),
    sign_flip = sign(estimate_ipw) != sign(estimate_unw),
    sig_flip  = (p_unw < 0.05) != (p_ipw < 0.05)
  ) |>
  select(lifestyle, model_name, term,
         estimate_unw, estimate_ipw, delta_abs, delta_pct,
         p_unw, p_ipw, sign_flip, sig_flip)
write.csv(sipw_compare,
          file.path(output, "robustness_sampling_ipw_summary.csv"),
          row.names = FALSE)

n_sign <- sum(sipw_compare$sign_flip, na.rm = TRUE)
n_sig  <- sum(sipw_compare$sig_flip,  na.rm = TRUE)
max_d  <- max(abs(sipw_compare$delta_pct), na.rm = TRUE)
cat(sprintf("  Headline vs. unweighted: sign_flip=%d, sig_flip=%d, max|delta|=%.0f%%\n",
            n_sign, n_sig, max_d))
cat("  Saved: sampling_ipw_weights.csv, sampling_ipw_cell_balance.csv,\n")
cat("         robustness_sampling_ipw.csv, robustness_sampling_ipw_summary.csv\n")

rm(sframe_raw, sframe, ps_fit, sw_tbl, sipw_person_co2e, sipw_results,
   unw, sipw_compare, cell_balance)
gc()
cat("\n")

} # end sampling_frame check

# ===========================================================================
# STEP 7 — Proportional re-spending benchmark
# ===========================================================================
cat("--- Step 7: Proportional Re-spending Benchmark ---\n")
lm_coefs_co2e <- read.csv(file.path(output, "interaction regressions co2e.csv")) |>
  select(-any_of("X"))
lm_coefs_kr   <- read.csv(file.path(output, "interaction regressions kr.csv")) |>
  select(-any_of("X"))

.coef_pull <- function(df, model_pat) df |>
  filter(grepl(model_pat, model_name),
         grepl("no_car|no_flying|no_meat", term),
         !grepl("esi", term)) |>
  mutate(lifestyle = case_when(grepl("car",    term) ~ "no_car",
                               grepl("flying", term) ~ "no_flying",
                               grepl("meat",   term) ~ "no_meat"))

direct_kr_reduction <- .coef_pull(lm_coefs_kr,   "Direct")
indirect_co2e_obs   <- .coef_pull(lm_coefs_co2e, "Indirect")

intensity_data <- selected_spending |>
  filter(!category %in% non_purchase_categories) |>
  group_by(aid) |>
  summarise(indirect_car_kr    = sum(kr[broad_category != "Car_Public_kr"]),
            indirect_flying_kr = sum(kr[broad_category != "Aviation_LDT_kr"]),
            indirect_meat_kr   = sum(kr[category       != "groceries.kr"]),
            .groups = "drop") |>
  left_join(
    selected_emissions |> group_by(aid) |>
      summarise(indirect_car_co2e    = sum(co2e[broad_category != "Car_Public_co2e"]),
                indirect_flying_co2e = sum(co2e[broad_category != "Aviation_LDT_co2e"]),
                indirect_meat_co2e   = sum(co2e[category       != "groceries.co2e"]),
                .groups = "drop"), by = "aid") |>
  left_join(target_data |> select(aid, no_car, no_flying, no_meat), by = "aid")

benchmark_table <- data.frame()
for (lf in LIFESTYLES) {
  s <- sub("no_", "", lf)
  na <- intensity_data[intensity_data[[lf]] == FALSE, ]
  intensity <- sum(na[[paste0("indirect_", s, "_co2e")]]) /
               sum(na[[paste0("indirect_", s, "_kr")]])
  dv <- direct_kr_reduction |> filter(lifestyle == lf) |> pull(estimate)
  ov <- indirect_co2e_obs   |> filter(lifestyle == lf) |> pull(estimate)
  ds <- if (length(dv) > 0) -dv[[1]] else NA_real_
  obs <- if (length(ov) > 0) ov[[1]] else NA_real_
  pred <- ds * intensity
  benchmark_table <- rbind(benchmark_table, data.frame(
    Lifestyle = lf,
    Direct_saving_SEK     = round(ds, 0),
    CO2e_per_SEK_indirect = round(intensity, 4),
    Predicted_rebound_kg  = round(pred, 0),
    Observed_indirect_kg  = round(obs, 0),
    Gap_kg                = round(obs - pred, 0)))
  cat(sprintf("  %s: direct %d SEK | benchmark %+d kg | observed %+d kg | gap %+d kg\n",
              lf, round(ds), round(pred), round(obs), round(obs - pred)))
}
write.csv(benchmark_table, file.path(output, "rebound_benchmark.csv"), row.names = FALSE)
cat("  Saved: rebound_benchmark.csv\n\n")

# ===========================================================================
# STEP 7b — Non-adopter baseline footprints (predicted at ESI = 0)
# ===========================================================================
cat("--- Step 7b: Non-adopter baseline footprints ---\n")
lifestyle_map <- list(
  no_car    = list(label = "Car owners",  model = "Total emissions Car ownership"),
  no_flying = list(label = "Flyers",      model = "Total emissions Air travel"),
  no_meat   = list(label = "Meat eaters", model = "Total emissions Diet")
)
non_adopter_baselines <- data.frame()
# Build a "typical non-adopter" prediction row from the model frame:
#   - factors      -> reference level (first level); construction in
#                     build_control_data ensures this is the documented baseline.
#   - logicals     -> FALSE (will be overridden for the lifestyle indicator).
#   - characters   -> alphabetically first (sentinel; should not occur for
#                     control variables, which are explicitly factored).
#   - numerics     -> sample mean (already centered for income / pop_density).
# This avoids the previous "row[1]" hack, whose factor levels depended on
# arbitrary row order in the model frame.
.typical_row <- function(model) {
  mf  <- model$model
  rsp <- all.vars(formula(model))[1]
  preds <- setdiff(names(mf), rsp)
  out <- lapply(preds, function(nm) {
    v <- mf[[nm]]
    if      (is.factor(v))    factor(levels(v)[1], levels = levels(v))
    else if (is.logical(v))   FALSE
    else if (is.character(v)) sort(unique(v))[1]
    else                       mean(v, na.rm = TRUE)
  })
  names(out) <- preds
  as.data.frame(out, stringsAsFactors = FALSE)
}
for (lf in names(lifestyle_map)) {
  info <- lifestyle_map[[lf]]
  na_data <- aggregate_person_co2e(selected_emissions) |>
    transmute(aid, annual_co2e = total) |>
    left_join(target_data |> select(aid, all_of(lf)), by = "aid") |>
    filter(.data[[lf]] == FALSE)
  raw_mean <- mean(na_data$annual_co2e, na.rm = TRUE)
  mod      <- lm_models[[info$model]]
  pred_row <- .typical_row(mod)
  if (lf %in% names(pred_row))    pred_row[[lf]] <- FALSE
  if ("esi" %in% names(pred_row)) pred_row[["esi"]] <- 0
  pm <- predict(mod, newdata = pred_row)[[1]]
  non_adopter_baselines <- rbind(non_adopter_baselines, data.frame(
    lifestyle = lf, group_label = info$label, n = nrow(na_data),
    raw_mean_co2e = round(raw_mean, 0), predicted_mean_co2e = round(pm, 0),
    predicted_co2e_t = pm / 1000))
  cat(sprintf("  %s (N=%d): raw %d kg, predicted at ESI=0 = %d kg (%.1ft)\n",
              info$label, nrow(na_data), round(raw_mean), round(pm), pm / 1000))
}
write.csv(non_adopter_baselines,
          file.path(output, "non_adopter_baselines.csv"), row.names = FALSE)
cat("  Saved: non_adopter_baselines.csv\n\n")

# ===========================================================================
# STEP 8 — Income-quartile and urban/rural sensitivity
# ===========================================================================
.run_threeway <- function(moderator_var, file_label, sens_data) {
  cat(sprintf("--- Step 8%s: %s 3-way interaction ---\n",
              if (moderator_var == "factor(income_quartile)") "" else "b",
              if (moderator_var == "factor(income_quartile)") "Income Quartile" else "Major City"))
  out <- tibble()
  # Drop the control that is collinear with the moderator: major_city when
  # urban/rural is the moderator; income_centered when income quartile is the
  # moderator (both derive from monthly_income_selected_months).
  ctrl <- if (moderator_var == "major_city") setdiff(ctrl_vars, "major_city")
          else if (moderator_var == "factor(income_quartile)")
            setdiff(ctrl_vars, c("income_centered", "income_missing"))
          else ctrl_vars
  for (lf in LIFESTYLES) {
    f <- as.formula(paste("total_co2e ~ esi *", lf, "*", moderator_var, "+",
                          paste(ctrl, collapse = "+")))
    rb <- tryCatch(fit_robust(f, sens_data), error = function(e) {
      cat("  Warning: model failed for", lf, "\n"); NULL })
    if (is.null(rb)) next
    out <- bind_rows(out, tibble(lifestyle = lf, N = nrow(sens_data),
                                 variable = rb$variable, estimate = rb$estimate,
                                 stderr = rb$stderr, t = rb$t, p = rb$p))
    tw <- rb |> filter(grepl(":", variable),
                       grepl(sub("factor\\((.*)\\)", "\\1", moderator_var), variable),
                       grepl("esi", variable))
    if (nrow(tw) > 0) {
      cat(sprintf("  %s: 3-way interactions (lifestyle x ESI x %s):\n",
                  lf, sub("factor\\((.*)\\)", "\\1", moderator_var)))
      for (i in seq_len(nrow(tw)))
        cat(sprintf("    %s: est = %.1f, p = %.4f\n",
                    tw$variable[i], tw$estimate[i], tw$p[i]))
    }
  }
  write.csv(out, file.path(output, file_label), row.names = FALSE)
  cat(sprintf("  Saved: %s\n\n", file_label))
}

income_quartiles <- desc_data |>
  transmute(aid, monthly_income = monthly_income_selected_months) |>
  filter(!is.na(monthly_income)) |>
  mutate(income_quartile = ntile(monthly_income, 4)) |>
  select(aid, income_quartile)
sensitivity_data <- selected_emissions |> group_by(aid) |>
  summarise(total_co2e = sum(co2e), .groups = "drop") |>
  left_join(target_data, by = "aid") |>
  left_join(income_quartiles, by = "aid")
.run_threeway("factor(income_quartile)", "sensitivity_income_quartile.csv", sensitivity_data)
.run_threeway("major_city",              "sensitivity_urban_rural.csv",     sensitivity_data)

# ===========================================================================
# STEP 9 — Uncategorized-threshold sensitivity
# ===========================================================================
cat("--- Step 9: Uncategorized Threshold Sensitivity ---\n")
uncat_results <- tibble()
for (thresh in c(0.05, 0.15, 0.20)) {
  cat(sprintf("  Running with max_cost_uncategorized = %.0f%%...\n", thresh * 100))
  uncat_months <- select_uncat_months(monthly_spending, threshold = thresh)
  uncat_aids   <- intersect(unique(uncat_months$aid), selected_aids)
  uncat_em <- selected_emissions |> filter(aid %in% uncat_aids) |>
    semi_join(uncat_months, by = c("aid", "date"))
  uncat_person <- aggregate_person_co2e(uncat_em) |>
    rename(total = total) |>
    inner_join(target_data, by = "aid")
  cat(sprintf("    N = %d at %.0f%% threshold\n", nrow(uncat_person), thresh * 100))
  if (nrow(uncat_person) < 50) { cat("    Skipping (too few)\n"); next }
  for (lf in LIFESTYLES) {
    for (dv in c("total", paste0("indirect_", lf))) {
      rb <- tryCatch(fit_robust(make_interaction_formula(dv, lf, ctrl_vars),
                                uncat_person), error = function(e) NULL)
      if (is.null(rb)) next
      key <- grepl(paste0(lf, "TRUE|esi:", lf, "|", lf, "TRUE:esi"), rb$variable)
      if (any(key))
        uncat_results <- bind_rows(uncat_results,
          tibble(threshold = thresh, lifestyle = lf, outcome = dv,
                 N = nrow(uncat_person), variable = rb$variable[key],
                 estimate = rb$estimate[key], stderr = rb$stderr[key],
                 t = rb$t[key], p = rb$p[key]))
    }
  }
}
write.csv(uncat_results, file.path(output, "sensitivity_uncat_threshold.csv"), row.names = FALSE)
cat("  Saved: sensitivity_uncat_threshold.csv\n\n")

# ===========================================================================
# STEP 9b — P99 winsorization sensitivity ladder
# ===========================================================================
# Reviewers will reasonably ask whether headline lifestyle*ESI coefficients
# survive different winsorization choices. We re-fit the 9 main interaction
# models under a ladder of cap policies, from "no capping" to "cap everything",
# plus a person-total-cap variant that controls influence at the person level
# rather than per category. Output: one row per (specification × model × term)
# in `sensitivity_p99_ladder.csv` with side-by-side comparison vs. the headline
# (specification = "headline_residual_plus_financial").
cat("--- Step 9b: P99 winsorization sensitivity ladder ---\n")

.em_raw <- monthly_emissions |>
  filter(aid %in% selected_aids) |>
  semi_join(selected_months, by = c("aid", "date"))

.headline_cats <- intersect(
  c(grep("_other\\.co2e$", unique(.em_raw$category), value = TRUE),
    p99_target_suffix_co2e),
  unique(.em_raw$category)
)
.focal_cats <- intersect(
  unique(c(monthly_emissions |>
             filter(broad_category %in% c("Car_Public_co2e", "Aviation_LDT_co2e")) |>
             pull(category) |> unique(),
           "groceries.co2e")),
  unique(.em_raw$category)
)
.all_cats <- unique(.em_raw$category)

# Cap the *person-level total* rather than per-category: scale every monthly
# emission of an over-cap person uniformly so their annual total equals the
# 99th percentile of annual totals. Equivalent to influence-point control.
.cap_person_total <- function(em) {
  totals <- em |> fgroup_by(aid) |>
    fsummarise(annual_total = fsum(co2e))
  cap <- quantile(totals$annual_total, P99_QUANTILE_DEFAULT, na.rm = TRUE)
  scales <- totals |> filter(annual_total > cap) |>
    mutate(scale = cap / annual_total) |> select(aid, scale)
  if (nrow(scales) == 0) return(list(data = em, n_capped = 0))
  capped <- em |>
    left_join(scales, by = "aid") |>
    mutate(co2e = if_else(!is.na(scale), co2e * scale, co2e)) |>
    select(-scale)
  list(data = capped, n_capped = nrow(scales))
}

.spec_defs <- list(
  none                            = list(kind = "per_cat", cats = character()),
  headline_residual_plus_financial = list(kind = "per_cat", cats = .headline_cats),
  headline_plus_focal             = list(kind = "per_cat",
                                         cats = unique(c(.headline_cats, .focal_cats))),
  all_categories                  = list(kind = "per_cat", cats = .all_cats),
  person_total                    = list(kind = "person",  cats = NULL)
)

.fit_spec <- function(spec_name, spec) {
  if (spec$kind == "person") {
    capped <- .cap_person_total(.em_raw)
  } else {
    capped <- if (length(spec$cats) == 0) list(data = .em_raw, n_capped = 0)
              else winsorize_p99(.em_raw, "co2e", spec$cats)
  }
  dat <- aggregate_person_co2e(capped$data) |> left_join(target_data, by = "aid")
  res <- run_interaction_models(dat, ctrl_vars, value = "co2e")$df
  list(df = res, n_capped = capped$n_capped,
       n_target_cats = if (spec$kind == "per_cat") length(spec$cats) else NA_integer_)
}

.ladder <- list()
for (nm in names(.spec_defs)) {
  fs <- .fit_spec(nm, .spec_defs[[nm]])
  cat(sprintf("  %-32s n_capped = %5d, target_cats = %s\n",
              nm, fs$n_capped,
              if (is.na(fs$n_target_cats)) "person-total" else fs$n_target_cats))
  .ladder[[nm]] <- fs$df |>
    transmute(specification = nm, model_name, term,
              estimate, std.error, p.value)
}
.ladder_df <- bind_rows(.ladder) |>
  filter(term_is_lifestyle(term))

.head_ref <- .ladder_df |>
  filter(specification == "headline_residual_plus_financial") |>
  transmute(model_name, term, estimate_head = estimate, p_head = p.value)

.ladder_out <- .ladder_df |>
  inner_join(.head_ref, by = c("model_name", "term")) |>
  mutate(delta_abs = estimate - estimate_head,
         delta_pct = ifelse(abs(estimate_head) > 0,
                            100 * delta_abs / abs(estimate_head), NA_real_),
         sign_flip = sign(estimate) != sign(estimate_head),
         sig_flip  = (p_head < 0.05) != (p.value < 0.05)) |>
  select(specification, model_name, term,
         estimate_head, estimate, delta_abs, delta_pct,
         p_head, p.value, sign_flip, sig_flip)

write.csv(.ladder_out,
          file.path(output, "sensitivity_p99_ladder.csv"),
          row.names = FALSE)

.flip_summary <- .ladder_out |>
  filter(specification != "headline_residual_plus_financial") |>
  group_by(specification) |>
  summarise(n_terms   = n(),
            n_sign_flip = sum(sign_flip, na.rm = TRUE),
            n_sig_flip  = sum(sig_flip,  na.rm = TRUE),
            max_abs_delta_pct = max(abs(delta_pct), na.rm = TRUE),
            .groups = "drop")
cat("  Robustness vs. headline (sign / significance flips per spec):\n")
for (i in seq_len(nrow(.flip_summary)))
  cat(sprintf("    %-32s sign_flip=%d sig_flip=%d max|delta|=%.0f%%\n",
              .flip_summary$specification[i],
              .flip_summary$n_sign_flip[i],
              .flip_summary$n_sig_flip[i],
              .flip_summary$max_abs_delta_pct[i]))
cat("  Saved: sensitivity_p99_ladder.csv\n\n")

rm(.em_raw, .headline_cats, .focal_cats, .all_cats, .spec_defs,
   .fit_spec, .cap_person_total, .ladder, .ladder_df, .head_ref,
   .ladder_out, .flip_summary)
gc()

# ===========================================================================
# STEP 10 — Deferred-consumption tests
# ===========================================================================
cat("--- Step 10: Deferred Consumption Tests ---\n")
source_R("80_deferred_consumption_tests.R"); cat("\n")

# ===========================================================================
# STEP 11 — Recategorized-transactions robustness
# ===========================================================================
cat("--- Step 11: Recategorized Transactions Robustness ---\n")
.winsorize_col <- function(x) pmin(x, quantile(x, 0.99, na.rm = TRUE))
recat_candidates <- c(
  file.path(dirname(dirname(getwd())), "data_raw", "konsumtionskollen", "full_data_norecats_filters_260121.dta"),
  "/safe/data/studie_konsumtion_och_attityder/data_raw/konsumtionskollen/full_data_norecats_filters_260121.dta",
  "/safe/data/studie_konsumtion_och_attityder/data_raw/konsumtionskollen/full_data_no_recats_filters_260121.dta"
)
recat_existing <- recat_candidates[file.exists(recat_candidates)]
recat_file     <- if (length(recat_existing) > 0) recat_existing[[1]] else recat_candidates[[1]]

if (file.exists(recat_file)) {
  cat("  Loading recategorized transactions...\n")
  tr <- haven::read_dta(recat_file) |> as_tibble(.name_repair = "universal")
  names(tr) <- gsub("_pr2$",  "",     names(tr))
  names(tr) <- gsub("co2e$",  ".co2e", names(tr))
  names(tr) <- gsub("kr$",    ".kr",   names(tr))
  for (col in names(tr)[grepl("\\.(kr|co2e)$", names(tr))])
    tr[[col]] <- .winsorize_col(tr[[col]])

  recat_selected <- tr |> filter(aid %in% selected_aids) |>
    left_join(users |> select(aid, `user-created`), by = "aid") |>
    filter(date < `user-created`) |>
    mutate(date = clock::date_build(clock::get_year(date), clock::get_month(date))) |>
    semi_join(selected_months, by = c("aid", "date"))

  recat_emissions <- recat_selected |> select(aid, date, ends_with(".co2e")) |>
    collapse::pivot(ids = c("aid", "date")) |>
    rename(category = variable, co2e = value) |>
    mutate(co2e = if_else(category %in% c("exclude.co2e","savings.co2e","charity.co2e"), 0, co2e)) |>
    fgroup_by(aid, date, category) |> fsummarise(co2e = fsum(co2e, na.rm = TRUE)) |>
    left_join(broad_cat, by = "category")

  recat_person <- aggregate_person_co2e(recat_emissions) |>
    inner_join(target_data, by = "aid")
  cat(sprintf("  Recategorized sample: N = %d\n", nrow(recat_person)))

  recat_results <- run_lifestyle_models(recat_person, ctrl_vars)
  write.csv(recat_results, file.path(output, "robustness_recategorized.csv"), row.names = FALSE)
  write.csv(recat_results |>
              filter(outcome == "total",
                     variable %in% c("no_carTRUE","no_flyingTRUE","no_meatTRUE",
                                     "esi:no_carTRUE","esi:no_flyingTRUE","esi:no_meatTRUE")) |>
              transmute(status = "ok", lifestyle, outcome, N, variable, estimate, p),
            file.path(output, "robustness_recategorized_summary.csv"), row.names = FALSE)
  cat("  Saved: robustness_recategorized.csv + summary.csv\n")
  rm(tr, recat_selected, recat_emissions, recat_person, recat_results); gc()
} else {
  cat("  Recategorized file not found:", recat_file, "\n  Skipping.\n")
  write.csv(tibble(status = "missing_recat_file", recat_file = recat_file,
                   recat_candidates = paste(recat_candidates, collapse = " | "),
                   lifestyle = NA_character_, outcome = NA_character_, N = NA_integer_,
                   variable = NA_character_, estimate = NA_real_, p = NA_real_),
            file.path(output, "robustness_recategorized_summary.csv"), row.names = FALSE)
}
cat("\n")

# ===========================================================================
# STEP 12 — Green-sample replication
# ===========================================================================
cat("--- Step 12: Green Sample Replication ---\n")
saved_selected_aids <- selected_aids; saved_output <- output

green_aids_all <- users |> filter(randomsample == 0) |> pull(aid)
cat(sprintf("  Green sample users in data: %d\n", length(green_aids_all)))

if (length(green_aids_all) > 0) {
  green_single <- users |> filter(profile.field_profile_household_adults <= 1) |> pull(aid)
  green_edu    <- users |> filter(Sun2020Niva != 999)                          |> pull(aid)
  green_eligible <- Reduce(intersect, list(green_aids_all, green_single, green_edu,
                                           unique(monthly_spending$aid)))
  green_months <- select_uncat_months(
    monthly_spending |> filter(aid %in% green_eligible))
  green_pre_cc <- unique(green_months$aid)
  green_cc_excluded <- compute_cc_excluded_aids(monthly_spending, green_pre_cc, green_months,
                                                CC_SHARE_THRESHOLD_DEFAULT)
  cat(sprintf("  Green sample: CC share exclusion removed %d aids\n",
              length(green_cc_excluded)))
  green_final_aids <- setdiff(green_pre_cc, green_cc_excluded)
  cat(sprintf("  Green sample after all filters: N = %d\n", length(green_final_aids)))

  if (length(green_final_aids) >= 30) {
    green_em <- monthly_emissions |> filter(aid %in% green_final_aids) |>
      semi_join(green_months, by = c("aid", "date"))
    g_caps <- apply_p99_caps(green_em,
                             monthly_spending |> filter(aid %in% green_final_aids) |>
                               semi_join(green_months, by = c("aid","date")))
    green_em <- g_caps$emissions
    cat(sprintf("  Green sample P99: %d person×category values capped\n", g_caps$n_capped_co2e))

    green_esi    <- list(esi = project_esi(survey, green_final_aids, esi_main))
    green_ctrl   <- build_control_data(users, monthly_incomes,
                                       green_months, green_final_aids)
    green_target <- build_target_data(users, green_em, green_esi$esi, green_ctrl)

    .esi_bin_row <- function(d, label)
      d |> mutate(bin = case_when(esi < -1 ~ "z_lt_minus_1",
                                  esi <  0 ~ "z_minus_1_to_0",
                                  esi <  1 ~ "z_0_to_1",
                                  TRUE     ~ "z_ge_1")) |>
        count(bin, name = "n") |> mutate(share = n / sum(n)) |>
        select(bin, share) |> pivot_wider(names_from = bin, values_from = share) |>
        mutate(sample = label)
    write.csv(
      bind_rows(.esi_bin_row(target_data, "analytical_sample"),
                .esi_bin_row(green_target, "green_sample_nonrandom")) |>
        select(sample, z_lt_minus_1, z_minus_1_to_0, z_0_to_1, z_ge_1),
      file.path(output, "table5_esi_distribution_with_green.csv"), row.names = FALSE)
    cat("  Saved: table5_esi_distribution_with_green.csv\n")

    green_person <- aggregate_person_co2e(green_em) |> inner_join(green_target, by = "aid")
    green_ctrl_vars <- ctrl_var_names(green_ctrl)
    cat(sprintf("  Running regressions on green sample (N = %d)...\n", nrow(green_person)))
    cat(sprintf("    Car-free: %d, Flight-free: %d, Meat-free: %d\n",
                sum(green_person$no_car, na.rm = TRUE),
                sum(green_person$no_flying, na.rm = TRUE),
                sum(green_person$no_meat, na.rm = TRUE)))

    green_results <- run_lifestyle_models(green_person, green_ctrl_vars)
    write.csv(green_results, file.path(output, "robustness_green_sample.csv"), row.names = FALSE)
    write.csv(green_results |>
                filter(outcome == "total",
                       variable %in% c("no_carTRUE","no_flyingTRUE","no_meatTRUE",
                                       "esi:no_carTRUE","esi:no_flyingTRUE","esi:no_meatTRUE")) |>
                transmute(status = "ok", lifestyle, outcome, N, variable, estimate, p),
              file.path(output, "robustness_green_sample_summary.csv"), row.names = FALSE)
    cat("  Saved: robustness_green_sample.csv + summary.csv\n")
    for (lf in LIFESTYLES) {
      key <- green_results |> filter(lifestyle == lf, outcome == "total",
                                     grepl(paste0(lf, "TRUE"), variable))
      if (nrow(key) > 0)
        cat(sprintf("    %s total: est = %.0f, p = %.4f\n", lf, key$estimate[1], key$p[1]))
    }
    rm(green_em, green_esi, green_ctrl, green_target, green_person, green_results); gc()
  } else {
    cat("  Too few green sample observations after filtering. Skipping.\n")
    write.csv(tibble(status = "too_few_green_after_filters",
                     lifestyle = NA_character_, outcome = NA_character_,
                     N = length(green_final_aids), variable = NA_character_,
                     estimate = NA_real_, p = NA_real_),
              file.path(output, "robustness_green_sample_summary.csv"), row.names = FALSE)
  }
} else {
  cat("  No green sample users found.\n")
  write.csv(tibble(status = "no_green_users_found", lifestyle = NA_character_,
                   outcome = NA_character_, N = 0L, variable = NA_character_,
                   estimate = NA_real_, p = NA_real_),
            file.path(output, "robustness_green_sample_summary.csv"), row.names = FALSE)
}
selected_aids <- saved_selected_aids; output <- saved_output
rm(saved_selected_aids, saved_output); cat("\n")

# ===========================================================================
# STEP 13 — Diet-definition robustness
# ===========================================================================
cat("--- Step 13: Diet Definition Robustness ---\n")
diet_data <- selected_emissions |> group_by(aid) |>
  summarise(total = sum(co2e),
            direct_no_meat   = sum(co2e[category == "groceries.co2e"]),
            indirect_no_meat = sum(co2e[category != "groceries.co2e"]),
            .groups = "drop") |>
  inner_join(target_data |> select(aid, esi, all_of(ctrl_vars)), by = "aid") |>
  inner_join(selected_users |> select(aid, profile.field_food_diet), by = "aid") |>
  rename(diet_group = profile.field_food_diet)

diet_counts <- diet_data |> count(diet_group, name = "N") |>
  mutate(share = N / sum(N), sufficient_for_standalone_model = N >= 30) |> arrange(desc(N))
write.csv(diet_counts, file.path(output, "robustness_diet_group_counts.csv"), row.names = FALSE)

diet_definitions <- list(
  baseline_non_meat   = function(x) x != "mixed",
  non_meat_excl_vegan = function(x) x != "mixed" & x != "vegan",
  vegan_only          = function(x) x == "vegan",
  vegetarian_only     = function(x) x == "vegetarian",
  vegfish_only        = function(x) x == "vegfish"
)
diet_summary <- tibble(); diet_results <- tibble()
for (def_name in names(diet_definitions)) {
  d <- diet_data |>
    mutate(diet_indicator = as.logical(diet_definitions[[def_name]](diet_group)))
  ng <- sum(d$diet_indicator, na.rm = TRUE)
  no <- sum(!d$diet_indicator, na.rm = TRUE)
  ran <- ng >= 10 & no >= 10
  diet_summary <- bind_rows(diet_summary, tibble(
    definition = def_name, N = nrow(d), n_group = ng, n_other = no,
    share_group = ng / nrow(d),
    sufficient_for_standalone_model = ng >= 30, model_ran = ran))
  if (!ran) {
    cat(sprintf("  %s: skipped (n_group=%d, n_other=%d)\n", def_name, ng, no)); next
  }
  cat(sprintf("  %s: n_group=%d (%.1f%%), n_other=%d\n",
              def_name, ng, 100 * ng / nrow(d), no))
  for (dv in c("total", "direct_no_meat", "indirect_no_meat")) {
    f  <- as.formula(paste(dv, "~ esi * diet_indicator +",
                           paste(ctrl_vars, collapse = "+")))
    rb <- tryCatch(fit_robust(f, d), error = function(e) NULL)
    if (is.null(rb)) next
    key <- grepl("diet_indicatorTRUE|esi:diet_indicatorTRUE|diet_indicatorTRUE:esi", rb$variable)
    if (any(key))
      diet_results <- bind_rows(diet_results, tibble(
        definition = def_name, outcome = dv, N = nrow(d),
        n_group = ng, n_other = no, variable = rb$variable[key],
        estimate = rb$estimate[key], stderr = rb$stderr[key],
        t = rb$t[key], p = rb$p[key]))
  }
}
write.csv(diet_summary, file.path(output, "robustness_diet_definitions_summary.csv"), row.names = FALSE)
write.csv(diet_results, file.path(output, "robustness_diet_definitions.csv"),         row.names = FALSE)
vegan_n <- diet_counts |> filter(diet_group == "vegan") |> pull(N)
if (length(vegan_n) == 0) vegan_n <- 0
cat(sprintf("  Vegan subgroup size: n = %d (%s for n>=30)\n",
            vegan_n, if (vegan_n >= 30) "sufficient" else "small"))
cat("  Saved: robustness_diet_*\n\n")

# ===========================================================================
# STEP 14 — SI residual diagnostics
# ===========================================================================
cat("--- Step 14: SI diagnostics ---\n")
si_ok <- TRUE
tryCatch(source_R("90_SI.R"),
         error = function(e) { si_ok <<- FALSE
           cat("  Warning: SI diagnostics failed:", e$message, "\n") })
if (si_ok) cat("  Saved SI diagnostics (Residuals distribution figures/tables).\n")
cat("\n")

# ===========================================================================
# STEP 14b — Manuscript figures
# ===========================================================================
cat("--- Step 14b: Manuscript figures ---\n")
old_wd_14b <- getwd()
project_root <- if (dir.exists(file.path(old_wd_14b, "..", "Results")) ||
                    dir.exists(file.path(old_wd_14b, "..", "results"))) {
  normalizePath(file.path(old_wd_14b, ".."))
} else if (dir.exists("Results") || dir.exists("results")) {
  getwd()
} else old_wd_14b
setwd(project_root)
results_14b <- if (dir.exists("results")) "results" else if (dir.exists("Results")) "Results" else "results"
dir.create(results_14b, recursive = TRUE, showWarnings = FALSE)
abs_output <- if (file.exists(file.path(output, "interaction regressions co2e.csv"))) {
  output
} else {
  normalizePath(file.path(old_wd_14b, output), mustWork = FALSE)
}
if (dir.exists(abs_output)) {
  csvs <- list.files(abs_output, pattern = "\\.(csv|txt)$", full.names = TRUE)
  if (length(csvs) > 0) {
    file.copy(csvs, results_14b, overwrite = TRUE)
    cat(sprintf("  Pre-synced %d files from %s to %s\n", length(csvs), abs_output, results_14b))
  }
}
saved_output_14b <- output; output <- results_14b
for (script in c("91_interaction_plot.R", "92_category_decomposition_esi.R", "93_si_figures.R")) {
  tryCatch(source_R(script),
           error = function(e) cat(sprintf("  Warning: %s failed: %s\n",
                                           script, conditionMessage(e))))
}
output <- saved_output_14b; setwd(old_wd_14b); cat("\n")

# ===========================================================================
# STEP 15 — Key numbers + optional sync to manuscript Results/
# ===========================================================================
cat("====== KEY NUMBERS FOR MANUSCRIPT ======\n\nSAMPLE:\n")
cat(sprintf("  N = %d\n  Months/person: %.1f +/- %.1f\n",
            nrow(desc_data), mean(desc_data$n_months), sd(desc_data$n_months)))
for (lf in LIFESTYLES)
  cat(sprintf("  %s: %d (%.1f%%)\n", lf,
              sum(desc_data[[lf]]), 100 * mean(desc_data[[lf]])))
cat(sprintf("  Female: %d (%.1f%%)\n",
            sum(desc_data$sex == "female", na.rm = TRUE),
            100 * mean(desc_data$sex == "female", na.rm = TRUE)))
cat(sprintf("\nESI:\n  Cronbach's alpha = %.3f\n  Variance explained = %.1f%%\n",
            env_alpha$total$raw_alpha, env_fa$Vaccounted[2, 1] * 100))
cat(sprintf("\nEMISSIONS:\n  Monthly CO2e: %.0f +/- %.0f kg (median %.0f)\n",
            mean(desc_data$monthly_co2e), sd(desc_data$monthly_co2e),
            median(desc_data$monthly_co2e)))
cat(sprintf("  Monthly spending: %.0f +/- %.0f SEK\n",
            mean(desc_data$monthly_kr), sd(desc_data$monthly_kr)))
cat(sprintf("  Monthly income: %.0f +/- %.0f SEK\n\n",
            mean(desc_data$monthly_income, na.rm = TRUE),
            sd(desc_data$monthly_income, na.rm = TRUE)))

cat("MAIN RESULTS (interaction regressions, HC3 robust SEs):\n")
main_res <- read.csv(file.path(output, "interaction regressions co2e.csv"))
for (mn in c("Total emissions Car ownership","Total emissions Air travel","Total emissions Diet")) {
  coefs <- main_res |> filter(model_name == mn, grepl("TRUE$|esi:", term)) |>
    select(term, estimate, p.value)
  cat(sprintf("\n  %s:\n", mn))
  for (i in seq_len(nrow(coefs))) {
    sig <- ifelse(coefs$p.value[i] < 0.001, "***",
           ifelse(coefs$p.value[i] < 0.01,  "**",
           ifelse(coefs$p.value[i] < 0.05,  "*",
           ifelse(coefs$p.value[i] < 0.1,   ".", ""))))
    cat(sprintf("    %s: %.1f (p = %.4f) %s\n",
                coefs$term[i], coefs$estimate[i], coefs$p.value[i], sig))
  }
}

if (isTRUE(EXPORT_CSV_TABLES_AS_PNG)) {
  cat("\nRendering CSV tables to PNG images...\n")
  render_csv_tables_to_png(root_dir = output,
                           rows_per_page = CSV_TABLE_ROWS_PER_PAGE,
                           max_pages     = CSV_TABLE_MAX_PAGES)
  cat("  Saved PNG table views next to CSV files.\n")
}

cat("\n\n====== All analyses complete ======\nOutput directory:", output, "\n\n")

# ---- Optional sync to manuscript Results/ ----------------------------------
if (isTRUE(SYNC_RESULTS_TO_MANUSCRIPT_DIR)) {
  candidates <- c("Results", "results",
                  file.path("..", "Results"), file.path("..", "results"),
                  file.path(getwd(), "Results"), file.path(getwd(), "results"),
                  file.path(dirname(getwd()), "Results"),
                  file.path(dirname(getwd()), "results"))
  manuscript_results_dirs <- unique(candidates[dir.exists(candidates)])
  if (length(manuscript_results_dirs) == 0)
    manuscript_results_dirs <- unique(c(file.path("..", "results"), file.path("..", "Results")))

  files_to_sync <- c(
    "interaction regressions co2e.csv","interaction regressions kr.csv",
    "interaction regressions co2e.txt","interaction regressions kr.txt",
    "category regression co2e.csv","category regression kr.csv",
    "Waterfall.png","Waterfall_pres_main.png","Waterfall_pres_esi.png",
    "category_decomposition_esi.png","esi_distribution.png","Residuals distribution.png",
    "sample_flow.csv","table1_continuous.csv","table1_categorical.csv",
    "table4_controls_descriptives.csv","table5_esi_distribution_with_green.csv",
    "distributional_ks_tests.csv","rebound_benchmark.csv","ipw_robustness.csv",
    "robustness_sampling_ipw.csv","robustness_sampling_ipw_summary.csv",
    "sampling_ipw_weights.csv","sampling_ipw_cell_balance.csv",
    "sensitivity_income_quartile.csv","sensitivity_urban_rural.csv",
    "sensitivity_uncat_threshold.csv",
    "sensitivity_p99_ladder.csv",
    "deferred_consumption_extended_window.csv",
    "robustness_green_sample_summary.csv","robustness_recategorized_summary.csv",
    "robustness_diet_definitions_summary.csv"
  )
  cat("Manuscript Results sync:\n"); missing_files <- character()
  for (target_dir in manuscript_results_dirs) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    synced <- 0L
    for (nm in files_to_sync) {
      src <- file.path(output, nm); dst <- file.path(target_dir, nm)
      if (file.exists(src)) {
        if (isTRUE(file.copy(src, dst, overwrite = TRUE))) synced <- synced + 1L
      } else missing_files <- union(missing_files, nm)
    }
    cat(sprintf("  Synced %d/%d files to %s\n", synced, length(files_to_sync), target_dir))
  }
  if (length(missing_files) > 0) {
    cat("  Missing (not produced in this run):\n")
    cat(paste0("    - ", missing_files, collapse = "\n"), "\n")
  }
}
