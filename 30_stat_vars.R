###############################################################################
# stat_vars.R — build analysis-ready data frames from filter_data.R outputs.
# Defines: selected_spending, selected_emissions, selected_incomes,
#          selected_survey, selected_users, control_data, target_data,
#          esi, env_fa, env_alpha (+ leave-one-out alphas).
# Heavy logic lives in utils.R (build_control_data, build_esi, build_target_data).
###############################################################################

suppressPackageStartupMessages(library(dplyr))
if (!requireNamespace("psych", quietly = TRUE))
  stop("Package 'psych' is required. Install it with install.packages('psych').")

selected_spending  <- monthly_spending  |> filter(aid %in% selected_aids) |>
  semi_join(selected_months, by = c("aid", "date"))
selected_emissions <- monthly_emissions |> filter(aid %in% selected_aids) |>
  semi_join(selected_months, by = c("aid", "date"))
selected_incomes   <- monthly_incomes   |> filter(aid %in% selected_aids)
selected_survey    <- filter(survey, aid %in% selected_aids)
selected_users     <- filter(users,  aid %in% selected_aids)

# --- ESI -------------------------------------------------------------------
# Keep the full ESI object (factor solution + raw items + score moments)
# so that any subsample (e.g. the green-sample replication in master_analysis.R
# Step 12) can project its survey items onto this factor solution rather than
# re-fitting a separate factor model on a different sample.
esi_main  <- build_esi(selected_survey, selected_aids)
esi       <- esi_main$esi
env_fa    <- esi_main$fa
env_alpha <- esi_main$alpha

.alpha_quiet <- function(d) suppressWarnings(psych::alpha(d, warnings = FALSE))
env_alpha_no8  <- selected_survey |> select(array3_9,  array3_11) |> .alpha_quiet()
env_alpha_no9  <- selected_survey |> select(array3_8,  array3_11) |> .alpha_quiet()
env_alpha_no11 <- selected_survey |> select(array3_8,  array3_9)  |> .alpha_quiet()

# --- Controls + target data ------------------------------------------------
control_data <- build_control_data(selected_users, selected_incomes,
                                   selected_months, selected_aids)
target_data  <- build_target_data(selected_users, selected_emissions,
                                  esi, control_data)
