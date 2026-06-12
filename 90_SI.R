###############################################################################
# SI.R — Supplementary residual diagnostics + ESI-tetrile group summary table.
# Outputs:
#   <output>/Residuals distribution.png
#   <output>/Residuals distribution ESI.png
#   <output>/Residuals table.html
#   <output>/Residuals table ESI.html
#   <output>/ESI tetrile groups.html
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(purrr)
  library(tidysdm); library(gt); library(boot); library(patchwork)
})

ctrl_vars <- ctrl_var_names(control_data)

esi_values <- target_data$esi[is.finite(target_data$esi)]
if (length(esi_values) < 3)
  stop("SI diagnostics require at least 3 finite ESI values.")

# Residual sign-share statistic for bootstrapping
.res_share_stat <- function(formula, data, indices) {
  res <- residuals(lm(formula, data = data[indices, ]))
  c(mean(res >= 0), mean(res < 0))
}

.safe_norm_ci <- function(reps, index, prefix) {
  ci <- tryCatch(boot.ci(reps, type = "norm", index = index)$normal,
                 error = function(e) rep(NA_real_, 3))
  names(ci) <- paste0(prefix, ".V", seq_along(ci))
  ci
}

esi_tetriles <- quantile(esi_values, c(0, 1/3, 2/3, 1), na.rm = TRUE)
lm_data_si <- lm_data |>
  filter(is.finite(esi)) |>
  mutate(
  esi_tetrile = case_when(
    esi < esi_tetriles[2] ~ "Low",
    esi > esi_tetriles[3] ~ "High",
    TRUE                  ~ "Middle"
  )
)

lm_cov_residuals <- bind_rows(lapply(LIFESTYLES, function(ls) {
  indirect <- paste0("indirect_", ls)
  f <- as.formula(sprintf("%s ~ %s", indirect, paste(ctrl_vars, collapse = "+")))
  reps <- boot(lm_data_si, statistic = .res_share_stat, R = 500, formula = f)
  ci_pos <- .safe_norm_ci(reps, index = 1, prefix = "ci_pos")
  ci_neg <- .safe_norm_ci(reps, index = 2, prefix = "ci_neg")
  res    <- residuals(lm(f, data = lm_data_si))
  cbind(res = res * 12 / 1000,
        lm_data_si |> select(all_of(ls), esi_tetrile) |> rename(lifestyle = 1),
        mname = indirect, ci_pos = ci_pos, ci_neg = ci_neg)
}))

plot_data <- lm_cov_residuals |>
  filter(esi_tetrile != "Middle") |>
  mutate(
    mname     = dplyr::case_match(mname,
                                  "indirect_no_car"    ~ "Car-free",
                                  "indirect_no_flying" ~ "Flight-free",
                                  "indirect_no_meat"   ~ "Meat-free"),
    mname     = factor(mname, levels = c("Car-free", "Flight-free", "Meat-free")),
    lifestyle = relevel(factor(if_else(lifestyle, "Adopter", "Non-adopter")),
                        ref = "Non-adopter")
  )

# --- Plots ---------------------------------------------------------------
.violin_base <- function(data) ggplot(data, aes(x = mname, y = res, fill = lifestyle)) +
  geom_split_violin(alpha = 0.7) +
  geom_boxplot(outliers = FALSE, width = 0.2, fill = "white", colour = "grey30") +
  labs(x = NULL,
       y = expression("Deviation from predicted indirect emissions (tCO"[2]*"e per person/year)"),
       fill = NULL) +
  scale_fill_manual(values = c(Adopter = "#4477AA", `Non-adopter` = "#cccccc"),
                    breaks = c("Adopter", "Non-adopter")) +
  ylim(-6, 8.5) +
  coord_flip() +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        panel.grid.major.y = element_blank())

ggsave(file.path(output, "Residuals distribution ESI.png"),
       .violin_base(plot_data) + facet_grid(. ~ esi_tetrile),
       units = "cm", width = 24, height = 14, bg = "white")
ggsave(file.path(output, "Residuals distribution.png"),
       .violin_base(plot_data),
       units = "cm", width = 15, height = 12, bg = "white")


.residual_table <- plot_data |>
  group_by(mname, lifestyle, esi_tetrile) |>
  summarise(
    mean_res   = mean(res, na.rm = TRUE),
    median_res = median(res, na.rm = TRUE),
    share_pos  = mean(res >= 0, na.rm = TRUE),
    ci_pos     = sprintf("(%.2f%%, %.2f%%)", median(ci_pos.V2), median(ci_pos.V3)),
    share_neg  = mean(res < 0, na.rm = TRUE),
    ci_neg     = sprintf("(%.2f%%, %.2f%%)", median(ci_neg.V2), median(ci_neg.V3)),
    .groups    = "drop"
  ) |>
  gt() |> tab_header(title = "Regression residuals") |>
  cols_label(lifestyle = "Lifestyle", mean_res = "Mean", median_res = "Median",
             share_pos = "% >= 0", ci_pos = "95% CI",
             share_neg = "% < 0",  ci_neg = "95% CI", esi_tetrile = "ESI") |>
  fmt_percent(columns = c(share_pos, share_neg), decimals = 1) |>
  fmt_number(columns = c(mean_res, median_res), decimals = 0)

gtsave(.residual_table, file.path(output, "Residuals table ESI.html"))
gtsave(.residual_table, file.path(output, "Residuals table.html"))

# --- ESI tetrile group summary ------------------------------------------
.pct <- function(tab) paste0(round(tab * 100, 0), "%", collapse = ", ")

.summary_row <- function(d) d |> summarise(
  n           = n(),
  income      = round(mean(income_centered)),
  gender      = list(table(sex)/n()),
  age         = list(table(age_group)/n()),
  children    = list(table(no_children)/n()),
  pop_density = round(mean(pop_density_log_centered), 2),
  city        = list(table(major_city)/n()),
  edu         = list(table(education)/n()),
  house       = list(table(separate_house)/n()),
  no_car      = mean(no_car == TRUE),
  no_meat     = mean(no_meat == TRUE),
  no_flying   = mean(no_flying == TRUE),
  .groups     = "drop"
) |> mutate(across(c(gender, age, children, city, edu, house), ~ map_chr(.x, .pct)))

per_tetrile <- lm_data_si |> group_by(esi_tetrile) |> .summary_row()
all_users   <- lm_data_si |> .summary_row() |> mutate(esi_tetrile = "All")

user_table <- bind_rows(per_tetrile, all_users) |>
  gt(rowname_col = "esi_tetrile") |> tab_stubhead(label = "ESI") |>
  fmt_percent(columns = c("no_car","no_meat","no_flying"), decimals = 0) |>
  cols_label(income = "Centered income", gender = "Male/Female",
             age = "Age group", children = "Have/no children",
             pop_density = "Centered log population density",
             city = "In major city", edu = "Education level",
             house = "Apartment/House",
             no_car = "Car-free", no_meat = "Meat-free", no_flying = "Flight-free") |>
  tab_footnote("18-29, 30-44, 45-65, 65+",
               locations = cells_column_labels(columns = age)) |>
  tab_footnote("Grundskola, Gymnasium, Eftergymnasial <2 år, Eftergymnasial >=2 år, Forskare",
               locations = cells_column_labels(columns = edu)) |>
  cols_align(align = "center")

gtsave(user_table, file.path(output, "ESI tetrile groups.html"))
