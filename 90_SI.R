###############################################################################
# SI.R — Supplementary residual diagnostics + ESI-tetrile group summary table.
# Outputs:
#   <output>/Residuals distribution.png
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
# Use the FULL sample so the main residual figure matches the full-sample KS
# tests in master_analysis.R (Step 5). Respondents with a missing ESI score get
# esi_tetrile = NA: they appear in the main figure but are excluded from the
# ESI-faceted companion figure and the residual tables.
lm_data_si <- lm_data |>
  mutate(
  esi_tetrile = case_when(
    !is.finite(esi)       ~ NA_character_,
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
  mutate(
    mname     = dplyr::case_match(mname,
                                  "indirect_no_car"    ~ "Car-free",
                                  "indirect_no_flying" ~ "Flight-free",
                                  "indirect_no_meat"   ~ "Meat-free"),
    mname     = factor(mname, levels = c("Meat-free", "Flight-free", "Car-free")),
    lifestyle = relevel(factor(if_else(lifestyle, "Adopter", "Non-adopter")),
                        ref = "Non-adopter")
  )

# --- Plots ---------------------------------------------------------------
.life_col <- c("Car-free" = "#4477AA", "Flight-free" = "#EE6677", "Meat-free" = "#228833")

# fk gives each adopter half its lifestyle colour; non-adopter halves all grey
.fk_levels <- c("Car-free Non-adopter", "Car-free Adopter",
                 "Flight-free Non-adopter", "Flight-free Adopter",
                 "Meat-free Non-adopter", "Meat-free Adopter")
.fk_cols   <- c("Car-free Non-adopter" = "#cccccc", "Car-free Adopter" = "#4477AA",
                 "Flight-free Non-adopter" = "#cccccc", "Flight-free Adopter" = "#EE6677",
                 "Meat-free Non-adopter" = "#cccccc", "Meat-free Adopter" = "#228833")

plot_data <- plot_data |>
  mutate(fk = factor(paste(mname, lifestyle), levels = .fk_levels))

# High vs Low ESI subset used only for the ESI-faceted companion figure and
# the residual tables; the main figure (below) uses the full sample so that it
# matches its caption and the full-sample KS tests in master_analysis.R.
plot_data_esi <- plot_data |> filter(esi_tetrile %in% c("High", "Low"))

.violin_base <- function(data) {
  ado <- filter(data, lifestyle == "Adopter")
  nad <- filter(data, lifestyle == "Non-adopter")
  # per-lifestyle adopter box colours
  ado_cols <- .life_col[as.character(unique(ado$mname))]
  ggplot(data, aes(x = mname, y = res, fill = fk)) +
    geom_split_violin(alpha = 0.7) +
    geom_boxplot(data = ado, aes(x = mname, y = res, fill = mname), inherit.aes = FALSE,
                 width = 0.15, position = position_nudge(x = 0.13),
                 outliers = FALSE, colour = "grey20", linewidth = 0.4) +
    geom_boxplot(data = nad, aes(x = mname, y = res), inherit.aes = FALSE,
                 width = 0.15, position = position_nudge(x = -0.13),
                 outliers = FALSE, fill = "#cccccc", colour = "grey20", linewidth = 0.4) +
    labs(x = NULL,
         y = expression("Deviation from predicted indirect emissions (tCO"[2]*"e per person/year)"),
         fill = NULL) +
    scale_fill_manual(values = c(.fk_cols, .life_col), guide = "none") +
    coord_flip() +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank())
}

# Hand-drawn legend: tri-colour adopter swatch + grey non-adopter swatch
legend_v2 <- ggplot() + xlim(0, 12) + ylim(0, 1) +
  annotate("rect", xmin = 2.0, xmax = 2.27, ymin = 0.30, ymax = 0.70, fill = "#4477AA") +
  annotate("rect", xmin = 2.27, xmax = 2.54, ymin = 0.30, ymax = 0.70, fill = "#EE6677") +
  annotate("rect", xmin = 2.54, xmax = 2.81, ymin = 0.30, ymax = 0.70, fill = "#228833") +
  annotate("text", x = 2.96, y = 0.5, label = "Adopter", hjust = 0,
           fontface = "italic", size = 3.8) +
  annotate("rect", xmin = 6.0, xmax = 6.6, ymin = 0.30, ymax = 0.70, fill = "#cccccc") +
  annotate("text", x = 6.75, y = 0.5, label = "Non-adopter", hjust = 0,
           fontface = "italic", size = 3.8) +
  annotate("rect", xmin = 1.7, xmax = 9.1, ymin = 0.18, ymax = 0.82,
           color = "grey75", fill = NA, linewidth = 0.4) +
  theme_void() +
  theme(plot.margin = margin(4, 6, 2, 6))

ggsave(file.path(output, "Residuals distribution.png"),
       legend_v2 / .violin_base(plot_data) + plot_layout(heights = c(1, 13)),
       units = "cm", width = 15, height = 12, bg = "white")


.residual_table <- plot_data_esi |>
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
