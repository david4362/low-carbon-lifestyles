###############################################################################
# SI figures: replace key tables with forest/coefficient plots
#
# Generates:
#   1. si_forest_main.png       — Key regression coefficients (replaces Tables 10+11)
#   2. si_threshold_sensitivity.png — Uncategorized threshold robustness (replaces Table 6)
#   3. si_forest_diet.png       — Diet-definition robustness (replaces Table 7)
#   4. si_forest_robustness.png — IPW + green sample comparison (replaces Tables 15+18)
#
# Reads from exported CSVs in results/
###############################################################################

library(ggplot2)
library(dplyr)
library(tidyr)

if (!exists("output") || !nzchar(output)) output <- "results"

# ---------- Shared helpers ----------
lifestyle_labels <- c("no_car" = "No car", "no_flying" = "No flying", "no_meat" = "Non-meat")
outcome_labels <- c("total" = "Total", "direct" = "Direct", "indirect" = "Indirect")

# Common theme for forest plots
theme_forest <- function() {
  theme_minimal(base_size = 10) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 10),
      legend.position = "top",
      legend.title = element_blank(),
      plot.margin = margin(10, 15, 10, 10)
    )
}

###########################################################################
# 1. Key regression coefficients — forest plot (replaces Tables 10+11)
###########################################################################
cat("[1/4] Key regression coefficients forest plot...\n")

reg_co2e <- read.csv(file.path(output, "interaction regressions co2e.csv"))

# Extract lifestyle main effects and ESI × lifestyle interactions
# model_name encodes lifestyle × outcome
key_terms <- reg_co2e |>
  filter(grepl("TRUE$", term) | grepl(":.*TRUE$", term)) |>
  filter(!grepl("separate_house|sex|no_children|pop_density|major_city|education|income|age_group", term)) |>
  mutate(
    # Identify lifestyle
    lifestyle = case_when(
      grepl("no_car", term) ~ "no_car",
      grepl("no_flying", term) ~ "no_flying",
      grepl("no_meat", term) ~ "no_meat",
      TRUE ~ NA_character_
    ),
    # Identify coefficient type
    coef_type = ifelse(grepl("esi", term), "ESI \u00d7 Lifestyle", "Lifestyle main effect"),
    # Identify outcome from model_name
    outcome = case_when(
      grepl("^Total", model_name) ~ "Total",
      grepl("^Direct", model_name) ~ "Direct",
      grepl("^Indirect", model_name) ~ "Indirect"
    ),
    ci_low = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error,
    # Convert to tCO2e for readability
    est_t = estimate / 1000,
    ci_low_t = ci_low / 1000,
    ci_high_t = ci_high / 1000,
    sig = ifelse(p.value < 0.05, "p < 0.05", "n.s.")
  ) |>
  filter(!is.na(lifestyle)) |>
  mutate(
    lifestyle_label = lifestyle_labels[lifestyle],
    lifestyle_label = factor(lifestyle_label, levels = c("Non-meat", "No flying", "No car")),
    outcome = factor(outcome, levels = c("Total", "Direct", "Indirect")),
    coef_type = factor(coef_type, levels = c("Lifestyle main effect", "ESI \u00d7 Lifestyle"))
  )

p1 <- ggplot(key_terms, aes(x = est_t, y = lifestyle_label, colour = sig, shape = coef_type)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
  geom_pointrange(aes(xmin = ci_low_t, xmax = ci_high_t),
                  position = position_dodge(width = 0.5), size = 0.4, linewidth = 0.5) +
  facet_wrap(~ outcome, ncol = 3) +
  scale_colour_manual(values = c("p < 0.05" = "#1b7837", "n.s." = "grey55")) +
  scale_shape_manual(values = c("Lifestyle main effect" = 16, "ESI \u00d7 Lifestyle" = 17)) +
  labs(
    x = expression("Coefficient (tCO"[2]*"e/year)"),
    y = NULL,
    caption = "Points: estimate; whiskers: 95% CI (HC3 robust SEs). Green: p < 0.05."
  ) +
  theme_forest() +
  theme(
    plot.caption = element_text(size = 7, colour = "grey40", hjust = 0)
  )

ggsave(file.path(output, "si_forest_main.png"), p1,
       width = 22, height = 8, units = "cm", dpi = 300, bg = "white")
cat("  Saved: si_forest_main.png\n")

###########################################################################
# 2. Threshold sensitivity — dot plot (replaces Table 6)
###########################################################################
cat("[2/4] Threshold sensitivity dot plot...\n")

sens <- read.csv(file.path(output, "sensitivity_uncat_threshold.csv"))

# Add baseline 10% from main interaction regressions
baseline_10 <- reg_co2e |>
  filter(grepl("TRUE$", term), !grepl("esi", term),
         !grepl("separate_house|sex|no_children|pop_density|major_city|education|income|age_group", term)) |>
  mutate(
    lifestyle = case_when(
      grepl("no_car", term) ~ "no_car",
      grepl("no_flying", term) ~ "no_flying",
      grepl("no_meat", term) ~ "no_meat"
    ),
    outcome_type = case_when(
      grepl("^Total", model_name) ~ "total",
      grepl("^Indirect", model_name) ~ paste0("indirect_", lifestyle)
    )
  ) |>
  filter(!is.na(outcome_type)) |>
  transmute(
    threshold = 0.10, lifestyle = lifestyle, outcome = outcome_type,
    N = model_n, variable = term,
    estimate = estimate, stderr = std.error, t = statistic, p = p.value
  )

sens_all <- bind_rows(sens, baseline_10)

# Extract lifestyle main effects (total + indirect)
sens_plot <- sens_all |>
  filter(grepl("TRUE$", variable), !grepl("esi", variable)) |>
  mutate(
    lifestyle = case_when(
      grepl("no_car", variable) ~ "No car",
      grepl("no_flying", variable) ~ "No flying",
      grepl("no_meat", variable) ~ "Non-meat"
    ),
    outcome_label = case_when(
      outcome == "total" ~ "Total",
      grepl("indirect", outcome) ~ "Indirect"
    ),
    ci_low = estimate - 1.96 * stderr,
    ci_high = estimate + 1.96 * stderr,
    sig = ifelse(p < 0.05, "p < 0.05", "n.s."),
    threshold_pct = paste0(threshold * 100, "%"),
    threshold_pct = factor(threshold_pct, levels = c("5%", "10%", "15%", "20%"))
  ) |>
  filter(!is.na(outcome_label)) |>
  mutate(
    lifestyle = factor(lifestyle, levels = c("No car", "No flying", "Non-meat")),
    outcome_label = factor(outcome_label, levels = c("Total", "Indirect"))
  )

p2 <- ggplot(sens_plot, aes(x = threshold_pct, y = estimate / 1000, colour = sig)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
  geom_pointrange(aes(ymin = ci_low / 1000, ymax = ci_high / 1000),
                  size = 0.35, linewidth = 0.45) +
  facet_grid(outcome_label ~ lifestyle, scales = "free_y") +
  scale_colour_manual(values = c("p < 0.05" = "#1b7837", "n.s." = "grey55")) +
  labs(
    x = "Uncategorized-share threshold",
    y = expression("Coefficient (tCO"[2]*"e/year)"),
    caption = "Baseline threshold = 10%. Whiskers: 95% CI (HC3 robust SEs)."
  ) +
  theme_forest() +
  theme(
    plot.caption = element_text(size = 7, colour = "grey40", hjust = 0)
  )

ggsave(file.path(output, "si_threshold_sensitivity.png"), p2,
       width = 20, height = 14, units = "cm", dpi = 300, bg = "white")
cat("  Saved: si_threshold_sensitivity.png\n")

###########################################################################
# 3. Diet-definition robustness — forest plot (replaces Table 7)
###########################################################################
cat("[3/4] Diet-definition forest plot...\n")

diet <- read.csv(file.path(output, "robustness_diet_definitions.csv"))

# Extract lifestyle main effects for each definition × outcome
diet_plot <- diet |>
  filter(grepl("TRUE$", variable), !grepl("esi", variable)) |>
  mutate(
    outcome_label = case_when(
      outcome == "total" ~ "Total",
      outcome == "indirect_no_meat" ~ "Indirect",
      outcome == "direct_no_meat" ~ "Direct"
    ),
    ci_low = estimate - 1.96 * stderr,
    ci_high = estimate + 1.96 * stderr,
    sig = ifelse(p < 0.05, "p < 0.05", "n.s."),
    def_label = case_when(
      definition == "baseline_non_meat" ~ paste0("Non-meat (n=", n_group, ")"),
      definition == "non_meat_excl_vegan" ~ paste0("Excl. vegan (n=", n_group, ")"),
      definition == "vegan_only" ~ paste0("Vegan only (n=", n_group, ")"),
      definition == "vegetarian_only" ~ paste0("Vegetarian only (n=", n_group, ")"),
      definition == "vegfish_only" ~ paste0("Vegfish only (n=", n_group, ")")
    )
  ) |>
  filter(!is.na(outcome_label)) |>
  mutate(
    outcome_label = factor(outcome_label, levels = c("Total", "Direct", "Indirect")),
    def_label = factor(def_label, levels = rev(unique(def_label[order(definition)])))
  )

# Reorder factor levels for clean display
ordered_defs <- diet_plot |>
  distinct(definition, def_label) |>
  mutate(ord = case_when(
    definition == "baseline_non_meat" ~ 1,
    definition == "non_meat_excl_vegan" ~ 2,
    definition == "vegan_only" ~ 3,
    definition == "vegetarian_only" ~ 4,
    definition == "vegfish_only" ~ 5
  )) |>
  arrange(desc(ord)) |>
  pull(def_label)

diet_plot$def_label <- factor(diet_plot$def_label, levels = ordered_defs)

p3 <- ggplot(diet_plot, aes(x = estimate / 1000, y = def_label, colour = sig)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
  geom_pointrange(aes(xmin = ci_low / 1000, xmax = ci_high / 1000),
                  size = 0.4, linewidth = 0.5) +
  facet_wrap(~ outcome_label, ncol = 3, scales = "free_x") +
  scale_colour_manual(values = c("p < 0.05" = "#1b7837", "n.s." = "grey55")) +
  labs(
    x = expression("Coefficient (tCO"[2]*"e/year)"),
    y = NULL,
    caption = "Whiskers: 95% CI (HC3 robust SEs). Vegan-only (n=19): interpret with caution."
  ) +
  theme_forest() +
  theme(
    plot.caption = element_text(size = 7, colour = "grey40", hjust = 0)
  )

ggsave(file.path(output, "si_forest_diet.png"), p3,
       width = 22, height = 9, units = "cm", dpi = 300, bg = "white")
cat("  Saved: si_forest_diet.png\n")

###########################################################################
# 4. Robustness comparison: baseline vs IPW vs green sample (replaces
#    Tables 15 + 18)
###########################################################################
cat("[4/4] Robustness comparison forest plot...\n")

# --- Baseline (from interaction regressions) ---
baseline <- reg_co2e |>
  filter(grepl("TRUE$", term), !grepl("esi", term),
         !grepl("separate_house|sex|no_children|pop_density|major_city|education|income|age_group", term)) |>
  mutate(
    lifestyle = case_when(
      grepl("no_car", term) ~ "no_car",
      grepl("no_flying", term) ~ "no_flying",
      grepl("no_meat", term) ~ "no_meat"
    ),
    outcome = case_when(
      grepl("^Total", model_name) ~ "Total",
      grepl("^Direct", model_name) ~ "Direct",
      grepl("^Indirect", model_name) ~ "Indirect"
    ),
    source = "Baseline"
  ) |>
  filter(!is.na(lifestyle), !is.na(outcome)) |>
  select(lifestyle, outcome, estimate, std.error, p.value, source)

# --- IPW ---
ipw <- read.csv(file.path(output, "ipw_robustness.csv"))
ipw_main <- ipw |>
  filter(grepl("TRUE$", variable), !grepl("esi", variable),
         !grepl("separate_house|sex|no_children|pop_density|major_city|education|income|age_group", variable)) |>
  mutate(
    lifestyle = case_when(
      grepl("no_car", variable) ~ "no_car",
      grepl("no_flying", variable) ~ "no_flying",
      grepl("no_meat", variable) ~ "no_meat"
    ),
    outcome = case_when(
      grepl("^Total", model) | model == "Total (IPW)" ~ "Total",
      grepl("^Direct", model) ~ "Direct",
      grepl("^Indirect", model) ~ "Indirect"
    ),
    source = "IPW",
    p.value = p
  ) |>
  filter(!is.na(lifestyle), !is.na(outcome)) |>
  select(lifestyle, outcome, estimate, stderr, p.value, source) |>
  rename(std.error = stderr)

# --- Green sample ---
green <- read.csv(file.path(output, "robustness_green_sample.csv"))
green_main <- green |>
  filter(grepl("TRUE$", variable), !grepl("esi", variable),
         !grepl("separate_house|sex|no_children|pop_density|major_city|education|income|age_group", variable)) |>
  mutate(
    lifestyle = case_when(
      grepl("no_car", variable) ~ "no_car",
      grepl("no_flying", variable) ~ "no_flying",
      grepl("no_meat", variable) ~ "no_meat"
    ),
    outcome_raw = outcome,
    outcome = case_when(
      outcome_raw == "total" ~ "Total",
      grepl("direct", outcome_raw) & !grepl("indirect", outcome_raw) ~ "Direct",
      grepl("indirect", outcome_raw) ~ "Indirect"
    ),
    source = "Green sample",
    p.value = p
  ) |>
  filter(!is.na(lifestyle), !is.na(outcome)) |>
  select(lifestyle, outcome, estimate, stderr, p.value, source) |>
  rename(std.error = stderr)

# Combine
robust_df <- bind_rows(baseline, ipw_main, green_main) |>
  mutate(
    ci_low = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error,
    sig = ifelse(p.value < 0.05, "p < 0.05", "n.s."),
    lifestyle_label = lifestyle_labels[lifestyle],
    lifestyle_label = factor(lifestyle_label, levels = c("Non-meat", "No flying", "No car")),
    outcome = factor(outcome, levels = c("Total", "Direct", "Indirect")),
    source = factor(source, levels = c("Baseline", "IPW", "Green sample"))
  )

p4 <- ggplot(robust_df, aes(x = estimate / 1000, y = lifestyle_label,
                              colour = source, shape = source)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey50", linetype = "dashed") +
  geom_pointrange(aes(xmin = ci_low / 1000, xmax = ci_high / 1000),
                  position = position_dodge(width = 0.5),
                  size = 0.35, linewidth = 0.45) +
  facet_wrap(~ outcome, ncol = 3, scales = "free_x") +
  scale_colour_manual(values = c("Baseline" = "#2c3e50", "IPW" = "#e67e22", "Green sample" = "#27ae60")) +
  scale_shape_manual(values = c("Baseline" = 16, "IPW" = 17, "Green sample" = 15)) +
  labs(
    x = expression("Lifestyle main effect (tCO"[2]*"e/year)"),
    y = NULL,
    caption = "Whiskers: 95% CI (HC3 robust SEs). Green sample is non-random (Section S1)."
  ) +
  theme_forest() +
  theme(
    plot.caption = element_text(size = 7, colour = "grey40", hjust = 0)
  )

ggsave(file.path(output, "si_forest_robustness.png"), p4,
       width = 22, height = 8, units = "cm", dpi = 300, bg = "white")
cat("  Saved: si_forest_robustness.png\n")

cat("\nAll SI figures generated.\n")
