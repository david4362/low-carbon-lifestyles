# Category-level decomposition of indirect CO2e at high/low ESI
# Produces a grouped bar chart for Extended Data / SI

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# --- Read data ---
cat_df <- read.csv(file.path("results", "category regression co2e.csv")) |>
  select(-X)

# Broad indirect categories (excluding direct/focal domains and aggregates)
broad_cats <- c(
  "Other_products_co2e", "Food_co2e", "Aviation_LDT_co2e",
  "Other_services_co2e", "Car_Public_co2e", "Housing_co2e",
  "Other_misc_co2e", "Vacation_LDT_co2e"
)

# Nice labels for display
cat_labels <- c(
  "Other_products_co2e" = "Other products",
  "Food_co2e" = "Food",
  "Aviation_LDT_co2e" = "Aviation & LDT",
  "Other_services_co2e" = "Other services",
  "Car_Public_co2e" = "Car & public transport",
  "Housing_co2e" = "Housing",
  "Other_misc_co2e" = "Other/misc",
  "Vacation_LDT_co2e" = "Vacation & LDT"
)

# Lifestyle contrasts
lifestyles <- c("no_car", "no_flying", "no_meat")
lifestyle_labels <- c("no_car" = "Car-free", "no_flying" = "Flight-free", "no_meat" = "Meat-free")

# For each lifestyle, the focal domain to EXCLUDE from indirect
focal_domain <- c(
  "no_car" = "Car_Public_co2e",
  "no_flying" = "Aviation_LDT_co2e",
  "no_meat" = "Food_co2e"
)

# --- Extract interaction rows (which have est_low and est_high populated) ---
# est_low / est_high give the within-ESI lifestyle difference (adopter vs non-adopter
# at the same ESI level). To match Fig. 1's common reference (ESI=0 non-adopter),
# we add the ESI main effect: high = est_high + beta_esi, low = est_low - beta_esi.
interaction_data <- cat_df |>
  filter(
    part %in% broad_cats,
    lifestyle_variable %in% lifestyles,
    grepl(":esi$", variable)
  ) |>
  select(lifestyle_variable, part, variable, estimate, stderr, p, est_low, est_high) |>
  rename(
    lifestyle = lifestyle_variable,
    category = part,
    interaction_est = estimate,
    interaction_se = stderr,
    interaction_p = p
  )

# Extract the ESI main effect (beta_esi) for each lifestyle x category
esi_main <- cat_df |>
  filter(
    part %in% broad_cats,
    lifestyle_variable %in% lifestyles,
    variable == "esi"
  ) |>
  select(lifestyle_variable, part, estimate) |>
  rename(
    lifestyle = lifestyle_variable,
    category = part,
    beta_esi = estimate
  )

# --- Build plot data ---
# Common reference = ESI=0 non-adopter (matching Fig. 1)
# High ESI adopter vs ESI=0 non-adopter: est_high + beta_esi
# Low ESI adopter vs ESI=0 non-adopter: est_low - beta_esi
plot_data <- interaction_data |>
  select(lifestyle, category, est_low, est_high) |>
  left_join(esi_main, by = c("lifestyle", "category")) |>
  mutate(
    est_high = est_high + beta_esi,
    est_low  = est_low  - beta_esi
  ) |>
  pivot_longer(
    cols = c(est_low, est_high),
    names_to = "esi_level",
    values_to = "estimate_kg"
  ) |>
  mutate(
    # Convert from kg/month to t/year
    estimate_t = estimate_kg * 12 / 1000,
    esi_level = factor(
      ifelse(esi_level == "est_high", "High ESI (+1 SD)", "Low ESI (\u22121 SD)"),
      levels = c("High ESI (+1 SD)", "Low ESI (\u22121 SD)")
    ),
    category_label = cat_labels[category],
    lifestyle_label = lifestyle_labels[lifestyle]
  )

# Remove the focal domain for each lifestyle (it's direct, not indirect)
plot_data <- plot_data |>
  filter(!(lifestyle == "no_car" & category == "Car_Public_co2e") &
         !(lifestyle == "no_flying" & category == "Aviation_LDT_co2e") &
         !(lifestyle == "no_meat" & category == "Food_co2e"))

# Order categories by average absolute magnitude across all lifestyles
cat_order <- plot_data |>
  group_by(category_label) |>
  summarise(avg_abs = mean(abs(estimate_t)), .groups = "drop") |>
  arrange(avg_abs) |>
  pull(category_label)

plot_data$category_label <- factor(plot_data$category_label, levels = cat_order)
plot_data$lifestyle_label <- factor(plot_data$lifestyle_label,
                                     levels = c("Car-free", "Flight-free", "Meat-free"))

# Lifestyle colors (full saturation; alpha encodes ESI level)
.colours_lifestyle_cat <- c("Car-free" = "#4477AA", "Flight-free" = "#EE6677", "Meat-free" = "#228833")

# --- Plot ---
# fill = lifestyle color, alpha = ESI level → simple 5-entry legend (3 colors + 2 alphas)
p <- ggplot(plot_data, aes(x = category_label, y = estimate_t,
                            fill = lifestyle_label, alpha = esi_level)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey20") +
  facet_wrap(~ lifestyle_label, nrow = 1, scales = "fixed") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(-0.4, 0.4, 0.2),
    labels = function(x) sprintf("%.1f", x)
  ) +
  scale_fill_manual(values = .colours_lifestyle_cat, name = NULL,
                    guide = guide_legend(order = 1)) +
  scale_alpha_manual(
    values = c("High ESI (+1 SD)" = 1.0, "Low ESI (\u22121 SD)" = 0.45),
    name = NULL,
    guide = guide_legend(order = 2,
                         override.aes = list(fill = "grey30"))
  ) +
  labs(
    title = "High-ESI spillovers are broad-based across the household budget",
    subtitle = "Own-domain reductions are omitted; bars show how re-spending shifts emissions elsewhere.",
    x = NULL,
    y = expression("Difference in emissions (tCO"[2]*"e per person-year)")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, colour = "grey40", margin = margin(b = 12)),
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave(
  file.path("results", "category_decomposition_esi.png"),
  plot = p, width = 14, height = 6.5, dpi = 300, bg = "white"
)

cat("Saved: results/category_decomposition_esi.png\n")
