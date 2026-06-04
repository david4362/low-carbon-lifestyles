###############################################################################
# waterfall_vertical.R — vertical 90-degree rotated waterfall plots
# Reads waterfall_stage_components.csv and creates vertical versions where
# reductions point downwards (negative y-axis)
#
# Outputs:
#   <output>/Waterfall_vertical_main.png       (average effect only)
#   <output>/Waterfall_vertical_esi.png        (high/low ESI comparison)
###############################################################################

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(stringr)
})

# ---- Read waterfall stage components ----------------------------------------
stage_data <- read.csv(file.path(output, "waterfall_stage_components.csv"))

# ---- Prepare plot data (total emissions: direct + indirect) ----------------
plot_data <- stage_data |>
  pivot_longer(c(average, high_esi, low_esi),
               names_to = "scenario", values_to = "value") |>
  mutate(
    scenario_label = dplyr::case_match(scenario,
                                       "average"  ~ "Average",
                                       "high_esi" ~ "High ESI",
                                       "low_esi"  ~ "Low ESI"),
    scenario_label = factor(scenario_label,
                            levels = c("Average", "High ESI", "Low ESI")),
    category = factor(as.character(category),
                      levels = c("Car-free", "Flight-free", "Meat-free"))
  ) |>
  select(category, stage, scenario, scenario_label, value) |>
  # Stack direct and indirect for each scenario
  pivot_wider(names_from = stage, values_from = value) |>
  mutate(
    total = direct + indirect,
    # For stacking: direct on bottom, indirect on top
    direct_bottom = 0,
    direct_top = direct,
    indirect_bottom = direct,
    indirect_top = direct + indirect
  )

# ---- Shared theme -----------------------------------------------------------
.theme_vertical <- function() theme_minimal() + theme(
  panel.background = element_rect(fill = "#f5f5f5", color = NA),
  plot.background  = element_rect(fill = "white",   color = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "white", linewidth = 0.5),
  panel.grid.minor = element_blank(),
  legend.position = "top",
  legend.justification = "left",
  legend.box = "vertical",
  legend.margin = margin(b = 12),
  axis.text.x = element_text(angle = 0, hjust = 0.5),
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
)

# ---- Vertical main effects plot (Average only) -----------------------------------
p_vert_main <- plot_data |>
  filter(scenario == "average") |>
  ggplot(aes(x = category, y = total)) +
  geom_col(fill = "#2c3e50", width = 0.6, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "#2c3e50", linewidth = 0.8) +
  scale_y_continuous(
    limits = c(-2.5, 0),
    breaks = seq(-2.5, 0, 0.5),
    labels = function(x) sprintf("%.1f", abs(x))
  ) +
  labs(
    x = "Lifestyle",
    y = expression("Emission differences (tCO"[2]*"e/year)"),
    subtitle = "Reductions point downward (negative direction)"
  ) +
  .theme_vertical()

ggsave(file.path(output, "Waterfall_vertical_main.png"), p_vert_main,
       units = "cm", width = 16, height = 12, dpi = 200)
cat("  Saved: Waterfall_vertical_main.png\n")

# ---- Vertical ESI comparison plot (High vs Low ESI) -------------------------
# Use grouped bars for ESI stratification
plot_esi_comp <- plot_data |>
  filter(scenario != "average") |>
  select(category, scenario_label, total) |>
  pivot_wider(names_from = scenario_label, values_from = total) |>
  pivot_longer(c("High ESI", "Low ESI"),
               names_to = "esi_level", values_to = "value") |>
  mutate(esi_level = factor(esi_level, levels = c("High ESI", "Low ESI")))

p_vert_esi <- ggplot(plot_esi_comp, aes(x = category, y = value, fill = esi_level)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "#2c3e50", linewidth = 0.8) +
  scale_fill_manual(
    values = c("High ESI" = "#333333", "Low ESI" = "#999999"),
    name = NULL
  ) +
  scale_y_continuous(
    limits = c(-1.5, 0.5),
    breaks = seq(-1.5, 0, 0.5),
    labels = function(x) sprintf("%.1f", abs(x))
  ) +
  labs(
    x = "Lifestyle",
    y = expression("Emission differences (tCO"[2]*"e/year)"),
    subtitle = "Reductions point downward; darker bars = higher environmental identity"
  ) +
  .theme_vertical() +
  theme(legend.position = "top")

ggsave(file.path(output, "Waterfall_vertical_esi.png"), p_vert_esi,
       units = "cm", width = 18, height = 12, dpi = 200)
cat("  Saved: Waterfall_vertical_esi.png\n")

cat("Vertical waterfall plots complete.\n\n")
