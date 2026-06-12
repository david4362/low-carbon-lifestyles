# Category-level decomposition of indirect CO2e at high/low ESI
# Produces a grouped bar chart for Extended Data / SI

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(patchwork)

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
  "Aviation_LDT_co2e" = "Air travel",
  "Other_services_co2e" = "Other services",
  "Car_Public_co2e" = "Car & public transport",
  "Housing_co2e" = "Housing",
  "Other_misc_co2e" = "Remaining categories",
  "Vacation_LDT_co2e" = "Vacation travel"
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

# Keep the focal domain row present (drawn as N/A), but remove its BAR for the
# lifestyle it is focal to (it is the direct effect, not an indirect one).
focal_label_map <- c("Car-free" = "Car & public transport",
                     "Flight-free" = "Air travel",
                     "Meat-free" = "Food")
plot_data <- plot_data |>
  filter(!(lifestyle == "no_car"    & category == "Car_Public_co2e") &
         !(lifestyle == "no_flying" & category == "Aviation_LDT_co2e") &
         !(lifestyle == "no_meat"   & category == "Food_co2e"))

# Fixed category order. coord_flip() renders level 1 at the BOTTOM, so list
# bottom -> top: 5 remaining categories first, then the 3 focal domains on top.
cat_levels <- c("Remaining categories", "Other products", "Other services",
                "Vacation travel", "Housing",
                "Food", "Air travel", "Car & public transport")
n_cat       <- length(cat_levels)          # 8
n_focal     <- 3                           # top 3 = focal domains
sep_y       <- n_cat - n_focal + 0.5       # 5.5: divider between groups
focal_idx   <- (n_cat - n_focal + 1):n_cat # 6,7,8
band_idx    <- seq(n_cat, 1, by = -2)      # grey bands on alternating rows

plot_data$category_label  <- factor(plot_data$category_label, levels = cat_levels)
plot_data$lifestyle_label <- factor(plot_data$lifestyle_label,
                                     levels = c("Car-free", "Flight-free", "Meat-free"))

# Colored strip headers; bars use dark/light shade of the lifestyle colour
strip_bg <- c("Car-free" = "#4477AA", "Flight-free" = "#EE6677", "Meat-free" = "#228833")
esi_cols <- list(
  "Car-free"    = c("High ESI (+1 SD)" = "#2255AA", "Low ESI (\u22121 SD)" = "#99BBDD"),
  "Flight-free" = c("High ESI (+1 SD)" = "#CC3344", "Low ESI (\u22121 SD)" = "#F7AABB"),
  "Meat-free"   = c("High ESI (+1 SD)" = "#115522", "Low ESI (\u22121 SD)" = "#88BB99")
)
grey_band <- "#ECECEC"

# Shared x-axis limits across all panels
x_lim    <- max(abs(plot_data$estimate_t), na.rm = TRUE) * 1.05
x_breaks <- pretty(c(-x_lim, x_lim), n = 4)

make_panel <- function(ls, show_y = TRUE, show_bracket = FALSE) {
  df        <- plot_data |> filter(lifestyle_label == ls) |> mutate(facet_label = ls)
  focal_lab <- focal_label_map[[ls]]
  yfocal    <- which(cat_levels == focal_lab)
  slash_w   <- x_lim * 0.10

  p <- ggplot(df, aes(x = category_label, y = estimate_t, fill = esi_level)) +
    # alternating grey row bands
    annotate("rect", xmin = band_idx - 0.5, xmax = band_idx + 0.5,
             ymin = -Inf, ymax = Inf, fill = grey_band) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey20") +
    # dotted divider between focal domains and remaining categories
    geom_vline(xintercept = sep_y, linetype = "dotted",
               linewidth = 0.4, colour = "grey45") +
    # N/A horizontal dash for this lifestyle's own focal domain
    annotate("segment", x = yfocal, xend = yfocal,
             y = -slash_w, yend = slash_w, colour = "grey55", linewidth = 0.9) +
    scale_x_discrete(limits = cat_levels, drop = FALSE) +
    facet_wrap(~facet_label) +
    coord_flip(ylim = c(-x_lim, x_lim), clip = "off") +
    scale_y_continuous(breaks = x_breaks,
                       labels = function(x) sprintf("%.1f", x)) +
    scale_fill_manual(values = esi_cols[[ls]], name = NULL, guide = "none") +
    labs(x = NULL,
         y = if (ls == "Flight-free")
               expression("Difference in emission (tCO"[2]*"e per person/year)")
             else NULL) +
    theme_minimal(base_size = 12.5) +
    theme(
      panel.background   = element_blank(),
      panel.border       = element_rect(colour = "grey80", fill = NA, linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      strip.background   = element_rect(fill = strip_bg[ls], colour = NA),
      strip.text         = element_text(face = "bold", size = 13.5, colour = "white"),
      axis.text.x  = element_text(size = 10.5),
      axis.title.x = element_text(size = 12),
      axis.text.y  = if (show_y) element_text(size = 11) else element_blank(),
      axis.ticks.y = if (show_y) element_line() else element_blank(),
      legend.position = "none",
      plot.margin = margin(4, if (show_bracket) 118 else 6, 4, if (show_y) 4 else 2)
    )

  # Right-hand group brackets (only on the rightmost panel)
  if (show_bracket) {
    bx  <- x_lim * 1.12          # bracket spine position (estimate units)
    tick <- x_lim * 0.045
    p <- p +
      annotate("segment", x = sep_y + 0.1, xend = n_cat + 0.45,
               y = bx, yend = bx, colour = "grey45", linewidth = 0.4) +
      annotate("segment", x = sep_y + 0.1, xend = sep_y + 0.1,
               y = bx, yend = bx - tick, colour = "grey45", linewidth = 0.4) +
      annotate("segment", x = n_cat + 0.45, xend = n_cat + 0.45,
               y = bx, yend = bx - tick, colour = "grey45", linewidth = 0.4) +
      annotate("text", x = (sep_y + 0.1 + n_cat + 0.45) / 2, y = bx + tick * 1.5,
               label = "Focal\ndomains", angle = -90, size = 3.8,
               colour = "grey35", lineheight = 0.9) +
      annotate("segment", x = 0.55, xend = sep_y - 0.1,
               y = bx, yend = bx, colour = "grey45", linewidth = 0.4) +
      annotate("segment", x = 0.55, xend = 0.55,
               y = bx, yend = bx - tick, colour = "grey45", linewidth = 0.4) +
      annotate("segment", x = sep_y - 0.1, xend = sep_y - 0.1,
               y = bx, yend = bx - tick, colour = "grey45", linewidth = 0.4) +
      annotate("text", x = (0.55 + sep_y - 0.1) / 2, y = bx + tick * 1.5,
               label = "Remaining\ncategories", angle = -90, size = 3.8,
               colour = "grey35", lineheight = 0.9)
  }
  p
}

# Custom legend: N/A dash, High ESI (dark), Low ESI (light)
legend_plot <- ggplot() + xlim(0, 12) + ylim(0, 1) +
  annotate("segment", x = 3.0, xend = 3.6, y = 0.5, yend = 0.5,
           colour = "grey55", linewidth = 0.9) +
  annotate("text", x = 3.8, y = 0.5, label = "N/A", hjust = 0,
           fontface = "italic", size = 4.2) +
  annotate("rect", xmin = 5.2, xmax = 5.8, ymin = 0.34, ymax = 0.66, fill = "#555555") +
  annotate("text", x = 5.95, y = 0.5, label = "High ESI", hjust = 0,
           fontface = "italic", size = 4.2) +
  annotate("rect", xmin = 7.8, xmax = 8.4, ymin = 0.34, ymax = 0.66, fill = "#BBBBBB") +
  annotate("text", x = 8.55, y = 0.5, label = "Low ESI", hjust = 0,
           fontface = "italic", size = 4.2) +
  theme_void() +
  theme(plot.margin = margin(2, 6, 0, 6),
        panel.border = element_rect(colour = "grey75", fill = NA, linewidth = 0.4))

p_panels <- make_panel("Car-free", TRUE) +
            make_panel("Flight-free", FALSE) +
            make_panel("Meat-free", FALSE, show_bracket = TRUE) +
  plot_layout(ncol = 3)

p <- legend_plot / p_panels + plot_layout(heights = c(1, 14))

ggsave(
  file.path("results", "category_decomposition_esi.png"),
  plot = p, width = 15.4, height = 7.6, dpi = 300, bg = "white"
)

cat("Saved: results/category_decomposition_esi.png\n")
