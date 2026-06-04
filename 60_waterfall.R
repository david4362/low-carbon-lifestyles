###############################################################################
# waterfall.R — main manuscript waterfall plot (+ optional variants).
# Always outputs:
#   <output>/Waterfall.png
#   <output>/Waterfall_pres_main.png
#   <output>/Waterfall_pres_esi.png
#   <output>/waterfall_stage_components.csv
#   <output>/waterfall_rebound_values.csv
#   <output>/waterfall_plot_values.csv
# When `export_waterfall_variants <- TRUE`:
#   <output>/Waterfall_main_no_arrows.png
#   <output>/Waterfall_combined.png
###############################################################################

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(stringr)
  library(sandwich)
})

if (!exists("export_waterfall_variants")) export_waterfall_variants <- FALSE

# ---- Load model summaries from interactions.R ---------------------------
lm_models_df    <- read.csv(file.path(output, "interaction regressions co2e.csv")) |> select(-X)
lm_models_kr_df <- read.csv(file.path(output, "interaction regressions kr.csv"))   |> select(-X)

# ---- Re-spending benchmark ---------------------------------------------
unspent <- lm_models_kr_df |>
  filter(grepl("Direct", model_name),
         grepl("no_car|no_flying|no_meat", term),
         !grepl("esi", term)) |>
  select(estimate, term)

.indirect_kr <- selected_spending |>
  filter(!category %in% non_purchase_categories) |>
  group_by(aid) |>
  summarise(
    indirect_flying_kr = sum(kr[broad_category != "Aviation_LDT_kr"]),
    indirect_meat_kr   = sum(kr[category       != "groceries.kr"]),
    indirect_car_kr    = sum(kr[broad_category != "Car_Public_kr"]),
    .groups = "drop"
  )

rebound <- selected_emissions |>
  group_by(aid) |>
  summarise(
    indirect_flying = sum(co2e[broad_category != "Aviation_LDT_co2e"]),
    indirect_meat   = sum(co2e[category       != "groceries.co2e"]),
    indirect_car    = sum(co2e[broad_category != "Car_Public_co2e"]),
    .groups = "drop"
  ) |>
  left_join(.indirect_kr, by = "aid") |>
  left_join(target_data,  by = "aid") |>
  summarise(
    no_carTRUE          = sum(indirect_car   [no_car == FALSE]) / sum(indirect_car_kr   [no_car == FALSE]),
    no_flyingTRUE       = sum(indirect_flying[no_flying == FALSE]) / sum(indirect_flying_kr[no_flying == FALSE]),
    no_meatTRUE         = sum(indirect_meat  [no_meat == FALSE]) / sum(indirect_meat_kr  [no_meat == FALSE]),
    no_carTRUE_focal    = sum(indirect_car   [no_car == TRUE])  / sum(indirect_car_kr   [no_car == TRUE]),
    no_flyingTRUE_focal = sum(indirect_flying[no_flying == TRUE]) / sum(indirect_flying_kr[no_flying == TRUE]),
    no_meatTRUE_focal   = sum(indirect_meat  [no_meat == TRUE]) / sum(indirect_meat_kr  [no_meat == TRUE])
  ) |>
  pivot_longer(everything(),
               names_to = c("term","is_focal"),
               names_pattern = "(no_.*TRUE)(_focal)?",
               values_to = "value") |>
  mutate(is_focal = if_else(is_focal == "_focal", "focal", "non_focal")) |>
  pivot_wider(names_from = is_focal, values_from = value) |>
  left_join(unspent, by = "term") |>
  mutate(
    direct_spending_change_kr = estimate,
    rebound_focal     = focal     * estimate / -1000,
    rebound_non_focal = non_focal * estimate / -1000,
    rebound_mean      = rowMeans(cbind(rebound_focal, rebound_non_focal), na.rm = TRUE),
    rebound_mean      = if_else(is.nan(rebound_mean), NA_real_, rebound_mean),
    scenario          = "average",
    term = dplyr::case_match(term,
                             "no_carTRUE"    ~ "Car-free",
                             "no_flyingTRUE" ~ "Flight-free",
                             "no_meatTRUE"   ~ "Meat-free",
                             .default = term)
  ) |>
  rename(category = term) |>
  select(category, scenario, direct_spending_change_kr, focal, non_focal,
         rebound_focal, rebound_non_focal, rebound_mean)

# ---- Stage components (direct + indirect base/interaction estimates) ----
stage_components <- lm_models_df |>
  filter(grepl("Direct|Indirect", model_name),
         grepl("no_car|no_flying|no_meat", term)) |>
  mutate(
    category = case_when(
      str_detect(term, "car")    ~ "Car-free",
      str_detect(term, "flying") ~ "Flight-free",
      str_detect(term, "meat")   ~ "Meat-free"
    ),
    stage          = if_else(str_detect(model_name, "Indirect"), "indirect", "direct"),
    is_interaction = str_detect(term, "esi:"),
    std_error      = if ("std.error" %in% names(lm_models_df)) std.error else NA_real_
  ) |>
  group_by(category, stage) |>
  summarise(
    base_val   = sum(estimate[!is_interaction]),
    inter_val  = sum(estimate[is_interaction]),
    base_se    = sqrt(sum((std_error[!is_interaction])^2, na.rm = TRUE)),
    inter_se   = sqrt(sum((std_error[is_interaction])^2, na.rm = TRUE)),
    average    = -base_val / 1000,
    average_se = base_se / 1000,
    .groups    = "drop"
  )

# ESI main-effect rows
esi_main_effects <- lm_models_df |>
  filter(grepl("Direct|Indirect", model_name), term == "esi") |>
  mutate(
    category = case_when(
      str_detect(model_name, regex("car",       ignore_case = TRUE)) ~ "Car-free",
      str_detect(model_name, regex("air|fly",   ignore_case = TRUE)) ~ "Flight-free",
      str_detect(model_name, regex("meat|diet", ignore_case = TRUE)) ~ "Meat-free"
    ),
    stage  = if_else(str_detect(model_name, "Indirect"), "indirect", "direct"),
    esi_se = if ("std.error" %in% names(lm_models_df)) std.error else NA_real_
  ) |>
  select(category, stage, esi_main = estimate, esi_se)

# HC3 SE for the lifestyle + esi*lifestyle linear combination at ±1 SD ESI
.lincomb_se <- function(category, stage, esi_val) {
  model_label <- paste(if (stage == "direct") "Direct" else "Indirect",
                       "emissions",
                       switch(category,
                              "Car-free" = "Car ownership",
                              "Flight-free" = "Air travel",
                              "Meat-free" = "Diet"))
  ls_term <- switch(category,
                    "Car-free" = "no_carTRUE",
                    "Flight-free" = "no_flyingTRUE",
                    "Meat-free" = "no_meatTRUE")
  m  <- lm_models[[model_label]]
  V  <- vcovHC(m, type = "HC3")
  ix <- paste0("esi:", ls_term)
  if (!ix %in% rownames(V)) ix <- paste0(ls_term, ":esi")
  terms    <- c(ls_term, "esi", ix)
  contrast <- c(1, esi_val, esi_val)
  sqrt(as.numeric(t(contrast) %*% V[terms, terms] %*% contrast)) / 1000
}

stage_components <- stage_components |>
  left_join(esi_main_effects, by = c("category","stage")) |>
  mutate(
    high_esi = -(base_val + inter_val + esi_main) / 1000,
    low_esi  = -(base_val - inter_val - esi_main) / 1000
  ) |>
  rowwise() |>
  mutate(high_esi_se = .lincomb_se(category, stage,  1),
         low_esi_se  = .lincomb_se(category, stage, -1)) |>
  ungroup()

# ---- Plot data ---------------------------------------------------------
plot_data <- stage_components |>
  pivot_longer(c(high_esi, average, low_esi),
               names_to = "scenario", values_to = "value") |>
  select(category, stage, scenario, value) |>
  pivot_wider(names_from = stage, values_from = value) |>
  left_join(
    stage_components |>
      pivot_longer(c(high_esi_se, average_se, low_esi_se),
                   names_to = "scenario_se", values_to = "value_se") |>
      mutate(scenario = str_replace(scenario_se, "_se$", "")) |>
      select(category, stage, scenario, value_se) |>
      pivot_wider(names_from = stage, values_from = value_se, names_prefix = "se_"),
    by = c("category","scenario")
  ) |>
  left_join(
    stage_components |>
      select(category, stage, base_val, inter_val) |>
      pivot_wider(names_from = stage, values_from = c(base_val, inter_val), names_sep = "_"),
    by = "category"
  ) |>
  mutate(
    baseline = 0, x_mid = baseline + direct, x_end = x_mid + indirect,
    category = factor(category, levels = c("Car-free","Flight-free","Meat-free")),
    label = dplyr::case_match(scenario,
                              "average"  ~ "Average",
                              "high_esi" ~ "High ESI",
                              "low_esi"  ~ "Low ESI"),
    y_solid  = dplyr::case_match(scenario,
                                 "average"  ~ 1.5,
                                 "high_esi" ~ 1.0,
                                 "low_esi"  ~ 0.7),
    y_dotted = dplyr::case_match(scenario,
                                 "average"  ~ 1.6,
                                 "high_esi" ~ 1.1,
                                 "low_esi"  ~ 0.8),
    arrow_start    = x_end - sign(x_end - x_mid) * 0.01,
    direct_ci_low  = x_mid - 1.96 * se_direct,   direct_ci_high = x_mid + 1.96 * se_direct,
    total_ci_low   = x_end - 1.96 * se_indirect, total_ci_high  = x_end + 1.96 * se_indirect
  ) |>
  left_join(rebound, by = c("scenario","category")) |>
  mutate(rebound_plot_x = x_mid - rebound_mean)

# Sort categories by total magnitude
.cat_order <- plot_data |> filter(scenario == "average") |>
  group_by(category) |>
  summarise(m = abs(mean(x_end, na.rm = TRUE)), .groups = "drop") |>
  arrange(desc(m)) |> pull(category) |> as.character()

plot_data <- plot_data |> mutate(
  category = factor(as.character(category), levels = .cat_order),
  indirect_sign = case_when(indirect > 0 ~ "positive",
                            indirect < 0 ~ "negative",
                            TRUE ~ "neutral"),
  y_solid_adjusted = dplyr::case_match(scenario,
                                       "average"  ~ 1.5,
                                       "high_esi" ~ 1.0,
                                       "low_esi"  ~ 0.65)
)

# ---- CSV exports -------------------------------------------------------
write.csv(stage_components, file.path(output, "waterfall_stage_components.csv"), row.names = FALSE)
write.csv(rebound |> rename(`Redirected spending` = rebound_mean),
          file.path(output, "waterfall_rebound_values.csv"), row.names = FALSE)
write.csv(
  plot_data |> mutate(category = as.character(category)) |>
    select(category, scenario, label,
           direct, indirect, base_val_direct, inter_val_direct,
           base_val_indirect, inter_val_indirect,
           baseline, x_mid, x_end, arrow_start,
           direct_ci_low, direct_ci_high, total_ci_low, total_ci_high,
           y_solid, y_dotted, direct_spending_change_kr, focal, non_focal,
           rebound_focal, rebound_non_focal, rebound_mean, rebound_plot_x),
  file.path(output, "waterfall_plot_values.csv"), row.names = FALSE
)

# ---- Shared ggplot helpers --------------------------------------------
.linetypes_main <- c("Direct effect" = "solid",
                     "Indirect effect" = "dotted",
                     "Re-spending benchmark" = "solid")
.colours_esi    <- c("High ESI" = "#333333",
                     "Average"  = "#808080",
                     "Low ESI"  = "#cccccc")
.theme_wf <- function(strip_size = 11, x_size = 11) theme_minimal() + theme(
  strip.background = element_rect(fill = "#f5f5f5", color = NA),
  panel.background = element_rect(fill = "#f5f5f5", color = NA),
  plot.background  = element_rect(fill = "white",   color = NA),
  panel.spacing.y  = unit(0.4, "cm"),
  panel.grid.major.x = element_line(color = "#f5f5f5", linewidth = 0.5),
  panel.grid.major.y = element_blank(),
  panel.grid.minor   = element_blank(),
  strip.text.y.left  = element_text(angle = 0, hjust = 0, vjust = 1,
                                    face = "bold", size = strip_size),
  strip.placement    = "outside",
  strip.switch.pad.grid = unit(0, "cm"),
  legend.position    = "top",
  legend.justification = "left",
  legend.box         = "vertical",
  legend.margin      = margin(l = -10, b = 12),
  legend.spacing.y   = unit(0, "cm"),
  axis.text.y        = element_blank(),
  axis.ticks.y       = element_blank(),
  plot.margin        = margin(t = 20, r = 20, b = 20, l = 20),
  axis.text.x        = element_text(size = x_size)
)

# ---- Main manuscript Waterfall.png ------------------------------------
p <- ggplot(plot_data) +
  geom_rect(data = plot_data |> filter(indirect_sign == "positive") |>
              distinct(category, indirect_sign),
            xmin = 0, xmax = Inf, ymin = 0.5, ymax = Inf,
            fill = "#e8f5e9", alpha = 0.25, inherit.aes = FALSE) +
  geom_rect(data = plot_data |> filter(indirect_sign == "negative") |>
              distinct(category, indirect_sign),
            xmin = -Inf, xmax = 0, ymin = 0.5, ymax = Inf,
            fill = "#e8f5e9", alpha = 0.25, inherit.aes = FALSE) +
  geom_vline(xintercept = 0, color = "#2c3e50", linewidth = 0.8) +
  geom_segment(aes(x = direct_ci_low, xend = direct_ci_high,
                   y = y_solid_adjusted, yend = y_solid_adjusted),
               color = "grey45", linewidth = 0.5, alpha = 0.8, na.rm = TRUE) +
  geom_segment(aes(x = baseline, xend = x_mid,
                   y = y_solid_adjusted, yend = y_solid_adjusted,
                   color = label, linetype = "Direct effect"),
               linewidth = 1.1, alpha = 0.9) +
  geom_segment(aes(x = x_mid, xend = x_mid,
                   y = y_solid_adjusted, yend = y_solid_adjusted + 0.13, color = label),
               linetype = "dotted", linewidth = 0.9, alpha = 0.7) +
  geom_segment(aes(x = x_mid, xend = arrow_start,
                   y = y_solid_adjusted + 0.13, yend = y_solid_adjusted + 0.13,
                   color = label, linetype = "Indirect effect"),
               linewidth = 0.85, alpha = 0.85) +
  geom_segment(aes(x = total_ci_low, xend = total_ci_high,
                   y = y_solid_adjusted + 0.13, yend = y_solid_adjusted + 0.13),
               color = "grey45", linewidth = 0.5, alpha = 0.8, na.rm = TRUE) +
  geom_segment(aes(x = arrow_start, xend = x_end,
                   y = y_solid_adjusted + 0.13, yend = y_solid_adjusted + 0.13, color = label),
               linetype = "solid", linewidth = 1,
               arrow = arrow(length = unit(0.2,"cm"), type = "closed"), alpha = 0.9) +
  geom_segment(aes(x = rebound_plot_x, xend = rebound_plot_x,
                   y = y_solid_adjusted - 0.07, yend = y_solid_adjusted + 0.07,
                   linetype = "Re-spending benchmark"),
               color = "#CC79A7", linewidth = 1.4, na.rm = TRUE) +
  facet_grid(category ~ ., switch = "y") +
  scale_color_manual(values = .colours_esi) +
  scale_linetype_manual(values = .linetypes_main) +
  scale_y_continuous(limits = c(0.5, 1.85), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(0, 2.5, 0.5),
                     labels = function(x) ifelse(x == 0, "0.0",
                       paste0("\u2212", sprintf("%.1f", x)))) +
  labs(x = expression("Emission differences (OLS model-estimated), CO"[2]*"e"),
       y = NULL, color = NULL, linetype = NULL,
       caption = paste("Thin gray whiskers: 95% confidence intervals.",
                       "Light background: shows emission direction.",
                       "Direct and indirect effects separated vertically to highlight ESI heterogeneity.",
                       sep = "\n")) +
  guides(color = guide_legend(order = 1, override.aes = list(alpha = 1, linewidth = 1.1, linetype = 1)),
         linetype = guide_legend(order = 2,
                                 override.aes = list(color = c("gray40","gray40","#CC79A7"),
                                                     linewidth = c(1, 1, 1.4)))) +
  .theme_wf() + theme(axis.line.x = element_line(arrow = arrow(length = unit(0.3,"cm"), ends = "last")),
                      plot.caption = element_text(size = 8.5, color = "#555555",
                                                  hjust = 0, lineheight = 1.3),
                      plot.caption.position = "plot")

ggsave(file.path(output, "Waterfall.png"), p,
       units = "cm", width = 24, height = 14)

# ---- Presentation main-effects plot -----------------------------------
baseline_file <- file.path(output, "non_adopter_baselines.csv")
bl_labels <- if (file.exists(baseline_file)) {
  na_b <- read.csv(baseline_file)
  m    <- c("Car-free" = "no_car","Flight-free" = "no_flying","Meat-free" = "no_meat")
  data.frame(
    category = factor(names(m), levels = c("Car-free","Flight-free","Meat-free")),
    label    = sapply(names(m), function(c)
      sprintf("(%.1f t)", na_b$predicted_co2e_t[na_b$lifestyle == m[c]]))
  )
} else NULL

pres_main <- plot_data |> filter(scenario == "average")

p_pres_main <- ggplot(pres_main) +
  geom_vline(xintercept = 0, color = "#2c3e50", linewidth = 0.8) +
  { if (!is.null(bl_labels))
      geom_text(data = bl_labels, aes(x = 0, y = 1.5, label = label),
                size = 3.2, color = "grey45", fontface = "italic", hjust = 0.5) } +
  geom_segment(aes(x = baseline, xend = x_mid, y = 1.2, yend = 1.2,
                   linetype = "Direct effect"), color = "black", linewidth = 1.3) +
  geom_segment(aes(x = x_mid, xend = x_mid, y = 1.2, yend = 1.35),
               linetype = "dotted", linewidth = 0.9, color = "black") +
  geom_segment(aes(x = x_mid, xend = x_end, y = 1.35, yend = 1.35,
                   linetype = "Indirect effect"), color = "black", linewidth = 1.1) +
  geom_segment(aes(x = x_end, xend = x_end, y = 1.35 - 0.07, yend = 1.35 + 0.07),
               color = "black", linewidth = 1.3) +
  geom_segment(aes(x = rebound_plot_x, xend = rebound_plot_x,
                   y = 1.2 - 0.07, yend = 1.2 + 0.07,
                   linetype = "Re-spending benchmark"),
               color = "#CC79A7", linewidth = 1.6, na.rm = TRUE) +
  facet_grid(category ~ ., switch = "y") +
  scale_linetype_manual(values = .linetypes_main) +
  scale_y_continuous(limits = c(1.0, 1.55), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(0, 2.5, 0.5),
                     labels = function(x) ifelse(x == 0, "0.0",
                       paste0("\u2212", sprintf("%.1f", x)))) +
  labs(x = expression("Emission differences (tCO"[2]*"e/year)"),
       y = NULL, linetype = NULL) +
  guides(linetype = guide_legend(override.aes = list(
    color = c("gray20","gray20","#CC79A7"), linewidth = c(1.2,1.2,1.6)))) +
  .theme_wf(strip_size = 13)

ggsave(file.path(output, "Waterfall_pres_main.png"), p_pres_main,
       units = "cm", width = 22, height = 12, dpi = 200)

# ---- Presentation ESI plot --------------------------------------------
pres_esi <- plot_data |>
  filter(scenario %in% c("high_esi","low_esi")) |>
  mutate(y_solid_p  = if_else(scenario == "high_esi", 1.3, 0.9),
         y_dotted_p = if_else(scenario == "high_esi", 1.45, 1.05))

p_pres_esi <- ggplot(pres_esi) +
  geom_vline(xintercept = 0, color = "#2c3e50", linewidth = 0.8) +
  geom_segment(aes(x = baseline, xend = x_mid,
                   y = y_solid_p, yend = y_solid_p, color = label,
                   linetype = "Direct effect"), linewidth = 1.3) +
  geom_segment(aes(x = x_mid, xend = x_mid,
                   y = y_solid_p, yend = y_dotted_p, color = label),
               linetype = "dotted", linewidth = 0.9) +
  geom_segment(aes(x = x_mid, xend = arrow_start,
                   y = y_dotted_p, yend = y_dotted_p,
                   color = label, linetype = "Indirect effect"), linewidth = 1.0) +
  geom_segment(aes(x = arrow_start, xend = x_end,
                   y = y_dotted_p, yend = y_dotted_p, color = label),
               linewidth = 1.2,
               arrow = arrow(length = unit(0.22,"cm"), type = "closed")) +
  facet_grid(category ~ ., switch = "y") +
  scale_color_manual(values = c("High ESI" = "#333333","Low ESI" = "#999999")) +
  scale_linetype_manual(values = c("Direct effect" = "solid","Indirect effect" = "dotted")) +
  scale_y_continuous(limits = c(0.7, 1.65), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(-0.5, 2.5, 0.5),
                     labels = function(x) ifelse(x == 0, "0.0",
                       ifelse(x > 0, paste0("\u2212", sprintf("%.1f", x)),
                              paste0("+", sprintf("%.1f", abs(x)))))) +
  labs(x = expression("Emission differences (tCO"[2]*"e/year)"),
       y = NULL, color = NULL, linetype = NULL) +
  guides(color    = guide_legend(order = 1, override.aes = list(linewidth = 1.5, linetype = 1)),
         linetype = guide_legend(order = 2, override.aes = list(color = "gray30", linewidth = 1.2))) +
  .theme_wf(strip_size = 13)

ggsave(file.path(output, "Waterfall_pres_esi.png"), p_pres_esi,
       units = "cm", width = 22, height = 12, dpi = 200)

# ---- Optional variants -------------------------------------------------
if (isTRUE(export_waterfall_variants)) {
  p_main <- ggplot(pres_main) +
    geom_vline(xintercept = 0, color = "#2c3e50", linewidth = 0.8) +
    geom_segment(aes(x = direct_ci_low, xend = direct_ci_high,
                     y = y_solid, yend = y_solid),
                 color = "grey45", linewidth = 0.5, alpha = 0.8, na.rm = TRUE) +
    geom_segment(aes(x = baseline, xend = x_mid, y = y_solid, yend = y_solid,
                     linetype = "Direct effect"), color = "black", linewidth = 1) +
    geom_segment(aes(x = x_mid, xend = x_mid, y = y_solid, yend = y_dotted),
                 linetype = "dotted", linewidth = 0.8, color = "black") +
    geom_segment(aes(x = x_mid, xend = x_end, y = y_dotted, yend = y_dotted,
                     linetype = "Indirect effect"), color = "black", linewidth = 0.9) +
    geom_segment(aes(x = total_ci_low, xend = total_ci_high,
                     y = y_dotted, yend = y_dotted),
                 color = "grey45", linewidth = 0.5, alpha = 0.8, na.rm = TRUE) +
    geom_segment(aes(x = x_end, xend = x_end, y = y_dotted - 0.06, yend = y_dotted + 0.06),
                 color = "black", linewidth = 1.2) +
    geom_segment(aes(x = rebound_plot_x, xend = rebound_plot_x,
                     y = y_solid - 0.06, yend = y_solid + 0.06,
                     linetype = "Re-spending benchmark"),
                 color = "#CC79A7", linewidth = 1.4, na.rm = TRUE) +
    facet_grid(category ~ ., switch = "y") +
    scale_linetype_manual(values = .linetypes_main) +
    scale_y_continuous(limits = c(1.35, 1.7), expand = c(0,0)) +
    scale_x_continuous(breaks = seq(0, 2, 0.5)) + .theme_wf(strip_size = 10)
  ggsave(file.path(output, "Waterfall_main_no_arrows.png"), p_main,
         units = "cm", width = 20, height = 10)
}
