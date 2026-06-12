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
  library(sandwich); library(patchwork)
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
) |>
  mutate(color_group = paste(as.character(category), label))

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
.colours_lifestyle <- c("Car-free" = "#4477AA", "Flight-free" = "#EE6677", "Meat-free" = "#228833")
.colours_esi_combined <- c(
  "Car-free High ESI"     = "#2255AA", "Car-free Average"     = "#4477AA", "Car-free Low ESI"     = "#99BBDD",
  "Flight-free High ESI"  = "#CC3344", "Flight-free Average"  = "#EE6677", "Flight-free Low ESI"  = "#F7AABB",
  "Meat-free High ESI"    = "#115522", "Meat-free Average"    = "#228833", "Meat-free Low ESI"    = "#88BB99"
)
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
                   color = color_group, linetype = "Direct effect"),
               linewidth = 1.1, alpha = 0.9) +
  geom_segment(aes(x = x_mid, xend = x_mid,
                   y = y_solid_adjusted, yend = y_solid_adjusted + 0.13, color = color_group),
               linetype = "dotted", linewidth = 0.9, alpha = 0.7) +
  geom_segment(aes(x = x_mid, xend = arrow_start,
                   y = y_solid_adjusted + 0.13, yend = y_solid_adjusted + 0.13,
                   color = color_group, linetype = "Indirect effect"),
               linewidth = 0.85, alpha = 0.85) +
  geom_segment(aes(x = total_ci_low, xend = total_ci_high,
                   y = y_solid_adjusted + 0.13, yend = y_solid_adjusted + 0.13),
               color = "grey45", linewidth = 0.5, alpha = 0.8, na.rm = TRUE) +
  geom_segment(aes(x = arrow_start, xend = x_end,
                   y = y_solid_adjusted + 0.13, yend = y_solid_adjusted + 0.13, color = color_group),
               linetype = "solid", linewidth = 1,
               arrow = arrow(length = unit(0.2,"cm"), type = "closed"), alpha = 0.9) +
  geom_segment(aes(x = rebound_plot_x, xend = rebound_plot_x,
                   y = y_solid_adjusted - 0.07, yend = y_solid_adjusted + 0.07,
                   linetype = "Re-spending benchmark"),
               color = "#CC79A7", linewidth = 1.4, na.rm = TRUE) +
  facet_grid(category ~ ., switch = "y") +
  scale_color_manual(values = .colours_esi_combined, guide = guide_legend(ncol = 3, title = NULL)) +
  scale_linetype_manual(values = .linetypes_main) +
  scale_y_continuous(limits = c(0.5, 1.85), expand = c(0,0)) +
  scale_x_reverse(breaks = seq(0, 2.5, 0.5),
                  labels = function(x) ifelse(x == 0, "0.0", paste0("\u2212", sprintf("%.1f", x)))) +
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

# ---- Presentation main-effects plot (FIGURE 1 design) -----------------
# Single-panel layout: coloured lifestyle block (left), direct effect (solid)
# + indirect effect (dashed line ending in a filled circle) joined by a step,
# the re-spending benchmark (dotted line + diamond), and a grey "emission
# difference" box (gap between indirect endpoint and benchmark) annotated with
# a square bracket. Non-adopter baseline footprint shown in parentheses (right).
baseline_file <- file.path(output, "non_adopter_baselines.csv")
baseline_t <- if (file.exists(baseline_file)) {
  na_b <- read.csv(baseline_file)
  m    <- c("Car-free" = "no_car","Flight-free" = "no_flying","Meat-free" = "no_meat")
  setNames(sapply(names(m),
                  function(c) na_b$predicted_co2e_t[na_b$lifestyle == m[c]]),
           names(m))
} else setNames(rep(NA_real_, 3), c("Car-free","Flight-free","Meat-free"))

.lvl_main <- c("Car-free","Flight-free","Meat-free")   # top -> bottom
.yc_main  <- setNames(c(3, 2, 1), .lvl_main)
.grey_box <- "#ECECEC"

pres_main <- plot_data |>
  filter(scenario == "average") |>
  mutate(category = factor(as.character(category), levels = .lvl_main)) |>
  arrange(category) |>
  mutate(
    yc       = .yc_main[as.character(category)],
    base_lab = ifelse(is.na(baseline_t[as.character(category)]), "",
                      sprintf("(%.1f)", baseline_t[as.character(category)])),
    box_lo   = pmin(x_end, rebound_plot_x),
    box_hi   = pmax(x_end, rebound_plot_x),
    diff_lab = ifelse(is.na(rebound_mean), "", sprintf("%.1f", abs(rebound_mean)))
  )

# Axis + layout geometry (data units = emission reductions, plotted reversed)
x_top   <- ceiling(max(c(pres_main$x_mid, pres_main$x_end, pres_main$rebound_plot_x,
                         pres_main$box_hi), na.rm = TRUE) / 0.5) * 0.5
blk_in  <- x_top * 1.07
blk_out <- x_top * 1.36
base_x  <- -x_top * 0.04

# Vertical offsets within each lifestyle row (relative to row centre yc)
yo_dir <-  0.00     # direct effect (solid)
yo_ind <- -0.12     # indirect effect (dashed + circle)
yo_ben <-  0.13     # re-spending benchmark (dotted + diamond)
yo_blo <- -0.23; yo_bhi <- 0.23          # grey box extent
yo_brk <-  0.31; yo_num <- 0.41          # bracket + number

brk <- pres_main |> filter(diff_lab != "")

p_pres_main <- ggplot(pres_main) +
  # thin separators between lifestyle rows
  geom_hline(yintercept = c(1.5, 2.5), color = "grey85", linewidth = 0.4) +
  # grey "emission difference" box (drawn first, behind everything)
  geom_rect(aes(xmin = box_lo, xmax = box_hi,
                ymin = yc + yo_blo, ymax = yc + yo_bhi),
            fill = .grey_box, color = NA, na.rm = TRUE) +
  # coloured lifestyle block (left) with label
  geom_rect(aes(xmin = blk_in, xmax = blk_out, ymin = yc - 0.4, ymax = yc + 0.4,
                fill = category), color = NA) +
  geom_text(aes(x = (blk_in + blk_out) / 2, y = yc, label = category),
            color = "white", fontface = "bold", size = 4.4) +
  geom_vline(xintercept = 0, color = "#2c3e50", linewidth = 0.7) +
  # direct effect (solid)
  geom_segment(aes(x = 0, xend = x_mid, y = yc + yo_dir, yend = yc + yo_dir,
                   color = category), linewidth = 1.5) +
  # step connector between direct and indirect
  geom_segment(aes(x = x_mid, xend = x_mid, y = yc + yo_dir, yend = yc + yo_ind,
                   color = category), linewidth = 1.0) +
  # indirect effect (dashed) ending in a filled circle
  geom_segment(aes(x = x_mid, xend = x_end, y = yc + yo_ind, yend = yc + yo_ind,
                   color = category), linewidth = 1.2, linetype = "22") +
  geom_point(aes(x = x_end, y = yc + yo_ind, fill = category),
             shape = 21, size = 4.2, stroke = 0) +
  # re-spending benchmark (dotted) + diamond
  geom_segment(aes(x = x_mid, xend = x_mid, y = yc + yo_dir, yend = yc + yo_ben),
               linetype = "12", color = "#333333", linewidth = 0.7, na.rm = TRUE) +
  geom_segment(aes(x = x_mid, xend = rebound_plot_x, y = yc + yo_ben, yend = yc + yo_ben),
               linetype = "12", color = "#333333", linewidth = 0.7, na.rm = TRUE) +
  geom_point(aes(x = rebound_plot_x, y = yc + yo_ben), shape = 18, size = 4,
             color = "#111111", na.rm = TRUE) +
  # square bracket above the grey box + difference number
  geom_segment(data = brk, aes(x = box_lo, xend = box_hi,
                               y = yc + yo_brk, yend = yc + yo_brk),
               color = "#555555", linewidth = 0.5) +
  geom_segment(data = brk, aes(x = box_lo, xend = box_lo,
                               y = yc + yo_brk, yend = yc + yo_brk - 0.05),
               color = "#555555", linewidth = 0.5) +
  geom_segment(data = brk, aes(x = box_hi, xend = box_hi,
                               y = yc + yo_brk, yend = yc + yo_brk - 0.05),
               color = "#555555", linewidth = 0.5) +
  geom_text(data = brk, aes(x = (box_lo + box_hi) / 2, y = yc + yo_num, label = diff_lab),
            fontface = "italic", size = 3.8, color = "#333333") +
  # non-adopter baseline footprint (right)
  geom_text(aes(x = base_x, y = yc, label = base_lab),
            hjust = 0, size = 3.7, color = "grey25") +
  scale_fill_manual(values = .colours_lifestyle, guide = "none") +
  scale_color_manual(values = .colours_lifestyle, guide = "none") +
  scale_x_reverse(breaks = seq(0, x_top, 0.5),
                  labels = function(x) ifelse(x == 0, "0.0", paste0("\u2212", sprintf("%.1f", x))),
                  limits = c(blk_out * 1.02, base_x * 1.8)) +
  scale_y_continuous(limits = c(0.45, 3.65), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = expression(italic("Difference in emission (tCO"[2]*"e per person/year)")),
       y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor    = element_blank(),
        panel.grid.major.x  = element_line(color = "#EEEEEE", linewidth = 0.5),
        axis.text.y  = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11),
        plot.margin  = margin(4, 16, 6, 6))

# Custom shared legend, ordered to follow the figure's narrative:
# direct effect -> indirect effect -> re-spending benchmark -> emission difference
legend_plot <- ggplot() + xlim(0, 14.6) + ylim(0, 1) +
  annotate("segment", x = 0.4, xend = 1.3, y = 0.5, yend = 0.5,
           color = "#333333", linewidth = 1.3) +
  annotate("text", x = 1.45, y = 0.5, label = "Direct effect",
           hjust = 0, fontface = "italic", size = 3.8) +
  annotate("segment", x = 3.7, xend = 4.6, y = 0.5, yend = 0.5,
           linetype = "22", color = "#333333", linewidth = 1) +
  annotate("point", x = 3.7, y = 0.5, shape = 16, size = 3, color = "#333333") +
  annotate("text", x = 4.75, y = 0.5, label = "Indirect effect",
           hjust = 0, fontface = "italic", size = 3.8) +
  annotate("segment", x = 7.0, xend = 7.9, y = 0.5, yend = 0.5,
           linetype = "12", color = "#333333", linewidth = 1) +
  annotate("point", x = 7.45, y = 0.5, shape = 18, size = 3.6, color = "#111111") +
  annotate("text", x = 8.05, y = 0.5, label = "Re-spending benchmark",
           hjust = 0, fontface = "italic", size = 3.8) +
  annotate("rect", xmin = 11.7, xmax = 12.4, ymin = 0.34, ymax = 0.66, fill = .grey_box) +
  annotate("text", x = 12.55, y = 0.5, label = "Emission difference",
           hjust = 0, fontface = "italic", size = 3.8) +
  theme_void() +
  theme(plot.margin = margin(2, 10, 0, 6),
        panel.border = element_rect(color = "grey75", fill = NA, linewidth = 0.4))

p_pres_main_full <- legend_plot / p_pres_main + plot_layout(heights = c(1, 13))

ggsave(file.path(output, "Waterfall_pres_main.png"), p_pres_main_full,
       units = "cm", width = 24, height = 13, dpi = 200, bg = "white")

# ---- Presentation ESI plot (FIGURE 3 design) --------------------------
# Per lifestyle: two sub-rows (High ESI = dark shade on a grey band, Low ESI =
# light shade). Each sub-row shows direct (solid) + indirect (dashed + circle)
# joined by a step. Axis allows positive results (emission increases, shown +).
.lvl_esi <- c("Car-free","Flight-free","Meat-free")
.ctr_esi <- setNames(c(5, 3, 1), .lvl_esi)
.off_esi <- 0.32     # vertical offset of each ESI sub-row from its row centre

pres_esi <- plot_data |>
  filter(scenario %in% c("high_esi","low_esi")) |>
  mutate(category    = factor(as.character(category), levels = .lvl_esi),
         color_group = paste(as.character(category), label),
         ctr = .ctr_esi[as.character(category)],
         yc  = ctr + if_else(scenario == "high_esi", .off_esi, -.off_esi))

xr_hi   <- max(c(pres_esi$x_mid, pres_esi$x_end), na.rm = TRUE)
xr_lo   <- min(c(pres_esi$x_mid, pres_esi$x_end, 0), na.rm = TRUE)
x_top_e <- ceiling(xr_hi / 0.5) * 0.5
x_bot_e <- floor(min(xr_lo, 0) / 0.5) * 0.5
blk_in_e  <- x_top_e * 1.16
blk_out_e <- x_top_e * 1.46
lab_x_e   <- x_top_e * 1.03
right_lim <- min(x_bot_e, 0) - x_top_e * 0.04

yo_dir_e <-  0.00
yo_ind_e <- -0.13

blocks_e <- data.frame(category = factor(.lvl_esi, levels = .lvl_esi),
                       ctr = .ctr_esi[.lvl_esi])
bands_e  <- pres_esi |> filter(scenario == "high_esi") |> distinct(category, yc)

p_pres_esi <- ggplot(pres_esi) +
  # grey band behind High ESI sub-rows
  geom_rect(data = bands_e, aes(ymin = yc - .off_esi, ymax = yc + .off_esi),
            xmin = -Inf, xmax = Inf,
            fill = "#ECECEC", color = NA, inherit.aes = FALSE) +
  # coloured lifestyle blocks
  geom_rect(data = blocks_e,
            aes(xmin = blk_in_e, xmax = blk_out_e,
                ymin = ctr - 0.6, ymax = ctr + 0.6, fill = category),
            color = NA, inherit.aes = FALSE) +
  geom_text(data = blocks_e,
            aes(x = (blk_in_e + blk_out_e) / 2, y = ctr, label = category),
            color = "white", fontface = "bold", size = 4.4, inherit.aes = FALSE) +
  # ESI sub-row labels
  geom_text(aes(x = lab_x_e, y = yc, label = label),
            hjust = 1, fontface = "italic", size = 3.3, color = "grey35") +
  geom_vline(xintercept = 0, color = "#2c3e50", linewidth = 0.7) +
  # direct effect (solid)
  geom_segment(aes(x = 0, xend = x_mid, y = yc + yo_dir_e, yend = yc + yo_dir_e,
                   color = color_group), linewidth = 1.4) +
  # step connector
  geom_segment(aes(x = x_mid, xend = x_mid, y = yc + yo_dir_e, yend = yc + yo_ind_e,
                   color = color_group), linewidth = 0.9) +
  # indirect effect (dashed) + filled circle
  geom_segment(aes(x = x_mid, xend = x_end, y = yc + yo_ind_e, yend = yc + yo_ind_e,
                   color = color_group), linewidth = 1.1, linetype = "22") +
  geom_point(aes(x = x_end, y = yc + yo_ind_e, fill = color_group),
             shape = 21, size = 4, stroke = 0) +
  scale_color_manual(values = .colours_esi_combined, guide = "none") +
  scale_fill_manual(values = c(.colours_lifestyle, .colours_esi_combined), guide = "none") +
  scale_x_reverse(breaks = seq(x_bot_e, x_top_e, 0.5),
                  labels = function(x) ifelse(x == 0, "0.0",
                    ifelse(x > 0, paste0("\u2212", sprintf("%.1f", x)),
                           paste0("+", sprintf("%.1f", abs(x))))),
                  limits = c(blk_out_e * 1.02, right_lim)) +
  scale_y_continuous(limits = c(0.0, 6.0), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = expression(italic("Difference in emission (tCO"[2]*"e per person/year)")),
       y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor    = element_blank(),
        panel.grid.major.x  = element_line(color = "#EEEEEE", linewidth = 0.5),
        axis.text.y  = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11),
        plot.margin  = margin(4, 16, 6, 6))

legend_esi <- ggplot() + xlim(0, 14.6) + ylim(0, 1) +
  annotate("segment", x = 9.0, xend = 9.9, y = 0.5, yend = 0.5,
           linetype = "22", color = "#333333", linewidth = 1) +
  annotate("point", x = 9.0, y = 0.5, shape = 16, size = 3, color = "#333333") +
  annotate("text", x = 10.05, y = 0.5, label = "Indirect effect",
           hjust = 0, fontface = "italic", size = 3.8) +
  annotate("segment", x = 12.2, xend = 13.1, y = 0.5, yend = 0.5,
           color = "#333333", linewidth = 1.3) +
  annotate("text", x = 13.25, y = 0.5, label = "Direct effect",
           hjust = 0, fontface = "italic", size = 3.8) +
  theme_void() +
  theme(plot.margin = margin(2, 10, 0, 6),
        panel.border = element_rect(color = "grey75", fill = NA, linewidth = 0.4))

p_pres_esi_full <- legend_esi / p_pres_esi + plot_layout(heights = c(1, 13))

ggsave(file.path(output, "Waterfall_pres_esi.png"), p_pres_esi_full,
       units = "cm", width = 22, height = 13, dpi = 200, bg = "white")

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
