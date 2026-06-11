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
    mname     = factor(mname, levels = c("Meat-free", "Flight-free", "Car-free")),
    lifestyle = relevel(factor(if_else(lifestyle, "Adopter", "Non-adopter")),
                        ref = "Non-adopter")
  )

# --- Plots ---------------------------------------------------------------
# Shared design language: coloured lifestyle blocks, adopter half in the
# lifestyle colour, non-adopter half in grey.
.lvls_v   <- c("Meat-free", "Flight-free", "Car-free")          # bottom -> top
.life_col <- c("Car-free" = "#4477AA", "Flight-free" = "#EE6677", "Meat-free" = "#228833")
.fk_levels <- as.vector(rbind(paste(.lvls_v, "Non-adopter"), paste(.lvls_v, "Adopter")))
.fk_cols   <- setNames(ifelse(grepl("Non-adopter", .fk_levels), "#cccccc",
                              .life_col[sub(" Adopter", "", .fk_levels)]), .fk_levels)
.grey_v <- "#ECECEC"

plot_data <- plot_data |>
  mutate(fk = factor(paste(mname, lifestyle), levels = .fk_levels))

.violin_base <- function(data, blocks = TRUE) {
  blk <- data.frame(mname = factor(.lvls_v, levels = .lvls_v), pos = seq_along(.lvls_v))
  p <- ggplot(data, aes(x = mname, y = res, fill = fk)) +
    geom_split_violin(alpha = 0.85, colour = "grey40", linewidth = 0.3) +
    geom_boxplot(outliers = FALSE, width = 0.2, fill = "white", colour = "grey30") +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey45", linewidth = 0.4)
  if (blocks) {
    p <- p +
      geom_rect(data = blk, aes(xmin = pos - 0.46, xmax = pos + 0.46),
                ymin = -9.4, ymax = -6.7, fill = .life_col[.lvls_v], inherit.aes = FALSE) +
      geom_text(data = blk, aes(x = pos, y = -8.05, label = mname),
                colour = "white", fontface = "bold", size = 4.0, inherit.aes = FALSE)
  }
  p +
    scale_fill_manual(values = .fk_cols, guide = "none") +
    labs(x = NULL,
         y = expression(italic("Deviation from predicted indirect emissions (tCO"[2]*"e per person/year)"))) +
    coord_flip(ylim = c(-6, 8.5), clip = "off") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor    = element_blank(),
          axis.title.x = element_text(size = 10),
          axis.text.y  = if (blocks) element_blank() else element_text(size = 10),
          axis.ticks.y = element_blank(),
          plot.margin  = margin(6, 14, 6, if (blocks) 70 else 6))
}

# Custom legend: split glyph (grey = Non-adopter, lifestyle colour = Adopter)
legend_v <- ggplot() + xlim(0, 12) + ylim(0, 1) +
  annotate("rect", xmin = 2.0, xmax = 2.6, ymin = 0.34, ymax = 0.66, fill = "#cccccc") +
  annotate("text", x = 2.75, y = 0.5, label = "Non-adopter", hjust = 0,
           fontface = "italic", size = 3.8) +
  annotate("rect", xmin = 5.6, xmax = 5.73, ymin = 0.34, ymax = 0.66, fill = "#4477AA") +
  annotate("rect", xmin = 5.73, xmax = 5.86, ymin = 0.34, ymax = 0.66, fill = "#EE6677") +
  annotate("rect", xmin = 5.86, xmax = 5.99, ymin = 0.34, ymax = 0.66, fill = "#228833") +
  annotate("text", x = 6.14, y = 0.5, label = "Adopter (lifestyle colour)", hjust = 0,
           fontface = "italic", size = 3.8) +
  theme_void() +
  theme(plot.margin = margin(2, 6, 0, 6),
        panel.border = element_rect(colour = "grey75", fill = NA, linewidth = 0.4))

ggsave(file.path(output, "Residuals distribution ESI.png"),
       legend_v / (.violin_base(plot_data, blocks = FALSE) + facet_grid(. ~ esi_tetrile)) +
         plot_layout(heights = c(1, 13)),
       units = "cm", width = 24, height = 14, bg = "white")
ggsave(file.path(output, "Residuals distribution.png"),
       legend_v / .violin_base(plot_data) + plot_layout(heights = c(1, 13)),
       units = "cm", width = 16, height = 12, bg = "white")


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
