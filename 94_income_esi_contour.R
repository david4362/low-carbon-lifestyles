###############################################################################
# Income × ESI contour plot for total carbon footprints.
#
# Uses local mock/cached data when raw TRE data are unavailable. Produces both
# line-contour and filled-contour variants from a smoothed person-level surface.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(mgcv)
  library(scales)
  library(patchwork)
  library(MASS)
  library(psych)
})

filter <- dplyr::filter
select <- dplyr::select
group_by <- dplyr::group_by
summarise <- dplyr::summarise
mutate <- dplyr::mutate
transmute <- dplyr::transmute
left_join <- dplyr::left_join
semi_join <- dplyr::semi_join
if_else <- dplyr::if_else
rename <- dplyr::rename

.src <- function(f) {
  for (p in c(f, file.path("R", f))) {
    if (file.exists(p)) {
      source(p, keep.source = TRUE)
      return(invisible())
    }
  }
  stop("Missing required script: ", f)
}

.src("00_constants.R")
.src("01_utils.R")
.src("10_load_data.R")
suppressPackageStartupMessages(library(dplyr, warn.conflicts = FALSE))
suppressPackageStartupMessages(library(tidyr, warn.conflicts = FALSE))

if (!exists("output") || is.null(output) || !nzchar(output)) {
  output <- if (file.exists("default_filter.RData")) file.path("output", "mock") else "output"
}
dir.create(output, recursive = TRUE, showWarnings = FALSE)
dir.create("results", recursive = TRUE, showWarnings = FALSE)

if (!all(c("selected_aids", "selected_months", "monthly_emissions", "monthly_spending") %in% ls())) {
  source_R("20_filter_data.R")
}

selected_emissions <- monthly_emissions |>
  filter(aid %in% selected_aids) |>
  semi_join(selected_months, by = c("aid", "date"))

selected_spending <- monthly_spending |>
  filter(aid %in% selected_aids) |>
  semi_join(selected_months, by = c("aid", "date"))

esi_input <- survey |>
  filter(aid %in% selected_aids) |>
  select(aid, array3_8, array3_9, array3_11)

esi_fit <- psych::fa(esi_input |> select(-aid), rotate = "none")
esi_scores <- tibble(
  aid = esi_input$aid,
  esi = as.numeric(scale(esi_fit$scores))
)

person_footprint <- aggregate_person_co2e(selected_emissions) |>
  transmute(aid, total_co2e_t = total / 1000)

person_spending <- aggregate_person_kr(selected_spending, drop_non_cost = TRUE) |>
  transmute(
    aid,
    total_kr = total,
    total_spend_ksek = total / 1000
  )

person_income <- monthly_incomes |>
  filter(aid %in% selected_aids) |>
  semi_join(selected_months, by = c("aid", "date")) |>
  group_by(aid, date) |>
  summarise(income = sum(income, na.rm = TRUE), .groups = "drop") |>
  group_by(aid) |>
  summarise(mean_monthly_income = mean(income, na.rm = TRUE), .groups = "drop")

analysis_data_raw <- esi_scores |>
  select(aid, esi) |>
  left_join(person_income, by = "aid") |>
  left_join(person_spending, by = "aid") |>
  left_join(person_footprint, by = "aid") |>
  mutate(
    income_ksek = mean_monthly_income / 1000,
    intensity_kg_per_ksek = if_else(
      total_spend_ksek > 0,
      (total_co2e_t * 1000) / total_spend_ksek,
      NA_real_
    )
  ) |>
  filter(
    is.finite(esi),
    is.finite(income_ksek),
    is.finite(total_co2e_t),
    is.finite(intensity_kg_per_ksek)
  )

trim_surface_data <- function(data, outcome_col) {
  income_bounds <- quantile(data$income_ksek, c(0.01, 0.99), na.rm = TRUE)
  outcome_bounds <- quantile(data[[outcome_col]], c(0.01, 0.99), na.rm = TRUE)

  data |>
    filter(
      .data$income_ksek >= income_bounds[[1]],
      .data$income_ksek <= income_bounds[[2]],
      .data[[outcome_col]] >= outcome_bounds[[1]],
      .data[[outcome_col]] <= outcome_bounds[[2]]
    )
}

build_surface_grid <- function(data, outcome_col) {
  surface_model <- mgcv::gam(
    stats::as.formula(sprintf("%s ~ s(esi, income_ksek, k = 60)", outcome_col)),
    data = data,
    method = "REML"
  )

  grid <- expand.grid(
    esi = seq(min(data$esi), max(data$esi), length.out = 180),
    income_ksek = seq(min(data$income_ksek), max(data$income_ksek), length.out = 180)
  )

  pred <- predict(surface_model, newdata = grid, se.fit = TRUE)
  grid$pred <- as.numeric(pred$fit)
  grid$pred_lo <- grid$pred - 1.96 * as.numeric(pred$se.fit)
  grid$pred_hi <- grid$pred + 1.96 * as.numeric(pred$se.fit)

  density_surface <- MASS::kde2d(
    x = data$esi,
    y = data$income_ksek,
    n = 180,
    lims = c(range(grid$esi), range(grid$income_ksek))
  )

  .nearest_index <- function(values, grid_values) {
    pmax(1L, pmin(length(grid_values), findInterval(values, grid_values)))
  }

  grid$density <- density_surface$z[cbind(
    .nearest_index(grid$esi, density_surface$x),
    .nearest_index(grid$income_ksek, density_surface$y)
  )]

  density_cutoff <- quantile(grid$density, 0.15, na.rm = TRUE)
  grid <- grid |>
    mutate(pred_masked = if_else(density >= density_cutoff, pred, NA_real_))

  list(model = surface_model, grid = grid)
}

make_surface_plots <- function(data, grid, title, fill_name, file_stub, line_colour = "#0F5D7A") {
  if (nrow(data) < 100) {
    stop("Too few complete observations for a stable contour surface.")
  }

  surface_points <- data |>
    sample_n(min(800, n()))

  base_plot <- ggplot(grid, aes(x = .data$esi, y = .data$income_ksek)) +
    geom_point(
      data = surface_points,
      inherit.aes = FALSE,
      aes(x = .data$esi, y = .data$income_ksek),
      colour = "grey55",
      alpha = 0.28,
      size = 0.9
    ) +
    labs(
      x = "Environmental self-identity (standardized)",
      y = "Mean monthly disposable income (kSEK)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "grey75", fill = NA, linewidth = 0.4),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10)
    )

  p_lines <- base_plot +
    geom_contour(
      aes(z = .data$pred_masked),
      colour = line_colour,
      bins = 9,
      linewidth = 0.45,
      na.rm = TRUE
    ) +
    labs(
      title = title,
      subtitle = "Contour lines from a smoothed surface; display trimmed to the central 98% range"
    )

  p_filled <- base_plot +
    geom_contour_filled(
      aes(z = .data$pred_masked),
      bins = 9,
      na.rm = TRUE,
      alpha = 0.92
    ) +
    scale_fill_brewer(
      palette = "YlOrRd",
      direction = 1,
      name = fill_name
    ) +
    labs(
      title = title,
      subtitle = "Filled contours make the gradient easier to read; display trimmed to the central 98% range"
    )

  comparison_plot <- p_lines + p_filled + plot_layout(guides = "collect")

  ggsave(
    file.path("results", sprintf("%s_lines.png", file_stub)),
    plot = p_lines,
    width = 7.2,
    height = 5.6,
    dpi = 300,
    bg = "white"
  )

  ggsave(
    file.path("results", sprintf("%s_filled.png", file_stub)),
    plot = p_filled,
    width = 7.2,
    height = 5.6,
    dpi = 300,
    bg = "white"
  )

  ggsave(
    file.path("results", sprintf("%s_comparison.png", file_stub)),
    plot = comparison_plot,
    width = 12.8,
    height = 5.8,
    dpi = 300,
    bg = "white"
  )

  list(lines = p_lines, filled = p_filled, comparison = comparison_plot)
}

footprint_data <- trim_surface_data(analysis_data_raw, "total_co2e_t")
footprint_surface <- build_surface_grid(footprint_data, "total_co2e_t")
footprint_plots <- make_surface_plots(
  data = footprint_data,
  grid = footprint_surface$grid,
  title = "Predicted annual carbon footprint",
  fill_name = expression("tCO"[2] * "e/year"),
  file_stub = "income_esi_contour"
)

intensity_data <- trim_surface_data(analysis_data_raw, "intensity_kg_per_ksek")
intensity_surface <- build_surface_grid(intensity_data, "intensity_kg_per_ksek")
intensity_plots <- make_surface_plots(
  data = intensity_data,
  grid = intensity_surface$grid,
  title = "Predicted carbon intensity of consumption",
  fill_name = "kg CO2e / kSEK",
  file_stub = "income_esi_intensity",
  line_colour = "#7A3E00"
)

control_data_local <- build_control_data(
  users = users,
  monthly_incomes = monthly_incomes,
  months = selected_months,
  aids = selected_aids
)

target_data_local <- build_target_data(
  users = users,
  emissions = selected_emissions,
  esi_tbl = esi_scores,
  control_data = control_data_local
)

partial_covars <- c(
  "no_car",
  "no_flying",
  "no_meat",
  setdiff(names(control_data_local), c("aid", "income_centered"))
)

partial_data <- analysis_data_raw |>
  left_join(
    target_data_local |>
      select(aid, all_of(partial_covars)),
    by = "aid"
  ) |>
  filter(stats::complete.cases(across(all_of(partial_covars))))

residualize_formula <- stats::as.formula(
  paste("intensity_kg_per_ksek ~", paste(partial_covars, collapse = " + "))
)

residualize_model <- stats::lm(residualize_formula, data = partial_data)
partial_data <- partial_data |>
  mutate(intensity_resid = as.numeric(stats::residuals(residualize_model)))

residual_intensity_data <- trim_surface_data(partial_data, "intensity_resid")
residual_intensity_surface <- build_surface_grid(residual_intensity_data, "intensity_resid")
residual_intensity_plots <- make_surface_plots(
  data = residual_intensity_data,
  grid = residual_intensity_surface$grid,
  title = "Residual carbon intensity (controls and lifestyles removed)",
  fill_name = "Residual kg CO2e / kSEK",
  file_stub = "income_esi_intensity_residual",
  line_colour = "#355C7D"
)

model_data <- intensity_data |>
  mutate(
    income_10k_centered = (income_ksek - mean(income_ksek, na.rm = TRUE)) / 10
  )

intensity_model_rb <- fit_robust(
  intensity_kg_per_ksek ~ esi * income_10k_centered,
  data = model_data,
  glance = TRUE
)
intensity_model <- attr(intensity_model_rb, "model")

model_summary <- intensity_model_rb |>
  transmute(
    term = variable,
    estimate = round(estimate, 3),
    std_error = round(stderr, 3),
    p_value = signif(p, 3),
    r_squared = round(r.squared, 3),
    adj_r_squared = round(adj.r.squared, 3),
    n = model_n
  )

write.csv(model_summary, file.path("results", "income_esi_intensity_model.csv"), row.names = FALSE)

model_text <- c(
  "Robust linear interaction model",
  "Outcome: kg CO2e / kSEK",
  "Model: intensity ~ ESI x income",
  sprintf("N = %d", nrow(model_data)),
  sprintf("R^2 = %.3f", summary(intensity_model)$r.squared),
  "",
  sprintf("Intercept: %.3f (p = %.3g)", intensity_model_rb$estimate[intensity_model_rb$variable == "(Intercept)"], intensity_model_rb$p[intensity_model_rb$variable == "(Intercept)"]),
  sprintf("ESI: %.3f (p = %.3g)", intensity_model_rb$estimate[intensity_model_rb$variable == "esi"], intensity_model_rb$p[intensity_model_rb$variable == "esi"]),
  sprintf("Income, 10k SEK: %.3f (p = %.3g)", intensity_model_rb$estimate[intensity_model_rb$variable == "income_10k_centered"], intensity_model_rb$p[intensity_model_rb$variable == "income_10k_centered"]),
  sprintf("ESI x income: %.3f (p = %.3g)", intensity_model_rb$estimate[intensity_model_rb$variable == "esi:income_10k_centered"], intensity_model_rb$p[intensity_model_rb$variable == "esi:income_10k_centered"])
)

writeLines(model_text, con = file.path("results", "income_esi_intensity_model.txt"))

model_panel <- ggplot() +
  annotate(
    "text",
    x = 0,
    y = 1,
    label = paste(model_text, collapse = "\n"),
    hjust = 0,
    vjust = 1,
    family = "mono",
    size = 3.7
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    panel.border = element_rect(colour = "grey75", fill = NA, linewidth = 0.4)
  )

ggsave(
  file.path("results", "income_esi_intensity_with_model.png"),
  plot = intensity_plots$filled + model_panel + plot_layout(widths = c(2.0, 1.35)),
  width = 12.6,
  height = 5.8,
  dpi = 300,
  bg = "white"
)

resid_model_data <- residual_intensity_data |>
  mutate(
    income_10k_centered = (income_ksek - mean(income_ksek, na.rm = TRUE)) / 10
  )

resid_interaction_rb <- fit_robust(
  intensity_resid ~ esi * income_10k_centered,
  data = resid_model_data,
  glance = TRUE
)
resid_interaction_model <- attr(resid_interaction_rb, "model")

resid_model_summary <- resid_interaction_rb |>
  transmute(
    term = variable,
    estimate = round(estimate, 3),
    std_error = round(stderr, 3),
    p_value = signif(p, 3),
    r_squared = round(r.squared, 3),
    adj_r_squared = round(adj.r.squared, 3),
    n = model_n
  )

write.csv(
  resid_model_summary,
  file.path("results", "income_esi_intensity_residual_model.csv"),
  row.names = FALSE
)

resid_model_text <- c(
  "Robust linear interaction model",
  "Outcome: residual kg CO2e / kSEK",
  "Model: residual intensity ~ ESI x income",
  "Adjusted for demographics and no_car/no_flying/no_meat",
  sprintf("N = %d", nrow(resid_model_data)),
  sprintf("R^2 = %.3f", summary(resid_interaction_model)$r.squared),
  "",
  sprintf("Intercept: %.3f (p = %.3g)", resid_interaction_rb$estimate[resid_interaction_rb$variable == "(Intercept)"], resid_interaction_rb$p[resid_interaction_rb$variable == "(Intercept)"]),
  sprintf("ESI: %.3f (p = %.3g)", resid_interaction_rb$estimate[resid_interaction_rb$variable == "esi"], resid_interaction_rb$p[resid_interaction_rb$variable == "esi"]),
  sprintf("Income, 10k SEK: %.3f (p = %.3g)", resid_interaction_rb$estimate[resid_interaction_rb$variable == "income_10k_centered"], resid_interaction_rb$p[resid_interaction_rb$variable == "income_10k_centered"]),
  sprintf("ESI x income: %.3f (p = %.3g)", resid_interaction_rb$estimate[resid_interaction_rb$variable == "esi:income_10k_centered"], resid_interaction_rb$p[resid_interaction_rb$variable == "esi:income_10k_centered"])
)

writeLines(
  resid_model_text,
  con = file.path("results", "income_esi_intensity_residual_model.txt")
)

resid_model_panel <- ggplot() +
  annotate(
    "text",
    x = 0,
    y = 1,
    label = paste(resid_model_text, collapse = "\n"),
    hjust = 0,
    vjust = 1,
    family = "mono",
    size = 3.5
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    panel.border = element_rect(colour = "grey75", fill = NA, linewidth = 0.4)
  )

ggsave(
  file.path("results", "income_esi_intensity_residual_with_model.png"),
  plot = residual_intensity_plots$filled + resid_model_panel + plot_layout(widths = c(2.0, 1.45)),
  width = 13.1,
  height = 5.8,
  dpi = 300,
  bg = "white"
)

write.csv(analysis_data_raw, file.path("results", "income_esi_contour_microdata.csv"), row.names = FALSE)
write.csv(footprint_data, file.path("results", "income_esi_contour_microdata_trimmed.csv"), row.names = FALSE)
write.csv(footprint_surface$grid, file.path("results", "income_esi_contour_surface.csv"), row.names = FALSE)
write.csv(intensity_data, file.path("results", "income_esi_intensity_microdata_trimmed.csv"), row.names = FALSE)
write.csv(intensity_surface$grid, file.path("results", "income_esi_intensity_surface.csv"), row.names = FALSE)
write.csv(residual_intensity_data, file.path("results", "income_esi_intensity_residual_microdata_trimmed.csv"), row.names = FALSE)
write.csv(residual_intensity_surface$grid, file.path("results", "income_esi_intensity_residual_surface.csv"), row.names = FALSE)

category_shares <- selected_spending |>
  filter(!(category %in% non_cost_categories)) |>
  group_by(aid) |>
  summarise(
    total_cost_kr = sum(kr, na.rm = TRUE),
    aviation_kr = sum(kr[broad_category == "Aviation_LDT_kr"], na.rm = TRUE),
    car_public_kr = sum(kr[broad_category == "Car_Public_kr"], na.rm = TRUE),
    groceries_kr = sum(kr[category == "groceries.kr"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    aviation_share = if_else(total_cost_kr > 0, aviation_kr / total_cost_kr, NA_real_),
    car_public_share = if_else(total_cost_kr > 0, car_public_kr / total_cost_kr, NA_real_),
    groceries_share = if_else(total_cost_kr > 0, groceries_kr / total_cost_kr, NA_real_)
  )

share_data_base <- analysis_data_raw |>
  select(aid, esi, income_ksek) |>
  left_join(category_shares, by = "aid") |>
  left_join(
    target_data_local |>
      select(aid, all_of(partial_covars)),
    by = "aid"
  )

share_specs <- list(
  list(var = "aviation_share", label = "Aviation share", file_stub = "income_esi_share_aviation_residual"),
  list(var = "car_public_share", label = "Car/Public transport share", file_stub = "income_esi_share_car_public_residual"),
  list(var = "groceries_share", label = "Groceries share", file_stub = "income_esi_share_groceries_residual")
)

share_model_rows <- list()
share_plot_list <- list()

for (spec in share_specs) {
  outcome <- spec$var

  share_data <- share_data_base |>
    filter(is.finite(.data[[outcome]])) |>
    filter(stats::complete.cases(across(all_of(partial_covars))))

  share_formula <- stats::as.formula(
    paste(outcome, "~", paste(partial_covars, collapse = " + "))
  )
  share_resid_model <- stats::lm(share_formula, data = share_data)

  share_data <- share_data |>
    mutate(share_resid = as.numeric(stats::residuals(share_resid_model)))

  share_data_trimmed <- trim_surface_data(share_data, "share_resid")
  share_surface <- build_surface_grid(share_data_trimmed, "share_resid")
  share_plots <- make_surface_plots(
    data = share_data_trimmed,
    grid = share_surface$grid,
    title = paste0("Residual ", spec$label, " (controls and lifestyles removed)"),
    fill_name = "Residual share",
    file_stub = spec$file_stub,
    line_colour = "#2A6F97"
  )

  share_plot_list[[spec$label]] <- share_plots$filled

  share_model_data <- share_data_trimmed |>
    mutate(income_10k_centered = (income_ksek - mean(income_ksek, na.rm = TRUE)) / 10)

  share_rb <- fit_robust(
    share_resid ~ esi * income_10k_centered,
    data = share_model_data,
    glance = TRUE
  )

  share_model_rows[[length(share_model_rows) + 1L]] <- share_rb |>
    transmute(
      outcome = spec$label,
      term = variable,
      estimate = estimate,
      std_error = stderr,
      p_value = p,
      r_squared = r.squared,
      adj_r_squared = adj.r.squared,
      n = model_n
    )

  write.csv(
    share_data_trimmed,
    file.path("results", paste0(spec$file_stub, "_microdata_trimmed.csv")),
    row.names = FALSE
  )
  write.csv(
    share_surface$grid,
    file.path("results", paste0(spec$file_stub, "_surface.csv")),
    row.names = FALSE
  )
}

share_model_summary <- bind_rows(share_model_rows) |>
  mutate(
    estimate = round(estimate, 4),
    std_error = round(std_error, 4),
    p_value = signif(p_value, 3),
    r_squared = round(r_squared, 3),
    adj_r_squared = round(adj_r_squared, 3)
  )

write.csv(
  share_model_summary,
  file.path("results", "income_esi_share_residual_models.csv"),
  row.names = FALSE
)

share_panel <- (
  share_plot_list[["Aviation share"]] +
  share_plot_list[["Car/Public transport share"]] +
  share_plot_list[["Groceries share"]]
) + plot_layout(ncol = 3, guides = "collect")

ggsave(
  file.path("results", "income_esi_share_residual_filled_panel.png"),
  plot = share_panel,
  width = 16.0,
  height = 5.6,
  dpi = 300,
  bg = "white"
)

cat("Saved: results/income_esi_contour_lines.png\n")
cat("Saved: results/income_esi_contour_filled.png\n")
cat("Saved: results/income_esi_contour_comparison.png\n")
cat("Saved: results/income_esi_contour_microdata.csv\n")
cat("Saved: results/income_esi_contour_microdata_trimmed.csv\n")
cat("Saved: results/income_esi_contour_surface.csv\n")
cat("Saved: results/income_esi_intensity_lines.png\n")
cat("Saved: results/income_esi_intensity_filled.png\n")
cat("Saved: results/income_esi_intensity_comparison.png\n")
cat("Saved: results/income_esi_intensity_with_model.png\n")
cat("Saved: results/income_esi_intensity_microdata_trimmed.csv\n")
cat("Saved: results/income_esi_intensity_surface.csv\n")
cat("Saved: results/income_esi_intensity_model.csv\n")
cat("Saved: results/income_esi_intensity_model.txt\n")
cat("Saved: results/income_esi_intensity_residual_lines.png\n")
cat("Saved: results/income_esi_intensity_residual_filled.png\n")
cat("Saved: results/income_esi_intensity_residual_comparison.png\n")
cat("Saved: results/income_esi_intensity_residual_with_model.png\n")
cat("Saved: results/income_esi_intensity_residual_microdata_trimmed.csv\n")
cat("Saved: results/income_esi_intensity_residual_surface.csv\n")
cat("Saved: results/income_esi_intensity_residual_model.csv\n")
cat("Saved: results/income_esi_intensity_residual_model.txt\n")
cat("Saved: results/income_esi_share_aviation_residual_*.png/csv\n")
cat("Saved: results/income_esi_share_car_public_residual_*.png/csv\n")
cat("Saved: results/income_esi_share_groceries_residual_*.png/csv\n")
cat("Saved: results/income_esi_share_residual_filled_panel.png\n")
cat("Saved: results/income_esi_share_residual_models.csv\n")