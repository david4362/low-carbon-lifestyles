# Interaction plot: adopter vs non-adopter lines across continuous ESI
# For total, direct, and indirect emissions × 3 lifestyles
#
# Two modes:
#   1. Microdata available (TRE / pipeline) → fits models, computes HC3 CIs,
#      saves prediction data to interaction_plot_predictions.csv, then plots.
#   2. No microdata but CSV exists → reads saved predictions and plots.

library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(stringr)

pred_csv_path <- file.path("results", "interaction_plot_predictions.csv")
has_microdata <- exists("selected_emissions") && exists("target_data")

# Try loading from RData if not already in memory
if (!has_microdata) {
  rdata_path <- if (file.exists("R/default_filter.RData")) "R/default_filter.RData" else "default_filter.RData"
  if (file.exists(rdata_path)) {
    load(rdata_path)
    stat_vars_path <- if (file.exists("R/30_stat_vars.R")) "R/30_stat_vars.R" else "30_stat_vars.R"
    source(stat_vars_path)
    has_microdata <- exists("selected_emissions") && exists("target_data")
  }
}

if (has_microdata) {
  library(sandwich)
  library(lmtest)
  if (!exists("fit_robust")) {
    utils_path <- if (file.exists("R/01_utils.R")) "R/01_utils.R" else "01_utils.R"
    if (file.exists(utils_path)) source(utils_path)
  }

  # Build lm_data (same as interactions.R)
  lm_data <- selected_emissions |>
    group_by(aid) |>
    summarise(
      total = sum(co2e),
      direct_no_car = sum(co2e[broad_category == "Car_Public_co2e"]),
      indirect_no_car = sum(co2e[broad_category != "Car_Public_co2e"]),
      direct_no_flying = sum(co2e[broad_category == "Aviation_LDT_co2e"]),
      indirect_no_flying = sum(co2e[broad_category != "Aviation_LDT_co2e"]),
      direct_no_meat = sum(co2e[category == "groceries.co2e"]),
      indirect_no_meat = sum(co2e[category != "groceries.co2e"])
    ) |>
    left_join(target_data, by = "aid")

  lm_factors <- names(control_data)[names(control_data) != "aid"]

  no_car <- append("esi * no_car", lm_factors)
  no_flying <- append("esi * no_flying", lm_factors)
  no_meat <- append("esi * no_meat", lm_factors)

  # --- Fit 9 models and extract predictions with proper HC3 CIs ---
  model_specs <- list(
    list(dv = "total",             lifestyle = "no_car",    label_ls = "Car",    label_out = "Total"),
    list(dv = "direct_no_car",     lifestyle = "no_car",    label_ls = "Car",    label_out = "Direct"),
    list(dv = "indirect_no_car",   lifestyle = "no_car",    label_ls = "Car",    label_out = "Indirect"),
    list(dv = "total",             lifestyle = "no_flying", label_ls = "Flying", label_out = "Total"),
    list(dv = "direct_no_flying",  lifestyle = "no_flying", label_ls = "Flying", label_out = "Direct"),
    list(dv = "indirect_no_flying",lifestyle = "no_flying", label_ls = "Flying", label_out = "Indirect"),
    list(dv = "total",             lifestyle = "no_meat",   label_ls = "Meat",   label_out = "Total"),
    list(dv = "direct_no_meat",    lifestyle = "no_meat",   label_ls = "Meat",   label_out = "Direct"),
    list(dv = "indirect_no_meat",  lifestyle = "no_meat",   label_ls = "Meat",   label_out = "Indirect")
  )

  esi_range <- seq(-2, 2, length.out = 100)

  pred_all <- list()
  for (spec in model_specs) {
    vars_string <- paste(get(spec$lifestyle), collapse = "+")
    form <- as.formula(sprintf("%s ~ %s", spec$dv, vars_string))
    rb  <- fit_robust(form, data = lm_data)
    mod <- attr(rb, "model")
    V   <- attr(rb, "vcov")

    # Identify key column names
    ls_var <- paste0(spec$lifestyle, "TRUE")
    coef_names <- names(coef(mod))
    int_var <- coef_names[grepl("esi", coef_names) & grepl(":", coef_names) & grepl(ls_var, coef_names)]
    if (length(int_var) != 1) stop("Could not find unique interaction term for ", spec$lifestyle)

    # Drop aliased (NA) coefficients; model matrix columns match estimable params
    beta_all <- coef(mod)
    keep <- !is.na(beta_all)
    beta <- beta_all[keep]

    # Build "typical" x-vector: model matrix column means (controls at sample averages)
    X_full <- model.matrix(mod)
    x_mean <- colMeans(X_full[, keep, drop = FALSE])

    # Predict absolute emissions for each group across ESI range
    for (group in c("Non-adopter", "Low-carbon lifestyle adopter")) {
      y_vec <- numeric(length(esi_range))
      se_vec <- numeric(length(esi_range))

      for (i in seq_along(esi_range)) {
        x <- x_mean
        x["esi"] <- esi_range[i]
        if (group == "Non-adopter") {
          x[ls_var] <- 0
          x[int_var] <- 0
        } else {
          x[ls_var] <- 1
          x[int_var] <- esi_range[i]
        }
        y_vec[i] <- sum(x * beta)
        se_vec[i] <- sqrt(as.numeric(t(x) %*% V %*% x))
      }

      pred_all[[length(pred_all) + 1]] <- data.frame(
        lifestyle = spec$label_ls,
        outcome = spec$label_out,
        group = group,
        esi = esi_range,
        pred_t = y_vec / 1000,
        ci_lo_t = (y_vec - 1.96 * se_vec) / 1000,
        ci_hi_t = (y_vec + 1.96 * se_vec) / 1000,
        stringsAsFactors = FALSE
      )
    }
  }
  pred_df <- bind_rows(pred_all)

  # Save prediction data so the plot can be regenerated without microdata
  write.csv(pred_df, pred_csv_path, row.names = FALSE)
  cat("Saved prediction data:", pred_csv_path, "\n")

} else if (file.exists(pred_csv_path)) {
  cat("No microdata available; reading saved predictions from", pred_csv_path, "\n")
  pred_df <- read.csv(pred_csv_path, stringsAsFactors = FALSE)
} else {
  stop("No microdata and no saved prediction CSV (", pred_csv_path, "). Run in TRE first.")
}

# Factor ordering
pred_df$lifestyle <- factor(pred_df$lifestyle, levels = c("Car", "Flying", "Meat"))
pred_df$outcome   <- factor(pred_df$outcome,   levels = c("Total", "Direct", "Indirect"))
pred_df$group     <- factor(pred_df$group,     levels = c("Low-carbon lifestyle adopter", "Non-adopter"))

# --- Plot ---
p <- ggplot(pred_df, aes(x = esi, y = pred_t, colour = group, fill = group)) +
  geom_ribbon(aes(ymin = ci_lo_t, ymax = ci_hi_t), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.8) +
  facet_grid(outcome ~ lifestyle, scales = "free_y") +
  scale_colour_manual(values = c("Low-carbon lifestyle adopter" = "turquoise3", "Non-adopter" = "orange2"), name = NULL) +
  scale_fill_manual(values = c("Low-carbon lifestyle adopter" = "turquoise3", "Non-adopter" = "orange2"), name = NULL) +
  labs(
    x = "Environmental self-identity (standardised)",
    y = expression("Predicted CO"[2]*"e emissions (tCO"[2]*"e/year)")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    panel.border = element_rect(colour = "grey70", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.8, "lines")
  )

ggsave("results/interaction_plot.png",
       plot = p, width = 10, height = 8, dpi = 300, bg = "white")

cat("Saved: results/interaction_plot.png\n")
