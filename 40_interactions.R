###############################################################################
# interactions.R — fit ESI × lifestyle interaction models and write
#   "interaction regressions {co2e,kr}.{csv,txt}" + per-model PNGs.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(jtools); library(interactions)
  library(stargazer); library(broom); library(sandwich); library(lmtest)
})

# Person-level outcomes joined to controls/ESI/lifestyle indicators.
lm_data    <- aggregate_person_co2e(selected_emissions) |> left_join(target_data, by = "aid")
lm_data_kr <- aggregate_person_kr(selected_spending,  drop_non_cost = TRUE) |>
              left_join(target_data, by = "aid")

ctrl_vars <- ctrl_var_names(control_data)

# --- Interaction plot helper ----------------------------------------------
m       <- mean(lm_data$esi, na.rm = TRUE)
s       <- sd(  lm_data$esi, na.rm = TRUE)
min_esi <- min( lm_data$esi, na.rm = TRUE)
max_esi <- max( lm_data$esi, na.rm = TRUE)

inter_plot <- function(lifestyle, type, model, value) {
  args <- list(model = model, pred = quote(esi), modx = as.name(lifestyle),
               xvals = c(m - s, m, m + s),
               x.label = "ESI score (-1 SD, m, +1 SD)",
               y.label = if (value == "co2e") "CO2e emissions" else "Kr spent",
               legend.main = plot_labels[[lifestyle]],
               main.title  = plot_labels[[type]],
               x.lim = c(min_esi, max_esi))
  p <- tryCatch(do.call(interact_plot, c(args, list(robust = "HC3", interval = TRUE))),
                error = function(e) {
                  message(sprintf("interact_plot fallback for %s/%s: %s",
                                  lifestyle, type, conditionMessage(e)))
                  do.call(interact_plot, c(args, list(interval = FALSE)))
                })
  p + theme_apa() +
    scale_x_continuous(limits = c(min_esi, max_esi)) +
    theme(legend.position = "bottom",
          plot.title  = element_text(face = "bold", size = 18),
          axis.title  = element_text(size = 18),
          axis.text   = element_text(size = 16),
          legend.text = element_text(size = 16),
          legend.title= element_text(size = 16)) +
    scale_color_brewer(palette = "Dark2") +
    scale_linetype_manual(values = c("solid","dashed","dotted"))
}

# --- Fit + plot all 9 models per outcome ---------------------------------
.run_and_plot <- function(person_df, value) {
  res <- run_interaction_models(person_df, ctrl_vars, value = value)
  for (i in seq_along(res$models)) {
    label    <- names(res$models)[i]
    parts    <- strsplit(label, " ")[[1]]
    type_lc  <- tolower(parts[[1]])                  # total / direct / indirect
    lifestyle <- LIFESTYLES[ceiling(i / 3)]
    p <- inter_plot(lifestyle, type_lc, res$models[[i]], value)
    fn <- file.path(output, sprintf("%s %s %s.png", type_lc, lifestyle, value))
    ggsave(fn, p, create.dir = TRUE)
  }
  res
}

co2e_res <- .run_and_plot(lm_data,    "co2e")
kr_res   <- .run_and_plot(lm_data_kr, "kr")

lm_models     <- co2e_res$models
lm_models_kr  <- kr_res$models
lm_models_df  <- co2e_res$df
lm_models_kr_df <- kr_res$df

# --- Stargazer + CSV outputs ---------------------------------------------
.write_outputs <- function(models, df, value) {
  txt_path <- file.path(output, sprintf("interaction regressions %s.txt", value))
  csv_path <- file.path(output, sprintf("interaction regressions %s.csv", value))
  ses <- lapply(models, function(m) sqrt(diag(vcovHC(m, type = "HC3"))))
  tryCatch(
    stargazer(models, se = ses, out = txt_path),
    error = function(e) {
      message(sprintf("stargazer text export fallback for %s: %s",
                      value, conditionMessage(e)))
    }
  )
  if (!file.exists(txt_path) || isTRUE(file.info(txt_path)$size == 0)) {
    fallback_txt <- unlist(lapply(seq_along(models), function(i) {
      c(sprintf("==== %s ====", names(models)[i]),
        capture.output(print(summary(models[[i]]))),
        "")
    }))
    writeLines(fallback_txt, txt_path)
  }
  write.csv(df, csv_path)
}
.write_outputs(lm_models,    lm_models_df,    "co2e")
.write_outputs(lm_models_kr, lm_models_kr_df, "kr")
