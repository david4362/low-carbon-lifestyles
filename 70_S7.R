###############################################################################
# S7.R — broad-category waterfall decomposition for CO2e and SEK.
# Outputs: <output>/S7 detailed co2e.png, <output>/S7 detailed kr.png
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(readr)
  library(ggplot2); library(gridExtra)
})

# Simple ggplot waterfall (used here and in waterfall.R fallback path).
make_waterfall <- function(values, labels, title = "") {
  df <- tibble(label = labels, value = values,
               end   = cumsum(value)) |>
        mutate(start = lag(end, default = 0),
               y_pos = (start + end) / 2,
               fill_color = if_else(value > 0, "positive", "negative"))
  df$fill_color[nrow(df)] <- "total"
  ggplot(df, aes(x = reorder(label, seq_len(nrow(df))))) +
    geom_col(aes(y = value, fill = fill_color), alpha = 0.8) +
    geom_text(aes(y = y_pos, label = paste0(round(value))), vjust = 0.5, size = 3) +
    scale_fill_manual(values = c(positive = "#2E7D32",
                                 negative = "#C62828",
                                 total    = "#1565C0"), guide = "none") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Category", y = "Value", title = title)
}

# Local label additions (S7-specific terms not in plot_labels).
s7_labels <- c(plot_labels,
               list("no_carTRUE:esi" = "Car-free",
                    "no_flyingTRUE:esi" = "Flight-free"))

# Stack the regression CSV into long form ready for plotting.
prep_decomposition <- function(reg_df, broad_cats) {
  reg_df |>
    rename(category = part) |>
    filter(category %in% broad_cats, str_detect(variable, ":")) |>
    pivot_longer(c(lh_high, lh_low, est_high, est_low),
                 names_to = c(".value", "esi"), names_sep = "_") |>
    mutate(est = est * 12, label = broad_labels[category])
}

# Build the panel of per-(lifestyle × interaction × esi) waterfalls.
make_panel <- function(decomp_df) {
  pls <- list()
  for (lsv in unique(decomp_df$lifestyle_variable))
    for (lsp in unique(decomp_df$variable))
      for (esi_val in unique(decomp_df$esi)) {
        wf <- decomp_df |>
          filter(lifestyle_variable == lsv, variable == lsp, esi == esi_val) |>
          arrange(desc(abs(est)))
        if (nrow(wf) == 0) next
        pls[[length(pls) + 1L]] <- make_waterfall(
          values = round(wf$est), labels = wf$label,
          title  = paste(c(s7_labels[[lsp]], s7_labels[[esi_val]]), collapse = " "))
      }
  pls
}

write_panel <- function(pls, path) {
  pl <- do.call(grid.arrange, c(pls, ncol = 1))
  ggsave(path, pl, units = "cm", width = 22, height = length(pls) * 8)
}

categories  <- read.csv2("categories.csv")
output_co2e <- read_csv(file.path(output, "category regression co2e.csv"), show_col_types = FALSE)
output_kr   <- read_csv(file.path(output, "category regression kr.csv"),   show_col_types = FALSE)

write_panel(make_panel(prep_decomposition(output_co2e, broad_cats_co2e)),
            file.path(output, "S7 detailed co2e.png"))
write_panel(make_panel(prep_decomposition(output_kr,   broad_cats_kr)),
            file.path(output, "S7 detailed kr.png"))
