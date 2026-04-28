###############################################################################
# regressions.R — per-category interaction regressions for both CO2e and SEK.
# Outputs:
#   <output>/category regression co2e.csv
#   <output>/category regression kr.csv
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(sandwich); library(lmtest); library(collapse)
})

# Reload broad-category lookups once (idempotent if already loaded by load_data.R).
if (!exists("broad_cat"))    broad_cat    <- read.csv("broad-category.csv")
if (!exists("broad_cat_kr")) broad_cat_kr <- read.csv("broad-category-kr.csv")

###############################################################################
# Build a list of "data generator" closures: each takes the long emissions/
# spending table and returns it aggregated to person×month for one category
# slice (direct, indirect, total, each broad category, each leaf category, ...).
###############################################################################
.make_data_generators <- function(broad_table, value_col,
                                  cat_field, cat_name,
                                  exclude_pattern,
                                  drop_non_cost = FALSE) {
  v <- value_col
  agg <- function(df) {
    df2 <- df
    df2[["reg_val_tmp"]] <- df2[[v]]
    out <- df2 |> fgroup_by(aid, date) |>
      fsummarise(reg_val_tmp = fsum(reg_val_tmp))
    names(out)[names(out) == "reg_val_tmp"] <- v
    out
  }

  gens <- list()
  if (value_col == "co2e") {
    gens$direct_emissions   <- function(x) x |> filter(.data[[cat_field]] == cat_name) |> agg()
    gens$indirect_emissions <- function(x) x |> filter(.data[[cat_field]] != cat_name) |> agg()
    gens$total_emissions    <- function(x) x |> agg()
  } else {
    gens$direct_costs       <- function(x) x |> filter(.data[[cat_field]] == cat_name) |> agg()
    gens$indirect_costs     <- function(x) x |> filter(.data[[cat_field]] != cat_name,
                                                       !(category %in% non_cost_categories)) |> agg()
    gens$non_purchase_costs <- function(x) x |> filter(category %in% non_cost_categories) |> agg()
    gens$total_costs        <- function(x) x |> agg()
  }

  for (k in unique(broad_table$broad_category))
    if (!grepl(exclude_pattern, k, ignore.case = TRUE))
      gens[[k]] <- local({ kk <- k; function(x) x |> filter(broad_category == kk) |> agg() })

  for (k in broad_table$category)
    if (!grepl(exclude_pattern, k, ignore.case = TRUE))
      gens[[k]] <- local({ kk <- k; function(x) x |> filter(category == kk) |> agg() })

  other_set <- if (value_col == "co2e")
    c("Other_services_co2e","Other_products_co2e","Other_misc_co2e") else
    c("Other_services_kr","Other_products_kr","Other_misc_kr")
  other_label <- if (value_col == "co2e") "Other_total_co2e" else "Other_total_kr"
  gens[[other_label]] <- function(x) x |> filter(broad_category %in% other_set) |> agg()
  gens
}

###############################################################################
# Fit one category regression and return a tibble row(s) with HC3 robust coefs
# plus the lh_low/lh_high/est_low/est_high columns (effect at ±1 SD ESI).
###############################################################################
.fit_category <- function(selected_data, value_col, lifestyle, k, ctrl_vars) {
  participants <- n_distinct(selected_data$aid)
  if (participants <= 1) return(NULL)

  in_group <- if (lifestyle == "no_meat") NA_real_ else
    selected_data |> distinct(aid, .keep_all = TRUE) |> pull(all_of(lifestyle)) |> fsum()

  rhs  <- paste(c(ctrl_vars, sprintf("%s*esi", lifestyle)), collapse = " + ")
  form <- as.formula(sprintf("%s ~ %s", value_col, rhs))
  rb   <- fit_robust(form, selected_data)
  m    <- attr(rb, "model")
  V    <- attr(rb, "vcov")
  cf   <- as.data.frame(rb[, c("estimate", "stderr", "t", "p")])
  rownames(cf) <- rb$variable
  est  <- setNames(rb$estimate, rb$variable)

  # Effect at +/-1 SD of ESI for each interaction term
  lh <- cf[, 1:4] * 0
  names(lh) <- c("lh_low","lh_high","est_low","est_high")
  ints <- grep(":", rownames(V), value = TRUE)
  df_res <- df.residual(m)
  for (int in ints) {
    lsv  <- str_split_i(int, ":", 1)
    vara <- V[lsv, lsv]; varb <- V[int, int]; covab <- V[lsv, int]
    se_p <- sqrt(vara + varb + 2 * covab)
    se_m <- sqrt(vara + varb - 2 * covab)
    est_p <- est[[lsv]] + est[[int]]
    est_m <- est[[lsv]] - est[[int]]
    p_p <- 2 * pt(abs(est_p / se_p), df = df_res, lower.tail = FALSE)
    p_m <- 2 * pt(abs(est_m / se_m), df = df_res, lower.tail = FALSE)
    lh[int, ] <- c(p_m, p_p, est_m, est_p)
  }

  bind_cols(
    tibble(lifestyle_variable = lifestyle, part = k,
           participants = participants, in_group = in_group),
    variable = rb$variable, cf, lh
  )
}

###############################################################################
# Loop over (lifestyle × category) and stack results.
###############################################################################
.run_category_regressions <- function(long_df, broad_table, value_col,
                                      focal_map, exclude_pattern) {
  ctrl_vars <- ctrl_var_names(control_data)
  rows <- list()
  for (ls in names(focal_map)) {
    spec <- focal_map[[ls]]
    gens <- .make_data_generators(broad_table, value_col,
                                  spec$field, spec$value, exclude_pattern)
    for (k in names(gens)) {
      sd <- gens[[k]](long_df) |> left_join(target_data, by = "aid")
      rows[[length(rows) + 1L]] <- .fit_category(sd, value_col, ls, k, ctrl_vars)
    }
  }
  bind_rows(rows)
}

# ---- CO2e ----------------------------------------------------------------
output_co2e <- .run_category_regressions(
  selected_emissions, broad_cat, "co2e",
  lifestyle_focal_co2e, exclude_pattern = "income|exclude"
)
write.csv(output_co2e, file = file.path(output, "category regression co2e.csv"))
gc()

# ---- SEK -----------------------------------------------------------------
output_kr <- .run_category_regressions(
  selected_spending, broad_cat_kr, "kr",
  lifestyle_focal_kr, exclude_pattern = "income|exclude"
)
write.csv(output_kr, file = file.path(output, "category regression kr.csv"))
gc()
