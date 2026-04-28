###############################################################################
# deferred_consumption_tests.R — extended-window robustness for the no-rebound
# finding. Outputs:
#   <output>/deferred_consumption_extended_window.csv
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
  library(sandwich); library(lmtest); library(broom)
})

if (!exists("selected_aids") || !exists("monthly_emissions"))
  stop("Run this script after loading data and sourcing stat_vars.R")

cat("\n========== DEFERRED CONSUMPTION ROBUSTNESS TESTS ==========\n\n")

# --- Setup: cc exclusion (mirrors master_analysis.R) ---------------------
cc_excluded   <- compute_cc_excluded_aids(monthly_spending, selected_aids, selected_months)
analysis_aids <- setdiff(selected_aids, cc_excluded)
cat(sprintf("Analysis sample: N = %d (after cc exclusion of %d)\n",
            length(analysis_aids), length(cc_excluded)))

ctrl_vars <- ctrl_var_names(control_data)
sig <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .1, ".", ""))))

# --- Person-level annualised emissions (with P99 caps) -------------------
build_annualised <- function(em_data, month_counts) {
  cap_targets <- intersect(
    c(grep("_other\\.co2e$", unique(em_data$category), value = TRUE),
      p99_target_suffix_co2e),
    unique(em_data$category)
  )
  capped <- winsorize_p99(em_data, "co2e", cap_targets)$data
  agg    <- aggregate_person_co2e(capped) |> rename_with(~ paste0(.x, "_raw"), -aid)

  agg |>
    left_join(month_counts, by = "aid") |>
    mutate(across(ends_with("_raw"), ~ .x / n_months * 12,
                  .names = "{sub('_raw$','', .col)}")) |>
    select(-ends_with("_raw")) |>
    left_join(target_data, by = "aid")
}

run_models <- function(dat, label) {
  cat(sprintf("\n--- %s (N=%d) ---\n", label, nrow(dat)))
  rows <- list()
  for (ls in LIFESTYLES) {
    for (tp in c("total","direct","indirect")) {
      dv <- if (tp == "total") "total" else paste0(tp, "_", ls)
      rb <- fit_robust(make_interaction_formula(dv, ls, ctrl_vars), data = dat)
      get_row <- function(v) {
        i <- match(v, rb$variable)
        c(rb$estimate[i], rb$stderr[i], rb$t[i], rb$p[i])
      }
      ls_term <- paste0(ls, "TRUE")
      ix_row  <- paste0("esi:", ls, "TRUE")
      l  <- get_row(ls_term)
      e  <- get_row("esi")
      ix <- get_row(ix_row)
      cat(sprintf("  %s_%s: %s=%6.0f(p=%.3f%s) esi=%6.0f(p=%.3f%s) esi×%s=%6.0f(p=%.3f%s)\n",
          ls, tp, ls, l[1], l[4], sig(l[4]), e[1], e[4], sig(e[4]),
          ls, ix[1], ix[4], sig(ix[4])))
      rows[[length(rows) + 1L]] <- tibble(
        label = label, lifestyle = ls, outcome = tp,
        variable = c(ls_term, "esi", ix_row),
        estimate = c(l[1], e[1], ix[1]),
        se       = c(l[2], e[2], ix[2]),
        p        = c(l[4], e[4], ix[4]),
        n        = nrow(dat)
      )
    }
  }
  bind_rows(rows)
}

# A: 12-month window (annualised, for comparability)
em_12m     <- monthly_emissions |> filter(aid %in% analysis_aids) |>
                semi_join(selected_months, by = c("aid","date"))
months_12m <- selected_months   |> filter(aid %in% analysis_aids) |>
                count(aid, name = "n_months")
dat_12m    <- build_annualised(em_12m, months_12m)
res_12m    <- run_models(dat_12m, "12-month window (annualised)")

# B: Extended window — all post-COVID months
em_ext     <- monthly_emissions |>
                filter(aid %in% analysis_aids, date >= as.Date("2021-03-01"))
months_ext <- em_ext |> distinct(aid, date) |> count(aid, name = "n_months")
dat_ext    <- build_annualised(em_ext, months_ext)
res_ext    <- run_models(dat_ext,
                sprintf("Extended window (all post-COVID, mean=%.0f months)",
      mean(months_ext$n_months, na.rm = TRUE)))

# C: Extended window with redefined no_flying indicator
no_flying_ext <- em_ext |> group_by(aid) |>
  summarise(no_flying = sum(co2e[category == "aviation.co2e"], na.rm = TRUE) == 0,
            .groups = "drop")
dat_ext_redef <- dat_ext |> select(-no_flying) |> left_join(no_flying_ext, by = "aid")

n_nofly_12m <- sum(dat_ext$no_flying,       na.rm = TRUE)
n_nofly_ext <- sum(dat_ext_redef$no_flying, na.rm = TRUE)
cat(sprintf("\nNo-flying group: 12m=%d (%.1f%%), extended=%d (%.1f%%)\n",
            n_nofly_12m, 100*n_nofly_12m/nrow(dat_ext),
            n_nofly_ext, 100*n_nofly_ext/nrow(dat_ext_redef)))

res_ext_redef <- run_models(dat_ext_redef,
                  sprintf("Extended window (redefined no-flying, mean=%.0f months)",
        mean(months_ext$n_months, na.rm = TRUE)))

cat("\n\n--- COMPARISON: 12m vs Extended window (indirect effects) ---\n")
comp <- bind_rows(res_12m, res_ext, res_ext_redef) |>
  filter(grepl("indirect", outcome), !grepl("^esi$", variable)) |>
  select(label, lifestyle, variable, estimate, p) |>
  arrange(lifestyle, variable, label)
print(as.data.frame(comp), row.names = FALSE)

out_dir <- if (exists("output") && nzchar(output)) output else "output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
res_all <- bind_rows(res_12m, res_ext, res_ext_redef)
write.csv(res_all,
          file.path(out_dir, "deferred_consumption_extended_window.csv"),
          row.names = FALSE)
cat(sprintf("\nResults saved to:\n  %s\n",
            file.path(out_dir, "deferred_consumption_extended_window.csv")))
cat("\n========== END DEFERRED CONSUMPTION TESTS ==========\n")
