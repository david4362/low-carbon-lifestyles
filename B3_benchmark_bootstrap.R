# B3_benchmark_bootstrap.R
# Propagate sampling uncertainty of the proportional re-spending benchmark
# B_j = dS_j * iota_{-j} into the rebound-exclusion test, by bootstrapping
# households and recomputing dS_j, iota_{-j} and the observed indirect
# coefficient jointly on each resample. Mirrors master_analysis.R Step 7.
#
# Run locally on mock data:  Rscript B3_benchmark_bootstrap.R
suppressPackageStartupMessages({ library(dplyr) })

set.seed(20260929)
options(keep.source = TRUE)

# --- Setup: build the standard downstream objects from the mock RData --------
source("00_constants.R")
source("01_utils.R")
source("10_load_data.R")
source("20_filter_data.R")
source("30_stat_vars.R")

ctrl_vars <- setdiff(names(control_data), "aid")

# --- Person-level benchmark inputs (focal vs non-focal, per lifestyle) --------
focal_cfg <- list(
  no_car    = list(broad = "Car_Public",   cat = NA),
  no_flying = list(broad = "Aviation_LDT", cat = NA),
  no_meat   = list(broad = NA,             cat = "groceries")
)

kr <- selected_spending  |> filter(!category %in% non_purchase_categories)
em <- selected_emissions |> filter(!category %in% non_purchase_categories)

is_focal <- function(df, cfg, unit) {
  if (!is.na(cfg$broad)) df$broad_category == paste0(cfg$broad, "_", unit)
  else df$category == paste0(cfg$cat, ".", unit)
}

person <- target_data |>
  select(aid, no_car, no_flying, no_meat, esi) |>
  left_join(control_data, by = "aid")

for (lf in names(focal_cfg)) {
  cfg <- focal_cfg[[lf]]
  s <- sub("no_", "", lf)
  kr$foc <- is_focal(kr, cfg, "kr")
  em$foc <- is_focal(em, cfg, "co2e")
  fk <- kr |> group_by(aid) |>
    summarise(!!paste0("focal_kr_", s)    := sum(kr[foc]),
              !!paste0("indir_kr_", s)    := sum(kr[!foc]), .groups = "drop")
  fe <- em |> group_by(aid) |>
    summarise(!!paste0("indir_co2e_", s)  := sum(co2e[!foc]), .groups = "drop")
  person <- person |> left_join(fk, by = "aid") |> left_join(fe, by = "aid")
}
person <- person |> filter(if_all(everything(), ~ !is.na(.)))
cat(sprintf("Analytical N for bootstrap: %d\n\n", nrow(person)))

# --- One benchmark computation on a data frame -------------------------------
rhs <- paste(c(ctrl_vars, "esi"), collapse = " + ")
one_gap <- function(df, lf) {
  s <- sub("no_", "", lf)
  # dS_j: additive coef of lifestyle on focal-domain spending, sign-flipped
  fk_form <- as.formula(sprintf("focal_kr_%s ~ %s + %s", s, lf, rhs))
  dS <- -coef(lm(fk_form, df))[[paste0(lf, "TRUE")]]
  # iota_{-j}: comparison-group non-focal emission intensity
  na <- df[!df[[lf]], ]
  iota <- sum(na[[paste0("indir_co2e_", s)]]) / sum(na[[paste0("indir_kr_", s)]])
  # observed indirect coef
  ic_form <- as.formula(sprintf("indir_co2e_%s ~ %s + %s", s, lf, rhs))
  obs <- coef(lm(ic_form, df))[[paste0(lf, "TRUE")]]
  c(dS = dS, iota = iota, benchmark = dS * iota, observed = obs,
    gap = obs - dS * iota)
}

# --- Point estimates ---------------------------------------------------------
LF <- c("no_car", "no_flying", "no_meat")
point <- t(sapply(LF, function(lf) one_gap(person, lf)))
cat("Point estimates (kg CO2e/year):\n"); print(round(point, 1)); cat("\n")

# --- Bootstrap ---------------------------------------------------------------
B <- 2000
aids <- person$aid
boot <- setNames(vector("list", length(LF)), LF)
for (lf in LF) boot[[lf]] <- matrix(NA_real_, B, 5,
                                    dimnames = list(NULL, c("dS","iota","benchmark","observed","gap")))
for (b in seq_len(B)) {
  idx <- sample(nrow(person), replace = TRUE)
  db  <- person[idx, ]
  for (lf in LF) boot[[lf]][b, ] <- tryCatch(one_gap(db, lf),
                                             error = function(e) rep(NA_real_, 5))
}

cat("Propagated benchmark uncertainty (2,000 household bootstrap):\n\n")
summ <- lapply(LF, function(lf) {
  g   <- boot[[lf]][, "gap"];        g <- g[is.finite(g)]
  bmk <- boot[[lf]][, "benchmark"];  bmk <- bmk[is.finite(bmk)]
  obs <- boot[[lf]][, "observed"];   obs <- obs[is.finite(obs)]
  data.frame(
    lifestyle       = lf,
    benchmark_kg    = round(point[lf, "benchmark"], 1),
    benchmark_se    = round(sd(bmk), 1),
    observed_kg     = round(point[lf, "observed"], 1),
    observed_se     = round(sd(obs), 1),
    gap_kg          = round(point[lf, "gap"], 1),
    gap_se          = round(sd(g), 1),
    gap_ci_lo       = round(quantile(g, 0.05), 1),
    gap_ci_hi       = round(quantile(g, 0.95), 1),
    # one-sided propagated p: P(gap >= 0) = P(observed indirect not below benchmark)
    p_exclude       = round(mean(g >= 0), 4)
  )
})
summ <- do.call(rbind, summ)
print(summ, row.names = FALSE)
write.csv(summ, file.path(output, "benchmark_bootstrap.csv"), row.names = FALSE)
cat(sprintf("\nSaved: %s/benchmark_bootstrap.csv\n", output))
