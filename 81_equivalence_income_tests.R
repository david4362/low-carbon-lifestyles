###############################################################################
# equivalence_income_tests.R — two reviewer-requested additions:
#
#   PART A  Equivalence (TOST) tests for the indirect lifestyle effects.
#           Addresses the "absence of evidence is not evidence of absence"
#           critique: instead of reading a non-significant indirect coefficient
#           as "no rebound", we test whether the indirect effect is
#           statistically EQUIVALENT to zero within a pre-specified rebound
#           margin of policy interest. The margin defaults to the proportional
#           re-spending benchmark (Step 7), i.e. the indirect emissions a simple
#           re-spending model predicts. Rejecting the TOST null means the data
#           exclude a rebound at least as large as that benchmark.
#
#   PART B  Income x lifestyle two-way interaction on indirect emissions.
#           Tests whether the (no-)rebound pattern depends on financial slack:
#           do higher-income adopters — who free more money — show more
#           re-spending? A null interaction rebuts the "low-budget artefact"
#           explanation for the headline no-rebound result.
#
# Outputs (written to `output`):
#   equivalence_tests.csv
#   income_lifestyle_interactions.csv
#
# Conventions mirror 40_interactions.R / 80_deferred_consumption_tests.R: this
# script is sourced AFTER 10_load_data.R + 30_stat_vars.R + 40_interactions.R,
# and relies only on the standard downstream objects (selected_emissions,
# target_data, control_data) — so it is agnostic to which data loader
# (TRE eriksson-code copy vs. local fallback) produced them.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(sandwich); library(lmtest); library(broom)
})

if (!exists("selected_emissions") || !exists("target_data") || !exists("control_data"))
  stop("Run 81_equivalence_income_tests.R after 10_load_data.R, 30_stat_vars.R and 40_interactions.R")
if (!exists("output") || is.null(output)) output <- get0("output_dir", ifnotfound = ".")

cat("\n========== EQUIVALENCE (TOST) + INCOME x LIFESTYLE TESTS ==========\n\n")

ctrl_vars <- ctrl_var_names(control_data)
sig <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .1, ".", ""))))

# Person-level emissions joined to controls/ESI/lifestyle indicators — exactly
# the frame the headline additive models in 40_interactions.R are fit on.
person_co2e <- aggregate_person_co2e(selected_emissions) |>
  left_join(target_data, by = "aid")

# ===========================================================================
# PART A — Equivalence tests (two one-sided tests, TOST) for indirect effects
# ===========================================================================
# Margins: by default the proportional re-spending benchmark's predicted
# rebound (kg CO2e/year) for each lifestyle, read from rebound_benchmark.csv
# (Step 7). If that file is missing, fall back to a fixed default margin.
DEFAULT_EQUIV_MARGIN_KG <- 250  # ~0.25 tCO2e/year, used only if benchmark absent

benchmark_path <- file.path(output, "rebound_benchmark.csv")
margin_lookup <- NULL
if (file.exists(benchmark_path)) {
  margin_lookup <- read.csv(benchmark_path) |>
    transmute(lifestyle = Lifestyle, margin_kg = abs(Predicted_rebound_kg))
  cat("Equivalence margins from rebound_benchmark.csv (proportional re-spending):\n")
  print(margin_lookup)
} else {
  cat(sprintf("rebound_benchmark.csv not found; using default margin +/-%d kg for all lifestyles.\n",
              DEFAULT_EQUIV_MARGIN_KG))
}
cat("\n")

# TOST for H0: |beta| >= margin  vs  H1: |beta| < margin (equivalence to zero
# within +/- margin), plus the one-sided "rebound-exclusion" test
# H0: beta >= +margin vs H1: beta < +margin (can we rule out rebound this big?).
.tost_one <- function(est, se, df, margin) {
  # Two one-sided tests
  t_lower <- (est + margin) / se                       # H0: beta <= -margin
  p_lower <- pt(t_lower, df, lower.tail = FALSE)
  t_upper <- (est - margin) / se                       # H0: beta >= +margin
  p_upper <- pt(t_upper, df, lower.tail = TRUE)
  p_tost  <- max(p_lower, p_upper)                      # equivalence p-value
  tcrit90 <- qt(0.95, df)                               # 90% CI = 1 - 2*alpha, alpha=.05
  tibble(
    estimate_kg    = est,
    se_kg          = se,
    df             = df,
    margin_kg      = margin,
    ci90_low_kg    = est - tcrit90 * se,
    ci90_high_kg   = est + tcrit90 * se,
    p_tost         = p_tost,                            # < .05 => equivalent within +/-margin
    equivalent     = p_tost < 0.05,
    p_exclude_rebound = p_upper                         # < .05 => exclude rebound >= +margin
  )
}

equiv_rows <- list()
cat("--- Part A: Equivalence (TOST) on indirect emissions (headline additive model) ---\n")
for (ls in LIFESTYLES) {
  dv  <- paste0("indirect_", ls)
  rb  <- fit_robust(make_additive_formula(dv, ls, ctrl_vars), person_co2e, glance = TRUE)
  m   <- attr(rb, "model")
  trm <- paste0(ls, "TRUE")
  i   <- match(trm, rb$variable)
  est <- rb$estimate[i]; se <- rb$stderr[i]; df <- df.residual(m)
  margin <- if (!is.null(margin_lookup) && ls %in% margin_lookup$lifestyle)
    margin_lookup$margin_kg[margin_lookup$lifestyle == ls] else DEFAULT_EQUIV_MARGIN_KG
  if (length(margin) != 1 || is.na(margin) || margin <= 0) margin <- DEFAULT_EQUIV_MARGIN_KG
  res <- .tost_one(est, se, df, margin) |> mutate(lifestyle = ls, .before = 1)
  equiv_rows[[length(equiv_rows) + 1L]] <- res
  cat(sprintf("  %-10s indirect = %7.1f kg (90%% CI %7.1f, %7.1f) | margin +/-%5.0f | TOST p=%.3f%s | exclude-rebound p=%.3f%s\n",
              ls, est, res$ci90_low_kg, res$ci90_high_kg, margin,
              res$p_tost, sig(res$p_tost), res$p_exclude_rebound, sig(res$p_exclude_rebound)))
}
equivalence_tests <- bind_rows(equiv_rows)
write.csv(equivalence_tests, file.path(output, "equivalence_tests.csv"), row.names = FALSE)
cat("  Saved: equivalence_tests.csv\n\n")

# ===========================================================================
# PART B — Income x lifestyle two-way interaction on {total, direct, indirect}
# ===========================================================================
# Standardize income (per SD among income-observed households) so the
# interaction coefficient is "change in the adopter-vs-comparison gap per 1 SD
# of income". Households with missing income are imputed to the mean
# (income_centered = 0) and flagged by income_missing, which stays in the model
# as a control so the imputed group does not distort the income slope.
inc_sd <- sd(person_co2e$income_centered[person_co2e$income_missing == 0], na.rm = TRUE)
if (!is.finite(inc_sd) || inc_sd == 0) inc_sd <- 1
person_co2e$income_z <- person_co2e$income_centered / inc_sd

# Controls minus the raw income term (replaced by income_z); esi kept as a
# main-effect control so the income moderation is net of environmental identity.
.income_formula <- function(outcome, lifestyle) {
  ctrl2 <- setdiff(ctrl_vars, "income_centered")
  rhs <- paste(c(paste("income_z *", lifestyle), "esi", ctrl2), collapse = " + ")
  as.formula(paste(outcome, "~", rhs))
}

inc_rows <- list()
cat("--- Part B: Income x lifestyle two-way interaction (per 1 SD income) ---\n")
for (ls in LIFESTYLES) {
  for (tp in c("total", "direct", "indirect")) {
    dv <- if (tp == "total") "total" else paste0(tp, "_", ls)
    rb <- fit_robust(.income_formula(dv, ls), person_co2e, glance = TRUE)
    ls_term  <- paste0(ls, "TRUE")
    inc_term <- "income_z"
    ix_term  <- paste0("income_z:", ls, "TRUE")
    grab <- function(v) { i <- match(v, rb$variable)
                          c(est = rb$estimate[i], se = rb$stderr[i], p = rb$p[i]) }
    l  <- grab(ls_term); inc <- grab(inc_term); ix <- grab(ix_term)
    inc_rows[[length(inc_rows) + 1L]] <- tibble(
      lifestyle = ls, outcome = tp, n = attr(rb, "model") |> nobs(),
      lifestyle_est = l["est"],  lifestyle_p = l["p"],
      income_est    = inc["est"], income_p   = inc["p"],
      income_x_lifestyle_est = ix["est"], income_x_lifestyle_p = ix["p"]
    )
    cat(sprintf("  %-10s %-8s | %s=%7.0f(p=%.3f%s) income=%6.1f(p=%.3f%s) income x %s=%7.1f(p=%.3f%s)\n",
        ls, tp, ls, l["est"], l["p"], sig(l["p"]),
        inc["est"], inc["p"], sig(inc["p"]),
        ls, ix["est"], ix["p"], sig(ix["p"])))
  }
}
income_lifestyle_interactions <- bind_rows(inc_rows)
write.csv(income_lifestyle_interactions,
          file.path(output, "income_lifestyle_interactions.csv"), row.names = FALSE)
cat("  Saved: income_lifestyle_interactions.csv\n\n")
