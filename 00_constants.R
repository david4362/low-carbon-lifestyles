###############################################################################
# constants.R — single source of truth for category lists, lifestyle defs,
# and label maps used across the pipeline.
###############################################################################

# Bank-deposit / savings / transfer categories that must be excluded when
# computing "consumption" totals.
non_cost_categories <- c(
  'income_salary.kr', 'income_financial.kr', 'income_benefits.kr',
  'income_pensions.kr', 'income_refund.kr', 'income_other.kr',
  'da_credit_.kr', 'transaction.kr', 'savings.kr', 'exclude.kr'
)

# Categories that look like inter-account transfers rather than purchases.
transfer_like_categories <- c(
  'transaction.kr', 'savings.kr', 'exclude.kr',
  'transfer_to_creditcard.kr', 'da_credit_.kr'
)

# Used by the proportional re-spending benchmark and waterfall.
non_purchase_categories <- c(
  "transaction.kr", "exclude.kr", "savings.kr",
  "transfer_to_creditcard.kr", "da_credit_.kr"
)

# Lifestyle definitions: each entry maps a lifestyle indicator to the column
# and value that defines its DIRECT (focal) emissions/spending.
lifestyle_focal_co2e <- list(
  no_car    = list(field = "broad_category", value = "Car_Public_co2e"),
  no_flying = list(field = "broad_category", value = "Aviation_LDT_co2e"),
  no_meat   = list(field = "category",       value = "groceries.co2e")
)
lifestyle_focal_kr <- list(
  no_car    = list(field = "broad_category", value = "Car_Public_kr"),
  no_flying = list(field = "broad_category", value = "Aviation_LDT_kr"),
  no_meat   = list(field = "category",       value = "groceries.kr")
)

LIFESTYLES <- c("no_car", "no_flying", "no_meat")

# Categories targeted for P99 winsorization (high-volatility / non-routine flows).
# Keys here are CO2e suffixes; the .kr equivalents are derived in utils.R.
p99_target_suffix_co2e <- c("financial_service.co2e", "transfer_to_creditcard.co2e")

# Plot / output labels
plot_labels <- list(
  no_car    = "Car ownership",
  no_flying = "Air travel",
  no_meat   = "Diet",
  total     = "Total",
  direct    = "Direct",
  indirect  = "Indirect",
  high      = "High ESI",
  low       = "Low ESI"
)

interaction_model_labels <- c(
  "Total emissions Car ownership", "Direct emissions Car ownership", "Indirect emissions Car ownership",
  "Total emissions Air travel",    "Direct emissions Air travel",    "Indirect emissions Air travel",
  "Total emissions Diet",          "Direct emissions Diet",          "Indirect emissions Diet"
)
interaction_model_labels_kr <- c(
  "Total kr Car ownership", "Direct kr Car ownership", "Indirect kr Car ownership",
  "Total kr Air travel",    "Direct kr Air travel",    "Indirect kr Air travel",
  "Total kr Diet",          "Direct kr Diet",          "Indirect kr Diet"
)

# Default thresholds (kept as variables so master can override).
CC_SHARE_THRESHOLD_DEFAULT       <- 0.30
UNCAT_THRESHOLD_DEFAULT          <- 0.10
CONSECUTIVE_MONTHS_DEFAULT       <- 12
P99_QUANTILE_DEFAULT             <- 0.99
COVID_START_DEFAULT              <- "2020-03-01"
COVID_END_DEFAULT                <- "2021-03-01"

# ---- Broad-category labels for waterfall / S7 / category_decomposition_esi --
broad_cats_co2e <- c("Car_Public_co2e", "Aviation_LDT_co2e", "Food_co2e",
                     "Housing_co2e", "Other_products_co2e", "Other_services_co2e",
                     "Other_misc_co2e", "Vacation_LDT_co2e")
broad_cats_kr   <- sub("_co2e$", "_kr", broad_cats_co2e)

broad_labels <- c(
  "Car_Public_co2e"     = "Car & Public\nTransport",
  "Aviation_LDT_co2e"   = "Aviation &\nLong-dist Travel",
  "Food_co2e"           = "Food",
  "Housing_co2e"        = "Housing",
  "Other_products_co2e" = "Other\nProducts",
  "Other_services_co2e" = "Other\nServices",
  "Other_misc_co2e"     = "Other\nMisc",
  "Vacation_LDT_co2e"   = "Vacation\nTravel"
)
broad_labels <- c(broad_labels, setNames(unname(broad_labels), broad_cats_kr))
