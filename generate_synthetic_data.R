###############################################################################
# generate_synthetic_data.R
#
# Generates synthetic data for the Konsumtionskollen analysis pipeline.
# This allows running the complete analysis outside the secure TRE environment.
#
# Usage:
#   source("generate_synthetic_data.R")
#
# Creates: default_filter.RData with all required objects
###############################################################################

library(dplyr)
library(tidyr)

# ============================================================================
# CONFIGURATION
# ============================================================================

N_USERS <- 1500        # Target sample size
N_MONTHS <- 42         # Months of data per user (spanning pre and post-COVID)
SEED <- 42

# ============================================================================
# HELPER: Generate correlated Likert items
# ============================================================================

generate_esi_items <- function(n, rho = 0.6) {
  latent <- rnorm(n)
  noise_sd <- sqrt(1 - rho^2)
  
  items <- cbind(
    latent + rnorm(n, sd = noise_sd),
    latent + rnorm(n, sd = noise_sd),
    latent + rnorm(n, sd = noise_sd)
  )
  
  # Scale to 1-5 Likert
  apply(items, 2, function(x) {
    x_std <- (x - mean(x)) / sd(x)
    pmax(1, pmin(5, round(3 + x_std * 0.8)))
  })
}

# ============================================================================
# MAIN
# ============================================================================

set.seed(SEED)
cat("Generating synthetic data for Konsumptionskollen pipeline...\n\n")

# --- Users ------------------------------------------------------------------

cat("  Creating users (N =", N_USERS, ")...\n")

aids <- sprintf("user_%06d", seq_len(N_USERS))
ages <- sample(18:80, N_USERS, replace = TRUE)
sexes <- sample(c("male", "female"), N_USERS, replace = TRUE)
randomsample <- rbinom(N_USERS, 1, 0.8)

# Education (SUN codes)
edu_codes <- sample(c(100, 200, 300, 400, 500, 600, 999), N_USERS, 
                    replace = TRUE, prob = c(0.1, 0.2, 0.15, 0.1, 0.3, 0.1, 0.05))

n_adults <- sample(1:2, N_USERS, replace = TRUE, prob = c(0.4, 0.6))
hometypes <- sample(c("Lägenhet", "Villa/Radhus"), N_USERS, replace = TRUE)
municipalities <- sample(c("Stockholm", "Göteborg", "Malmö", "Uppsala", "Other"), 
                         N_USERS, replace = TRUE, prob = c(0.15, 0.08, 0.05, 0.04, 0.68))
pop_density <- exp(rnorm(N_USERS, log(500), 1.5))
n_cars <- rbinom(N_USERS, 3, 0.3)
diets <- sample(c("mixed", "vegetarian", "vegfish", "vegan"), N_USERS, 
                replace = TRUE, prob = c(0.75, 0.10, 0.10, 0.05))

user_created <- sample(seq(as.Date("2019-01-01"), as.Date("2022-01-01"), by = "day"),
                       N_USERS, replace = TRUE)

users <- tibble(
  aid = aids,
  date = as.Date(Sys.time()),
  `historic-rank-value` = NA_real_,
  `bank-updated-in-period` = NA_real_,
  `first-bank-access` = user_created - sample(30:180, N_USERS, replace = TRUE),
  bank1 = "bank_a",
  bank2 = NA_character_,
  bank3 = NA_character_,
  `nr-of-bank-accounts` = 1,
  `bank-accounts-not-shared` = 0,
  `bank-accounts-shared-2` = 0,
  `bank-accounts-shared-3` = 0,
  `bank-accounts-shared-gt-3` = 0,
  `has-excluded-account` = 0,
  `user-created` = user_created,
  `last-bank-date` = user_created + sample(365:730, N_USERS, replace = TRUE),
  `last-access` = as.Date(Sys.time()),
  `income-level` = sample(1:5, N_USERS, replace = TRUE),
  `nr-of-saved-actions` = sample(0:50, N_USERS, replace = TRUE),
  `study-group` = "control",
  `absolute-impact-guess` = NA_real_,
  `relative-impact-guess` = NA_character_,
  `rank-stat-for-impact-guess` = NA_real_,
  `activity-total-engagements` = sample(0:100, N_USERS, replace = TRUE),
  `activity-total-seconds` = sample(0:10000, N_USERS, replace = TRUE),
  `activity-groups-engagements` = NA,
  `activity-groups-seconds` = NA,
  `activity-target-engagements` = sample(0:50, N_USERS, replace = TRUE),
  `activity-target-seconds` = sample(0:5000, N_USERS, replace = TRUE),
  `activity-overview-engagements` = sample(0:30, N_USERS, replace = TRUE),
  `activity-overview-seconds` = sample(0:3000, N_USERS, replace = TRUE),
  `profile:date` = as.Date(Sys.time()),
  `profile:has-adress` = 1,
  `profile:kommun` = municipalities,
  `profile:kommungrupp` = "Sweden",
  `profile:hometype` = hometypes,
  `profile:home-heating` = "district_heating",
  `profile:home-heating-efficiency` = NA_real_,
  `profile:home-heating-efficiency-part-electric` = NA_real_,
  `profile:ncars` = n_cars,
  `profile:car1:fuel_term` = ifelse(n_cars >= 1, "petrol", NA_character_),
  `profile:car1:fuel_consumption` = ifelse(n_cars >= 1, runif(sum(n_cars >= 1), 6, 12), NA_real_),
  `profile:car1:distance` = ifelse(n_cars >= 1, runif(sum(n_cars >= 1), 5000, 20000), NA_real_),
  `profile:car2:fuel_term` = ifelse(n_cars >= 2, "petrol", NA_character_),
  `profile:car2:fuel_consumption` = NA_real_,
  `profile:car2:distance` = NA_real_,
  `profile:car3:fuel_term` = NA_character_,
  `profile:car3:fuel_consumption` = NA_real_,
  `profile:car3:distance` = NA_real_,
  `profile.field_profile_household_adults` = n_adults,
  `profile.field_profile_household_children` = rbinom(N_USERS, 3, 0.3),
  `profile.field_profile_home_size` = sample(30:200, N_USERS, replace = TRUE),
  `profile.field_profile_home_kwh` = NA_real_,
  `profile.field_profile_home_m3` = NA_real_,
  `profile.field_profile_home_year` = sample(1950:2020, N_USERS, replace = TRUE),
  `profile.field_profile_home_kg` = NA_real_,
  `profile.field_profile_home_el_know` = NA_character_,
  `profile.field_profile_commute_distance` = sample(1:50, N_USERS, replace = TRUE),
  `profile.field_profile_commute_public` = runif(N_USERS),
  `profile.field_profile_commute_bike` = runif(N_USERS),
  `profile.field_profile_commute_car` = runif(N_USERS),
  `profile.field_profile_target_level` = sample(1:5, N_USERS, replace = TRUE),
  `profile.field_food_diet` = diets,
  `profile.field_food_physical_activity` = sample(1:5, N_USERS, replace = TRUE),
  `profile.field_food_waste` = sample(1:5, N_USERS, replace = TRUE),
  `profile.field_food_price_class` = sample(1:5, N_USERS, replace = TRUE),
  `profile.field_food_cheese` = sample(1:5, N_USERS, replace = TRUE),
  `profile.field_food_milk` = sample(1:5, N_USERS, replace = TRUE),
  `profile.field_food_creame` = sample(1:5, N_USERS, replace = TRUE),
  `profile.field_food_fish` = sample(1:5, N_USERS, replace = TRUE),
  `profile.field_food_beef` = sample(1:5, N_USERS, replace = TRUE),
  `profile.field_food_pork` = sample(1:5, N_USERS, replace = TRUE),
  `profile.field_food_game` = sample(1:5, N_USERS, replace = TRUE)
)

users$age <- ages
users$sex <- sexes
users$Sun2020Niva <- edu_codes
users$randomsample <- randomsample
users$pop_density <- pop_density

# Convert column names from colon (:) to dot (.) notation for compatibility
# The pipeline expects profile.hometype not profile:hometype
names(users) <- gsub(":", ".", names(users))

# --- Survey -----------------------------------------------------------------

cat("  Creating survey...\n")

esi_items <- generate_esi_items(N_USERS, rho = 0.65)

survey <- tibble(
  aid = aids,
  array3_8 = esi_items[, 1],
  array3_9 = esi_items[, 2],
  array3_11 = esi_items[, 3]
)

# --- Monthly Incomes --------------------------------------------------------

cat("  Creating monthly incomes...\n")

base_income <- 25000 + edu_codes * 50 + (ages - 30) * 200
base_income <- pmax(15000, pmin(80000, base_income + rnorm(N_USERS, 0, 10000)))

monthly_incomes <- tibble(
  aid = rep(aids, each = N_MONTHS),
  date = rep(seq(as.Date("2018-01-01"), by = "month", length.out = N_MONTHS), N_USERS),
  category = "salary",
  income = rep(base_income, each = N_MONTHS) * runif(N_USERS * N_MONTHS, 0.9, 1.1)
)

# --- Transactions -----------------------------------------------------------
# Generate in wide format (one row per user-month)

cat("  Creating transactions (vectorized)...\n")

# Lifestyle indicators
no_car <- n_cars == 0
no_flying <- runif(N_USERS) < 0.15
no_meat <- diets != "mixed"

# Spending categories to generate
cats <- c("groceries", "alcohol", "tobacco", "clothing", "rent", "electricity",
          "fuel", "car_maint", "vehicles", "aviation", "restaurant", "culture",
          "health", "insurance", "furniture", "electronics", "other_other",
          "uncategorized", "transaction", "savings")

# Build base weights for each user (vectorized)
base_weights <- matrix(0.02, nrow = N_USERS, ncol = length(cats))
colnames(base_weights) <- cats

# Adjust by lifestyle
base_weights[, "groceries"] <- ifelse(no_meat, 0.14, 0.18)
base_weights[, "rent"] <- 0.28
base_weights[, "fuel"] <- ifelse(no_car, 0.005, 0.07)
base_weights[, "car_maint"] <- ifelse(no_car, 0.003, 0.02)
base_weights[, "vehicles"] <- ifelse(no_car, 0.001, 0.03)
base_weights[, "aviation"] <- ifelse(no_flying, 0.002, 0.05)
base_weights[, "restaurant"] <- 0.08
base_weights[, "uncategorized"] <- 0.06

# Normalize per user
weight_sums <- rowSums(base_weights)
base_weights <- base_weights / weight_sums

# Create transaction data frame
n_rows <- N_USERS * N_MONTHS

transactions <- tibble(
  aid = rep(aids, each = N_MONTHS),
  date = rep(seq(as.Date("2018-01-01"), by = "month", length.out = N_MONTHS), N_USERS)
)

# Monthly spending total
spend_ratio <- runif(N_USERS, 0.6, 0.85)
monthly_total <- rep(base_income * spend_ratio, each = N_MONTHS) * runif(n_rows, 0.85, 1.15)

# Add spending columns (vectorized)
for (i in seq_along(cats)) {
  cat_name <- cats[i]
  
  # Get weights for this category (expanded to all months)
  user_weights <- rep(base_weights[, i], each = N_MONTHS)
  
  # Calculate spending with variation
  kr_vals <- monthly_total * user_weights * runif(n_rows, 0.8, 1.2)
  
  # Special handling: non-flyers should have zero aviation spending
  if (cat_name == "aviation") {
    # Expand no_flying flag to all months
    no_flying_expanded <- rep(no_flying, each = N_MONTHS)
    kr_vals[no_flying_expanded] <- 0
  }
  
  transactions[[paste0(cat_name, ".kr")]] <- kr_vals
  
  # Emissions (kg CO2e) with category-specific intensities
  intensity <- switch(cat_name,
    fuel = 0.003,
    groceries = 0.001,
    aviation = 0.0015,
    electricity = 0.0003,
    rent = 0.0002,
    clothing = 0.0015,
    0.0005  # default
  )
  
  transactions[[paste0(cat_name, ".co2e")]] <- kr_vals * intensity * runif(n_rows, 0.8, 1.2)
}

# Add remaining required columns (set to 0 for simplicity)
remaining_cats <- c("repair_rent", "repair_build", "gas", "liquid_fuels", "solid_fuels",
                    "district_heating", "repair_home", "appliances", "services",
                    "pharmacy", "glasses_lenses", "car_rent", "train_bus", "bus", "taxi",
                    "ferry", "public_trans", "escooter", "internet_tele", "sport_equip",
                    "toys", "agriculture", "pets", "sports", "books", "travel",
                    "snacks", "bar", "beauty", "jewelry", "health_care",
                    "financial_service", "food_other", "shopping_other", "housing_other",
                    "home_garden_other", "utilities_heating_other", "health_other",
                    "transport_other", "leisure_other", "charity", "cash", "outlay",
                    "transfer_to_creditcard", "exclude",
                    "income_salary", "income_financial", "income_benefits", 
                    "income_pensions", "income_refund", "income_other")

for (cat_name in remaining_cats) {
  transactions[[paste0(cat_name, ".kr")]] <- 0
  transactions[[paste0(cat_name, ".co2e")]] <- 0
}

# --- Sampling Frame ---------------------------------------------------------

cat("  Creating sampling frame...\n")

frame_n <- N_USERS * 8

sampling_frame <- tibble(
  aid = c(aids, sprintf("frame_%06d", seq_len(frame_n - N_USERS))),
  postort = sample(c("Stockholm", "GÖTEBORG", "MALMÖ", "Uppsala", "Other"),
                   frame_n, replace = TRUE, prob = c(0.15, 0.08, 0.05, 0.04, 0.68)),
  age = sample(18:80, frame_n, replace = TRUE),
  gender = sample(c("Man", "Kvinna"), frame_n, replace = TRUE),
  randomsample = c(rep(1, N_USERS), rep(0, frame_n - N_USERS)),
  efid = c(aids, sprintf("frame_%06d", seq_len(frame_n - N_USERS)))
)

# --- Save -------------------------------------------------------------------

cat("\nSaving to default_filter.RData...\n")

save(transactions, survey, users, monthly_incomes, sampling_frame,
     file = "default_filter.RData")

cat(sprintf("Done! File size: %.1f MB\n", file.info("default_filter.RData")$size / 1024^2))
cat("\nObjects created:\n")
cat(sprintf("  transactions: %d x %d\n", nrow(transactions), ncol(transactions)))
cat(sprintf("  users: %d x %d\n", nrow(users), ncol(users)))
cat(sprintf("  survey: %d x %d\n", nrow(survey), ncol(survey)))
cat(sprintf("  monthly_incomes: %d x %d\n", nrow(monthly_incomes), ncol(monthly_incomes)))
cat(sprintf("  sampling_frame: %d x %d\n", nrow(sampling_frame), ncol(sampling_frame)))
cat("\nTo run the pipeline:\n")
cat('  source("master_analysis.R")\n')
