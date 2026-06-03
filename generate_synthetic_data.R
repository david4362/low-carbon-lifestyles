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
# HELPER: Define category structure with spending weights and emission intensities
# ============================================================================

# All categories organized by broad category with weights (% of total spending)
# Weights represent typical Swedish household spending patterns
# Based on SCB household expenditure surveys

get_category_params <- function() {
  
  # Weight definitions by broad category and specific categories
  # Format: list(broad_category = list(category = weight), ...)
  
  list(
    # === FOCAL: Car & Public Transport (no_car) ===
    Car_Public = list(
      fuel         = 0.050,      # Car fuel
      car_maint    = 0.015,      # Car maintenance/repairs
      car_rent     = 0.008,      # Car rental
      vehicles     = 0.025,      # Vehicle purchase (amortized)
      public_trans = 0.012,      # Public transportation
      bus          = 0.003,      # Bus
      taxi         = 0.005,      # Taxi
      transport_other = 0.004,  # Other transport
      escooter     = 0.003       # Escooter rentals
    ),
    
    # === FOCAL: Aviation (no_flying) ===
    Aviation_LDT = list(
      aviation = 0.035          # Air travel
    ),
    
    # === FOCAL: Food (no_meat) ===
    Food = list(
      groceries   = 0.140,       # Groceries (main food spending)
      restaurant  = 0.055,       # Restaurants
      alcohol     = 0.012,       # Alcohol
      bar         = 0.008,       # Bars
      snacks      = 0.010,       # Snacks
      tobacco     = 0.006,       # Tobacco
      food_other  = 0.009        # Other food
    ),
    
    # === Housing ===
    Housing = list(
      rent               = 0.180,  # Rent
      electricity        = 0.025,  # Electricity
      gas                = 0.005,  # Natural gas
      district_heating  = 0.035,  # District heating
      solid_fuels       = 0.002,   # Wood/solid fuels
      liquid_fuels      = 0.003,   # Heating oil
      repair_home       = 0.015,  # Home repairs
      repair_build      = 0.008,  # Building repairs
      housing_other     = 0.010,  # Other housing
      utilities_heating_other = 0.005  # Other heating utilities
    ),
    
    # === Other Products ===
    Other_products = list(
      clothing       = 0.035,     # Clothing
      electronics   = 0.020,     # Electronics
      furniture     = 0.018,     # Furniture
      appliances    = 0.012,     # Home appliances
      books         = 0.006,      # Books
      toys          = 0.005,      # Toys
      sports        = 0.008,      # Sports equipment
      pets          = 0.007,      # Pet supplies
      agriculture   = 0.003,      # Agricultural products
      pharmacy      = 0.008,      # Pharmacy
      glasses_lenses = 0.004,     # Glasses/lenses
      jewelry       = 0.004,      # Jewelry
      shopping_other = 0.010,     # Other shopping
      home_garden_other = 0.006   # Home/garden
    ),
    
    # === Other Services ===
    Other_services = list(
      beauty         = 0.010,    # Beauty services
      culture        = 0.015,    # Cultural activities
      health         = 0.008,    # Health services
      health_care    = 0.006,    # Healthcare
      health_other   = 0.004,    # Other health
      insurance      = 0.020,     # Insurance
      financial_service = 0.008, # Financial services
      internet_tele  = 0.012,    # Internet/telecom
      leisure_other  = 0.008,    # Other leisure
      repair_rent    = 0.005,    # Repair services
      services       = 0.010,    # General services
      sports         = 0.007      # Sports services
    ),
    
    # === Vacation & Long-distance Travel ===
    Vacation_LDT = list(
      ferry     = 0.004,    # Ferry
      train_bus = 0.008,    # Train/long-distance bus
      travel    = 0.015     # Other travel
    ),
    
    # === Other Misc ===
    Other_misc = list(
      uncategorized      = 0.050,  # Uncategorized transactions
      transaction       = 0.030,  # General transactions
      cash              = 0.015,  # Cash withdrawals
      outlay            = 0.010,  # Outlays
      transfer_to_creditcard = 0.025  # Credit card transfers
    )
  )
}

# Emission intensities (kg CO2e per SEK) by category
# Based on Swedish environmental agency (Naturvårdsverket) and research
get_emission_intensities <- function() {
  list(
    # Transport (high emissions)
    fuel         = 0.0029,     # Petrol/diesel
    car_maint    = 0.0015,
    car_rent     = 0.0020,
    vehicles     = 0.0015,
    public_trans = 0.0004,
    bus          = 0.0005,
    taxi         = 0.0020,
    transport_other = 0.0015,
    escooter     = 0.0008,
    aviation     = 0.0014,     # High-emission air travel
    ferry        = 0.0008,
    train_bus    = 0.0003,
    travel       = 0.0010,
    
    # Food (medium-high emissions)
    groceries   = 0.0012,     # Varies by diet
    restaurant   = 0.0010,
    alcohol      = 0.0008,
    bar          = 0.0008,
    snacks       = 0.0008,
    tobacco      = 0.0020,
    food_other   = 0.0008,
    
    # Housing (medium emissions)
    rent         = 0.0002,
    electricity  = 0.0003,     # Swedish grid (low carbon)
    gas          = 0.0020,
    district_heating = 0.0004,
    solid_fuels  = 0.0010,
    liquid_fuels = 0.0025,
    repair_home  = 0.0010,
    repair_build = 0.0010,
    housing_other = 0.0008,
    utilities_heating_other = 0.0005,
    
    # Other products (medium emissions)
    clothing     = 0.0018,
    electronics  = 0.0008,
    furniture    = 0.0010,
    appliances   = 0.0012,
    books        = 0.0005,
    toys         = 0.0010,
    sports       = 0.0010,
    pets         = 0.0010,
    agriculture  = 0.0015,
    pharmacy     = 0.0005,
    glasses_lenses = 0.0005,
    jewelry      = 0.0010,
    shopping_other = 0.0010,
    home_garden_other = 0.0010,
    
    # Other services (low-medium emissions)
    beauty       = 0.0006,
    culture      = 0.0005,
    health       = 0.0005,
    health_care  = 0.0005,
    health_other = 0.0005,
    insurance    = 0.0003,
    financial_service = 0.0002,
    internet_tele = 0.0004,
    leisure_other = 0.0005,
    repair_rent  = 0.0010,
    services     = 0.0005,
    sports       = 0.0005,
    
    # Misc (zero/low emissions - transfers)
    uncategorized = 0.0003,
    transaction  = 0.0,
    cash         = 0.0,
    outlay       = 0.0,
    transfer_to_creditcard = 0.0,
    
    # Savings/excluded (zero)
    savings      = 0.0,
    exclude      = 0.0
  )
}

# ============================================================================
# MAIN
# ============================================================================

set.seed(SEED)
cat("Generating synthetic data for Konsumtionskollen pipeline...\n\n")

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
# Uses realistic category weights and emission intensities

cat("  Creating transactions (with all categories)...\n")

# Lifestyle indicators
no_car <- n_cars == 0
no_flying <- runif(N_USERS) < 0.15
if (all(no_flying)) no_flying[1] <- FALSE  # guarantee >=1 flyer => aviation always present
no_meat <- diets != "mixed"

# Get category parameters
cat_params <- get_category_params()
emission_intensities <- get_emission_intensities()

# Flatten the nested params into one named base-weight vector. unlist() prefixes
# names with the broad category ("Car_Public.fuel"), so strip that to recover the
# transaction-level category name. ("sports" is listed under two broad
# categories; keep the first definition so the names stay unique.)
base_weights_vec <- unlist(cat_params, recursive = TRUE)
names(base_weights_vec) <- sub(".*\\.", "", names(base_weights_vec))
base_weights_vec <- base_weights_vec[!duplicated(names(base_weights_vec))]
all_cats <- names(base_weights_vec)

# Build user-specific weight matrix (one column per category), then apply the
# lifestyle adjustments below.
user_weights <- matrix(rep(base_weights_vec, each = N_USERS),
                       nrow = N_USERS, dimnames = list(NULL, all_cats))

# Apply lifestyle adjustments
# no_car: reduce car-related spending, increase public transport
user_weights[, "fuel"] <- ifelse(no_car, 0.002, user_weights[, "fuel"])
user_weights[, "car_maint"] <- ifelse(no_car, 0.003, user_weights[, "car_maint"])
user_weights[, "car_rent"] <- ifelse(no_car, 0.001, user_weights[, "car_rent"])
user_weights[, "vehicles"] <- ifelse(no_car, 0.002, user_weights[, "vehicles"])
user_weights[, "public_trans"] <- ifelse(no_car, 0.020, user_weights[, "public_trans"])
user_weights[, "taxi"] <- ifelse(no_car, 0.008, user_weights[, "taxi"])

# no_flying: zero aviation (the analysis flags no_flying as *exactly* zero
# aviation emissions); the kr override below also enforces this on the output.
user_weights[, "aviation"] <- ifelse(no_flying, 0, user_weights[, "aviation"])
user_weights[, "ferry"] <- ifelse(no_flying, 0.006, user_weights[, "ferry"])
user_weights[, "travel"] <- ifelse(no_flying, 0.010, user_weights[, "travel"])

# no_meat: reduce food categories (especially groceries, meat is implicit)
user_weights[, "groceries"] <- ifelse(no_meat, 0.120, user_weights[, "groceries"])
user_weights[, "restaurant"] <- ifelse(no_meat, 0.045, user_weights[, "restaurant"])
user_weights[, "alcohol"] <- ifelse(no_meat, 0.008, user_weights[, "alcohol"])

# Renters vs owners (affects housing spending patterns)
is_renter <- hometypes == "Lägenhet"
user_weights[, "rent"] <- ifelse(is_renter, 0.220, 0.120)
user_weights[, "repair_home"] <- ifelse(is_renter, 0.005, 0.025)
user_weights[, "repair_build"] <- ifelse(is_renter, 0.003, 0.015)

# Normalize weights to sum to 1
row_sums <- rowSums(user_weights)
user_weights <- user_weights / row_sums

# Create transaction data frame
n_rows <- N_USERS * N_MONTHS

transactions <- tibble(
  aid = rep(aids, each = N_MONTHS),
  date = rep(seq(as.Date("2018-01-01"), by = "month", length.out = N_MONTHS), N_USERS)
)

# Monthly spending total (discretionary spending ratio varies by income)
spend_ratio <- runif(N_USERS, 0.55, 0.80)
monthly_total <- rep(base_income * spend_ratio, each = N_MONTHS) * runif(n_rows, 0.85, 1.15)

# Generate spending for each category
for (cat_name in all_cats) {
  # Get weights for this category (expanded to all months)
  user_wts <- rep(user_weights[, cat_name], each = N_MONTHS)
  
  # Calculate spending with variation
  kr_vals <- monthly_total * user_wts * runif(n_rows, 0.7, 1.3)
  
  # Aviation defines the no_flying lifestyle, which the analysis treats as
  # *exactly zero* aviation emissions. Force designated non-flyers to zero, and
  # guarantee flyers a positive amount, so the synthetic data always contains
  # some aviation transactions and no_flying stays a meaningful (~15%) group.
  if (cat_name == "aviation") {
    flyer <- rep(!no_flying, each = N_MONTHS)
    kr_vals[!flyer] <- 0
    kr_vals[flyer]  <- pmax(kr_vals[flyer], monthly_total[flyer] * 0.005)
  }
  
  # Non-car owners: minimal fuel/car spending
  if (cat_name %in% c("fuel", "car_maint", "car_rent", "vehicles")) {
    no_car_expanded <- rep(no_car, each = N_MONTHS)
    kr_vals[no_car_expanded] <- kr_vals[no_car_expanded] * 0.15
  }
  
  # Add to transactions
  transactions[[paste0(cat_name, ".kr")]] <- kr_vals
  
  # Calculate emissions with category-specific intensity
  intensity <- emission_intensities[[cat_name]]
  if (is.null(intensity)) intensity <- 0.0005  # default
  
  transactions[[paste0(cat_name, ".co2e")]] <- kr_vals * intensity * runif(n_rows, 0.8, 1.2)
}

# Add savings and exclude columns (transfers, not spending)
transactions$savings.kr <- monthly_total * runif(n_rows, 0.02, 0.08)
transactions$savings.co2e <- 0
transactions$exclude.kr <- 0
transactions$exclude.co2e <- 0

# Add charity (small but non-zero)
transactions$charity.kr <- monthly_total * runif(n_rows, 0.005, 0.02)
transactions$charity.co2e <- transactions$charity.kr * 0.0003

# Add income categories (as negative values / refunds in spending data)
# These are typically excluded from consumption totals
transactions$income_salary.kr <- 0
transactions$income_salary.co2e <- 0
transactions$income_financial.kr <- 0
transactions$income_financial.co2e <- 0
transactions$income_benefits.kr <- 0
transactions$income_benefits.co2e <- 0
transactions$income_pensions.kr <- 0
transactions$income_pensions.co2e <- 0
transactions$income_refund.kr <- 0
transactions$income_refund.co2e <- 0
transactions$income_other.kr <- 0
transactions$income_other.co2e <- 0

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

OUTPUT_FILE <- if (exists("OUTPUT_FILE")) OUTPUT_FILE else "default_filter.RData"
cat(sprintf("\nSaving to %s...\n", OUTPUT_FILE))

save(transactions, survey, users, monthly_incomes, sampling_frame,
     file = OUTPUT_FILE)

cat(sprintf("Done! File size: %.1f MB\n", file.info(OUTPUT_FILE)$size / 1024^2))
cat("\nObjects created:\n")
cat(sprintf("  transactions: %d x %d\n", nrow(transactions), ncol(transactions)))
cat(sprintf("  users: %d x %d\n", nrow(users), ncol(users)))
cat(sprintf("  survey: %d x %d\n", nrow(survey), ncol(survey)))
cat(sprintf("  monthly_incomes: %d x %d\n", nrow(monthly_incomes), ncol(monthly_incomes)))
cat(sprintf("  sampling_frame: %d x %d\n", nrow(sampling_frame), ncol(sampling_frame)))
cat("\nTo run the pipeline:\n")
cat('  source("master_analysis.R")\n')
