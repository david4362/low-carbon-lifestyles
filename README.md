# Konsumptionskollen

Analysis pipeline for the manuscript *"Low-carbon lifestyles deliver broad climate benefits strengthened by environmental self-identity"*.

The project examines whether adopting low-carbon lifestyles — no car, no flying, or non-meat diet — reduces emissions not only directly but also indirectly through other consumption categories, and whether this indirect effect is moderated by environmental self-identity (ESI).

## Data

The analysis uses transaction-level bank data from Swedish households linked with survey responses and national income registers. The real data resides in a secure research environment (TRE); `10_load_data.R` is a local placeholder that falls back to a pre-filtered `.RData` file when the shared data directory is unavailable.

Key input objects expected after the data-loading step:

| Object | Description |
|---|---|
| `transactions` | Transaction-level spending and emissions per user-month-category |
| `survey` | Survey responses including ESI items (array3_8, array3_9, array3_11) |
| `users` | User demographics, profile fields, population density |
| `monthly_incomes` | Monthly income from SCB registers |
| `sampling_frame` | Random-invitation frame for sampling-IPW robustness |

Static lookups: `broad-category.csv`, `broad-category-kr.csv`, `categories.csv`, `deso_2018_density.csv`.

## Pipeline overview

The pipeline is orchestrated by `master_analysis.R`, which sources all other scripts in sequence.

### Execution order

| Phase | Scripts |
|---|---|
| Foundation | `00_constants.R`, `01_utils.R`, `02_csv_table_png.R` |
| Data loading | `10_load_data.R` |
| Filtering | `20_filter_data.R` |
| Variable construction | `30_stat_vars.R` |
| Main models | `40_interactions.R`, `50_regressions.R` |
| Main figures | `60_waterfall.R`, `70_S7.R` |
| Robustness & SI | `80_deferred_consumption_tests.R`, `90_SI.R`, `91_interaction_plot.R`, `92_category_decomposition_esi.R`, `93_si_figures.R` |

## File descriptions

### Foundation

| File | Purpose |
|---|---|
| `00_constants.R` | Single source of truth: category lists, lifestyle definitions, label maps, default thresholds |
| `01_utils.R` | Reusable helpers: person-level aggregation, P99 winsorization, robust regression (`fit_robust`), control-data builder, ESI construction |
| `02_csv_table_png.R` | Renders CSV output tables as PNG images for quick inspection |
| `col_specs.R` | Column type specifications for faster CSV reading |

### Pipeline steps (run in order)

| File | Step | Description |
|---|---|---|
| `10_load_data.R` | 1 | Loads data from the shared TRE directory or falls back to a local `.RData`; applies aviation emission-intensity override |
| `20_filter_data.R` | 2 | Selects the analytical sample: consecutive qualifying months, random-sample restriction, adult-only households, credit-card-share exclusion |
| `30_stat_vars.R` | 3 | Builds ESI (factor analysis on 3 survey items), control variables, and lifestyle indicators (`no_car`, `no_flying`, `no_meat`) |
| `40_interactions.R` | 4 | Fits 9 ESI x lifestyle interaction models (3 lifestyles x {total, direct, indirect}) for both CO2e and SEK, with HC3 robust SEs |
| `50_regressions.R` | 5 | Per-category interaction regressions decomposing indirect effects across broad and leaf categories |
| `60_waterfall.R` | 6 | Main manuscript waterfall plot showing direct vs indirect emission differences, with ESI heterogeneity and re-spending benchmarks |
| `70_S7.R` | 7 | Supplementary broad-category waterfall decomposition for CO2e and SEK |

### Robustness and supplementary analyses (run by master_analysis.R)

| File | Step | Description |
|---|---|---|
| `80_deferred_consumption_tests.R` | 10 | Extended-window robustness: re-fits models over all post-COVID months to test deferred consumption |
| `90_SI.R` | 14 | Supplementary residual diagnostics: violin plots, bootstrap CIs, ESI-tetrile group tables |
| `91_interaction_plot.R` | 14b | Continuous ESI interaction plot (adopter vs non-adopter lines with HC3 CIs) |
| `92_category_decomposition_esi.R` | 14b | Category-level decomposition of indirect CO2e at high/low ESI - grouped bar chart |
| `93_si_figures.R` | 14b | Forest/coefficient plots replacing SI tables: main results, threshold sensitivity, diet definitions, robustness comparison |

### Orchestrator

| File | Purpose |
|---|---|
| `master_analysis.R` | Runs the full 15-step pipeline: main analysis + all robustness checks + key-number summary + optional sync to manuscript directory |

## Key analytical concepts

- **Three low-carbon lifestyles**: `no_car` (car-free), `no_flying` (no air travel), `no_meat` (non-meat diet)
- **Direct vs indirect emissions**: Direct = focal domain (e.g., car transport for no_car); Indirect = everything else
- **ESI (Environmental Self-Identity)**: Standardised factor score from 3 survey items (Cronbach's alpha reported)
- **ESI x lifestyle interaction**: Tests whether high-ESI adopters show different indirect emission patterns than low-ESI adopters
- **P99 winsorization**: Caps person-category annual totals at the 99th percentile to control outliers
- **Re-spending benchmark**: Proportional re-spending counterfactual - predicts indirect emissions if saved money is spent at average non-adopter intensity

## Robustness checks

The pipeline includes extensive robustness tests:

1. **IPW robustness** (Step 6): Inverse-probability weighting for lifestyle selection
2. **Sampling IPW** (Step 6b): Weights for representativeness vs. the 80k random-invitation frame
3. **Proportional re-spending benchmark** (Step 7): Counterfactual rebound estimation
4. **Non-adopter baselines** (Step 7b): Predicted footprints at ESI = 0
5. **Income-quartile & urban/rural sensitivity** (Step 8): Three-way interactions
6. **Uncategorized-threshold sensitivity** (Step 9): Varies the max uncategorized-share threshold (5%-20%)
7. **P99 winsorization sensitivity ladder** (Step 9b): Compares no-capping through full-capping specifications
8. **Deferred-consumption tests** (Step 10): Extended observation window post-COVID
9. **Recategorized-transactions robustness** (Step 11): Uses original (non-recategorized) transaction classifications
10. **Green-sample replication** (Step 12): Non-random self-selected subsample
11. **Diet-definition robustness** (Step 13): Alternative diet group definitions
12. **SI residual diagnostics** (Step 14): Bootstrap CIs on residual sign shares

## How to run

### In the TRE (Trusted Research Environment)

```r
source("master_analysis.R")
```

This runs the complete 15-step pipeline and writes all outputs to `output/`.

Configuration switches at the top of `master_analysis.R`:

- `ANALYSIS_DATA_MODE` -- `"auto"` (default), `"mock"`, or `"tre"`
- `SYNC_RESULTS_TO_MANUSCRIPT_DIR` -- auto-copy outputs to a manuscript `Results/` directory
- `EXPORT_CSV_TABLES_AS_PNG` -- render CSV tables as PNG images

### Locally (with mock data)

Ensure a `default_filter.RData` exists in `R/` or the working directory, then:

```r
source("master_analysis.R")
```

The data loader will detect the missing shared directory and fall back to the `.RData` file automatically.

### Individual steps

Each script can also be sourced individually after the prerequisite steps have been run. For example, to regenerate only the waterfall plot:

```r
source("00_constants.R")
source("01_utils.R")
source("10_load_data.R")
source("20_filter_data.R")
source("30_stat_vars.R")
source("40_interactions.R")
source("60_waterfall.R")
```

## Required R packages

dplyr, stringr, purrr, tidyr, readr, haven, readxl, clock, collapse, zoo, lubridate, ggplot2, jtools, interactions, stargazer, broom, tidysdm, gt, boot, sandwich, lmtest, car, gridExtra, psych

## Outputs

All outputs are written to the `output/` directory (or `output/mock/` in mock mode). Key files include:

- **Regression tables**: `interaction regressions {co2e,kr}.{csv,txt}`, `category regression {co2e,kr}.csv`
- **Main figures**: `Waterfall.png`, `Waterfall_pres_main.png`, `Waterfall_pres_esi.png`
- **SI figures**: `S7 detailed {co2e,kr}.png`, `Residuals distribution*.png`, `si_forest_*.png`, `interaction_plot.png`, `category_decomposition_esi.png`
- **Descriptive tables**: `table1_continuous.csv`, `table1_categorical.csv`, `sample_flow.csv`
- **Robustness outputs**: `ipw_robustness.csv`, `robustness_sampling_ipw*.csv`, `rebound_benchmark.csv`, `sensitivity_*.csv`, `deferred_consumption_extended_window.csv`, `robustness_green_sample.csv`, `robustness_recategorized.csv`, `robustness_diet_definitions.csv`
