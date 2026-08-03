# Gallup Analysis: make multiverse summary tables

# ---------------------------------------------------------------------------
# 1. Setup
# ---------------------------------------------------------------------------

# Run with the working directory set to the repository folder.
project_dir <- getwd()

derived_dir <- file.path(project_dir, "results", "derived")
tables_dir <- file.path(project_dir, "results", "tables")

dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

library(dplyr)

results <- read.csv(
  file.path(derived_dir, "gallup_multiverse_results.csv"),
  stringsAsFactors = FALSE
)

valid_results <- results %>% filter(!is.na(coef))

# ---------------------------------------------------------------------------
# 2. Overall multiverse summary
# ---------------------------------------------------------------------------

overall_summary <- data.frame(
  metric = c(
    "Total specifications",
    "Valid specifications",
    "Median coefficient",
    "Mean coefficient",
    "SD coefficient",
    "Min coefficient",
    "Max coefficient",
    "Percent positive coefficient",
    "Percent significant at p < .05",
    "Percent positive and significant at p < .05",
    "Median standard error",
    "Median N observations",
    "Median R-squared"
  ),
  value = c(
    nrow(results),
    nrow(valid_results),
    round(median(valid_results$coef, na.rm = TRUE), 3),
    round(mean(valid_results$coef, na.rm = TRUE), 3),
    round(sd(valid_results$coef, na.rm = TRUE), 3),
    round(min(valid_results$coef, na.rm = TRUE), 3),
    round(max(valid_results$coef, na.rm = TRUE), 3),
    round(100 * mean(valid_results$coef > 0, na.rm = TRUE), 1),
    round(100 * mean(valid_results$pvalue < .05, na.rm = TRUE), 1),
    round(100 * mean(valid_results$coef > 0 & valid_results$pvalue < .05, na.rm = TRUE), 1),
    round(median(valid_results$se, na.rm = TRUE), 3),
    round(median(valid_results$n_obs, na.rm = TRUE), 0),
    round(median(valid_results$r_squared, na.rm = TRUE), 3)
  )
)

write.csv(
  overall_summary,
  file.path(tables_dir, "gallup_multiverse_summary_overall.csv"),
  row.names = FALSE
)

# ---------------------------------------------------------------------------
# 3. Appendix Table B1: summary by modeling choice
# ---------------------------------------------------------------------------

summarize_dimension <- function(data, variable, dimension_label) {
  data %>%
    group_by(choice = .data[[variable]]) %>%
    summarize(
      n_specs = n(),
      median_coef = round(median(coef, na.rm = TRUE), 3),
      mean_coef = round(mean(coef, na.rm = TRUE), 3),
      pct_positive = round(100 * mean(coef > 0, na.rm = TRUE), 1),
      pct_positive_sig_05 = round(100 * mean(coef > 0 & pvalue < .05, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    mutate(dimension = dimension_label) %>%
    select(dimension, choice, everything())
}

table_b1 <- bind_rows(
  summarize_dimension(valid_results, "ownership_var", "Ownership operationalization"),
  summarize_dimension(valid_results, "fixed_effects", "Fixed effects"),
  summarize_dimension(
    valid_results %>% mutate(include_lag = ifelse(include_lag == 1, "With lag", "No lag")),
    "include_lag",
    "Lagged dependent variable"
  ),
  summarize_dimension(
    valid_results %>% mutate(first_year_interaction = ifelse(first_year_interaction == 1, "With interaction", "No interaction")),
    "first_year_interaction",
    "First-year term"
  ),
  summarize_dimension(valid_results, "objective_controls", "Objective controls"),
  summarize_dimension(
    valid_results %>% mutate(event_controls = ifelse(event_controls == 1, "With events", "No events")),
    "event_controls",
    "Event controls"
  ),
  summarize_dimension(valid_results, "political_controls", "Political controls")
)

write.csv(
  table_b1,
  file.path(tables_dir, "gallup_appendix_b1_by_modeling_choice.csv"),
  row.names = FALSE
)

# ---------------------------------------------------------------------------
# 4. Appendix Table D1: all multiverse specifications
# ---------------------------------------------------------------------------

table_d1 <- valid_results %>%
  transmute(
    spec_id,
    ownership_var,
    fixed_effects,
    include_lag,
    first_year_interaction,
    objective_controls,
    event_controls,
    political_controls,
    estimate = coef,
    standard_error = se,
    p_value = pvalue,
    n_obs,
    r_squared
  )

write.csv(
  table_d1,
  file.path(tables_dir, "gallup_appendix_d1_all_specifications.csv"),
  row.names = FALSE
)

cat("Saved Gallup summary tables to:", tables_dir, "\n")
