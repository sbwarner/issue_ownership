# Gallup Analysis: define the multiverse specifications

# ---------------------------------------------------------------------------
# 1. Setup
# ---------------------------------------------------------------------------

# Run with the working directory set to the repository folder.
project_dir <- getwd()

derived_dir <- file.path(project_dir, "results", "derived")
dir.create(derived_dir, showWarnings = FALSE, recursive = TRUE)

library(tidyr)
library(dplyr)

# ---------------------------------------------------------------------------
# 2. Define modeling choices
# ---------------------------------------------------------------------------

# The multiverse varies choices that are defensible but not obvious from the
# theory alone. The dependent variable, OLS estimator, year fixed effects, and
# clustering by Gallup series are held fixed across specifications.

ownership_var <- c("owner_is_president", "owner_has_unified")

fixed_effects <- c(
  "year_only",     # year fixed effects only
  "topic",         # year fixed effects + Gallup-series fixed effects
  "topic_trend"    # year fixed effects + Gallup-series trends
)

include_lag <- c(0, 1)
first_year_interaction <- c(0, 1)
objective_controls <- c("none", "all", "strong_only")
event_controls <- c(0, 1)
political_controls <- c("none", "both")

# ---------------------------------------------------------------------------
# 3. Translate modeling choices into formulas
# ---------------------------------------------------------------------------

paste_terms <- function(terms) {
  terms <- terms[terms != ""]
  paste(terms, collapse = " + ")
}

needs_lagged_outcome <- function(include_lag, fixed_effects) {
  include_lag == 1 || fixed_effects == "year_only"
}

ownership_formula_piece <- function(ownership_var, first_year_interaction) {
  if (first_year_interaction == 1) {
    paste0(ownership_var, " * first_year_term")
  } else {
    ownership_var
  }
}

control_formula_piece <- function(
  include_lag,
  fixed_effects,
  objective_controls,
  event_controls,
  political_controls
) {
  controls <- character()

  if (needs_lagged_outcome(include_lag, fixed_effects)) {
    controls <- c(controls, "lag_pct_positive")
  }

  if (objective_controls != "none") {
    controls <- c(controls, "objective_zscore")
  }

  if (event_controls == 1) {
    controls <- c(
      controls,
      "event_defense_war",
      "event_cold_war",
      "event_crime",
      "event_race_progress",
      "event_race_police"
    )
  }

  if (political_controls == "both") {
    controls <- c(controls, "policy_mood_adjusted", "pres_approval_net")
  }

  paste_terms(controls)
}

lm_fixed_effect_piece <- function(fixed_effects) {
  if (fixed_effects == "year_only") {
    "factor(year)"
  } else if (fixed_effects == "topic") {
    "factor(year) + factor(gallup_series)"
  } else {
    "factor(year) + factor(gallup_series) + factor(gallup_series):year"
  }
}

fixest_fixed_effect_piece <- function(fixed_effects) {
  if (fixed_effects == "year_only") {
    "year"
  } else if (fixed_effects == "topic") {
    "year + gallup_series"
  } else {
    "year + gallup_series[year]"
  }
}

# ---------------------------------------------------------------------------
# 4. Build and save the specification grid
# ---------------------------------------------------------------------------

specification_grid <- expand_grid(
  ownership_var = ownership_var,
  fixed_effects = fixed_effects,
  include_lag = include_lag,
  first_year_interaction = first_year_interaction,
  objective_controls = objective_controls,
  event_controls = event_controls,
  political_controls = political_controls
) %>%
  mutate(spec_id = row_number()) %>%
  select(spec_id, everything()) %>%
  mutate(
    lag_required = mapply(needs_lagged_outcome, include_lag, fixed_effects),
    ownership_piece = mapply(
      ownership_formula_piece,
      ownership_var,
      first_year_interaction
    ),
    control_piece = mapply(
      control_formula_piece,
      include_lag,
      fixed_effects,
      objective_controls,
      event_controls,
      political_controls
    ),
    lm_fixed_effect_piece = vapply(
      fixed_effects,
      lm_fixed_effect_piece,
      character(1)
    ),
    fixest_fixed_effect_piece = vapply(
      fixed_effects,
      fixest_fixed_effect_piece,
      character(1)
    ),
    lm_rhs = mapply(
      function(ownership, controls, fixed_effects) {
        paste_terms(c(ownership, controls, fixed_effects))
      },
      ownership_piece,
      control_piece,
      lm_fixed_effect_piece
    ),
    fixest_rhs = mapply(
      function(ownership, controls) {
        paste_terms(c(ownership, controls))
      },
      ownership_piece,
      control_piece
    ),
    lm_formula = paste("pct_positive ~", lm_rhs)
  )

write.csv(
  specification_grid,
  file.path(derived_dir, "gallup_specification_grid.csv"),
  row.names = FALSE
)

cat("Gallup specification grid:", nrow(specification_grid), "specifications\n")
