# Gallup Analysis: estimate the multiverse
#
# This script estimates the 288 Gallup topline specifications defined in
# gallup_01_define_specifications.R.

# ---------------------------------------------------------------------------
# 1. Setup
# ---------------------------------------------------------------------------

# Run with the working directory set to the repository folder.
project_dir <- getwd()

data_file <- file.path(project_dir, "data", "gallup_analysis.csv")
derived_dir <- file.path(project_dir, "results", "derived")

dir.create(derived_dir, showWarnings = FALSE, recursive = TRUE)

library(estimatr)

gallup_data <- read.csv(data_file, stringsAsFactors = FALSE)
specification_grid <- read.csv(
  file.path(derived_dir, "gallup_specification_grid.csv"),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# 2. Helper functions for one specification
# ---------------------------------------------------------------------------

# Build the analysis sample for one specification. This is where rows are
# dropped because a requested control is missing.
make_model_data <- function(spec, data) {
  model_data <- data

  if (spec$objective_controls == "strong_only") {
    model_data$objective_zscore[model_data$match_strength != "strong"] <- NA_real_
  }

  keep_row <- !is.na(model_data$pct_positive) &
    !is.na(model_data[[spec$ownership_var]])

  if (spec$lag_required) {
    keep_row <- keep_row & !is.na(model_data$lag_pct_positive)
  }

  if (spec$objective_controls != "none") {
    keep_row <- keep_row & !is.na(model_data$objective_zscore)
  }

  if (spec$political_controls == "both") {
    keep_row <- keep_row &
      !is.na(model_data$policy_mood_adjusted) &
      !is.na(model_data$pres_approval_net)
  }

  model_data[keep_row, ]
}

# Estimate one specification and return the ownership coefficient row.
estimate_one_specification <- function(spec, data) {
  model_data <- make_model_data(spec, data)

  if (nrow(model_data) < 30) {
    return(data.frame(
      coef = NA_real_,
      se = NA_real_,
      pvalue = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      n_obs = nrow(model_data),
      r_squared = NA_real_,
      error = "Insufficient observations",
      stringsAsFactors = FALSE
    ))
  }

  tryCatch({
    fit <- lm_robust(
      as.formula(spec$lm_formula),
      data = model_data,
      clusters = model_data$gallup_series,
      se_type = "CR2"
    )

    term <- spec$ownership_var

    data.frame(
      coef = unname(coef(fit)[term]),
      se = unname(fit$std.error[term]),
      pvalue = unname(fit$p.value[term]),
      ci_lower = unname(fit$conf.low[term]),
      ci_upper = unname(fit$conf.high[term]),
      n_obs = nrow(model_data),
      r_squared = fit$r.squared,
      error = NA_character_,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      coef = NA_real_,
      se = NA_real_,
      pvalue = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      n_obs = NA_integer_,
      r_squared = NA_real_,
      error = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

# ---------------------------------------------------------------------------
# 3. Estimate all specifications
# ---------------------------------------------------------------------------

multiverse_rows <- vector("list", nrow(specification_grid))

for (i in seq_len(nrow(specification_grid))) {
  spec <- specification_grid[i, ]
  estimate <- estimate_one_specification(spec, gallup_data)
  multiverse_rows[[i]] <- cbind(spec, estimate)

  if (i %% 25 == 0) {
    cat("Completed", i, "of", nrow(specification_grid), "Gallup specifications\n")
  }
}

multiverse_results <- do.call(rbind, multiverse_rows)

# ---------------------------------------------------------------------------
# 4. Save results
# ---------------------------------------------------------------------------

write.csv(
  multiverse_results,
  file.path(derived_dir, "gallup_multiverse_results.csv"),
  row.names = FALSE
)

cat(
  "Gallup multiverse complete. Valid specifications:",
  sum(!is.na(multiverse_results$coef)),
  "\n"
)
