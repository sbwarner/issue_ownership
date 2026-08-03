# Gallup Analysis: slow null-curve inference for Table 3
#
# What this script does:
# 1. Reads the multiverse results from gallup_02_run_multiverse.R.
# 2. Removes the estimated ownership effect from each specification's outcome.
# 3. Bootstraps those null outcomes to see how unusual the observed curve is.
#
# The script is slow because each bootstrap draw reruns all valid Gallup
# specifications. The default 500 draws reproduce the manuscript Table 3 check.

# ---------------------------------------------------------------------------
# 1. Setup
# ---------------------------------------------------------------------------

# Run with the working directory set to the repository folder.
project_dir <- getwd()

data_file <- file.path(project_dir, "data", "gallup_analysis.csv")
derived_dir <- file.path(project_dir, "results", "derived")
tables_dir <- file.path(project_dir, "results", "tables")

dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

library(fixest)

setFixest_notes(FALSE)

bootstrap_draws <- 500
set.seed(42)

gallup_data <- read.csv(data_file, stringsAsFactors = FALSE)

# The specification grid already contains the model formula pieces created in
# gallup_01_define_specifications.R. Keeping those pieces there avoids repeating
# all of the specification-building logic in this slow inference script.
specification_grid <- read.csv(
  file.path(derived_dir, "gallup_specification_grid.csv"),
  stringsAsFactors = FALSE
)

# The observed multiverse results are the starting point for the null curve.
# We use the observed ownership coefficient from each valid specification.
multiverse_results <- read.csv(
  file.path(derived_dir, "gallup_multiverse_results.csv"),
  stringsAsFactors = FALSE
)

observed_results <- multiverse_results[
  !is.na(multiverse_results$coef),
  c("spec_id", "ownership_var", "coef", "se", "pvalue")
]

# ---------------------------------------------------------------------------
# 2. Create one null outcome for each specification
# ---------------------------------------------------------------------------

# For each specification, y_star is the original Gallup topline outcome after
# subtracting that specification's estimated ownership effect:
#
#   y_star = pct_positive - b_ownership * ownership_variable
#
# If the original ownership estimate is positive, this asks what the curve
# would look like in repeated samples where that estimated ownership association
# had been removed.

data_with_null_outcomes <- gallup_data

for (i in seq_len(nrow(specification_grid))) {
  spec <- specification_grid[i, ]
  ystar_col <- paste0("y_star_", spec$spec_id)
  b_hat <- observed_results$coef[observed_results$spec_id == spec$spec_id]

  if (length(b_hat) == 0 || is.na(b_hat)) {
    data_with_null_outcomes[[ystar_col]] <- NA_real_
  } else {
    data_with_null_outcomes[[ystar_col]] <-
      data_with_null_outcomes$pct_positive -
      b_hat * data_with_null_outcomes[[spec$ownership_var]]
  }
}

# ---------------------------------------------------------------------------
# 3. Small helpers for the bootstrap
# ---------------------------------------------------------------------------

# Apply the same sample restrictions used in gallup_02, but with a y_star
# outcome column in place of pct_positive.
make_bootstrap_model_data <- function(spec, boot_data, ystar_col) {
  model_data <- boot_data

  if (spec$objective_controls == "strong_only") {
    model_data$objective_zscore[model_data$match_strength != "strong"] <- NA_real_
  }

  keep_row <- !is.na(model_data[[ystar_col]]) &
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

# Estimate one specification on one bootstrap sample. Formula pieces come from
# the specification grid, so the code here can focus on the null outcome.
estimate_bootstrap_specification <- function(spec, boot_data) {
  ystar_col <- paste0("y_star_", spec$spec_id)
  model_data <- make_bootstrap_model_data(spec, boot_data, ystar_col)

  if (nrow(model_data) < 30) {
    return(data.frame(
      spec_id = spec$spec_id,
      coef = NA_real_,
      se = NA_real_,
      pvalue = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  model_formula <- as.formula(
    paste(
      ystar_col,
      "~",
      spec$fixest_rhs,
      "|",
      spec$fixest_fixed_effect_piece
    )
  )

  tryCatch({
    fit <- feols(
      model_formula,
      data = model_data,
      cluster = ~gallup_series,
      lean = TRUE
    )

    term <- spec$ownership_var

    data.frame(
      spec_id = spec$spec_id,
      coef = unname(coef(fit)[term]),
      se = unname(se(fit)[term]),
      pvalue = unname(pvalue(fit)[term]),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      spec_id = spec$spec_id,
      coef = NA_real_,
      se = NA_real_,
      pvalue = NA_real_,
      stringsAsFactors = FALSE
    )
  })
}

# The manuscript Table 3 reports three features of a specification curve:
# median coefficient, share positive and significant, and median z-statistic.
summarize_curve <- function(results) {
  valid_results <- results[!is.na(results$coef) & !is.na(results$se), ]

  data.frame(
    median_coef = median(valid_results$coef, na.rm = TRUE),
    share_pos_sig = mean(
      valid_results$coef > 0 & valid_results$pvalue < .05,
      na.rm = TRUE
    ),
    median_zscore = median(valid_results$coef / valid_results$se, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# 4. Bootstrap null curves
# ---------------------------------------------------------------------------

valid_spec_ids <- observed_results$spec_id
observed_stats <- summarize_curve(observed_results)
null_rows <- vector("list", bootstrap_draws)

for (draw in seq_len(bootstrap_draws)) {
  boot_index <- sample(seq_len(nrow(data_with_null_outcomes)), replace = TRUE)
  boot_data <- data_with_null_outcomes[boot_index, ]
  boot_results <- list()

  for (i in seq_len(nrow(specification_grid))) {
    spec <- specification_grid[i, ]

    if (spec$spec_id %in% valid_spec_ids) {
      boot_results[[length(boot_results) + 1]] <-
        estimate_bootstrap_specification(spec, boot_data)
    }
  }

  draw_stats <- summarize_curve(do.call(rbind, boot_results))
  draw_stats$draw <- draw
  null_rows[[draw]] <- draw_stats[, c("draw", "median_coef", "share_pos_sig", "median_zscore")]

  if (draw %% 25 == 0 || draw == 1) {
    cat("Completed Gallup null draw", draw, "of", bootstrap_draws, "\n")
  }
}

null_stats <- do.call(rbind, null_rows)

# ---------------------------------------------------------------------------
# 5. Save Table 3 inference results
# ---------------------------------------------------------------------------

table3 <- data.frame(
  statistic = c("median_coef", "share_pos_sig", "median_zscore"),
  observed = c(
    observed_stats$median_coef,
    observed_stats$share_pos_sig,
    observed_stats$median_zscore
  ),
  null_mean = c(
    mean(null_stats$median_coef, na.rm = TRUE),
    mean(null_stats$share_pos_sig, na.rm = TRUE),
    mean(null_stats$median_zscore, na.rm = TRUE)
  ),
  null_median = c(
    median(null_stats$median_coef, na.rm = TRUE),
    median(null_stats$share_pos_sig, na.rm = TRUE),
    median(null_stats$median_zscore, na.rm = TRUE)
  ),
  null_sd = c(
    sd(null_stats$median_coef, na.rm = TRUE),
    sd(null_stats$share_pos_sig, na.rm = TRUE),
    sd(null_stats$median_zscore, na.rm = TRUE)
  ),
  pvalue = c(
    mean(null_stats$median_coef >= observed_stats$median_coef, na.rm = TRUE),
    mean(null_stats$share_pos_sig >= observed_stats$share_pos_sig, na.rm = TRUE),
    mean(null_stats$median_zscore >= observed_stats$median_zscore, na.rm = TRUE)
  ),
  n_bootstrap = bootstrap_draws,
  n_specs = nrow(observed_results),
  stringsAsFactors = FALSE
)

write.csv(
  null_stats,
  file.path(derived_dir, "gallup_null_curve_draw_statistics.csv"),
  row.names = FALSE
)

write.csv(
  table3,
  file.path(tables_dir, "gallup_table3_null_curve_inference.csv"),
  row.names = FALSE
)

cat("Saved Gallup null-curve inference table to:", tables_dir, "\n")
