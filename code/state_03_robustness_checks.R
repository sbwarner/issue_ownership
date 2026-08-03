# State-Issues Analysis: robustness checks for ownership coding and weights
#
# This script reproduces robustness checks for two modeling choices:
# 1. Giving each issue the same total influence in the pooled model.
# 2. Replacing the 0-3 ownership measure with alternate ownership codings.
#
# The checks use the analysis-ready replication dataset.

# ---------------------------------------------------------------------------
# 1. Setup
# ---------------------------------------------------------------------------

# Run with the working directory set to the repository folder.
project_dir <- getwd()

data_file <- file.path(project_dir, "data", "state_issue_analysis.csv")
tables_dir <- file.path(project_dir, "results", "tables")

dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

library(lme4)

state_data <- read.csv(data_file, stringsAsFactors = FALSE)

state_data$pid <- factor(state_data$pid)
state_data$age_group <- factor(state_data$age_group)
state_data$topic <- factor(state_data$topic)

maxfun <- 1000

ctrl <- glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = maxfun),
  calc.derivs = FALSE
)

# ---------------------------------------------------------------------------
# 2. Create robustness variables
# ---------------------------------------------------------------------------

# Topic-balanced weights keep the respondent-level survey weights but rescale
# them so ACA, budget, crime, and schools have the same total weight in the
# pooled model.
topic_weight_totals <- tapply(state_data$weight, state_data$topic, sum, na.rm = TRUE)
average_topic_total <- mean(topic_weight_totals)
state_data$topic_weight_total <- topic_weight_totals[as.character(state_data$topic)]
state_data$topic_balanced_weight <-
  state_data$weight * average_topic_total / state_data$topic_weight_total

# Binary unified ownership asks only whether the owning party has unified state
# government. All other arrangements are coded 0.
state_data$owner_unified_binary <- as.integer(state_data$own_013 == 3)
state_data$owner_unified_binary[is.na(state_data$own_013)] <- NA_integer_

# The three-point version collapses the two divided-government categories:
# 0 = non-owning unified government
# 1 = divided government or owning/non-owning governor only
# 2 = owning unified government
state_data$ownership_three_point <- NA_integer_
state_data$ownership_three_point[state_data$own_013 == 0] <- 0L
state_data$ownership_three_point[state_data$own_013 %in% c(1, 2)] <- 1L
state_data$ownership_three_point[state_data$own_013 == 3] <- 2L

# The categorical version estimates separate differences for 1, 2, and 3
# relative to 0, the non-owning unified-government reference category.
state_data$ownership_category <- factor(state_data$own_013, levels = c(0, 1, 2, 3))

# ---------------------------------------------------------------------------
# 3. Small extraction helper
# ---------------------------------------------------------------------------

coef_row <- function(fit, check, term, label, note) {
  coef_table <- summary(fit)$coef
  if (!(term %in% rownames(coef_table))) {
    return(data.frame(
      check = check,
      term = label,
      estimate = NA_real_,
      standard_error = NA_real_,
      z_value = NA_real_,
      p_value_two_tailed = NA_real_,
      n = nobs(fit),
      note = note,
      stringsAsFactors = FALSE
    ))
  }

  zval <- coef_table[term, "z value"]
  data.frame(
    check = check,
    term = label,
    estimate = coef_table[term, "Estimate"],
    standard_error = coef_table[term, "Std. Error"],
    z_value = zval,
    p_value_two_tailed = 2 * pnorm(abs(zval), lower.tail = FALSE),
    n = nobs(fit),
    note = note,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# 4. Topic-balanced weighting check
# ---------------------------------------------------------------------------

fit_topic_balanced <- glmer(
  perception ~ own_013 + cond1_z + cond2_z +
    net_gub_approve_z +
    pid * topic + gub_copartisan +
    ideo3 * topic + white * topic + black * topic + male * topic +
    bachelors * topic + age_group * topic +
    topic +
    (1 | state_year_topic),
  data = state_data,
  weights = topic_balanced_weight,
  family = binomial(),
  control = ctrl
)

# ---------------------------------------------------------------------------
# 5. Alternate ownership-coding checks
# ---------------------------------------------------------------------------

fit_binary_unified <- glmer(
  perception ~ owner_unified_binary + cond1_z + cond2_z +
    net_gub_approve_z +
    pid * topic + gub_copartisan +
    ideo3 * topic + white * topic + black * topic + male * topic +
    bachelors * topic + age_group * topic +
    topic +
    (1 | state_year_topic),
  data = state_data,
  weights = weight,
  family = binomial(),
  control = ctrl
)

fit_three_point <- glmer(
  perception ~ ownership_three_point + cond1_z + cond2_z +
    net_gub_approve_z +
    pid * topic + gub_copartisan +
    ideo3 * topic + white * topic + black * topic + male * topic +
    bachelors * topic + age_group * topic +
    topic +
    (1 | state_year_topic),
  data = state_data,
  weights = weight,
  family = binomial(),
  control = ctrl
)

fit_categorical <- glmer(
  perception ~ ownership_category + cond1_z + cond2_z +
    net_gub_approve_z +
    pid * topic + gub_copartisan +
    ideo3 * topic + white * topic + black * topic + male * topic +
    bachelors * topic + age_group * topic +
    topic +
    (1 | state_year_topic),
  data = state_data,
  weights = weight,
  family = binomial(),
  control = ctrl
)

# ---------------------------------------------------------------------------
# 6. Save robustness summary
# ---------------------------------------------------------------------------

robustness_rows <- rbind(
  coef_row(
    fit_topic_balanced,
    check = "Issue-balanced weights",
    term = "own_013",
    label = "Ownership (0-3)",
    note = "Survey weights rescaled so each issue has the same total model weight."
  ),
  coef_row(
    fit_binary_unified,
    check = "Binary unified ownership",
    term = "owner_unified_binary",
    label = "Owning party unified government",
    note = "1 = owning party has unified state government; 0 = all other arrangements."
  ),
  coef_row(
    fit_three_point,
    check = "Three-point ownership",
    term = "ownership_three_point",
    label = "Ownership category (0-2)",
    note = "0 = non-owning unified; 1 = divided/governor-only; 2 = owning unified."
  ),
  coef_row(
    fit_categorical,
    check = "Categorical ownership",
    term = "ownership_category1",
    label = "Non-owning governor only vs non-owning unified",
    note = "Reference category is 0, non-owning unified government."
  ),
  coef_row(
    fit_categorical,
    check = "Categorical ownership",
    term = "ownership_category2",
    label = "Owning governor only vs non-owning unified",
    note = "Reference category is 0, non-owning unified government."
  ),
  coef_row(
    fit_categorical,
    check = "Categorical ownership",
    term = "ownership_category3",
    label = "Owning unified vs non-owning unified",
    note = "Reference category is 0, non-owning unified government."
  )
)

write.csv(
  robustness_rows,
  file.path(tables_dir, "state_robustness_ownership_and_weights.csv"),
  row.names = FALSE
)

cat("State robustness checks written to:", tables_dir, "\n")
