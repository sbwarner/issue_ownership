# State-Issues Analysis: Table 5 and Appendix Tables C1-C2
#
# This script starts from the analysis-ready state_issue_analysis.csv file.
# It estimates the pooled manuscript model, two holdout checks, and the 
# issue-specific models.

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

# The default favors a practical runtime. Increase to 100000 for a longer
# optimizer search.
pooled_maxfun <- 1000

ctrl <- glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = pooled_maxfun),
  calc.derivs = FALSE
)

# ---------------------------------------------------------------------------
# 2. Small formatting helpers
# ---------------------------------------------------------------------------

fmt <- function(x, d = 3) {
  formatC(x, format = "f", digits = d)
}

stars_2tail <- function(zval) {
  p <- 2 * pnorm(abs(zval), lower.tail = FALSE)
  stars <- rep("", length(p))
  stars[p < .05] <- "*"
  stars[p < .01] <- "**"
  stars
}

stars_1tail_positive <- function(zval) {
  p <- ifelse(
    zval >= 0,
    pnorm(zval, lower.tail = FALSE),
    pnorm(zval, lower.tail = TRUE)
  )
  stars <- rep("", length(p))
  stars[p < .10] <- "^"
  stars[p < .05] <- "*"
  stars[p < .01] <- "**"
  stars
}

model_cell <- function(fit, term, stars = stars_2tail) {
  coef_table <- summary(fit)$coef
  if (!(term %in% rownames(coef_table))) {
    return("--")
  }
  est <- coef_table[term, "Estimate"]
  se <- coef_table[term, "Std. Error"]
  zval <- coef_table[term, "z value"]
  paste0(fmt(est), stars(zval), " (", fmt(se), ")")
}

model_stats <- function(fit, include_re = FALSE) {
  out <- c(
    N = as.character(nobs(fit)),
    AIC = fmt(AIC(fit), d = 1),
    BIC = fmt(BIC(fit), d = 1)
  )

  if (include_re) {
    out <- c(out, RE_variance = fmt(as.numeric(VarCorr(fit)[[1]][1]), d = 3))
  } else {
    out <- c(out, logLik = fmt(as.numeric(logLik(fit)), d = 1))
  }

  out
}

# ---------------------------------------------------------------------------
# 3. Pooled model for manuscript Table 5
# ---------------------------------------------------------------------------

# The ownership measure runs from 0 to 3:
# 0 = non-owning party unified government
# 1 = non-owning party governor only
# 2 = owning party governor only
# 3 = owning party unified government
#
# Objective conditions are standardized so that higher values mean better
# conditions from the public's perspective.

table5_formula <- perception ~ own_013 + cond1_z + cond2_z +
  net_gub_approve_z +
  pid * topic + gub_copartisan +
  ideo3 * topic + white * topic + black * topic + male * topic +
  bachelors * topic + age_group * topic +
  topic +
  (1 | state_year_topic)

fit_table5 <- glmer(
  table5_formula,
  data = state_data,
  weights = weight,
  family = binomial(),
  control = ctrl
)

# Appendix C1 checks whether the pooled result is driven by the two 2020 CES
# outcomes, which have the least direct state-year contextual variation.
fit_excl_crime <- glmer(
  table5_formula,
  data = state_data[state_data$topic != "Crime", ],
  weights = weight,
  family = binomial(),
  control = ctrl
)

fit_excl_schools <- glmer(
  table5_formula,
  data = state_data[state_data$topic != "Schools", ],
  weights = weight,
  family = binomial(),
  control = ctrl
)

pooled_rows <- data.frame(
  term = c(
    "Governing party ownership (0-3)",
    "Conditions measure 1 (z)",
    "Conditions measure 2 (z)",
    "Net gubernatorial approval (z)",
    "Copartisan to governor",
    "",
    "Topic FE: Budget",
    "Topic FE: Crime",
    "Topic FE: Schools",
    "",
    "N",
    "AIC",
    "BIC",
    "RE variance (state-year-topic)"
  ),
  Table5 = c(
    model_cell(fit_table5, "own_013"),
    model_cell(fit_table5, "cond1_z"),
    model_cell(fit_table5, "cond2_z"),
    model_cell(fit_table5, "net_gub_approve_z"),
    model_cell(fit_table5, "gub_copartisan"),
    "",
    model_cell(fit_table5, "topicBudget"),
    model_cell(fit_table5, "topicCrime"),
    model_cell(fit_table5, "topicSchools"),
    "",
    model_stats(fit_table5, include_re = TRUE)
  ),
  Excl_Crime = c(
    model_cell(fit_excl_crime, "own_013"),
    model_cell(fit_excl_crime, "cond1_z"),
    model_cell(fit_excl_crime, "cond2_z"),
    model_cell(fit_excl_crime, "net_gub_approve_z"),
    model_cell(fit_excl_crime, "gub_copartisan"),
    "",
    model_cell(fit_excl_crime, "topicBudget"),
    model_cell(fit_excl_crime, "topicCrime"),
    model_cell(fit_excl_crime, "topicSchools"),
    "",
    model_stats(fit_excl_crime, include_re = TRUE)
  ),
  Excl_Schools = c(
    model_cell(fit_excl_schools, "own_013"),
    model_cell(fit_excl_schools, "cond1_z"),
    model_cell(fit_excl_schools, "cond2_z"),
    model_cell(fit_excl_schools, "net_gub_approve_z"),
    model_cell(fit_excl_schools, "gub_copartisan"),
    "",
    model_cell(fit_excl_schools, "topicBudget"),
    model_cell(fit_excl_schools, "topicCrime"),
    model_cell(fit_excl_schools, "topicSchools"),
    "",
    model_stats(fit_excl_schools, include_re = TRUE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  pooled_rows,
  file.path(tables_dir, "state_table5_and_c1_pooled_models.csv"),
  row.names = FALSE
)

# ---------------------------------------------------------------------------
# 4. Issue-specific models for Appendix Table C2
# ---------------------------------------------------------------------------

# Each model uses the same respondent controls as the pooled model, but without
# topic interactions because each analysis is now within a single issue area.

issue_formula <- perception ~ own_013 + cond1_z + cond2_z +
  net_gub_approve_z +
  pid + gub_copartisan +
  ideo3 + white + black + male + bachelors + age_group +
  (1 | state_year_topic)

aca_data <- state_data[state_data$topic == "ACA", ]
budget_data <- state_data[state_data$topic == "Budget", ]
crime_data <- state_data[state_data$topic == "Crime", ]
schools_data <- state_data[state_data$topic == "Schools", ]

fit_aca <- glmer(
  issue_formula,
  data = aca_data,
  weights = weight,
  family = binomial(),
  control = ctrl
)

fit_budget <- glmer(
  issue_formula,
  data = budget_data,
  weights = weight,
  family = binomial(),
  control = ctrl
)

fit_crime <- glmer(
  issue_formula,
  data = crime_data,
  weights = weight,
  family = binomial(),
  control = ctrl
)

fit_schools <- glmer(
  issue_formula,
  data = schools_data,
  weights = weight,
  family = binomial(),
  control = ctrl
)

issue_rows <- data.frame(
  term = c(
    "Ownership (0 non-own unified, 1 non-own gov, 2 own gov, 3 own unified)",
    "Conditions #1 (z)",
    "Conditions #2 (z)",
    "Net gubernatorial approval (z)",
    "Gov copartisan",
    "N",
    "AIC",
    "BIC",
    "logLik"
  ),
  ACA = c(
    model_cell(fit_aca, "own_013", stars = stars_1tail_positive),
    model_cell(fit_aca, "cond1_z", stars = stars_1tail_positive),
    model_cell(fit_aca, "cond2_z", stars = stars_1tail_positive),
    model_cell(fit_aca, "net_gub_approve_z", stars = stars_1tail_positive),
    model_cell(fit_aca, "gub_copartisan", stars = stars_1tail_positive),
    model_stats(fit_aca, include_re = FALSE)
  ),
  Budget = c(
    model_cell(fit_budget, "own_013", stars = stars_1tail_positive),
    model_cell(fit_budget, "cond1_z", stars = stars_1tail_positive),
    model_cell(fit_budget, "cond2_z", stars = stars_1tail_positive),
    model_cell(fit_budget, "net_gub_approve_z", stars = stars_1tail_positive),
    model_cell(fit_budget, "gub_copartisan", stars = stars_1tail_positive),
    model_stats(fit_budget, include_re = FALSE)
  ),
  Crime = c(
    model_cell(fit_crime, "own_013", stars = stars_1tail_positive),
    model_cell(fit_crime, "cond1_z", stars = stars_1tail_positive),
    model_cell(fit_crime, "cond2_z", stars = stars_1tail_positive),
    model_cell(fit_crime, "net_gub_approve_z", stars = stars_1tail_positive),
    model_cell(fit_crime, "gub_copartisan", stars = stars_1tail_positive),
    model_stats(fit_crime, include_re = FALSE)
  ),
  Schools = c(
    model_cell(fit_schools, "own_013", stars = stars_1tail_positive),
    model_cell(fit_schools, "cond1_z", stars = stars_1tail_positive),
    model_cell(fit_schools, "cond2_z", stars = stars_1tail_positive),
    model_cell(fit_schools, "net_gub_approve_z", stars = stars_1tail_positive),
    model_cell(fit_schools, "gub_copartisan", stars = stars_1tail_positive),
    model_stats(fit_schools, include_re = FALSE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  issue_rows,
  file.path(tables_dir, "state_table_c2_issue_models.csv"),
  row.names = FALSE
)

cat("State model outputs written to:", tables_dir, "\n")
cat("Table 5 N:", nobs(fit_table5), "\n")
cat("Table 5 ownership coefficient:", fmt(summary(fit_table5)$coef["own_013", "Estimate"]), "\n")
