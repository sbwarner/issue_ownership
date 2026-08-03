# State-Issues Analysis: copartisan mechanism checks
#
# These checks address whether the ownership association is concentrated among
# respondents who share the governor's party.

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

model_cell <- function(fit, term) {
  coef_table <- summary(fit)$coef
  if (!(term %in% rownames(coef_table))) {
    return("--")
  }
  est <- coef_table[term, "Estimate"]
  se <- coef_table[term, "Std. Error"]
  zval <- coef_table[term, "z value"]
  paste0(fmt(est), stars_2tail(zval), " (", fmt(se), ")")
}

model_stats <- function(fit) {
  c(
    N = as.character(nobs(fit)),
    AIC = fmt(AIC(fit), d = 1),
    BIC = fmt(BIC(fit), d = 1),
    logLik = fmt(as.numeric(logLik(fit)), d = 1)
  )
}

# ---------------------------------------------------------------------------
# 3. Ownership x governor-copartisan check
# ---------------------------------------------------------------------------

fit_owner_x_copartisan <- glmer(
  perception ~ own_013 * gub_copartisan +
    cond1_z + cond2_z + net_gub_approve_z +
    pid * topic +
    ideo3 * topic + white * topic + black * topic + male * topic +
    bachelors * topic + age_group * topic +
    topic +
    (1 | state_year_topic),
  data = state_data,
  weights = weight,
  family = binomial(),
  control = ctrl
)

owner_x_copartisan <- data.frame(
  term = c(
    "Ownership (0-3)",
    "Gov copartisan",
    "Ownership x gov copartisan",
    "Conditions #1 (z)",
    "Conditions #2 (z)",
    "Net gubernatorial approval (z)",
    "N",
    "AIC",
    "BIC",
    "logLik"
  ),
  Pooled = c(
    model_cell(fit_owner_x_copartisan, "own_013"),
    model_cell(fit_owner_x_copartisan, "gub_copartisan"),
    model_cell(fit_owner_x_copartisan, "own_013:gub_copartisan"),
    model_cell(fit_owner_x_copartisan, "cond1_z"),
    model_cell(fit_owner_x_copartisan, "cond2_z"),
    model_cell(fit_owner_x_copartisan, "net_gub_approve_z"),
    model_stats(fit_owner_x_copartisan)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  owner_x_copartisan,
  file.path(tables_dir, "state_mechanism_ownership_x_governor_copartisan.csv"),
  row.names = FALSE
)

# ---------------------------------------------------------------------------
# 4. Ownership x governor-copartisan x condition-index check
# ---------------------------------------------------------------------------

fit_three_way <- glmer(
  perception ~ own_013 * gub_copartisan * condition_index_z +
    net_gub_approve_z +
    pid * topic +
    ideo3 * topic + white * topic + black * topic + male * topic +
    bachelors * topic + age_group * topic +
    topic +
    (1 | state_year_topic),
  data = state_data,
  weights = weight,
  family = binomial(),
  control = ctrl
)

three_way <- data.frame(
  term = c(
    "Ownership (0-3)",
    "Gov copartisan",
    "Condition index (z)",
    "Net gubernatorial approval (z)",
    "Ownership x gov copartisan",
    "Ownership x condition index",
    "Gov copartisan x condition index",
    "Ownership x gov copartisan x condition index",
    "N",
    "AIC",
    "BIC",
    "logLik"
  ),
  Pooled = c(
    model_cell(fit_three_way, "own_013"),
    model_cell(fit_three_way, "gub_copartisan"),
    model_cell(fit_three_way, "condition_index_z"),
    model_cell(fit_three_way, "net_gub_approve_z"),
    model_cell(fit_three_way, "own_013:gub_copartisan"),
    model_cell(fit_three_way, "own_013:condition_index_z"),
    model_cell(fit_three_way, "gub_copartisan:condition_index_z"),
    model_cell(fit_three_way, "own_013:gub_copartisan:condition_index_z"),
    model_stats(fit_three_way)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  three_way,
  file.path(tables_dir, "state_mechanism_three_way_ownership_x_copartisan_x_condition.csv"),
  row.names = FALSE
)

# ---------------------------------------------------------------------------
# 5. Copartisan-only ownership x condition-index check
# ---------------------------------------------------------------------------

copartisan_data <- state_data[state_data$gub_copartisan == 1, ]

fit_copartisan_only <- glmer(
  perception ~ own_013 * condition_index_z +
    net_gub_approve_z +
    pid * topic +
    ideo3 * topic + white * topic + black * topic + male * topic +
    bachelors * topic + age_group * topic +
    topic +
    (1 | state_year_topic),
  data = copartisan_data,
  weights = weight,
  family = binomial(),
  control = ctrl
)

copartisan_only <- data.frame(
  term = c(
    "Ownership (0-3)",
    "Condition index (z)",
    "Net gubernatorial approval (z)",
    "Ownership x condition index",
    "N",
    "AIC",
    "BIC",
    "logLik"
  ),
  Copartisans = c(
    model_cell(fit_copartisan_only, "own_013"),
    model_cell(fit_copartisan_only, "condition_index_z"),
    model_cell(fit_copartisan_only, "net_gub_approve_z"),
    model_cell(fit_copartisan_only, "own_013:condition_index_z"),
    model_stats(fit_copartisan_only)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  copartisan_only,
  file.path(tables_dir, "state_mechanism_copartisan_only_ownership_x_condition.csv"),
  row.names = FALSE
)

cat("Mechanism-check outputs written to:", tables_dir, "\n")
