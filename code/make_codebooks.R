# Make PDF codebooks for the public analysis datasets

# ---------------------------------------------------------------------------
# 1. Setup
# ---------------------------------------------------------------------------

# Run with the working directory set to the repository folder.
project_dir <- getwd()

data_dir <- file.path(project_dir, "data")
state_data_file <- file.path(data_dir, "state_issue_analysis.csv")

gallup_data <- read.csv(file.path(data_dir, "gallup_analysis.csv"), stringsAsFactors = FALSE)
state_data <- read.csv(state_data_file, stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# 2. PDF writer
# ---------------------------------------------------------------------------

write_codebook_pdf <- function(entries, data, file, title, intro) {
  pdf(file, width = 11, height = 8.5)
  on.exit(dev.off(), add = TRUE)

  blank_if_missing <- function(x) {
    ifelse(is.na(x), "", x)
  }

  format_stat <- function(x) {
    ifelse(is.na(x), "", formatC(x, format = "f", digits = 3))
  }

  describe_variable <- function(entry) {
    paste(
      entry[["label"]],
      paste0("Values: ", entry[["values"]])
    )
  }

  summarize_variable <- function(variable) {
    if (!(variable %in% names(data))) {
      return(c(minimum = NA_character_, maximum = NA_character_, mean = NA_character_, missing = NA_character_))
    }

    x <- data[[variable]]
    missing_n <- sum(is.na(x) | (is.character(x) & trimws(x) == ""))

    if (is.character(x)) {
      return(c(minimum = "", maximum = "", mean = "", missing = as.character(missing_n)))
    }

    x_num <- suppressWarnings(as.numeric(x))
    if (all(is.na(x_num))) {
      return(c(minimum = "", maximum = "", mean = "", missing = as.character(missing_n)))
    }
    c(
      minimum = format_stat(min(x_num, na.rm = TRUE)),
      maximum = format_stat(max(x_num, na.rm = TRUE)),
      mean = format_stat(mean(x_num, na.rm = TRUE)),
      missing = as.character(missing_n)
    )
  }

  table_rows <- data.frame(
    variable = entries$variable,
    description = vapply(seq_len(nrow(entries)), function(i) describe_variable(entries[i, ]), character(1)),
    stringsAsFactors = FALSE
  )

  stats <- t(vapply(entries$variable, summarize_variable, character(4)))
  table_rows$minimum <- stats[, "minimum"]
  table_rows$maximum <- stats[, "maximum"]
  table_rows$mean <- stats[, "mean"]
  table_rows$missing <- stats[, "missing"]

  col_left <- c(0.04, 0.19, 0.72, 0.79, 0.86, 0.93)
  col_right <- c(0.18, 0.71, 0.78, 0.85, 0.92, 0.98)
  headers <- c("Variable", "Description", "Min", "Max", "Mean", "NAs")

  new_page <- function(page_number) {
    plot.new()
    par(mar = c(0, 0, 0, 0))
    text(0.04, 0.965, title, adj = 0, cex = 1.12, font = 2)
    text(0.98, 0.965, paste("Page", page_number), adj = 1, cex = 0.72)

    intro_lines <- unlist(strwrap(intro, width = 145))
    y_intro <- 0.925
    for (line in intro_lines) {
      text(0.04, y_intro, line, adj = 0, cex = 0.68)
      y_intro <- y_intro - 0.025
    }

    y_header <- y_intro - 0.015
    rect(0.04, y_header - 0.027, 0.98, y_header + 0.012, col = "gray92", border = "gray60")
    for (j in seq_along(headers)) {
      text(col_left[j] + 0.004, y_header - 0.008, headers[j], adj = 0, cex = 0.66, font = 2)
      segments(col_right[j], y_header + 0.012, col_right[j], y_header - 0.027, col = "gray70")
    }
    y_header - 0.045
  }

  page <- 1
  y <- new_page(page)
  row_gap <- 0.010
  line_height <- 0.019

  for (i in seq_len(nrow(table_rows))) {
    desc_lines <- unlist(strwrap(table_rows$description[i], width = 78))
    var_lines <- unlist(strwrap(table_rows$variable[i], width = 20))
    row_lines <- max(length(desc_lines), length(var_lines), 1)
    row_height <- row_lines * line_height + row_gap

    if (y - row_height < 0.02) {
      page <- page + 1
      y <- new_page(page)
    }

    y_top <- y + 0.006
    y_bottom <- y - row_height + 0.006
    rect(0.04, y_bottom, 0.98, y_top, border = "gray82")
    for (j in seq_along(col_right)) {
      segments(col_right[j], y_top, col_right[j], y_bottom, col = "gray88")
    }

    for (k in seq_along(var_lines)) {
      text(col_left[1] + 0.004, y - (k - 1) * line_height, var_lines[k], adj = 0, cex = 0.61, font = 2)
    }

    for (k in seq_along(desc_lines)) {
      text(col_left[2] + 0.004, y - (k - 1) * line_height, desc_lines[k], adj = 0, cex = 0.56)
    }

    text(col_left[3] + 0.004, y, blank_if_missing(table_rows$minimum[i]), adj = 0, cex = 0.58)
    text(col_left[4] + 0.004, y, blank_if_missing(table_rows$maximum[i]), adj = 0, cex = 0.58)
    text(col_left[5] + 0.004, y, blank_if_missing(table_rows$mean[i]), adj = 0, cex = 0.58)
    text(col_left[6] + 0.004, y, blank_if_missing(table_rows$missing[i]), adj = 0, cex = 0.58)

    y <- y - row_height
  }
}

# ---------------------------------------------------------------------------
# 3. Gallup codebook
# ---------------------------------------------------------------------------

gallup_entries <- data.frame(
  variable = c(
    "year",
    "topic",
    "gallup_series",
    "series_id",
    "pct_positive",
    "lag_pct_positive",
    "objective_zscore",
    "match_strength",
    "party_owns_issue",
    "president_party",
    "unified_govt",
    "owner_is_president",
    "owner_has_unified",
    "first_year_term",
    "policy_mood_adjusted",
    "pres_approval_net",
    "event_defense_war",
    "event_cold_war",
    "event_crime",
    "event_race_progress",
    "event_race_police"
  ),
  type = c(
    "integer", "character", "character", "integer", "numeric", "numeric",
    "numeric", "character", "character", "character", "integer", "integer",
    "integer", "integer", "numeric", "numeric", "integer", "integer",
    "integer", "integer", "integer"
  ),
  label = c(
    "Survey year.",
    "Issue topic used to group related Gallup series.",
    "Specific Gallup Trends question series.",
    "Numeric identifier for Gallup series.",
    "Percent of respondents giving the positive-coded response.",
    "Lagged percent positive within Gallup series.",
    "Z-scored objective condition measure, oriented so higher values usually indicate more favorable real-world conditions.",
    "Strength of match between the objective measure and the Gallup question.",
    "Party coded as owning the issue.",
    "Party of the president in that year.",
    "Indicator for unified federal government under the president's party.",
    "Indicator that the issue-owning party holds the presidency.",
    "Indicator that the issue-owning party holds unified federal government.",
    "Indicator for the first year of a presidential administration.",
    "Policy mood z-score adjusted so higher values favor the issue-owning party's position.",
    "Annual net presidential approval.",
    "Defense war-period event control.",
    "Cold War end-period event control.",
    "Crime event-period control.",
    "Civil Rights era race-relations event control.",
    "Race-relations policing event control."
  ),
  values = c(
    "1956-2025.",
    "Nine issue topics.",
    "Eighteen Gallup Trends series.",
    "1-18.",
    "0-100.",
    "0-100 or missing for first series observation.",
    "Standardized within source measure; missing when no objective measure is available.",
    "strong, weak, or missing.",
    "D or R.",
    "D or R.",
    "0/1.",
    "0/1.",
    "0/1.",
    "0/1.",
    "Continuous z-score.",
    "Approval minus disapproval.",
    "0/1.",
    "0/1.",
    "0/1.",
    "0/1.",
    "0/1."
  ),
  stringsAsFactors = FALSE
)

write_codebook_pdf(
  gallup_entries,
  gallup_data,
  file.path(data_dir, "gallup_analysis_codebook.pdf"),
  "Codebook: gallup_analysis.csv",
  "This dataset reproduces the Gallup topline multiverse analysis. Each row is an annual Gallup Trends topline observation for one question series."
)

# ---------------------------------------------------------------------------
# 4. State issue codebook
# ---------------------------------------------------------------------------

state_entries <- data.frame(
  variable = c(
    "participant_id",
    "survey_source",
    "topic",
    "state",
    "year",
    "state_year_topic",
    "perception",
    "owner_party",
    "own_013",
    "cond1_z",
    "cond2_z",
    "condition_index_z",
    "net_gub_approve_z",
    "condition_1",
    "condition_2",
    "pid",
    "gub_copartisan",
    "owning_party_governor",
    "copartisan_owned_governor",
    "ideo3",
    "white",
    "black",
    "male",
    "bachelors",
    "age_group",
    "weight"
  ),
  type = c(
    "character", "character", "character", "character", "integer",
    "character", "integer", "character", "integer", "numeric", "numeric",
    "numeric", "numeric", "character", "character", "factor/integer", "integer",
    "integer", "integer", "integer", "integer", "integer", "integer",
    "integer", "factor/integer", "numeric"
  ),
  label = c(
    "Stable participant identifier, prefixed by source survey.",
    "Original survey source.",
    "Issue area in the pooled state analysis.",
    "Respondent state abbreviation.",
    "Survey year.",
    "Random-effect grouping variable for state-year-topic.",
    "Positive assessment of state or local conditions.",
    "Party that owns the issue.",
    "Ordinal governing-party ownership measure.",
    "First z-scored objective/contextual condition measure.",
    "Second z-scored objective/contextual condition measure.",
    "Within-issue z-scored average of cond1_z and cond2_z.",
    "Z-scored net gubernatorial approval.",
    "Name of the first condition measure.",
    "Name of the second condition measure.",
    "Three-category party identification.",
    "Respondent shares the governor's party.",
    "Owning party holds the governorship.",
    "Respondent is copartisan with an owning-party governor.",
    "Three-category ideology.",
    "Respondent coded white.",
    "Respondent coded Black.",
    "Respondent coded male.",
    "Respondent has a bachelor's degree or higher.",
    "Three-category age group.",
    "Survey weight."
  ),
  values = c(
    "Source prefix plus source respondent ID where available.",
    "kff16_aca, kff17_aca, kff19_aca, abc02_budget, abc03_budget, pew11_budget, ces20_was_crime, ces20_was_schools.",
    "ACA, Budget, Crime, Schools.",
    "Two-letter postal abbreviation.",
    "2002, 2003, 2011, 2016, 2017, 2019, 2020.",
    "State-year-topic text label.",
    "1 = positive assessment, 0 = not positive.",
    "D or R.",
    "0 = non-owning party unified control, 1 = non-owning governor, 2 = owning governor, 3 = owning party unified control.",
    "Continuous z-score.",
    "Continuous z-score.",
    "Continuous z-score.",
    "Continuous z-score.",
    "Measure name.",
    "Measure name.",
    "1 = Democrat/lean Democrat, 2 = independent/other, 3 = Republican/lean Republican.",
    "0/1.",
    "0/1.",
    "0/1.",
    "1 = liberal, 2 = moderate/unknown, 3 = conservative.",
    "0/1.",
    "0/1.",
    "0/1.",
    "0/1.",
    "1 = 30 or younger, 2 = 31-60, 3 = over 60.",
    "Numeric survey weight."
  ),
  stringsAsFactors = FALSE
)

write_codebook_pdf(
  state_entries,
  state_data,
  file.path(data_dir, "state_issue_analysis_codebook.pdf"),
  "Codebook: state_issue_analysis.csv",
  "This dataset reproduces the pooled and issue-specific state-level survey analyses. Each row is a respondent-issue observation with the variables needed for Tables 5, C1, C2, and the copartisan mechanism checks."
)

cat("PDF codebooks written to:", data_dir, "\n")
