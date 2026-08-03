# Ownership PRQ: run the public replication package
#
# The default run reproduces the manuscript-facing Gallup and state analyses
# from the analysis-ready datasets in data/. The slow Gallup null-curve
# inference is skipped unless requested because it reruns the whole multiverse
# inside each bootstrap draw.

# ---------------------------------------------------------------------------
# 1. Setup
# ---------------------------------------------------------------------------

# Run with the working directory set to the repository folder.
project_dir <- getwd()

# Set to TRUE to reproduce the 500-draw Table 3 null-curve inference.
run_slow_gallup_inference <- FALSE

dir.create(file.path(project_dir, "results", "derived"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(project_dir, "results", "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(project_dir, "results", "tables"), showWarnings = FALSE, recursive = TRUE)

run_script <- function(path) {
  cat("\n--- Running", path, "---\n")
  source(file.path(project_dir, path), echo = FALSE)
}

# ---------------------------------------------------------------------------
# 2. Gallup topline analysis
# ---------------------------------------------------------------------------

run_script("code/gallup_01_define_specifications.R")
run_script("code/gallup_02_run_multiverse.R")
run_script("code/gallup_03_make_specification_curve.R")
run_script("code/gallup_04_make_summary_tables.R")

if (isTRUE(run_slow_gallup_inference)) {
  run_script("code/gallup_05_null_curve_inference_slow.R")
} else {
  cat("\nSkipped slow Gallup null-curve inference.\n")
  cat("To run Table 3 inference, set run_slow_gallup_inference <- TRUE near the top of 00_run_all.R.\n")
}

# ---------------------------------------------------------------------------
# 3. State-level survey analysis
# ---------------------------------------------------------------------------

run_script("code/state_01_run_models.R")
run_script("code/state_02_mechanism_checks.R")
run_script("code/state_03_robustness_checks.R")

# ---------------------------------------------------------------------------
# 4. Codebooks and session information
# ---------------------------------------------------------------------------

run_script("code/make_codebooks.R")

writeLines(
  capture.output(sessionInfo()),
  con = file.path(project_dir, "results", "session_info.txt")
)

cat("\nReplication run complete. Generated files are in the results folder.\n")
