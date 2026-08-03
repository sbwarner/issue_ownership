# Requirements

The replication was verified with R 4.5.2 on Windows.

## Required R Packages

- `dplyr`
- `tidyr`
- `ggplot2`
- `patchwork`
- `estimatr`
- `fixest`
- `lme4`

## Analysis Options

- `run_slow_gallup_inference` in `00_run_all.R`: set to `TRUE` to run the 500-draw Gallup null-curve inference.
- `bootstrap_draws` in `code/gallup_05_null_curve_inference_slow.R`: defaults to `500`.
- `pooled_maxfun` in `code/state_01_run_models.R`: defaults to `1000`; increase to `100000` for a longer optimizer search.
- `maxfun` in `code/state_02_mechanism_checks.R` and `code/state_03_robustness_checks.R`: defaults to `1000`.

From the repository root, run:

```r
source("00_run_all.R")
```
