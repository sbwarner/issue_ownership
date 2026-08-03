# Replication Package: Governing Party Issue Ownership and Citizen Perceptions

This repository reproduces the analyses for the Political Research Quarterly article, "Governing party issue ownership and how citizens assess conditions."

## Contents

- `00_run_all.R`: runs the replication workflow.
- `data/gallup_analysis.csv`: Gallup topline analysis dataset.
- `data/gallup_analysis_codebook.pdf`: Gallup dataset codebook.
- `data/state_issue_analysis.csv`: pooled state and local issue dataset.
- `data/state_issue_analysis_codebook.pdf`: state dataset codebook.
- `code/`: Gallup and state analysis scripts.
- `REQUIREMENTS.md`: R and package requirements.

## How To Run

Set the R working directory to the repository folder and run:

```r
source("00_run_all.R")
```

The standard run reproduces the Gallup multiverse, Figure 1, summary tables, state models, mechanism checks, robustness checks, and codebooks. Figure 1 is saved as a 600-dpi PNG and a vector PDF.

The 500-draw Gallup null-curve inference for Table 3 is skipped by default because it takes substantially longer. To include it, set `run_slow_gallup_inference <- TRUE` near the top of `00_run_all.R`.

## Expected Checks

- Gallup analysis data: 473 rows.
- Gallup specification grid: 288 specifications.
- Gallup median coefficient: about 0.591.
- Gallup positive and significant specifications: about 4.2%.
- State pooled model: N = 8,014 and ownership coefficient about 0.10.
- Appendix C1 ownership coefficients: about 0.104 excluding crime and 0.115 excluding schools.
- Appendix C2 ownership coefficients: about 0.085 for ACA, 0.213 for budget, 0.097 for crime, and -0.067 for schools.

Generated files are written to `results/`. They are not required source files and are not included in the deposited archive.

## Data Note

The state dataset includes participant IDs prefixed by survey source. Raw survey files are not redistributed in this package.
