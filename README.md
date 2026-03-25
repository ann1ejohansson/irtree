# IRTree Analysis of Skip Behaviour in an Adaptive Learning Platform 

## 🌳

This repository contains the analysis code for a study of student skip behaviour in [Prowise Learn](https://www.prowise.com/en/learn/), an adaptive mathematics platform used in Dutch primary schools. Students completing number-sequence exercises can bypass any item by clicking a question mark (?) button. We model this skip decision jointly with response accuracy using an **Item Response Tree (IRTree)** framework.

## Research Questions

1. Are skip propensity and response accuracy empirically distinguishable latent traits — or do students who skip more simply perform worse?
2. How are individual skip propensity and accuracy related across students and items?

## Model

Each response is modelled as a two-stage decision process:

- **Node 1** — did the student skip? (skip propensity, θ₁)
- **Node 2** — given a response, was it incorrect? (error propensity / inverse ability, θ₂)

This is implemented as a mixed-effects logistic regression on long-format data (one row per response per node), using `glmer` from the `lme4` package. Four nested models are compared to test whether node-specific random effects are warranted for students, items, both, or neither.

## Repository Structure

```
code/
├── 00_config.R                  # Central configuration — sourced by all scripts
├── 01_generate_example_data.R   # Simulate synthetic data for local testing
├── 02_irtree_prep.R             # Reshape data to long format; filter users
├── 03_irtree_fit.R              # Fit and compare four IRTree models
├── 04_irtree_kfolds.R           # 10-fold cross-validation robustness check
├── 05_itree_analysis.Rmd        # Results and figures
└── data/
    └── simulated/               # Output of 01_generate_example_data.R
results/
    ├── models/                  # Fitted model objects and summaries
    ├── k_folds/                 # Per-fold cross-validation output
    └── plots/                   # Exported figures
```

Output directories are created automatically on first run — no manual setup needed.

## Getting Started

### Dependencies

```r
install.packages(c("lme4", "MASS", "data.table", "ggplot2",
                   "ggExtra", "cowplot", "tidyverse", "DescTools",
                   "lmtest", "truncdist"))
```

### Running with Simulated Data

The repository ships with a data simulator so the full pipeline can be run without access to the proprietary Prowise Learn dataset. To use it, make sure `use_simulated_data <- TRUE` in `00_config.R` (this is the default), then run the scripts in order:

```r
source("code/01_generate_example_data.R")  # generates data/simulated/sim_logs.Rdata
source("code/03_irtree_fit.R")             # fits all four models
source("code/04_irtree_kfolds.R")          # runs 10-fold cross-validation
rmarkdown::render("code/05_itree_analysis.Rmd")
```

Scripts 03 and 04 source `02_irtree_prep.R` automatically, so you do not need to run it separately.

### Running with Real Data

1. Set `use_simulated_data <- FALSE` in `00_config.R`.
2. Update `data_path` to point to your copy of `logs_clean.Rdata` and confirm that `data_object` matches the R object name inside that file.
3. Run scripts 03–05 as above (step 01 is not needed).

The real dataset is proprietary and not included in this repository.

## Simulated Data

`01_generate_example_data.R` generates a synthetic dataset of 500 students × ~100 trials that mirrors the structure of the real Prowise Learn data. Person parameters (θ₁, θ₂) are drawn from a bivariate normal with a moderate positive correlation (ρ = 0.35), and item parameters vary by difficulty level (Easy / Medium / Hard). Response times are drawn from a truncated log-normal, with skips faster than correct responses, and correct responses faster than errors. A sanity-check summary is printed on completion.

## Configuration

All shared settings live in `00_config.R` and are sourced automatically:

| Parameter | Default | Description |
|---|---|---|
| `use_simulated_data` | `TRUE` | Switch between simulated and real data |
| `random_seed` | `1234` | Shared seed for reproducibility |
| `min_skips` | `10` | Minimum skip responses required per student |
| `n_folds` | `10` | Number of cross-validation folds |
| `holdout_frac` | `0.1` | Fraction of responses held out per fold |

## Output

`03_irtree_fit.R` saves four model objects and a model comparison table (`model_comp.Rdata`) to `results/models/`. 
`04_irtree_kfolds.R` saves per-fold summaries and a combined `k_folds.Rdata` to `results/k_folds/`. 
Figures are saved to `results/plots/` via `save_plot()`, defined in `00_config.R`.