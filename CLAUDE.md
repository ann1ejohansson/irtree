# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this project is

An R-based analysis pipeline for an Item Response Tree (IRTree) study of student skip behaviour in an adaptive mathematics platform. Each response is modelled as a two-stage decision: Node 1 (did the student skip?) and Node 2 (given a response, was it incorrect?). Four nested mixed-effects logistic regression models are compared.

## Running the pipeline

All scripts must be run from inside the `code/` directory, or via `source()` — the working-directory detection in `00_config.R` handles RStudio, `Rscript`, and `source()` contexts automatically.

**Simulated data (default, no proprietary data needed):**
```r
source("code/01_generate_example_data.R")  # creates data/simulated/sim_logs.Rdata
source("code/03_irtree_fit.R")             # fits all four models; sources 02 automatically
source("code/04_irtree_kfolds.R")          # 10-fold CV; sources 02 automatically
rmarkdown::render("code/05_itree_analysis.Rmd")
```

**Real data:** set `use_simulated_data <- FALSE` in `00_config.R` and update `data_path`.

Output directories (`results/models/`, `results/k_folds/`, `results/plots/`) are created automatically on first run.

## Configuration

`00_config.R` is the single source of truth — sourced automatically by every other script. Key switches:

| Variable | Default | Effect |
|---|---|---|
| `use_simulated_data` | `TRUE` | Simulated vs. real data; also changes output path prefix (`results/simulated/` vs. `results/`) |
| `min_skips` | `10` | Minimum skips per student for inclusion |
| `min_grade` / `max_grade` | `3` / `8` | Grade range filter |
| `random_seed` | `1234` | Shared seed for reproducibility |

## Pipeline architecture

```
00_config.R          ← sourced by all scripts; sets paths, loads packages, defines save_plot()
01_generate_…        ← standalone; creates sim_logs.Rdata
02_irtree_prep.R     ← sourced by 03 and 04; outputs dat_irt (long, two rows per response)
03_irtree_fit.R      ← fits models 1–4; saves .Rdata per model + model_comp.Rdata
04_irtree_kfolds.R   ← 10-fold CV; saves per-fold summaries + k_folds.Rdata
05_itree_analysis.Rmd← loads model outputs; produces all manuscript figures via save_plot()
```

**Key data object:** `dat_irt` — a `data.table` in long format with two rows per original response. The `node` column (1 or 2) distinguishes the skip decision from the accuracy decision. Node 2 rows for skipped items have `response = NA` and are silently excluded from the node 2 likelihood by `glmer`.

**Model formulas** (all use `glmer(..., family = "binomial")`):
- Model 1 (full IRTree): `response ~ 1 + (0 + factor(node) | item_id) + (0 + factor(node) | user_id)`
- Model 2 (item-constrained): `response ~ 1 + (1 | item_id) + (0 + factor(node) | user_id)`
- Model 3 (user-constrained): `response ~ 1 + (0 + factor(node) | item_id) + (1 | user_id)`
- Model 4 (unidimensional): `response ~ 1 + factor(node) + (1 | item_id) + (1 | user_id)`

## Dependencies

```r
install.packages(c("lme4", "MASS", "data.table", "ggplot2",
                   "ggExtra", "cowplot", "tidyverse", "DescTools",
                   "lmtest", "ggrepel", "truncdist"))
```

`truncdist` is optional (used in RT simulation; falls back to clipping if absent).

## Manuscript versions

The `LAK-25/`, `EDM-25/`, `EMIP-25/`, and `IOPS-24/` directories contain LaTeX source for conference submissions. These are independent of the R pipeline.
