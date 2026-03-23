## config.R
##
## Central configuration for the IRTree analysis pipeline.
## Source this file at the top of any script in the pipeline.
##
## Usage: source("config.R")

# ============================================================
# WORKING DIRECTORY
# ============================================================
# Detects the location of config.R itself and sets wd accordingly,
# so all relative paths resolve correctly regardless of how/where
# the pipeline is launched.
.get_script_dir <- function() {
  # When run via: Rscript script.R
  args     <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("--file=", "", file_arg))))
  }
  # When sourced inside RStudio
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ctx <- rstudioapi::getSourceEditorContext()
    if (!is.null(ctx) && nchar(ctx$path) > 0) {
      return(dirname(normalizePath(ctx$path)))
    }
  }
  # When called via source("config.R") from another script
  src <- tryCatch(
    dirname(normalizePath(sys.frames()[[1]]$ofile)),
    error = function(e) NULL
  )
  if (!is.null(src)) return(src)
  # Fallback: keep current wd
  getwd()
}
setwd(.get_script_dir())
rm(.get_script_dir)

# ============================================================
# DATA MODE
# ============================================================
# TRUE  = use simulated data (local testing, no access to real data)
# FALSE = use real Prowise Learn data
use_simulated_data <- TRUE

# ============================================================
# DATA PATHS
# ============================================================
if (use_simulated_data) {
  data_path   <- "data/sim_logs.Rdata"
  data_object <- "sim_logs"            # name of object restored by load()
} else {
  data_path   <- "~/research-collaboration/question-mark/new data/logs_clean.Rdata"
  data_object <- "logs_clean"
}

# ============================================================
# ANALYSIS PARAMETERS
# ============================================================

# Inclusion criterion: minimum number of skip responses per user
# (users below this threshold are excluded in irtree_prep.R)
min_skips <- 10

# K-fold cross-validation (irtree_kfolds.R)
n_folds      <- 10   # number of folds
holdout_frac <- 0.1  # proportion of responses held out per fold per node

# Reproducibility: shared seed used across all scripts
random_seed <- 1234

# ============================================================
# OUTPUT DIRECTORIES
# ============================================================
results_dir <- "results"
plots_dir   <- file.path(results_dir, "plots")
kfolds_dir  <- file.path(results_dir, "k_folds")
models_dir  <- file.path(results_dir, "models")

for (.d in c(results_dir, plots_dir, kfolds_dir, models_dir)) {
  dir.create(.d, showWarnings = FALSE, recursive = TRUE)
}
rm(.d)
