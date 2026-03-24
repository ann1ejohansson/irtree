## 02_irtree_prep.R
##
## Loads and reshapes the response data into the long format required by the
## IRTree model. Sourced automatically by 03_irtree_fit.R and 04_irtree_kfolds.R.
##
## The IRTree treats each response as a two-stage decision process:
##   Node 1: did the student skip?       (response = q)
##   Node 2: given a response, was it incorrect? (response = 1 - correct_answered)
##
## This is achieved by duplicating every observation into two rows — one per
## node — and stacking them into a single long-format data.table (dat_irt).
## The `node` column identifies which decision stage each row belongs to.
## Node 2 rows for skipped items have response = NA and are excluded from
## the node 2 likelihood automatically by glmer.
##
## Output: logs_clean (data.frame, wide format, grade-filtered, with derived columns)
##         dat_irt   (data.table, long format, eligible users only)
## Both are available to sourcing scripts.

library(data.table)
library(lme4)

# Source config via absolute path so wd does not need to be correct beforehand
.this_dir <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  dirname(normalizePath(rstudioapi::getActiveDocumentContext()$path))
} else {
  args <- commandArgs(trailingOnly = FALSE)
  dirname(normalizePath(sub("--file=", "", grep("--file=", args, value = TRUE))))
}
source(file.path(.this_dir, "00_config.R"))
rm(.this_dir)

# ============================================================
# LOAD DATA
# ============================================================
load(data_path)
logs_clean <- get(data_object)  # works for both "sim_logs" and "logs_clean"

# ============================================================
# CLEAN DATA
# ============================================================
# Grade filter: thresholds are defined in 00_config.R so they stay consistent
# across the modelling pipeline and the descriptive analysis in the Rmd.
logs_clean <- logs_clean[logs_clean$grade >= min_grade & logs_clean$grade <= max_grade, ]

# Derive skip indicator from the answer string ("¿" is the Prowise Learn
# platform's internal encoding for a skip). This makes the skip flag available
# by a self-explanatory column name for descriptive plots, independently of the
# node-specific `q` column used in the IRTree model.
logs_clean$question_mark <- as.integer(logs_clean$answer == "\u00bf")

# Three-level response label for descriptive plots in 05_itree_analysis.Rmd.
logs_clean$response_type <- ifelse(
  logs_clean$question_mark == 1, "qm",
  ifelse(logs_clean$correct_answered == 0, "error", "correct")
)

# ============================================================
# RESHAPE TO LONG FORMAT (one row per observation per node)
# ============================================================

# Node 1 rows: response = 1 if the student skipped, 0 if they responded
logs_clean_node1          <- logs_clean
logs_clean_node1$node     <- 1
logs_clean_node1$response <- logs_clean_node1$q

# Node 2 rows: response = 1 if incorrect, 0 if correct, NA if skipped
# (NA rows are excluded from the node 2 likelihood by glmer)
logs_clean_node2          <- logs_clean
logs_clean_node2$node     <- 2
logs_clean_node2$response <- ifelse(logs_clean_node2$correct_answered == 0, 1, 0)

# Stack and sort by original observation id to keep student-item pairs together
dat_irt <- rbind(logs_clean_node1, logs_clean_node2)
dat_irt <- dat_irt[order(dat_irt$id), ]

# logs_clean is kept in the environment: the Rmd uses it for individual user
# response-time plots (which require the original wide format).
rm(logs_clean_node1, logs_clean_node2)
gc()
dat_irt <- data.table(dat_irt)

# ============================================================
# FILTER USERS
# ============================================================
# Keep only students with at least min_skips skip responses (node 1, response = 1).
# This ensures each included student has enough skip data to estimate
# their skip propensity (theta1) reliably.
users   <- dat_irt[node == 1 & response == 1, .N, .(user_id)][N >= min_skips, user_id]
dat_irt <- dat_irt[user_id %in% users, ]
rm(users)
