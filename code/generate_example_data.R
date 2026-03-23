## generate_example_data.R
##
## Generates synthetic data that mimics the structure of the Prowise Learn
## (Math Garden) dataset used in the IRTree analysis.
##
## The real data comes from the 'Series' game, where students fill in missing
## numbers in sequences. The platform uses a speed-accuracy scoring model
## based on the Elo Rating System (see manuscript Section 2.1 for full
## description of the scoring rule and ability update equations).
##
## This script saves logs_clean.Rdata to the working directory, which is
## the input expected by irtree_prep.R, irtree_fit.R, and irtree_kfolds.R.
##
## Usage:
##   source("generate_example_data.R")
##   # or: Rscript generate_example_data.R
##
## Author: Annie M. Johansson

library(MASS) # mvrnorm() for correlated person/item parameters
library(data.table) # consistent with downstream scripts

# Source config.R
.this_dir <- if (
  requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
) {
  dirname(normalizePath(rstudioapi::getActiveDocumentContext()$path))
} else {
  args <- commandArgs(trailingOnly = FALSE)
  dirname(normalizePath(sub(
    "--file=",
    "",
    grep("--file=", args, value = TRUE)
  )))
}
source(file.path(.this_dir, "config.R"))
rm(.this_dir)

set.seed(random_seed)

# ============================================================
# SIMULATION PARAMETERS
# ============================================================

n_users <- 500 # number of students (real dataset: ~3500; reduced for speed)
n_items <- 100 # number of unique items in the item pool
mean_trials <- 100 # mean number of trials per student (Poisson-distributed)
deadline <- 20 # item response deadline in seconds (fixed at 20s in platform)

# -- Person parameters (node 1 = skip propensity, node 2 = error propensity) --
# Higher theta1 -> student more likely to skip
# Higher theta2 -> student more likely to respond incorrectly (lower ability)
# Moderate positive correlation: students who skip more also tend to err more
# (see manuscript RQ2 and Figure cor.pdf for empirical estimates)
theta_mu <- c(0, 0)
theta_cor <- 0.35 # moderate positive correlation
theta_sd <- c(1.2, 1.0)
theta_sigma <- matrix(
  c(
    theta_sd[1]^2,
    theta_cor * prod(theta_sd),
    theta_cor * prod(theta_sd),
    theta_sd[2]^2
  ),
  nrow = 2
)

# -- Item parameters (node 1 = skip threshold, node 2 = difficulty) --
# Higher beta1 -> item has a higher threshold to be skipped (less likely skipped)
# Higher beta2 -> item is harder (more likely to be answered incorrectly)
#
# Difficulty is assigned first; beta2 is then drawn conditional on difficulty
# so that at the average person (theta2 = 0), error rates match the platform's
# three difficulty levels:
#   Easy   (0): ~90% correct -> ~10% error  -> beta2 mean = -logit(0.10) ≈  2.20
#   Medium (1): ~75% correct -> ~25% error  -> beta2 mean = -logit(0.25) ≈  1.10
#   Hard   (2): ~60% correct -> ~40% error  -> beta2 mean = -logit(0.40) ≈  0.41
#
# beta1 is drawn with a global offset of ~2 to keep the overall skip rate low
# (~12%); harder items have a slightly lower offset (easier to skip).
beta2_means <- c(-qlogis(0.10), -qlogis(0.25), -qlogis(0.40)) # ≈ c(2.20, 1.10, 0.41)
beta2_sd <- 0.4 # within-level variability in item difficulty
beta1_means <- c(2.2, 2.0, 1.8) # easy items least likely to be skipped
beta1_sd <- 0.6

# ============================================================
# GENERATE PERSON AND ITEM PARAMETERS
# ============================================================

theta <- mvrnorm(n_users, mu = theta_mu, Sigma = theta_sigma)
users <- data.frame(
  user_id = 1:n_users,
  theta1 = theta[, 1], # skip propensity
  theta2 = theta[, 2], # error propensity (inverse of ability)
  grade = sample(
    3:8,
    n_users,
    replace = TRUE,
    prob = c(0.15, 0.20, 0.20, 0.20, 0.15, 0.10)
  )
)

items <- data.frame(
  item_id = 1:n_items,
  difficulty = sample(
    0:2,
    n_items,
    replace = TRUE,
    prob = c(1 / 3, 1 / 3, 1 / 3)
  )
)
# beta2: draw conditional on difficulty to hit target error rates
items$beta2 <- rnorm(
  n_items,
  mean = beta2_means[items$difficulty + 1],
  sd = beta2_sd
)
# beta1: global offset keeps skip rate low; harder items slightly lower threshold
items$beta1 <- rnorm(
  n_items,
  mean = beta1_means[items$difficulty + 1],
  sd = beta1_sd
)

# ============================================================
# SIMULATE TRIALS
# ============================================================

# Number of trials per student follows a Poisson distribution,
# with a minimum of 30 to ensure enough skip responses after filtering.
n_trials_per_user <- pmax(rpois(n_users, lambda = mean_trials), 30)

# Build trial log (one row per student-item interaction)
logs_list <- vector("list", n_users)
for (u in seq_len(n_users)) {
  nt <- n_trials_per_user[u]
  # Adaptive systems re-sample from the item pool; items can repeat
  items_seen <- sample(items$item_id, nt, replace = TRUE)
  logs_list[[u]] <- data.frame(
    user_id = users$user_id[u],
    item_id = items_seen,
    grade = users$grade[u],
    theta1 = users$theta1[u],
    theta2 = users$theta2[u],
    trial = seq_len(nt)
  )
}
logs <- rbindlist(logs_list)

# Merge in item parameters
logs <- merge(
  logs,
  items[, c("item_id", "beta1", "beta2", "difficulty")],
  by = "item_id",
  all.x = TRUE
)
logs <- logs[order(user_id, trial)]

# -- Node 1: Did the student skip? --
# P(skip) = logistic(theta1 - beta1)
p_skip <- plogis(logs$theta1 - logs$beta1)
logs$q <- rbinom(nrow(logs), 1, p_skip) # 1 = skip, 0 = responded

# -- Node 2: Given a response, was it incorrect? --
# P(error | responded) = logistic(theta2 - beta2)
# Only meaningful when q == 0; set to NA for skipped items.
p_error <- plogis(logs$theta2 - logs$beta2)
error <- rbinom(nrow(logs), 1, p_error)

# correct_answered: 1 = correct, 0 = incorrect, NA = skip
# NA propagation: ifelse(NA == 0, 1, 0) returns NA in irtree_prep.R,
# correctly excluding skipped items from node 2 likelihood in glmer.
logs$correct_answered <- ifelse(logs$q == 1, NA_real_, ifelse(error == 1, 0, 1))

# -- Answer string --
# "¿" is the platform's internal encoding for a skip response
logs$answer <- ifelse(
  logs$q == 1,
  "¿",
  ifelse(
    logs$correct_answered == 1,
    as.character(sample(1:50, nrow(logs), replace = TRUE)),
    as.character(sample(1:50, nrow(logs), replace = TRUE))
  )
)

# ============================================================
# SIMULATE RESPONSE TIMES
# ============================================================
# The platform uses a high-speed, high-stakes (HSHS) scoring rule (see eq. 3
# in manuscript): S = (2x - 1)(alpha*d - alpha*t), with alpha = 1/d, d = 20s.
# Response time distributions differ by response type:
#   - Skip: rapid decision, 0.5 - 6 seconds
#   - Correct response: student took time to think and answer, 2 - 18 seconds
#   - Incorrect response: attempted but failed, wide range 1 - 19 seconds

rt_sec <- ifelse(
  logs$q == 1,
  runif(nrow(logs), 0.5, 6), # Skip: rapid decision
  ifelse(
    logs$correct_answered == 1,
    pmax(1, rnorm(nrow(logs), mean = 9, sd = 4)), # Correct: moderate time
    pmax(1, rnorm(nrow(logs), mean = 11, sd = 5)) # Incorrect: slightly longer
  )
)
rt_sec <- pmin(rt_sec, deadline) # cap at deadline

logs$rt <- round(rt_sec, 1)
logs$response_in_milliseconds <- as.integer(round(rt_sec * 1000))

# ============================================================
# SESSION COUNT
# ============================================================
# new_user_domain_modified_count: cumulative session count per student-domain.
# Here approximated as the within-student trial index.
logs$new_user_domain_modified_count <- logs$trial

# ============================================================
# FINALIZE
# ============================================================
logs$id <- seq_len(nrow(logs))

sim_logs <- logs[, .(
  id,
  user_id,
  item_id,
  grade,
  difficulty,
  q,
  correct_answered,
  answer,
  rt,
  response_in_milliseconds,
  new_user_domain_modified_count
)]

# ============================================================
# SANITY CHECKS
# ============================================================
cat("=== Synthetic data summary ===\n")
cat("Total responses        :", nrow(sim_logs), "\n")
cat("Unique students        :", uniqueN(sim_logs$user_id), "\n")
cat("Unique items           :", uniqueN(sim_logs$item_id), "\n")
cat("Overall skip rate      :", round(mean(sim_logs$q), 3), "\n")
cat(
  "Error rate (responses) :",
  round(mean(sim_logs[q == 0]$correct_answered == 0, na.rm = TRUE), 3),
  "\n"
)
cat("Error rate by difficulty (target: Easy=0.10, Medium=0.25, Hard=0.40):\n")
for (d in 0:2) {
  label <- c("  Easy  (0)", "  Medium(1)", "  Hard  (2)")[d + 1]
  rate <- round(
    mean(
      sim_logs[q == 0 & difficulty == d]$correct_answered == 0,
      na.rm = TRUE
    ),
    3
  )
  cat(label, ":", rate, "\n")
}

# Inclusion criterion: irtree_prep.R keeps users with >= 10 skip responses
skips_per_user <- sim_logs[q == 1, .N, user_id]
n_eligible <- sum(skips_per_user$N >= 10)
cat(
  "Students with >= 10 skips:",
  n_eligible,
  "/",
  n_users,
  sprintf("(%.0f%%)\n", 100 * n_eligible / n_users)
)

# Node correlation (should be close to theta_cor = 0.35)
user_means <- sim_logs[,
  .(
    skip_rate = mean(q),
    error_rate = mean(correct_answered == 0, na.rm = TRUE)
  ),
  user_id
]
cat(
  "Observed skip/error correlation (user level):",
  round(cor(user_means$skip_rate, user_means$error_rate), 3),
  "\n"
)

# ============================================================
# SAVE
# ============================================================
dir.create("data", showWarnings = FALSE, recursive = TRUE)
save(sim_logs, file = data_path)
cat("\nSaved: sim_logs.Rdata\n")
cat("Run irtree_prep.R, irtree_fit.R, or irtree_kfolds.R as usual.\n")
