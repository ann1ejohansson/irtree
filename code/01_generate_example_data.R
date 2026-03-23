## 01_generate_example_data.R
##
## Generates synthetic data for local testing of the IRTree pipeline.
## Simulates student responses from the Prowise Learn 'Series' game, where
## students fill in missing numbers in sequences and can skip items by clicking
## a question mark button.
##
## Responses are generated from an IRTree model with two nodes:
##   Node 1: did the student skip? (skip propensity)
##   Node 2: given a response, was it incorrect? (error propensity / inv. ability)
##
## The real platform uses an Elo-based speed-accuracy scoring rule; see
## manuscript Section 2.1 for full details.
##
## Output: data/sim_logs.Rdata (loaded by 02_irtree_prep.R)
##
## Usage: source("01_generate_example_data.R")
##        Rscript 01_generate_example_data.R

library(MASS)        # mvrnorm() for drawing correlated person/item parameters
library(data.table)  # consistent with downstream scripts

# Source config via absolute path so wd does not need to be set beforehand
.this_dir <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  dirname(normalizePath(rstudioapi::getActiveDocumentContext()$path))
} else {
  args <- commandArgs(trailingOnly = FALSE)
  dirname(normalizePath(sub("--file=", "", grep("--file=", args, value = TRUE))))
}
source(file.path(.this_dir, "00_config.R"))
rm(.this_dir)

set.seed(random_seed)

# ============================================================
# SIMULATION PARAMETERS
# ============================================================

n_users     <- 500   # students (real dataset: ~3500; reduced for speed)
n_items     <- 100   # unique items in the pool
mean_trials <- 100   # mean trials per student (Poisson-distributed)
deadline    <- 20    # response deadline in seconds (fixed in platform)

# Person parameters: theta1 = skip propensity, theta2 = error propensity
# Both are drawn from a bivariate normal with a moderate positive correlation:
# students who skip more also tend to make more errors (lower ability).
theta_mu    <- c(0, 0)
theta_cor   <- 0.35
theta_sd    <- c(1.2, 1.0)
theta_sigma <- matrix(c(theta_sd[1]^2, theta_cor * prod(theta_sd),
                        theta_cor * prod(theta_sd), theta_sd[2]^2), nrow = 2)

# Item parameters: beta1 = skip threshold, beta2 = item difficulty
# beta2 is drawn conditional on difficulty level to match the platform's
# target correct rates: Easy = 90%, Medium = 75%, Hard = 60%.
# beta1 is offset upward (~2) so the overall skip rate stays low (~12%).
beta2_means <- c(-qlogis(0.10), -qlogis(0.25), -qlogis(0.40))  # ≈ 2.20, 1.10, 0.41
beta2_sd    <- 0.4
beta1_means <- c(2.2, 2.0, 1.8)  # easier items are harder to skip
beta1_sd    <- 0.6

# ============================================================
# GENERATE PERSON AND ITEM PARAMETERS
# ============================================================

theta <- mvrnorm(n_users, mu = theta_mu, Sigma = theta_sigma)
users <- data.frame(
  user_id = 1:n_users,
  theta1  = theta[, 1],
  theta2  = theta[, 2],
  grade   = sample(3:8, n_users, replace = TRUE,
                   prob = c(0.15, 0.20, 0.20, 0.20, 0.15, 0.10))
)

# Assign difficulty first, then draw beta2 conditional on it
items <- data.frame(
  item_id    = 1:n_items,
  difficulty = sample(0:2, n_items, replace = TRUE)
)
items$beta2 <- rnorm(n_items, mean = beta2_means[items$difficulty + 1], sd = beta2_sd)
items$beta1 <- rnorm(n_items, mean = beta1_means[items$difficulty + 1], sd = beta1_sd)

# ============================================================
# SIMULATE TRIALS
# ============================================================

# Each student completes a Poisson-distributed number of trials (min. 30)
n_trials_per_user <- pmax(rpois(n_users, lambda = mean_trials), 30)

# Build one row per student-item interaction; items are sampled with
# replacement to mimic an adaptive system drawing from a shared item pool
logs_list <- vector("list", n_users)
for (u in seq_len(n_users)) {
  nt <- n_trials_per_user[u]
  logs_list[[u]] <- data.frame(
    user_id = users$user_id[u],
    item_id = sample(items$item_id, nt, replace = TRUE),
    grade   = users$grade[u],
    theta1  = users$theta1[u],
    theta2  = users$theta2[u],
    trial   = seq_len(nt)
  )
}
logs <- rbindlist(logs_list)
logs <- merge(logs, items[, c("item_id", "beta1", "beta2", "difficulty")],
              by = "item_id", all.x = TRUE)
logs <- logs[order(user_id, trial)]

# Node 1: skip decision — P(skip) = logistic(theta1 - beta1)
logs$q <- rbinom(nrow(logs), 1, plogis(logs$theta1 - logs$beta1))  # 1 = skip

# Node 2: error given response — P(error) = logistic(theta2 - beta2)
# Set to NA for skipped items: ifelse(NA == 0, 1, 0) returns NA in
# 02_irtree_prep.R, correctly excluding skips from the node 2 likelihood.
error <- rbinom(nrow(logs), 1, plogis(logs$theta2 - logs$beta2))
logs$correct_answered <- ifelse(logs$q == 1, NA_real_, ifelse(error == 1, 0, 1))

# Answer string: "¿" is the platform's internal encoding for a skip response
logs$answer <- ifelse(logs$q == 1, "¿",
                      as.character(sample(1:50, nrow(logs), replace = TRUE)))

# ============================================================
# SIMULATE RESPONSE TIMES
# ============================================================
# Based on the HSHS scoring rule: S = (2x-1)(alpha*d - alpha*t), alpha = 1/d
# Skips are rapid; correct responses moderate; incorrect responses slow.
rt_sec <- ifelse(
  logs$q == 1,
  runif(nrow(logs), 0.5, 6),
  ifelse(logs$correct_answered == 1,
         pmax(1, rnorm(nrow(logs), mean = 9,  sd = 4)),
         pmax(1, rnorm(nrow(logs), mean = 11, sd = 5)))
)
rt_sec <- pmin(rt_sec, deadline)

logs$rt                       <- round(rt_sec, 1)
logs$response_in_milliseconds <- as.integer(round(rt_sec * 1000))

# Cumulative trial count per student approximates session count
logs$new_user_domain_modified_count <- logs$trial

# ============================================================
# FINALISE DATASET
# ============================================================
logs$id <- seq_len(nrow(logs))

sim_logs <- logs[, .(id, user_id, item_id, grade, difficulty,
                     q, correct_answered, answer,
                     rt, response_in_milliseconds,
                     new_user_domain_modified_count)]

# ============================================================
# SANITY CHECKS
# ============================================================
cat("=== Synthetic data summary ===\n")
cat("Total responses        :", nrow(sim_logs), "\n")
cat("Unique students        :", uniqueN(sim_logs$user_id), "\n")
cat("Unique items           :", uniqueN(sim_logs$item_id), "\n")
cat("Overall skip rate      :", round(mean(sim_logs$q), 3), "\n")
cat("Error rate (responses) :",
    round(mean(sim_logs[q == 0]$correct_answered == 0, na.rm = TRUE), 3), "\n")

cat("Error rate by difficulty (targets: Easy=0.10, Medium=0.25, Hard=0.40):\n")
for (d in 0:2) {
  cat(c("  Easy  (0)", "  Medium(1)", "  Hard  (2)")[d + 1], ":",
      round(mean(sim_logs[q == 0 & difficulty == d]$correct_answered == 0,
                 na.rm = TRUE), 3), "\n")
}

# Check inclusion criterion applied in 02_irtree_prep.R
skips_per_user <- sim_logs[q == 1, .N, user_id]
n_eligible     <- sum(skips_per_user$N >= min_skips)
cat("Students with >=", min_skips, "skips:", n_eligible, "/", n_users,
    sprintf("(%.0f%%)\n", 100 * n_eligible / n_users))

# Person-level correlation should approximate theta_cor = 0.35
user_means <- sim_logs[, .(skip_rate  = mean(q),
                            error_rate = mean(correct_answered == 0, na.rm = TRUE)),
                        user_id]
cat("Skip/error correlation (user level):",
    round(cor(user_means$skip_rate, user_means$error_rate), 3), "\n")

# ============================================================
# SAVE
# ============================================================
save(sim_logs, file = data_path)
cat("\nSaved:", data_path, "\n")
