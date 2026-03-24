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

library(MASS) # mvrnorm() for drawing correlated person/item parameters
library(data.table) # consistent with downstream scripts

# Source config via absolute path so the working directory need not be set
# beforehand. Mirrors the detection logic in 00_config.R.
# Priority: Rscript CLI -> RStudio -> source() call -> working directory.
.this_dir <- (function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("--file=", "", file_arg))))
  }
  if (
    requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
  ) {
    ctx <- rstudioapi::getSourceEditorContext()
    if (!is.null(ctx) && nchar(ctx$path) > 0) {
      return(dirname(normalizePath(ctx$path)))
    }
  }
  src <- tryCatch(
    dirname(normalizePath(sys.frames()[[1]]$ofile)),
    error = function(e) NULL
  )
  if (!is.null(src)) {
    return(src)
  }
  getwd()
})()
source(file.path(.this_dir, "00_config.R"))
rm(.this_dir)

set.seed(random_seed)

# ============================================================
# SIMULATION PARAMETERS
# ============================================================

n_users <- 500 # students (real dataset: ~3500; reduced for speed)
n_items <- 300 # unique items in the pool
trials_per_session <- 10 # fixed number of trials per session
deadline <- 20000 # response deadline in milliseconds (fixed in platform)

# Session counts follow a right-skewed log-normal distribution:
# many learners complete only a handful of sessions, but a meaningful
# proportion reach 50-100 sessions (matching the real platform).
session_meanlog <- log(10) # log-scale mean → median ≈ 10 sessions (mode ≈ 4, clipped to min)
session_sdlog <- 1.0 # log-scale SD   → ~5% of learners exceed ~50 sessions,
#                   ~1% exceed ~100 sessions
session_min <- 5 # minimum sessions per learner
session_max <- 150 # cap to keep simulation times reasonable

# Person parameters: theta1 = skip propensity, theta2 = error propensity
# Both are drawn from a bivariate normal with a moderate positive correlation:
# students who skip more also tend to make more errors.
theta_mu <- c(0, 0)
theta_cor <- 0.35
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

# Item parameters: beta1 = skip threshold, beta2 = item difficulty
# beta2 is drawn conditional on difficulty level to match the platform's
# target correct rates: Easy = 90%, Medium = 75%, Hard = 60%.
# beta1 is offset upward (~2) so the overall skip rate stays low (~12%).
beta2_means <- c(-qlogis(0.10), -qlogis(0.25), -qlogis(0.40)) # ≈ 2.20, 1.10, 0.41
beta2_sd <- 0.4
beta1_means <- c(2.2, 2.0, 1.8) # easier items are harder to skip
beta1_sd <- 0.6

# ============================================================
# GENERATE PERSON AND ITEM PARAMETERS
# ============================================================

theta <- mvrnorm(n_users, mu = theta_mu, Sigma = theta_sigma)
users <- data.frame(
  user_id = 1:n_users,
  theta1 = theta[, 1],
  theta2 = theta[, 2],
  grade = sample(
    3:8,
    n_users,
    replace = TRUE,
    prob = c(0.15, 0.20, 0.20, 0.20, 0.15, 0.10)
  )
)

# Assign difficulty first, then draw beta2 conditional on it
items <- data.frame(
  item_id = 1:n_items,
  difficulty = sample(0:2, n_items, replace = TRUE)
)
items$beta2 <- rnorm(
  n_items,
  mean = beta2_means[items$difficulty + 1],
  sd = beta2_sd
)
items$beta1 <- rnorm(
  n_items,
  mean = beta1_means[items$difficulty + 1],
  sd = beta1_sd
)

# ============================================================
# SIMULATE TRIALS
# ============================================================

# Draw session counts from a right-skewed log-normal, then convert to trial
# counts. Most learners complete a few sessions; the long right tail ensures
# a meaningful proportion reach 50-100 sessions, as in the real platform.
n_sessions_per_user <- as.integer(pmin(
  pmax(
    ceiling(rlnorm(n_users, meanlog = session_meanlog, sdlog = session_sdlog)),
    session_min
  ),
  session_max
))
n_trials_per_user <- n_sessions_per_user * trials_per_session

# Build one row per student-item interaction; items are sampled with
# replacement to mimic an adaptive system drawing from a shared item pool
logs_list <- vector("list", n_users)
for (u in seq_len(n_users)) {
  nt <- n_trials_per_user[u]
  logs_list[[u]] <- data.frame(
    user_id = users$user_id[u],
    item_id = sample(items$item_id, nt, replace = TRUE),
    grade = users$grade[u],
    theta1 = users$theta1[u],
    theta2 = users$theta2[u],
    session_count = (seq_len(nt) - 1) %/% trials_per_session + 1
  )
}
logs <- rbindlist(logs_list)
logs <- merge(
  logs,
  items[, c("item_id", "beta1", "beta2", "difficulty")],
  by = "item_id",
  all.x = TRUE
)
logs <- logs[order(user_id, session_count)]

# Node 1: skip decision — P(skip) = logistic(theta1 - beta1)
logs$q <- rbinom(nrow(logs), 1, plogis(logs$theta1 - logs$beta1)) # 1 = skip

# Node 2: error given response — P(error) = logistic(theta2 - beta2)
# Set to NA for skipped items: ifelse(NA == 0, 1, 0) returns NA in
# 02_irtree_prep.R, correctly excluding skips from the node 2 likelihood.
error <- rbinom(nrow(logs), 1, plogis(logs$theta2 - logs$beta2))
logs$correct_answered <- ifelse(logs$q == 1, NA_real_, ifelse(error == 1, 0, 1))

# Answer string: "¿" is the platform's internal encoding for a skip response
logs$answer <- ifelse(
  logs$q == 1,
  "¿",
  as.character(sample(1:50, nrow(logs), replace = TRUE))
)

# ============================================================
# SIMULATE RESPONSE TIMES
# ============================================================
# Skips are rapid; correct responses moderate; incorrect responses slow.
# simulate_rt() (defined in 00_config.R) draws from a truncated log-normal
# parameterised by mode, so values are naturally right-skewed and bounded.

logs$rt <- ifelse(
  logs$q == 1,
  simulate_rt(
    nrow(logs),
    mode = 2000,
    sigma = 0.6,
    min_rt = 500,
    max_rt = deadline
  ),
  ifelse(
    logs$correct_answered == 1,
    simulate_rt(
      nrow(logs),
      mode = 5000,
      sigma = 0.7,
      min_rt = 1000,
      max_rt = deadline
    ),
    simulate_rt(
      nrow(logs),
      mode = 6000,
      sigma = 0.7,
      min_rt = 1000,
      max_rt = deadline
    )
  )
)
# response_in_milliseconds mirrors `rt` and is kept to match the column name
# used in the real Prowise Learn export (consumed by 05_itree_analysis.Rmd).
logs$response_in_milliseconds <- logs$rt

# new_user_domain_modified_count is the Prowise Learn platform's internal name
# for cumulative session count. Retained here so the simulated data is a
# drop-in replacement for the real dataset without renaming columns downstream.
logs$new_user_domain_modified_count <- logs$session_count

# ============================================================
# FINALISE DATASET
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

cat("Error rate by difficulty (targets: Easy=0.10, Medium=0.25, Hard=0.40):\n")
for (d in 0:2) {
  cat(
    c("  Easy  (0)", "  Medium(1)", "  Hard  (2)")[d + 1],
    ":",
    round(
      mean(
        sim_logs[q == 0 & difficulty == d]$correct_answered == 0,
        na.rm = TRUE
      ),
      3
    ),
    "\n"
  )
}

# Session count distribution
sessions_per_user <- sim_logs[, max(new_user_domain_modified_count), user_id]$V1
cat(
  "Sessions per learner (median / mean / max) :",
  round(median(sessions_per_user)),
  "/",
  round(mean(sessions_per_user)),
  "/",
  max(sessions_per_user),
  "\n"
)
cat(
  "% learners with >= 50 sessions :",
  round(100 * mean(sessions_per_user >= 50), 1),
  "%\n"
)
cat(
  "% learners with >= 100 sessions :",
  round(100 * mean(sessions_per_user >= 100), 1),
  "%\n"
)

# Check inclusion criterion applied in 02_irtree_prep.R
skips_per_user <- sim_logs[q == 1, .N, user_id]
n_eligible <- sum(skips_per_user$N >= min_skips)
cat(
  "Students with >=",
  min_skips,
  "skips:",
  n_eligible,
  "/",
  n_users,
  sprintf("(%.0f%%)\n", 100 * n_eligible / n_users)
)

# Person-level correlation should approximate theta_cor = 0.35
user_means <- sim_logs[,
  .(
    skip_rate = mean(q),
    error_rate = mean(correct_answered == 0, na.rm = TRUE)
  ),
  user_id
]
cat(
  "Skip/error correlation (user level):",
  round(cor(user_means$skip_rate, user_means$error_rate), 3),
  "\n"
)

# ============================================================
# SAVE
# ============================================================
save(sim_logs, file = data_path)
cat("\nSaved:", data_path, "\n")
