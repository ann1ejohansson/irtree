## 04_irtree_kfolds.R
## Author: Annie M. Johansson
##
## Robustness check for the four IRTree models using k-fold cross-validation.
## For each fold, 10% of responses per student per node are held out. All four
## models from 03_irtree_fit.R are refitted on the remaining data, and RMSE
## is computed on the held-out responses. This tests whether model fit
## generalises beyond the training data.
##
##
## Output: results/k_folds/ — per-model per-fold summaries, and k_folds.Rdata

source("02_irtree_prep.R")

# Unique row identifier (for auditing which rows were held out per fold)
dat_irt$id_node <- paste0(dat_irt$id, "-", dat_irt$node)

# Within-student, within-node row index (used for reproducible holdout sampling)
dat_irt[, i := 1:.N, .(user_id, node)]

# response_NA is a copy of response; held-out values will be set to NA
dat_irt[, response_NA := response]

# Results table: one row per model per fold
k_folds <- data.frame(
  k = rep(1:n_folds, each = 4),
  perc_na = NA, # actual fraction held out (sanity check)
  model = rep(1:4, times = n_folds),
  aic = NA,
  bic = NA,
  rmse = NA # root mean squared error on held-out responses
)

# Track which rows were held out in each fold (for reproducibility checks)
removed <- data.frame(k = numeric(), id_node = numeric())

set.seed(random_seed)

for (k in 1:n_folds) {
  cat("\n\nFold:", k, "— Start time:")
  print(Sys.time())
  t_fold <- Sys.time()

  # Sample holdout_frac of responses per student per node
  dat_val <- dat_irt[
    !is.na(response),
    .(i = sample(1:.N, ceiling(.N * holdout_frac))),
    .(user_id, node)
  ]
  dat_val$remove <- 1 # flag rows to be held out

  # Merge flags back into the full dataset and set held-out responses to NA
  dat_fit <- dat_val[dat_irt, on = c("node", "user_id", "i")]
  dat_fit[is.na(remove), remove := 0]
  dat_fit[remove == 1, response_NA := NA]

  # Record actual holdout fraction and which rows were removed
  k_folds[k_folds$k == k, ]$perc_na <-
    rep(
      sum(is.na(dat_fit$response_NA) & dat_fit$remove == 1, na.rm = TRUE) /
        dat_fit[, .N],
      4
    )
  removed <- rbind(
    removed,
    data.frame(k = k, id_node = dat_fit[remove == 1, id_node])
  )

  # ----------------------------------------------------------
  # Fit all four models on training data (response_NA), then
  # predict on held-out rows and compute RMSE.
  # Models mirror those in 03_irtree_fit.R
  # ----------------------------------------------------------

  # Model 1: fully estimated IRTree
  t <- Sys.time()
  m <- try(glmer(
    response_NA ~ 1 +
      (0 + factor(node) | item_id) +
      (0 + factor(node) | user_id),
    data = dat_fit,
    family = "binomial"
  ))
  if (class(m) != "try-error") {
    cat(
      "\n  Model 1 time:",
      round(difftime(Sys.time(), t, units = "mins"), 1),
      "min"
    )
    p <- predict(m, newdata = dat_fit, allow.new.levels = TRUE)
    dat_fit[, pred_mod := plogis(p)]
    rmse <- dat_fit[
      remove == 1 & !is.na(response),
      sqrt(mean((response - pred_mod)^2))
    ]
    k_folds[k_folds$k == k & k_folds$model == 1, c("aic", "bic", "rmse")] <-
      list(AIC(m), BIC(m), rmse)
    model_sum <- summary(m)
    save(model_sum, file = file.path(kfolds_dir, paste0("mod1_k", k, ".Rdata")))
    rm(m)
    gc()
  }

  # Model 2: item-constrained
  t <- Sys.time()
  m <- try(glmer(
    response_NA ~ 1 + (1 | item_id) + (0 + factor(node) | user_id),
    data = dat_fit,
    family = "binomial"
  ))
  if (class(m) != "try-error") {
    cat(
      "\n  Model 2 time:",
      round(difftime(Sys.time(), t, units = "mins"), 1),
      "min"
    )
    p <- predict(m, newdata = dat_fit, allow.new.levels = TRUE)
    dat_fit[, pred_mod := plogis(p)]
    rmse <- dat_fit[
      remove == 1 & !is.na(response),
      sqrt(mean((response - pred_mod)^2))
    ]
    k_folds[k_folds$k == k & k_folds$model == 2, c("aic", "bic", "rmse")] <-
      list(AIC(m), BIC(m), rmse)
    model_sum <- summary(m)
    save(model_sum, file = file.path(kfolds_dir, paste0("mod2_k", k, ".Rdata")))
    rm(m)
    gc()
  }

  # Model 3: user-constrained
  t <- Sys.time()
  m <- try(glmer(
    response_NA ~ 1 + (0 + factor(node) | item_id) + (1 | user_id),
    data = dat_fit,
    family = "binomial"
  ))
  if (class(m) != "try-error") {
    cat(
      "\n  Model 3 time:",
      round(difftime(Sys.time(), t, units = "mins"), 1),
      "min"
    )
    p <- predict(m, newdata = dat_fit, allow.new.levels = TRUE)
    dat_fit[, pred_mod := plogis(p)]
    rmse <- dat_fit[
      remove == 1 & !is.na(response),
      sqrt(mean((response - pred_mod)^2))
    ]
    k_folds[k_folds$k == k & k_folds$model == 3, c("aic", "bic", "rmse")] <-
      list(AIC(m), BIC(m), rmse)
    model_sum <- summary(m)
    save(model_sum, file = file.path(kfolds_dir, paste0("mod3_k", k, ".Rdata")))
    rm(m)
    gc()
  }

  # Model 4: fully constrained (unidimensional baseline)
  t <- Sys.time()
  m <- try(glmer(
    response_NA ~ 1 + factor(node) + (1 | item_id) + (1 | user_id),
    data = dat_fit,
    family = "binomial"
  ))
  if (class(m) != "try-error") {
    cat(
      "\n  Model 4 time:",
      round(difftime(Sys.time(), t, units = "mins"), 1),
      "min"
    )
    p <- predict(m, newdata = dat_fit, allow.new.levels = TRUE)
    dat_fit[, pred_mod := plogis(p)]
    rmse <- dat_fit[
      remove == 1 & !is.na(response),
      sqrt(mean((response - pred_mod)^2))
    ]
    k_folds[k_folds$k == k & k_folds$model == 4, c("aic", "bic", "rmse")] <-
      list(AIC(m), BIC(m), rmse)
    model_sum <- summary(m)
    save(model_sum, file = file.path(kfolds_dir, paste0("mod4_k", k, ".Rdata")))
    rm(m)
    gc()
  }

  cat(
    "\n  Fold",
    k,
    "total time:",
    round(difftime(Sys.time(), t_fold, units = "mins"), 1),
    "min"
  )
  rm(list = intersect(ls(), c("dat_val", "dat_fit", "p", "rmse")))
  gc()

  # Save progress after every fold so partial results are not lost
  save(k_folds, file = file.path(kfolds_dir, "k_folds.Rdata"))
}

# ============================================================
# SUMMARISE RESULTS
# ============================================================
load(file.path(kfolds_dir, "k_folds.Rdata"))

cat("\n\nMean RMSE across folds:\n")
for (mod in 1:4) {
  cat(
    "  Model",
    mod,
    ":",
    round(mean(k_folds[k_folds$model == mod, ]$rmse, na.rm = TRUE), 4),
    "\n"
  )
}

# ============================================================
# DEVELOPMENT CHECKS (commented out)
# ============================================================
# set.seed(1234)
# users <- sample(unique(dat_irt$user_id), 20)         # test on small subset
# dat_irt <- dat_irt[dat_irt$user_id %in% users, ]; gc()
# table(dat_fit$response, dat_fit$remove, dat_fit$node, useNA = "always")
# table(is.na(match(removed[removed$k == 4, ]$id_node, removed[removed$k == 9, ]$id_node)))
