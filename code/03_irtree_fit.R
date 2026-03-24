## 03_irtree_fit.R
##
## Fits four nested IRTree models to the response data and compares their fit.
## Each model is a mixed-effects logistic regression (glmer) on the long-format
## data from 02_irtree_prep.R, with `node` as a grouping factor.
##
## The four models differ in how much skip propensity and accuracy are allowed
## to vary independently across students and items:
##
##   Model 1 — fully estimated:   separate random effects per node for both
##                                 students and items. Estimates cor(theta1, theta2)
##                                 and cor(beta1, beta2). This is the full IRTree.
##
##   Model 2 — item-constrained:  item effects are pooled across nodes (single
##                                 random effect per item). Tests whether items
##                                 need node-specific difficulty parameters.
##
##   Model 3 — user-constrained:  student effects are pooled across nodes.
##                                 Tests whether students need node-specific
##                                 ability/propensity parameters.
##
##   Model 4 — fully constrained: both students and items are pooled across nodes.
##                                 Equivalent to a unidimensional IRT model;
##                                 serves as the baseline comparison.
##
## Model comparison (AIC, BIC) addresses RQ1: are skip propensity and accuracy
## distinguishable latent traits? The node correlations address RQ2: how are they related?
##
## Output: results/models/ — model objects and summaries, plus model_comp.Rdata

source("02_irtree_prep.R")

# Results table: one row per model, columns for node correlations and fit indices
model_comp <- data.frame(
  model     = c("fully estimated", "item-constrained", "user-constrained", "fully constrained"),
  cor_theta = NA,  # correlation between student skip propensity and error propensity
  cor_beta  = NA,  # correlation between item skip threshold and item difficulty
  aic       = NA,
  bic       = NA
)

# ============================================================
# MODEL 1: Fully estimated IRTree
# Both students and items have separate random effects per node,
# allowing free estimation of the cross-node correlations.
# Formula: response ~ 1 + (0 + node | item_id) + (0 + node | user_id)
# ============================================================
cat("\n\nModel 1: fully estimated")
mod_tree <- try(glmer(response ~ 1 + (0 + factor(node) | item_id) + (0 + factor(node) | user_id),
                      data = dat_irt, family = "binomial"))

if (!inherits(mod_tree, "try-error")) {
  mod_summary <- summary(mod_tree)
  cor_theta   <- attr(VarCorr(mod_tree)$user_id, "correlation")[1, 2]  # theta1/theta2 correlation
  cor_beta    <- attr(VarCorr(mod_tree)$item_id, "correlation")[1, 2]  # beta1/beta2 correlation
  model_comp[model_comp$model == "fully estimated", c("cor_theta", "cor_beta", "aic", "bic")] <-
    list(cor_theta, cor_beta, AIC(mod_tree), BIC(mod_tree))
  print(model_comp[model_comp$model == "fully estimated", ])
  save(mod_tree,    file = file.path(models_dir, "mod_tree.Rdata"))
  save(mod_summary, file = file.path(models_dir, "sum_mod_tree.Rdata"))
  rm(mod_tree); gc()
} else {
  print("Model 1 estimation failed.")
}

# ============================================================
# MODEL 2: Item-constrained
# Items share a single random effect across both nodes (no node-specific
# item parameters). Students still have separate per-node effects.
# ============================================================
cat("\n\nModel 2: item-constrained")
mod_tree_con_item <- try(glmer(response ~ 1 + (1 | item_id) + (0 + factor(node) | user_id),
                               data = dat_irt, family = "binomial"))

if (!inherits(mod_tree_con_item, "try-error")) {
  mod_summary <- summary(mod_tree_con_item)
  cor_theta   <- attr(VarCorr(mod_tree_con_item)$user_id, "correlation")[1, 2]
  model_comp[model_comp$model == "item-constrained", c("cor_theta", "cor_beta", "aic", "bic")] <-
    list(cor_theta, NA, AIC(mod_tree_con_item), BIC(mod_tree_con_item))
  print(model_comp[model_comp$model == "item-constrained", ])
  save(mod_tree_con_item, file = file.path(models_dir, "mod_tree_con_item.Rdata"))
  save(mod_summary,       file = file.path(models_dir, "sum_mod_tree_con_item.Rdata"))
  rm(mod_tree_con_item); gc()
} else {
  print("Model 2 estimation failed.")
}

# ============================================================
# MODEL 3: User-constrained
# Students share a single random effect across nodes. Items still have
# separate per-node effects.
# ============================================================
cat("\n\nModel 3: user-constrained")
mod_tree_con_person <- try(glmer(response ~ 1 + (0 + factor(node) | item_id) + (1 | user_id),
                                 data = dat_irt, family = "binomial"))

if (!inherits(mod_tree_con_person, "try-error")) {
  mod_summary <- summary(mod_tree_con_person)
  cor_beta    <- attr(VarCorr(mod_tree_con_person)$item_id, "correlation")[1, 2]
  model_comp[model_comp$model == "user-constrained", c("cor_theta", "cor_beta", "aic", "bic")] <-
    list(NA, cor_beta, AIC(mod_tree_con_person), BIC(mod_tree_con_person))
  print(model_comp[model_comp$model == "user-constrained", ])
  save(mod_tree_con_person, file = file.path(models_dir, "mod_tree_con_person.Rdata"))
  save(mod_summary,         file = file.path(models_dir, "sum_mod_tree_con_person.Rdata"))
  rm(mod_tree_con_person); gc()
} else {
  print("Model 3 estimation failed.")
}

# ============================================================
# MODEL 4: Fully constrained (unidimensional baseline)
# Both students and items share a single effect across nodes, with a
# fixed node effect capturing the average difference in response rates
# between node 1 and node 2. Equivalent to a standard 1PL IRT model.
# ============================================================
cat("\n\nModel 4: fully constrained")
mod_tree_con <- try(glmer(response ~ 1 + factor(node) + (1 | item_id) + (1 | user_id),
                          data = dat_irt, family = "binomial"))

if (!inherits(mod_tree_con, "try-error")) {
  mod_summary <- summary(mod_tree_con)
  model_comp[model_comp$model == "fully constrained", c("cor_theta", "cor_beta", "aic", "bic")] <-
    list(NA, NA, AIC(mod_tree_con), BIC(mod_tree_con))
  print(model_comp[model_comp$model == "fully constrained", ])
  save(mod_tree_con, file = file.path(models_dir, "mod_tree_con.Rdata"))
  save(mod_summary,  file = file.path(models_dir, "sum_mod_tree_con.Rdata"))
  rm(mod_tree_con); gc()
} else {
  print("Model 4 estimation failed.")
}

# Save the full model comparison table
save(model_comp, file = file.path(models_dir, "model_comp.Rdata"))
