## fit irtree models

source("irtree_prep.R")

model_comp <- data.frame(model = c("fully estimated", "item-constrained", "user-constrained", "fully constrained"),
                         cor_theta = NA,
                         cor_beta = NA,
                         aic = NA,
                         bic = NA)
# model 1
# multidimensional random item model for linear response trees - freely estimated
cat("\n\n Model 1")
mod_tree <- try(glmer(response ~ 1 + (0 + factor(node) | item_id) + (0 + factor(node) | user_id),
                  data = dat_irt,
                  family = "binomial"))
if(class(mod_tree) != "try-error") {
  sum <- summary(mod_tree)
  cor_theta <- attr(VarCorr(mod_tree)$user_id, "correlation")[1, 2]
  cor_beta <- attr(VarCorr(mod_tree)$item_id, "correlation")[1, 2]
  aic <- AIC(mod_tree)
  bic <- BIC(mod_tree)
  model_comp[model_comp$model == "fully estimated",]$cor_theta <- cor_theta
  model_comp[model_comp$model == "fully estimated",]$cor_beta <- cor_beta
  model_comp[model_comp$model == "fully estimated",]$aic <- aic
  model_comp[model_comp$model == "fully estimated",]$bic <- bic

  print(model_comp[model_comp$model == "fully estimated",])

  save(mod_tree, file = "mod_tree.Rdata")
  save(sum, file = "sum_mod_tree.Rdata")
  rm(mod_tree); gc()
} else {
  print("Model estimation failed.")
}

# model 2
# multidimensional random item model for linear response trees - constrained item param
cat("\n\n Model 2")
mod_tree_con_item <- try(glmer(response ~ 1 + (1 | item_id) + (0 + factor(node) | user_id),
                           data = dat_irt,
                           family = "binomial"))
if(class(mod_tree_con_item) != "try-error") {
  sum <- summary(mod_tree_con_item)
  cor_theta <- attr(VarCorr(mod_tree_con_item)$user_id, "correlation")[1, 2]
  cor_beta <- NA
  aic <- AIC(mod_tree_con_item)
  bic <- BIC(mod_tree_con_item)
  model_comp[model_comp$model == "item-constrained",]$cor_theta <- cor_theta
  model_comp[model_comp$model == "item-constrained",]$cor_beta <- cor_beta
  model_comp[model_comp$model == "item-constrained",]$aic <- aic
  model_comp[model_comp$model == "item-constrained",]$bic <- bic

  print(model_comp[model_comp$model == "item-constrained",])

  save(mod_tree_con_item, file = "mod_tree_con_item.Rdata")
  save(sum, file = "sum_mod_tree_con_item.Rdata")
  rm(mod_tree_con_item); gc()
} else {
  print("Model estimation failed.")
}

# model 3
# multidimensional random item model for linear response trees - constrained person param
cat("\n\n Model 3")
mod_tree_con_person <- try(glmer(response ~ 1 + (0 + factor(node) | item_id) + (1 | user_id),
                             data = dat_irt,
                             family = "binomial"))
if(class(mod_tree_con_person) != "try=error") {
  sum <- summary(mod_tree_con_person)
  cor_theta <- NA
  cor_beta <- attr(VarCorr(mod_tree_con_person)$item_id, "correlation")[1, 2]
  aic <- AIC(mod_tree_con_person)
  bic <- BIC(mod_tree_con_person)
  model_comp[model_comp$model == "user-constrained",]$cor_theta <- cor_theta
  model_comp[model_comp$model == "user-constrained",]$cor_beta <- cor_beta
  model_comp[model_comp$model == "user-constrained",]$aic <- aic
  model_comp[model_comp$model == "user-constrained",]$bic <- bic

  print(model_comp[model_comp$model == "user-constrained",])

  save(mod_tree_con_person, file = "mod_tree_con_person.Rdata")
  save(sum, file = "sum_mod_tree_con_person.Rdata")
  rm(mod_tree_con_person); gc()
} else {
  print("Model estimation failed.")
}

# model 4
# multidimensional random item model for linear response trees - both nodes constrained
cat("\n\n Model 4")
mod_tree_con <- try(glmer(response ~ 1 + factor(node) + (1 | item_id) + (1 | user_id),
                      data = dat_irt,
                      family = "binomial"))
if(class(mod_tree_con) != "try-error") {
  sum <- summary(mod_tree_con)
  cor_theta <- NA
  cor_beta <- NA
  aic <- AIC(mod_tree_con)
  bic <- BIC(mod_tree_con)
  model_comp[model_comp$model == "fully constrained",]$cor_theta <- cor_theta
  model_comp[model_comp$model == "fully constrained",]$cor_beta <- cor_beta
  model_comp[model_comp$model == "fully constrained",]$aic <- aic
  model_comp[model_comp$model == "fully constrained",]$bic <- bic

  print(model_comp[model_comp$model == "fully constrained",])

  save(mod_tree_con, file = "mod_tree_con.Rdata")
  save(sum, file = "sum_mod_tree_con.Rdata")
  rm(mod_tree_con); gc()
} else {
  print("Model estimation failed.")
}

save(model_comp, file = "model_comp.Rdata")
