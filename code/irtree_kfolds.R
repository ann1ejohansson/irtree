## model robustness check
## k-folds cross validation

source("irtree_prep.R")

# make unique row identifier (for data checking purposes)
dat_irt$id_node <- paste0(dat_irt$id, "-", dat_irt$node)

# row indices
dat_irt[, i := 1:.N, .(user_id, node)]
dat_irt[, response_NA := response]

# final model data
k_folds <- data.frame(k = rep(1:10, each = 4),
                      perc_na = NA,
                      model = rep(c(1:4), times = 10),
                      aic = NA,
                      bic = NA,
                      rmse = NA)

removed = data.frame(k = numeric(),
                     id_node = numeric())

set.seed(1234)
for (k in 1:10) { # 10 folds
  cat("\n\nFold nr:", k)
  t_fold <- Sys.time()
  cat("\nStart time:"); print(t_fold)
  # for every person and node, set 10% responses missing
  # indices for to-be removed rows
  dat_val <- dat_irt[!is.na(response), .(i = sample(1:.N, ceiling(.N*.1))),.(user_id, node)]
  dat_val$remove = 1 # rows to be set to NA

  # merge data tables
  dat_fit <- dat_val[dat_irt, on = c("node", "user_id", "i")]
  # set rows (response) to NA
  dat_fit[is.na(remove), remove := 0]
  dat_fit[remove == 1, response_NA := NA]

  k_folds[k_folds$k == k,]$perc_na <- rep(sum(is.na(dat_fit$response_NA) & dat_fit$remove == 1, na.rm = T)/dat_fit[, .N], 4)

  removed_tmp = data.frame(k = k,
                          id_node = dat_fit[remove == 1, id_node])
  removed <- rbind(removed, removed_tmp)

  # fit tree models
  # fit mod 1 (full tree)
  t <- Sys.time()
  m <- try(glmer(response_NA ~ 1 + (0 + factor(node) | item_id) + (0 + factor(node) | user_id),
                    data = dat_fit,
                    family = "binomial"))
  if(class(m) != "try-error") {
    cat("\nt mod 1, fold", k, ": ", difftime(Sys.time(), t), units(difftime(Sys.time(), t)))
    k_folds[k_folds$k == k & k_folds$model == 1, ]$aic <- AIC(m)
    k_folds[k_folds$k == k & k_folds$model == 1, ]$bic <- BIC(m)

    p <- predict(m, newdata = dat_fit, allow.new.levels = TRUE)
    dat_fit[, pred_mod := plogis(p)]
    rmse <- dat_fit[remove == 1 & !is.na(response), sqrt(mean((response - pred_mod)^2))]
    k_folds[k_folds$k == k & k_folds$model == 1, ]$rmse <- rmse
    cat("\n")
    print(k_folds[k_folds$k == k & k_folds$model == 1, c("aic", "bic", "rmse")])
    model_sum <- summary(m)
    save(model_sum, file = paste0("~/research-collaboration/question-mark/k_folds_results/mod1_k", k, ".Rdata"))
    rm(m); gc()
  }

  # fit mod 2 (item constrained)
  t <- Sys.time()
  m <- try(glmer(response_NA ~ 1 + (1 | item_id) + (0 + factor(node) | user_id),
                             data = dat_fit,
                             family = "binomial"))
  if(class(m) != "try-error") {
    cat("\nt mod 2, fold", k, ": ", difftime(Sys.time(), t), units(difftime(Sys.time(), t)))
    k_folds[k_folds$k == k & k_folds$model == 2, ]$aic <- AIC(m)
    k_folds[k_folds$k == k & k_folds$model == 2, ]$bic <- BIC(m)

    p <- predict(m, newdata = dat_fit, allow.new.levels = TRUE)
    dat_fit[, pred_mod := plogis(p)]
    rmse <- dat_fit[remove == 1 & !is.na(response), sqrt(mean((response - pred_mod)^2))]
    k_folds[k_folds$k == k & k_folds$model == 2, ]$rmse <- rmse

    cat("\n")
    print(k_folds[k_folds$k == k & k_folds$model == 2, c("aic", "bic", "rmse")])
    model_sum <- summary(m)
    save(model_sum, file = paste0("~/research-collaboration/question-mark/k_folds_results/mod2_k", k, ".Rdata"))
    rm(m); gc()
  }


  # fit mod 3 (person constrained)
  t <- Sys.time()
  m <- try(glmer(response_NA ~ 1 + (0 + factor(node) | item_id) + (1 | user_id),
                               data = dat_fit,
                               family = "binomial"))
  if(class(m) != "try-error") {
    cat("\nt mod 3, fold", k, ": ", difftime(Sys.time(), t), units(difftime(Sys.time(), t)))
    k_folds[k_folds$k == k & k_folds$model == 3, ]$aic <- AIC(m)
    k_folds[k_folds$k == k & k_folds$model == 3, ]$bic <- BIC(m)

    p <-predict(m, newdata = dat_fit, allow.new.levels = TRUE)
    dat_fit[, pred_mod := plogis(p)]
    rmse <- dat_fit[remove == 1 & !is.na(response), sqrt(mean((response - pred_mod)^2))]
    k_folds[k_folds$k == k & k_folds$model == 3, ]$rmse <- rmse

    cat("\n")
    print(k_folds[k_folds$k == k & k_folds$model == 3, c("aic", "bic", "rmse")])
    model_sum <- summary(m)
    save(model_sum, file = paste0("~/research-collaboration/question-mark/k_folds_results/mod3_k", k, ".Rdata"))
    rm(m); gc()
  }


  # fit mod 4 (fully constrained)
  t <- Sys.time()
  m <- try(glmer(response_NA ~ 1 + factor(node) + (1 | item_id) + (1 | user_id),
                        data = dat_fit,
                        family = "binomial"))
  if(class(m) != "try-error") {
    cat("\nt mod 4, fold", k, ": ", difftime(Sys.time(), t), units(difftime(Sys.time(), t)))
    k_folds[k_folds$k == k & k_folds$model == 4, ]$aic <- AIC(m)
    k_folds[k_folds$k == k & k_folds$model == 4, ]$bic <- BIC(m)

    p <- predict(m, newdata = dat_fit[remove == 1, ], allow.new.levels = TRUE)
    dat_fit[remove == 1, pred_mod := plogis(p)]
    rmse <- dat_fit[remove == 1 & !is.na(response), sqrt(mean((response - pred_mod)^2))]
    k_folds[k_folds$k == k & k_folds$model == 4, ]$rmse <- rmse

    cat("\n")
    print(k_folds[k_folds$k == k & k_folds$model == 4, c("aic", "bic", "rmse")])
    model_sum <- summary(m)
    save(model_sum, file = paste0("~/research-collaboration/question-mark/k_folds_results/mod4_k", k, ".Rdata"))
    rm(m); gc()
  }

  cat("\nt fold:", k, ": ", difftime(Sys.time(), t_fold), units(difftime(Sys.time(), t_fold)))
  rm(dat_val, dat_fit, p, rmse); gc()

  save(k_folds, file = "~/research-collaboration/question-mark/k_folds_results/k_folds.Rdata")
}

# Average RMSE per model across folds
load("~/research-collaboration/question-mark/k_folds_results/k_folds.Rdata")
mean(k_folds[k_folds$model == 1, ]$rmse, na.rm = T)
mean(k_folds[k_folds$model == 2, ]$rmse, na.rm = T)
mean(k_folds[k_folds$model == 3, ]$rmse, na.rm = T)
mean(k_folds[k_folds$model == 4, ]$rmse, na.rm = T)

# CHECKS
# set.seed(1234)
# users <- sample(unique(dat_irt$user_id), 20)
# dat_irt <- dat_irt[dat_irt$user_id %in% users,]; gc()
# table(dat_fit$response, dat_fit$remove, dat_fit$node, useNA = "always")
# table(is.na(match(removed[removed$k == 4,]$id_node, removed[removed$k == 9,]$id_node)))



