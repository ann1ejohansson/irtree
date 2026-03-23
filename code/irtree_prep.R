## prep data for irtree

library(data.table)
library(lme4)

# get data
load("logs_clean.Rdata")

logs_clean_node1 <- logs_clean
logs_clean_node1$node <- 1
logs_clean_node1$response <- logs_clean_node1$q
logs_clean_node2 <- logs_clean
logs_clean_node2$node <- 2
logs_clean_node2$response <- ifelse(logs_clean_node2$correct_answered == 0, 1, 0)
dat_irt <- rbind(logs_clean_node1,  logs_clean_node2)
dat_irt <- dat_irt[order(dat_irt$id),]

rm(logs_clean, logs_clean_node1, logs_clean_node2); gc()
dat_irt <- data.table(dat_irt)

# filter users with at least 10 qm responses
users <- dat_irt[node == 1 & response == 1, .N, .(user_id)][N >= 10, user_id]
dat_irt <- dat_irt[user_id %in% users, ]
rm(users)


