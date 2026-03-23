## prep data for irtree

library(data.table)
library(lme4)

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
source(file.path(.this_dir, "00_config.R"))
rm(.this_dir)

# get data
load(data_path)
logs_clean <- get(data_object)

logs_clean_node1 <- logs_clean
logs_clean_node1$node <- 1
logs_clean_node1$response <- logs_clean_node1$q
logs_clean_node2 <- logs_clean
logs_clean_node2$node <- 2
logs_clean_node2$response <- ifelse(
  logs_clean_node2$correct_answered == 0,
  1,
  0
)
dat_irt <- rbind(logs_clean_node1, logs_clean_node2)
dat_irt <- dat_irt[order(dat_irt$id), ]

rm(logs_clean, logs_clean_node1, logs_clean_node2)
gc()
dat_irt <- data.table(dat_irt)

# filter users with at least min_skips skip responses (defined in 00_config.R)
users <- dat_irt[node == 1 & response == 1, .N, .(user_id)][
  N >= min_skips,
  user_id
]
dat_irt <- dat_irt[user_id %in% users, ]
rm(users)
