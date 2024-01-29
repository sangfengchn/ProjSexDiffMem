 # data.frame/tibble
library(tidyverse)
# prediction model
library(xgboost)
# detect number of core
library(parallel)

# >>>>>>>>>>>>>>>>>> RUN <<<<<<<<<<<<<<<<<<<<<
# parameters of command line
args = commandArgs(trailingOnly=TRUE)
# work path, e.g. /brain/babri_in/sangf/Projects/S_task-SexDifferences/work
wkPath <- args[1]
# dataset path, e.g. thickness_n1n5_Female
tmpPath <- file.path(wkPath, args[2])
# index of sample, e.g. 1, 2, ...
idxResample <- as.numeric(args[3])
# index of hyper parameters table, e.g. 1, 2, ...
idxGridSearch <- as.numeric(args[4])
# result path, e.g. 'xgboost_GridSearch'
resPath <- file.path(tmpPath, args[5])


# hyper parameters
# total 4356 combines: 11 * 11 * 6 * 6
# params <- expand.grid(
#   eta = 0.001,
#   max_depth = 2^seq(0, 10),
#   max_leaves = 2^seq(0, 10),
#   subsample = seq(0.5, 1, 0.1),
#   colsample_bytree = seq(0.5, 1, 0.1)
# )
params <- read.csv(file.path(wkPath, 'xgboost_hyperparameters.csv')) %>%
  as_tibble()

# train & test dataset
data.train <- read.csv(file.path(tmpPath, sprintf('dtrain_resample-%06d.csv', idxResample))) %>%
  as_tibble()
dtrain <- xgb.DMatrix(data = as.matrix(select(data.train, -y)), label = data.train$y)
# dtest <- xgb.DMatrix(data = as.matrix(select(data.test, -y)), label = data.test$y)
# dtrain <- xgb.DMatrix(file.path(tmpPath, sprintf('dtrain_resample-%06d.buffer', idxResample)))

# train model by best parameters
cv.model <- xgb.cv(
  params = as.list(params[idxGridSearch,]),
  data = dtrain,
  nrounds = 10000,
  objective = "reg:squarederror",
  early_stopping_rounds = 50,
  nfold = 10,
  nthread = detectCores(),
  verbose = TRUE
)

# save result of model
result.cv.model <- bind_cols(params[idxGridSearch,], cv.model$evaluation_log[cv.model$best_iteration])
write.csv(result.cv.model, file = file.path(resPath, sprintf('ResultCvModel_resample-%06d_GridSearch-%06d.csv', idxResample, idxGridSearch)), row.names = FALSE)
