# tibble data operation
library(tidyverse)
library(xgboost)

projPath <- '/Users/fengsang/Library/CloudStorage/OneDrive-mail.bnu.edu.cn/Projects/S_task-SexDifference'
anaPath <- file.path(projPath, 'analysis', 'a02_desc-hcpmmp')
workPath <- file.path(projPath, 'analysis', 'a02_desc-hcpmmp', '_tab')
args = commandArgs(trailingOnly=TRUE)

tmpMea <- args[1]
tmpCog <- args[2]
tmpSex <- args[3]
tmpResultPath <- sprintf('%s_%s_%s', tmpMea, tmpCog, tmpSex)

nResample <- 200
logging::loginfo(tmpResultPath)
data.gridsearch.cv <- read.csv(file.path(workPath, tmpResultPath, 'xgboost_GridSearch_v2', 'MergedResultCvModel.csv')) %>%
  as_tibble()

data.gridsearch.mean <- data.gridsearch.cv %>%
  group_by(GridSearch, eta, max_depth, max_leaves, subsample, colsample_bytree) %>%
  summarise(rmse_train_mean = mean(CvRmse_train_mean), rmse_train_std = sd(CvRmse_train_mean), rmse_test_mean = mean(CvRmse_test_mean), rmse_test_std = sd(CvRmse_test_mean)) %>%
  ungroup() %>%
  arrange(rmse_test_mean, rmse_test_std)
best.param <- as.list(data.gridsearch.mean[1, c('eta', 'max_depth', 'max_leaves', 'subsample', 'colsample_bytree')])

tmp.results <- data.frame() %>% as_tibble()
for (j in 1:nResample) {
  logging::loginfo(sprintf('resample %d', j))
  data.train <- read.csv(file.path(workPath, tmpResultPath, sprintf('dtrain_resample-%06d.csv', j))) %>%
    as_tibble()
  data.test <- read.csv(file.path(workPath, tmpResultPath, sprintf('dtest_resample-%06d.csv', j))) %>%
    as_tibble()
  dtrain <- xgb.DMatrix(data = as.matrix(select(data.train, -y)), label = data.train$y)
  dtest <- xgb.DMatrix(data = as.matrix(select(data.test, -y)), label = data.test$y)
  
  best.model <- xgb.train(
    data = dtrain,
    params = best.param,
    watchlist = list(train = dtrain, test = dtest),
    nround = 10000,
    objective = "reg:squarederror",
    early_stopping_rounds = 50,
    verbose = FALSE
  )
  tmp.results <- tmp.results %>%
    rbind(., c(j, unlist(best.model$evaluation_log[best.model$best_iteration]), Metrics::rmse(data.test$y, rep(mean(data.train$y), nrow(data.test)))))
}
colnames(tmp.results) <- c('Resample', 'iter', 'train_rmse', 'test_rmse', 'Benchmark_mean')
tmp.results <- as_tibble(tmp.results)
write.csv(tmp.results, file = file.path(workPath, tmpResultPath, 'xgboost_ValidatePerformance.csv'), row.names = FALSE)
logging::loginfo('Done.')
