# tibble data operation
library(tidyverse)
# predicition model
library(xgboost)
# detect number of core
library(parallel)

func_GridSearch <- function(params.list, dtrain) {
  best.results <- tibble()
  # pb <- txtProgressBar(style = 3)
  for (i in seq(length(params.list))) {
    logging::loginfo(paste('GridSearch', i))
    tmp.cv.model <- xgb.cv(
      params = params.list[[i]],
      data = dtrain,
      nrounds = 10000,
      objective = "reg:squarederror",
      early_stopping_rounds = 50,
      nfold = 10,
      verbose = TRUE
    )
    best.results <- best.results %>% bind_rows(tmp.cv.model$evaluation_log[tmp.cv.model$best_iteration])
    gc() 
    # setTxtProgressBar(pb, i/length(params.list))
  }
  # close(pb)
  return(best.results)
}

# >>>>>>>>>>>> RUN <<<<<<<<<<<<<
wkPath <- '/work/path'
anaPath <- file.path(wkPath, 'analysis', 'path')
data <- read.csv(file.path(anaPath, 'data', 'filename')) %>%
  as_tibble()

nRepeat <- 200
params <- expand.grid(
  nthread = detectCores(),
  eta = 0.001,
  max_depth = seq(1, 200, 10),
  max_leaves = c(10, 100, 500, 1000),
  subsample = seq(0.5, 1, 0.1),
  colsample_bytree = seq(0.5, 1, 0.1)
)
# params <- expand.grid(
#   nthread = detectCores(),
#   eta = 0.1,
#   max_depth = 10,
#   max_leaves = c(63, 100),
#   subsample = 0.5,
#   colsample_bytree = 0.5
# )

# for each sex, cognition and measures
tmpMea <- 'thickness'
tmpCog <- 'n1n5'
tmpSex <- 'Female'

logging::loginfo(paste(tmpMea, tmpCog, tmpSex, sep = '-'))

# tidy and select data
data.pred <- data %>%
  select(-contains('.....')) %>%
  select(participant_id, sex, contains(tmpCog), matches("aparc_HCP_MMP1_freesurfer_roi")) %>%
  filter(sex == tmpSex) %>% 
  # transformation data in each data
  mutate_at(vars(matches('residuals')), ~ (.x - min(.x)) / (max(.x) - min(.x))) %>%
  # mutate_at(vars(matches('residuals')), ~ (.x - mean(.x)) / sd(.x)) %>%
  select(contains('residuals'))
headers <- colnames(data.pred)
headers <- gsub('.residuals', '', gsub(paste0('.', tmpMea, '.aparc_HCP_MMP1_freesurfer_roi'), '', headers))
headers[1] <- 'y'
colnames(data.pred) <- headers

# resampling data
result.repeat <- tibble()
set.seed(123)
for (j in 1:nRepeat) {
  logging::loginfo(paste('Resampling', j))
  indices <- sample(1:nrow(data.pred), size = 0.80 * nrow(data.pred))
  
  # split train and test data
  data.train <- data.pred[indices,]
  data.test <- data.pred[-indices,]
  dtrain <- xgb.DMatrix(data = as.matrix(select(data.train, -y)), label = data.train$y)
  dtest <- xgb.DMatrix(data = as.matrix(select(data.test, -y)), label = data.test$y)
  
  # grid search
  params.list <- lapply(split(params, 1:nrow(params)), as.list)
  result.GridSearch <- func_GridSearch(params.list = params.list, dtrain = dtrain)
  result.GridSearch <- bind_cols(params, result.GridSearch) %>% arrange(test_rmse_mean)
  
  # train model by best parameters
  best.params <- as.list(result.GridSearch[1, 1:6])
  best.model <- xgb.train(
    data = dtrain,
    params = best.params,
    watchlist = list(train = dtrain, test = dtest),
    nround = 10000,
    objective = "reg:squarederror",
    early_stopping_rounds = 10,
    verbose = TRUE
  )
  
  # append performance of model
  result.repeat <- result.repeat %>%
    rbind(c(tmpMea, tmpCog, tmpSex, j, best.model$best_score, Metrics::rmse(data.test$y, rep(mean(data.train$y, nrow(data.test))))))
}
colnames(result.repeat) <- c('Measure', 'Cognition', 'Sex', 'Repeat', 'Model', 'Benchmark')
result.repeat <- as_tibble(result.repeat)
write.csv(result.repeat, file = sprintf('t04_desc-CompareBenchmark_rmse_%s_%s_%s.csv', tmpMea, tmpCog, tmpSex), row.names = FALSE)