library(conflicted)
library(tidyverse)
library(mlr3verse)
library(arrow)

# workPath <- here::here('work', 'xgb_hcpmmp1')
workPath <- here::here('~/Downloads/xgb_hcpmmp1')

nResample <- 200
datasets <- expand.grid(
  sex = c('Female', 'Male'),
  cog = c('n1n5', 'rodelay'),
  mea = c('thickness')
) %>%
  as_tibble()

for (i in 1:nrow(datasets)) {
  # i <- 1
  tmpMea <- as.character(datasets$mea[i])
  tmpCog <- as.character(datasets$cog[i])
  tmpSex <- as.character(datasets$sex[i])
  
  tmpResultPath <- sprintf('%s_%s_%s', tmpMea, tmpCog, tmpSex)
  logging::loginfo(tmpResultPath)
  
  dat.res <- tibble()
  for (idxResample in 1:nResample) {
    # idxResample <- 1
    logging::loginfo(sprintf('Resample %d', idxResample))
    
    # load data
    dat.train <- read_csv_arrow(here::here(workPath, tmpResultPath, sprintf('dtrain_resample-%06d.csv', idxResample))) %>% 
      as_tibble()
    dat.test <- read_csv_arrow(here::here(workPath, tmpResultPath, sprintf('dtest_resample-%06d.csv', idxResample))) %>% 
      as_tibble()
    
    dat.task <- dplyr::bind_rows(dat.train, dat.test)

    # train model
    model.task <- as_task_regr(dat.task, target = 'y')
    model.lrn <- lrn('regr.lm')
    model.lrn$train(model.task, 1:nrow(dat.train))
    model.train.predict <- model.lrn$predict(model.task, 1:nrow(dat.train))
    model.test.predict <- model.lrn$predict(model.task, (nrow(dat.train)+1):nrow(dat.task))
    
    # performance of model
    dat.res <- dat.res %>% 
      bind_rows(list(
        Resample = idxResample, 
        train_rmse = Metrics::rmse(model.train.predict$truth, model.train.predict$response), 
        test_rmse = Metrics::rmse(model.test.predict$truth, model.test.predict$response)))
  }
  write_csv_arrow(dat.res, file = here::here(workPath, tmpResultPath, sprintf('LinearModel_ValidatePerformance.csv')))
}
logging::loginfo('Done.')
