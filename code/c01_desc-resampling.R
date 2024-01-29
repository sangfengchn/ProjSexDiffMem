rm(list = ls())
# data.frame/tibble
library(tidyverse)
# prediction model
library(xgboost)


# >>>>>>>>>>>>>>>>>>>> RUN <<<<<<<<<<<<<<<<<<<<
projPath <- here::here()
anaPath <- file.path(projPath, 'analysis', 'a02_desc-hcpmmp')
# wkPath <- file.path(projPath, 'analysis', 'a02_desc-hcpmmp', '_tab')
wkPath <- '~/Downloads/work'

dat <- read.csv(file.path(anaPath, '_tab', 't01_desc-structure_residuals_thickness.csv')) %>%
  as_tibble()

nResample <- 200

datasets <- expand.grid(
  sex = c('Female', 'Male'),
  cog = c('rodelay', 'n1n5'),
  mea = c('thickness')
) %>%
  as_tibble()

# for each sex, cognition and measures
for (i in 1:nrow(datasets)) {
  # i <- 1
  tmpMea <- as.character(datasets$mea[i])
  tmpCog <- as.character(datasets$cog[i])
  tmpSex <- as.character(datasets$sex[i])
  logging::loginfo(paste(tmpMea, tmpCog, tmpSex))
  
  # data folder
  tmpWkPath <- file.path(wkPath, paste(tmpMea, tmpCog, tmpSex, sep = '_'))
  if (!dir.exists(tmpWkPath)) {
    dir.create(tmpWkPath, recursive = TRUE)
  }
  
  # tidy and select data
  data.pred <- dat %>%
    dplyr::select(-contains('.....')) %>%
    dplyr::select(participant_id, sex, contains(tmpCog), matches("aparc_HCP_MMP1_freesurfer_roi")) %>%
    dplyr::filter(sex == tmpSex) %>% 
    # transformation data in each data
    mutate_at(vars(matches('residuals')), ~ (.x - min(.x)) / (max(.x) - min(.x))) %>%
    # mutate_at(vars(matches('residuals')), ~ (.x - mean(.x)) / sd(.x)) %>%
    dplyr::select(participant_id, contains('residuals'))
  
  headers <- colnames(data.pred)
  headers <- gsub('.residuals', '', gsub(paste0('.', tmpMea, '.aparc_HCP_MMP1_freesurfer_roi'), '', headers))
  headers[2] <- 'y'
  colnames(data.pred) <- headers
  
  # resampling data
  set.seed(123)
  for (j in 1:nResample) {
    logging::loginfo(paste('save data of resampling', j))
    indices <- sample(1:nrow(data.pred), size = 0.80 * nrow(data.pred))
    
    # split train and test data
    data.train <- data.pred[indices,]
    data.test <- data.pred[-indices,]
    
    write.csv(data.train, file.path(tmpWkPath, sprintf('dtrain_resample-%06d.csv', j)), row.names = FALSE)
    write.csv(data.test, file.path(tmpWkPath, sprintf('dtest_resample-%06d.csv', j)), row.names = FALSE)
    
    # # xgboost data format
    # dtrain <- xgb.DMatrix(data = as.matrix(select(data.train, -y)), label = data.train$y)
    # dtest <- xgb.DMatrix(data = as.matrix(select(data.test, -y)), label = data.test$y)
    # # save xgb.DMatrix
    # xgb.DMatrix.save(dtrain, file.path(tmpWkPath, sprintf('dtrain_resample-%06d.buffer', j)))
    # xgb.DMatrix.save(dtest, file.path(tmpWkPath, sprintf('dtest_resample-%06d.buffer', j)))
  }
}
logging::loginfo('Done.')
