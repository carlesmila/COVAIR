#-----------------------------------------------------------------------------#
#                   Cluster RS imputation: MAIAC terra and aqua               #
#-----------------------------------------------------------------------------#

# Prep analysis libraries and paths
library("tidyverse")
library("caret")
library("ranger")
library("tictoc")
library("parallel")
path1 <- "****"
path2 <- "****"
print(paste0("Started process with ", detectCores(), " cores."))


# Control and parameter grid
fitControl <- trainControl(method = "oob", returnData=F, trim=T)
rangerGrid <-  expand.grid(mtry = c(4, 8, 12, 16), min.node.size = 5,
                           splitrule = "variance")

#### 2018 ----

# Terra
maiac_train <- read_csv(paste0(path1, "2018/maiac_terra.csv"))
maiac_test <- dplyr::filter(maiac_train, split=="test")
maiac_train <- dplyr::filter(maiac_train, split=="train") %>%
  dplyr::select(-split)

tic()
terramod <- train(AOD55_terra ~ .,
                  method = "ranger",
                  trControl = fitControl,
                  tuneGrid = rangerGrid, 
                  importance = "impurity",
                  data = maiac_train, 
                  num.threads = detectCores()-1,
                  num.trees = 75,
                  seed=1234)
toc()
write_rds(terramod, paste0(path2, "modmaiac_terra_2018.rds"))
maiac_test$preds <- predict(terramod, maiac_test)
rmse <- with(maiac_test, sqrt(mean((AOD55_terra-preds)^2)))
r2 <- with(maiac_test, cor(AOD55_terra, preds)^2)
test_validation <- data.frame(model="maiac terra", year=2018, method="OOB", 
                              rmse=getTrainPerf(terramod)$TrainRMSE,
                              r2=getTrainPerf(terramod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="maiac terra", year=2018, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modmaiac_terra_2018.csv"))
varimp <- as.data.frame(varImp(terramod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimpmaiac_terra_2018.csv"))
rm("maiac_train", "maiac_test", "terramod", "test_validation", "varimp")

# Aqua
maiac_train <- read_csv(paste0(path1, "2018/maiac_aqua.csv"))
maiac_test <- dplyr::filter(maiac_train, split=="test")
maiac_train <- dplyr::filter(maiac_train, split=="train") %>%
  dplyr::select(-split)

tic()
aquamod <- train(AOD55_aqua ~ .,
                 method = "ranger",
                 trControl = fitControl,
                 tuneGrid = rangerGrid, 
                 importance = "impurity",
                 data = maiac_train, 
                 num.threads = detectCores()-1,
                 num.trees = 75,
                 seed=1234)
toc()
write_rds(aquamod, paste0(path2, "modmaiac_aqua_2018.rds"))
maiac_test$preds <- predict(aquamod, maiac_test)
rmse <- with(maiac_test, sqrt(mean((AOD55_aqua-preds)^2)))
r2 <- with(maiac_test, cor(AOD55_aqua, preds)^2)
test_validation <- data.frame(model="maiac aqua", year=2018, method="OOB", 
                              rmse=getTrainPerf(aquamod)$TrainRMSE,
                              r2=getTrainPerf(aquamod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="maiac aqua", year=2018, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modmaiac_aqua_2018.csv"))
varimp <- as.data.frame(varImp(aquamod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimpmaiac_aqua_2018.csv"))
rm("maiac_train", "maiac_test", "aquamod", "test_validation", "varimp")



#### 2019 ----

# Terra
maiac_train <- read_csv(paste0(path1, "2019/maiac_terra.csv"))
maiac_test <- dplyr::filter(maiac_train, split=="test")
maiac_train <- dplyr::filter(maiac_train, split=="train") %>%
  dplyr::select(-split)

tic()
terramod <- train(AOD55_terra ~ .,
                  method = "ranger",
                  trControl = fitControl,
                  tuneGrid = rangerGrid, 
                  importance = "impurity",
                  data = maiac_train, 
                  num.threads = detectCores()-1,
                  num.trees = 75,
                  seed=1234)
toc()
write_rds(terramod, paste0(path2, "modmaiac_terra_2019.rds"))
maiac_test$preds <- predict(terramod, maiac_test)
rmse <- with(maiac_test, sqrt(mean((AOD55_terra-preds)^2)))
r2 <- with(maiac_test, cor(AOD55_terra, preds)^2)
test_validation <- data.frame(model="maiac terra", year=2019, method="OOB", 
                              rmse=getTrainPerf(terramod)$TrainRMSE,
                              r2=getTrainPerf(terramod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="maiac terra", year=2019, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modmaiac_terra_2019.csv"))
varimp <- as.data.frame(varImp(terramod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimpmaiac_terra_2019.csv"))
rm("maiac_train", "maiac_test", "terramod", "test_validation", "varimp")

# Aqua
maiac_train <- read_csv(paste0(path1, "2019/maiac_aqua.csv"))
maiac_test <- dplyr::filter(maiac_train, split=="test")
maiac_train <- dplyr::filter(maiac_train, split=="train") %>%
  dplyr::select(-split)

tic()
aquamod <- train(AOD55_aqua ~ .,
                 method = "ranger",
                 trControl = fitControl,
                 tuneGrid = rangerGrid, 
                 importance = "impurity",
                 data = maiac_train, 
                 num.threads = detectCores()-1,
                 num.trees = 75,
                 seed=1234)
toc()
write_rds(aquamod, paste0(path2, "modmaiac_aqua_2019.rds"))
maiac_test$preds <- predict(aquamod, maiac_test)
rmse <- with(maiac_test, sqrt(mean((AOD55_aqua-preds)^2)))
r2 <- with(maiac_test, cor(AOD55_aqua, preds)^2)
test_validation <- data.frame(model="maiac aqua", year=2019, method="OOB", 
                              rmse=getTrainPerf(aquamod)$TrainRMSE,
                              r2=getTrainPerf(aquamod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="maiac aqua", year=2019, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modmaiac_aqua_2019.csv"))
varimp <- as.data.frame(varImp(aquamod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimpmaiac_aqua_2019.csv"))
rm("maiac_train", "maiac_test", "aquamod", "test_validation", "varimp")



#### 2020 ----

# Terra
maiac_train <- read_csv(paste0(path1, "2020/maiac_terra.csv"))
maiac_test <- dplyr::filter(maiac_train, split=="test")
maiac_train <- dplyr::filter(maiac_train, split=="train") %>%
  dplyr::select(-split)

tic()
terramod <- train(AOD55_terra ~ .,
                  method = "ranger",
                  trControl = fitControl,
                  tuneGrid = rangerGrid, 
                  importance = "impurity",
                  data = maiac_train, 
                  num.threads = detectCores()-1,
                  num.trees = 75,
                  seed=1234)
toc()
write_rds(terramod, paste0(path2, "modmaiac_terra_2020.rds"))
maiac_test$preds <- predict(terramod, maiac_test)
rmse <- with(maiac_test, sqrt(mean((AOD55_terra-preds)^2)))
r2 <- with(maiac_test, cor(AOD55_terra, preds)^2)
test_validation <- data.frame(model="maiac terra", year=2020, method="OOB", 
                              rmse=getTrainPerf(terramod)$TrainRMSE,
                              r2=getTrainPerf(terramod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="maiac terra", year=2020, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modmaiac_terra_2020.csv"))
varimp <- as.data.frame(varImp(terramod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimpmaiac_terra_2020.csv"))
rm("maiac_train", "maiac_test", "terramod", "test_validation", "varimp")

# Aqua
maiac_train <- read_csv(paste0(path1, "2020/maiac_aqua.csv"))
maiac_test <- dplyr::filter(maiac_train, split=="test")
maiac_train <- dplyr::filter(maiac_train, split=="train") %>%
  dplyr::select(-split)

tic()
aquamod <- train(AOD55_aqua ~ .,
                 method = "ranger",
                 trControl = fitControl,
                 tuneGrid = rangerGrid, 
                 importance = "impurity",
                 data = maiac_train, 
                 num.threads = detectCores()-1,
                 num.trees = 75,
                 seed=1234)
toc()
write_rds(aquamod, paste0(path2, "modmaiac_aqua_2020.rds"))
maiac_test$preds <- predict(aquamod, maiac_test)
rmse <- with(maiac_test, sqrt(mean((AOD55_aqua-preds)^2)))
r2 <- with(maiac_test, cor(AOD55_aqua, preds)^2)
test_validation <- data.frame(model="maiac aqua", year=2020, method="OOB", 
                              rmse=getTrainPerf(aquamod)$TrainRMSE,
                              r2=getTrainPerf(aquamod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="maiac aqua", year=2020, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modmaiac_aqua_2020.csv"))
varimp <- as.data.frame(varImp(aquamod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimpmaiac_aqua_2020.csv"))
rm("maiac_train", "maiac_test", "aquamod", "test_validation", "varimp")
