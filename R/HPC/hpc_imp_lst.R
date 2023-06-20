#-----------------------------------------------------------------------------#
#                   Cluster RS imputation: MAIAC terra and aqua               #
#-----------------------------------------------------------------------------#

# Prep analysis libraries and paths
library("tidyverse")
library("caret")
library("ranger")
library("tictoc")
library("parallel")
path1 <- "****/modis_imp/"
path2 <- "****/LST_imp/"
print(paste0("Started process with ", detectCores(), " cores."))

# Control and parameter grid
fitControl <- trainControl(method = "oob", returnData=F, trim=T)
rangerGrid <-  expand.grid(mtry = c(4, 8, 12), 
                           min.node.size = 5,
                           splitrule = "variance")

#### 2018 ----

# Terra day
lst_train <- read_csv(paste0(path1, "2018/dayLST_terra.csv"))
lst_test <- dplyr::filter(lst_train, split=="test")
lst_train <- dplyr::filter(lst_train, split=="train") %>%
  dplyr::select(-split)

tic()
terramod <- train(dayLST_terra ~ .,
                  method = "ranger",
                  trControl = fitControl,
                  tuneGrid = rangerGrid, 
                  importance = "impurity",
                  data = lst_train, 
                  num.threads = detectCores()-1,
                  num.trees=50,
                  seed=1234)
toc()
write_rds(terramod, paste0(path2, "modlst_terraday_2018.rds"))
lst_test$preds <- predict(terramod, lst_test)
rmse <- with(lst_test, sqrt(mean((dayLST_terra-preds)^2)))
r2 <- with(lst_test, cor(dayLST_terra, preds)^2)
test_validation <- data.frame(model="LST terra day", year=2018, method="OOB", 
                              rmse=getTrainPerf(terramod)$TrainRMSE,
                              r2=getTrainPerf(terramod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="LST terra day", year=2018, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modlst_terraday_2018.csv"))
varimp <- as.data.frame(varImp(terramod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimplst_terraday_2018.csv"))
rm("lst_train", "lst_test", "terramod", "test_validation", "varimp")

# Terra night
lst_train <- read_csv(paste0(path1, "2018/nightLST_terra.csv"))
lst_test <- dplyr::filter(lst_train, split=="test")
lst_train <- dplyr::filter(lst_train, split=="train") %>%
  dplyr::select(-split)

tic()
terramod <- train(nightLST_terra ~ .,
                  method = "ranger",
                  trControl = fitControl,
                  tuneGrid = rangerGrid, 
                  importance = "impurity",
                  data = lst_train, 
                  num.threads = detectCores()-1,
                  num.trees=50,
                  seed=1234)
toc()
write_rds(terramod, paste0(path2, "modlst_terranight_2018.rds"))
lst_test$preds <- predict(terramod, lst_test)
rmse <- with(lst_test, sqrt(mean((nightLST_terra-preds)^2)))
r2 <- with(lst_test, cor(nightLST_terra, preds)^2)
test_validation <- data.frame(model="LST terra night", year=2018, method="OOB", 
                              rmse=getTrainPerf(terramod)$TrainRMSE,
                              r2=getTrainPerf(terramod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="LST terra night", year=2018, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modlst_terranight_2018.csv"))
varimp <- as.data.frame(varImp(terramod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimplst_terranight_2018.csv"))
rm("lst_train", "lst_test", "terramod", "test_validation", "varimp")

# Aqua day
lst_train <- read_csv(paste0(path1, "2018/dayLST_aqua.csv"))
lst_test <- dplyr::filter(lst_train, split=="test")
lst_train <- dplyr::filter(lst_train, split=="train") %>%
  dplyr::select(-split)

tic()
aquamod <- train(dayLST_aqua ~ .,
                 method = "ranger",
                 trControl = fitControl,
                 tuneGrid = rangerGrid, 
                 importance = "impurity",
                 data = lst_train, 
                 num.threads = detectCores()-1,
                 num.trees=50,
                 seed=1234)
toc()
write_rds(aquamod, paste0(path2, "modlst_aquaday_2018.rds"))
lst_test$preds <- predict(aquamod, lst_test)
rmse <- with(lst_test, sqrt(mean((dayLST_aqua-preds)^2)))
r2 <- with(lst_test, cor(dayLST_aqua, preds)^2)
test_validation <- data.frame(model="LST aqua day", year=2018, method="OOB", 
                              rmse=getTrainPerf(aquamod)$TrainRMSE,
                              r2=getTrainPerf(aquamod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="LST aqua day", year=2018, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modlst_aquaday_2018.csv"))
varimp <- as.data.frame(varImp(aquamod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimplst_aquaday_2018.csv"))
rm("lst_train", "lst_test", "aquamod", "test_validation", "varimp")


# Aqua night
lst_train <- read_csv(paste0(path1, "2018/nightLST_aqua.csv"))
lst_test <- dplyr::filter(lst_train, split=="test")
lst_train <- dplyr::filter(lst_train, split=="train") %>%
  dplyr::select(-split)

tic()
aquamod <- train(nightLST_aqua ~ .,
                 method = "ranger",
                 trControl = fitControl,
                 tuneGrid = rangerGrid, 
                 importance = "impurity",
                 data = lst_train, 
                 num.threads = detectCores()-1,
                 num.trees = 50,
                 seed = 1234)
toc()
write_rds(aquamod, paste0(path2, "modlst_aquanight_2018.rds"))
lst_test$preds <- predict(aquamod, lst_test)
rmse <- with(lst_test, sqrt(mean((nightLST_aqua-preds)^2)))
r2 <- with(lst_test, cor(nightLST_aqua, preds)^2)
test_validation <- data.frame(model="LST aqua night", year=2018, method="OOB", 
                              rmse=getTrainPerf(aquamod)$TrainRMSE,
                              r2=getTrainPerf(aquamod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="LST aqua night", year=2018, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modlst_aquanight_2018.csv"))
varimp <- as.data.frame(varImp(aquamod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimplst_aquanight_2018.csv"))
rm("lst_train", "lst_test", "aquamod", "test_validation", "varimp")


#### 2019 ----

# Terra day
lst_train <- read_csv(paste0(path1, "2019/dayLST_terra.csv"))
lst_test <- dplyr::filter(lst_train, split=="test")
lst_train <- dplyr::filter(lst_train, split=="train") %>%
  dplyr::select(-split)

tic()
terramod <- train(dayLST_terra ~ .,
                  method = "ranger",
                  trControl = fitControl,
                  tuneGrid = rangerGrid, 
                  importance = "impurity",
                  data = lst_train, 
                  num.threads = detectCores()-1,
                  num.trees=50,
                  seed=1234)
toc()
write_rds(terramod, paste0(path2, "modlst_terraday_2019.rds"))
lst_test$preds <- predict(terramod, lst_test)
rmse <- with(lst_test, sqrt(mean((dayLST_terra-preds)^2)))
r2 <- with(lst_test, cor(dayLST_terra, preds)^2)
test_validation <- data.frame(model="LST terra day", year=2019, method="OOB", 
                              rmse=getTrainPerf(terramod)$TrainRMSE,
                              r2=getTrainPerf(terramod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="LST terra day", year=2019, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modlst_terraday_2019.csv"))
varimp <- as.data.frame(varImp(terramod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimplst_terraday_2019.csv"))
rm("lst_train", "lst_test", "terramod", "test_validation", "varimp")


# Terra night
lst_train <- read_csv(paste0(path1, "2019/nightLST_terra.csv"))
lst_test <- dplyr::filter(lst_train, split=="test")
lst_train <- dplyr::filter(lst_train, split=="train") %>%
  dplyr::select(-split)

tic()
terramod <- train(nightLST_terra ~ .,
                  method = "ranger",
                  trControl = fitControl,
                  tuneGrid = rangerGrid, 
                  importance = "impurity",
                  data = lst_train, 
                  num.threads = detectCores()-1,
                  num.trees=50,
                  seed=1234)
toc()
write_rds(terramod, paste0(path2, "modlst_terranight_2019.rds"))
lst_test$preds <- predict(terramod, lst_test)
rmse <- with(lst_test, sqrt(mean((nightLST_terra-preds)^2)))
r2 <- with(lst_test, cor(nightLST_terra, preds)^2)
test_validation <- data.frame(model="LST terra night", year=2019, method="OOB", 
                              rmse=getTrainPerf(terramod)$TrainRMSE,
                              r2=getTrainPerf(terramod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="LST terra night", year=2019, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modlst_terranight_2019.csv"))
varimp <- as.data.frame(varImp(terramod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimplst_terranight_2019.csv"))
rm("lst_train", "lst_test", "terramod", "test_validation", "varimp")

# Aqua day
lst_train <- read_csv(paste0(path1, "2019/dayLST_aqua.csv"))
lst_test <- dplyr::filter(lst_train, split=="test")
lst_train <- dplyr::filter(lst_train, split=="train") %>%
  dplyr::select(-split)

tic()
aquamod <- train(dayLST_aqua ~ .,
                 method = "ranger",
                 trControl = fitControl,
                 tuneGrid = rangerGrid, 
                 importance = "impurity",
                 data = lst_train, 
                 num.threads = detectCores()-1,
                 num.trees=50,
                 seed=1234)
toc()
write_rds(aquamod, paste0(path2, "modlst_aquaday_2019.rds"))
lst_test$preds <- predict(aquamod, lst_test)
rmse <- with(lst_test, sqrt(mean((dayLST_aqua-preds)^2)))
r2 <- with(lst_test, cor(dayLST_aqua, preds)^2)
test_validation <- data.frame(model="LST aqua day", year=2019, method="OOB", 
                              rmse=getTrainPerf(aquamod)$TrainRMSE,
                              r2=getTrainPerf(aquamod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="LST aqua day", year=2019, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modlst_aquaday_2019.csv"))
varimp <- as.data.frame(varImp(aquamod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimplst_aquaday_2019.csv"))
rm("lst_train", "lst_test", "aquamod", "test_validation", "varimp")


# Aqua night
lst_train <- read_csv(paste0(path1, "2019/nightLST_aqua.csv"))
lst_test <- dplyr::filter(lst_train, split=="test")
lst_train <- dplyr::filter(lst_train, split=="train") %>%
  dplyr::select(-split)

tic()
aquamod <- train(nightLST_aqua ~ .,
                 method = "ranger",
                 trControl = fitControl,
                 tuneGrid = rangerGrid, 
                 importance = "impurity",
                 data = lst_train, 
                 num.threads = detectCores()-1,
                 num.trees=50,
                 seed=1234)
toc()
write_rds(aquamod, paste0(path2, "modlst_aquanight_2019.rds"))
lst_test$preds <- predict(aquamod, lst_test)
rmse <- with(lst_test, sqrt(mean((nightLST_aqua-preds)^2)))
r2 <- with(lst_test, cor(nightLST_aqua, preds)^2)
test_validation <- data.frame(model="LST aqua night", year=2019, method="OOB", 
                              rmse=getTrainPerf(aquamod)$TrainRMSE,
                              r2=getTrainPerf(aquamod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="LST aqua night", year=2019, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modlst_aquanight_2019.csv"))
varimp <- as.data.frame(varImp(aquamod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimplst_aquanight_2019.csv"))
rm("lst_train", "lst_test", "aquamod", "test_validation", "varimp")


#### 2020 ----

# Terra day
lst_train <- read_csv(paste0(path1, "2020/dayLST_terra.csv"))
lst_test <- dplyr::filter(lst_train, split=="test")
lst_train <- dplyr::filter(lst_train, split=="train") %>%
  dplyr::select(-split)

tic()
terramod <- train(dayLST_terra ~ .,
                  method = "ranger",
                  trControl = fitControl,
                  tuneGrid = rangerGrid, 
                  importance = "impurity",
                  data = lst_train, 
                  num.threads = detectCores()-1,
                  num.trees=50,
                  seed=1234)
toc()
write_rds(terramod, paste0(path2, "modlst_terraday_2020.rds"))
lst_test$preds <- predict(terramod, lst_test)
rmse <- with(lst_test, sqrt(mean((dayLST_terra-preds)^2)))
r2 <- with(lst_test, cor(dayLST_terra, preds)^2)
test_validation <- data.frame(model="LST terra day", year=2020, method="OOB", 
                              rmse=getTrainPerf(terramod)$TrainRMSE,
                              r2=getTrainPerf(terramod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="LST terra day", year=2020, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modlst_terraday_2020.csv"))
varimp <- as.data.frame(varImp(terramod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimplst_terraday_2020.csv"))
rm("lst_train", "lst_test", "terramod", "test_validation", "varimp")

# Terra night
lst_train <- read_csv(paste0(path1, "2020/nightLST_terra.csv"))
lst_test <- dplyr::filter(lst_train, split=="test")
lst_train <- dplyr::filter(lst_train, split=="train") %>%
  dplyr::select(-split)

tic()
terramod <- train(nightLST_terra ~ .,
                  method = "ranger",
                  trControl = fitControl,
                  tuneGrid = rangerGrid, 
                  importance = "impurity",
                  data = lst_train, 
                  num.threads = detectCores()-1,
                  num.trees=50,
                  seed=1234)
toc()
write_rds(terramod, paste0(path2, "modlst_terranight_2020.rds"))
lst_test$preds <- predict(terramod, lst_test)
rmse <- with(lst_test, sqrt(mean((nightLST_terra-preds)^2)))
r2 <- with(lst_test, cor(nightLST_terra, preds)^2)
test_validation <- data.frame(model="LST terra night", year=2020, method="OOB", 
                              rmse=getTrainPerf(terramod)$TrainRMSE,
                              r2=getTrainPerf(terramod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="LST terra night", year=2020, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modlst_terranight_2020.csv"))
varimp <- as.data.frame(varImp(terramod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimplst_terranight_2020.csv"))
rm("lst_train", "lst_test", "terramod", "test_validation", "varimp")

# Aqua day
lst_train <- read_csv(paste0(path1, "2020/dayLST_aqua.csv"))
lst_test <- dplyr::filter(lst_train, split=="test")
lst_train <- dplyr::filter(lst_train, split=="train") %>%
  dplyr::select(-split)

tic()
aquamod <- train(dayLST_aqua ~ .,
                 method = "ranger",
                 trControl = fitControl,
                 tuneGrid = rangerGrid, 
                 importance = "impurity",
                 data = lst_train, 
                 num.threads = detectCores()-1,
                 num.trees=50,
                 seed=1234)
toc()
write_rds(aquamod, paste0(path2, "modlst_aquaday_2020.rds"))
lst_test$preds <- predict(aquamod, lst_test)
rmse <- with(lst_test, sqrt(mean((dayLST_aqua-preds)^2)))
r2 <- with(lst_test, cor(dayLST_aqua, preds)^2)
test_validation <- data.frame(model="LST aqua day", year=2020, method="OOB", 
                              rmse=getTrainPerf(aquamod)$TrainRMSE,
                              r2=getTrainPerf(aquamod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="LST aqua day", year=2020, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modlst_aquaday_2020.csv"))
varimp <- as.data.frame(varImp(aquamod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimplst_aquaday_2020.csv"))
rm("lst_train", "lst_test", "aquamod", "test_validation", "varimp")

# Aqua night
lst_train <- read_csv(paste0(path1, "2020/nightLST_aqua.csv"))
lst_test <- dplyr::filter(lst_train, split=="test")
lst_train <- dplyr::filter(lst_train, split=="train") %>%
  dplyr::select(-split)

tic()
aquamod <- train(nightLST_aqua ~ .,
                 method = "ranger",
                 trControl = fitControl,
                 tuneGrid = rangerGrid, 
                 importance = "impurity",
                 data = lst_train, 
                 num.threads = detectCores()-1,
                 num.trees=50,
                 seed=1234)
toc()
write_rds(aquamod, paste0(path2, "modlst_aquanight_2020.rds"))
lst_test$preds <- predict(aquamod, lst_test)
rmse <- with(lst_test, sqrt(mean((nightLST_aqua-preds)^2)))
r2 <- with(lst_test, cor(nightLST_aqua, preds)^2)
test_validation <- data.frame(model="LST aqua night", year=2020, method="OOB", 
                              rmse=getTrainPerf(aquamod)$TrainRMSE,
                              r2=getTrainPerf(aquamod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="LST aqua night", year=2020, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modlst_aquanight_2020.csv"))
varimp <- as.data.frame(varImp(aquamod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimplst_aquanight_2020.csv"))
rm("lst_train", "lst_test", "aquamod", "test_validation", "varimp")