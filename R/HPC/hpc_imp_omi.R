#-----------------------------------------------------------------------------#
#                    Cluster RS imputation: OMI NO2 and O3                    #
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
rangerGrid <-  expand.grid(mtry = c(4, 8, 12, 16, 20),  
                           min.node.size = 5,
                           splitrule = "variance")


#### 2018 ----

# no2
omi_train <- read_csv(paste0(path1, "2018/omi_NO2.csv"))
omi_test <- dplyr::filter(omi_train, split=="test")
omi_train <- dplyr::filter(omi_train, split=="train") %>%
  dplyr::select(-split)

tic()
no2mod <- train(omi_no2 ~ .,
                method = "ranger",
                trControl = fitControl,
                tuneGrid = rangerGrid, 
                importance = "impurity",
                data = omi_train, 
                num.threads = detectCores()-1,
                num.trees = 300,
                seed=1234)
toc()
write_rds(no2mod, paste0(path2, "modomi_no2_2018.rds"))
omi_test$preds <- predict(no2mod, omi_test)
rmse <- with(omi_test, sqrt(mean((omi_no2-preds)^2)))
r2 <- with(omi_test, cor(omi_no2, preds)^2)
test_validation <- data.frame(model="omi no2", year=2018, method="OOB", 
                              rmse=getTrainPerf(no2mod)$TrainRMSE,
                              r2=getTrainPerf(no2mod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="omi no2", year=2018, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modomi_no2_2018.csv"))
varimp <- as.data.frame(varImp(no2mod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimpomi_no2_2018.csv"))
rm("omi_train", "omi_test", "no2mod", "test_validation", "varimp")

# O3
omi_train <- read_csv(paste0(path1, "2018/omi_O3.csv"))
omi_test <- dplyr::filter(omi_train, split=="test")
omi_train <- dplyr::filter(omi_train, split=="train") %>%
  dplyr::select(-split)

tic()
o3mod <- train(omi_o3 ~ .,
               method = "ranger",
               trControl = fitControl,
               tuneGrid = rangerGrid, 
               importance = "impurity",
               data = omi_train, 
               num.threads = detectCores()-1,
               num.trees = 300,
               seed=1234)
toc()
write_rds(o3mod, paste0(path2, "modomi_o3_2018.rds"))
omi_test$preds <- predict(o3mod, omi_test)
rmse <- with(omi_test, sqrt(mean((omi_o3-preds)^2)))
r2 <- with(omi_test, cor(omi_o3, preds)^2)
test_validation <- data.frame(model="omi o3", year=2018, method="OOB", 
                              rmse=getTrainPerf(o3mod)$TrainRMSE,
                              r2=getTrainPerf(o3mod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="omi o3", year=2018, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modomi_o3_2018.csv"))
varimp <- as.data.frame(varImp(o3mod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimpomi_o3_2018.csv"))
rm("omi_train", "omi_test", "o3mod", "test_validation", "varimp")


#### 2019 ----

# no2
omi_train <- read_csv(paste0(path1, "2019/omi_NO2.csv"))
omi_test <- dplyr::filter(omi_train, split=="test")
omi_train <- dplyr::filter(omi_train, split=="train") %>%
  dplyr::select(-split)

tic()
no2mod <- train(omi_no2 ~ .,
                method = "ranger",
                trControl = fitControl,
                tuneGrid = rangerGrid, 
                importance = "impurity",
                data = omi_train, 
                num.threads = detectCores()-1,
                num.trees = 300,
                seed=1234)
toc()
write_rds(no2mod, paste0(path2, "modomi_no2_2019.rds"))
omi_test$preds <- predict(no2mod, omi_test)
rmse <- with(omi_test, sqrt(mean((omi_no2-preds)^2)))
r2 <- with(omi_test, cor(omi_no2, preds)^2)
test_validation <- data.frame(model="omi no2", year=2019, method="OOB", 
                              rmse=getTrainPerf(no2mod)$TrainRMSE,
                              r2=getTrainPerf(no2mod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="omi no2", year=2019, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modomi_no2_2019.csv"))
varimp <- as.data.frame(varImp(no2mod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimpomi_no2_2019.csv"))
rm("omi_train", "omi_test", "no2mod", "test_validation", "varimp")

# O3
omi_train <- read_csv(paste0(path1, "2019/omi_O3.csv"))
omi_test <- dplyr::filter(omi_train, split=="test")
omi_train <- dplyr::filter(omi_train, split=="train") %>%
  dplyr::select(-split)

tic()
o3mod <- train(omi_o3 ~ .,
               method = "ranger",
               trControl = fitControl,
               tuneGrid = rangerGrid, 
               importance = "impurity",
               data = omi_train, 
               num.threads = detectCores()-1,
               num.trees = 300,
               seed=1234)
toc()
write_rds(o3mod, paste0(path2, "modomi_o3_2019.rds"))
omi_test$preds <- predict(o3mod, omi_test)
rmse <- with(omi_test, sqrt(mean((omi_o3-preds)^2)))
r2 <- with(omi_test, cor(omi_o3, preds)^2)
test_validation <- data.frame(model="omi o3", year=2019, method="OOB", 
                              rmse=getTrainPerf(o3mod)$TrainRMSE,
                              r2=getTrainPerf(o3mod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="omi o3", year=2019, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modomi_o3_2019.csv"))
varimp <- as.data.frame(varImp(o3mod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimpomi_o3_2019.csv"))
rm("omi_train", "omi_test", "o3mod", "test_validation", "varimp")



#### 2020 ----

# no2
omi_train <- read_csv(paste0(path1, "2020/omi_NO2.csv"))
omi_test <- dplyr::filter(omi_train, split=="test")
omi_train <- dplyr::filter(omi_train, split=="train") %>%
  dplyr::select(-split)

tic()
no2mod <- train(omi_no2 ~ .,
                method = "ranger",
                trControl = fitControl,
                tuneGrid = rangerGrid, 
                importance = "impurity",
                data = omi_train, 
                num.threads = detectCores()-1,
                num.trees = 300,
                seed=1234)
toc()
write_rds(no2mod, paste0(path2, "modomi_no2_2020.rds"))
omi_test$preds <- predict(no2mod, omi_test)
rmse <- with(omi_test, sqrt(mean((omi_no2-preds)^2)))
r2 <- with(omi_test, cor(omi_no2, preds)^2)
test_validation <- data.frame(model="omi no2", year=2020, method="OOB", 
                              rmse=getTrainPerf(no2mod)$TrainRMSE,
                              r2=getTrainPerf(no2mod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="omi no2", year=2020, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modomi_no2_2020.csv"))
varimp <- as.data.frame(varImp(no2mod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimpomi_no2_2020.csv"))
rm("omi_train", "omi_test", "no2mod", "test_validation", "varimp")

# O3
omi_train <- read_csv(paste0(path1, "2020/omi_O3.csv"))
omi_test <- dplyr::filter(omi_train, split=="test")
omi_train <- dplyr::filter(omi_train, split=="train") %>%
  dplyr::select(-split)

tic()
o3mod <- train(omi_o3 ~ .,
               method = "ranger",
               trControl = fitControl,
               tuneGrid = rangerGrid, 
               importance = "impurity",
               data = omi_train, 
               num.threads = detectCores()-1,
               num.trees = 300,
               seed=1234)
toc()
write_rds(o3mod, paste0(path2, "modomi_o3_2020.rds"))
omi_test$preds <- predict(o3mod, omi_test)
rmse <- with(omi_test, sqrt(mean((omi_o3-preds)^2)))
r2 <- with(omi_test, cor(omi_o3, preds)^2)
test_validation <- data.frame(model="omi o3", year=2020, method="OOB", 
                              rmse=getTrainPerf(o3mod)$TrainRMSE,
                              r2=getTrainPerf(o3mod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="omi o3", year=2020, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modomi_o3_2020.csv"))
varimp <- as.data.frame(varImp(o3mod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimpomi_o3_2020.csv"))
rm("omi_train", "omi_test", "o3mod", "test_validation", "varimp")
