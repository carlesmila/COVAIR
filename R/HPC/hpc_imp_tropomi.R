#-----------------------------------------------------------------------------#
#                 Cluster RS imputation: TROPOMI NO2 and O3                   #
#-----------------------------------------------------------------------------#

# Prep analysis libraries and paths
library("tidyverse")
library("caret")
library("ranger")
library("tictoc")
library("parallel")
path1 <- "****/tropomi_imp/"
path2 <- "****/tropomi_imp/"
print(paste0("Started process with ", detectCores(), " cores."))

# Control and parameter grid
fitControl <- trainControl(method = "oob", returnData=F, trim=T)
rangerGrid <-  expand.grid(mtry = c(4, 8, 12, 16),  
                           min.node.size = 5,
                           splitrule = "variance")

#### 2018 ----

# no2
tropomi_train <- read_csv(paste0(path1, "2018/tropomi_NO2.csv"))
tropomi_test <- dplyr::filter(tropomi_train, split=="test")
tropomi_train <- dplyr::filter(tropomi_train, split=="train") %>%
  dplyr::select(-split)

tic()
no2mod <- train(tropo_no2 ~ .,
                  method = "ranger",
                  trControl = fitControl,
                  tuneGrid = rangerGrid, 
                  importance = "impurity",
                  data = tropomi_train, 
                  num.threads = detectCores()-1,
                  num.trees = 100,
                  seed=1234)
toc()
write_rds(no2mod, paste0(path2, "modtropomi_no2_2018.rds"))
tropomi_test$preds <- predict(no2mod, tropomi_test)
rmse <- with(tropomi_test, sqrt(mean((tropo_no2-preds)^2)))
r2 <- with(tropomi_test, cor(tropo_no2, preds)^2)
test_validation <- data.frame(model="tropomi no2", year=2018, method="OOB", 
                              rmse=getTrainPerf(no2mod)$TrainRMSE,
                              r2=getTrainPerf(no2mod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="tropomi no2", year=2018, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modtropomi_no2_2018.csv"))
varimp <- as.data.frame(varImp(no2mod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimptropomi_no2_2018.csv"))
rm("tropomi_train", "tropomi_test", "no2mod", "test_validation", "varimp")

# O3
tropomi_train <- read_csv(paste0(path1, "2018/tropomi_O3.csv"))
tropomi_test <- dplyr::filter(tropomi_train, split=="test")
tropomi_train <- dplyr::filter(tropomi_train, split=="train") %>%
  dplyr::select(-split)

tic()
o3mod <- train(tropo_o3 ~ .,
                 method = "ranger",
                 trControl = fitControl,
                 tuneGrid = rangerGrid, 
                 importance = "impurity",
                 data = tropomi_train, 
                 num.threads = detectCores()-1,
                 num.trees = 100,
                 seed=1234)
toc()
write_rds(o3mod, paste0(path2, "modtropomi_o3_2018.rds"))
tropomi_test$preds <- predict(o3mod, tropomi_test)
rmse <- with(tropomi_test, sqrt(mean((tropo_o3-preds)^2)))
r2 <- with(tropomi_test, cor(tropo_o3, preds)^2)
test_validation <- data.frame(model="tropomi o3", year=2018, method="OOB", 
                              rmse=getTrainPerf(o3mod)$TrainRMSE,
                              r2=getTrainPerf(o3mod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="tropomi o3", year=2018, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modtropomi_o3_2018.csv"))
varimp <- as.data.frame(varImp(o3mod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimptropomi_o3_2018.csv"))
rm("tropomi_train", "tropomi_test", "o3mod", "test_validation", "varimp")


#### 2019 ----

# no2
tropomi_train <- read_csv(paste0(path1, "2019/tropomi_NO2.csv"))
tropomi_test <- dplyr::filter(tropomi_train, split=="test")
tropomi_train <- dplyr::filter(tropomi_train, split=="train") %>%
  dplyr::select(-split)

tic()
no2mod <- train(tropo_no2 ~ .,
                method = "ranger",
                trControl = fitControl,
                tuneGrid = rangerGrid, 
                importance = "impurity",
                data = tropomi_train, 
                num.threads = detectCores()-1,
                num.trees = 100,
                seed=1234)
toc()
write_rds(no2mod, paste0(path2, "modtropomi_no2_2019.rds"))
tropomi_test$preds <- predict(no2mod, tropomi_test)
rmse <- with(tropomi_test, sqrt(mean((tropo_no2-preds)^2)))
r2 <- with(tropomi_test, cor(tropo_no2, preds)^2)
test_validation <- data.frame(model="tropomi no2", year=2019, method="OOB", 
                              rmse=getTrainPerf(no2mod)$TrainRMSE,
                              r2=getTrainPerf(no2mod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="tropomi no2", year=2019, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modtropomi_no2_2019.csv"))
varimp <- as.data.frame(varImp(no2mod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimptropomi_no2_2019.csv"))
rm("tropomi_train", "tropomi_test", "no2mod", "test_validation", "varimp")

# O3
tropomi_train <- read_csv(paste0(path1, "2019/tropomi_O3.csv"))
tropomi_test <- dplyr::filter(tropomi_train, split=="test")
tropomi_train <- dplyr::filter(tropomi_train, split=="train") %>%
  dplyr::select(-split)

tic()
o3mod <- train(tropo_o3 ~ .,
               method = "ranger",
               trControl = fitControl,
               tuneGrid = rangerGrid, 
               importance = "impurity",
               data = tropomi_train, 
               num.threads = detectCores()-1,
               num.trees = 100,
               seed=1234)
toc()
write_rds(o3mod, paste0(path2, "modtropomi_o3_2019.rds"))
tropomi_test$preds <- predict(o3mod, tropomi_test)
rmse <- with(tropomi_test, sqrt(mean((tropo_o3-preds)^2)))
r2 <- with(tropomi_test, cor(tropo_o3, preds)^2)
test_validation <- data.frame(model="tropomi o3", year=2019, method="OOB", 
                              rmse=getTrainPerf(o3mod)$TrainRMSE,
                              r2=getTrainPerf(o3mod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="tropomi o3", year=2019, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modtropomi_o3_2019.csv"))
varimp <- as.data.frame(varImp(o3mod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimptropomi_o3_2019.csv"))
rm("tropomi_train", "tropomi_test", "o3mod", "test_validation", "varimp")



#### 2020 ----

# no2
tropomi_train <- read_csv(paste0(path1, "2020/tropomi_NO2.csv"))
tropomi_test <- dplyr::filter(tropomi_train, split=="test")
tropomi_train <- dplyr::filter(tropomi_train, split=="train") %>%
  dplyr::select(-split)

tic()
no2mod <- train(tropo_no2 ~ .,
                method = "ranger",
                trControl = fitControl,
                tuneGrid = rangerGrid, 
                importance = "impurity",
                data = tropomi_train, 
                num.threads = detectCores()-1,
                num.trees = 100,
                seed=1234)
toc()
write_rds(no2mod, paste0(path2, "modtropomi_no2_2020.rds"))
tropomi_test$preds <- predict(no2mod, tropomi_test)
rmse <- with(tropomi_test, sqrt(mean((tropo_no2-preds)^2)))
r2 <- with(tropomi_test, cor(tropo_no2, preds)^2)
test_validation <- data.frame(model="tropomi no2", year=2020, method="OOB", 
                              rmse=getTrainPerf(no2mod)$TrainRMSE,
                              r2=getTrainPerf(no2mod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="tropomi no2", year=2020, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modtropomi_no2_2020.csv"))
varimp <- as.data.frame(varImp(no2mod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimptropomi_no2_2020.csv"))
rm("tropomi_train", "tropomi_test", "no2mod", "test_validation", "varimp")

# O3
tropomi_train <- read_csv(paste0(path1, "2020/tropomi_O3.csv"))
tropomi_test <- dplyr::filter(tropomi_train, split=="test")
tropomi_train <- dplyr::filter(tropomi_train, split=="train") %>%
  dplyr::select(-split)

tic()
o3mod <- train(tropo_o3 ~ .,
               method = "ranger",
               trControl = fitControl,
               tuneGrid = rangerGrid, 
               importance = "impurity",
               data = tropomi_train, 
               num.threads = detectCores()-1,
               num.trees = 100,
               seed=1234)
toc()
write_rds(o3mod, paste0(path2, "modtropomi_o3_2020.rds"))
tropomi_test$preds <- predict(o3mod, tropomi_test)
rmse <- with(tropomi_test, sqrt(mean((tropo_o3-preds)^2)))
r2 <- with(tropomi_test, cor(tropo_o3, preds)^2)
test_validation <- data.frame(model="tropomi o3", year=2020, method="OOB", 
                              rmse=getTrainPerf(o3mod)$TrainRMSE,
                              r2=getTrainPerf(o3mod)$TrainRsquared)
test_validation <- bind_rows(test_validation,
                             data.frame(model="tropomi o3", year=2020, 
                                        method="test", rmse=rmse, r2=r2))
write_csv(test_validation, paste0(path2, "modtropomi_o3_2020.csv"))
varimp <- as.data.frame(varImp(o3mod)$importance) %>%
  rownames_to_column()
write_csv(varimp, paste0(path2, "varimptropomi_o3_2020.csv"))
rm("tropomi_train", "tropomi_test", "o3mod", "test_validation", "varimp")
