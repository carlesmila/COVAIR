#-----------------------------------------------------------------------------#
#                            SHAP values  (in HPC)                            #
#-----------------------------------------------------------------------------#

library("ranger")
library("fastshap", lib.loc = "****")
library("tidyverse")
library("doParallel")

# parallel
registerDoParallel(10)

# path to models
pathmod <- "****"

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

# Temperature ----
mod_temp <- readRDS(paste0(pathmod, "temp_model.rds"))
data_temp <- mod_temp$trainingData %>%
  select(-.outcome) %>%
  mutate(across(where(is.character), as.factor)) %>%
  as.data.frame()
mod_temp <- mod_temp$finalModel$forest
shap_temp <- fastshap::explain(
  mod_temp, 
  X = data_temp,
  nsim = 20, 
  pred_wrapper = pfun,
  .parallel = TRUE
)
shap_temp <- shapviz(shap_temp, X = data_temp)
saveRDS(shap_temp, "****/temp_shap.rds")
rm("mod_temp", "data_temp", "shap_temp")

# PM2.5 ----
mod_pm25 <- readRDS(paste0(pathmod, "pm25_model.rds"))
data_pm25 <- mod_pm25$trainingData %>%
  select(-.outcome) %>%
  mutate(across(where(is.character), as.factor)) %>%
  as.data.frame()
mod_pm25 <- mod_pm25$finalModel$forest
shap_pm25 <- fastshap::explain(
  mod_pm25, 
  X = data_pm25,
  nsim = 20, 
  pred_wrapper = pfun,
  .parallel = TRUE
)
shap_pm25 <- shapviz(shap_pm25, X = data_pm25)
saveRDS(shap_pm25, "****/pm25_shap.rds")
rm("mod_pm25", "data_pm25", "shap_pm25")

# PM10 ----
mod_pm10 <- readRDS(paste0(pathmod, "pm10_model.rds"))
data_pm10 <- mod_pm10$trainingData %>%
  select(-.outcome) %>%
  mutate(across(where(is.character), as.factor)) %>%
  as.data.frame()
mod_pm10 <- mod_pm10$finalModel$forest
shap_pm10 <- fastshap::explain(
  mod_pm10, 
  X = data_pm10,
  nsim = 20, 
  pred_wrapper = pfun,
  .parallel = TRUE
)
shap_pm10 <- shapviz(shap_pm10, X = data_pm10)
saveRDS(shap_pm10, "****/pm10_shap.rds")
rm("mod_pm10", "data_pm10", "shap_pm10")

# NO2 ----
mod_no2 <- readRDS(paste0(pathmod, "no2_model.rds"))
data_no2 <- mod_no2$trainingData %>%
  select(-.outcome) %>%
  mutate(across(where(is.character), as.factor)) %>%
  as.data.frame()
mod_no2 <- mod_no2$finalModel$forest
shap_no2 <- fastshap::explain(
  mod_no2, 
  X = data_no2,
  nsim = 20, 
  pred_wrapper = pfun,
  .parallel = TRUE
)
shap_no2 <- shapviz(shap_no2, X = data_no2)
saveRDS(shap_no2, "****/no2_shap.rds")
rm("mod_no2", "data_no2", "shap_no2")

# O3 ----
mod_o3 <- readRDS(paste0(pathmod, "o3_model.rds"))
data_o3 <- mod_o3$trainingData %>%
  select(-.outcome) %>%
  mutate(across(where(is.character), function(x) as.integer(as.factor(x)))) %>%
  as.data.frame()
mod_o3 <- mod_o3$finalModel$forest
shap_o3 <- fastshap::explain(
  mod_o3, 
  X = data_o3,
  nsim = 20, 
  pred_wrapper = pfun,
  .parallel = TRUE
)
shap_o3 <- shapviz(shap_o3, X = data_o3)
saveRDS(shap_o3, "****/o3_shap.rds")