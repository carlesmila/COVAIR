#-----------------------------------------------------------------------------#
#                       Cluster Air Pollution modelling                       #
#-----------------------------------------------------------------------------#

# Prepare ----
rm(list = ls())
library("tidyverse")
library("caret")
library("ranger")
library("parallel")
library("doParallel")
library("sf")
library("gstat")
library("spacetime")

# HPC config
path_in <- "****"
path_out <- "****"
library("CAST", lib.loc = "****")

# Read utils functions
source(paste0(path_in, "R/prediction/mod_algorithm.R"))

# Global objects
outcomevar <- "PM10"
ncores <- detectCores()
print(paste0("Number of cores available: ", ncores))

# Read data
apdata <- read_csv(paste0(path_in, "database/ap_pred/apdata.csv"), guess_max = 10000) %>%
  dplyr::select(-st_type, -area_type, 
                -measure_PM2.5, -measure_PM10, # Not relevant
                -NO2, -PM2.5, -O3, -PM2.5_pemisdist, -NOx_pemisdist, # Not relevant
                -CAMSrean_no2, -CAMSrean_o3, -CAMSrean_pm25, # Not relevant
                -CAMSanaly_no2, -CAMSanaly_pm25, -CAMSanaly_o3, # Not relevant 
                -omi_o3, -tropomi_o3, # Not relevant 
                -water, -water_focal, # Not enough variability
                -coast_dist) %>% # Not enough variability
  dplyr::filter(!is.na(PM10))

# Clean
apdata <- mutate(apdata, 
                 CAMS_pm10 =ifelse(!is.na(CAMSrean_pm10), CAMSrean_pm10, CAMSanaly_pm10)) %>%
  dplyr::select(-CAMSrean_pm10, -CAMSanaly_pm10) %>%
  dplyr::filter(date >= as.Date("2018-04-30")) # No TROPOMI
if(isTRUE(any(!complete.cases(apdata)))){
  warning("Missing data. Please check")
}
print("Number of stations: ")
print(length(unique(apdata$ID))) 

# fit trend ----
set.seed(123)
mod <- rf_ranger(apdata, outcomevar, ncores, fmod = T)
varimp <- cbind(varImp(mod, scale=F)$importance, varImp(mod, scale=T)$importance)
names(varimp) <- c("raw", "scaled")
write_rds(mod, paste0(path_out, "pm10_model.rds"))
write.csv(varimp, paste0(path_out, "pm10_varimp.csv"))

# Variogram ----

# Prepare
apdata$res <- apdata$PM10 - predict(mod)
sfres <- st_as_sf(apdata, coords = c("x", "y"), crs = 25831) %>%
  dplyr::select(PM10, res, ID, date, yday, julian) 
# Empirical
empvar <- variogram(res~as.factor(julian), sfres, cutoff = 120000, dX = 0)
apdata$res <- NULL

# Fit pooled
fitvar <- vgm(psill=2, "Exp", range=80000, nugget=4)
fitvar <- fit.variogram(empvar, fitvar, fit.method = 6)
png(paste0(paste0(path_out, "pm10_pooledvar.png")), res = 300, width = 2000, height = 1200)
plot(empvar, fitvar)
dev.off()
write_rds(fitvar, paste0(paste0(path_out, "pm10_variog.rds")))
write_rds(sfres, paste0(paste0(path_out, "pm10_resid.rds")))

# Empirical ST variogram
STIdata <- STIDF(as(st_geometry(sfres), "Spatial"),
                 time = sfres$date,
                 data = st_drop_geometry(sfres))
STSdata <- as(STIdata, "STSDF")
empvar_st <- variogramST(res~1, STSdata, tlags=0:5, cutoff = 120000)
png(paste0(paste0(path_out, "pm10_STvar.png")), res = 300, width = 2000, height = 1200)
plot(empvar_st, map = F)
dev.off()

rm("empvar", "sfres", "mod", "varimp", "empvar_st")


# nested CV ----
set.seed(123)
nestedcv <- data.frame(ID = unique(apdata$ID))
nestedcv$fold <- sample(rep(1:10, ceiling(nrow(nestedcv)/10)), nrow(nestedcv))
results <- data.frame()
for(k in 1:10){
  cat(paste0("\nNested CV: outer fold ", k, "\n"))

  # Train and test sets
  ID_it <- nestedcv$ID[nestedcv$fold %in% k]
  data_train <- apdata[!apdata$ID %in% ID_it,]
  data_test <- apdata[apdata$ID %in% ID_it,]

  # Fit model
  mod_it <- rf_ranger(data_train, outcomevar, ncores)

  # Compute out-of-sample predictions
  data_train$pred1 <- predict(mod_it)
  data_train$res1 <- data_train$PM10 - data_train$pred1
  data_test$pred1 <- predict(mod_it, newdata=data_test)

  # Data to sf
  sfdata_train <- st_as_sf(data_train, coords = c("x", "y"), crs = 25831) %>%
    dplyr::select(ID, res1, date)
  sfdata_test <- st_as_sf(data_test, coords = c("x", "y"), crs = 25831) %>%
    dplyr::select(ID, date)

  krigepreds <- map_df(unique(sfdata_test$date), multikrige,
                       trst = sfdata_train, tesf = sfdata_test, mod = fitvar)
  krigepreds <- krigepreds %>%
    dplyr::select(ID, date, var1.pred) %>%
    rename(krigedres = var1.pred)
  data_test <- left_join(data_test, krigepreds, by = c("ID", "date")) %>%
    mutate(pred2 = pred1 + krigedres)
  
  # Process
  data_test$k <- k
  data_test <- dplyr::select(data_test, ID, date, yday, julian, k, PM10, pred1, pred2)
  results <- bind_rows(results, data_test)

  # Clean
  rm("ID_it", "data_train", "data_test", 
     "sfdata_train", "sfdata_test",
     "krigepreds", "mod_it")
}
write_csv(results, paste0(path_out, "pm10_perf.csv"))
rm("apdata", "results", "nestedcv", "fitvar")

# Clean ----
rm(list = ls())