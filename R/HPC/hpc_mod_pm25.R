#-----------------------------------------------------------------------------#
#                       Cluster Air Pollution modelling                       #
#-----------------------------------------------------------------------------#

# Preparation ----
rm(list = ls())
library("tidyverse")
library("caret")
library("ranger")
library("mgcv")
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
outcomevar <- "PM2.5"
ncores <- detectCores()-1
print(paste0("Number of cores available: ", ncores +1))

# Read data
apdata <- read_csv(paste0(path_in, "database/ap_pred/apdata.csv"), guess_max = 10000) %>%
  dplyr::select(-st_type, -area_type, 
                -NO2, -O3, -PM10_pemisdist, -NOx_pemisdist, # Not relevant
                -CAMSrean_no2, -CAMSrean_o3, -CAMSrean_pm10, # Not relevant
                -CAMSanaly_no2, -CAMSanaly_pm10, -CAMSanaly_o3, # Not relevant 
                -omi_o3, -tropomi_o3, # Not relevant 
                -water, -water_focal, # Not enough variability
                -coast_dist) # Not enough variability

# Clean
apdata <- mutate(apdata, 
                 CAMS_pm25 =ifelse(!is.na(CAMSrean_pm25), CAMSrean_pm25, CAMSanaly_pm25)) %>%
  dplyr::select(-CAMSrean_pm25, -CAMSanaly_pm25) %>%
  dplyr::filter(date >= as.Date("2018-04-30") & (!is.na(PM10)|!is.na(PM2.5))) # No TROPOMI
if(isTRUE(any(!complete.cases(dplyr::select(apdata, -PM10, -PM2.5,
                                            -measure_PM10, -measure_PM2.5))))){
  warning("Missing data in training data other than PM2.5 and PM10. Please check")
}
print("Number of stations: ")
print(length(unique(apdata$ID))) 

# PM2.5 imputation ----
set.seed(123)
apdata_imp <- apdata
impdata <- apdata_imp[complete.cases(apdata_imp),] %>%
  dplyr::filter(measure_PM2.5 == measure_PM10)
impmod <- gam(PM2.5 ~ te(PM10, yday, k=c(3, 25)), data = impdata)
imps <- predict(impmod, apdata_imp)
apdata_imp$PM2.5 <- ifelse(!is.na(apdata_imp$PM2.5), apdata_imp$PM2.5, imps)
apdata_imp$PM10 <- NULL
apdata_imp$measure_PM2.5 <- NULL
apdata_imp$measure_PM10 <- NULL

# Check missing again
if(isTRUE(any(!complete.cases(apdata_imp)))){
  warning("Missing data in training data other than PM2.5 and PM10. Please check")
}

# fit trend ----
set.seed(123)
mod <- rf_ranger(apdata_imp, outcomevar, ncores, fmod = T)
varimp <- cbind(varImp(mod, scale=F)$importance, varImp(mod, scale=T)$importance)
names(varimp) <- c("raw", "scaled")
write_rds(mod, paste0(path_out, "pm25_model.rds"))
write.csv(varimp, paste0(path_out, "pm25_varimp.csv"))
rm("impdata", "impmod", "imps")

#  Variogram ----

# Prepare
apdata_imp$res <- apdata_imp$PM2.5 - predict(mod)
sfres <- st_as_sf(apdata_imp, coords = c("x", "y"), crs = 25831) %>%
  dplyr::select(PM2.5, res, ID, date, yday, julian) 
# Empirical
empvar <- variogram(res~as.factor(julian), sfres, cutoff = 120000, dX = 0)
apdata_imp$res <- NULL

# Fit pooled
fitvar <- vgm(psill=0.5, "Exp", range=80000, nugget=1.5)
fitvar <- fit.variogram(empvar, fitvar, fit.method = 6)
png(paste0(paste0(path_out, "pm25_pooledvar.png")), res = 300, width = 2000, height = 1200)
plot(empvar, fitvar)
dev.off()
write_rds(fitvar, paste0(paste0(path_out, "pm25_variog.rds")))
write_rds(sfres, paste0(paste0(path_out, "pm25_resid.rds")))

# Empirical ST variogram
STIdata <- STIDF(as(st_geometry(sfres), "Spatial"),
                 time = sfres$date,
                 data = st_drop_geometry(sfres))
STSdata <- as(STIdata, "STSDF")
empvar_st <- variogramST(res~1, STSdata, tlags=0:5, cutoff = 120000)
png(paste0(paste0(path_out, "pm25_STvar.png")), res = 300, width = 2000, height = 1200)
plot(empvar_st, map = F)
dev.off()

rm("empvar", "sfres", "mod", "varimp", "empvar_st")

# Nested CV ----
set.seed(123)
nestedcv <- data.frame(ID = unique(apdata$ID[!is.na(apdata$PM2.5)]))
nestedcv$fold <- sample(rep(1:10, ceiling(nrow(nestedcv)/10)), nrow(nestedcv))
results <- data.frame()
for(k in 1:10){
  cat(paste0("\nNested CV: outer fold ", k, "\n"))

  # Train and test sets
  ID_it <- nestedcv$ID[nestedcv$fold %in% k]
  data_train <- apdata[!apdata$ID %in% ID_it,]
  data_test <- apdata[apdata$ID %in% ID_it,]
  data_test <- dplyr::filter(data_test, !is.na(PM2.5))

  # Impute PM2.5 with PM10 data
  impdata_it <- data_train[complete.cases(data_train),] %>%
    dplyr::filter(measure_PM2.5 == measure_PM10)
  impmod_it <- gam(PM2.5 ~ te(PM10, yday, k=c(3, 25)), data = impdata_it)
  imps_it <- predict(impmod_it, data_train)
  data_train$PM2.5 <- ifelse(!is.na(data_train$PM2.5), data_train$PM2.5, imps_it)
  data_train$PM10 <- NULL
  data_train$measure_PM2.5 <- NULL
  data_train$measure_PM10 <- NULL

  # Fit model
  mod_it <- rf_ranger(data_train, outcomevar, ncores)

  # Compute out-of-sample predictions
  data_train$pred1 <- predict(mod_it)
  data_train$res1 <- data_train$PM2.5 - data_train$pred1
  data_test$pred1 <- predict(mod_it, newdata=data_test)
  
  # Data to ST
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
  data_test <- dplyr::select(data_test, ID, date, yday, julian, k, PM2.5, pred1, pred2)
  results <- bind_rows(results, data_test)
  
  # Clean
  rm("ID_it", "data_train", "data_test", 
     "sfdata_train", "sfdata_test", 
     "krigepreds", "mod_it", 
     "impdata_it", "imps_it", "impmod_it")
}
write_csv(results, paste0(path_out, "pm25_perf.csv"))
rm("apdata", "results", "nestedcv", "fitvar")

# Clean ----
rm(list = ls())