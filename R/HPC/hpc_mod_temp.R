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
outcomevar <- "tavg"
ncores <- detectCores()
print(paste0("Number of cores available: ", ncores))

# Read data
tempdata <- read_csv(paste0(path_in, "database/temp_pred/tempdata.csv"), guess_max = 10000) %>%
  dplyr::select(-entity) %>%
  dplyr::filter(!is.na(tavg))

# Check missing and stations
if(isTRUE(any(!complete.cases(tempdata)))){
  warning("Missing data in training data. Please check")
}
print("Number of stations: ")
print(length(unique(tempdata$ID))) 

# Trend ----
set.seed(123)
mod <- rf_ranger(tempdata, outcomevar, ncores, fmod = T)
varimp <- cbind(varImp(mod, scale=F)$importance, varImp(mod, scale=T)$importance)
names(varimp) <- c("raw", "scaled")
write_rds(mod, paste0(path_out, "temp_model.rds"))
write.csv(varimp, paste0(path_out, "temp_varimp.csv"))

# Variogram ----

# Prepare
set.seed(123)
tempdata$res <- tempdata$tavg - predict(mod)
sfres <- st_as_sf(tempdata, coords = c("x", "y"), crs = 25831) %>%
  dplyr::select(tavg, res, ID, date, yday, julian) 

# Empirical
empvar <- variogram(res~as.factor(julian), sfres, cutoff = 120000, dX = 0)
tempdata$res <- NULL

# Fit pooled
fitvar <- vgm(psill=0.1, "Exp", range=80000, nugget=0.05)
fitvar <- fit.variogram(empvar, fitvar)
png(paste0(paste0(path_out, "temp_pooledvar.png")), res = 300, width = 2000, height = 1200)
plot(empvar, fitvar)
dev.off()
write_rds(fitvar, paste0(paste0(path_out, "temp_variog.rds")))
write_rds(sfres, paste0(paste0(path_out, "temp_resid.rds")))

# Empirical ST variogram
STIdata <- STIDF(as(st_geometry(sfres), "Spatial"),
                 time = sfres$date,
                 data = st_drop_geometry(sfres))
STSdata <- as(STIdata, "STSDF")
empvar_st <- variogramST(res~1, STSdata, tlags=0:5, cutoff = 120000)
png(paste0(paste0(path_out, "temp_STvar.png")), res = 300, width = 2000, height = 1200)
plot(empvar_st, map = F)
dev.off()

rm("empvar", "sfres", "mod", "varimp", "empvar_st")

# nested CV ----
set.seed(123)
nestedcv <- data.frame(ID = unique(tempdata$ID))
nestedcv$fold <- sample(rep(1:10, ceiling(nrow(nestedcv)/10)), nrow(nestedcv))
results <- data.frame()
for(k in 1:10){
  cat(paste0("\nNested CV: outer fold ", k, "\n"))
  
  # Train and test sets
  ID_it <- nestedcv$ID[nestedcv$fold %in% k]
  data_train <- tempdata[!tempdata$ID %in% ID_it,]
  data_test <- tempdata[tempdata$ID %in% ID_it,]
  
  # Fit model
  mod_it <- rf_ranger(data_train, outcomevar, ncores)
  
  # Compute out-of-sample predictions
  data_train$pred1 <- predict(mod_it)
  data_train$res1 <- data_train$tavg - data_train$pred1
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
  data_test <- dplyr::select(data_test, ID, date, yday, julian, k, tavg, pred1, pred2)
  results <- bind_rows(results, data_test)
  
  # Clean
  rm("ID_it", "data_train", "data_test", 
     "sfdata_train", "sfdata_test", 
     "krigepreds", "mod_it")
}
write_csv(results, paste0(path_out, "temp_perf.csv"))
rm("tempdata", "results", "nestedcv", "fitvar")


# Clean ----
rm(list = ls())