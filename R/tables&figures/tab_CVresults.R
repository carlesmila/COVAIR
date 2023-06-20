#-----------------------------------------------------------------------------#
#                           Table 2: CV results                               #
#-----------------------------------------------------------------------------#

library("tidyverse")
library("lubridate")
library("xtable")

# PM2.5: no kriging ----
performance <- read_csv("outputs/expo_mod2/pm25_perf.csv") %>%
  mutate(year = year(date)) %>%
  rename(predicted = pred1)
results <- data.frame()
for(y in list(2018:2020, 2018, 2019, 2020)){
  
  # Filter by time
  qfiles <- dplyr::filter(performance, year %in% y)
  
  # intercept, slope, R2, RMSE
  allmod <- lm(PM2.5 ~ predicted, data = qfiles)
  intercept <- allmod$coefficients[1]
  slope <- allmod$coefficients[2]
  R2 <- summary(allmod)$r.squared
  RMSE <- with(qfiles, sqrt(mean((PM2.5 - predicted)^2)))
  
  # Spatial R2 and RMSE
  qfiles_spat <-  qfiles %>% 
    group_by(ID) %>%
    summarise(annual_PM2.5 = mean(PM2.5, na.rm=T),
              annual_predicted = mean(predicted))
  R2_spatial <- summary(lm(annual_PM2.5 ~ annual_predicted, data = qfiles_spat))$r.squared
  RMSE_spatial <- with(qfiles_spat, sqrt(mean((annual_PM2.5 - annual_predicted)^2)))
  
  # Temporal R2 and RMSE
  qfiles_temp <-  qfiles %>% 
    group_by(ID) %>%
    mutate(annual_PM2.5 = mean(PM2.5),
           annual_predicted = mean(predicted)) %>%
    group_by(year) %>%
    mutate(delta_PM2.5 = PM2.5 - annual_PM2.5,
           delta_predicted = predicted - annual_predicted)
  R2_temporal <- summary(lm(delta_PM2.5 ~ delta_predicted, data = qfiles_temp))$r.squared
  RMSE_temporal <- with(qfiles_temp, sqrt(mean((delta_PM2.5 - delta_predicted)^2)))
  
  # Results
  results_it <- data.frame(year = ifelse(length(y) == 1, as.character(y), "2018-2020"), 
                           RMSE = RMSE, R2 = R2, intercept = intercept, slope = slope,
                           RMSE_spatial = RMSE_spatial, R2_spatial = R2_spatial,
                           RMSE_temporal = RMSE_temporal, R2_temporal = R2_temporal)
  results <- bind_rows(results, results_it)
  
  # Clean
  rm("RMSE", "R2", "intercept", "slope", "allmod", 
     "qfiles", "qfiles_spat", "qfiles_temp", "results_it",
     "RMSE_spatial", "R2_spatial", "RMSE_temporal", "R2_temporal")  
}
pm25 <- bind_rows(data.frame(expo = "PM2.5"), results)
row.names(pm25) <- NULL
rm("results", "performance")


# PM10: with kriging ----
performance <- read_csv("outputs/expo_mod2/pm10_perf.csv") %>%
  mutate(year = year(date)) %>%
  rename(predicted = pred2)
results <- data.frame()
for(y in list(2018:2020, 2018, 2019, 2020)){
  
  # Filter by time
  qfiles <- dplyr::filter(performance, year %in% y)
  
  # intercept, slope, R2, RMSE
  allmod <- lm(PM10 ~ predicted, data = qfiles)
  intercept <- allmod$coefficients[1]
  slope <- allmod$coefficients[2]
  R2 <- summary(allmod)$r.squared
  RMSE <- with(qfiles, sqrt(mean((PM10 - predicted)^2)))
  
  # Spatial R2 and RMSE
  qfiles_spat <-  qfiles %>% 
    group_by(ID) %>%
    summarise(annual_PM10 = mean(PM10, na.rm=T),
              annual_predicted = mean(predicted))
  R2_spatial <- summary(lm(annual_PM10 ~ annual_predicted, data = qfiles_spat))$r.squared
  RMSE_spatial <- with(qfiles_spat, sqrt(mean((annual_PM10 - annual_predicted)^2)))
  
  # Temporal R2 and RMSE
  qfiles_temp <-  qfiles %>% 
    group_by(ID) %>%
    mutate(annual_PM10 = mean(PM10),
           annual_predicted = mean(predicted)) %>%
    group_by(year) %>%
    mutate(delta_PM10 = PM10 - annual_PM10,
           delta_predicted = predicted - annual_predicted)
  R2_temporal <- summary(lm(delta_PM10 ~ delta_predicted, data = qfiles_temp))$r.squared
  RMSE_temporal <- with(qfiles_temp, sqrt(mean((delta_PM10 - delta_predicted)^2)))
  
  # Results
  results_it <- data.frame(year = ifelse(length(y) == 1, as.character(y), "2018-2020"), 
                           RMSE = RMSE, R2 = R2, intercept = intercept, slope = slope,
                           RMSE_spatial = RMSE_spatial, R2_spatial = R2_spatial,
                           RMSE_temporal = RMSE_temporal, R2_temporal = R2_temporal)
  results <- bind_rows(results, results_it)
  
  # Clean
  rm("RMSE", "R2", "intercept", "slope", "allmod", 
     "qfiles", "qfiles_spat", "qfiles_temp", "results_it",
     "RMSE_spatial", "R2_spatial", "RMSE_temporal", "R2_temporal")  
}
pm10 <- bind_rows(data.frame(expo = "PM10"), results)
row.names(pm10) <- NULL
rm("results", "performance")


# NO2: with kriging ----
performance <- read_csv("outputs/expo_mod2/no2_perf.csv") %>%
  mutate(year = year(date)) %>%
  rename(predicted = pred2)
results <- data.frame()
for(y in list(2018:2020, 2018, 2019, 2020)){
  
  # Filter by time
  qfiles <- dplyr::filter(performance, year %in% y)
  
  # intercept, slope, R2, RMSE
  allmod <- lm(NO2 ~ predicted, data = qfiles)
  intercept <- allmod$coefficients[1]
  slope <- allmod$coefficients[2]
  R2 <- summary(allmod)$r.squared
  RMSE <- with(qfiles, sqrt(mean((NO2 - predicted)^2)))
  
  # Spatial R2 and RMSE
  qfiles_spat <-  qfiles %>% 
    group_by(ID) %>%
    summarise(annual_NO2 = mean(NO2, na.rm=T),
              annual_predicted = mean(predicted))
  R2_spatial <- summary(lm(annual_NO2 ~ annual_predicted, data = qfiles_spat))$r.squared
  RMSE_spatial <- with(qfiles_spat, sqrt(mean((annual_NO2 - annual_predicted)^2)))
  
  # Temporal R2 and RMSE
  qfiles_temp <-  qfiles %>% 
    group_by(ID) %>%
    mutate(annual_NO2 = mean(NO2),
           annual_predicted = mean(predicted)) %>%
    group_by(year) %>%
    mutate(delta_NO2 = NO2 - annual_NO2,
           delta_predicted = predicted - annual_predicted)
  R2_temporal <- summary(lm(delta_NO2 ~ delta_predicted, data = qfiles_temp))$r.squared
  RMSE_temporal <- with(qfiles_temp, sqrt(mean((delta_NO2 - delta_predicted)^2)))
  
  # Results
  results_it <- data.frame(year = ifelse(length(y) == 1, as.character(y), "2018-2020"), 
                           RMSE = RMSE, R2 = R2, intercept = intercept, slope = slope,
                           RMSE_spatial = RMSE_spatial, R2_spatial = R2_spatial,
                           RMSE_temporal = RMSE_temporal, R2_temporal = R2_temporal)
  results <- bind_rows(results, results_it)
  
  # Clean
  rm("RMSE", "R2", "intercept", "slope", "allmod", 
     "qfiles", "qfiles_spat", "qfiles_temp", "results_it",
     "RMSE_spatial", "R2_spatial", "RMSE_temporal", "R2_temporal")  
}
no2 <- bind_rows(data.frame(expo = "NO2"), results)
row.names(no2) <- NULL
rm("results", "performance")


# O3: with kriging ----
performance <- read_csv("outputs/expo_mod2/o3_perf.csv") %>%
  mutate(year = year(date)) %>%
  rename(predicted = pred2)
results <- data.frame()
for(y in list(2018:2020, 2018, 2019, 2020)){
  
  # Filter by time
  qfiles <- dplyr::filter(performance, year %in% y)
  
  # intercept, slope, R2, RMSE
  allmod <- lm(O3 ~ predicted, data = qfiles)
  intercept <- allmod$coefficients[1]
  slope <- allmod$coefficients[2]
  R2 <- summary(allmod)$r.squared
  RMSE <- with(qfiles, sqrt(mean((O3 - predicted)^2)))
  
  # Spatial R2 and RMSE
  qfiles_spat <-  qfiles %>% 
    group_by(ID) %>%
    summarise(annual_O3 = mean(O3, na.rm=T),
              annual_predicted = mean(predicted))
  R2_spatial <- summary(lm(annual_O3 ~ annual_predicted, data = qfiles_spat))$r.squared
  RMSE_spatial <- with(qfiles_spat, sqrt(mean((annual_O3 - annual_predicted)^2)))
  
  # Temporal R2 and RMSE
  qfiles_temp <-  qfiles %>% 
    group_by(ID) %>%
    mutate(annual_O3 = mean(O3),
           annual_predicted = mean(predicted)) %>%
    group_by(year) %>%
    mutate(delta_O3 = O3 - annual_O3,
           delta_predicted = predicted - annual_predicted)
  R2_temporal <- summary(lm(delta_O3 ~ delta_predicted, data = qfiles_temp))$r.squared
  RMSE_temporal <- with(qfiles_temp, sqrt(mean((delta_O3 - delta_predicted)^2)))
  
  # Results
  results_it <- data.frame(year = ifelse(length(y) == 1, as.character(y), "2018-2020"), 
                           RMSE = RMSE, R2 = R2, intercept = intercept, slope = slope,
                           RMSE_spatial = RMSE_spatial, R2_spatial = R2_spatial,
                           RMSE_temporal = RMSE_temporal, R2_temporal = R2_temporal)
  results <- bind_rows(results, results_it)
  
  # Clean
  rm("RMSE", "R2", "intercept", "slope", "allmod", 
     "qfiles", "qfiles_spat", "qfiles_temp", "results_it",
     "RMSE_spatial", "R2_spatial", "RMSE_temporal", "R2_temporal")  
}
o3 <- bind_rows(data.frame(expo = "O3"), results)
row.names(o3) <- NULL
rm("results", "performance")


# temp: with kriging ----
performance <- read_csv("outputs/expo_mod2/temp_perf.csv") %>%
  mutate(year = year(date)) %>%
  rename(predicted = pred2)
results <- data.frame()
for(y in list(2018:2020, 2018, 2019, 2020)){
  
  # Filter by time
  qfiles <- dplyr::filter(performance, year %in% y)
  
  # intercept, slope, R2, RMSE
  allmod <- lm(tavg ~ predicted, data = qfiles)
  intercept <- allmod$coefficients[1]
  slope <- allmod$coefficients[2]
  R2 <- summary(allmod)$r.squared
  RMSE <- with(qfiles, sqrt(mean((tavg - predicted)^2)))
  
  # Spatial R2 and RMSE
  qfiles_spat <-  qfiles %>% 
    group_by(ID) %>%
    summarise(annual_tavg = mean(tavg, na.rm=T),
              annual_predicted = mean(predicted))
  R2_spatial <- summary(lm(annual_tavg ~ annual_predicted, data = qfiles_spat))$r.squared
  RMSE_spatial <- with(qfiles_spat, sqrt(mean((annual_tavg - annual_predicted)^2)))
  
  # Temporal R2 and RMSE
  qfiles_temp <-  qfiles %>% 
    group_by(ID) %>%
    mutate(annual_tavg = mean(tavg),
           annual_predicted = mean(predicted)) %>%
    group_by(year) %>%
    mutate(delta_tavg = tavg - annual_tavg,
           delta_predicted = predicted - annual_predicted)
  R2_temporal <- summary(lm(delta_tavg ~ delta_predicted, data = qfiles_temp))$r.squared
  RMSE_temporal <- with(qfiles_temp, sqrt(mean((delta_tavg - delta_predicted)^2)))
  
  # Results
  results_it <- data.frame(year = ifelse(length(y) == 1, as.character(y), "2018-2020"), 
                           RMSE = RMSE, R2 = R2, intercept = intercept, slope = slope,
                           RMSE_spatial = RMSE_spatial, R2_spatial = R2_spatial,
                           RMSE_temporal = RMSE_temporal, R2_temporal = R2_temporal)
  results <- bind_rows(results, results_it)
  
  # Clean
  rm("RMSE", "R2", "intercept", "slope", "allmod", 
     "qfiles", "qfiles_spat", "qfiles_temp", "results_it",
     "RMSE_spatial", "R2_spatial", "RMSE_temporal", "R2_temporal")  
}
temp <- bind_rows(data.frame(expo = "temp"), results)
row.names(temp) <- NULL
rm("results", "performance")

# Table ----
tab2 <- bind_rows(temp, pm25, pm10, no2, o3)
print(xtable(tab2), include.rownames=FALSE)
