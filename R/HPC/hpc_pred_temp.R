#-----------------------------------------------------------------------------#
#                  Cluster prediction: Temperature exposure                   #
#-----------------------------------------------------------------------------#

#### Prep ----
rm(list = ls())
library("tidyverse")
library("lubridate")
library("parallel")
library("gstat")
library("doParallel")
library("caret")
library("ranger")
library("stars")
library("ncdf4")

# Config HPC
pathin <- "****"
pathorig <- "****"
pathout <- "****"

# Read utils functions
source("****/CM_exposure/R/prediction/mod_algorithm.R")

# Read common spatial, focal, and temporal predictors
spstack <- read_rds(paste0(pathorig, "spatial/spstack.rds"))
focal <- read_rds(paste0(pathorig, "spatial/spstack_focal.rds"))
spstack <- c(spstack, focal) 
rm("focal")
calendar <- read_rds(paste0(pathorig, "calendar.rds"))

# Grid to predict residuals
resgrid <- st_as_sf(spstack[1], as_points = T, na.rm = TRUE)
resgrid <- st_geometry(resgrid)

# Load models
temp_trendmod <- read_rds(paste0(pathin, "/expo_mod2/temp_model.rds"))
temp_resmod <- read_rds(paste0(pathin, "/expo_mod2/temp_variog.rds"))
temp_resdata <- read_rds(paste0(pathin, "/expo_mod2/temp_resid.rds"))
vars <- row.names(varImp(temp_trendmod)$importance)
vars <- unique(vars)
spstack <- spstack[names(spstack) %in% vars]

#### 2018 ----
print("Predicting 2018 data...")
y <- 2018

for(m in 1:12){

  print(paste0("Month: ", m))

  # Prepare calendar and empty objects
  calendar_m <- dplyr::filter(calendar, year==y & month==m) %>%
    dplyr::select(date, yday, julian)
  pred_temp_m <- list()
  
  # Prepare NDVI and focal
  linkdate_m <- floor_date(calendar_m$date[1], "quarter")
  ndvi_m <- c(read_rds(paste0(pathorig, y, "/ndvi.rds")),
              read_rds(paste0(pathorig, y, "/ndvi_focal.rds"))) %>%
    dplyr::filter(date == linkdate_m) %>%
    adrop()

  # Loop by day
  for(yd in calendar_m$yday){
    print(paste0("Processing day: ", yd))
    yd2 <- yd - min(calendar_m$yday) +1 # counter to store monthly results
    
    # Residual kriging
    krige_temp_yd <- krige_res(temp_resdata[temp_resdata$date == calendar_m$date[calendar_m$yday == yd],],
                               resgrid, temp_resmod)

    # Trend
    preds_yd <- c(read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_2mtemp.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_totlprec.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_10mvwind.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_10muwind.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/lst_aquaday.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/lst_aquanight.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/lst_terraday.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/lst_terranight.tif"), yd))
    preds_yd <- adrop(preds_yd)

    # Add temporal and spatial variables
    preds_yd$julian <- calendar_m$julian[calendar_m$yday == yd]
    preds_yd <- c(preds_yd, st_warp(ndvi_m, preds_yd))
    preds_yd <- c(preds_yd, st_warp(spstack, preds_yd))

    # Execute prediction function
    res_temp_yd <- pred_exposure(preds_yd, temp_trendmod, krige_temp_yd)
    pred_temp_m[[yd2]] <- res_temp_yd
    
    rm("preds_yd", "res_temp_yd", "yd2", "krige_temp_yd")
  }

  print("Writing monthly results to disk...")

  # Write predictions
  preds_parse <- paste0("c(", paste0("pred_temp_m[[", 1:length(pred_temp_m), "]]", collapse = ", "), ", along = 3)")
  pred_temp_m <- eval(parse(text=preds_parse))
  pred_temp_m <- st_set_dimensions(pred_temp_m, 3, names = "time", values = calendar_m$date)
  write_stars_nc(pred_temp_m, as.character(min(calendar_m$date)-1), "degrees C", 
                 paste0(pathout, y, "/temp_", y, "_", str_pad(m, 2, pad="0"), ".nc"))

  # Clean
  rm("calendar_m", "linkdate_m", "ndvi_m", "pred_temp_m")
}
rm("y")


#### 2019 ----
print("Predicting 2019 data...")
y <- 2019

# Predictors
vars <- row.names(varImp(temp_trendmod)$importance)
vars <- unique(vars)
spstack <- spstack[names(spstack) %in% vars]

for(m in 1:12){
  
  print(paste0("Month: ", m))
  
  # Prepare calendar and empty objects
  calendar_m <- dplyr::filter(calendar, year==y & month==m) %>%
    dplyr::select(date, yday, julian)
  pred_temp_m <- list()
  
  # Prepare NDVI and focal
  linkdate_m <- floor_date(calendar_m$date[1], "quarter")
  ndvi_m <- c(read_rds(paste0(pathorig, y, "/ndvi.rds")),
              read_rds(paste0(pathorig, y, "/ndvi_focal.rds"))) %>%
    dplyr::filter(date == linkdate_m) %>%
    adrop()
  
  # Loop by day
  for(yd in calendar_m$yday){
    print(paste0("Processing day: ", yd))
    yd2 <- yd - min(calendar_m$yday) +1 # counter to store monthly results
    
    # Residual kriging
    krige_temp_yd <- krige_res(temp_resdata[temp_resdata$date == calendar_m$date[calendar_m$yday == yd],],
                               resgrid, temp_resmod)
    
    # Trend
    preds_yd <- c(read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_2mtemp.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_totlprec.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_10mvwind.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_10muwind.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/lst_aquaday.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/lst_aquanight.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/lst_terraday.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/lst_terranight.tif"), yd))
    preds_yd <- adrop(preds_yd)
    
    # Add temporal and spatial variables
    preds_yd$julian <- calendar_m$julian[calendar_m$yday == yd]
    preds_yd <- c(preds_yd, st_warp(ndvi_m, preds_yd))
    preds_yd <- c(preds_yd, st_warp(spstack, preds_yd))
    
    # Execute prediction function
    res_temp_yd <- pred_exposure(preds_yd, temp_trendmod, krige_temp_yd)
    pred_temp_m[[yd2]] <- res_temp_yd
    
    rm("preds_yd", "res_temp_yd", "yd2", "krige_temp_yd")
  }
  
  print("Writing monthly results to disk...")
  
  # Write predictions
  preds_parse <- paste0("c(", paste0("pred_temp_m[[", 1:length(pred_temp_m), "]]", collapse = ", "), ", along = 3)")
  pred_temp_m <- eval(parse(text=preds_parse))
  pred_temp_m <- st_set_dimensions(pred_temp_m, 3, names = "time", values = calendar_m$date)
  write_stars_nc(pred_temp_m, as.character(min(calendar_m$date)-1), "degrees C", 
                 paste0(pathout, y, "/temp_", y, "_", str_pad(m, 2, pad="0"), ".nc"))
  
  # Clean
  rm("calendar_m", "linkdate_m", "ndvi_m", "pred_temp_m")
}
rm("y")


#### 2020 ----
print("Predicting 2020 data...")
y <- 2020

# Predictors
vars <- row.names(varImp(temp_trendmod)$importance)
vars <- unique(vars)
spstack <- spstack[names(spstack) %in% vars]

for(m in 1:12){
  
  print(paste0("Month: ", m))
  
  # Prepare calendar and empty objects
  calendar_m <- dplyr::filter(calendar, year==y & month==m) %>%
    dplyr::select(date, yday, julian)
  pred_temp_m <- list()
  
  # Prepare NDVI and focal
  linkdate_m <- floor_date(calendar_m$date[1], "quarter")
  ndvi_m <- c(read_rds(paste0(pathorig, y, "/ndvi.rds")),
              read_rds(paste0(pathorig, y, "/ndvi_focal.rds"))) %>%
    dplyr::filter(date == linkdate_m) %>%
    adrop()
  
  # Loop by day
  for(yd in calendar_m$yday){
    print(paste0("Processing day: ", yd))
    yd2 <- yd - min(calendar_m$yday) +1 # counter to store monthly results
    
    # Residual kriging
    krige_temp_yd <- krige_res(temp_resdata[temp_resdata$date == calendar_m$date[calendar_m$yday == yd],],
                               resgrid, temp_resmod)
    
    # Trend
    preds_yd <- c(read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_2mtemp.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_totlprec.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_10mvwind.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_10muwind.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/lst_aquaday.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/lst_aquanight.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/lst_terraday.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/lst_terranight.tif"), yd))
    preds_yd <- adrop(preds_yd)
    
    # Add temporal and spatial variables
    preds_yd$julian <- calendar_m$julian[calendar_m$yday == yd]
    preds_yd <- c(preds_yd, st_warp(ndvi_m, preds_yd))
    preds_yd <- c(preds_yd, st_warp(spstack, preds_yd))
    
    # Execute prediction function
    res_temp_yd <- pred_exposure(preds_yd, temp_trendmod, krige_temp_yd)
    pred_temp_m[[yd2]] <- res_temp_yd
    
    rm("preds_yd", "res_temp_yd", "yd2", "krige_temp_yd")
  }
  
  print("Writing monthly results to disk...")
  
  # Write predictions
  preds_parse <- paste0("c(", paste0("pred_temp_m[[", 1:length(pred_temp_m), "]]", collapse = ", "), ", along = 3)")
  pred_temp_m <- eval(parse(text=preds_parse))
  pred_temp_m <- st_set_dimensions(pred_temp_m, 3, names = "time", values = calendar_m$date)
  write_stars_nc(pred_temp_m, as.character(min(calendar_m$date)-1), "degrees C", 
                 paste0(pathout, y, "/temp_", y, "_", str_pad(m, 2, pad="0"), ".nc"))
  
  # Clean
  rm("calendar_m", "linkdate_m", "ndvi_m", "pred_temp_m")
}
rm("y")



#### Finish ----
rm(list = ls())