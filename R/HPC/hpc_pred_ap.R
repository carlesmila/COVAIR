#-----------------------------------------------------------------------------#
#                 Cluster prediction: Air pollution exposures                 #
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
source("****/mod_algorithm.R")

# Read common spatial, focal, and temporal predictors
spstack <- read_rds(paste0(pathorig, "spatial/spstack.rds"))
focal <- read_rds(paste0(pathorig, "spatial/spstack_focal.rds"))
spstack <- c(spstack, focal) %>%
  mutate(primary_dens = primary_dens + secondary_dens,
         primary_dens_focal = primary_dens_focal + secondary_dens_focal) %>%
  dplyr::select(-secondary_dens, -secondary_dens_focal)
rm("focal")
calendar <- read_rds(paste0(pathorig, "calendar.rds"))

# Grid to predict residuals
resgrid <- st_as_sf(spstack[1], as_points = T, na.rm = TRUE)
resgrid <- st_geometry(resgrid)

# Load models
pm10_trendmod <- read_rds(paste0(pathin, "/expo_mod2/pm10_model.rds"))
pm25_trendmod <- read_rds(paste0(pathin, "/expo_mod2/pm25_model.rds"))
no2_trendmod <- read_rds(paste0(pathin, "/expo_mod2/no2_model.rds"))
o3_trendmod <- read_rds(paste0(pathin, "/expo_mod2/o3_model.rds"))
pm10_resmod <- read_rds(paste0(pathin, "/expo_mod2/pm10_variog.rds"))
no2_resmod <- read_rds(paste0(pathin, "/expo_mod2/no2_variog.rds"))
o3_resmod <- read_rds(paste0(pathin, "/expo_mod2/o3_variog.rds"))
pm10_resdata <- read_rds(paste0(pathin, "/expo_mod2/pm10_resid.rds"))
no2_resdata <- read_rds(paste0(pathin, "/expo_mod2/no2_resid.rds"))
o3_resdata <- read_rds(paste0(pathin, "/expo_mod2/o3_resid.rds"))

#### 2018 ----
print("Predicting 2018 data...")
y <- 2018
ntli_y <- read_rds(paste0(pathin, "/resampled250/", y, "/ntli.rds"))

for(m in 5:12){

  print(paste0("Month: ", m))

  # Prepare calendar and empty objects
  calendar_m <- dplyr::filter(calendar, year==y & month==m) %>%
    dplyr::select(date, yday, julian, holiday, dust)
  pred_pm25_m <- list()
  pred_pm10_m <- list()
  pred_no2_m <- list()
  pred_o3_m <- list()

  # Prepare NDVI and focal, add ntli
  linkdate_m <- floor_date(calendar_m$date[1], "quarter")
  ndvi_m <- c(read_rds(paste0(pathorig, y, "/ndvi.rds")),
              read_rds(paste0(pathorig, y, "/ndvi_focal.rds"))) %>%
    dplyr::filter(date == linkdate_m) %>%
    adrop()
  ndvi_m <- c(ndvi_m, st_warp(ntli_y, ndvi_m))

  # Loop by day
  for(yd in calendar_m$yday){
    print(paste0("Processing day: ", yd))
    yd2 <- yd - min(calendar_m$yday) +1 # counter to store monthly results

    # Residual kriging
    krige_pm10_yd <- krige_res(pm10_resdata[pm10_resdata$date == calendar_m$date[calendar_m$yday == yd],],
                               resgrid, pm10_resmod)
    krige_no2_yd <- krige_res(no2_resdata[no2_resdata$date == calendar_m$date[calendar_m$yday == yd],],
                              resgrid, no2_resmod)
    krige_o3_yd <- krige_res(o3_resdata[o3_resdata$date == calendar_m$date[calendar_m$yday == yd],],
                             resgrid, o3_resmod)

    # Trend
    preds_yd <- c(read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_2mtemp.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_10muwind.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_10mvwind.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_totlprec.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_pbh.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/CAMSrean_no2.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/CAMSrean_o3.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/CAMSrean_pm10.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/CAMSrean_pm25.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/maiac_terra.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/maiac_aqua.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/omi_no2.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/omi_o3.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/tropomi_no2.tif"), yd-119), # Starts at 2018-04-30
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/tropomi_o3.tif"), yd-119)) # Starts at 2018-04-30
    preds_yd <- adrop(preds_yd)
    names(preds_yd) <- gsub("CAMSrean", "CAMS", names(preds_yd))

    # Add temporal and spatial variables
    preds_yd$julian <- calendar_m$julian[calendar_m$yday == yd]
    preds_yd$dust <- calendar_m$dust[calendar_m$yday == yd]
    preds_yd$holiday <- calendar_m$holiday[calendar_m$yday == yd]
    preds_yd <- c(preds_yd, st_warp(ndvi_m, preds_yd))
    preds_yd <- c(preds_yd, st_warp(spstack, preds_yd))

    # Execute prediction function: PM2.5
    res_pm25_yd <- pred_exposure(preds_yd, pm25_trendmod)
    pred_pm25_m[[yd2]] <- res_pm25_yd

    # Execute prediction function: PM10
    res_pm10_yd <- pred_exposure(preds_yd, pm10_trendmod, krige_pm10_yd)
    pred_pm10_m[[yd2]] <- res_pm10_yd

    # Execute prediction function: NO2
    res_no2_yd <- pred_exposure(preds_yd, no2_trendmod, krige_no2_yd)
    pred_no2_m[[yd2]] <- res_no2_yd

    # Execute prediction function: O3
    res_o3_yd <- pred_exposure(preds_yd, o3_trendmod, krige_o3_yd)
    pred_o3_m[[yd2]] <- res_o3_yd

    rm("preds_yd", "res_pm25_yd", "res_pm10_yd", "res_no2_yd", "res_o3_yd",
       "krige_pm10_yd", "krige_no2_yd", "krige_o3_yd", "yd2")
  }

  print("Writing monthly results to disk...")

  # Write predictions: PM2.5
  preds_parse <- paste0("c(", paste0("pred_pm25_m[[", 1:length(pred_pm25_m), "]]", collapse = ", "), ", along = 3)")
  pred_pm25_m <- eval(parse(text=preds_parse))
  pred_pm25_m <- st_set_dimensions(pred_pm25_m, 3, names = "time", values = calendar_m$date)
  write_stars_nc(pred_pm25_m, as.character(min(calendar_m$date)-1), "ug/m3",
                 paste0(pathout, y, "/pm25_", y, "_", str_pad(m, 2, pad="0"), ".nc"))

  # Write predictions: PM10
  preds_parse <- paste0("c(", paste0("pred_pm10_m[[", 1:length(pred_pm10_m), "]]", collapse = ", "), ", along = 3)")
  pred_pm10_m <- eval(parse(text=preds_parse))
  pred_pm10_m <- st_set_dimensions(pred_pm10_m, 3, names = "time", values = calendar_m$date)
  write_stars_nc(pred_pm10_m, as.character(min(calendar_m$date)-1), "ug/m3",
                 paste0(pathout, y, "/pm10_", y, "_", str_pad(m, 2, pad="0"), ".nc"))

  # Write predictions: NO2
  preds_parse <- paste0("c(", paste0("pred_no2_m[[", 1:length(pred_no2_m), "]]", collapse = ", "), ", along = 3)")
  pred_no2_m <- eval(parse(text=preds_parse))
  pred_no2_m <- st_set_dimensions(pred_no2_m, 3, names = "time", values = calendar_m$date)
  write_stars_nc(pred_no2_m, as.character(min(calendar_m$date)-1), "ug/m3",
                 paste0(pathout, y, "/no2_", y, "_", str_pad(m, 2, pad="0"), ".nc"))

  # Write predictions: O3
  preds_parse <- paste0("c(", paste0("pred_o3_m[[", 1:length(pred_o3_m), "]]", collapse = ", "), ", along = 3)")
  pred_o3_m <- eval(parse(text=preds_parse))
  pred_o3_m <- st_set_dimensions(pred_o3_m, 3, names = "time", values = calendar_m$date)
  write_stars_nc(pred_o3_m, as.character(min(calendar_m$date)-1), "ug/m3",
                 paste0(pathout, y, "/o3_", y, "_", str_pad(m, 2, pad="0"), ".nc"))

  # Clean
  rm("calendar_m", "linkdate_m", "ndvi_m",
     "pred_pm25_m", "pred_pm10_m", "pred_no2_m", "pred_o3_m")
}
rm("y", "ntli_y")


#### 2019 ----
print("Predicting 2019 data...")
y <- 2019
ntli_y <- read_rds(paste0(pathin, "/resampled250/", y, "/ntli.rds"))

for(m in 1:12){

  print(paste0("Month: ", m))

  # Prepare calendar and empty objects
  calendar_m <- dplyr::filter(calendar, year==y & month==m) %>%
    dplyr::select(date, yday, julian, holiday, dust)
  pred_pm25_m <- list()
  pred_pm10_m <- list()
  pred_no2_m <- list()
  pred_o3_m <- list()

  # Prepare NDVI and focal, add ntli
  linkdate_m <- floor_date(calendar_m$date[1], "quarter")
  ndvi_m <- c(read_rds(paste0(pathorig, y, "/ndvi.rds")),
              read_rds(paste0(pathorig, y, "/ndvi_focal.rds"))) %>%
    dplyr::filter(date == linkdate_m) %>%
    adrop()
  ndvi_m <- c(ndvi_m, st_warp(ntli_y, ndvi_m))

  # Loop by day
  for(yd in calendar_m$yday){
    print(paste0("Processing day: ", yd))
    yd2 <- yd - min(calendar_m$yday) +1 # counter to store monthly results

    # Residual kriging
    krige_pm10_yd <- krige_res(pm10_resdata[pm10_resdata$date == calendar_m$date[calendar_m$yday == yd],],
                               resgrid, pm10_resmod)
    krige_no2_yd <- krige_res(no2_resdata[no2_resdata$date == calendar_m$date[calendar_m$yday == yd],],
                              resgrid, no2_resmod)
    krige_o3_yd <- krige_res(o3_resdata[o3_resdata$date == calendar_m$date[calendar_m$yday == yd],],
                             resgrid, o3_resmod)

    # Trend
    preds_yd <- c(read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_2mtemp.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_10muwind.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_10mvwind.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_totlprec.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_pbh.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/CAMSanaly_no2.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/CAMSanaly_o3.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/CAMSanaly_pm10.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/CAMSanaly_pm25.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/maiac_terra.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/maiac_aqua.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/omi_no2.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/omi_o3.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/tropomi_no2.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/tropomi_o3.tif"), yd))
    preds_yd <- adrop(preds_yd)
    names(preds_yd) <- gsub("CAMSanaly", "CAMS", names(preds_yd))

    # Add temporal and spatial variables
    preds_yd$julian <- calendar_m$julian[calendar_m$yday == yd]
    preds_yd$dust <- calendar_m$dust[calendar_m$yday == yd]
    preds_yd$holiday <- calendar_m$holiday[calendar_m$yday == yd]
    preds_yd <- c(preds_yd, st_warp(ndvi_m, preds_yd))
    preds_yd <- c(preds_yd, st_warp(spstack, preds_yd))

    # Execute prediction function: PM2.5
    res_pm25_yd <- pred_exposure(preds_yd, pm25_trendmod)
    pred_pm25_m[[yd2]] <- res_pm25_yd

    # Execute prediction function: PM10
    res_pm10_yd <- pred_exposure(preds_yd, pm10_trendmod, krige_pm10_yd)
    pred_pm10_m[[yd2]] <- res_pm10_yd

    # Execute prediction function: NO2
    res_no2_yd <- pred_exposure(preds_yd, no2_trendmod, krige_no2_yd)
    pred_no2_m[[yd2]] <- res_no2_yd

    # Execute prediction function: O3
    res_o3_yd <- pred_exposure(preds_yd, o3_trendmod, krige_o3_yd)
    pred_o3_m[[yd2]] <- res_o3_yd

    rm("preds_yd", "res_pm25_yd", "res_pm10_yd", "res_no2_yd", "res_o3_yd",
       "krige_pm10_yd", "krige_no2_yd", "krige_o3_yd", "yd2")
  }

  print("Writing monthly results to disk...")

  # Write predictions: PM2.5
  preds_parse <- paste0("c(", paste0("pred_pm25_m[[", 1:length(pred_pm25_m), "]]", collapse = ", "), ", along = 3)")
  pred_pm25_m <- eval(parse(text=preds_parse))
  pred_pm25_m <- st_set_dimensions(pred_pm25_m, 3, names = "time", values = calendar_m$date)
  write_stars_nc(pred_pm25_m, as.character(min(calendar_m$date)-1), "ug/m3",
                 paste0(pathout, y, "/pm25_", y, "_", str_pad(m, 2, pad="0"), ".nc"))

  # Write predictions: PM10
  preds_parse <- paste0("c(", paste0("pred_pm10_m[[", 1:length(pred_pm10_m), "]]", collapse = ", "), ", along = 3)")
  pred_pm10_m <- eval(parse(text=preds_parse))
  pred_pm10_m <- st_set_dimensions(pred_pm10_m, 3, names = "time", values = calendar_m$date)
  write_stars_nc(pred_pm10_m, as.character(min(calendar_m$date)-1), "ug/m3",
                 paste0(pathout, y, "/pm10_", y, "_", str_pad(m, 2, pad="0"), ".nc"))

  # Write predictions: NO2
  preds_parse <- paste0("c(", paste0("pred_no2_m[[", 1:length(pred_no2_m), "]]", collapse = ", "), ", along = 3)")
  pred_no2_m <- eval(parse(text=preds_parse))
  pred_no2_m <- st_set_dimensions(pred_no2_m, 3, names = "time", values = calendar_m$date)
  write_stars_nc(pred_no2_m, as.character(min(calendar_m$date)-1), "ug/m3",
                 paste0(pathout, y, "/no2_", y, "_", str_pad(m, 2, pad="0"), ".nc"))

  # Write predictions: O3
  preds_parse <- paste0("c(", paste0("pred_o3_m[[", 1:length(pred_o3_m), "]]", collapse = ", "), ", along = 3)")
  pred_o3_m <- eval(parse(text=preds_parse))
  pred_o3_m <- st_set_dimensions(pred_o3_m, 3, names = "time", values = calendar_m$date)
  write_stars_nc(pred_o3_m, as.character(min(calendar_m$date)-1), "ug/m3",
                 paste0(pathout, y, "/o3_", y, "_", str_pad(m, 2, pad="0"), ".nc"))

  # Clean
  rm("calendar_m", "linkdate_m", "ndvi_m",
     "pred_pm25_m", "pred_pm10_m", "pred_no2_m", "pred_o3_m")
}
rm("y", "ntli_y")


#### 2020 ----
print("Predicting 2020 data...")
y <- 2020
ntli_y <- read_rds(paste0(pathin, "/resampled250/", y, "/ntli.rds"))

for(m in 1:12){

  print(paste0("Month: ", m))

  # Prepare calendar and empty objects
  calendar_m <- dplyr::filter(calendar, year==y & month==m) %>%
    dplyr::select(date, yday, julian, holiday, dust)
  pred_pm25_m <- list()
  pred_pm10_m <- list()
  pred_no2_m <- list()
  pred_o3_m <- list()

  # Prepare NDVI and focal, add ntli
  linkdate_m <- floor_date(calendar_m$date[1], "quarter")
  ndvi_m <- c(read_rds(paste0(pathorig, y, "/ndvi.rds")),
              read_rds(paste0(pathorig, y, "/ndvi_focal.rds"))) %>%
    dplyr::filter(date == linkdate_m) %>%
    adrop()
  ndvi_m <- c(ndvi_m, st_warp(ntli_y, ndvi_m))

  # Loop by day
  for(yd in calendar_m$yday){
    print(paste0("Processing day: ", yd))
    yd2 <- yd - min(calendar_m$yday) +1 # counter to store monthly results

    # Residual kriging
    krige_pm10_yd <- krige_res(pm10_resdata[pm10_resdata$date == calendar_m$date[calendar_m$yday == yd],],
                               resgrid, pm10_resmod)
    krige_no2_yd <- krige_res(no2_resdata[no2_resdata$date == calendar_m$date[calendar_m$yday == yd],],
                              resgrid, no2_resmod)
    krige_o3_yd <- krige_res(o3_resdata[o3_resdata$date == calendar_m$date[calendar_m$yday == yd],],
                             resgrid, o3_resmod)

    # Trend
    preds_yd <- c(read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_2mtemp.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_10muwind.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_10mvwind.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_totlprec.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/ERA5_pbh.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/CAMSanaly_no2.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/CAMSanaly_o3.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/CAMSanaly_pm10.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/CAMSanaly_pm25.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/maiac_terra.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/maiac_aqua.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/omi_no2.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/omi_o3.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/tropomi_no2.tif"), yd),
                  read_ydtif(paste0(pathin, "/resampled250/", y, "/tropomi_o3.tif"), yd))
    preds_yd <- adrop(preds_yd)
    names(preds_yd) <- gsub("CAMSanaly", "CAMS", names(preds_yd))

    # Add temporal and spatial variables
    preds_yd$julian <- calendar_m$julian[calendar_m$yday == yd]
    preds_yd$dust <- calendar_m$dust[calendar_m$yday == yd]
    preds_yd$holiday <- calendar_m$holiday[calendar_m$yday == yd]
    preds_yd <- c(preds_yd, st_warp(ndvi_m, preds_yd))
    preds_yd <- c(preds_yd, st_warp(spstack, preds_yd))

    # Execute prediction function: PM2.5
    res_pm25_yd <- pred_exposure(preds_yd, pm25_trendmod)
    pred_pm25_m[[yd2]] <- res_pm25_yd

    # Execute prediction function: PM10
    res_pm10_yd <- pred_exposure(preds_yd, pm10_trendmod, krige_pm10_yd)
    pred_pm10_m[[yd2]] <- res_pm10_yd

    # Execute prediction function: NO2
    res_no2_yd <- pred_exposure(preds_yd, no2_trendmod, krige_no2_yd)
    pred_no2_m[[yd2]] <- res_no2_yd

    # Execute prediction function: O3
    res_o3_yd <- pred_exposure(preds_yd, o3_trendmod, krige_o3_yd)
    pred_o3_m[[yd2]] <- res_o3_yd

    rm("preds_yd", "res_pm25_yd", "res_pm10_yd", "res_no2_yd", "res_o3_yd",
       "krige_pm10_yd", "krige_no2_yd", "krige_o3_yd", "yd2")
  }

  print("Writing monthly results to disk...")

  # Write predictions: PM2.5
  preds_parse <- paste0("c(", paste0("pred_pm25_m[[", 1:length(pred_pm25_m), "]]", collapse = ", "), ", along = 3)")
  pred_pm25_m <- eval(parse(text=preds_parse))
  pred_pm25_m <- st_set_dimensions(pred_pm25_m, 3, names = "time", values = calendar_m$date)
  write_stars_nc(pred_pm25_m, as.character(min(calendar_m$date)-1), "ug/m3",
                 paste0(pathout, y, "/pm25_", y, "_", str_pad(m, 2, pad="0"), ".nc"))

  # Write predictions: PM10
  preds_parse <- paste0("c(", paste0("pred_pm10_m[[", 1:length(pred_pm10_m), "]]", collapse = ", "), ", along = 3)")
  pred_pm10_m <- eval(parse(text=preds_parse))
  pred_pm10_m <- st_set_dimensions(pred_pm10_m, 3, names = "time", values = calendar_m$date)
  write_stars_nc(pred_pm10_m, as.character(min(calendar_m$date)-1), "ug/m3",
                 paste0(pathout, y, "/pm10_", y, "_", str_pad(m, 2, pad="0"), ".nc"))

  # Write predictions: NO2
  preds_parse <- paste0("c(", paste0("pred_no2_m[[", 1:length(pred_no2_m), "]]", collapse = ", "), ", along = 3)")
  pred_no2_m <- eval(parse(text=preds_parse))
  pred_no2_m <- st_set_dimensions(pred_no2_m, 3, names = "time", values = calendar_m$date)
  write_stars_nc(pred_no2_m, as.character(min(calendar_m$date)-1), "ug/m3",
                 paste0(pathout, y, "/no2_", y, "_", str_pad(m, 2, pad="0"), ".nc"))

  # Write predictions: O3
  preds_parse <- paste0("c(", paste0("pred_o3_m[[", 1:length(pred_o3_m), "]]", collapse = ", "), ", along = 3)")
  pred_o3_m <- eval(parse(text=preds_parse))
  pred_o3_m <- st_set_dimensions(pred_o3_m, 3, names = "time", values = calendar_m$date)
  write_stars_nc(pred_o3_m, as.character(min(calendar_m$date)-1), "ug/m3",
                 paste0(pathout, y, "/o3_", y, "_", str_pad(m, 2, pad="0"), ".nc"))

  # Clean
  rm("calendar_m", "linkdate_m", "ndvi_m",
     "pred_pm25_m", "pred_pm10_m", "pred_no2_m", "pred_o3_m")
}
rm("y", "ntli_y")

#### Finish ----
rm(list = ls())