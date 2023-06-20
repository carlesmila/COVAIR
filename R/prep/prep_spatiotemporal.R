#-----------------------------------------------------------------------------#
#                 Pre-processing spatio-temporal data functions               #
#-----------------------------------------------------------------------------#

# Prepare Sentinel 2 NDVI quarterly composites for analysis
prep_ndvi <- function(paths_ndvi, gridr, cat){
  
  # Read inputs
  gridr <- read_rds(gridr)
  cat <- read_rds(cat)
  
  # Read NDVI
  ndvi <- read_stars(paths_ndvi, proxy=FALSE)
  
  # Prepare NDVI data cube
  qdates <-  map_df(paths_ndvi, 
                    function(x) data.frame(year=strsplit(x, "_")[[1]][2],
                                           quarter=strsplit(x, "_")[[1]][3]))%>%
    mutate(quarter = gsub("Q", "", quarter),
           quarter = as.integer(gsub(".tif", "", quarter)))
  ndvi_dates <- as.Date(paste0(qdates$year, "-", (qdates$quarter-1)*3+1, "-01"))
  ndvi <- merge(ndvi) %>%
    setNames("ndvi") %>%
    st_set_dimensions(names = c("x", "y", "date")) %>% 
    st_set_dimensions("date", values = ndvi_dates)
  ndvi <- ndvi[st_buffer(cat, 200)]
  
  # Impute Q1 2018 with Q2 2018, 69 missing pixels in the first image (0.002%)
  miss1 <- as.data.frame(which(is.na(ndvi[,,,1]$ndvi), arr.ind = TRUE)[,1:2])
  miss2 <- as.data.frame(which(is.na(ndvi[,,,2]$ndvi), arr.ind = TRUE)[,1:2])
  missdiff <- setdiff(miss1, miss2)
  missdiff$ndvi <- purrr::map_dbl(1:nrow(missdiff), function(i){
    ndvi[, missdiff$dim1[i], missdiff$dim2[i], 2]$ndvi
  })
  for(i in 1:nrow(missdiff)){
    ndvi[["ndvi"]][missdiff$dim1[i], missdiff$dim2[i], 1] <- missdiff$ndvi[i]
  }
  
  # Resample to reference grid
  ndvi_near <- st_warp(ndvi, gridr)
  ndvi <- st_warp(ndvi, ndvi_near, method="average", use_gdal=TRUE,
                  no_data_value = -9999)
  
  # Clean again for export
  ndvi <- ndvi %>%
    setNames("ndvi") %>%
    st_set_dimensions(names = c("x", "y", "date")) %>% 
    st_set_dimensions("date", values = ndvi_dates)
  ndvi <- ndvi[st_buffer(cat, 100)]
  
  # Store and return paths
  ndvi18 <- dplyr::filter(ndvi, year(date)==2018)
  ndvi19 <- dplyr::filter(ndvi, year(date)==2019)
  ndvi20 <- dplyr::filter(ndvi, year(date)==2020)
  write_rds(ndvi18, "database/original/2018/ndvi.rds")
  write_rds(ndvi19, "database/original/2019/ndvi.rds")
  write_rds(ndvi20, "database/original/2020/ndvi.rds")
  list("ndvi18"="database/original/2018/ndvi.rds",
       "ndvi19"="database/original/2019/ndvi.rds",
       "ndvi20"="database/original/2020/ndvi.rds")
}

# Prepare annual Night-Time Lights products from VIIRS for analysis
prep_ntli <- function(paths_ntli, cat){
  
  # Read inputs
  cat <- read_rds(cat)
  
  # Read NTLI and tidy time dimension
  dates_ntli <- map_chr(paths_ntli, 
                        function(x) gsub(".tif", "" , strsplit(x, "_")[[1]][2]))
  dates_ntli <- paste0(dates_ntli, "-01-01")
  ntli <- read_stars(paths_ntli) %>%
    merge() %>%
    st_set_dimensions(3,  
                      values = as.Date(dates_ntli)) %>%
    st_set_dimensions(names = c("x", "y", "date"))
  
  # Project to study area CRS  ~500m
  raster_model <- st_warp(ntli, crs = st_crs(cat))
  ntli <- st_warp(ntli, raster_model, method="bilinear", use_gdal = T,
                  no_data_value = -9999)
  names(ntli) <- "ntli"
  ntli <- st_set_dimensions(ntli, 3, names="date", 
                            values=as.Date(dates_ntli))
  ntli <- st_crop(ntli, st_buffer(cat, 750))
  
  # Store and return paths
  ntli18 <- dplyr::filter(ntli, year(date)==2018)
  ntli19 <- dplyr::filter(ntli, year(date)==2019)
  ntli20 <- dplyr::filter(ntli, year(date)==2020)
  write_rds(ntli18, "database/original/2018/ntli.rds")
  write_rds(ntli19, "database/original/2019/ntli.rds")
  write_rds(ntli20, "database/original/2020/ntli.rds")
  list("ntli18"="database/original/2018/ntli.rds",
       "ntli19"="database/original/2019/ntli.rds",
       "ntli20"="database/original/2020/ntli.rds") 
}

# Prepare ERA5 reanalysis products for analysis
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land
prep_era5rean <- function(paths_era5rean, cat){
  
  # Read inputs
  cat <- read_rds(cat)
  
  # Read ERA5 Reanalysis data: mean sea level pressure + PBH
  # Parse object dates and names
  datetime_era5 <- seq.POSIXt(ISOdatetime(2018, 1, 1, 0, 0, 0),
                              ISOdatetime(2020, 12, 31, 23, 0, 0), by="1 hour")
  era5rean <- read_stars(paths_era5rean)  %>%
    st_set_dimensions(3,  values = datetime_era5, name="datetime") 
  names(era5rean) <- gsub(".grib", "", names(era5rean))
  date_era5 <- seq.Date(as.Date("2018-01-01"), as.Date("2020-12-31"), "1 day")
  
  # Sat overpass times: 10h30 (10h, Terra), 13:30 (13h, Aqua, Aura, S5P)
  era5rean_10 <- dplyr::filter(era5rean, hour(datetime) == 10) %>%
    st_set_dimensions("datetime", names="date", values = date_era5)
  era5rean_13 <- dplyr::filter(era5rean, hour(datetime) == 13) %>%
    st_set_dimensions("datetime", names="date", values = date_era5) 
  
  # Aggregate daily values using mean
  era5rean <- aggregate(era5rean, "1 day", mean)
  era5rean <-  st_set_dimensions(era5rean, 1, names="date", values = date_era5)
  
  # warp, fix names, merge, crop to cat
  era5rean_model <- st_warp(era5rean, crs = st_crs(cat))
  era5rean_names <- names(era5rean)
  parsing_txt <- paste0("c(", paste0("st_warp(era5rean[", 1:length(era5rean),
  "], era5rean_model, use_gdal = T, method = 'bilinear', no_data_value = -9999)",
  collapse = ","), ")")
  era5rean <- eval(parse(text=parsing_txt))
  names(era5rean) <- era5rean_names
  parsing_txt <- paste0("c(", paste0("st_warp(era5rean_10[", 1:length(era5rean_10),
  "], era5rean_model, use_gdal = T, method = 'bilinear', no_data_value = -9999)",
  collapse = ","), ")")
  era5rean_10 <- eval(parse(text=parsing_txt))
  names(era5rean_10) <- paste0(era5rean_names, "_10")
  parsing_txt <- paste0("c(", paste0("st_warp(era5rean_13[", 1:length(era5rean_13),
  "], era5rean_model, use_gdal = T, method = 'bilinear', no_data_value = -9999)", collapse = ","), ")")
  era5rean_13 <- eval(parse(text=parsing_txt))
  names(era5rean_13) <- paste0(era5rean_names, "_13")
  era5rean <- c(era5rean, era5rean_10, era5rean_13)%>%
    st_set_dimensions("band", names="date", values = date_era5) 
  era5rean <- st_crop(era5rean, st_buffer(cat, 40000))
  
  # Store and return paths
  era5rean18 <- dplyr::filter(era5rean, year(date)==2018)
  era5rean19 <- dplyr::filter(era5rean, year(date)==2019)
  era5rean20 <- dplyr::filter(era5rean, year(date)==2020)
  write_rds(era5rean18, "database/original/2018/era5rean.rds")
  write_rds(era5rean19, "database/original/2019/era5rean.rds")
  write_rds(era5rean20, "database/original/2020/era5rean.rds")
  list("era5rean18"="database/original/2018/era5rean.rds",
       "era5rean19"="database/original/2019/era5rean.rds",
       "era5rean20"="database/original/2020/era5rean.rds") 
}

# Prepare ERA5 land reanalysis products for analysis
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land
prep_era5land <- function(paths_era5land, cat){
  
  # Read inputs
  cat <- read_rds(cat)
  
  # Read ERA5 land data: temperature, wind components, total precipitation
  # Parse object dates and names
  datetime_era5 <- seq.POSIXt(ISOdatetime(2018, 1, 1, 0, 0, 0),
                              ISOdatetime(2020, 12, 31, 23, 0, 0), by="1 hour")
  era5land <- read_stars(paths_era5land)  %>%
    st_set_dimensions(3,  values = datetime_era5) %>%
    st_set_dimensions(names = c("x", "y", "datetime"))
  names(era5land) <- gsub(".grib", "", names(era5land))
  date_era5 <- seq.Date(as.Date("2018-01-01"), as.Date("2020-12-31"), "1 day")
  
  # Convert degrees in Kelvin to celsius by subtracting 273.15
  era5land <- mutate(era5land, ERA5Land_2mtemp = ERA5Land_2mtemp - 273.15)
  
  # Sat overpass times: 10h30 (10h, Terra), 13:30 (13h, Aqua, Aura, S5P),
  # Sat overpass times: 22h30 (22h, Terra), 01:30 (01h, Aqua)
  era5land_10 <- dplyr::filter(era5land, hour(datetime) == 10) %>%
    st_set_dimensions("datetime", names="date", values = date_era5)
  names(era5land_10) <- paste0(names(era5land_10), "_10")
  era5land_13 <- dplyr::filter(era5land, hour(datetime) == 13) %>%
    st_set_dimensions("datetime", names="date", values = date_era5) 
  names(era5land_13) <- paste0(names(era5land_13), "_13")
  era5land_22 <- dplyr::filter(era5land, hour(datetime) == 22) %>%
    st_set_dimensions("datetime", names="date", values = date_era5)
  names(era5land_22) <- paste0(names(era5land_22), "_22")
  era5land_01 <- dplyr::filter(era5land, hour(datetime) == 01) %>%
    st_set_dimensions("datetime", names="date", values = date_era5)
  names(era5land_01) <- paste0(names(era5land_01), "_01")
  
  # Aggregate using mean for temperature and wind components
  era5land_mean <- dplyr::select(era5land, ERA5Land_2mtemp,
                                 ERA5Land_10muwind, ERA5Land_10mvwind) %>%
    aggregate("1 day", mean) %>%
    st_set_dimensions("time", names="date", values = date_era5)
  
  # Aggregate using sum for daily total precipitation
  era5land_sum <- dplyr::select(era5land, ERA5Land_totlprec) %>%
    aggregate("1 day", sum) %>%
    st_set_dimensions("time", names="date", values = date_era5)
  
  # Aggregate using min/max for daily temperature
  era5land_min <- dplyr::select(era5land, ERA5Land_2mtemp) %>%
    aggregate("1 day", min)%>%
    st_set_dimensions("time", names="date", values = date_era5)
  names(era5land_min) <- "ERA5Land_2mtemp_min"
  era5land_max <- dplyr::select(era5land, ERA5Land_2mtemp) %>%
    aggregate("1 day", max)%>%
    st_set_dimensions("time", names="date", values = date_era5)
  names(era5land_max) <- "ERA5Land_2mtemp_max"
  
  # Merge
  era5land <- c(era5land_mean, era5land_sum, era5land_min, era5land_max)
  era5land_hourly <- c(era5land_10, era5land_13, era5land_22, era5land_01)
  
  # Warp - daily data
  era5land_names <- names(era5land)
  era5land_model <- st_warp(era5land, crs=st_crs(cat))[1]
  parsing_txt <- paste0("c(", paste0("st_warp(era5land[", 1:length(era5land),
  "], era5land_model, use_gdal = T, method = 'bilinear',
                 no_data_value = -9999)", collapse = ","), ")")
  era5land <- eval(parse(text=parsing_txt))%>%
    st_set_dimensions("band", names="date", values = date_era5) 
  names(era5land) <-era5land_names
  
  # Warp - hourly data
  era5land_names <- names(era5land_hourly)
  era5land_model <- st_warp(era5land_hourly, crs=st_crs(cat))[1]
  parsing_txt <- paste0("c(", paste0("st_warp(era5land_hourly[", 1:length(era5land_hourly),
  "], era5land_model, use_gdal = T, method = 'bilinear',
                 no_data_value = -9999)", collapse = ","), ")")
  era5land_hourly <- eval(parse(text=parsing_txt))%>%
    st_set_dimensions("band", names="date", values = date_era5) 
  names(era5land_hourly) <- era5land_names
  
  # Fill missing coastal pixels of ERA5land via 5x5 focal mean.
  # Some of them leave out coastal areas for which we want to predict.
  era5land <- c(era5land, era5land_hourly)
  for(attr_indx in names(era5land)){
    print(paste0("Filling coastal pixels. Attribute: ", attr_indx))
    
    # Loop in time
    for(time_indx in 1:1096){
      
      # 1st fill
      targetr <- adrop(era5land[attr_indx,,,time_indx])
      focalr <- focal2(targetr, matrix(1, 3, 3), "mean", na.rm = T)
      fixed_grid <- ifelse(is.na(targetr[[1]]), focalr[[1]], targetr[[1]])
      targetr[[attr_indx]] <- fixed_grid
      # 2nd fill
      focalr <- focal2(targetr, matrix(1, 3, 3), "mean", na.rm = T)
      fixed_grid <- ifelse(is.na(targetr[[1]]), focalr[[1]], targetr[[1]])
      targetr[[attr_indx]] <- fixed_grid
      names(targetr) <- as.character(time_indx)
      
      if(!exists("era5land2")){
        era5land2 <- targetr
      }else{
        era5land2 <- c(era5land2, targetr)
      }
    }
    # Merge object in third dimension
    era5land2 <- merge(era5land2) %>%
      st_set_dimensions(names = c("x", "y", "date")) %>%
      st_set_dimensions(3, values = date_era5)
    names(era5land2) <- attr_indx
    if(!exists("era5land3")){
      era5land3 <- era5land2
    }else{
      era5land3 <- c(era5land3, era5land2)
    }
    rm("era5land2")
  }
  
  # Store and return paths
  era5land18 <- dplyr::filter(era5land3, year(date)==2018)
  era5land19 <- dplyr::filter(era5land3, year(date)==2019)
  era5land20 <- dplyr::filter(era5land3, year(date)==2020)
  write_rds(era5land18, "database/original/2018/era5land.rds")
  write_rds(era5land19, "database/original/2019/era5land.rds")
  write_rds(era5land20, "database/original/2020/era5land.rds")
  list("era5land18"="database/original/2018/era5land.rds",
       "era5land19"="database/original/2019/era5land.rds",
       "era5land20"="database/original/2020/era5land.rds") 
}

# Prepare CAMS global reanalysis products for analysis
# https://ads.atmosphere.copernicus.eu/cdsapp#!/dataset/cams-europe-air-quality-reanalyses?tab=overview
prep_camsglobal <- function(paths_camsglobal, cat){
  
  # Read inputs
  cat <- read_rds(cat)
  
  # Parse object dates and names
  date_camsglobal <- seq.Date(as.Date("2018-01-01"), as.Date("2020-12-31"), "1 day")
  camsglobal <- read_stars(paths_camsglobal) %>%
    st_set_dimensions(3,  values = date_camsglobal, name="date")
  names(camsglobal) <- gsub(".grib", "", names(camsglobal))
  
  # warp, fix names, merge, crop to cat
  camsglobal_model <- st_warp(camsglobal, crs = st_crs(cat))
  camsglobal_names <- names(camsglobal)
  parsing_txt <- paste0("c(", paste0("st_warp(camsglobal[", 1:length(camsglobal),
                                     "], camsglobal_model, use_gdal = T, method = 'bilinear', no_data_value = -9999)",
                                     collapse = ","), ")")
  camsglobal <- eval(parse(text=parsing_txt))
  names(camsglobal) <- camsglobal_names
  camsglobal <- st_set_dimensions(camsglobal, "band", names="date", values = date_camsglobal) 
  camsglobal <- st_crop(camsglobal, st_buffer(cat, 100000))
  
  # Store and return paths
  camsglobal18 <- dplyr::filter(camsglobal, year(date)==2018)
  camsglobal19 <- dplyr::filter(camsglobal, year(date)==2019)
  camsglobal20 <- dplyr::filter(camsglobal, year(date)==2020)
  write_rds(camsglobal18, "database/original/2018/camsglobal.rds")
  write_rds(camsglobal19, "database/original/2019/camsglobal.rds")
  write_rds(camsglobal20, "database/original/2020/camsglobal.rds")
  list("camsglobal18"="database/original/2018/camsglobal.rds",
       "camsglobal19"="database/original/2019/camsglobal.rds",
       "camsglobal20"="database/original/2020/camsglobal.rds") 
}

# Prepare CAMS European reanalysis products for analysis
prep_camsrean <- function(paths_camsrean, cat){
  
  # Read inputs
  cat <- read_rds(cat)
  
  # Read by pollutant, merge, clean, daily average, warp: no2
  no2 <- paths_camsrean[grepl("no2", paths_camsrean)]
  no2 <- paste("c(", paste(paste0("read_rds('", no2, "')"), collapse = ", "), ")")
  no2 <- eval(parse(text=no2))
  st_crs(no2) <- 4326
  no2 <- st_set_dimensions(no2, "time", values = st_get_dimension_values(no2, "time") +1)
  dates <- unique(as.Date(st_get_dimension_values(no2, "time")))
  no2 <- aggregate(no2, "1 day", mean)
  no2 <- st_set_dimensions(no2, "time", names = "date", values = dates)
  warpmodel <- st_warp(no2, crs = st_crs(cat))
  no2 <- st_warp(no2, warpmodel, method = "bilinear", use_gdal = T, no_data_value = -9999) %>%
    st_set_dimensions("band", names = "date", values = dates)
  names(no2) <- "CAMSrean_no2"
  
  # Read by pollutant, merge, clean, daily average: pm10
  pm10 <- paths_camsrean[grepl("pm10", paths_camsrean)]
  pm10 <- paste("c(", paste(paste0("read_rds('", pm10, "')"), collapse = ", "), ")")
  pm10 <- eval(parse(text=pm10))
  st_crs(pm10) <- 4326
  pm10 <- st_set_dimensions(pm10, "time", values = st_get_dimension_values(pm10, "time") +1)
  dates <- unique(as.Date(st_get_dimension_values(pm10, "time")))
  pm10 <- aggregate(pm10, "1 day", mean)
  pm10 <- st_set_dimensions(pm10, "time", names = "date", values = dates)
  pm10 <- st_warp(pm10, warpmodel, method = "bilinear", use_gdal = T, no_data_value = -9999) %>%
    st_set_dimensions("band", names = "date", values = dates)
  names(pm10) <- "CAMSrean_pm10"
  
  # Read by pollutant, merge, clean, daily average: pm25
  pm25 <- paths_camsrean[grepl("pm2p5", paths_camsrean)]
  pm25 <- paste("c(", paste(paste0("read_rds('", pm25, "')"), collapse = ", "), ")")
  pm25 <- eval(parse(text=pm25))
  st_crs(pm25) <- 4326
  pm25 <- st_set_dimensions(pm25, "time", values = st_get_dimension_values(pm25, "time") +1)
  dates <- unique(as.Date(st_get_dimension_values(pm25, "time")))
  pm25 <- aggregate(pm25, "1 day", mean)
  pm25 <- st_set_dimensions(pm25, "time", names = "date", values = dates)
  pm25 <- st_warp(pm25, warpmodel, method = "bilinear", use_gdal = T, no_data_value = -9999) %>%
    st_set_dimensions("band", names = "date", values = dates)
  names(pm25) <- "CAMSrean_pm25"
  
  # Read by pollutant, merge, clean, daily average: o3
  o3 <- paths_camsrean[grepl("o3", paths_camsrean)]
  o3 <- paste("c(", paste(paste0("read_rds('", o3, "')"), collapse = ", "), ")")
  o3 <- eval(parse(text=o3))
  st_crs(o3) <- 4326
  o3 <- st_set_dimensions(o3, "time", values = st_get_dimension_values(o3, "time") +1)
  dates <- unique(as.Date(st_get_dimension_values(o3, "time")))
  o3 <- aggregate(o3, "1 day", max)
  o3 <- st_set_dimensions(o3, "time", names = "date", values = dates)
  o3_months <- aggregate(o3, "months", mean)
  o3 <- st_warp(o3, warpmodel, method = "bilinear", use_gdal = T, no_data_value = -9999) %>%
    st_set_dimensions("band", names = "date", values = dates)
  names(o3) <- "CAMSrean_o3"
  
  # Merge pollutants, warp, and crop to cat
  camsrean <- c(pm25, pm10, no2, o3)
  camsrean <- st_crop(camsrean, st_buffer(cat, 10000))
    
  # Store and return paths
  write_rds(camsrean, "database/original/2018/camsrean.rds")
  list("camsrean18"="database/original/2018/camsrean.rds") 
}

# Prepare CAMS European analysis products for analysis
prep_camsanaly <- function(paths_camsanaly, cat){
  
  # Read inputs
  cat <- read_rds(cat)
  
  # Fix dates sequentially, merge
  for(y0 in c(2019,2020)){
    for(m0 in 1:12){
      
      # Sequence of times
      y1 <- ifelse(m0!=12, y0, y0+1)
      m1 <- ifelse(m0!=12, m0+1, 1)
      m0 <- str_pad(m0, 2, "left", "0")
      y0 <- str_pad(y0, 2, "left", "0")
      m1 <- str_pad(m1, 2, "left", "0")
      y1 <- str_pad(y1, 2, "left", "0")
      t0 <- as.POSIXct(paste0(y0,"-",m0,"-01", "00:00:00"), tz = "UTC")
      t1 <- as.POSIXct(paste0(y1,"-",m1,"-01", "00:00:00"), tz = "UTC") -1
      times <- seq.POSIXt(t0, t1, "1 hour")

      # Define path
      path <- paths_camsanaly[grepl(paste0("_", y0, "_"), paths_camsanaly) & 
                                grepl(paste0("_", m0, "."), paths_camsanaly)]
      
      # Read data and correct times
      camsit <- suppressWarnings(adrop(read_stars(path, quiet = T))) %>%
        st_set_dimensions("time", values = times)
      if(!exists("camsanaly")){
        camsanaly <- camsit
      }else{
        camsanaly <- c(camsanaly, camsit, along = 3)
      }
      y0 <- as.integer(y0)
    }
  }
  
  # Clean, daily average, warp, crop to cat
  st_crs(camsanaly) <- 4326
  dates <- unique(as.Date(st_get_dimension_values(camsanaly, "time")))
  camsanaly <- c(aggregate(dplyr::select(camsanaly, -o3_conc), "1 day", mean),
                 aggregate(dplyr::select(camsanaly, o3_conc), "1 day", max))
  camsnames <- paste0("CAMSanaly_", names(camsanaly))
  camsnames <- gsub("pm2p5", "pm25", gsub("_conc", "", camsnames))
  names(camsanaly) <- camsnames
  warpmodel <- st_warp(camsanaly[1], crs = st_crs(cat))
  parsing_txt <- paste0("c(", paste0("st_warp(camsanaly[", 1:length(camsanaly),
                                     "], warpmodel, use_gdal = T, method = 'bilinear', no_data_value = -9999)",
                                     collapse = ","), ")")
  camsanaly <- eval(parse(text=parsing_txt))
  names(camsanaly) <- camsnames
  camsanaly <- st_set_dimensions(camsanaly, "band", names = "date", values = dates)
  camsanaly <- st_crop(camsanaly, st_buffer(cat, 10000))
  
  # Store and return paths
  camsanaly19 <- dplyr::filter(camsanaly, year(date)==2019)
  camsanaly20 <- dplyr::filter(camsanaly, year(date)==2020)
  write_rds(camsanaly19, "database/original/2019/camsanaly.rds")
  write_rds(camsanaly20, "database/original/2020/camsanaly.rds")
  list("camsanaly19"="database/original/2019/camsanaly.rds",
       "camsanaly20"="database/original/2020/camsanaly.rds") 
}