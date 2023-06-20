#-----------------------------------------------------------------------------#
#                      Pre-process TROPOMI NO2 and O3 data                    #
#-----------------------------------------------------------------------------#

# Preprocess harp-converted TROPOMI tropospheric NO2 files
clean_tropomi_no2 <- function(path_tropomi_no2){
  
  # Read, define grid, set CRS, crop to study area
  tropomi_no2 <- suppressMessages(suppressWarnings(
    read_ncdf(path_tropomi_no2, make_time=F))) %>%
    adrop()
  tropomi_no2 <- tropomi_no2 %>%
    st_set_dimensions("longitude", delta=0.025, offset=0) %>%
    st_set_dimensions("latitude", delta=0.025, offset=40)
  tropomi_no2 <- st_set_crs(tropomi_no2, 4326)
  names(tropomi_no2) <- "tropo_no2"
  
  tropomi_no2
}

# Prepare TROPOMI tropospheric NO2 data for analysis
prep_tropomi_no2 <- function(paths_tropomi_no2, cat){
  
  # Read inputs
  cat <- read_rds(cat)
  
  # Compute dates
  dates <- paste0(substr(paths_tropomi_no2, 43, 46), "-", 
                  substr(paths_tropomi_no2, 47, 48), "-", 
                  substr(paths_tropomi_no2, 49, 50))
  dates <- as.Date(dates)
  
  # Clean each TROPOMI NO2 file
  options(future.rng.onMisuse="ignore")
  plan(multisession, workers=10)
  tropomi_no2 <- future_map(paths_tropomi_no2, clean_tropomi_no2, .progress=T)
  plan(sequential)
  
  # Merge in a single stars objects
  parsing_txt <- paste0("c(", 
                        paste0("tropomi_no2[[", 1:length(tropomi_no2),"]]", 
                               collapse = ","), ", along=3)")
  tropomi_no2 <- eval(parse(text=parsing_txt))
  tropomi_no2 <- st_set_dimensions(tropomi_no2, 3, values=dates, names="date")
  
  # Aggregate by date
  tropomi_no2 <- aggregate(tropomi_no2, by="1 day", mean, na.rm=T)
  dates_aggr <- st_get_dimension_values(tropomi_no2, "time")
  
  # Warp object, crop
  raster_model <- st_warp(tropomi_no2, crs=st_crs(cat))
  tropomi_no2 <- st_warp(tropomi_no2, raster_model, method="bilinear",
                         use_gdal=TRUE, no_data_value = -9999)
  tropomi_no2 <- st_set_dimensions(tropomi_no2, 3, names="date", values=dates_aggr)
  names(tropomi_no2) <- "tropo_no2"
  tropomi_no2 <- st_crop(tropomi_no2, st_buffer(cat, 2500))
  
  # unit conversion from mol/m^2 (TROPOMI), to mol/cm^2
  tropomi_no2 <- tropomi_no2*10000
  
  # Store and return paths
  tropomi_no2_18 <- dplyr::filter(tropomi_no2, year(date)==2018)
  tropomi_no2_19 <- dplyr::filter(tropomi_no2, year(date)==2019)
  tropomi_no2_20 <- dplyr::filter(tropomi_no2, year(date)==2020)
  write_rds(tropomi_no2_18, "database/original/2018/tropomi_no2.rds")
  write_rds(tropomi_no2_19, "database/original/2019/tropomi_no2.rds")
  write_rds(tropomi_no2_20, "database/original/2020/tropomi_no2.rds")
  list("tropomi_no2_18"="database/original/2018/tropomi_no2.rds",
       "tropomi_no2_19"="database/original/2019/tropomi_no2.rds",
       "tropomi_no2_20"="database/original/2020/tropomi_no2.rds") 
}


# Preprocess harp-converted TROPOMI O3 files
clean_tropomi_o3 <- function(path_tropomi_o3){
  
  # Read, define grid, set CRS, crop to study area
  tropomi_o3 <- suppressMessages(suppressWarnings(
    read_ncdf(path_tropomi_o3, make_time=F))) %>%
    adrop()
  tropomi_o3 <- tropomi_o3 %>%
    st_set_dimensions("longitude", delta=0.025, offset=0) %>%
    st_set_dimensions("latitude", delta=0.025, offset=40)
  tropomi_o3 <- st_set_crs(tropomi_o3, 4326)
  names(tropomi_o3) <- "total_o3"
  
  tropomi_o3
}

# Prepare TROPOMI O3 data for analysis
prep_tropomi_o3 <- function(paths_tropomi_o3, cat){
  
  # Read inputs
  cat <- read_rds(cat)
  
  # Compute dates
  dates <- paste0(substr(paths_tropomi_o3, 42, 45), "-", 
                  substr(paths_tropomi_o3, 46, 47), "-", 
                  substr(paths_tropomi_o3, 48, 49))
  dates <- as.Date(dates)
  
  # Clean each TROPOMI O3 file
  options(future.rng.onMisuse="ignore")
  plan(multisession, workers=10)
  tropomi_o3 <- future_map(paths_tropomi_o3, clean_tropomi_o3, .progress=T)
  plan(sequential)
  
  # Merge in a single stars objects
  parsing_txt <- paste0("c(", 
                        paste0("tropomi_o3[[", 1:length(tropomi_o3),"]]", 
                               collapse = ","), ", along=3)")
  tropomi_o3 <- eval(parse(text=parsing_txt))
  tropomi_o3 <- st_set_dimensions(tropomi_o3, 3, values=dates, names="date")
  
  # Aggregate by date
  tropomi_o3 <- aggregate(tropomi_o3, by="1 day", mean, na.rm=T)
  dates_aggr <- st_get_dimension_values(tropomi_o3, "time")
  
  # Warp object, crop
  raster_model <- st_warp(tropomi_o3, crs=st_crs(cat))
  tropomi_o3 <- st_warp(tropomi_o3, raster_model, method="bilinear",
                         use_gdal=TRUE, no_data_value = -9999)
  tropomi_o3 <- st_set_dimensions(tropomi_o3, 3, names="date", values=dates_aggr)
  names(tropomi_o3) <- "tropo_o3"
  tropomi_o3 <- st_crop(tropomi_o3, st_buffer(cat, 2500))
  
  # unit conversion from mol/m^2 (TROPOMI) to Dobson Units (OMI)
  tropomi_o3 <- tropomi_o3*2241.15 
  
  # Store and return paths
  tropomi_o3_18 <- dplyr::filter(tropomi_o3, year(date)==2018)
  tropomi_o3_19 <- dplyr::filter(tropomi_o3, year(date)==2019)
  tropomi_o3_20 <- dplyr::filter(tropomi_o3, year(date)==2020)
  write_rds(tropomi_o3_18, "database/original/2018/tropomi_o3.rds")
  write_rds(tropomi_o3_19, "database/original/2019/tropomi_o3.rds")
  write_rds(tropomi_o3_20, "database/original/2020/tropomi_o3.rds")
  list("tropomi_o3_18"="database/original/2018/tropomi_o3.rds",
       "tropomi_o3_19"="database/original/2019/tropomi_o3.rds",
       "tropomi_o3_20"="database/original/2020/tropomi_o3.rds") 
}
