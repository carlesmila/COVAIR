#-----------------------------------------------------------------------------#
#                        Pre-processing OMI data functions                    #
#-----------------------------------------------------------------------------#

# OMNO2d product 0.25º x 0.25º
# https://disc.gsfc.nasa.gov/datasets/OMNO2d_003/summary?keywords=OMNO2d
# Preprocess OMNO2d raw files
clean_omi_no2 <- function(path_omi_no2){
  
  # Read and set CRS
  omi_no2 <- suppressWarnings(read_stars(path_omi_no2))
  omi_no2 <- st_set_crs(omi_no2, 4326)
  names(omi_no2) <- "omi_no2"

  omi_no2
}

# Prepare tropospheric NO2 column cloud screened for analysis
prep_omi_no2 <- function(paths_omi_no2, cat){
  
  # Read inputs
  cat <- read_rds(cat)
  
  # Compute dates
  dates <- paste0(substr(paths_omi_no2, 41, 44), "-", 
                  substr(paths_omi_no2, 46, 47), "-", 
                  substr(paths_omi_no2, 48, 49))
  dates <- as.Date(dates)
  
  # Clean each OMNO2d file
  options(future.rng.onMisuse="ignore")
  plan(multisession, workers=10)
  OMNO2d <- future_map(paths_omi_no2, clean_omi_no2, .progress=T)
  plan(sequential)
  
  # Merge in a single stars objects
  parsing_txt <- paste0("c(", 
                        paste0("OMNO2d[[", 1:length(OMNO2d),"]]", 
                               collapse = ","), ", along=3)")
  OMNO2d <- eval(parse(text=parsing_txt))
  
  # Project to UTM31N, crop to study area
  raster_model <- st_warp(OMNO2d, crs=st_crs(cat))
  OMNO2d <- st_warp(OMNO2d, raster_model, method="bilinear", use_gdal=TRUE,
                    no_data_value = -9999)
  names(OMNO2d) <- "omi_no2"
  OMNO2d <- st_set_dimensions(OMNO2d, 3, values=dates, names="date")
  OMNO2d <- st_crop(OMNO2d, st_buffer(cat, 25000))
  
  # unit conversion from molecules/cm^2 (OMI) to mol/m^2 (TROPOMI), to mol/cm^2
  OMNO2d <- OMNO2d/6.02214e+19*10000
  
  # Store and return paths
  omi_no2_18 <- dplyr::filter(OMNO2d, year(date)==2018)
  omi_no2_19 <- dplyr::filter(OMNO2d, year(date)==2019)
  omi_no2_20 <- dplyr::filter(OMNO2d, year(date)==2020)
  write_rds(omi_no2_18, "database/original/2018/omi_no2.rds")
  write_rds(omi_no2_19, "database/original/2019/omi_no2.rds")
  write_rds(omi_no2_20, "database/original/2020/omi_no2.rds")
  list("omi_no2_18"="database/original/2018/omi_no2.rds",
       "omi_no2_19"="database/original/2019/omi_no2.rds",
       "omi_no2_20"="database/original/2020/omi_no2.rds") 
}



# https://disc.gsfc.nasa.gov/datasets/OMTO3e_003/summary?keywords=OMI%20O3%20L3
# Preprocess OMTO3e raw files
clean_omi_o3 <- function(path_omi_o3){
  
  # Read, set CRS, crop to study area
  omi_o3 <- suppressWarnings(read_stars(path_omi_o3))
  omi_o3 <- st_set_crs(omi_o3, 4326)
  names(omi_o3) <- "omi_o3"

  omi_o3
}

# Prepare OMI O3 columns for analysis
prep_omi_o3 <- function(paths_omi_o3, cat, omi_no2){
  
  # Read inputs
  cat <- read_rds(cat)
  omi_no2 <- c(read_rds(omi_no2$omi_no2_18), read_rds(omi_no2$omi_no2_19), 
               read_rds(omi_no2$omi_no2_20), along=3)
  
  # Compute dates
  dates <- paste0(substr(paths_omi_o3, 37, 40), "-", 
                  substr(paths_omi_o3, 42, 43), "-", 
                  substr(paths_omi_o3, 44, 45))
  dates <- as.Date(dates)
  
  # Clean each files
  options(future.rng.onMisuse="ignore")
  plan(multisession, workers=10)
  OMTO3e <- future_map(paths_omi_o3, clean_omi_o3, .progress=T)
  plan(sequential)
  
  # Merge in a single stars objects
  parsing_txt <- paste0("c(", 
                        paste0("OMTO3e[[", 1:length(OMTO3e),"]]", 
                               collapse = ","), ", along=3)")
  OMTO3e <- eval(parse(text=parsing_txt))
  
  # Project to UTM31N, OMI no2 grid
  OMTO3e <- st_warp(OMTO3e, omi_no2, method="bilinear", use_gdal = TRUE, 
                    no_data_value = -9999)
  names(OMTO3e) <- "omi_o3"
  OMTO3e <- st_set_dimensions(OMTO3e, 3, values=dates, names="date")
  OMTO3e <- st_crop(OMTO3e, st_buffer(cat, 25000))
  
  # Store and return paths
  omi_o3_18 <- dplyr::filter(OMTO3e, year(date)==2018)
  omi_o3_19 <- dplyr::filter(OMTO3e, year(date)==2019)
  omi_o3_20 <- dplyr::filter(OMTO3e, year(date)==2020)
  write_rds(omi_o3_18, "database/original/2018/omi_o3.rds")
  write_rds(omi_o3_19, "database/original/2019/omi_o3.rds")
  write_rds(omi_o3_20, "database/original/2020/omi_o3.rds")
  list("omi_o3_18"="database/original/2018/omi_o3.rds",
       "omi_o3_19"="database/original/2019/omi_o3.rds",
       "omi_o3_20"="database/original/2020/omi_o3.rds") 
}