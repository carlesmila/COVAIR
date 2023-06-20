#-----------------------------------------------------------------------------#
#                     Pre-processing AOD MAIAC data functions                 #
#-----------------------------------------------------------------------------#

# Preprocess raw MAIAC files
preprocess_maiac <- function(hdf_file, aoi_extent){
  
  # Read file
  hdf_data <- get_subdatasets(hdf_file)
  
  # Get overpass information: datetime and satellite
  overpass_info <- suppressWarnings(GDALinfo(hdf_file, returnScaleOffset=FALSE))
  (overpass_info <- attr(overpass_info, "mdata")[59])
  overpass_info <- gsub("Orbit_time_stamp=", "  ", overpass_info)
  overpass_info <- strsplit(overpass_info, "  ")[[1]]
  overpass_info <- setdiff(overpass_info, "")
  overpass_info <- data.frame(oinf = overpass_info) %>%
    mutate(satellite = ifelse(grepl("T", oinf), "Terra", "Aqua"),
           yearorigin = as.Date(paste0(substr(oinf,1,4),"-01-01")),
           dcount = as.integer(substr(oinf,5,7)),
           date = as.character(yearorigin + (dcount-1)),
           time = paste0(substr(oinf,8,9),":", substr(oinf,10,11),":00"),
           datetime = as.POSIXct(paste0(date, " ", time))) %>%
    dplyr::select(satellite, datetime)
  
  # Read QA layers, parse QA flags
  auxfile <- paste0(overpass_info$date[1], ".tif")
  gdal_translate(hdf_data[6], dst_dataset = auxfile)
  aod_qa <- read_stars(auxfile) %>%
    st_set_dimensions(3, values = overpass_info$datetime, name="datetime") 
  names(aod_qa) <- "aod_qa"
  aod_qa <- st_crop(aod_qa, st_bbox(st_transform(aoi_extent, crs=st_crs(aod_qa))))
  aod_qa <- aod_qa %>%
    mutate(aod_qa = str_pad(R.utils::intToBin(aod_qa), 16, pad="0"),
           # Best quality (0000) & single adjacent cloud (0011)
           aod_qa = ifelse(substr(aod_qa, 5, 8)%in%c("0000", "0011"), T, F))  
  unlink(auxfile)
  unlink(paste0(auxfile, ".aux.xml"))
  
  # Read AOD 55
  gdal_translate(hdf_data[2], dst_dataset = auxfile)
  aod_55 <- read_stars(auxfile) 
  aod_55 <- st_crop(aod_55, st_bbox(st_transform(aoi_extent, crs=st_crs(aod_55)))) %>%
    st_set_dimensions(3, values = overpass_info$datetime, name="datetime")
  names(aod_55) <- "AOD55"
  aod_55 <- c(aod_55, aod_qa)
  aod_55 <- mutate(aod_55, AOD55 = ifelse(aod_qa, AOD55, NA)) %>%
    dplyr::select(AOD55) %>%
    st_set_dimensions(3, values = overpass_info$satellite, name="satellite") 
  unlink(auxfile)
  unlink(paste0(auxfile, ".aux.xml"))
  
  # Check if missing aqua or terra: fill with NAs
  if(!"Aqua" %in% st_get_dimension_values(aod_55, 3)){
    aqua <- aod_55[,,,1] %>%
      st_set_dimensions(3, values = "Aqua", name="satellite") 
    aqua$AOD55 <- as.numeric(NA)
    aod_55 <- c(aod_55, aqua, along=3)
    rm("aqua")
  }
  if(!"Terra" %in% st_get_dimension_values(aod_55, 3)){
    terra <- aod_55[,,,1] %>%
      st_set_dimensions(3, values = "Terra", name="satellite") 
    terra$AOD55 <- as.numeric(NA)
    aod_55 <- c(aod_55, terra, along=3)
    rm("terra")
  }
  
  # Take average AOD by pixel and satellite, merge and crop
  aod_55_terra <- dplyr::filter(aod_55, satellite=="Terra")
  aod_55_terra <- st_apply(aod_55_terra, 1:2, mean, na.rm=T)
  names(aod_55_terra) <- "AOD55_terra"
  aod_55_aqua <- dplyr::filter(aod_55, satellite=="Aqua")
  aod_55_aqua <- st_apply(aod_55_aqua, 1:2, mean, na.rm=T)
  names(aod_55_aqua) <- "AOD55_aqua"
  aod_55 <- c(aod_55_terra, aod_55_aqua)
  
  # Return AOD 
  attr(aod_55, "date_file") <- as.character(as.Date(overpass_info$datetime[1]))
  aod_55
}

# Prepare MAIAC data for analysis
prep_maiac <- function(path_maiac, cat){
  
  # Read inputs
  cat <- read_rds(cat)
  
  # Pre-process MODIS AOD for each file
  aoi_extent <- st_buffer(cat, 2000)
  options(future.rng.onMisuse="ignore")
  plan(multisession, workers=10)
  aod_files <- future_map(path_maiac, preprocess_maiac, aoi_extent=aoi_extent)
  plan(sequential)

  # Stack
  parsing_txt <- paste0("c(", 
                        paste0("aod_files[[", 1:length(aod_files),"]]", 
                               collapse = ","), ", along=3)")
  aod_stack <- eval(parse(text=parsing_txt))
  
  # Reproject aqua and terra separately (multi-band not handled for now)
  aod_stack_terra <- aod_stack[1,,,]
  raster_model <- st_warp(aod_stack_terra, crs=st_crs(cat))
  aod_stack_terra <- st_warp(aod_stack_terra, raster_model, method="bilinear",
                             use_gdal=TRUE, no_data_value = -9999)
  aod_stack_aqua <- aod_stack[2,,,]
  aod_stack_aqua <- st_warp(aod_stack_aqua, raster_model, method="bilinear", 
                            use_gdal=TRUE, no_data_value = -9999)
  aod_stack_both <- c(aod_stack_terra, aod_stack_aqua)
  names(aod_stack_both) <- c("AOD55_terra", "AOD55_aqua")
  aod_stack_both <- aod_stack_both[st_buffer(cat, 950)]
  
  # Parse dates
  parsing_txt <- paste0("c(", 
                        paste0("attr(aod_files[[", 1:length(aod_files),
                               "]], 'date_file')", collapse = ","), ")")
  dates_all <- as.Date(eval(parse(text=parsing_txt)))
  aod_stack_both <- st_set_dimensions(aod_stack_both, 3, names="date", 
                                      values=dates_all)
  
  # Store and return paths
  maiac18 <- dplyr::filter(aod_stack_both, year(date)==2018)
  maiac19 <- dplyr::filter(aod_stack_both, year(date)==2019)
  maiac20 <- dplyr::filter(aod_stack_both, year(date)==2020)
  write_rds(maiac18, "database/original/2018/maiac.rds")
  write_rds(maiac19, "database/original/2019/maiac.rds")
  write_rds(maiac20, "database/original/2020/maiac.rds")
  list("maiac18"="database/original/2018/maiac.rds",
       "maiac19"="database/original/2019/maiac.rds",
       "maiac20"="database/original/2020/maiac.rds") 
}
