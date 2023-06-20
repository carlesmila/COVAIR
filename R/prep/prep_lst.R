#-----------------------------------------------------------------------------#
#                       Pre-processing LST data functions                     #
#-----------------------------------------------------------------------------#

# Preprocess raw LST files from MODIS
preprocess_lst <- function(path_lst, aoi_extent){
  
  # Get overpass information: date
  date_file <- strsplit(path_lst, ".", fixed=T)[[1]][2]
  origin_file <- as.Date(paste0(substr(date_file, 2, 5),"-01-01"))
  date_file <- as.character(origin_file + as.integer(substr(date_file, 6, 8)) -1)

  # Read file
  hdf_data <- get_subdatasets(path_lst)
  
  # Read day LST and QA layers, crop extent, kelvin to celsius
  auxfile1 <- paste0(date_file, "_dayLST.tif")
  gdal_translate(hdf_data[1], dst_dataset = auxfile1)
  auxfile2 <- paste0(date_file, "_dayQA.tif")
  gdal_translate(hdf_data[2], dst_dataset = auxfile2)
  dayLST <- read_stars(auxfile1) 
  names(dayLST) <- "dayLST"
  dayLST <- st_crop(dayLST, st_bbox(st_transform(aoi_extent, crs=st_crs(dayLST))))
  dayLST <- mutate(dayLST, dayLST = as.numeric(dayLST) -273.15)
  dayQA <- read_stars(auxfile2) 
  names(dayQA) <- "dayQA"
  dayQA <- st_crop(dayQA, st_bbox(st_transform(aoi_extent, crs=st_crs(dayQA))))
  unlink(c(auxfile1, auxfile2))
  
  # Filter out average emissivity error > 0.04 and LST error > 3K
  dayQA <- mutate(dayQA, dayQA=R.utils::intToBin(dayQA))
  dayQA <- dayQA %>%
    mutate(dayQA_bin = substr(dayQA, 7, 8)=="00",
           dayQA_bin = ifelse(substr(dayQA, 7, 8)=="01"& 
                                substr(dayQA, 3, 4)!="11"&
                                substr(dayQA, 1, 2)!="11",T,dayQA_bin))
  dayLST <- c(dayLST, dayQA)
  dayLST <- mutate(dayLST, dayLST = ifelse(dayQA_bin==1, dayLST, NA)) %>%
    dplyr::select(dayLST)
  
  # Read night LST and QA layers, kelvin to celsius
  auxfile3 <- paste0(date_file, "_nightLST.tif")
  gdal_translate(hdf_data[5], dst_dataset = auxfile3)
  auxfile4 <- paste0(date_file, "_nightQA.tif")
  gdal_translate(hdf_data[6], dst_dataset = auxfile4)
  nightLST <- read_stars(auxfile3) 
  names(nightLST) <- "nightLST"
  nightLST <- st_crop(nightLST, st_bbox(st_transform(aoi_extent, crs=st_crs(nightLST))))
  nightLST <- mutate(nightLST, nightLST = as.numeric(nightLST) -273.15)
  nightQA <- read_stars(auxfile4) 
  names(nightQA) <- "nightQA"
  nightQA <- st_crop(nightQA, st_bbox(st_transform(aoi_extent, crs=st_crs(nightQA))))
  unlink(c(auxfile3, auxfile4))
  
  # Filter out average emissivity error > 0.04 and LST error > 3K
  nightQA <- mutate(nightQA, nightQA=R.utils::intToBin(nightQA))
  nightQA <- nightQA %>%
    mutate(nightQA_bin = substr(nightQA, 7, 8)=="00",
           nightQA_bin = ifelse(substr(nightQA, 7, 8)=="01"& 
                                  substr(nightQA, 3, 4)!="11"&
                                  substr(nightQA, 1, 2)!="11",T,nightQA_bin))
  nightLST <- c(nightLST, nightQA)
  nightLST <- mutate(nightLST, nightLST = ifelse(nightQA_bin==1, nightLST, NA)) %>%
    dplyr::select(nightLST)
  
  # Merge day and night layers
  fileLST <- c(dayLST, nightLST)
  attr(fileLST, "date_file") <- date_file
  fileLST
}

# Prepare LST data for analysis
prep_lst <- function(paths_lst, cat, maiac, sat){
  
  # Read inputs
  cat <- read_rds(cat)
  maiac <- read_rds(maiac$maiac18)
  maiac <- maiac[1,,,1]
  aoi_extent <- st_buffer(cat, 2000)
  
  # Pre-process MODIS LST for each terra/aqua file
  options(future.rng.onMisuse="ignore")
  plan(multisession, workers=10)
  lst_files <- future_map(paths_lst, preprocess_lst, aoi_extent=aoi_extent)
  plan(sequential)

  # Stack LST files, retrieve dates
  parsing_txt <- paste0("c(",
                        paste0("lst_files[[", 1:length(lst_files),"]]",
                               collapse = ","), ", along=3)")
  lst_stack <- eval(parse(text=parsing_txt))
  names(lst_stack) <- paste0(names(lst_stack), "_", sat)
  parsing_txt <- paste0("c(",
                        paste0("attr(lst_files[[", 1:length(lst_files),
                               "]], 'date_file')", collapse = ","), ")")
  lst_dates <- as.Date(eval(parse(text=parsing_txt)))
  lst_stack <- st_set_dimensions(lst_stack, 3, names="date", values=lst_dates)
  rm("lst_files")
  
  # Reproject LST day/night at MAIAC grid separately
  raster_model <- st_warp(lst_stack[1], maiac)
  lstday_stack <- st_warp(lst_stack[1], raster_model, method="bilinear",
                            use_gdal=TRUE, no_data_value = -9999) %>%
    st_set_dimensions(3, names="date", values=lst_dates)
  names(lstday_stack) <- names(lst_stack)[1]
  lstnight_stack <- st_warp(lst_stack[2], raster_model, method="bilinear",
                            use_gdal=TRUE, no_data_value = -9999) %>%
    st_set_dimensions(3, names="date", values=lst_dates)
  names(lstnight_stack) <- names(lst_stack)[2]
  rm("lst_stack")
  
  # Stack, clean
  lst_stack <- c(lstday_stack, lstnight_stack)
  lst_stack <- lst_stack[st_buffer(cat, 950)]
  rm("lstday_stack", "lstnight_stack")
  
  # Store and return paths
  lst18 <- dplyr::filter(lst_stack, year(date)==2018)
  lst19 <- dplyr::filter(lst_stack, year(date)==2019)
  lst20 <- dplyr::filter(lst_stack, year(date)==2020)
  rm("lst_stack")
  write_rds(lst18, paste0("database/original/2018/lst_", sat, ".rds"))
  write_rds(lst19, paste0("database/original/2019/lst_", sat, ".rds"))
  write_rds(lst20, paste0("database/original/2020/lst_", sat, ".rds"))
  list("lst18"=paste0("database/original/2018/lst_", sat, ".rds"),
       "lst19"=paste0("database/original/2019/lst_", sat, ".rds"),
       "lst20"=paste0("database/original/2020/lst_", sat, ".rds"))
}
