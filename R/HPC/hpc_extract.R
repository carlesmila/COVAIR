#-----------------------------------------------------------------------------#
#            HPC: Cluster extract ST predictors at station locations          #
#-----------------------------------------------------------------------------#

#### Prep ----
rm(list = ls())
library("tidyverse")
library("stars")
library("sf")
path_st <- "****/CM_exposure/database/original"
path_res <- "****"

#### Extraction function ----
extract_st <- function(ststars, stations){
  
  print(paste0("Extracting from file: ", ststars))
  
  # Get year info
  year <- strsplit(ststars, "/")[[1]]
  year <- year[length(year)-1]
  
  # Read file, if necessary parse dates
  if(grepl(".rds", ststars)){
    ststars_r <- read_rds(ststars)
  }else if(grepl(".tif", ststars)){
    ststars_r <- read_stars(ststars, proxy=F)
    if(grepl("tropomi_", ststars) & year == "2018"){
      ststars_r <- st_set_dimensions(ststars_r, 3, names = "date",
                                     values = as.Date(paste0("2018-04-30")) +
                                       st_get_dimension_values(ststars_r, 3) - 1)
    }else{
      ststars_r <- st_set_dimensions(ststars_r, 3, names = "date",
                                     values = as.Date(paste0(year, "-01-01")) +
                                       st_get_dimension_values(ststars_r, 3) - 1)
    }
    names(ststars_r) <- gsub("X", "", gsub(".tif", "", names(ststars_r)))
  }
  extracted <- as.data.frame(st_extract(ststars_r, stations))
  extracted$geometry <- NULL
  extracted$ID <- stations$ID
  rm("ststars_r")
  
  # for ntli: add date
  if("ntli" %in% names(extracted)){

    extracted$date <- as.Date(paste0(year, "-01-01"))
  }
  extracted
}


#### Read data ----

# Stations
meteo_stations <- paste0(path_st, "/meteo_stations.rds")
meteo_stations <- read_rds(meteo_stations)
print("Meteo stations: ")
print(meteo_stations)
ap_stations <- paste0(path_st, "/ap_stations.rds")
ap_stations <- read_rds(ap_stations)
print("AP stations: ")
print(ap_stations)

# Resampled datasets
res_data <- c(dir(path_res, ".rds", recursive = T, full.names = T), 
              dir(path_res, ".tif", recursive = T, full.names = T))
print("List of possible datasets to extract from: ")
print(res_data)

# #### Extract temperature ----
temp_paths <- c("2mtemp", "10muwind", "10mvwind", "ntli", "totlprec",
                "lst_aquaday", "lst_aquanight", "lst_terraday", "lst_terranight")
temp_paths <- res_data[grepl(paste(temp_paths, collapse = "|"), res_data)]
print("Temperature datasets: ")
print(temp_paths)
temp_extr <- map(temp_paths, extract_st, stations = meteo_stations)
write_rds(temp_extr, "****/extracted_temp.rds")

#### Extract Air pollution ----
ap_paths <- c("CAMSrean_", "CAMSanaly_", "ERA5_", "ntli", "maiac_", "omi_", "tropomi_")
ap_paths <- res_data[grepl(paste(ap_paths, collapse = "|"), res_data)]
print("Air pollution datasets: ")
print(ap_paths)
ap_extr <- map(ap_paths, extract_st, stations = ap_stations)
write_rds(ap_extr, "****/extracted_ap.rds")

#### Clean ----
rm(list = ls())