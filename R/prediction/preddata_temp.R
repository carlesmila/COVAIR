#-----------------------------------------------------------------------------#
#                      Temperature prediction dataset                         #
#-----------------------------------------------------------------------------#

# Function to extract data for temperature exposure model fitting
preptemp_moddata <- function(meteo_stations, spstack, focal_preds, calendar,
                             ndvi, extrtemp){
  
  # Read stations and readings
  readings <- meteo_stations[2:4]
  readings <- map_df(readings, read_rds) %>%
    dplyr::select(ID, date, tavg, entity) %>%
    dplyr::filter(complete.cases(.))
  stations <- read_rds(meteo_stations$stations)
  tempdata <- inner_join(stations, readings, by=c("ID", "entity"))
  rm("meteo_stations", "readings")
  
  # Spatial predictors
  spstack <- read_rds(spstack)
  spstack <- spstack[c("x", "y", "dem", "slope", "aspect", "imd", "tcd", 
                       "coast_dist")]
  spstackfocal <- read_rds(focal_preds$spstack_focal)
  spstackfocal <- spstackfocal[c("imd_focal", "tcd_focal")]
  spstack <- c(spstack, spstackfocal)
  spatialpreds <- as.data.frame(st_extract(spstack, stations)) 
  spatialpreds$geometry <- NULL
  spatialpreds$ID <- stations$ID
  tempdata <- left_join(tempdata, spatialpreds, by = "ID") 
  rm("spstack", "spstackfocal", "spatialpreds")
  
  # Temporal predictors
  calendar <- read_rds(calendar)
  calendar <- calendar[c("date", "yday", "julian")]
  tempdata <- left_join(tempdata, calendar, by="date")
  rm("calendar")
  
  # Spatio-temporal predictors: NDVI
  tempdata$link_date <- floor_date(tempdata$date, "quarter")
  ndvi <- c(c(read_rds(ndvi$ndvi18), read_rds(focal_preds$ndvi18_focal)), 
            c(read_rds(ndvi$ndvi19), read_rds(focal_preds$ndvi19_focal)), 
            c(read_rds(ndvi$ndvi20), read_rds(focal_preds$ndvi20_focal)),
            along=3)
  ndvitemp <- as.data.frame(st_extract(ndvi, stations))
  tempdata <- left_join(tempdata, ndvitemp, by=c("link_date"="date", "geometry"="geometry"))
  tempdata$link_date <- NULL
  rm("ndvi", "ndvitemp", "focal_preds")
  
  # Extracted resampled ST predictors
  extr_preds <- read_rds(extrtemp)
  extr_preds <- extr_preds[sapply(extr_preds, function(x) !grepl(names(x)[1], "ntli"))]
  # Rest of preds simply by date
  extr_preds <- bind_rows(extr_preds) %>%
    group_by(ID, date) %>%
    summarise(across(.fns = function(x) unique(x[!is.na(x)])))
  tempdata <- left_join(tempdata, extr_preds, by = c("ID", "date"))
  
  # Stratify by year and write to disk, return paths
  st_geometry(tempdata) <- NULL
  write_csv(tempdata, "database/temp_pred/tempdata.csv")
  return("database/temp_pred/tempdata.csv")
} 