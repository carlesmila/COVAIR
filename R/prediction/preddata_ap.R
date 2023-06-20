#-----------------------------------------------------------------------------#
#                      Air pollution prediction dataset                       #
#-----------------------------------------------------------------------------#

# Function to extract data for air pollution model fitting
prepap_moddata <- function(ap_stations, spstack, focal_preds, calendar,
                             ndvi, path_extrap){
  
  # Read stations and readings
  readings <- ap_stations[2:4]
  readings <- map_df(readings, read_rds)
  readings_pm <- dplyr::filter(readings, pollutant %in% c("PM10", "PM2.5")) %>%
    dplyr::select(ID, date, pollutant, measure_type) %>%
    pivot_wider(names_from = pollutant, values_from = measure_type, 
                names_prefix = "measure_") 
  readings <- readings %>%
    dplyr::select(-measure_type) %>%
    pivot_wider(id_cols = c("ID", "date"), names_from = "pollutant", values_from = "conc")
  readings <- left_join(readings, readings_pm, by=c("ID", "date"))
  stations <- ap_stations$stations
  stations <- read_rds(stations)
  apdata <- inner_join(stations, readings, by="ID")
  apdata <- mutate(apdata, date = as.Date(date))
  rm("ap_stations", "readings")
  
  # Spatial predictors
  spstack <- read_rds(spstack)
  spstack <- spstack[setdiff(names(spstack),
                             c("slope", "aspect", "school_dist",
                               "industry", "airport_dist", "port_dist"))]
  spstackfocal <- read_rds(focal_preds$spstack_focal) 
  spstack <- c(spstack, spstackfocal) %>%
    mutate(primary_dens = primary_dens + secondary_dens,
           primary_dens_focal = primary_dens_focal + secondary_dens_focal) %>%
    dplyr::select(-secondary_dens, -secondary_dens_focal, -industry_focal)
  spatialpreds <- as.data.frame(st_extract(spstack, stations)) 
  spatialpreds$geometry <- NULL
  spatialpreds$ID <- stations$ID
  apdata <- left_join(apdata, spatialpreds, by = "ID") 
  rm("spstack", "spstackfocal", "spatialpreds")
  
  # Temporal predictors
  calendar <- read_rds(calendar)
  calendar <- calendar[c("date", "yday", "julian", "holiday", "dust")]
  apdata <- left_join(apdata, calendar, by="date")
  rm("calendar")
  
  # Spatio-temporal predictors: NDVI
  apdata$link_date <- floor_date(apdata$date, "quarter")
  ndvi <- c(c(read_rds(ndvi$ndvi18), read_rds(focal_preds$ndvi18_focal)), 
            c(read_rds(ndvi$ndvi19), read_rds(focal_preds$ndvi19_focal)), 
            c(read_rds(ndvi$ndvi20), read_rds(focal_preds$ndvi20_focal)),
            along=3)
  ndviap <- as.data.frame(st_extract(ndvi, stations))
  apdata <- left_join(apdata, ndviap, by=c("link_date"="date", "geometry"="geometry"))
  apdata$link_date <- NULL
  rm("ndvi", "ndviap", "focal_preds")
  
  # Extracted resampled ST predictors
  extr_preds <- read_rds(path_extrap)
  ntli <- extr_preds[sapply(extr_preds, function(x) grepl(names(x)[1], "ntli"))]
  extr_preds <- extr_preds[sapply(extr_preds, function(x) !grepl(names(x)[1], "ntli"))]
  # NTLI by floor date
  apdata$link_date <- floor_date(apdata$date, "year")
  ntli <- bind_rows(ntli)
  apdata <- left_join(apdata, ntli, by = c("ID"="ID", "link_date"="date"))
  apdata$link_date <- NULL
  # Rest of preds simply by date and ID
  extr_preds <- bind_rows(extr_preds) %>%
    group_by(ID, date) %>%
    summarise(across(.fns = function(x) ifelse(all(is.na(x)), NA, unique(x[!is.na(x)]))))
  apdata <- left_join(apdata, extr_preds, by = c("ID", "date"))
  
  # Stratify by year and write to disk, return paths
  st_geometry(apdata) <- NULL
  write_csv(apdata, "database/ap_pred/apdata.csv")
  return("database/ap_pred/apdata.csv")
} 
