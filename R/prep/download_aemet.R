#-----------------------------------------------------------------------------#
#                        Download AEMET temperature data                      #
#-----------------------------------------------------------------------------#

library("climaemet")
library("tidyverse")

# API key: To be removed before commiting
apikey <- "*****"

# Query stations
stations <- aemet_stations(apikey)
stations <- stations %>%
  dplyr::filter(provincia %in% c("TARRAGONA", "BARCELONA", "LLEIDA", "GIRONA")) %>%
  rename(lon=longitud, lat=latitud, stationID=indicativo) %>%
  dplyr::select(stationID, lon, lat)
# write_csv(stations, "data/meteo/aemet_stations.csv")

# Get daily readings for each station
data_daily <- purrr::map_df(stations$stationID, function(x, apikey){
  aemet_daily_clim(x, apikey, "2018-01-01", "2020-12-31")}, apikey = apikey)
data_daily <- data_daily %>%
  dplyr::rename(stationID=indicativo, tavg = tmed, date=fecha) %>%
  dplyr::select(stationID, date, tavg, tmin, tmax) %>%
  mutate(across(c(tavg, tmin, tmax), ~as.numeric(gsub(",", ".", .x)))) 
data_daily <- data_daily[complete.cases(data_daily),]
# write_csv(data_daily, "data/meteo/aemet_readings.csv")