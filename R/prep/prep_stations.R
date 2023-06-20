#-----------------------------------------------------------------------------#
#                   Pre-processing data functions: Stations                   #
#-----------------------------------------------------------------------------#

# Pre-process station data
prep_ap_stations <- function(path_apmanual, path_apauto){
  
  #### Prepare automatic data ####
  apauto <- read_csv(path_apauto) 
  apauto <- apauto %>% # Clean columns and names
    dplyr::filter(CONTAMINANT %in% c("PM10", "PM2.5", "NO2", "O3")) %>%
    dplyr::rename(ID = `CODI EOI`, date = DATA, pollutant = CONTAMINANT,
                  st_type = `TIPUS ESTACIO`, area_type = `AREA URBANA`,
                  lat = `LATITUD`, lon = `LONGITUD`) %>%
    dplyr::select(ID, date, st_type, area_type, lat, lon, pollutant, ends_with("h"))
  apauto <- apauto %>% # Fix dates, wide to long, delete missing
    mutate(date = as.Date(date, format = "%d/%m/%Y"),
           measure_type = "automatic") %>%
    dplyr::filter(year(date) %in% 2018:2020|date == as.Date("2017-12-31")) %>%
    mutate(date = as.character(date)) %>%
    pivot_longer(cols = ends_with("h"), names_to = "hour", values_to = "conc") %>%
    dplyr::filter(!is.na(conc))
  
  #### Daily averages: NO2, PM2.5, PM10 ####
  apauto_avg <- apauto %>%
    dplyr::filter(pollutant %in% c("PM10", "PM2.5", "NO2"),
           year(date) %in% 2018:2020) %>%
    group_by(ID, date, st_type, area_type, measure_type, lat, lon, pollutant) %>%
    summarise(conc=mean(conc), n=n(), .groups="keep") %>%
    ungroup() %>%
    dplyr::filter(n>=18) %>% # At least 18/24 of the measurements
    dplyr::select(-n)
  
  ####  Daily max 8-hour O3  ####
  apauto_8hmax <- apauto %>%
    dplyr::filter(pollutant == "O3") %>%
    mutate(hour = gsub("h", "", hour),
           datetime = ymd_h(paste(date, hour, sep = " "))) %>%
    dplyr::select(ID, datetime, conc) %>%
    arrange(ID, datetime) %>%
    pad(group = "ID", by = "datetime", interval="hour",
        start_val = ymd_h("2017-12-31 01"), end_val=ymd_h("2020-12-31 23"),
        break_above = 2)
  O3_8h <- data.frame() 
  for(id in unique(apauto_8hmax$ID)){ # Compute 8h-averages
    O3_id <- dplyr::filter(apauto_8hmax, ID==id) 
    O3_id$conc2 <- NA
    O3_id$N <- NA
    for(i in 8:nrow(O3_id)){
      O3_id$conc2[i] <- mean(O3_id$conc[(i-7):i], na.rm=T)
      O3_id$N[i] <- sum(!is.na(O3_id$conc[(i-7):i]))
    }
    O3_8h <- bind_rows(O3_8h, O3_id)
    rm("O3_id")
  }
  O3_8h <- O3_8h %>% # Get daily max 8h
    dplyr::filter(!is.na(conc2) & !is.na(N) & year(datetime) %in% 2018:2020) %>%
    dplyr::filter(N >= 6) %>% # At least 6/8 of the hourly measurements
    dplyr::mutate(date = as.character(as.Date(datetime))) %>%
    group_by(ID, date) %>%
    summarise(conc = max(conc2), N=n()) %>%
    dplyr::filter(N>=18) %>% # At least 18/24 of the 8-hour averages measurements
    dplyr::select(-N) %>%
    ungroup()
  apauto_8hmax <- apauto %>% # Other info
    dplyr::filter(pollutant == "O3") %>%
    dplyr::select(ID, date, st_type, area_type, measure_type, lat, lon, pollutant) %>%
    dplyr::filter(!duplicated(.))
  apauto_8hmax <- inner_join(apauto_8hmax, O3_8h, by = c("ID", "date"))
  
  #### Prepare manual data ####
  apmanual <- read_csv(path_apmanual) 
  apmanual <- apmanual %>% # Clean columns and names
    dplyr::filter(ANY %in% 2018:2020) %>%
    dplyr::filter(`NOM CONTAMINANT` %in% c("PM10", "PM2.5")) %>%
    dplyr::rename(ID = `CODI EOI`, pollutant = `NOM CONTAMINANT`,
                  year = ANY, month = MES, 
                  type = `TIPUS ESTACIÓ`, lat = `LATITUD`, lon = `LONGITUD`) %>%
    dplyr::select(ID, year, month, type, lat, lon, pollutant, starts_with("D"))
  apmanual <- apmanual %>% # Fix station types
    mutate(st_type = strsplit(type, "-")[[1]][1],
           area_type = strsplit(type, "-")[[1]][2],
           measure_type = "manual") %>%
    dplyr::select(-type)
  apmanual <- apmanual %>% # wide to long, delete missing, fix dates
    pivot_longer(cols = starts_with("D"), names_to = "day", values_to = "conc") %>%
    mutate(month = str_pad(month, 2, "left", "0"),
           day = gsub("D", "", day),
           date = paste(year, month, day, sep = "-")) %>%
    dplyr::filter(!is.na(conc)) %>%
    dplyr::select(-year, -month, -day)
  
  #### Merge and georeference stations ####
  readings <- bind_rows(apmanual, apauto_avg, apauto_8hmax)
  # Some PM10 stations have both manual and auto measures, keep auto
  readings_pm10 <- dplyr::filter(readings, pollutant == "PM10") %>%
    dplyr::group_by(ID, date) %>%
    mutate(N = n()) %>%
    ungroup()
  readings_pm10_1 <- dplyr::filter(readings_pm10, N == 1)
  readings_pm10_2 <- dplyr::filter(readings_pm10, N > 1) %>%
    dplyr::filter(measure_type == "automatic")
  readings <- bind_rows(dplyr::filter(readings, pollutant != "PM10"),
                        readings_pm10_1, readings_pm10_2)
  # Sometimes we have very small changes in location (<20m),
  # Take the most frequent location by monitor
  stations <- readings %>%
    dplyr::group_by(ID, lat, lon, st_type, area_type) %>%
    summarise(N = n()) %>%
    group_by(ID) %>%
    dplyr::filter(N == max(N)) %>%
    ungroup() %>%
    dplyr::select(-N)
  stations <- stations %>%  # georeference
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs = 25831)

  #### Store, and return paths ####
  write_rds(stations, "database/original/ap_stations.rds")
  readings <- dplyr::select(readings, ID, date, pollutant, measure_type, conc) %>%
    mutate(date = as.Date(date))
  write_rds(dplyr::filter(readings, year(date)==2018), "database/original/2018/ap_readings.rds")
  write_rds(dplyr::filter(readings, year(date)==2019), "database/original/2019/ap_readings.rds")
  write_rds(dplyr::filter(readings, year(date)==2020), "database/original/2020/ap_readings.rds")
  list("stations"="database/original/ap_stations.rds",
       "readings18"="database/original/2018/ap_readings.rds",
       "readings19"="database/original/2019/ap_readings.rds",
       "readings20"="database/original/2020/ap_readings.rds")
}


prep_meteo_stations <- function(path_aemetread, path_aemetstat,
                                path_xemaread, path_xemastat){
  
  # Prepare station data
  aemet_stations <- read_csv(path_aemetstat) %>%
    st_as_sf(coords=c("lon", "lat"), crs=4326) %>%
    st_transform(crs=25831) %>%
    rename(ID = stationID) %>%
    mutate(entity = "aemet") %>%
    dplyr::filter(ID != "0200E") # Already in xema
  xema_stations <- read_csv(path_xemastat) %>%
    st_as_sf(coords=c("LONGITUD", "LATITUD"), crs=4326) %>%
    st_transform(crs=25831) %>%
    rename(ID = CODI_ESTACIO) %>%
    dplyr::select(ID) %>%
    mutate(entity = "xema") %>%
    dplyr::filter(ID != "D6") # Station beyond the French border

  # Prepare readings
  aemet_readings <- read_csv(path_aemetread) %>%
    rename(ID = stationID) %>%
    mutate(entity = "aemet")
  xema_readings <- read_csv(path_xemaread)
  xema_readings <- xema_readings %>%
    mutate(type = case_when(CODI_VARIABLE == 32 ~ "tavg",
                            CODI_VARIABLE == 40 ~ "tmax",
                            CODI_VARIABLE == 42 ~ "tmin")) %>%
    rename(ID = CODI_ESTACIO, temp=VALOR_LECTURA) %>%
    dplyr::select(ID, DATA_LECTURA, type, temp) 
  xema_readings <- xema_readings %>%
    mutate(date = as.Date(substr(DATA_LECTURA, 1, 10), format=c("%d/%m/%Y"))) %>%
    dplyr::filter(ID != "D6") %>% # Station beyond the French border
    group_by(ID, date, type) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    dplyr::filter(n==48) %>%
    dplyr::select(-n)
  xema_readings <- xema_readings %>%
    pivot_wider(names_from="type", values_from="temp") %>%
    group_by(ID, date) %>%
    summarise(tavg = mean(tavg, na.rm=T),
              tmax = max(tmax, na.rm=T),
              tmin = min(tmin, na.rm=T)) %>%
    ungroup()  %>%
    mutate(across(c("tavg", "tmax", "tmin"), function(x) ifelse(is.infinite(x), NA, x))) %>%
    mutate(entity = "xema")
  
  # Prepare yearly data
  readings <- bind_rows(xema_readings, aemet_readings)
  stations <- bind_rows(xema_stations, aemet_stations)
  readings <- dplyr::filter(readings, ID %in% stations$ID)
  stations <- dplyr::filter(stations, ID %in% readings$ID)
  readings18 <- dplyr::filter(readings, year(date)==2018)
  readings19 <- dplyr::filter(readings, year(date)==2019)
  readings20 <- dplyr::filter(readings, year(date)==2020)
  
  # Write and return
  write_rds(stations, "database/original/meteo_stations.rds")
  write_rds(readings18, "database/original/2018/meteo_readings.rds")
  write_rds(readings19, "database/original/2019/meteo_readings.rds")
  write_rds(readings20, "database/original/2020/meteo_readings.rds")
  
  list("stations"="database/original/meteo_stations.rds",
       "readings18"="database/original/2018/meteo_readings.rds",
       "readings19"="database/original/2019/meteo_readings.rds",
       "readings20"="database/original/2020/meteo_readings.rds")
}