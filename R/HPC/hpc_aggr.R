#-----------------------------------------------------------------------------#
#              Cluster aggregation from daily to yearly exposures             #
#-----------------------------------------------------------------------------#

library("stars")

# Paths 
path_in <- "****"
path_out <- "****"

# Temperature ----
expo_files <- list.files(path_in, pattern="temp", full.names = T, recursive = T)
expo_day <- read_stars(expo_files, proxy = F)
st_crs(expo_day) <- 25831
expo_year <- adrop(st_apply(expo_day, 1:2, FUN = mean))
write_stars(expo_year[1], paste0(path_out, "temp_allperiod_pred.tif"))
write_stars(expo_year[2], paste0(path_out, "temp_allperiod_se.tif"))
rm("expo_files", "expo_day", "expo_year")
for(y in 2018:2020){
  # Read daily rasters, compute monthly/yearly aggregates, write and clean
  expo_files <- list.files(paste0(path_in, y), pattern="temp", full.names = T)
  expo_day <- read_stars(expo_files, proxy = F)
  st_crs(expo_day) <- 25831
  expo_year <- adrop(st_apply(expo_day, 1:2, FUN = mean))
  write_stars(expo_year[1], paste0(path_out, y, "/temp_", y, "_year_pred.tif"))
  write_stars(expo_year[2], paste0(path_out, y, "/temp_", y, "_year_se.tif"))
  rm("expo_files", "expo_day", "expo_year")
}


# PM2.5 ----
expo_files <- list.files(path_in, pattern="pm25", full.names = T, recursive = T)
expo_day <- read_stars(expo_files, proxy = F)
st_crs(expo_day) <- 25831
expo_year <- adrop(st_apply(expo_day, 1:2, FUN = mean))
write_stars(expo_year[1], paste0(path_out, "pm25_allperiod_pred.tif"))
write_stars(expo_year[2], paste0(path_out, "pm25_allperiod_se.tif"))
rm("expo_files", "expo_day", "expo_year")
for(y in 2018:2020){
  # Read daily rasters, compute monthly/yearly aggregates, write and clean
  expo_files <- list.files(paste0(path_in, y), pattern="pm25", full.names = T)
  expo_day <- read_stars(expo_files, proxy = F)
  st_crs(expo_day) <- 25831
  expo_year <- adrop(st_apply(expo_day, 1:2, FUN = mean))
  write_stars(expo_year[1], paste0(path_out, y, "/pm25_", y, "_year_pred.tif"))
  write_stars(expo_year[2], paste0(path_out, y, "/pm25_", y, "_year_se.tif"))
  rm("expo_files", "expo_day", "expo_year")
}

# PM10 ----
expo_files <- list.files(path_in, pattern="pm10", full.names = T, recursive = T)
expo_day <- read_stars(expo_files, proxy = F)
st_crs(expo_day) <- 25831
expo_year <- adrop(st_apply(expo_day, 1:2, FUN = mean))
write_stars(expo_year[1], paste0(path_out, "pm10_allperiod_pred.tif"))
write_stars(expo_year[2], paste0(path_out, "pm10_allperiod_se.tif"))
rm("expo_files", "expo_day", "expo_year")
for(y in 2018:2020){
  # Read daily rasters, compute monthly/yearly aggregates, write and clean
  expo_files <- list.files(paste0(path_in, y), pattern="pm10", full.names = T)
  expo_day <- read_stars(expo_files, proxy = F)
  st_crs(expo_day) <- 25831
  expo_year <- adrop(st_apply(expo_day, 1:2, FUN = mean))
  write_stars(expo_year[1], paste0(path_out, y, "/pm10_", y, "_year_pred.tif"))
  write_stars(expo_year[2], paste0(path_out, y, "/pm10_", y, "_year_se.tif"))
  rm("expo_files", "expo_day", "expo_year")
}

# NO2 ----
expo_files <- list.files(path_in, pattern="no2", full.names = T, recursive = T)
expo_day <- read_stars(expo_files, proxy = F)
st_crs(expo_day) <- 25831
expo_year <- adrop(st_apply(expo_day, 1:2, FUN = mean))
write_stars(expo_year[1], paste0(path_out, "no2_allperiod_pred.tif"))
write_stars(expo_year[2], paste0(path_out, "no2_allperiod_se.tif"))
rm("expo_files", "expo_day", "expo_year")
for(y in 2018:2020){
  # Read daily rasters, compute monthly/yearly aggregates, write and clean
  expo_files <- list.files(paste0(path_in, y), pattern="no2", full.names = T)
  expo_day <- read_stars(expo_files, proxy = F)
  st_crs(expo_day) <- 25831
  expo_year <- adrop(st_apply(expo_day, 1:2, FUN = mean))
  write_stars(expo_year[1], paste0(path_out, y, "/no2_", y, "_year_pred.tif"))
  write_stars(expo_year[2], paste0(path_out, y, "/no2_", y, "_year_se.tif"))
  rm("expo_files", "expo_day", "expo_year")
}

# O3 ----
expo_files <- list.files(path_in, pattern="o3", full.names = T, recursive = T)
expo_day <- read_stars(expo_files, proxy = F)
st_crs(expo_day) <- 25831
expo_year <- adrop(st_apply(expo_day, 1:2, FUN = mean))
write_stars(expo_year[1], paste0(path_out, "o3_allperiod_pred.tif"))
write_stars(expo_year[2], paste0(path_out, "o3_allperiod_se.tif"))
rm("expo_files", "expo_day", "expo_year")
for(y in 2018:2020){
  # Read daily rasters, compute monthly/yearly aggregates, write and clean
  expo_files <- list.files(paste0(path_in, y), pattern="o3", full.names = T)
  expo_day <- read_stars(expo_files, proxy = F)
  st_crs(expo_day) <- 25831
  expo_year <- adrop(st_apply(expo_day, 1:2, FUN = mean))
  write_stars(expo_year[1], paste0(path_out, y, "/o3_", y, "_year_pred.tif"))
  write_stars(expo_year[2], paste0(path_out, y, "/o3_", y, "_year_se.tif"))
  rm("expo_files", "expo_day", "expo_year")
}