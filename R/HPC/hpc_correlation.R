#-----------------------------------------------------------------------------#
#                     STcorp: data comparison extraction                      #
#-----------------------------------------------------------------------------#

library("tidyverse")
library("stars")
library("sf")
library("units")

pathroot <- "****"

# 0. Generate 1000 sampling points grid ----
catmask <- read_rds(paste0(pathroot, "CM_exposure/database/original/cat.rds"))
catmask <- st_buffer(catmask, -1000)
catpoints <- st_sf(geom = st_sample(catmask, 1000, type = "regular"))
catpoints$ID <- 1:nrow(catpoints)
st_write(catpoints, paste0(pathroot, "HPC_outputs/correlation/catpoints.gpkg"))
rm("catmask")

# 5. COVAIR: Extract daily PM2.5, PM10, NO2, O3 ----
expoall <- data.frame()

for(y in 2018:2020){
  print(y)
  
  if(y == 2018){ # May-December
    monthvect <- str_pad(5:12, 2, pad = "0")
  }else{ # all year
    monthvect <- str_pad(1:12, 2, pad = "0")
  }
  
  for(m in monthvect){ 
    print(m)
    
    # temp
    temp <- read_stars(paste0(pathroot, "HPC_outputs/expo_pred2/", y, "/temp_", y, "_", m, ".nc"))[1]
    st_crs(temp) <- st_crs(catpoints)
    temp <- st_drop_geometry(st_as_sf(drop_units(st_extract(temp, catpoints))))
    temp$ID <- catpoints$ID
    temp <- pivot_longer(temp, -ID, names_to = "date", values_to = "temp")
    
    # pm25
    pm25 <- read_stars(paste0(pathroot, "HPC_outputs/expo_pred2/", y, "/pm25_", y, "_", m, ".nc"))[1]
    st_crs(pm25) <- st_crs(catpoints)
    pm25 <- st_drop_geometry(st_as_sf(drop_units(st_extract(pm25, catpoints))))
    pm25$ID <- catpoints$ID
    pm25 <- pivot_longer(pm25, -ID, names_to = "date", values_to = "PM2.5")

    # pm10
    pm10 <- read_stars(paste0(pathroot, "HPC_outputs/expo_pred2/", y, "/pm10_", y, "_", m, ".nc"))[1]
    st_crs(pm10) <- st_crs(catpoints)
    pm10 <- st_drop_geometry(st_as_sf(drop_units(st_extract(pm10, catpoints))))
    pm10$ID <- catpoints$ID
    pm10 <- pivot_longer(pm10, -ID, names_to = "date", values_to = "PM10")

    # no2
    no2 <- read_stars(paste0(pathroot, "HPC_outputs/expo_pred2/", y, "/no2_", y, "_", m, ".nc"))[1]
    st_crs(no2) <- st_crs(catpoints)
    no2 <- st_drop_geometry(st_as_sf(drop_units(st_extract(no2, catpoints))))
    no2$ID <- catpoints$ID
    no2 <- pivot_longer(no2, -ID, names_to = "date", values_to = "NO2")

    # o3
    o3 <- read_stars(paste0(pathroot, "HPC_outputs/expo_pred2/", y, "/o3_", y, "_", m, ".nc"))[1]
    st_crs(o3) <- st_crs(catpoints)
    o3 <- st_drop_geometry(st_as_sf(drop_units(st_extract(o3, catpoints))))
    o3$ID <- catpoints$ID
    o3 <- pivot_longer(o3, -ID, names_to = "date", values_to = "O3")

    # merge and clean
    expomonth <- full_join(temp, pm25, by = c("ID", "date")) |> 
      full_join(pm10, by = c("ID", "date")) |> 
      full_join(no2, by = c("ID", "date")) |> 
      full_join(o3, by = c("ID", "date")) 
    expoall <- bind_rows(expoall, expomonth)
    rm(temp, pm25, pm10, no2, o3, expomonth)
  }
}
write_csv(expoall, file = paste0(pathroot, "HPC_outputs/correlation/correlation.csv"))
rm(expoall, pathroot, monthvect, catpoints)