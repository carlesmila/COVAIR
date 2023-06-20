#-----------------------------------------------------------------------------#
#                         Station and population maps                         #
#-----------------------------------------------------------------------------#

library("targets")
library("tidyverse")
library("sf")
library("tmap")

# Load station locations
tar_load(ap_stations)
ap_readings <- bind_rows(read_rds(ap_stations$readings18),
                         read_rds(ap_stations$readings19),
                         read_rds(ap_stations$readings20)) %>%
  rename(exposure = pollutant,
         measure = conc) 
ap_stations <- read_rds(ap_stations$stations)
tar_load("meteo_stations")
meteo_readings <- bind_rows(read_rds(meteo_stations$readings18),
                            read_rds(meteo_stations$readings19),
                            read_rds(meteo_stations$readings20)) |> 
  mutate(exposure = "Temperature")
meteo_stations <- read_rds(meteo_stations$stations)

# AP stations per pollutant
ap_readings <- group_by(ap_readings, ID, exposure) |> 
  summarise(N = n())
meteo_readings <- group_by(meteo_readings, ID, exposure) |> 
  summarise(N = n())
ap_both <- left_join(ap_readings, ap_stations, by = "ID") |> 
  st_as_sf() |> 
  mutate(exposure = case_when(
    exposure == "PM2.5" ~ "PM\U02082.\U02085",
    exposure == "PM10" ~ "PM\U02081\U02080",
    exposure == "NO2" ~ "NO\U02082",
    exposure == "O3" ~ "O\U02083"
  ))
temp_both <- left_join(meteo_readings, meteo_stations, by = "ID") |> 
  st_as_sf()
allstations <- rbind(select(temp_both, exposure, N), select(ap_both, exposure, N))
allstations$exposure <- fct_relevel(allstations$exposure,
                                    c("Temperature", "PM\U02082.\U02085",
                                      "PM\U02081\U02080", "NO\U02082", "O\U02083"))
# Population density
tar_load(spstack)
spstack <- read_rds(spstack)
spstack <- spstack["popu_dens"]

# Map
tar_load("cat")
cat <- read_rds(cat)
stationmap <- tm_shape(cat, bbox = st_bbox(cat)) +
  tm_polygons(alpha = 0.1) +
  tm_shape(allstations, bbox = st_bbox(cat)) +
  tm_dots(col = "N", title = "Number\nof days", size = 0.2,
          palette = "Spectral") +
  tm_facets("exposure", nrow = 2)  +
  tm_layout(legend.outside.size = 0.15, main.title = "A")
popmap <- tm_shape(spstack) +
  tm_raster(n = 5, style = "fixed", breaks = c(0, 1, 10, 100, 1000, 3526),
            title = "Population density", palette = "-magma") +
  tm_shape(cat, bbox = st_bbox(cat)) +
  tm_borders(col = "black", lwd = 0.1) +
  tm_layout(legend.position = c("right", "bottom"), frame.lwd = 0, main.title = "B")
bothmap <- tmap_arrange(stationmap, popmap, nrow = 2, heights = c(0.5, 0.5))
tmap_save(bothmap, "figures/manuscript/stationmap.png", width = 8, height = 9)
