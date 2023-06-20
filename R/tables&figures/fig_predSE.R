#-----------------------------------------------------------------------------#
#                           Prediction SE figure                              #
#-----------------------------------------------------------------------------#

library("targets")
library("tidyverse")
library("stars")
library("tmap")

# boundaries ----
metro_towns <- c("Badalona",	"el Prat de Llobregat",	"Sant Boi de Llobregat",
                 "Badia del Vallès",	"Esplugues de Llobregat",	"Sant Climent de Llobregat",
                 "Barberà del Vallès",	"Gavà",	"Sant Cugat del Vallès",
                 "Barcelona",	"l'Hospitalet de Llobregat",	"Sant Feliu de Llobregat",
                 "Begues",	"la Palma de Cervelló",	"Sant Joan Despí",
                 "Castellbisbal",	"Molins de Rei",	"Sant Just Desvern",
                 "Castelldefels", "Montcada i Reixac",	"Sant Vicenç dels Horts",
                 "Cerdanyola del Vallès",	"Montgat",	"Santa Coloma de Cervelló",
                 "Cervelló",	"Pallejà",	"Santa Coloma de Gramenet",
                 "Corbera de Llobregat",	"Ripollet",	"Tiana",
                 "Cornellà de Llobregat",	"Sant Adrià de Besòs",	"Torrelles de Llobregat",
                 "el Papiol",	"Sant Andreu de la Barca",	"Viladecans")
bcnmetro <- st_read("data/boundaries/bm5mv21sh0tpm1_20200601_0.shp") |>
  dplyr::filter(NOMMUNI %in% metro_towns) |>
  st_union() |>
  st_cast("POLYGON") |>
  st_as_sf()
bcnmetro <- bcnmetro[1,]
bcn <- st_read("data/boundaries/bm5mv21sh0tpm1_20200601_0.shp") %>%
  dplyr::filter(NOMMUNI %in% "Barcelona")

# Day 1: Dust 24-01-2020
# Day 2: Lockdown 01-04-2020
# Day 3: Heatwave 08-08-2020
# temp ----
temp_1 <- read_ncdf("outputs/expo_pred2/2020/temp_2020_01.nc")[,,,24]
temp_2 <- read_ncdf("outputs/expo_pred2/2020/temp_2020_04.nc")[,,,1]
temp_3 <- read_ncdf("outputs/expo_pred2/2020/temp_2020_08.nc")[,,,8]
temp <- c(temp_1, temp_2, temp_3, along = 3) %>%
  st_set_dimensions(3, values = c("2020-01-24 Dust", "2020-04-01 Lockdown", "2020-08-08 Heatwave"))
st_crs(temp) <- 25831
rm("temp_1", "temp_2", "temp_3")
expobreaks <- seq(0, 5, 1)
m1 <- tm_shape(temp["se"]) +
  tm_raster(palette = hcl.colors(8, "Lajolla"),
            style = "fixed", breaks = expobreaks,
            title = "Temp. (ºC)", midpoint = NA) +
  tm_facets(nrow = 1, ncol = 3) +
  tm_layout(legend.outside.size = 0.15, panel.label.size = 0.75)

# pm25 ----
pm25_1 <- read_ncdf("outputs/expo_pred2/2020/pm25_2020_01.nc")[,,,24]
pm25_2 <- read_ncdf("outputs/expo_pred2/2020/pm25_2020_04.nc")[,,,1]
pm25_3 <- read_ncdf("outputs/expo_pred2/2020/pm25_2020_08.nc")[,,,8]
pm25 <- c(pm25_1, pm25_2, pm25_3, along = 3) %>%
  st_set_dimensions(3, values = c("2020-01-24 Dust", "2020-04-01 Lockdown", "2020-08-08 Heatwave"))
st_crs(pm25) <- 25831
rm("pm25_1", "pm25_2", "pm25_3")
expobreaks <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15)
m2 <- tm_shape(pm25["se"]) +
  tm_raster(palette = hcl.colors(8, "Lajolla"),
            style = "fixed", breaks = expobreaks,
            title = "PM\U02082.\U02085 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_facets(nrow = 1, ncol = 3) +
  tm_layout(legend.outside.size = 0.14, panel.label.size = 0.9,
            legend.position = c("right", "center"))
m2.1 <- tm_shape(pm25["se"]) +
  tm_raster(palette = hcl.colors(8, "Lajolla"),
            style = "fixed", breaks = expobreaks,
            title = "PM\U02082.\U02085 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_shape(bcnmetro) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_facets(nrow = 1, ncol = 3) +
  tm_layout(legend.outside.size = 0.14, panel.label.size = 0.9,
            legend.position = c("right", "center"),
            main.title = "A")
pm25_bcn <- st_crop(pm25, bcnmetro)
m2.2 <- tm_shape(pm25_bcn["se"]) +
  tm_raster(palette = hcl.colors(8, "Lajolla"),
            style = "fixed", breaks = expobreaks,
            title = "PM\U02082.\U02085 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_shape(bcn) +
  tm_borders(col = "black", lwd = 1) +
  tm_facets(nrow = 1, ncol = 3) +
  tm_layout(legend.outside.size = 0.14, panel.label.size = 0.9,
            legend.position = c("right", "center"),
            main.title = "B")


# pm10 ----
pm10_1 <- read_ncdf("outputs/expo_pred2/2020/pm10_2020_01.nc")[,,,24]
pm10_2 <- read_ncdf("outputs/expo_pred2/2020/pm10_2020_04.nc")[,,,1]
pm10_3 <- read_ncdf("outputs/expo_pred2/2020/pm10_2020_08.nc")[,,,8]
pm10 <- c(pm10_1, pm10_2, pm10_3, along = 3) %>%
  st_set_dimensions(3, values = c("2020-01-24 Dust", "2020-04-01 Lockdown", "2020-08-08 Heatwave"))
st_crs(pm10) <- 25831
rm("pm10_1", "pm10_2", "pm10_3")
expobreaks <- c(0, 5, 10, 15, 20, 30, 40, 50)
m3 <- tm_shape(pm10["se"]) +
  tm_raster(palette = hcl.colors(8, "Lajolla"),
            style = "fixed", breaks = expobreaks,
            title = "PM\U02081\U02080 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_facets(nrow = 1, ncol = 3) +
  tm_layout(legend.outside.size = 0.15, panel.label.size = 0.75)

# no2 ----
no2_1 <- read_ncdf("outputs/expo_pred2/2020/no2_2020_01.nc")[,,,24]
no2_2 <- read_ncdf("outputs/expo_pred2/2020/no2_2020_04.nc")[,,,1]
no2_3 <- read_ncdf("outputs/expo_pred2/2020/no2_2020_08.nc")[,,,8]
no2 <- c(no2_1, no2_2, no2_3, along = 3) %>%
  st_set_dimensions(3, values = c("2020-01-24 Dust", "2020-04-01 Lockdown", "2020-08-08 Heatwave"))
st_crs(no2) <- 25831
rm("no2_1", "no2_2", "no2_3")
expobreaks <- c(0, 2.5, 5, 7.5, 10, 15, 20, 25)
m4 <- tm_shape(no2["se"]) +
  tm_raster(palette = hcl.colors(8, "Lajolla"),
            style = "fixed", breaks = expobreaks,
            title = "NO\U02082 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_facets(nrow = 1, ncol = 3) +
  tm_layout(legend.outside.size = 0.15, panel.label.size = 0.75)

# o3 ----
o3_1 <- read_ncdf("outputs/expo_pred2/2020/o3_2020_01.nc")[,,,24]
o3_2 <- read_ncdf("outputs/expo_pred2/2020/o3_2020_04.nc")[,,,1]
o3_3 <- read_ncdf("outputs/expo_pred2/2020/o3_2020_08.nc")[,,,8]
o3 <- c(o3_1, o3_2, o3_3, along = 3) %>%
  st_set_dimensions(3, values = c("2020-01-24 Dust", "2020-04-01 Lockdown", "2020-08-08 Heatwave"))
st_crs(o3) <- 25831
rm("o3_1", "o3_2", "o3_3")
# expobreaks <- seq(0, 140, 20)
m5 <- tm_shape(o3["se"]) +
  tm_raster(palette = hcl.colors(8, "Lajolla"),
            # style = "fixed", breaks = expobreaks,
            title = "O\U02083 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_facets(nrow = 1, ncol = 3) +
  tm_layout(legend.outside.size = 0.15, panel.label.size = 0.75)

# compose figure ----
composite <- tmap_arrange(m1, m3, m4, m5, ncol = 1)
tmap_save(composite, "figures/manuscript/quantilemaps.png", width = 7, height = 8)
composite2 <- tmap_arrange(m2.1, m2.2, nrow = 2)
tmap_save(composite2, "figures/manuscript/quantilepm25.png", width = 9, height = 5)
