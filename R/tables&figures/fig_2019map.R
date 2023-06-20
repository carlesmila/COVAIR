#-----------------------------------------------------------------------------#
#                 Yearly average predictions maps Catalonia + BCN             #
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

# temp ----
temp_cat <- read_stars("outputs/expo_predaggr2/2019/temp_2019_year_pred.tif")
temp_metro <- st_crop(temp_cat, bcnmetro)
expobreaks <- c(-1, seq(3, 19, 2))
m1.1 <- tm_shape(temp_cat) +
  tm_raster(palette = hcl.colors(length(expobreaks)-1, "Blue-Red 3"), 
            style = "fixed", breaks = expobreaks,
            title = "Temp. (ºC)", midpoint = NA)  +
  tm_shape(bcnmetro) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(legend.outside = T, main.title = "A")
expobreaks <- seq(15, 18, 0.3)
m1.2 <- tm_shape(temp_metro) +
  tm_raster(palette = hcl.colors(length(expobreaks)-1, "Blue-Red 3"), 
            style = "fixed", breaks = expobreaks,
            title = "Temp. (ºC)", midpoint = NA)  +
  tm_shape(bcn) +
  tm_borders(col = "black", lwd = 1) +
  tm_layout(legend.outside = T, main.title = "B")
rm("temp_cat", "temp_metro", "expobreaks")

# pm25 ----
pm25_cat <- read_stars("outputs/expo_predaggr2/2019/pm25_2019_year_pred.tif")
pm25_metro <- st_crop(pm25_cat, bcnmetro)
expobreaks <- seq(4, 21, 2)
m2.1 <- tm_shape(pm25_cat) +
  tm_raster(palette = hcl.colors(length(expobreaks)-1, "Cividis"),
            style = "fixed", breaks = expobreaks,
            title = "PM\U02082.\U02085 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_shape(bcnmetro) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(legend.outside = T)
m2.2 <- tm_shape(pm25_metro) +
  tm_raster(palette = hcl.colors(length(expobreaks)-1, "Cividis"),
            style = "fixed", breaks = expobreaks,
            title = "PM\U02082.\U02085 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_shape(bcn) +
  tm_borders(col = "black", lwd = 1) +
  tm_layout(legend.outside = T)
rm("pm25_cat", "pm25_metro", "expobreaks")

# pm10 ----
pm10_cat <- read_stars("outputs/expo_predaggr2/2019/pm10_2019_year_pred.tif")
pm10_metro <- st_crop(pm10_cat, bcnmetro)
expobreaks <- seq(8, 32, 3)
m3.1 <- tm_shape(pm10_cat) +
  tm_raster(palette = hcl.colors(length(expobreaks)-1, "Cividis"),
            style = "fixed", breaks = expobreaks,
            title = "PM\U02081\U02080 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_shape(bcnmetro) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(legend.outside = T)
m3.2 <- tm_shape(pm10_metro) +
  tm_raster(palette = hcl.colors(length(expobreaks)-1, "Cividis"),
            style = "fixed", breaks = expobreaks,
            title = "PM\U02081\U02080 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_shape(bcn) +
  tm_borders(col = "black", lwd = 1) +
  tm_layout(legend.outside = T)
rm("pm10_cat", "pm10_metro", "expobreaks")

# no2 ----
no2_cat <- read_stars("outputs/expo_predaggr2/2019/no2_2019_year_pred.tif")
no2_metro <- st_crop(no2_cat, bcnmetro)
expobreaks <- c(0, 3, 6, 9, 12, seq(15, 40, 5), 55)
m4.1 <- tm_shape(no2_cat) +
  tm_raster(palette = hcl.colors(length(expobreaks)-1, "Cividis"),
            style = "fixed", breaks = expobreaks,
            title = "NO\U02082 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_shape(bcnmetro) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(legend.outside = T)
m4.2 <- tm_shape(no2_metro) +
  tm_raster(palette = hcl.colors(length(expobreaks)-1, "Cividis"),
            style = "fixed", breaks = expobreaks,
            title = "NO\U02082 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_shape(bcn) +
  tm_borders(col = "black", lwd = 1) +
  tm_layout(legend.outside = T)
rm("no2_cat", "no2_metro", "expobreaks")

# o3 ----
o3_cat <- read_stars("outputs/expo_predaggr2/2019/o3_2019_year_pred.tif")
o3_metro <- st_crop(o3_cat, bcnmetro)
expobreaks <- seq(50, 104, 6)
m5.1 <- tm_shape(o3_cat) +
  tm_raster(palette = hcl.colors(length(expobreaks)-1, "Cividis"),
            style = "fixed", breaks = expobreaks,
            title = "O\U02083 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_shape(bcnmetro) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(legend.outside = T)
m5.2 <- tm_shape(o3_metro) +
  tm_raster(palette = hcl.colors(length(expobreaks)-1, "Cividis"),
            style = "fixed", breaks = expobreaks,
            title = "O\U02083 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_shape(bcn) +
  tm_borders(col = "black", lwd = 1) +
  tm_layout(legend.outside = T)
rm("o3_cat", "o3_metro", "expobreaks")

# Combine ---- 
composite <- tmap_arrange(m1.1, m1.2, m2.1, m2.2, 
                          m3.1, m3.2, m4.1, m4.2, m5.1, m5.2,
                          ncol = 2, nrow = 5, heights = c(0.24, rep(0.19, 4)))
tmap_save(composite, "figures/manuscript/yearlymap2019.png", width = 9, height = 10)
rm(list = ls())