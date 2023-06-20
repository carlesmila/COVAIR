#-----------------------------------------------------------------------------#
#                     Yearly average predictions maps (BCN)                   #
#-----------------------------------------------------------------------------#

library("targets")
library("tidyverse")
library("stars")
library("tmap")

# bcn metro area: 43% of population in 2019 ---
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
bcn <- st_read("data/boundaries/bm5mv21sh0tpm1_20200601_0.shp") %>%
  dplyr::filter(NOMMUNI %in% "Barcelona")

# temp ----
temp_18 <- read_stars("outputs/expo_predaggr2/2018/temp_2018_year_pred.tif")
temp_19 <- read_stars("outputs/expo_predaggr2/2019/temp_2019_year_pred.tif")
temp_20 <- read_stars("outputs/expo_predaggr2/2020/temp_2020_year_pred.tif")
temp <- c(temp_18, temp_19, temp_20, along = 3) %>%
  st_set_dimensions(3, names = "year", values = as.character(2018:2020))
temp <- st_crop(temp, bcn)
expobreaks <- seq(15, 18.3, 0.4)
m1 <- tm_shape(temp) +
  tm_raster(palette = hcl.colors(8, "Blue-Red 3"), 
            style = "fixed", breaks = expobreaks,
            title = "Temp. (ºC)", midpoint = NA) +
  tm_facets(nrow = 1, ncol = 3) +
  tm_layout(legend.outside.size = 0.15, panel.label.size = 0.75)
rm("temp_18", "temp_19", "temp_20", "temp", "expobreaks")

# pm25 ----
pm25_18 <- read_stars("outputs/expo_predaggr2/2018/pm25_2018_year_pred.tif")
pm25_19 <- read_stars("outputs/expo_predaggr2/2019/pm25_2019_year_pred.tif")
pm25_20 <- read_stars("outputs/expo_predaggr2/2020/pm25_2020_year_pred.tif")
pm25 <- c(pm25_18, pm25_19, pm25_20, along = 3) %>%
  st_set_dimensions(3, names = "year", values = as.character(2018:2020))
pm25 <- st_crop(pm25, bcn)
expobreaks <- seq(8, 22, 2)
m2 <- tm_shape(pm25) +
  tm_raster(palette = hcl.colors(7, "Cividis"),
            style = "fixed", breaks = expobreaks,
            title = "PM\U02082.\U02085 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_facets(nrow = 1, ncol = 3) +
  tm_layout(legend.outside.size = 0.15, panel.label.size = 0.75)
rm("pm25_18", "pm25_19", "pm25_20", "pm25", "expobreaks")

# pm10 ----
pm10_18 <- read_stars("outputs/expo_predaggr2/2018/pm10_2018_year_pred.tif")
pm10_19 <- read_stars("outputs/expo_predaggr2/2019/pm10_2019_year_pred.tif")
pm10_20 <- read_stars("outputs/expo_predaggr2/2020/pm10_2020_year_pred.tif")
pm10 <- c(pm10_18, pm10_19, pm10_20, along = 3) %>%
  st_set_dimensions(3, names = "year", values = as.character(2018:2020))
pm10 <- st_crop(pm10, bcn)
expobreaks <- seq(13, 33, 3)
m3 <- tm_shape(pm10) +
  tm_raster(palette = hcl.colors(7, "Cividis"),
            style = "fixed", breaks = expobreaks,
            title = "PM\U02081\U02080 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_facets(nrow = 1, ncol = 3) +
  tm_layout(legend.outside.size = 0.15, panel.label.size = 0.75)
rm("pm10_18", "pm10_19", "pm10_20", "pm10", "expobreaks")

# no2 ----
no2_18 <- read_stars("outputs/expo_predaggr2/2018/no2_2018_year_pred.tif")
no2_19 <- read_stars("outputs/expo_predaggr2/2019/no2_2019_year_pred.tif")
no2_20 <- read_stars("outputs/expo_predaggr2/2020/no2_2020_year_pred.tif")
no2 <- c(no2_18, no2_19, no2_20, along = 3) %>%
  st_set_dimensions(3, names = "year", values = as.character(2018:2020))
no2 <- st_crop(no2, bcn)
expobreaks <- c(seq(5, 55, 5))
m4 <- tm_shape(no2) +
  tm_raster(palette = hcl.colors(9, "Cividis"),
            style = "fixed", breaks = expobreaks,
            title = "NO\U02082 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_facets(nrow = 1, ncol = 3) +
  tm_layout(legend.outside.size = 0.15, panel.label.size = 0.75)
rm("no2_18", "no2_19", "no2_20", "no2", "expobreaks")

# o3 ----
o3_18 <- read_stars("outputs/expo_predaggr2/2018/o3_2018_year_pred.tif")
o3_19 <- read_stars("outputs/expo_predaggr2/2019/o3_2019_year_pred.tif")
o3_20 <- read_stars("outputs/expo_predaggr2/2020/o3_2020_year_pred.tif")
o3 <- c(o3_18, o3_19, o3_20, along = 3) %>%
  st_set_dimensions(3, names = "year", values = as.character(2018:2020))
o3 <- st_crop(o3, bcn)
expobreaks <- seq(50, 95, 5)
m5 <- tm_shape(o3) +
  tm_raster(palette = hcl.colors(8, "Cividis"),
            style = "fixed", breaks = expobreaks,
            title = "O\U02083 (\u03bcg/m\U000b3)", midpoint = NA) +
  tm_facets(nrow = 1, ncol = 3) +
  tm_layout(legend.outside.size = 0.15, panel.label.size = 0.75)
rm("o3_18", "o3_19", "o3_20", "o3", "expobreaks")

# compose figure ----
composite <- tmap_arrange(m1, m2, m3, m4, m5, ncol = 1)
# tmap_save(composite, "figures/manuscript/BCNyearlymaps.png", width = 7, height = 10)
