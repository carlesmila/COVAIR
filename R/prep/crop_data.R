#-----------------------------------------------------------------------------#
#                    Crop data to the study area and period                   #
#-----------------------------------------------------------------------------#

library("targets")
library("sf")
library("stars")
library("tidyverse")

# Read cat boundaries, buffer 1km, and project to EPSG 4258
tar_load("cat")
cat <- read_rds(cat)
cat_b1 <- st_buffer(cat, 5000)

#### DEM ----
dem1 <- read_stars("data/copernicusLMS/DEM/eu_dem_v11_E30N10.TIF")
dem2 <- read_stars("data/copernicusLMS/DEM/eu_dem_v11_E30N20.TIF")
dem <- st_mosaic(dem1, dem2)
dem <- st_crop(dem, st_bbox(st_transform(cat_b1, st_crs(dem))))
write_stars(dem, "data/copernicusLMS/DEM/DEM.tif")

#### Imperviousness Density (IMD) ----
imd <- read_stars("data/copernicusLMS/IMD/DATA/IMD_2018_100m_eu_03035_V2_0.tif")
imd <- st_crop(imd, st_bbox(st_transform(cat_b1, st_crs(imd))))
write_stars(imd, "data/copernicusLMS/IMD/IMD.tif")

#### Surface Built-Up (SBU) ----
sbu <- read_stars("data/copernicusLMS/SBU/DATA/SBU_2018_100m_eu_03035_V1_0.tif")
sbu <- st_crop(sbu, st_bbox(st_transform(cat_b1, st_crs(sbu))))
write_stars(sbu, "data/copernicusLMS/SBU/SBU.tif")

#### Tree Cover Density (TCD) ----
tcd <- read_stars("data/copernicusLMS/TCD/DATA/TCD_2018_100m_eu_03035_V2_0.tif")
tcd <- st_crop(tcd, st_bbox(st_transform(cat_b1, st_crs(tcd))))
write_stars(tcd, "data/copernicusLMS/TCD/TCD.tif")

#### HYDRO ----
hydro <- st_read("data/copernicusLMS/HYDRO/Shapefile/EUHYDRO_Coastline_EEA39_v013.shp")
hydro <- st_crop(hydro, st_bbox(st_transform(cat_b1, st_crs(hydro))))
st_write(hydro, "data/copernicusLMS/HYDRO/HYDRO.gpkg", "coastline")

#### Roads ----
query <- "SELECT fclass FROM gis_osm_roads_free_1"
roads <- st_read("data/roads/gis_osm_roads_free_1.shp", query = query)
no_road <- c("bridleway", "cycleway", "footway", "path", "pedestrian", "steps", 
             "track", "track_grade1", "track_grade2", "track_grade3",
             "track_grade4", "track_grade5", "unknown")
roads <- dplyr::filter(roads, !fclass %in% no_road)
st_write(roads, "data/roads/roads_spain.gpkg")
query2 <- "SELECT * FROM roads_spain AS r WHERE ST_INTERSECTS(r.geom, GeomFromText('POLYGON((0.14 40.51,0.14 42.88,3.34 42.88,3.34 40.51,0.14 40.51))', 4326))"
roads_cat <- st_read("data/roads/roads_spain.gpkg", query = query2)
st_write(roads_cat, "data/roads/roads_cat.gpkg")
file.remove("data/roads/roads_spain.gpkg")

#### NTLI ----
ntli_2018 <- read_stars("data/NTLI/VNL_v2_npp_2018_global_vcmslcfg_c202101211500.median.tif")
ntli_2018 <- st_crop(ntli_2018, st_transform(cat_b1, crs=st_crs(ntli_2018)))
write_stars(ntli_2018, "data/NTLI/NTLI_2018.tif")

ntli_2019 <- read_stars("data/NTLI/VNL_v2_npp_2019_global_vcmslcfg_c202101211500.median.tif")
ntli_2019 <- st_crop(ntli_2019, st_transform(cat_b1, crs=st_crs(ntli_2019)))
write_stars(ntli_2019, "data/NTLI/NTLI_2019.tif")

ntli_2020 <- read_stars("data/NTLI/VNL_v2_npp_2020_global_vcmslcfg_c202101211500.median.tif")
ntli_2020 <- st_crop(ntli_2020, st_transform(cat_b1, crs=st_crs(ntli_2020)))
write_stars(ntli_2020, "data/NTLI/NTLI_2020.tif")


#### XEMA ----
library(inborutils)
library("DBI")

# Convert large csv into an sqlite to allow for queries
inborutils::csv_to_sqlite("data/meteo/Dades_meteorol_giques_de_la_XEMA.csv", 
                          "data/meteo/xema.sqlite", "XEMA")
# Connect and query
mydb <- dbConnect(RSQLite::SQLite(), "data/meteo/xema.sqlite")
dbListTables(mydb)
meteo <- dbGetQuery(mydb, 'SELECT codi_estacio, codi_variable, data_lectura, valor_lectura FROM xema WHERE codi_estat = "V" AND codi_variable IN (32, 40, 42)')
meteo$year <- substr(meteo$DATA_LECTURA, 7, 10)
meteo <- meteo[meteo$year %in% c("2018", "2019", "2020"),]
meteo$year <- NULL
write_csv(meteo, "data/meteo/xema_readings.csv")

#### NDVI ----
ndvi_files <- list.files("data/NDVI_modis/NDVI", full.names = T, recursive = T)
map(ndvi_files, function(x){
  orig <- read_stars(x)
  cropped <- st_crop(orig, st_bbox(st_transform(cat_b1, st_crs(orig))))
  write_stars(cropped, gsub("/NDVI/", "/cropped/", x))
})
