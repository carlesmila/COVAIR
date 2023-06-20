#-----------------------------------------------------------------------------#
#                         Paths for the data sources                          #
#-----------------------------------------------------------------------------#

# Input data
path_cat <- "data/boundaries/bm5mv21sh0tpp1_20200601_0.shp"
path_apmanual <- "data/stations/estacions_manuals.csv"
path_apauto <- "data/stations/estacions_automatiques.csv"
path_aemetread <- "data/meteo/aemet_readings.csv"
path_aemetstat <- "data/meteo/aemet_stations.csv"
path_xemaread <- "data/meteo/xema_readings.csv"
path_xemastat <- "data/meteo/xema_stations.csv"
path_lulc <- "data/LULC/UsSol_2017_10m_v3.tiff"
path_coast <- "data/copernicusLMS/HYDRO/HYDRO.gpkg"
path_dem <- "data/copernicusLMS/DEM/DEM.tif"
path_imd <- "data/copernicusLMS/IMD/IMD.tif"
path_tcd <- "data/copernicusLMS/TCD/TCD.tif"
path_gridded <- "data/emissions/SPAIN_2021-GRIDDED_Annex_V_2019.xls"
path_point <- "data/emissions/SPAIN_2021-LPS_Annex_VI_2019.xls"
path_popu <- "data/population/rp2016_qtree_level2_ofus_allvar.shp"
path_roads <- "data/roads/roads_cat.gpkg"
paths_ndvi <- list.files("data/NDVI", pattern = "*.tif", full.names=T)
paths_ntli <- list.files("data/NTLI", pattern = "NTLI_.....tif", full.names=T)
paths_era5rean <- list.files("data/ERA5", pattern = "ERA5Rean*", full.names=T)
paths_era5land <- list.files("data/ERA5", pattern = "ERA5Land*", full.names=T)
paths_camsglobal <- list.files("data/CAMS", pattern = "CAMSglobal*", full.names=T)
paths_camsrean <- list.files("data/CAMS/reanalysis", full.names=T)
paths_camsanaly <- list.files("data/CAMS/analysis", full.names=T)
path_calendar <- "data/calendar/Calendari_laboral_de_Catalunya.csv"
path_dust <- "data/dust/saharian_dust.xlsx"
path_maiac <- list.files("data/MAIAC/raw_hdf", full.names = T)
paths_lst_terra <- list.files("data/LST/MOD11A1", ".hdf", recursive=T, full.names=T)
paths_lst_aqua <- list.files("data/LST/MYD11A1", ".hdf", recursive=T, full.names=T)
paths_omi_no2 <- list.files("data/OMI_NO2", ".he5.nc4", recursive = T, full.names = T)
paths_tropomi_no2 <- list.files("data/TROPOMI_NO2/harp", ".nc", recursive = T, full.names = T)
paths_omi_o3 <- list.files("data/OMI_O3", ".he5.SUB.nc4", recursive = T, full.names = T)
paths_tropomi_o3 <- list.files("data/TROPOMI_O3/harp", ".nc", recursive = T, full.names = T)
path_extrtemp <- "database/temp_pred/extracted_temp.rds"
path_extrap <- "database/ap_pred/extracted_ap.rds"