#-----------------------------------------------------------------------------#
#                       Targets file for the project                          #
#-----------------------------------------------------------------------------#

library("targets")
library("tarchetypes")

# Source pipeline functions for preprocessing
source("R/prep/prep_boundaries_grids.R")
source("R/prep/prep_stations.R")
source("R/prep/prep_spatial.R") 
source("R/prep/prep_temporal.R") 
source("R/prep/prep_spatiotemporal.R") 
source("R/prep/prep_maiac.R") 
source("R/prep/prep_omi.R") 
source("R/prep/prep_tropomi.R")
source("R/prep/prep_lst.R")

# Source pipeline functions for RS imputation
source("R/RSimp/imp_resample_split.R")
source("R/RSimp/imp_modis.R")
source("R/RSimp/imp_tropomi.R")
source("R/RSimp/imp_omi.R")

# Source pipeline functions for prediction
source("R/prediction/focal_mean.R")
source("R/prediction/preddata_temp.R")
source("R/prediction/preddata_ap.R")

# Datasets paths
source("R/prep/paths.R")

# Packages required to run the workflow
tar_option_set(packages = c("tidyverse", "sf", "conflicted", "stars", "raster",
                            "readxl", "nngeo", "readxl", "furrr", "lubridate",
                            "starsExtra", "gdalUtils", "rgdal", "ncdf4", "padr",
                            "cowplot", "exactextractr", "renv", "gstat"))
# Global options
options(dplyr.summarise.inform = FALSE)

# Pipeline
list(
  
  # 1: Data pre-processing
  list(
    
    # 1.1 Prepare boundaries and grids
    list(tar_target(cat, prep_cat(path_cat)),
         tar_target(gridr, prep_gridr(cat)),
         tar_target(gridp, prep_gridp(gridr, cat))
    ), 
    
    # 1.2 Prepare station data and report
    list(
      tar_target(ap_stations, prep_ap_stations(path_apmanual, path_apauto)),
      tar_target(meteo_stations, prep_meteo_stations(path_aemetread, path_aemetstat,
                                                     path_xemaread, path_xemastat)),
      tar_render(station_report, "reports/prep/station_report.Rmd")
    ),
    
    # 1.3 Prepare spatial data
    list(tar_target(coords, prep_coords(gridr, cat)),
         tar_target(terrain, prep_terrain(path_dem, gridr, cat)),
         tar_target(imd, prep_copernicusLMS(path_imd, gridr, "imd", cat)),
         tar_target(tcd, prep_copernicusLMS(path_tcd, gridr, "tcd", cat)),
         tar_target(coast, prep_coast(path_coast, gridp, gridr, cat)),
         tar_target(lulc, prep_lulc(path_lulc, gridr)),
         tar_target(psources, prep_psources(path_point, gridp, gridr, cat)),
         tar_target(popu, prep_popu(path_popu, gridr, cat)),
         tar_target(roaddens, prep_roaddens(path_roads, gridp, gridr, cat)),
         tar_target(spstack, stack_spatial(
           list(coords, terrain, imd, tcd, coast, lulc, psources, popu, roaddens)
         ))
    ),
    
    # 1.4 Prep temporal predictors
    list(
      tar_target(calendar, prep_calendar(path_calendar, path_dust))
    ),
    
    # 1.5 Prepare spatio-temporal predictors: 
    list(
      tar_target(ndvi, prep_ndvi(paths_ndvi, gridr, cat)),
      tar_target(ntli, prep_ntli(paths_ntli, cat)),
      tar_target(era5rean, prep_era5rean(paths_era5rean, cat)), 
      tar_target(era5land, prep_era5land(paths_era5land, cat)),
      tar_target(camsglobal, prep_camsglobal(paths_camsglobal, cat)),
      tar_target(camsrean, prep_camsrean(paths_camsrean, cat)),
      tar_target(camsanaly, prep_camsanaly(paths_camsanaly, cat))
    ),
    
    # 1.6 Prep AOD/NO2/O3/LST remote sensing predictors + report
    list(
      tar_target(maiac, prep_maiac(path_maiac, cat)),
      tar_target(tropomi_no2, prep_tropomi_no2(paths_tropomi_no2, cat)),
      tar_target(omi_no2, prep_omi_no2(paths_omi_no2, cat)),
      tar_target(tropomi_o3, prep_tropomi_o3(paths_tropomi_o3, cat)),
      tar_target(omi_o3, prep_omi_o3(paths_omi_o3, cat, omi_no2)),
      tar_target(lst_terra, prep_lst(paths_lst_terra, cat, maiac, "terra")),
      tar_target(lst_aqua, prep_lst(paths_lst_aqua, cat, maiac, "aqua")),
      tar_render(RS_report, "reports/prep/RS_report.Rmd")
    ),
    
    # 1.7 Predictor report
    list(tar_render(predictor_report, "reports/prep/predictors_report.Rmd"))
  ),
  
  # 2. Remote sensing imputation.
  list(
    
    # 2.1 Test/train splits
    list(tar_target(rs_splits, RSimp_splits(cat))),
    
    # 2.2 MODIS MAIAC and LST: resample, merge, split
    list(
      tar_target(modis_preds, resample_modis(maiac, spstack, path_dem, ndvi, ntli, 
                                             era5rean, era5land, camsglobal, cat)),
      tar_target(lst_train, impdata_lst(lst_terra, lst_aqua, modis_preds, calendar, rs_splits)),
      tar_target(maiac_train, impdata_maiac(maiac, modis_preds, calendar, rs_splits)),
      tar_render(lst_report, "reports/RSimp/lst_exploratory.Rmd"),
      tar_render(maiac_report, "reports/RSimp/maiac_exploratory.Rmd")
    ),
    
    # 2.3 TROPOMI: resample, merge, split
    list(
      tar_target(tropomi_preds,
                 resample_tropomi(tropomi_no2, spstack, ndvi, ntli, 
                                  era5rean, era5land, camsglobal, cat)),
      tar_target(tropomi_train, impdata_tropomi(tropomi_no2, tropomi_o3, 
                                                tropomi_preds, calendar, rs_splits)),
      tar_render(tropomi_report, "reports/RSimp/tropomi_exploratory.Rmd")
    ),
    
    # 2.4 OMI: resample, merge, split
    list(
      tar_target(omi_preds,
                 resample_omi(omi_no2, spstack, ndvi, ntli, 
                              era5rean, era5land, camsglobal, cat)),
      tar_target(omi_train, impdata_omi(omi_no2, omi_o3, omi_preds, calendar, rs_splits)),
      tar_render(omi_report, "reports/RSimp/omi_exploratory.Rmd")
    ),
    
    # 2.5 Fit and validate RS imputation models - done in HPC
    
    # 2.6 Imputation reports
    list(
      tar_render(lstimp_report, "reports/RSimp/lst_imputed.Rmd"),
      tar_render(maiacimp_report, "reports/RSimp/maiac_imputed.Rmd"),
      tar_render(omiimp_report, "reports/RSimp/omi_imputed.Rmd"),
      tar_render(tropomiimp_report, "reports/RSimp/tropomi_imputed.Rmd")
    )
  ),
  
  # 3. Prediction
  list(
    
    # 3.1 Focal predictors 
    list(tar_target(focal_preds, focal_mean(spstack, ndvi, cat))),
    
    # 3.2 Resample coarse predictors to target resolution 
    # NTLI, ERA5, CAMS, LST, MAIAC, TROPOMI and OMI - done in HPC
    
    # 3.3 Extract spatiotemporal predictor data at station locations:
    # For temperature and air pollution - done in HPC
    
    # 3.4 Average temperature: prepare data, explore, fit (HPC), predict (HPC)
    list(tar_target(temp_moddata, preptemp_moddata(meteo_stations, spstack, focal_preds,
                                                   calendar, ndvi, path_extrtemp)),
         tar_render(temp_exploratory, "reports/prediction/temp_exploratory.Rmd"),
         tar_render(temp_modreport, "reports/prediction/temp_mod.Rmd")),
    
    # 3.5 AP exposures: prepare data, explore, fit (HPC), predict (HPC)
    list(tar_target(ap_moddata, prepap_moddata(ap_stations, spstack, focal_preds,
                                               calendar, ndvi, path_extrap)),
         tar_render(ap_exploratory, "reports/prediction/ap_exploratory.Rmd"),
         tar_render(pm10_modreport, "reports/prediction/pm10_mod.Rmd"),
         tar_render(pm25_modreport, "reports/prediction/pm25_mod.Rmd"),
         tar_render(no2_modreport, "reports/prediction/no2_mod.Rmd"),
         tar_render(o3_modreport, "reports/prediction/o3_mod.Rmd"))
    
  )
)
