#-----------------------------------------------------------------------------#
#             HPC: Cluster resampling ST predictors to target grid            #
#-----------------------------------------------------------------------------#

#### Prep ----
rm(list = ls())
library("tidyverse")
library("stars")
path_in <- "****"
path_out <- "****"
spstack <- paste0(path_in, "CM_exposure/database/original/spatial/spstack.rds")
spstack <- read_rds(spstack)

#### Utils ----
resample_ST <- function(raster_in, spstack, pathout){
  
  if(!file.exists(pathout)){
    
    # Try to simplify time dimension and warp, fix names and dates
    raster_in <- adrop(raster_in)
    raster_proxy <- st_warp(raster_in, spstack)
    raster_proxy <- st_warp(raster_in, raster_proxy, method="bilinear",
                            use_gdal=T, no_data_value = -9999, debug = T)
    names(raster_proxy) <- names(raster_in)
    
    # Fix dates
    if(length(dim(raster_in))==3){
      dates_vector <- st_get_dimension_values(raster_in, "date")
      raster_proxy <- st_set_dimensions(raster_proxy, 3, names = "date", values = dates_vector)
    }
    
    # Unproxy, print and store object to disk
    raster_res <- st_as_stars(raster_proxy)
    if(grepl(".rds", pathout)){
      write_rds(raster_res, pathout)
    }else if(grepl(".tif", pathout)){
      write_stars(raster_res, pathout)
    }
    
    # Clean tmp files
    tmpfiles <- paste0(normalizePath(tempdir()), "/", dir(tempdir(), pattern = ".tif"))
    print(paste0("Cleaning files: ", tmpfiles))
    unlink(tmpfiles, recursive = TRUE)
  }
}


#### NTLI ----
print("Resampling NTLI...")
path_ntli <- paste0(path_in, "CM_exposure/database/original/t/ntli.rds")
for(t in 2018:2020){
  print(paste0("Year: ", t))
  path_it <- gsub("/t/", paste0("/", t, "/"), path_ntli)
  raster_it <- read_rds(path_it)
  resample_ST(raster_it, spstack, paste0(path_out, t, "/ntli.rds"))
  rm("path_it", "raster_it")
}
rm("path_ntli")


#### ERA5land ----
print("Resampling ERA5land...")
path_era5land <- paste0(path_in, "CM_exposure/database/original/t/era5land.rds")
for(t in 2018:2020){
  print(paste0("Year: ", t))
  path_it <- gsub("/t/", paste0("/", t, "/"), path_era5land)
  raster_it <- read_rds(path_it)
  resample_ST(raster_it["ERA5Land_2mtemp"], spstack, paste0(path_out, t, "/ERA5_2mtemp.tif"))
  resample_ST(raster_it["ERA5Land_10muwind"], spstack, paste0(path_out, t, "/ERA5_10muwind.tif"))
  resample_ST(raster_it["ERA5Land_10mvwind"], spstack, paste0(path_out, t, "/ERA5_10mvwind.tif"))
  resample_ST(raster_it["ERA5Land_totlprec"], spstack, paste0(path_out, t, "/ERA5_totlprec.tif"))
  rm("path_it", "raster_it")
}
rm("path_era5land")


#### ERA5rean ----
print("Resampling ERA5rean...")
path_era5rean <- paste0(path_in, "CM_exposure/database/original/t/era5rean.rds")
for(t in 2018:2020){
  print(paste0("Year: ", t))
  path_it <- gsub("/t/", paste0("/", t, "/"), path_era5rean)
  raster_it <- read_rds(path_it)
  resample_ST(raster_it["ERA5Rean_pbh"], spstack, paste0(path_out, t, "/ERA5_pbh.tif"))
  rm("path_it", "raster_it")
}
rm("path_era5rean")

#### CAMSrean ----
print("Resampling CAMSrean...")
path_camsrean <- paste0(path_in, "CM_exposure/database/original/t/camsrean.rds")
for(t in 2018){
  print(paste0("Year: ", t))
  path_it <- gsub("/t/", paste0("/", t, "/"), path_camsrean)
  raster_it <- read_rds(path_it)
  resample_ST(raster_it["CAMSrean_no2"], spstack, paste0(path_out, t, "/CAMSrean_no2.tif"))
  resample_ST(raster_it["CAMSrean_pm10"], spstack, paste0(path_out, t, "/CAMSrean_pm10.tif"))
  resample_ST(raster_it["CAMSrean_pm25"], spstack, paste0(path_out, t, "/CAMSrean_pm25.tif"))
  resample_ST(raster_it["CAMSrean_o3"], spstack, paste0(path_out, t, "/CAMSrean_o3.tif"))
  rm("path_it", "raster_it")
}
rm("path_camsrean")

#### CAMSanaly ----
print("Resampling CAMSanaly")
path_camsanaly <- paste0(path_in, "CM_exposure/database/original/t/camsanaly.rds")
for(t in 2019:2020){
  print(paste0("Year: ", t))
  path_it <- gsub("/t/", paste0("/", t, "/"), path_camsanaly)
  raster_it <- read_rds(path_it)
  resample_ST(raster_it["CAMSanaly_no2"], spstack, paste0(path_out, t, "/CAMSanaly_no2.tif"))
  resample_ST(raster_it["CAMSanaly_pm10"], spstack, paste0(path_out, t, "/CAMSanaly_pm10.tif"))
  resample_ST(raster_it["CAMSanaly_pm25"], spstack, paste0(path_out, t, "/CAMSanaly_pm25.tif"))
  resample_ST(raster_it["CAMSanaly_o3"], spstack, paste0(path_out, t, "/CAMSanaly_o3.tif"))
  rm("path_it", "raster_it")
}
rm("path_camsanaly")

#### LST ----
print("Resampling LST...")
path_terraday <- paste0(path_in, "HPC_outputs/LST_imp/implst_terraday_t.rds")
path_terranight <- paste0(path_in, "HPC_outputs/LST_imp/implst_terranight_t.rds")
path_aquaday <- paste0(path_in, "HPC_outputs/LST_imp/implst_aquaday_t.rds")
path_aquanight <- paste0(path_in, "HPC_outputs/LST_imp/implst_aquanight_t.rds")
for(t in 2018:2020){
  print(paste0("Year: ", t))
  path_terraday_it <- gsub("_t.", paste0("_", t, "."), path_terraday, fixed=T)
  path_terranight_it <- gsub("_t.", paste0("_", t, "."), path_terranight, fixed=T)
  path_aquaday_it <- gsub("_t.", paste0("_", t, "."), path_aquaday, fixed=T)
  path_aquanight_it <- gsub("_t.", paste0("_", t, "."), path_aquanight, fixed=T)
  resample_ST(read_rds(path_terraday_it), spstack, paste0(path_out, t, "/lst_terraday.tif"))
  resample_ST(read_rds(path_terranight_it), spstack, paste0(path_out, t, "/lst_terranight.tif"))
  resample_ST(read_rds(path_aquaday_it), spstack, paste0(path_out, t, "/lst_aquaday.tif"))
  resample_ST(read_rds(path_aquanight_it), spstack, paste0(path_out, t, "/lst_aquanight.tif"))
  rm("path_terraday_it", "path_terranight_it", "path_aquaday_it", "path_aquanight_it")
}
rm("path_terraday", "path_terranight", "path_aquaday", "path_aquanight")


#### MAIAC ----
print("Resampling MAIAC")
path_maiacterra <- paste0(path_in, "HPC_outputs/MAIAC_imp/impmaiac_terra_t.rds")
path_maiacaqua <- paste0(path_in, "HPC_outputs/MAIAC_imp/impmaiac_aqua_t.rds")
for(t in 2018:2020){
  print(paste0("Year: ", t))
  path_maiacterra_it <- gsub("_t.", paste0("_", t, "."), path_maiacterra, fixed=T)
  path_maiacaqua_it <- gsub("_t.", paste0("_", t, "."), path_maiacaqua, fixed=T)
  resample_ST(read_rds(path_maiacterra_it), spstack, paste0(path_out, t, "/maiac_terra.tif"))
  resample_ST(read_rds(path_maiacaqua_it), spstack, paste0(path_out, t, "/maiac_aqua.tif"))
  rm("path_maiacterra_it", "path_maiacaqua_it")
}
rm("path_maiacterra", "path_maiacaqua")


#### TROPOMI ----
print("Resampling TROPOMI...")
path_tropono2 <- paste0(path_in, "HPC_outputs/tropomi_imp/imptropomi_no2_t.rds")
path_tropoo3 <- paste0(path_in, "HPC_outputs/tropomi_imp/imptropomi_o3_t.rds")
for(t in 2018:2020){
  print(paste0("Year: ", t))
  path_tropono2_it <- gsub("_t.", paste0("_", t, "."), path_tropono2, fixed=T)
  path_tropoo3_it <- gsub("_t.", paste0("_", t, "."), path_tropoo3, fixed=T)
  resample_ST(read_rds(path_tropono2_it), spstack, paste0(path_out, t, "/tropomi_no2.tif"))
  resample_ST(read_rds(path_tropoo3_it), spstack, paste0(path_out, t, "/tropomi_o3.tif"))
  rm("path_tropono2_it", "path_tropoo3_it")
}
rm("path_tropono2", "path_tropoo3")


#### OMI ----
print("Resampling OMI...")
path_omino2 <- paste0(path_in, "HPC_outputs/omi_imp/impomi_no2_t.rds")
path_omio3 <- paste0(path_in, "HPC_outputs/omi_imp/impomi_o3_t.rds")
for(t in 2018:2020){
  print(paste0("Year: ", t))
  path_omino2_it <- gsub("_t.", paste0("_", t, "."), path_omino2, fixed=T)
  path_omio3_it <- gsub("_t.", paste0("_", t, "."), path_omio3, fixed=T)
  resample_ST(read_rds(path_omino2_it), spstack, paste0(path_out, t, "/omi_no2.tif"))
  resample_ST(read_rds(path_omio3_it), spstack, paste0(path_out, t, "/omi_o3.tif"))
  rm("path_omino2_it", "path_omio3_it")
}
rm("path_omino2", "path_omio3")


#### Finish ----
rm(list = ls())