#-----------------------------------------------------------------------------#
#                      RS imputation: MODIS terra and aqua                    #
#-----------------------------------------------------------------------------#

# Function to resample predictors for MODIS imputation models to the products' geometries
resample_modis <- function(maiac, spstack, path_dem, ndvi, ntli, era5rean, era5land, camsglobal, cat){
  
  # Read inputs
  cat <- read_rds(cat)
  maiac <- adrop(read_rds(maiac$maiac18)[1,,,1])
  
  # Select and resample spatial predictors
  spstack <- read_rds(spstack)
  spstack <- dplyr::select(spstack, c("dem", "slope", "imd", "tcd", "coast_dist", "water",
                                      "agriculture", "urban", "industry", "popu_dens",
                                      "primary_dens", "secondary_dens", "local_dens"))
  resample_preds(spstack, maiac, "'average'", cat, "database/modis_imp/spatial/spstack.rds")
  dem <- suppressWarnings(as(read_rds("database/modis_imp/spatial/spstack.rds")["dem",], "Raster"))
  dem[is.na(dem)] <- 0
  aspect <- terrain(dem, "aspect", unit="degrees")
  aspect <- st_as_stars(aspect) %>%
    st_set_dimensions(1, point=F) %>%
    st_set_dimensions(2, point=F)
  write_rds(aspect, "database/modis_imp/spatial/aspect.rds")
  rm("spstack", "aspect")
  
  # Spatio temporal predictors
  for(i in 1:3){
    t <- as.character(2018+(i-1))
    # Resample 
    resample_preds(read_rds(ndvi[[i]]), maiac, "'average'", cat, paste0("database/modis_imp/", t, "/ndvi.rds"))
    resample_preds(adrop(read_rds(ntli[[i]])), maiac, "'average'", cat, paste0("database/modis_imp/", t, "/ntli.rds"))
    resample_preds(read_rds(era5rean[[i]])["ERA5Rean_mslvlp_10"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/mslvlp_10.rds"))
    resample_preds(read_rds(era5rean[[i]])["ERA5Rean_mslvlp_13"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/mslvlp_13.rds"))
    resample_preds(read_rds(era5rean[[i]])["ERA5Rean_pbh_10"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/pbh_10.rds"))
    resample_preds(read_rds(era5rean[[i]])["ERA5Rean_pbh_13"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/pbh_13.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_10muwind_10"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/10muwind_10.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_10mvwind_10"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/10mvwind_10.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_2mtemp_10"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/2mtemp_10.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_skintemp_10"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/skintemp_10.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_totlprec_10"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/totlprec_10.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_10muwind_13"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/10muwind_13.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_10mvwind_13"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/10mvwind_13.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_2mtemp_13"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/2mtemp_13.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_skintemp_13"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/skintemp_13.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_totlprec_13"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/totlprec_13.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_10muwind_22"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/10muwind_22.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_10mvwind_22"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/10mvwind_22.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_skintemp_22"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/skintemp_22.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_totlprec_22"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/totlprec_22.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_10muwind_01"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/10muwind_01.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_10mvwind_01"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/10mvwind_01.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_skintemp_01"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/skintemp_01.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_totlprec_01"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/totlprec_01.rds"))
    resample_preds(read_rds(camsglobal[[i]])["CAMSglobal_AOD550_09"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/AOD550_09.rds"))
    resample_preds(read_rds(camsglobal[[i]])["CAMSglobal_AOD550_12"], maiac, "'bilinear'", cat, paste0("database/modis_imp/", t, "/AOD550_12.rds"))
  }

  # Return a list with the paths
  list("spstack"= "database/modis_imp/spatial/spstack.rds",
       "aspect"= "database/modis_imp/spatial/aspect.rds",
       "ndvi" = "database/modis_imp/year/ndvi.rds",
       "ntli" = "database/modis_imp/year/ntli.rds",
       "mslvlp" = "database/modis_imp/year/mslvlp_hour.rds",
       "pbh" = "database/modis_imp/year/pbh_hour.rds",
       "2mtemp" = "database/modis_imp/year/2mtemp_hour.rds",
       "10muwind" = "database/modis_imp/year/10muwind_hour.rds",
       "10mvwind" = "database/modis_imp/year/10mvwind_hour.rds",
       "totlprec" = "database/modis_imp/year/totlprec_hour.rds",
       "skintemp" = "database/modis_imp/year/skintemp_hour.rds",
       "AOD550" = "database/modis_imp/year/AOD550_hour.rds")
}

# Function to prepare the dataset for imputation model fitting of LST products
impdata_lst <- function(lst_terra, lst_aqua, modis_preds, calendar, rs_splits){
  
  # Read split data
  split_areas <- read_rds(rs_splits$split_areas)
  split_indicators <- read_rds(rs_splits$split_indicators)
  
  # Read spatial and temporal predictors
  aspect <- read_rds(modis_preds$aspect)
  spstack <- read_rds(modis_preds$spstack) %>%
    dplyr::select(dem, slope, imd, tcd, coast_dist, water)
  spstack <- c(spstack, aspect)
  calendar <- read_rds(calendar) %>%
    dplyr::select(date, yday, holiday, dust)
  
  # spstack to data frame
  spstack <- as.data.frame(spstack)
  spstack <- spstack[complete.cases(spstack),]
  
  # Join gapIDs
  gapID <- spstack[c("x", "y")]
  gapID <- gapID[!duplicated(gapID),]
  gapID <- st_as_sf(gapID, coords = c("x", "y"), crs = st_crs(split_areas))
  gapID <- st_join(gapID, split_areas) %>%
    mutate(x = st_coordinates(.)[,1],
           y = st_coordinates(.)[,2])
  st_geometry(gapID) <- NULL
  spstack <- inner_join(spstack, gapID, by=c("x", "y"))
  spstack <- spstack[complete.cases(spstack),]
  
  #### LST day Terra ----
  for(i in 1:3){
    # Time indicator
    t <- as.character(2018+(i-1))
    
    if(!file.exists(paste0("database/modis_imp/", t, "/dayLST_terra.csv"))){
      # Read LST terra
      data_it <- read_rds(lst_terra[[i]])["dayLST_terra"]
      
      # Read daily st predictors
      mpreds_it <- c(modis_preds$skintemp, modis_preds$`10muwind`,
                     modis_preds$`10mvwind`, modis_preds$totlprec)
      mpreds_it <- gsub("year", t, mpreds_it)
      mpreds_it <- gsub("hour", "10", mpreds_it)
      txt_parse <- paste0("c(data_it, ", paste0("read_rds('", mpreds_it, "')", collapse=", "),")")
      data_it <- eval(parse(text=txt_parse))
      data_it <- as.data.frame(data_it)
      data_it <- data_it[complete.cases(data_it),]
      
      # Merge with calendar and spatial predictors
      data_it <- inner_join(data_it, calendar, by="date") %>%
        dplyr::select(-holiday, - dust)
      data_it <- inner_join(data_it, spstack, by=c("x", "y")) 
      
      # Merge with NTLI
      ntli_it <- gsub("year", t, modis_preds$ntli)
      ntli_it <- as.data.frame(read_rds(ntli_it))
      ntli_it <- ntli_it[complete.cases(ntli_it),]
      data_it <- inner_join(data_it, ntli_it, by=c("x", "y"))
      rm("ntli_it")
      
      # Merge with NDVI
      ndvi_it <- gsub("year", t, modis_preds$ndvi)
      ndvi_it <- as.data.frame(read_rds(ndvi_it))
      ndvi_it <- ndvi_it[complete.cases(ndvi_it),]
      data_it$link_date <- floor_date(data_it$date, "quarter")
      data_it <- inner_join(data_it, ndvi_it, by=c("x"="x", "y"="y", "link_date"="date"))
      data_it$link_date <- NULL
      rm("ndvi_it")
      
      # Merge with train/test indicator
      split_it <- dplyr::filter(split_indicators, as.character(year(date))==t)
      data_it <- left_join(data_it, split_it, by=c("gapID", "date"))
      data_it$split <- ifelse(is.na(data_it$split), "train", data_it$split)
      
      # Store dataset
      data_it$date <- NULL
      data_it$gapID <- NULL
      write_csv(data_it, paste0("database/modis_imp/", t, "/dayLST_terra.csv"))
      rm("data_it")
    }
  }
  
  #### LST night Terra ----
  for(i in 1:3){
    # Time indicator
    t <- as.character(2018+(i-1))
    
    if(!file.exists(paste0("database/modis_imp/", t, "/nightLST_terra.csv"))){
      # Read LST terra
      data_it <- read_rds(lst_terra[[i]])["nightLST_terra"]
      
      # Read daily st predictors
      mpreds_it <- c(modis_preds$skintemp, modis_preds$`10muwind`,
                     modis_preds$`10mvwind`, modis_preds$totlprec)
      mpreds_it <- gsub("year", t, mpreds_it)
      mpreds_it <- gsub("hour", "22", mpreds_it)
      txt_parse <- paste0("c(data_it, ", paste0("read_rds('", mpreds_it, "')", collapse=", "),")")
      data_it <- eval(parse(text=txt_parse))
      data_it <- as.data.frame(data_it)
      data_it <- data_it[complete.cases(data_it),]
      
      # Merge with calendar and spatial predictors
      data_it <- inner_join(data_it, calendar, by="date") %>%
        dplyr::select(-holiday, - dust)
      data_it <- inner_join(data_it, spstack, by=c("x", "y")) 
      
      # Merge with NTLI
      ntli_it <- gsub("year", t, modis_preds$ntli)
      ntli_it <- as.data.frame(read_rds(ntli_it))
      ntli_it <- ntli_it[complete.cases(ntli_it),]
      data_it <- inner_join(data_it, ntli_it, by=c("x", "y"))
      rm("ntli_it")
      
      # Merge with NDVI
      ndvi_it <- gsub("year", t, modis_preds$ndvi)
      ndvi_it <- as.data.frame(read_rds(ndvi_it))
      ndvi_it <- ndvi_it[complete.cases(ndvi_it),]
      data_it$link_date <- floor_date(data_it$date, "quarter")
      data_it <- inner_join(data_it, ndvi_it, by=c("x"="x", "y"="y", "link_date"="date"))
      data_it$link_date <- NULL
      rm("ndvi_it")
      
      # Merge with train/test indicator
      split_it <- dplyr::filter(split_indicators, as.character(year(date))==t)
      data_it <- left_join(data_it, split_it, by=c("gapID", "date"))
      data_it$split <- ifelse(is.na(data_it$split), "train", data_it$split)
      
      # Store dataset
      data_it$date <- NULL
      data_it$gapID <- NULL
      write_csv(data_it, paste0("database/modis_imp/", t, "/nightLST_terra.csv"))
      rm("data_it")
    }
  }
  
  #### LST day Aqua ----
  for(i in 1:3){
    # Time indicator
    t <- as.character(2018+(i-1))
    
    if(!file.exists(paste0("database/modis_imp/", t, "/dayLST_aqua.csv"))){
      # Read LST aqua
      data_it <- read_rds(lst_aqua[[i]])["dayLST_aqua"]
      
      # Read daily st predictors
      mpreds_it <- c(modis_preds$skintemp, modis_preds$`10muwind`,
                     modis_preds$`10mvwind`, modis_preds$totlprec)
      mpreds_it <- gsub("year", t, mpreds_it)
      mpreds_it <- gsub("hour", "13", mpreds_it)
      txt_parse <- paste0("c(data_it, ", paste0("read_rds('", mpreds_it, "')", collapse=", "),")")
      data_it <- eval(parse(text=txt_parse))
      data_it <- as.data.frame(data_it)
      data_it <- data_it[complete.cases(data_it),]
      
      # Merge with calendar and spatial predictors
      data_it <- inner_join(data_it, calendar, by="date") %>%
        dplyr::select(-holiday, - dust)
      data_it <- inner_join(data_it, spstack, by=c("x", "y")) 
      
      # Merge with NTLI
      ntli_it <- gsub("year", t, modis_preds$ntli)
      ntli_it <- as.data.frame(read_rds(ntli_it))
      ntli_it <- ntli_it[complete.cases(ntli_it),]
      data_it <- inner_join(data_it, ntli_it, by=c("x", "y"))
      rm("ntli_it")
      
      # Merge with NDVI
      ndvi_it <- gsub("year", t, modis_preds$ndvi)
      ndvi_it <- as.data.frame(read_rds(ndvi_it))
      ndvi_it <- ndvi_it[complete.cases(ndvi_it),]
      data_it$link_date <- floor_date(data_it$date, "quarter")
      data_it <- inner_join(data_it, ndvi_it, by=c("x"="x", "y"="y", "link_date"="date"))
      data_it$link_date <- NULL
      rm("ndvi_it")
      
      # Merge with train/test indicator
      split_it <- dplyr::filter(split_indicators, as.character(year(date))==t)
      data_it <- left_join(data_it, split_it, by=c("gapID", "date"))
      data_it$split <- ifelse(is.na(data_it$split), "train", data_it$split)
      
      # Store dataset
      data_it$date <- NULL
      data_it$gapID <- NULL
      write_csv(data_it, paste0("database/modis_imp/", t, "/dayLST_aqua.csv"))
      rm("data_it")
    }
  }
  
  #### LST night Aqua ----
  for(i in 1:3){
    # Time indicator
    t <- as.character(2018+(i-1))
    
    if(!file.exists(paste0("database/modis_imp/", t, "/nightLST_aqua.csv"))){
      # Read LST aqua
      data_it <- read_rds(lst_aqua[[i]])["nightLST_aqua"]
      
      # Read daily st predictors
      mpreds_it <- c(modis_preds$skintemp, modis_preds$`10muwind`,
                     modis_preds$`10mvwind`, modis_preds$totlprec)
      mpreds_it <- gsub("year", t, mpreds_it)
      mpreds_it <- gsub("hour", "01", mpreds_it)
      txt_parse <- paste0("c(data_it, ", paste0("read_rds('", mpreds_it, "')", collapse=", "),")")
      data_it <- eval(parse(text=txt_parse))
      data_it <- as.data.frame(data_it)
      data_it <- data_it[complete.cases(data_it),]
      
      # Merge with calendar and spatial predictors
      data_it <- inner_join(data_it, calendar, by="date") %>%
        dplyr::select(-holiday, - dust)
      data_it <- inner_join(data_it, spstack, by=c("x", "y")) 
      
      # Merge with NTLI
      ntli_it <- gsub("year", t, modis_preds$ntli)
      ntli_it <- as.data.frame(read_rds(ntli_it))
      ntli_it <- ntli_it[complete.cases(ntli_it),]
      data_it <- inner_join(data_it, ntli_it, by=c("x", "y"))
      rm("ntli_it")
      
      # Merge with NDVI
      ndvi_it <- gsub("year", t, modis_preds$ndvi)
      ndvi_it <- as.data.frame(read_rds(ndvi_it))
      ndvi_it <- ndvi_it[complete.cases(ndvi_it),]
      data_it$link_date <- floor_date(data_it$date, "quarter")
      data_it <- inner_join(data_it, ndvi_it, by=c("x"="x", "y"="y", "link_date"="date"))
      data_it$link_date <- NULL
      rm("ndvi_it")
      
      # Merge with train/test indicator
      split_it <- dplyr::filter(split_indicators, as.character(year(date))==t)
      data_it <- left_join(data_it, split_it, by=c("gapID", "date"))
      data_it$split <- ifelse(is.na(data_it$split), "train", data_it$split)
      
      # Store dataset
      data_it$date <- NULL
      data_it$gapID <- NULL
      write_csv(data_it, paste0("database/modis_imp/", t, "/nightLST_aqua.csv"))
      rm("data_it")
    }

  }
  
  # Return a list with the paths
  list("LST_terra_day" = "database/modis_imp/year/dayLST_terra.csv",
       "LST_terra_night" = "database/modis_imp/year/nightLST_terra.csv",
       "LST_aqua_day" = "database/modis_imp/year/dayLST_aqua.csv",
       "LST_aqua_night" = "database/modis_imp/year/nightLST_aqua.csv")
}

# Function to prepare the dataset for imputation model fitting of MAIAC products
impdata_maiac <- function(maiac, modis_preds, calendar, rs_splits){
  
  # Read split data
  split_areas <- read_rds(rs_splits$split_areas)
  split_indicators <- read_rds(rs_splits$split_indicators)
  
  # Read spatial and temporal predictors
  spstack <- read_rds(modis_preds$spstack)  %>%
    dplyr::select(dem, imd, tcd, coast_dist, 
                  agriculture, urban, industry, popu_dens,
                  primary_dens, secondary_dens, local_dens)
  calendar <- read_rds(calendar) %>%
    dplyr::select(date, yday, holiday, dust)
  
  # spstack to data frame
  spstack <- as.data.frame(spstack)
  spstack <- spstack[complete.cases(spstack),]
  
  # Join gapIDs
  gapID <- spstack[c("x", "y")]
  gapID <- gapID[!duplicated(gapID),]
  gapID <- st_as_sf(gapID, coords = c("x", "y"), crs = st_crs(split_areas))
  gapID <- st_join(gapID, split_areas) %>%
    mutate(x = st_coordinates(.)[,1],
           y = st_coordinates(.)[,2])
  st_geometry(gapID) <- NULL
  spstack <- inner_join(spstack, gapID, by=c("x", "y"))
  spstack <- spstack[complete.cases(spstack),]
  
  #### MAIAC Terra ----
  for(i in 1:3){
    
    # Time indicator
    t <- as.character(2018+(i-1))
    
    if(!file.exists(paste0("database/modis_imp/", t, "/maiac_terra.csv"))){
      # Read MAIAC terra
      data_it <- read_rds(maiac[[i]])["AOD55_terra"]
      
      # Read daily st predictors
      mpreds_it <- c(modis_preds$pbh, modis_preds$`2mtemp`,
                     modis_preds$`10muwind`, modis_preds$`10mvwind`, 
                     modis_preds$totlprec, modis_preds$AOD550)
      mpreds_it <- gsub("year", t, mpreds_it)
      mpreds_it <- ifelse(!grepl("AOD", mpreds_it), 
                          gsub("hour", "10", mpreds_it),
                          gsub("hour", "09", mpreds_it))
      txt_parse <- paste0("c(data_it, ", paste0("read_rds('", mpreds_it, "')", collapse=", "),")")
      data_it <- eval(parse(text=txt_parse))
      data_it <- as.data.frame(data_it)
      data_it <- data_it[complete.cases(data_it),]
      
      # Merge with calendar and spatial predictors
      data_it <- inner_join(data_it, calendar, by="date")
      data_it <- inner_join(data_it, spstack, by=c("x", "y")) 
      
      # Merge with NTLI
      ntli_it <- gsub("year", t, modis_preds$ntli)
      ntli_it <- as.data.frame(read_rds(ntli_it))
      ntli_it <- ntli_it[complete.cases(ntli_it),]
      data_it <- inner_join(data_it, ntli_it, by=c("x", "y"))
      rm("ntli_it")
      
      # Merge with NDVI
      ndvi_it <- gsub("year", t, modis_preds$ndvi)
      ndvi_it <- as.data.frame(read_rds(ndvi_it))
      ndvi_it <- ndvi_it[complete.cases(ndvi_it),]
      data_it$link_date <- floor_date(data_it$date, "quarter")
      data_it <- inner_join(data_it, ndvi_it, by=c("x"="x", "y"="y", "link_date"="date"))
      data_it$link_date <- NULL
      rm("ndvi_it")
      
      # Merge with train/test indicator
      split_it <- dplyr::filter(split_indicators, as.character(year(date))==t)
      data_it <- left_join(data_it, split_it, by=c("gapID", "date"))
      data_it$split <- ifelse(is.na(data_it$split), "train", data_it$split)
      
      # Store dataset
      data_it$date <- NULL
      data_it$gapID <- NULL
      write_csv(data_it, paste0("database/modis_imp/", t, "/maiac_terra.csv"))
      rm("data_it")
    }
  }
  
  ####  MAIAC Aqua ----
  for(i in 1:3){
    
    # Time indicator
    t <- as.character(2018+(i-1))
    
    if(!file.exists(paste0("database/modis_imp/", t, "/maiac_aqua.csv"))){
      # Read MAIAC aqua
      data_it <- read_rds(maiac[[i]])["AOD55_aqua"]
      
      # Read daily st predictors
      mpreds_it <- c(modis_preds$pbh, modis_preds$`2mtemp`,
                     modis_preds$`10muwind`, modis_preds$`10mvwind`, 
                     modis_preds$totlprec, modis_preds$AOD550)
      mpreds_it <- gsub("year", t, mpreds_it)
      mpreds_it <- ifelse(!grepl("AOD", mpreds_it), 
                          gsub("hour", "13", mpreds_it),
                          gsub("hour", "12", mpreds_it))
      txt_parse <- paste0("c(data_it, ", paste0("read_rds('", mpreds_it, "')", collapse=", "),")")
      data_it <- eval(parse(text=txt_parse))
      data_it <- as.data.frame(data_it)
      data_it <- data_it[complete.cases(data_it),]
      
      # Merge with calendar and spatial predictors
      data_it <- inner_join(data_it, calendar, by="date")
      data_it <- inner_join(data_it, spstack, by=c("x", "y")) 
      
      # Merge with NTLI
      ntli_it <- gsub("year", t, modis_preds$ntli)
      ntli_it <- as.data.frame(read_rds(ntli_it))
      ntli_it <- ntli_it[complete.cases(ntli_it),]
      data_it <- inner_join(data_it, ntli_it, by=c("x", "y"))
      rm("ntli_it")
      
      # Merge with NDVI
      ndvi_it <- gsub("year", t, modis_preds$ndvi)
      ndvi_it <- as.data.frame(read_rds(ndvi_it))
      ndvi_it <- ndvi_it[complete.cases(ndvi_it),]
      data_it$link_date <- floor_date(data_it$date, "quarter")
      data_it <- inner_join(data_it, ndvi_it, by=c("x"="x", "y"="y", "link_date"="date"))
      data_it$link_date <- NULL
      rm("ndvi_it")
      
      # Merge with train/test indicator
      split_it <- dplyr::filter(split_indicators, as.character(year(date))==t)
      data_it <- left_join(data_it, split_it, by=c("gapID", "date"))
      data_it$split <- ifelse(is.na(data_it$split), "train", data_it$split)
      
      # Store dataset
      data_it$date <- NULL
      data_it$gapID <- NULL
      write_csv(data_it, paste0("database/modis_imp/", t, "/maiac_aqua.csv"))
      rm("data_it")
    }
  }
  
  # Return a list with the paths
  list("MAIAC_terra"= "database/modis_imp/year/maiac_terra.csv",
       "MAIAC_aqua"= "database/modis_imp/year/maiac_aqua.csv")
}