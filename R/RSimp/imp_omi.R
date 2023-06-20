#-----------------------------------------------------------------------------#
#                      RS imputation: omi NO2 and O3                      #
#-----------------------------------------------------------------------------#

# Function to resample predictors for OMI imputation models to the products' geometries
resample_omi <- function(omi_no2, spstack, ndvi, ntli, era5rean, era5land, camsglobal, cat){
  
  # Read inputs
  cat <- read_rds(cat)
  omi_no2 <- adrop(read_rds(omi_no2$omi_no2_18)[1,,,1])
  
  # Select and resample spatial predictors
  spstack <- read_rds(spstack)
  spstack <- dplyr::select(spstack, c("dem", "imd", "tcd", "coast_dist", 
                                      "agriculture", "urban", "industry", "popu_dens",
                                      "primary_dens", "secondary_dens", "local_dens"))
  resample_preds(spstack, omi_no2, "'average'", cat, "database/omi_imp/spatial/spstack.rds")
  rm("spstack")
  
  # Spatio temporal predictors
  for(i in 1:3){
    t <- as.character(2018+(i-1))
    # Resample 
    resample_preds(read_rds(ndvi[[i]]), omi_no2, "'average'", cat, paste0("database/omi_imp/", t, "/ndvi.rds"))
    resample_preds(adrop(read_rds(ntli[[i]])), omi_no2, "'average'", cat, paste0("database/omi_imp/", t, "/ntli.rds"))
    resample_preds(read_rds(era5rean[[i]])["ERA5Rean_pbh_13"], omi_no2, "'bilinear'", cat, paste0("database/omi_imp/", t, "/pbh_13.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_10muwind_13"], omi_no2, "'average'", cat, paste0("database/omi_imp/", t, "/10muwind_13.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_10mvwind_13"], omi_no2, "'average'", cat, paste0("database/omi_imp/", t, "/10mvwind_13.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_2mtemp_13"], omi_no2, "'average'", cat, paste0("database/omi_imp/", t, "/2mtemp_13.rds"))
    resample_preds(read_rds(era5land[[i]])["ERA5Land_totlprec_13"], omi_no2, "'average'", cat, paste0("database/omi_imp/", t, "/totlprec_13.rds"))
    resample_preds(read_rds(camsglobal[[i]])["CAMSglobal_NO2_12"], omi_no2, "'bilinear'", cat, paste0("database/omi_imp/", t, "/NO2_12.rds"))
    resample_preds(read_rds(camsglobal[[i]])["CAMSglobal_O3_12"], omi_no2, "'bilinear'", cat, paste0("database/omi_imp/", t, "/O3_12.rds"))
  }
  
  # Return a list with the paths
  list("spstack"= "database/omi_imp/spatial/spstack.rds",
       "ndvi" = "database/omi_imp/year/ndvi.rds",
       "ntli" = "database/omi_imp/year/ntli.rds",
       "mslvlp" = "database/omi_imp/year/mslvlp_hour.rds",
       "pbh" = "database/omi_imp/year/pbh_hour.rds",
       "2mtemp" = "database/omi_imp/year/2mtemp_hour.rds",
       "10muwind" = "database/omi_imp/year/10muwind_hour.rds",
       "10mvwind" = "database/omi_imp/year/10mvwind_hour.rds",
       "totlprec" = "database/omi_imp/year/totlprec_hour.rds",
       "NO2" = "database/omi_imp/year/NO2_hour.rds",
       "O3" = "database/omi_imp/year/O3_hour.rds")
}

# Function to prepare the dataset for imputation model fitting of OMI products
impdata_omi <- function(omi_no2, omi_o3, omi_preds, calendar, rs_splits){
  
  # Read split data
  split_areas <- read_rds(rs_splits$split_areas)
  split_indicators <- read_rds(rs_splits$split_indicators)
  
  # Read spatial and temporal predictors
  spstack <- read_rds(omi_preds$spstack)  %>%
    dplyr::select(dem, imd, tcd, coast_dist, 
                  agriculture, urban, industry, popu_dens,
                  primary_dens, secondary_dens, local_dens)
  calendar <- read_rds(calendar) %>%
    dplyr::select(date, yday, holiday)
  
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
  
  #### omi NO2 ----
  for(i in 1:3){
    
    # Time indicator
    t <- as.character(2018+(i-1))
    
    if(!file.exists(paste0("database/omi_imp/", t, "/omi_NO2.csv"))){
      # Read omi
      data_it <- read_rds(omi_no2[[i]])
      
      # Read daily st predictors
      mpreds_it <- c(omi_preds$pbh, omi_preds$`2mtemp`,
                     omi_preds$`10muwind`, omi_preds$`10mvwind`, 
                     omi_preds$totlprec, omi_preds$NO2)
      mpreds_it <- gsub("year", t, mpreds_it)
      mpreds_it <- ifelse(!grepl("NO2", mpreds_it), 
                          gsub("hour", "13", mpreds_it),
                          gsub("hour", "12", mpreds_it))
      txt_parse <- paste0("c(", paste0("read_rds('", mpreds_it, "')", 
                                       collapse=", "),")")
      mpreds_it <- eval(parse(text=txt_parse))
      data_it <- c(data_it, mpreds_it)
      data_it <- as.data.frame(data_it)
      data_it <- data_it[complete.cases(data_it),]
      
      # Merge with calendar and spatial predictors
      data_it <- inner_join(data_it, calendar, by="date")
      data_it <- inner_join(data_it, spstack, by=c("x", "y"))
      
      # Merge with NTLI
      ntli_it <- gsub("year", t, omi_preds$ntli)
      ntli_it <- as.data.frame(read_rds(ntli_it))
      ntli_it <- ntli_it[complete.cases(ntli_it),]
      data_it <- inner_join(data_it, ntli_it, by=c("x", "y"))
      rm("ntli_it")
      
      # Merge with NDVI
      ndvi_it <- gsub("year", t, omi_preds$ndvi)
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
      write_csv(data_it, paste0("database/omi_imp/", t, "/omi_NO2.csv"))
      rm("data_it")
    }
  }
  
  #### omi O3 ----
  for(i in 1:3){
    
    # Time indicator
    t <- as.character(2018+(i-1))
    
    if(!file.exists(paste0("database/omi_imp/", t, "/omi_O3.csv"))){
      # Read omi
      data_it <- read_rds(omi_o3[[i]])
      
      # Read daily st predictors
      mpreds_it <- c(omi_preds$pbh, omi_preds$`2mtemp`,
                     omi_preds$`10muwind`, omi_preds$`10mvwind`, 
                     omi_preds$totlprec, omi_preds$O3)
      mpreds_it <- gsub("year", t, mpreds_it)
      mpreds_it <- ifelse(!grepl("O3", mpreds_it), 
                          gsub("hour", "13", mpreds_it),
                          gsub("hour", "12", mpreds_it))
      txt_parse <- paste0("c(", paste0("read_rds('", mpreds_it, "')", 
                                       collapse=", "),")")
      mpreds_it <- eval(parse(text=txt_parse))
      data_it <- c(data_it, mpreds_it)
      data_it <- as.data.frame(data_it)
      data_it <- data_it[complete.cases(data_it),]
      
      # Merge with calendar and spatial predictors
      data_it <- inner_join(data_it, calendar, by="date")
      data_it <- inner_join(data_it, spstack, by=c("x", "y"))
      
      # Merge with NTLI
      ntli_it <- gsub("year", t, omi_preds$ntli)
      ntli_it <- as.data.frame(read_rds(ntli_it))
      ntli_it <- ntli_it[complete.cases(ntli_it),]
      data_it <- inner_join(data_it, ntli_it, by=c("x", "y"))
      rm("ntli_it")
      
      # Merge with NDVI
      ndvi_it <- gsub("year", t, omi_preds$ndvi)
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
      write_csv(data_it, paste0("database/omi_imp/", t, "/omi_O3.csv"))
      rm("data_it")
    }
  }
  
  # Return a list with the paths
  list("omi_no2"= "database/omi_imp/year/omi_NO2.csv",
       "omi_o3"= "database/omi_imp/year/omi_O3.csv")
  
}
