#-----------------------------------------------------------------------------#
#             RS imputation: resampling and train/test split tools            #
#-----------------------------------------------------------------------------#

# Divide the study area into spatial blocks for train/test structured split
RSimp_splits <- function(cat){
  
  # Read input
  cat <- read_rds(cat)
  
  # Build gapIDs 
  gapsplit <- st_as_stars(st_bbox(cat),
                          dx=30000, dy=30000) %>%
    st_as_sf() %>%
    st_filter(cat) %>%
    mutate(gapID = 1:n()) 
  gapsplit$values <- NULL
  
  # Randomly assign a 30% of squares to the test set, by date
  set.seed(1234)
  tsamp <- map_df(seq.Date(as.Date("2018-01-01"), as.Date("2020-12-31"), "1 day"),
                  function(d, nmax){
                    data.frame(date=d, gapID=sample(1:nmax, round(nmax*0.3)))
                  }, max(gapsplit$gapID))
  tsamp$split <- "test"
  
  # Write and return paths
  write_rds(gapsplit, "database/split_areas.rds")
  write_rds(tsamp, "database/split_indicators.rds")
  list("split_areas"="database/split_areas.rds",
       "split_indicators"="database/split_indicators.rds")
}

# Helper to resample stars rasters to another resolution
resample_preds <- function(dataset, raster_model, method, cat, savepath){
  
  # If file already exists, skip
  if(!file.exists(savepath)){
    
    # Perform resampling
    raster_model <- st_warp(dataset[1,], raster_model)
    txt_parse <- paste0("c(",
                        paste0("st_warp(dataset[", 1:length(dataset), 
                               "], raster_model, method=", method,
                               ", use_gdal=T, no_data_value = -9999)",
                               collapse = ","),
                        ")")
    dataset_resampled <- eval(parse(text=txt_parse))
    names(dataset_resampled) <- names(dataset)
    
    # If the original dataset has a third time dimension, set time stamps
    if(length(dim(dataset))==3){
      dates_vetor <- st_get_dimension_values(dataset, "date")
      dataset_resampled <- st_set_dimensions(dataset_resampled, 3,
                                             names = "date", 
                                             values = dates_vetor)
    }
    # Write to disk as RDS
    write_rds(dataset_resampled, savepath)
  }
}
