#-----------------------------------------------------------------------------#
#                            Focal mean predictors                            #
#-----------------------------------------------------------------------------#

# Function to compute NDVI focal means
ndvi_focal <- function(ndviyear, focalw, cat, labvar){
  
  ndvi_year1 <- adrop(ndviyear[,,,1])
  ndvi_sumvar1 <- focal2(ndvi_year1, focalw, "sum", na.rm = T)
  ndvi_sumw1 <- focal2(!is.na(ndvi_year1), focalw, "sum", na.rm = T)
  ndvi_gaus1 <- ndvi_sumvar1/ndvi_sumw1
  
  ndvi_year2 <- adrop(ndviyear[,,,2])
  ndvi_sumvar2 <- focal2(ndvi_year2, focalw, "sum", na.rm = T)
  ndvi_sumw2 <- focal2(!is.na(ndvi_year2), focalw, "sum", na.rm = T)
  ndvi_gaus2 <- ndvi_sumvar2/ndvi_sumw2
  
  ndvi_year3 <- adrop(ndviyear[,,,3])
  ndvi_sumvar3 <- focal2(ndvi_year3, focalw, "sum", na.rm = T)
  ndvi_sumw3 <- focal2(!is.na(ndvi_year3), focalw, "sum", na.rm = T)
  ndvi_gaus3 <- ndvi_sumvar3/ndvi_sumw3
  
  ndvi_year4 <- adrop(ndviyear[,,,4])
  ndvi_sumvar4 <- focal2(ndvi_year4, focalw, "sum", na.rm = T)
  ndvi_sumw4 <- focal2(!is.na(ndvi_year4), focalw, "sum", na.rm = T)
  ndvi_gaus4 <- ndvi_sumvar4/ndvi_sumw4
  
  ndvi_focalall <- c(ndvi_gaus1, ndvi_gaus2, ndvi_gaus3, ndvi_gaus4, along=3) %>%
    st_set_dimensions(3, names="date",
                      values = st_get_dimension_values(ndviyear,3))
  names(ndvi_focalall) <- labvar
  st_crop(ndvi_focalall, cat)
}

# Function to compute focal means of a selected set of predictors
focal_mean <- function(spstack, ndvi, cat){
  
  # Study area boundaries
  cat <- read_rds(cat)
  
  # Spatial predictors: Land use, road density, TCD and IMD
  spstack <- read_rds(spstack)
  spstack <- dplyr::select(spstack,
                           imd, tcd, water, agriculture, 
                           urban, industry, woodland, popu_dens,
                           primary_dens, secondary_dens, local_dens)
  
  # focal mean weights
  focalw <- raster::focalWeight(as(spstack[1], "Raster"), 1000, "Gaus")
  
  # Compute focal means
  for(attr_indx in names(spstack)){
    
    print(paste0("Computing spstack focal means. Attribute: ", attr_indx))
    
    # focal weighted average
    targetr <- adrop(spstack[attr_indx,,])
    focal_sumvar <- focal2(targetr, focalw, "sum", na.rm = T)
    focal_sumw <- focal2(!is.na(targetr), focalw, "sum", na.rm = T)
    focal_gaus <- focal_sumvar/focal_sumw
    
    # Stack
    if(!exists("spstack_gaus")){
      spstack_gaus <- focal_gaus
    }else{
      spstack_gaus <- c(spstack_gaus, focal_gaus)
    }
    rm("targetr", "focal_gaus")
  }
  spstack_gaus <- st_crop(spstack_gaus, cat)
  names(spstack_gaus) <- paste0(names(spstack_gaus), "_focal")
  write_rds(spstack_gaus, "database/original/spatial/spstack_focal.rds")
  rm("spstack", "spstack_gaus")
  
  # Spatio-temporal predictors: NDVI
  ndvi18 <- read_rds(ndvi$ndvi18)
  ndvi19 <- read_rds(ndvi$ndvi19)
  ndvi20 <- read_rds(ndvi$ndvi20)
  ndvi18_gaus <- ndvi_focal(ndvi18, focalw, cat, "ndvi_focal")
  ndvi19_gaus <- ndvi_focal(ndvi19, focalw, cat, "ndvi_focal")
  ndvi20_gaus <- ndvi_focal(ndvi20, focalw, cat, "ndvi_focal")
  write_rds(ndvi18_gaus, "database/original/2018/ndvi_focal.rds")
  write_rds(ndvi19_gaus, "database/original/2019/ndvi_focal.rds")
  write_rds(ndvi20_gaus, "database/original/2020/ndvi_focal.rds")
  rm("ndvi", "ndvi18", "ndvi19", "ndvi20",
     "ndvi18_gaus", "ndvi19_gaus", "ndvi20_gaus")
  
  # Return paths
  list("spstack_focal"="database/original/spatial/spstack_focal.rds", 
       "ndvi18_focal"="database/original/2018/ndvi_focal.rds",
       "ndvi19_focal"="database/original/2019/ndvi_focal.rds",
       "ndvi20_focal"="database/original/2020/ndvi_focal.rds")
}