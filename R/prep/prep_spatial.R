#-----------------------------------------------------------------------------#
#                    Pre-processing spatial data functions                    #
#-----------------------------------------------------------------------------#

# Prepare coordinates predictors
prep_coords <- function(gridr, cat){
  
  # Read inputs
  gridr <- read_rds(gridr)
  cat <- read_rds(cat)
  
  # Generate coordinate rasters
  coords <- gridr
  coords_df <- st_coordinates(coords)
  coords$x <- coords_df[,1]
  coords$y <- coords_df[,2]
  coords$values <- NULL
  coords <- coords %>%
    st_set_dimensions("x", point=F) %>%
    st_set_dimensions("y", point=F)
  coords <- coords[st_buffer(cat, 100)]
  
  # Write and return path
  write_rds(coords, "database/original/spatial/coords.rds")
  "database/original/spatial/coords.rds"
}

# Prepare terrain predictors: DEM, slope, aspect
prep_terrain <- function(path_dem, gridr, cat){
  
  # Read raster and inputs
  dem <- suppressWarnings(raster(path_dem))
  gridr <- read_rds(gridr)
  cat <- read_rds(cat)
  
  # Missing pixels are water (or coastline). Assign a 0 to the DEM.
  dem[is.na(dem)] <- 0
  
  # Warp to target CRS
  dem <- st_warp(st_as_stars(dem), gridr, method="average", use_gdal=TRUE,
                 no_data_value = -9999)
  names(dem) <- "dem"
  
  # Derive slope and aspect, to stars
  slope <- terrain(suppressWarnings(as(dem, "Raster")), "slope", unit="degrees")
  slope <- st_as_stars(slope) %>%
    st_set_dimensions(1, point=F) %>%
    st_set_dimensions(2, point=F)
  aspect <- terrain(suppressWarnings(as(dem, "Raster")), "aspect", unit="degrees")
  aspect <- st_as_stars(aspect) %>%
    st_set_dimensions(1, point=F) %>%
    st_set_dimensions(2, point=F)
  
  # Merge products, crop to Catalonia
  terrain <- c(dem, slope, aspect)
  terrain <- terrain[st_buffer(cat, 100)]
  
  # Write and return path
  write_rds(terrain, "database/original/spatial/terrain.rds")
  "database/original/spatial/terrain.rds"
}

# Prepare Copernicus Land Monitoring Service data: impervious and forest density
prep_copernicusLMS <- function(path_raster, gridr, label, cat){
  
  # Read raster and inputs
  rast <- read_stars(path_raster)
  gridr <- read_rds(gridr)
  cat <- read_rds(cat)
  
  # IMD and TCD need some value fixing
  rast[[1]] <- as.numeric(as.character(rast[[1]]))
  rast[[1]][rast[[1]]>100] <- NA
  
  # Transform the raster to the target CRS
  rast <- st_warp(rast, gridr, method="average", use_gdal=TRUE,
                  no_data_value = -9999)
  names(rast) <- label
  rast <- rast[st_buffer(cat, 100)]
  
  # Write and return path
  write_rds(rast, paste0("database/original/spatial/", label, ".rds"))
  paste0("database/original/spatial/", label, ".rds")
}

# Prepare distance to coastline predictor
prep_coast <- function(path_coast, gridp, gridr, cat){
  
  # Read inputs
  gridp <- read_rds(gridp)
  gridr <- read_rds(gridr)
  cat <- read_rds(cat)
  
  # Prepare coastline
  coast <- st_read(path_coast) %>%
    group_by() %>%
    summarise() %>%
    st_geometry() %>%
    st_transform(crs = st_crs(gridp))
  
  # Prepare data splits
  gridp_coast <- gridp
  reps <- 200
  slices <- sort(rep(1:reps, ceiling(nrow(gridp_coast)/reps))[1:nrow(gridp_coast)])
  hind <- split(gridp_coast, slices)
  
  # Run distance analysis in parallel
  options(future.rng.onMisuse="ignore")
  plan(multisession, workers=10)
  gridp_coast$coast_dist <- unlist(
    future_map(hind, st_nearest_dist_par, .progress=TRUE, target_feature=coast))
  plan(sequential)
  
  # Rasterize
  coast <- st_rasterize(gridp_coast["coast_dist"], gridr)
  coast <- coast[st_buffer(cat, 100)]
  
  # Write and return path
  write_rds(coast, "database/original/spatial/coast.rds")
  "database/original/spatial/coast.rds"
}

# Prepare Land Use Land Cover predictors
prep_lulc <- function(path_lulc, gridr){
  
  # Read inputs
  gridr <- read_rds(gridr)

  # Read LULC classes
  water <- lulc_aux(path_lulc, gridr, 1:2)
  agriculture <- lulc_aux(path_lulc, gridr, c(8:12,24:25))
  urban <- lulc_aux(path_lulc, gridr, 5:6)
  industry <- lulc_aux(path_lulc, gridr, 7)
  woodland <- lulc_aux(path_lulc, gridr, 17:19)
  
  # Single stars object with three dimensions
  lulc_names <- c("water", "agriculture", "urban", "industry", "woodland")
  lulc <- c(water, agriculture, urban, industry, woodland)
  names(lulc) <- lulc_names
  
  # Write and return path
  write_rds(lulc, "database/original/spatial/lulc.rds")
  "database/original/spatial/lulc.rds"
}

# Utils for Land Use Land Cover predictor derivation
lulc_aux <- function(path_lulc, gridr, codes_lulc){
  
  # Read LULC and aggregate to 100m
  lulc <- raster(path_lulc)
  lulc_recl <- matrix(c(1:255, rep(0, 255)), ncol=2)
  lulc_recl[codes_lulc, 2] <- 1
  lulc <- reclassify(lulc, lulc_recl)
  
  # To stars and warp to reference grid
  lulc <- st_as_stars(lulc, point=FALSE)
  lulc <- st_warp(lulc, gridr, method="average", use_gdal=TRUE,
                  no_data_value = -9999)
  names(lulc) <- "perc_lulc"
  lulc
}

# Prepare distance to point sources predictors
prep_psources <- function(path_point, gridp,  gridr, cat){
  
  # Read inputs
  gridp <- read_rds(gridp)
  gridr <- read_rds(gridr)
  cat <- read_rds(cat)

  # Read and clean data
  emissions <- read_xls(path_point, "NFR-14", skip=22)
  emissions <- emissions[-1,]
  emissions <- dplyr::select(emissions,
                             LPS, GNFR, `Height class (1-5)`, Longitude, Latitude,
                             starts_with("PM"), starts_with("NO"))  %>%
    rename(NOx=`NOx (as NO2)`, height=`Height class (1-5)`) %>%
    mutate(across(c(Longitude, Latitude, PM2.5, PM10, NOx), as.numeric)) %>%
    dplyr::filter(!is.na(PM2.5)|!is.na(PM10)|!is.na(NOx)) %>%
    dplyr::filter(LPS != "0330") %>% # There is nothing in there
    mutate_all(function(x) ifelse(is.na(x), 0, x)) %>%
    dplyr::filter(!duplicated(.))
  
  # Sum emissions for the same point source but different types
  emissions <- group_by(emissions, LPS, Longitude, Latitude, height) %>%
    summarise(across(c(PM2.5, PM10, NOx), sum))
  
  # Make a sf object, project, and filter to Catalonia
  emissions <- st_as_sf(emissions, coords=c("Longitude", "Latitude"), crs=4326) %>%
    st_transform(crs = st_crs(cat))
  emissions <- emissions[st_intersects(emissions, cat, sparse=F),]
  
  # New grid for storing distances
  emissions_gripd <- gridp
  
  # Emissions - distance
  emissions_pm25 <- dplyr::select(emissions, LPS, PM2.5) %>%
    dplyr::filter(PM2.5 != 0)
  emissions_pm25 <- dplyr::filter(emissions_pm25, PM2.5 >= 0.001)
  dist_pm25 <- st_nn(emissions_gripd, emissions_pm25, sparse=F, returnDist=T) %>%
    .$dist %>%
    unlist()
  emissions_gripd$dist_pm25 <- dist_pm25/1000
  emissions_pm25 <- st_rasterize(emissions_gripd["dist_pm25"], gridr)
  emissions_pm25 <- emissions_pm25[st_buffer(cat, 100)]
  
  # PM10 - distance
  emissions_pm10 <- dplyr::select(emissions, LPS, PM10) %>%
    dplyr::filter(PM10 != 0)
  emissions_pm10 <- dplyr::filter(emissions_pm10, PM10 >= 0.001)
  dist_pm10 <- st_nn(emissions_gripd, emissions_pm10, sparse=F, returnDist=T) %>%
    .$dist %>%
    unlist()
  emissions_gripd$dist_pm10 <- dist_pm10/1000
  emissions_pm10 <- st_rasterize(emissions_gripd["dist_pm10"], gridr)
  emissions_pm10 <- emissions_pm10[st_buffer(cat, 100)]
  
  # NOx - distance
  emissions_nox <- dplyr::select(emissions, LPS, NOx) %>%
    dplyr::filter(NOx != 0)
  emissions_nox <- dplyr::filter(emissions_nox, NOx >= 0.001)
  dist_nox <- st_nn(emissions_gripd, emissions_nox, sparse=F, returnDist=T) %>%
    .$dist %>%
    unlist()
  emissions_gripd$dist_nox <- dist_nox/1000
  emissions_nox <- st_rasterize(emissions_gripd["dist_nox"], gridr)
  emissions_nox <- emissions_nox[st_buffer(cat, 100)]
  
  # Return a single stars object
  emissions <- c(emissions_pm25, emissions_pm10, emissions_nox)
  names(emissions) <- c("PM2.5_pemisdist", "PM10_pemisdist", "NOx_pemisdist")
  
  # Write and return path
  write_rds(emissions, paste0("database/original/spatial/point_sources.rds"))
  "database/original/spatial/point_sources.rds"
}

# Prepare population density predictors
prep_popu <- function(path_popu, gridr, cat){
  
  # Read inputs
  gridr <- read_rds(gridr)
  cat <- read_rds(cat)
  
  # If population <17, values are set to NA. We assume population = 10 to avoid
  # having inhabited areas that are in fact populated 
  popu <- read_sf(path_popu) %>%
    mutate(TOTAL=ifelse(is.na(TOTAL), 10, TOTAL), 
           area=as.numeric(st_area(.)/10000), # hectares
           popu_dens=TOTAL/area) %>%
    dplyr::select(popu_dens)
  popu_dens <- st_rasterize(popu["popu_dens"], gridr)
  popu_dens <- popu_dens[st_buffer(cat, 100)]
  
  # Write and return path
  write_rds(popu_dens, paste0("database/original/spatial/popu_dens.rds"))
  "database/original/spatial/popu_dens.rds"
  
}

# Auxiliary function to make nearest distance computations parallel
st_nearest_dist_par <- function(rind, target_feature){
  nf <- st_nearest_feature(rind, target_feature)
  as.numeric(st_distance(rind, target_feature[nf,], by_element = TRUE))/1000
}

# Auxiliary function to compute road density
road_dens <- function(road_lines, gridpoly){
  
  # Compute intersection and road length per cell
  road_lines_inter <- st_intersection(road_lines, gridpoly)
  road_lines_inter$rdens <- as.numeric(st_length(road_lines_inter))
  road_lines_inter <- st_drop_geometry(road_lines_inter) %>%
    group_by(cellID) %>%
    summarise(rdens=sum(rdens))
  
  # Drop geometries and merge
  road_gridpoly <- st_drop_geometry(gridpoly)
  road_gridpoly <- left_join(road_gridpoly, road_lines_inter, by="cellID")
  
  # Fill with 0s if not covered by any road
  road_gridpoly$rdens <- ifelse(is.na(road_gridpoly$rdens), 0, road_gridpoly$rdens)
  road_gridpoly$rdens
}

# Prepare road density predictors
prep_roaddens <- function(path_roads, gridp, gridr, cat){
  
  # Read inputs
  gridp <- read_rds(gridp)
  gridr <- read_rds(gridr)
  cat <- read_rds(cat)
  
  # Read road object
  roads <- st_read(path_roads) %>%
    st_transform(crs = st_crs(cat))
  
  # Filter road classes
  primary <- dplyr::filter(roads, fclass %in% c("motorway", "motorway_link",
                                                "primary", "primary_link",
                                                "trunk", "trunk_link")) %>%
    dplyr::select(-fclass)
  secondary <- dplyr::filter(roads, fclass %in% c("secondary", "secondary_link",
                                                  "tertiary", "tertiary_link")) %>%
    dplyr::select(-fclass)
  local <- dplyr::filter(roads, fclass %in% c("living_street", "unclassified",
                                              "service", "residential")) %>%
    dplyr::select(-fclass)
  rm("roads")
  
  # Prepare polygon grid
  gridpoly <- st_as_sf(gridr[1], as_points = FALSE, merge = FALSE)
  gridpoly$values <- NULL
  gridpoly$cellID <- 1:nrow(gridpoly)
  gridpoly <- dplyr::filter(gridpoly, cellID %in% gridp$cellID)
  
  # Compute density
  gridp_roads <- gridp
  gridp_roads$primary_dens <- road_dens(primary, gridpoly)
  gridp_roads$secondary_dens <- road_dens(secondary, gridpoly)
  gridp_roads$local_dens <- road_dens(local, gridpoly)
  
  # Rasterize density fields and return a single stars object
  primary_dens <- st_rasterize(gridp_roads[c("primary_dens")], gridr)
  secondary_dens <- st_rasterize(gridp_roads[c("secondary_dens")], gridr)
  local_dens <- st_rasterize(gridp_roads[c("local_dens")], gridr)
  roads_dens <- c(primary_dens, secondary_dens, local_dens)
  names(roads_dens) <- c("primary_dens", "secondary_dens", "local_dens")
  roads_dens <- roads_dens[st_buffer(cat, 100)]
  
  # Write and return path
  write_rds(roads_dens, paste0("database/original/spatial/roads_dens.rds"))
  "database/original/spatial/roads_dens.rds"
}

# Stack rasters in stars format
stack_spatial <- function(list_datasets){
  
  parsing_txt <- paste0("c(", 
                        paste0("read_rds(list_datasets[[", 1:length(list_datasets),"]])", 
                               collapse = ","), ")")
  spstack <- eval(parse(text=parsing_txt))
  
  # Write and return path
  write_rds(spstack, paste0("database/original/spatial/spstack.rds"))
  "database/original/spatial/spstack.rds"
}