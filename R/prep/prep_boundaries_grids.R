#-----------------------------------------------------------------------------#
#              Pre-processing data functions: Boundaries and grids            #
#-----------------------------------------------------------------------------#

# Prepare a polygon with the study area based on province geometries
prep_cat <- function(path_provinces){
  
  # Read and dissolve polygons to get Catalonia boundaries
  cat <- read_sf(path_provinces) %>% 
    group_by() %>%
    summarise()
  
  # Get rid of small non-inhabited islands
  cat <- sf::st_cast(cat, "POLYGON") %>%
    mutate(area = sf::st_area(.)) %>%
    dplyr::filter(area==max(area)) %>%
    dplyr::select(-area)
  
  # Write and return path
  write_rds(cat, "database/original/cat.rds")
  "database/original/cat.rds"
}

# Prepare a 250m prediction grid in stars format
prep_gridr <- function(cat){
  
  # Read inputs
  cat <- read_rds(cat)
  
  # Construct target grid
  gridr <- st_as_stars(st_bbox(cat), dx=250, dy=250)
  
  # Write and return path
  write_rds(gridr, "database/original/gridr.rds")
  "database/original/gridr.rds"
}

# Prepare a 250m prediction grid in point format
prep_gridp <- function(gridr, cat){
  
  # Read inputs
  cat <- read_rds(cat)
  gridr <- read_rds(gridr) 
  
  # Convert to grid to points
  gridp <- st_as_sf(gridr[1], as_points = TRUE, merge = FALSE)
  gridp$values <- NULL
  gridp$cellID <- 1:nrow(gridp)
  gridp <- st_filter(gridp, st_buffer(cat, 250))
  
  # Write and return path
  write_rds(gridp, "database/original/gridp.rds")
  "database/original/gridp.rds"
}
