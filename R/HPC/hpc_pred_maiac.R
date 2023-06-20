#-----------------------------------------------------------------------------#
#                   Cluster RS prediction: MAIAC terra and aqua               #
#-----------------------------------------------------------------------------#

#### Prep ----
rm(list = ls())
library("tidyverse")
library("lubridate")
library("caret")
library("ranger")
library("stars")

# Config HPC
pathdb <- "****"
pathmod <- "****"

# Read common spatial and temporal predictors
spstack <- read_rds(paste0(pathdb, "modis_imp/spatial/spstack.rds"))
calendar <- read_rds(paste0(pathdb, "original/calendar.rds")) %>%
  dplyr::select(date, yday, dust, holiday) 

# Function to predict MAIAC
pred_maiac <- function(d, mod, dpreds, ndvi, ntli, spstack, calendar_it){
  
  # Print day
  print(paste0("Processing day: ", d))
  
  # Add variables to the layer
  pdata <- adrop(dpreds[,,,d])
  pdata$x <- st_coordinates(pdata)[,1]
  pdata$y <- st_coordinates(pdata)[,2]
  pdata$yday <- d
  pdata$holiday <- calendar_it$holiday[calendar_it$yday==d]
  pdata$dust <- calendar_it$dust[calendar_it$yday==d]
  pdata <- c(pdata, 
             adrop(ndvi[,,,calendar_it$ndvi_date[calendar_it$yday==d]]),
             ntli,
             spstack)
  
  # Predict and fill when necessary, stack, clean
  misspixels <- complete.cases(as.data.frame(dplyr::select(pdata, -outcome)))
  pdata$preds <- NA
  pdata$preds[misspixels] <- predict(mod, pdata)
  pdata <- mutate(pdata,
                  outcome = ifelse(is.na(outcome), preds, outcome)) %>%
    dplyr::select(outcome)
  names(pdata) <- paste0("outcome", d)
  pdata
}

#### Terra ----
print("Processing terra day...")
for(y in 2018:2020){

  print(paste0("Year: ", y))

  # Prepare calendar + non-daily predictors
  calendar_it <- dplyr::filter(calendar, year(date)==y)%>%
    mutate(ndvi_date = as.integer(as.factor(floor_date(date, "quarter"))))
  ndvi <- read_rds(paste0(pathdb, "modis_imp/", y, "/ndvi.rds"))
  ntli <- read_rds(paste0(pathdb, "modis_imp/", y, "/ntli.rds"))

  # Prepare daily predictors
  dpreds <- c(read_rds(paste0(pathdb, "original/", y, "/maiac.rds"))["AOD55_terra"],
              read_rds(paste0(pathdb, "modis_imp/", y, "/2mtemp_10.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/10muwind_10.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/10mvwind_10.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/totlprec_10.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/pbh_10.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/AOD550_09.rds")))
  names(dpreds)[1] <- "outcome"

  # Prepare model
  mod <- read_rds(paste0(pathmod, "modmaiac_terra_", y, ".rds"))

  # Impute data in parallel
  imps <- map(calendar_it$yday, pred_maiac,
              mod=mod, dpreds=dpreds, ndvi=ndvi, ntli=ntli, spstack=spstack,
              calendar_it=calendar_it)

  txt_parse <- paste0("c(", paste0("imps[[", 1:length(imps), "]]", collapse = ", "), ")")
  imps <- eval(parse(text=txt_parse))
  imps <- merge(imps) %>%
    st_set_dimensions(3, name="date", values=calendar_it$date)
  names(imps) <- "AOD55_terra"

  # Write and clean
  write_rds(imps, paste0(pathmod, "impmaiac_terra_", y, ".rds"))
  rm("imps", "txt_parse", "mod", "dpreds", "calendar_it", "ndvi", "ntli")
}

#### Aqua  ----
print("Processing aqua day...")
for(y in 2018:2020){
  
  print(paste0("Year: ", y))
  
  # Prepare calendar + non-daily predictors
  calendar_it <- dplyr::filter(calendar, year(date)==y)%>%
    mutate(ndvi_date = as.integer(as.factor(floor_date(date, "quarter"))))
  ndvi <- read_rds(paste0(pathdb, "modis_imp/", y, "/ndvi.rds"))
  ntli <- read_rds(paste0(pathdb, "modis_imp/", y, "/ntli.rds"))
  
  # Prepare daily predictors
  dpreds <- c(read_rds(paste0(pathdb, "original/", y, "/maiac.rds"))["AOD55_aqua"],
              read_rds(paste0(pathdb, "modis_imp/", y, "/2mtemp_13.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/10muwind_13.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/10mvwind_13.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/totlprec_13.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/pbh_13.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/AOD550_12.rds")))
  names(dpreds)[1] <- "outcome"
  
  # Prepare model
  mod <- read_rds(paste0(pathmod, "modmaiac_aqua_", y, ".rds"))
  
  # Impute data in parallel
  imps <- map(calendar_it$yday, pred_maiac,
              mod=mod, dpreds=dpreds, ndvi=ndvi, ntli=ntli, spstack=spstack,
              calendar_it=calendar_it)
  
  txt_parse <- paste0("c(", paste0("imps[[", 1:length(imps), "]]", collapse = ", "), ")")
  imps <- eval(parse(text=txt_parse))
  imps <- merge(imps) %>%
    st_set_dimensions(3, name="date", values=calendar_it$date)
  names(imps) <- "AOD55_aqua"
  
  # Write and clean
  write_rds(imps, paste0(pathmod, "impmaiac_aqua_", y, ".rds"))
  rm("imps", "txt_parse", "mod", "dpreds", "calendar_it", "ndvi", "ntli")
}

#### Finish ----
rm(list = ls())