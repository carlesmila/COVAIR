#-----------------------------------------------------------------------------#
#                Cluster RS prediction: OMI NO2 and O3 columns            #
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
spstack <- read_rds(paste0(pathdb, "omi_imp/spatial/spstack.rds"))
calendar <- read_rds(paste0(pathdb, "original/calendar.rds")) %>%
  dplyr::select(date, yday, dust, holiday)

# Function to predict omi
pred_omi <- function(d, mod, dpreds, ndvi, ntli, spstack, calendar_it){
  
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

#### Tropospheric NO2 ----
print("Processing OMI NO2...")
for(y in 2018:2020){

  print(paste0("Year: ", y))

  # Prepare calendar + non-daily predictors
  calendar_it <- dplyr::filter(calendar, year(date)==y)%>%
    mutate(ndvi_date = as.integer(as.factor(floor_date(date, "quarter"))))
  ndvi <- read_rds(paste0(pathdb, "omi_imp/", y, "/ndvi.rds"))
  ntli <- read_rds(paste0(pathdb, "omi_imp/", y, "/ntli.rds"))

  # Prepare daily predictors
  omi_no2 <- read_rds(paste0(pathdb, "original/", y, "/omi_no2.rds"))
  dpreds <- c(read_rds(paste0(pathdb, "omi_imp/", y, "/2mtemp_13.rds")),
              read_rds(paste0(pathdb, "omi_imp/", y, "/10muwind_13.rds")),
              read_rds(paste0(pathdb, "omi_imp/", y, "/10mvwind_13.rds")),
              read_rds(paste0(pathdb, "omi_imp/", y, "/totlprec_13.rds")),
              read_rds(paste0(pathdb, "omi_imp/", y, "/pbh_13.rds")),
              read_rds(paste0(pathdb, "omi_imp/", y, "/NO2_12.rds")))
  dpreds <- c(omi_no2, dpreds)
  names(dpreds)[1] <- "outcome"

  # Prepare model
  mod <- read_rds(paste0(pathmod, "modomi_no2_", y, ".rds"))

  # Impute data in parallel
  imps <- map(calendar_it$yday, pred_omi,
              mod=mod, dpreds=dpreds, ndvi=ndvi, ntli=ntli, spstack=spstack,
              calendar_it=calendar_it)

  txt_parse <- paste0("c(", paste0("imps[[", 1:length(imps), "]]", collapse = ", "), ")")
  imps <- eval(parse(text=txt_parse))
  imps <- merge(imps) %>%
    st_set_dimensions(3, name="date", values=calendar_it$date)
  names(imps) <- "omi_no2"

  # Write and clean
  write_rds(imps, paste0(pathmod, "impomi_no2_", y, ".rds"))
  rm("imps", "txt_parse", "mod", "dpreds", "calendar_it", "ndvi", "ntli", "omi_no2")
}

#### Tropospheric O3 ----
print("Processing OMI O3...")
for(y in 2018:2020){
  
  print(paste0("Year: ", y))
  
  # Prepare calendar + non-daily predictors
  calendar_it <- dplyr::filter(calendar, year(date)==y)%>%
    mutate(ndvi_date = as.integer(as.factor(floor_date(date, "quarter"))))
  ndvi <- read_rds(paste0(pathdb, "omi_imp/", y, "/ndvi.rds"))
  ntli <- read_rds(paste0(pathdb, "omi_imp/", y, "/ntli.rds"))
  
  # Prepare daily predictors
  omi_o3 <- read_rds(paste0(pathdb, "original/", y, "/omi_o3.rds"))
  dpreds <- c(read_rds(paste0(pathdb, "omi_imp/", y, "/2mtemp_13.rds")),
              read_rds(paste0(pathdb, "omi_imp/", y, "/10muwind_13.rds")),
              read_rds(paste0(pathdb, "omi_imp/", y, "/10mvwind_13.rds")),
              read_rds(paste0(pathdb, "omi_imp/", y, "/totlprec_13.rds")),
              read_rds(paste0(pathdb, "omi_imp/", y, "/pbh_13.rds")),
              read_rds(paste0(pathdb, "omi_imp/", y, "/O3_12.rds")))
  dpreds <- c(omi_o3, dpreds)
  names(dpreds)[1] <- "outcome"
  
  # Prepare model
  mod <- read_rds(paste0(pathmod, "modomi_o3_", y, ".rds"))
  
  # Impute data in parallel
  imps <- map(calendar_it$yday, pred_omi,
              mod=mod, dpreds=dpreds, ndvi=ndvi, ntli=ntli, spstack=spstack,
              calendar_it=calendar_it)
  
  txt_parse <- paste0("c(", paste0("imps[[", 1:length(imps), "]]", collapse = ", "), ")")
  imps <- eval(parse(text=txt_parse))
  imps <- merge(imps) %>%
    st_set_dimensions(3, name="date", values=calendar_it$date)
  names(imps) <- "omi_o3"
  
  # Write and clean
  write_rds(imps, paste0(pathmod, "impomi_o3_", y, ".rds"))
  rm("imps", "txt_parse", "mod", "dpreds", "calendar_it", "ndvi", "ntli", "omi_o3")
}


#### Finish ----
rm(list = ls())