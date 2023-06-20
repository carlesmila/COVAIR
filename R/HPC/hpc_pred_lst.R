#-----------------------------------------------------------------------------#
#               Cluster RS prediction: LST day/night terra and aqua           #
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
aspect <- read_rds(paste0(pathdb, "modis_imp/spatial/aspect.rds"))
spstack <- c(spstack, aspect)
calendar <- read_rds(paste0(pathdb, "original/calendar.rds")) %>%
  dplyr::select(date, yday) 

# Function to predict LST
pred_lst <- function(d, mod, dpreds, ndvi, ntli, spstack, calendar_it){
  
  # Print day
  print(paste0("Processing day: ", d))
  
  # Add variables to the layer
  pdata <- adrop(dpreds[,,,d])
  pdata$x <- st_coordinates(pdata)[,1]
  pdata$y <- st_coordinates(pdata)[,2]
  pdata$yday <- d
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

#### terra day ----
print("Processing terra day...")
for(y in 2018:2020){

  print(paste0("Year: ", y))

  # Prepare calendar + non-daily predictors
  calendar_it <- dplyr::filter(calendar, year(date)==y)%>%
    mutate(ndvi_date = as.integer(as.factor(floor_date(date, "quarter"))))
  ndvi <- read_rds(paste0(pathdb, "modis_imp/", y, "/ndvi.rds"))
  ntli <- read_rds(paste0(pathdb, "modis_imp/", y, "/ntli.rds"))

  # Prepare daily predictors
  dpreds <- c(read_rds(paste0(pathdb, "original/", y, "/lst_terra.rds"))["dayLST_terra"],
              read_rds(paste0(pathdb, "modis_imp/", y, "/skintemp_10.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/10muwind_10.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/10mvwind_10.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/totlprec_10.rds")))
  names(dpreds)[1] <- "outcome"

  # Prepare model
  mod <- read_rds(paste0(pathmod, "modlst_terraday_", y, ".rds"))

  # Impute data in parallel
  imps <- map(calendar_it$yday, pred_lst,
              mod=mod, dpreds=dpreds, ndvi=ndvi, ntli=ntli, spstack=spstack,
              calendar_it=calendar_it)

  txt_parse <- paste0("c(", paste0("imps[[", 1:length(imps), "]]", collapse = ", "), ")")
  imps <- eval(parse(text=txt_parse))
  imps <- merge(imps) %>%
    st_set_dimensions(3, name="date", values=calendar_it$date)
  names(imps) <- "dayLST_terra"

  # Write and clean
  write_rds(imps, paste0(pathmod, "implst_terraday_", y, ".rds"))
  rm("imps", "txt_parse", "mod", "dpreds", "calendar_it", "ndvi", "ntli")
}

#### terra night ----
print("Processing terra night...")
for(y in 2018:2020){

  print(paste0("Year: ", y))

  # Prepare calendar + non-daily predictors
  calendar_it <- dplyr::filter(calendar, year(date)==y)%>%
    mutate(ndvi_date = as.integer(as.factor(floor_date(date, "quarter"))))
  ndvi <- read_rds(paste0(pathdb, "modis_imp/", y, "/ndvi.rds"))
  ntli <- read_rds(paste0(pathdb, "modis_imp/", y, "/ntli.rds"))

  # Prepare daily predictors
  dpreds <- c(read_rds(paste0(pathdb, "original/", y, "/lst_terra.rds"))["nightLST_terra"],
              read_rds(paste0(pathdb, "modis_imp/", y, "/skintemp_22.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/10muwind_22.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/10mvwind_22.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/totlprec_22.rds")))
  names(dpreds)[1] <- "outcome"

  # Prepare model
  mod <- read_rds(paste0(pathmod, "modlst_terranight_", y, ".rds"))

  # Impute data in parallel
  imps <- map(calendar_it$yday, pred_lst,
                     mod=mod, dpreds=dpreds, ndvi=ndvi, ntli=ntli, spstack=spstack,
                     calendar_it=calendar_it)

  txt_parse <- paste0("c(", paste0("imps[[", 1:length(imps), "]]", collapse = ", "), ")")
  imps <- eval(parse(text=txt_parse))
  imps <- merge(imps) %>%
    st_set_dimensions(3, name="date", values=calendar_it$date)
  names(imps) <- "nightLST_terra"

  # Write and clean
  write_rds(imps, paste0(pathmod, "implst_terranight_", y, ".rds"))
  rm("imps", "txt_parse", "mod", "dpreds", "calendar_it", "ndvi", "ntli")
}

#### aqua day ----
print("Processing aqua day...")
for(y in 2018:2020){

  print(paste0("Year: ", y))

  # Prepare calendar + non-daily predictors
  calendar_it <- dplyr::filter(calendar, year(date)==y)%>%
    mutate(ndvi_date = as.integer(as.factor(floor_date(date, "quarter"))))
  ndvi <- read_rds(paste0(pathdb, "modis_imp/", y, "/ndvi.rds"))
  ntli <- read_rds(paste0(pathdb, "modis_imp/", y, "/ntli.rds"))

  # Prepare daily predictors
  dpreds <- c(read_rds(paste0(pathdb, "original/", y, "/lst_aqua.rds"))["dayLST_aqua"],
              read_rds(paste0(pathdb, "modis_imp/", y, "/skintemp_13.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/10muwind_13.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/10mvwind_13.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/totlprec_13.rds")))
  names(dpreds)[1] <- "outcome"

  # Prepare model
  mod <- read_rds(paste0(pathmod, "modlst_aquaday_", y, ".rds"))

  # Impute data in parallel
  imps <- map(calendar_it$yday, pred_lst,
                     mod=mod, dpreds=dpreds, ndvi=ndvi, ntli=ntli, spstack=spstack,
                     calendar_it=calendar_it)

  txt_parse <- paste0("c(", paste0("imps[[", 1:length(imps), "]]", collapse = ", "), ")")
  imps <- eval(parse(text=txt_parse))
  imps <- merge(imps) %>%
    st_set_dimensions(3, name="date", values=calendar_it$date)
  names(imps) <- "dayLST_aqua"

  # Write and clean
  write_rds(imps, paste0(pathmod, "implst_aquaday_", y, ".rds"))
  rm("imps", "txt_parse", "mod", "dpreds", "calendar_it", "ndvi", "ntli")
}

#### aqua night ----
print("Processing aqua night...")
for(y in 2018:2020){

  print(paste0("Year: ", y))

  # Prepare calendar + non-daily predictors
  calendar_it <- dplyr::filter(calendar, year(date)==y)%>%
    mutate(ndvi_date = as.integer(as.factor(floor_date(date, "quarter"))))
  ndvi <- read_rds(paste0(pathdb, "modis_imp/", y, "/ndvi.rds"))
  ntli <- read_rds(paste0(pathdb, "modis_imp/", y, "/ntli.rds"))

  # Prepare daily predictors
  dpreds <- c(read_rds(paste0(pathdb, "original/", y, "/lst_aqua.rds"))["nightLST_aqua"],
              read_rds(paste0(pathdb, "modis_imp/", y, "/skintemp_01.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/10muwind_01.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/10mvwind_01.rds")),
              read_rds(paste0(pathdb, "modis_imp/", y, "/totlprec_01.rds")))
  names(dpreds)[1] <- "outcome"

  # Prepare model
  mod <- read_rds(paste0(pathmod, "modlst_aquanight_", y, ".rds"))

  # Impute data in parallel
  imps <- map(calendar_it$yday, pred_lst,
                     mod=mod, dpreds=dpreds, ndvi=ndvi, ntli=ntli, spstack=spstack,
                     calendar_it=calendar_it)

  txt_parse <- paste0("c(", paste0("imps[[", 1:length(imps), "]]", collapse = ", "), ")")
  imps <- eval(parse(text=txt_parse))
  imps <- merge(imps) %>%
    st_set_dimensions(3, name="date", values=calendar_it$date)
  names(imps) <- "nightLST_aqua"

  # Write and clean
  write_rds(imps, paste0(pathmod, "implst_aquanight_", y, ".rds"))
  rm("imps", "txt_parse", "mod", "dpreds", "calendar_it", "ndvi", "ntli")
}

#### Finish ----
rm(list = ls())