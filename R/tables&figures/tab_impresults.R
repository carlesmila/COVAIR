#-----------------------------------------------------------------------------#
#                       Table S1: RS imputation results                       #
#-----------------------------------------------------------------------------#

library("targets")
library("tidyverse")
library("xtable")

# Files with results
qfiles1 <- list.files("outputs/LST_imp", "modlst", full.names = T)
qfiles2 <- list.files("outputs/MAIAC_imp", "modmaiac", full.names = T)
qfiles3 <- list.files("outputs/omi_imp", "modomi", full.names = T)
qfiles4 <- list.files("outputs/tropomi_imp", "modtropomi", full.names = T)
qfiles <- map_df(c(qfiles1, qfiles2, qfiles3, qfiles4), read_csv) %>%
  pivot_wider(names_from = "method", values_from = c("rmse", "r2"))
  
# Fix labels
qfiles$model[grepl("LST", qfiles$model)] <- paste0(qfiles$model[grepl("LST", qfiles$model)], " (ºC)")
qfiles$model[grepl("no2", qfiles$model)] <- paste0(qfiles$model[grepl("no2", qfiles$model)], " (mol/cm3)")
qfiles$model[grepl("o3", qfiles$model)] <- paste0(qfiles$model[grepl("o3", qfiles$model)], " (DU)")
qfiles <- qfiles %>%
  mutate(model = gsub("aqua", "Aqua", model),
         model = gsub("terra", "Terra", model),
         model = gsub("maiac", "AOD MAIAC", model),
         model = gsub("tropomi", "TROPOMI", model),
         model = gsub("omi", "OMI", model)) %>%
  mutate_if(is.numeric, function(x) as.character(round(x, 2))) %>%
  dplyr::select(model, year, rmse_OOB, r2_OOB, rmse_test, r2_test)
qfiles$model <- ifelse(duplicated(qfiles$model), "", qfiles$model)
qfiles$model <- paste0()
names(qfiles) <- c("Model", "Year", "RMSE_OOB", "R2_OOB", "RMSE_test$", "R2_test")

print(xtable(qfiles), include.rownames=FALSE)

