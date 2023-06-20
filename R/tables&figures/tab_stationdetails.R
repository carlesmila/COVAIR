#-----------------------------------------------------------------------------#
#                         Table 1: Station details                            #
#-----------------------------------------------------------------------------#

library("targets")
library("tidyverse")
library("lme4")
library("xtable")

# Read AP station data
tar_load("ap_stations")
ap_readings <- bind_rows(read_rds(ap_stations$readings18),
                      read_rds(ap_stations$readings19),
                      read_rds(ap_stations$readings20)) %>%
  rename(exposure = pollutant,
         measure = conc) 
tapply(ap_readings$measure_type, ap_readings$exposure, function(x) round(sum(x == "manual")/sum(!is.na(x))*100, 1))
ap_readings$measure_type <- NULL

# Read meteo station data
tar_load("meteo_stations")
temp_readings <- bind_rows(read_rds(meteo_stations$readings18),
                      read_rds(meteo_stations$readings19),
                      read_rds(meteo_stations$readings20)) %>%
  select(-tmax, -tmin, -entity) %>%
  mutate(exposure = "Temperature", 
         measure_type = "automatic") %>%
  rename(measure = tavg)

# Merge
all_readings <- bind_rows(ap_readings, temp_readings) %>%
  mutate(exposure = as.factor(exposure),
         exposure = fct_relevel(exposure, c("Temperature", "PM2.5", "PM10", "NO2", "O3")))

# Table - 1st pass
expotab <- all_readings %>%
  group_by(exposure, ID) %>%
  summarise(N_measurements = n(),
            N_manual = sum(measure_type=="manual"),
            N_automatic = sum(measure_type=="automatic")) %>%
  group_by(exposure) %>%
  summarise(N_stations = sum(!duplicated(ID)),
            MSD_measurements = paste0(round(mean(N_measurements)/3, 1), " (", round(sd(N_measurements)/3, 1), ")"))

# ICC
icc_lmer <- function(data, log=F){
  if(isTRUE(log)){
    mod <- lmer(log(measure+0.01)~(1|date)+(1|ID), data = data)
  }else{
    mod <- lmer(measure~(1|date)+(1|ID), data = data)
  }
  varmod <- as.data.frame(VarCorr(mod))$sdcor^2
  varmod <- round(varmod/sum(varmod)*100, 1)[1:2]
  names(varmod) <- c("ICCtemp", "ICCspat")
  return(varmod[1:2])
}

iccs <- bind_rows(
  icc_lmer(all_readings[!is.na(all_readings$measure)&all_readings$exposure=="Temperature",]),
  icc_lmer(all_readings[!is.na(all_readings$measure)&all_readings$exposure=="PM2.5",], log = T),
  icc_lmer(all_readings[!is.na(all_readings$measure)&all_readings$exposure=="PM10",], log = T),
  icc_lmer(all_readings[!is.na(all_readings$measure)&all_readings$exposure=="NO2",], log = T),
  icc_lmer(all_readings[!is.na(all_readings$measure)&all_readings$exposure=="O3",])
)
print(xtable(expotab), include.rownames=FALSE)

