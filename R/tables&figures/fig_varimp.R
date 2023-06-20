#-----------------------------------------------------------------------------#
#                         Variable importance figure                          #
#-----------------------------------------------------------------------------#

library("tidyverse")
library("gridExtra")

# temp ----
varimp <- read_csv("outputs/expo_mod2/temp_varimp.csv") 
names(varimp) <- c("Variable", "raw", "Importance")
varimp <- arrange(varimp, Importance) %>%
  mutate(Variable = gsub("maiac", "MAIAC", Variable),
         Variable = gsub("lst", "LST", Variable),
         Variable = gsub("tropomi", "TROPOMI", Variable),
         Variable = gsub("omi", "OMI", Variable),
         Variable = gsub("ndvi", "S2_NDVI", Variable),
         Variable = gsub("ntli", "VIIRS_NTLI", Variable),
         Variable = gsub("imd", "impervious", Variable),
         Variable = gsub("tcd", "treecover", Variable),
         Variable = gsub("primary_dens_focal", "primary roads", Variable),
         Variable = gsub("local_dens_focal", "secondary roads", Variable),
         Variable = gsub("popu_dens", "pop. dens.", Variable),
         Variable = gsub("PM2.5_pemisdist", "ind. sources PM2.5", Variable),
         Variable = gsub("PM10_pemisdist", "ind. sources PM10", Variable),
         Variable = gsub("NOx_pemisdist", "ind. sources NOx", Variable),
         Variable = gsub("coast_dist", "dist. to coast", Variable),
         Variable = gsub("anight", "a, night", Variable),
         Variable = gsub("aday", "a, day", Variable),
         Variable = gsub("dem", "elevation", Variable),
         Variable = gsub("pm25", "PM2.5", Variable),
         Variable = gsub("pm10", "PM10", Variable),
         Variable = gsub("o3", "O3", Variable),
         Variable = gsub("no2", "NO2", Variable),
         Variable = gsub("pbh", "BLH", Variable),
         Variable = gsub("julian", "julian day", Variable),
         Variable = gsub("dust", "dust day", Variable),
         Variable = gsub("holiday", "holiday day", Variable),
         Variable = gsub("focal", "(focal)", Variable),
         Variable = gsub("_", " ", Variable),
         Variable = fct_inorder(Variable)) %>%
  arrange(-Importance) %>%
  .[1:10,]
temp <- ggplot(varimp, aes(x=Variable, y=Importance)) +
  geom_hline(aes(yintercept = 0), size = 0.5, color = "grey50") +
  geom_col(col = "black", width = 0.5) +
  ggtitle("Temperature") +
  theme_light() + 
  ylab("Variable importance") + xlab("") +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(), 
    legend.position = "bottom")

# pm25 ----
varimp <- read_csv("outputs/expo_mod2/pm25_varimp.csv") 
names(varimp) <- c("Variable", "raw", "Importance")
varimp <- arrange(varimp, Importance) %>%
  mutate(Variable = gsub("maiac", "MAIAC", Variable),
         Variable = gsub("lst", "LST", Variable),
         Variable = gsub("tropomi", "TROPOMI", Variable),
         Variable = gsub("omi", "OMI", Variable),
         Variable = gsub("ndvi", "S2_NDVI", Variable),
         Variable = gsub("ntli", "VIIRS_NTLI", Variable),
         Variable = gsub("imd", "impervious", Variable),
         Variable = gsub("tcd", "treecover", Variable),
         Variable = gsub("primary_dens_focal", "primary roads", Variable),
         Variable = gsub("local_dens_focal", "secondary roads", Variable),
         Variable = gsub("popu_dens", "pop. dens.", Variable),
         Variable = gsub("PM2.5_pemisdist", "ind. sources PM2.5", Variable),
         Variable = gsub("PM10_pemisdist", "ind. sources PM10", Variable),
         Variable = gsub("NOx_pemisdist", "ind. sources NOx", Variable),
         Variable = gsub("coast_dist", "dist. to coast", Variable),
         Variable = gsub("anight", "a, night", Variable),
         Variable = gsub("aday", "a, day", Variable),
         Variable = gsub("dem", "elevation", Variable),
         Variable = gsub("pm25", "PM2.5", Variable),
         Variable = gsub("pm10", "PM10", Variable),
         Variable = gsub("o3", "O3", Variable),
         Variable = gsub("no2", "NO2", Variable),
         Variable = gsub("pbh", "BLH", Variable),
         Variable = gsub("julian", "julian day", Variable),
         Variable = gsub("dust", "dust day", Variable),
         Variable = gsub("holiday", "holiday day", Variable),
         Variable = gsub("focal", "(focal)", Variable),
         Variable = gsub("_", " ", Variable),
         Variable = fct_inorder(Variable)) %>%
  arrange(-Importance) %>%
  .[1:10,]
pm25 <- ggplot(varimp, aes(x=Variable, y=Importance)) +
  geom_hline(aes(yintercept = 0), size = 0.5, color = "grey50") +
  geom_col(col = "black", width = 0.5) +
  ggtitle(expression("PM"[2.5])) +
  theme_light() + 
  ylab("Variable importance") + xlab("") +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(), 
    legend.position = "bottom")

# pm10 ----
varimp <- read_csv("outputs/expo_mod2/pm10_varimp.csv") 
names(varimp) <- c("Variable", "raw", "Importance")
varimp <- arrange(varimp, Importance) %>%
  mutate(Variable = gsub("maiac", "MAIAC", Variable),
         Variable = gsub("lst", "LST", Variable),
         Variable = gsub("tropomi", "TROPOMI", Variable),
         Variable = gsub("omi", "OMI", Variable),
         Variable = gsub("ndvi", "S2_NDVI", Variable),
         Variable = gsub("ntli", "VIIRS_NTLI", Variable),
         Variable = gsub("imd", "impervious", Variable),
         Variable = gsub("tcd", "treecover", Variable),
         Variable = gsub("primary_dens_focal", "primary roads", Variable),
         Variable = gsub("local_dens_focal", "secondary roads", Variable),
         Variable = gsub("popu_dens", "pop. dens.", Variable),
         Variable = gsub("PM2.5_pemisdist", "ind. sources PM2.5", Variable),
         Variable = gsub("PM10_pemisdist", "ind. sources PM10", Variable),
         Variable = gsub("NOx_pemisdist", "ind. sources NOx", Variable),
         Variable = gsub("coast_dist", "dist. to coast", Variable),
         Variable = gsub("anight", "a, night", Variable),
         Variable = gsub("aday", "a, day", Variable),
         Variable = gsub("dem", "elevation", Variable),
         Variable = gsub("pm25", "PM2.5", Variable),
         Variable = gsub("pm10", "PM10", Variable),
         Variable = gsub("o3", "O3", Variable),
         Variable = gsub("no2", "NO2", Variable),
         Variable = gsub("pbh", "BLH", Variable),
         Variable = gsub("julian", "julian day", Variable),
         Variable = gsub("dust", "dust day", Variable),
         Variable = gsub("holiday", "holiday day", Variable),
         Variable = gsub("focal", "(focal)", Variable),
         Variable = gsub("_", " ", Variable),
         Variable = fct_inorder(Variable)) %>%
  arrange(-Importance) %>%
  .[1:10,]
pm10 <- ggplot(varimp, aes(x=Variable, y=Importance)) +
  geom_hline(aes(yintercept = 0), size = 0.5, color = "grey50") +
  geom_col(col = "black", width = 0.5) +
  ggtitle(expression("PM"[10])) +
  theme_light() + 
  ylab("Variable importance") + xlab("") +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(), 
    legend.position = "bottom")

# NO2 ----
varimp <- read_csv("outputs/expo_mod2/no2_varimp.csv") 
names(varimp) <- c("Variable", "raw", "Importance")
varimp <- arrange(varimp, Importance) %>%
  mutate(Variable = gsub("maiac", "MAIAC", Variable),
         Variable = gsub("lst", "LST", Variable),
         Variable = gsub("tropomi", "TROPOMI", Variable),
         Variable = gsub("omi", "OMI", Variable),
         Variable = gsub("ndvi", "S2_NDVI", Variable),
         Variable = gsub("ntli", "VIIRS_NTLI", Variable),
         Variable = gsub("imd", "impervious", Variable),
         Variable = gsub("tcd", "treecover", Variable),
         Variable = gsub("primary_dens_focal", "primary roads", Variable),
         Variable = gsub("local_dens_focal", "secondary roads", Variable),
         Variable = gsub("popu_dens", "pop. dens.", Variable),
         Variable = gsub("PM2.5_pemisdist", "ind. sources PM2.5", Variable),
         Variable = gsub("PM10_pemisdist", "ind. sources PM10", Variable),
         Variable = gsub("NOx_pemisdist", "ind. sources NOx", Variable),
         Variable = gsub("coast_dist", "dist. to coast", Variable),
         Variable = gsub("anight", "a, night", Variable),
         Variable = gsub("aday", "a, day", Variable),
         Variable = gsub("dem", "elevation", Variable),
         Variable = gsub("pm25", "PM2.5", Variable),
         Variable = gsub("pm10", "PM10", Variable),
         Variable = gsub("o3", "O3", Variable),
         Variable = gsub("no2", "NO2", Variable),
         Variable = gsub("pbh", "BLH", Variable),
         Variable = gsub("julian", "julian day", Variable),
         Variable = gsub("dust", "dust day", Variable),
         Variable = gsub("holiday", "holiday day", Variable),
         Variable = gsub("focal", "(focal)", Variable),
         Variable = gsub("_", " ", Variable),
         Variable = fct_inorder(Variable)) %>%
  arrange(-Importance) %>%
  .[1:10,]
no2 <- ggplot(varimp, aes(x=Variable, y=Importance)) +
  geom_hline(aes(yintercept = 0), size = 0.5, color = "grey50") +
  geom_col(col = "black", width = 0.5) +
  ggtitle(expression("NO"[2])) +
  theme_light() + 
  ylab("Variable importance") + xlab("") +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(), 
    legend.position = "bottom")

# o3 ----
varimp <- read_csv("outputs/expo_mod2/o3_varimp.csv") 
names(varimp) <- c("Variable", "raw", "Importance")
varimp <- arrange(varimp, Importance) %>%
  mutate(Variable = gsub("maiac", "MAIAC", Variable),
         Variable = gsub("lst", "LST", Variable),
         Variable = gsub("tropomi", "TROPOMI", Variable),
         Variable = gsub("omi", "OMI", Variable),
         Variable = gsub("ndvi", "S2_NDVI", Variable),
         Variable = gsub("ntli", "VIIRS_NTL", Variable),
         Variable = gsub("imd", "impervious", Variable),
         Variable = gsub("tcd", "treecover", Variable),
         Variable = gsub("primary_dens", "primary roads", Variable),
         Variable = gsub("local_dens", "secondary roads", Variable),
         Variable = gsub("popu_dens", "pop. dens.", Variable),
         Variable = gsub("PM2.5_pemisdist", "ind. sources PM2.5", Variable),
         Variable = gsub("PM10_pemisdist", "ind. sources PM10", Variable),
         Variable = gsub("NOx_pemisdist", "ind. sources NOx", Variable),
         Variable = gsub("coast_dist", "dist. to coast", Variable),
         Variable = gsub("anight", "a, night", Variable),
         Variable = gsub("aday", "a, day", Variable),
         Variable = gsub("dem", "elevation", Variable),
         Variable = gsub("pm25", "PM2.5", Variable),
         Variable = gsub("pm10", "PM10", Variable),
         Variable = gsub("o3", "O3", Variable),
         Variable = gsub("no2", "NO2", Variable),
         Variable = gsub("pbh", "BLH", Variable),
         Variable = gsub("julian", "julian day", Variable),
         Variable = gsub("dust", "dust day", Variable),
         Variable = gsub("holiday", "holiday day", Variable),
         Variable = gsub("focal", "(focal)", Variable),
         Variable = gsub("_", " ", Variable),
         Variable = fct_inorder(Variable)) %>%
  arrange(-Importance) %>%
  .[1:10,]
o3 <- ggplot(varimp, aes(x=Variable, y=Importance)) +
  geom_hline(aes(yintercept = 0), size = 0.5, color = "grey50") +
  geom_col(col = "black", width = 0.5) +
  ggtitle(expression("O"[3])) +
  theme_light() + 
  ylab("Variable importance") + xlab("") +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(), 
    legend.position = "bottom")

# Composite ----
varimp <- grid.arrange(temp, pm25, pm10, no2, o3, ncol = 3)
ggsave("figures/manuscript/varimp.png", varimp, dpi = 300, width = 9, height = 6)
