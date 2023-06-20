#-----------------------------------------------------------------------------#
#                            RS imputation maps                               #
#-----------------------------------------------------------------------------#

library("targets")
library("tidyverse")
library("sf")
library("stars")
library("viridis")
library("colorspace")
library("gridExtra")

# Day
yday <- 98 
as.Date("2019-01-01") + yday -1
tar_load("cat")
catbound <- read_rds(cat)

# LST terra day ----
original <- read_rds("database/original/2019/lst_terra.rds")["dayLST_terra"] 
imputed <- read_rds("outputs/LST_imp/implst_terraday_2019.rds")
plotdata <- c(original[,,,yday], imputed[,,,yday], along=4) %>%
  st_set_dimensions(4, names="product", values = c("Original", "Imputed"))
midpoint1 <- min(plotdata$dayLST_terra, na.rm=T) + (max(plotdata$dayLST_terra, na.rm=T)- min(plotdata$dayLST_terra, na.rm=T))/2
p1 <- ggplot() +
  geom_stars(data=plotdata) +
  facet_wrap(~ product) +
  scale_fill_continuous_diverging("Blue-Red 3", na.value = "#FFFFFF00", mid = midpoint1) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), aspect.ratio=1,
        panel.grid=element_blank(), title = element_text(size = 9)) +
  labs(x="", y="", fill = "LST (ºC)", title = "Land Surface Temperature - MODIS Terra day") 
rm("original", "imputed", "plotdata")

# LST aqua day ----
original <- read_rds("database/original/2019/lst_aqua.rds")["dayLST_aqua"] 
imputed <- read_rds("outputs/LST_imp/implst_aquaday_2019.rds")
plotdata <- c(original[,,,yday], imputed[,,,yday], along=4) %>%
  st_set_dimensions(4, names="product", values = c("Original", "Imputed"))
midpoint2 <- min(plotdata$dayLST_aqua, na.rm=T) + (max(plotdata$dayLST_aqua, na.rm=T)- min(plotdata$dayLST_aqua, na.rm=T))/2
p2 <- ggplot() +
  geom_stars(data=plotdata) +
  facet_wrap(~ product) +
  scale_fill_continuous_diverging("Blue-Red 3", na.value = "#FFFFFF00", mid = midpoint2) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), aspect.ratio=1,
        panel.grid=element_blank(), title = element_text(size = 9)) +
  labs(x="", y="", fill = "LST (ºC)", title = "Land Surface Temperature - MODIS Aqua day") 
rm("original", "imputed", "plotdata")

# LST terra night ----
original <- read_rds("database/original/2019/lst_terra.rds")["nightLST_terra"] 
imputed <- read_rds("outputs/LST_imp/implst_terranight_2019.rds")
plotdata <- c(original[,,,yday], imputed[,,,yday], along=4) %>%
  st_set_dimensions(4, names="product", values = c("Original", "Imputed"))
midpoint3 <- min(plotdata$nightLST_terra, na.rm=T) + (max(plotdata$nightLST_terra, na.rm=T)- min(plotdata$nightLST_terra, na.rm=T))/2
p3 <- ggplot() +
  geom_stars(data=plotdata) +
  facet_wrap(~ product) +
  scale_fill_continuous_diverging("Blue-Red 3", na.value = "#FFFFFF00", mid = midpoint3) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), aspect.ratio=1,
        panel.grid=element_blank(), title = element_text(size = 9)) +
  labs(x="", y="", fill = "LST (ºC)", title = "Land Surface Temperature - MODIS Terra night") 
rm("original", "imputed", "plotdata")

# LST aqua night ----
original <- read_rds("database/original/2019/lst_aqua.rds")["nightLST_aqua"] 
imputed <- read_rds("outputs/LST_imp/implst_aquanight_2019.rds")
plotdata <- c(original[,,,yday], imputed[,,,yday], along=4) %>%
  st_set_dimensions(4, names="product", values = c("Original", "Imputed"))
midpoint4 <- min(plotdata$nightLST_aqua, na.rm=T) + (max(plotdata$nightLST_aqua, na.rm=T)- min(plotdata$nightLST_aqua, na.rm=T))/2
p4 <- ggplot() +
  geom_stars(data=plotdata) +
  facet_wrap(~ product) +
  scale_fill_continuous_diverging("Blue-Red 3", na.value = "#FFFFFF00", mid = midpoint4) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), aspect.ratio=1,
        panel.grid=element_blank(), title = element_text(size = 9)) +
  labs(x="", y="", fill = "LST (ºC)", title = "Land Surface Temperature - MODIS Aqua night") 
rm("original", "imputed", "plotdata")

# AOD terra ----
original <- read_rds("database/original/2019/maiac.rds")["AOD55_terra"] 
imputed <- read_rds("outputs/MAIAC_imp/impmaiac_terra_2019.rds")
plotdata <- c(original[,,,yday], imputed[,,,yday], along=4) %>%
  st_set_dimensions(4, names="product", values = c("Original", "Imputed"))
p5 <- ggplot() +
  geom_stars(data=plotdata) +
  facet_wrap(~ product) +
  scale_fill_viridis(option = "E", na.value = "#FFFFFF00") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), aspect.ratio=1,
        panel.grid=element_blank(), title = element_text(size = 9)) +
  labs(x="", y="", fill = "AOD", title = "MAIAC Aerosol Optical Depth - MODIS Terra") 
rm("original", "imputed", "plotdata")

# AOD aqua ----
original <- read_rds("database/original/2019/maiac.rds")["AOD55_aqua"] 
imputed <- read_rds("outputs/MAIAC_imp/impmaiac_aqua_2019.rds")
plotdata <- c(original[,,,yday], imputed[,,,yday], along=4) %>%
  st_set_dimensions(4, names="product", values = c("Original", "Imputed"))
p6 <- ggplot() +
  geom_stars(data=plotdata) +
  facet_wrap(~ product) +
  scale_fill_viridis(option = "E", na.value = "#FFFFFF00") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), aspect.ratio=1,
        panel.grid=element_blank(), title = element_text(size = 9)) +
  labs(x="", y="", fill = "AOD", title = "MAIAC Aerosol Optical Depth - MODIS Aqua") 
rm("original", "imputed", "plotdata")

# OMI NO2 ----
original <- read_rds("database/original/2019/omi_no2.rds") 
imputed <- read_rds("outputs/omi_imp/impomi_no2_2019.rds")
plotdata <- c(original[,,,yday], imputed[,,,yday], along=4) %>%
  st_set_dimensions(4, names="product", values = c("Original", "Imputed"))
plotdata <- st_crop(plotdata, catbound)
p7 <- ggplot() +
  geom_stars(data=plotdata) +
  facet_wrap(~ product) +
  scale_fill_viridis(option = "E", na.value = "#FFFFFF00") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), aspect.ratio=1,
        panel.grid=element_blank(), title = element_text(size = 9)) +
  labs(x="", y="", fill = expression("NO"[2]~"(mol/cm"^2*")"), title = expression("Tropospheric NO"[2]*" column - OMI Aura"))
rm("original", "imputed", "plotdata")

# OMI O3 ----
original <- read_rds("database/original/2019/omi_o3.rds") 
imputed <- read_rds("outputs/omi_imp/impomi_o3_2019.rds")
plotdata <- c(original[,,,yday], imputed[,,,yday], along=4) %>%
  st_set_dimensions(4, names="product", values = c("Original", "Imputed"))
plotdata <- st_crop(plotdata, catbound)
p8 <- ggplot() +
  geom_stars(data=plotdata) +
  facet_wrap(~ product) +
  scale_fill_viridis(option = "E", na.value = "#FFFFFF00") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), aspect.ratio=1,
        panel.grid=element_blank(), title = element_text(size = 9)) +
  labs(x="", y="", fill = expression("O"[3]~"(DU)"), title = expression("Total O"[3]*" column - OMI Aura"))
rm("original", "imputed", "plotdata")

# TROPOMI NO2 ----
original <- read_rds("database/original/2019/tropomi_no2.rds") 
imputed <- read_rds("outputs/tropomi_imp/imptropomi_no2_2019.rds")
plotdata <- c(original[,,,yday], imputed[,,,yday], along=4) %>%
  st_set_dimensions(4, names="product", values = c("Original", "Imputed"))
p9 <- ggplot() +
  geom_stars(data=plotdata) +
  facet_wrap(~ product) +
  scale_fill_viridis(option = "E", na.value = "#FFFFFF00") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), aspect.ratio=1,
        panel.grid=element_blank(), title = element_text(size = 9)) +
  labs(x="", y="", fill = expression("NO"[2]~"(mol/cm"^2*")"), title = expression("Tropospheric NO"[2]*" column - TROPOMI S5p"))
rm("original", "imputed", "plotdata")

# TROPOMI O3 ----
original <- read_rds("database/original/2019/tropomi_o3.rds") 
imputed <- read_rds("outputs/tropomi_imp/imptropomi_o3_2019.rds")
plotdata <- c(original[,,,yday], imputed[,,,yday], along=4) %>%
  st_set_dimensions(4, names="product", values = c("Original", "Imputed"))
p10 <- ggplot() +
  geom_stars(data=plotdata) +
  facet_wrap(~ product) +
  scale_fill_viridis(option = "E", na.value = "#FFFFFF00") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), aspect.ratio=1,
        panel.grid=element_blank(), title = element_text(size = 9)) +
  labs(x="", y="", fill = expression("O"[3]~"(DU)"), title = expression("Total O"[3]*" column - TROPOMI S5p"))
rm("original", "imputed", "plotdata")

# composite ----
composite <- arrangeGrob(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, ncol = 2)
ggsave(composite, filename = "figures/manuscript/RSimpmaps.png", width = 7, height = 10, dpi = 300)
