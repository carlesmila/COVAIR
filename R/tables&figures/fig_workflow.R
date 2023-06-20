#-----------------------------------------------------------------------------#
#                             Workflow subfigures                             #
#-----------------------------------------------------------------------------#

library("targets")
library("stars")
library("tidyverse")
library("sf")
library("viridis")
library("tmap")
library("gridExtra")

# Original remote sensing data
tar_load("lst_terra")
lst_terra <- read_rds(lst_terra[[1]])
lst_terra <- lst_terra[1,,,1:4]
imputed <- read_rds("outputs/LST_imp/implst_terraday_2018.rds")[1,,,1]
imputed <- mutate(imputed, dayLST_terra = ifelse(!is.na(dayLST_terra), 0, NA))
breaks <- c(-20, -10, 0, 5, 10, 15, 22)
par(bg=NA)
for(i in 1:4){
  m1 <- tm_shape(imputed) +
    tm_raster(palette = "white", midpoint = NA) +
  tm_shape(lst_terra[,,,i]) +
    tm_raster(palette = hcl.colors(6, "Blue-Red 3"), 
              style = "fixed", breaks = breaks, midpoint = NA) +
    tm_layout(frame=FALSE, legend.show=FALSE, bg.color = "transparent")
  tmap_save(m1, filename = paste0("figures/manuscript/workflow1_",i,".png"), 
            width = 4, height = 4, bg="transparent") 
  rm("m1")
}
par(bg="white")
rm(list = ls())

# Temporal predictors
tar_load("calendar")
calendar <- read_rds(calendar) %>%
  filter(date < as.Date("2018-02-01"))
m2_1 <- ggplot(calendar) +
  geom_tile(aes(x=date, y=I(1), fill = holiday)) +
  scale_x_date(date_breaks = "1 week", date_labels =  "%Y-%m-%d") +
  scale_fill_viridis_d(end = 0.75) +
  theme_minimal() + ylab("") + xlab("") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_text(size = 12)) +
  coord_flip() 
m2_2 <- ggplot(calendar) +
  geom_tile(aes(x=date, y=I(1), fill = dust)) +
  scale_x_date(date_breaks = "1 week", date_labels =  "%Y-%m-%d") +
  scale_fill_viridis_d(end = 0.75) +
  theme_minimal() + ylab("") + xlab("") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(fill = "Event") +
  coord_flip() 
m2 <- grid.arrange(m2_1, m2_2, nrow = 1)
ggsave("figures/manuscript/workflow2.png", m2, width = 4, height = 4)

# Spatial predictors
tar_load("spstack")
spstack <- read_rds(spstack)["dem"]
m3 <- tm_shape(spstack) + 
  tm_raster(palette = terrain.colors(6)) +
  tm_layout(frame=FALSE, legend.show=FALSE, bg.color = "transparent")
tmap_save(m3, filename = paste0("figures/manuscript/workflow3.png"), 
          width = 4, height = 4, bg="transparent") 

# Spatiotemporal predictors
tar_load("era5land")
era5land <- read_rds(era5land$era5land18)["ERA5Land_10muwind",,,1:4]
tar_load("cat")
cat <- read_rds(cat)
era5land <- st_crop(era5land, cat)
breaks <- 0:8
par(bg=NA)
for(i in 1:4){
  m4 <- tm_shape(era5land[,,,i]) + 
    tm_raster(palette = hcl.colors(9, "cividis"), 
              style = "fixed", breaks = breaks, midpoint = NA) +
    tm_layout(frame=FALSE, legend.show=FALSE, bg.color = "transparent")
  tmap_save(m4, filename = paste0("figures/manuscript/workflow4_",i,".png"), 
            width = 4, height = 4, bg="transparent") 
  rm("m4")
}
par(bg="white")
dev.off()
rm(list = ls())

m4

# Imputed remote sensing data
imputed <- read_rds("outputs/LST_imp/implst_terraday_2018.rds")[1,,,1:4]
breaks <- c(-20, -10, 0, 5, 10, 15, 22)
par(bg=NA)
for(i in 1:4){
  m5 <- tm_shape(imputed[,,,i]) + 
    tm_raster(palette = hcl.colors(6, "Blue-Red 3"), 
              style = "fixed", breaks = breaks, midpoint = NA) +
    tm_layout(frame=FALSE, legend.show=FALSE, bg.color = "transparent")
  tmap_save(m5, filename = paste0("figures/manuscript/workflow5_",i,".png"), 
            width = 4, height = 4, bg="transparent") 
  rm("m5")
}
par(bg="white")
dev.off()
rm(list = ls())

# Monitoring station data
tar_load("meteo_stations")
meteo_stations <- read_rds(meteo_stations$stations) %>%
  mutate(exposure = "Temperature~stations") %>%
  dplyr::select(exposure)
tar_load("cat")
cat <- read_rds(cat)
m6_1 <- ggplot() +
  geom_sf(data=cat, fill="grey", alpha = 0.2, size = 0.1) +
  geom_sf(data=meteo_stations, size = 0.4, color = "black") +
  theme_bw() + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid = element_blank(), panel.border = element_blank())
ggsave("figures/manuscript/workflow6_1.png", plot = m6_1, width = 5, height = 5)
tar_load("meteo_stations")
meteo_readings <- read_rds(meteo_stations$readings18) %>%
  filter(date < as.Date("2018-01-31"))
m6_2 <- ggplot(meteo_readings) +
  geom_line(aes(x = date, y = tavg, col = ID, group = ID), alpha = 0.3) +
  ylab("Average temperature (ºC)") + xlab("Date") +
  scale_x_date(date_breaks = "1 week", date_labels =  "%Y-%m-%d") +
  theme_bw() +
  theme(legend.position = "none")
ggsave("figures/manuscript/workflow6_2.png", plot = m6_2, width = 7.5, height = 2.5)
rm(list = ls())

# GAM imputation
apdata <- read_csv("database/ap_pred/apdata.csv", guess_max = 10000) %>%
  dplyr::select(ID, PM10, PM2.5, measure_PM2.5, measure_PM10)  %>%
  dplyr::filter(date >= as.Date("2018-04-30") & (!is.na(PM10)|!is.na(PM2.5))) # No TROPOMI
print("Number of stations: ")
print(length(unique(apdata$ID))) 

# PM2.5 imputation ----
set.seed(123)
apdata_imp <- apdata
impdata <- apdata_imp[complete.cases(apdata_imp),] %>%
  dplyr::filter(measure_PM2.5 == measure_PM10)
m7 <- ggplot(impdata, aes(y = PM2.5, x = PM10)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "gam", col = "tomato") +
  xlim(0, 75) + ylim(0, 60) +
  ylab("PM\U02082.\U02085 (\u03bcg/m\U000b3)") +
  xlab("PM\U02081\U02080 (\u03bcg/m\U000b3)") +
  coord_equal() +
  theme_bw()
ggsave("figures/manuscript/workflow7.png", plot = m7, width = 5, height = 4)
rm(list = ls())

# 10-fold station nested CV
tar_load("meteo_stations")
meteo_stations <- read_rds(meteo_stations$stations) %>%
  mutate(exposure = "Temperature~stations") %>%
  dplyr::select(exposure)
meteo_stations$fold <- sample(1:10, size = nrow(meteo_stations), replace = T)
meteo_stations$fold <- ifelse(meteo_stations$fold == 1, "Test", "Train")
tar_load("cat")
cat <- read_rds(cat)
m10 <- ggplot() +
  geom_sf(data=cat, fill="grey", alpha = 0.2, size = 0.1) +
  geom_sf(data=meteo_stations, size = 0.4, aes(color = fold)) +
  scale_colour_viridis_d(end = 0.75) +
  theme_bw() + 
  labs(col = "") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid = element_blank(), panel.border = element_blank())
ggsave("figures/manuscript/workflow10.png", plot = m10, width = 4, height = 4)
rm(list = ls())

# Predicted exposures and SEs
temp <- read_ncdf("outputs/expo_pred2/2020/temp_2020_01.nc")[,,,1:4]
st_crs(temp) <- 25831 
breaks <- c(-10, 0, 2.5, 5, 7.5, 9, 15)
par(bg=NA)
for(i in 1:4){
  m11 <- tm_shape(temp[1,,,i]) +
    tm_raster(palette = hcl.colors(6, "Blue-Red 3"), 
              style = "fixed", breaks = breaks, midpoint = NA) +
    tm_layout(frame=FALSE, legend.show=FALSE, bg.color = "transparent")
  tmap_save(m11, filename = paste0("figures/manuscript/workflow11_",i,".png"), 
            width = 4, height = 4, bg="transparent") 
  rm("m11")
}
breaks <- c(0, 0.5, 1, 1.5, 2, 3, 5)
par(bg=NA)
for(i in 1:4){
  m12 <- tm_shape(temp[2,,,i]) +
    tm_raster(palette = hcl.colors(6, "Lajolla"), 
              style = "fixed", breaks = breaks, midpoint = NA) +
    tm_layout(frame=FALSE, legend.show=FALSE, bg.color = "transparent")
  tmap_save(m12, filename = paste0("figures/manuscript/workflow12_",i,".png"), 
            width = 4, height = 4, bg="transparent") 
  rm("m12")
}
par(bg="white")
rm(list = ls())
