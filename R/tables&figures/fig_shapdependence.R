#-----------------------------------------------------------------------------#
#                         SHAP figure per exposure                            #
#-----------------------------------------------------------------------------#

library("tidyverse")
library("shapviz")
library("cowplot")

# Function to fix predictor names
fix_prednames <- function(x){
  feature <- x
  feature = gsub("maiac", "MAIAC", feature)
  feature = gsub("lst", "LST", feature)
  feature = gsub("tropomi", "TROPOMI", feature)
  feature = gsub("omi", "OMI", feature)
  feature = gsub("ndvi", "S2_NDVI", feature)
  feature = gsub("ntli", "VIIRS_NTL", feature)
  feature = gsub("imd", "impervious", feature)
  feature = gsub("tcd", "treecover", feature)
  feature = gsub("primary_dens", "primary roads", feature)
  feature = gsub("local_dens", "secondary roads", feature)
  feature = gsub("popu_dens", "pop. dens.", feature)
  feature = gsub("PM2.5_pemisdist", "ind. sources PM2.5", feature)
  feature = gsub("PM10_pemisdist", "ind. sources PM10", feature)
  feature = gsub("NOx_pemisdist", "ind. sources NOx", feature)
  feature = gsub("coast_dist", "dist. to coast", feature)
  feature = gsub("anight", "a, night", feature)
  feature = gsub("aday", "a, day", feature)
  feature = gsub("dem", "elevation", feature)
  feature = gsub("pm25", "PM2.5", feature)
  feature = gsub("pm10", "PM10", feature)
  feature = gsub("o3", "O3", feature)
  feature = gsub("no2", "NO2", feature)
  feature = gsub("pbh", "BLH", feature)
  feature = gsub("julian", "julian day", feature)
  feature = gsub("dust", "dust day", feature)
  feature = gsub("holiday", "holiday day", feature)
  feature = gsub("focal", "(focal)", feature)
  feature = gsub("_", " ", feature)
  feature
}

dep_plot <- function(shapobj, var, minval, maxval){
  sv_dependence(shapobj, var, alpha = 0.2) + 
    geom_hline(yintercept = 0, lwd = 1, col = "grey20", alpha = 0.2) +
    geom_smooth(method = "gam", se = F, col = "orange") +
    ylim(minval, maxval) + ylab("SHAP value") +
    theme_bw() 
}

# temp ----
shap_temp <- readRDS("outputs/expo_mod2/temp_shap.rds")
colnames(shap_temp$S) <- fix_prednames(colnames(shap_temp$S))
colnames(shap_temp$X) <- fix_prednames(colnames(shap_temp$X))
top8_temp <- sapply(as.data.frame(shap_temp$S), function(x) mean(abs(x)))
top8_temp <- top8_temp[order(top8_temp, decreasing=TRUE)][1:8]
top8_temp <- rownames_to_column(as.data.frame(top8_temp))
min_temp <- min(shap_temp$S)
max_temp <- max(shap_temp$S)
names(top8_temp) <- c("feature", "avgshap")
plot_temp <- plot_grid(dep_plot(shap_temp, top8_temp$feature[1], min_temp, max_temp), 
                       dep_plot(shap_temp, top8_temp$feature[2], min_temp, max_temp), 
                       dep_plot(shap_temp, top8_temp$feature[3], min_temp, max_temp), 
                       dep_plot(shap_temp, top8_temp$feature[4], min_temp, max_temp), 
                       dep_plot(shap_temp, top8_temp$feature[5], min_temp, max_temp),  
                       dep_plot(shap_temp, top8_temp$feature[6], min_temp, max_temp), 
                       dep_plot(shap_temp, top8_temp$feature[7], min_temp, max_temp),  
                       dep_plot(shap_temp, top8_temp$feature[8], min_temp, max_temp),  
                       ncol = 3)
cowplot::save_plot("figures/manuscript/shap_temp.png", plot_temp, 
                   base_height = 8, base_asp = 1, bg = "white")

# pm25 ----
shap_pm25 <- readRDS("outputs/expo_mod2/pm25_shap.rds")
colnames(shap_pm25$S) <- fix_prednames(colnames(shap_pm25$S))
colnames(shap_pm25$X) <- fix_prednames(colnames(shap_pm25$X))
top8_pm25 <- sapply(as.data.frame(shap_pm25$S), function(x) mean(abs(x)))
top8_pm25 <- top8_pm25[order(top8_pm25, decreasing=TRUE)][1:8]
top8_pm25 <- rownames_to_column(as.data.frame(top8_pm25))
min_pm25 <- min(shap_pm25$S)
max_pm25 <- max(shap_pm25$S)
names(top8_pm25) <- c("feature", "avgshap")
plot_pm25 <- plot_grid(dep_plot(shap_pm25, top8_pm25$feature[1], min_pm25, max_pm25), 
                       dep_plot(shap_pm25, top8_pm25$feature[2], min_pm25, max_pm25), 
                       dep_plot(shap_pm25, top8_pm25$feature[3], min_pm25, max_pm25), 
                       dep_plot(shap_pm25, top8_pm25$feature[4], min_pm25, max_pm25), 
                       dep_plot(shap_pm25, top8_pm25$feature[5], min_pm25, max_pm25),  
                       dep_plot(shap_pm25, top8_pm25$feature[6], min_pm25, max_pm25), 
                       dep_plot(shap_pm25, top8_pm25$feature[7], min_pm25, max_pm25),  
                       dep_plot(shap_pm25, top8_pm25$feature[8], min_pm25, max_pm25),  
                       ncol = 3)
cowplot::save_plot("figures/manuscript/shap_pm25.png", plot_pm25, 
                   base_height = 8, base_asp = 1, bg = "white")
rm("shap_pm25", "top8_pm25", "plot_pm25", "min_pm25", "max_pm25")

# pm10 ----
shap_pm10 <- readRDS("outputs/expo_mod2/pm10_shap.rds")
colnames(shap_pm10$S) <- fix_prednames(colnames(shap_pm10$S))
colnames(shap_pm10$X) <- fix_prednames(colnames(shap_pm10$X))
top8_pm10 <- sapply(as.data.frame(shap_pm10$S), function(x) mean(abs(x)))
top8_pm10 <- top8_pm10[order(top8_pm10, decreasing=TRUE)][1:8]
top8_pm10 <- rownames_to_column(as.data.frame(top8_pm10))
min_pm10 <- min(shap_pm10$S)
max_pm10 <- max(shap_pm10$S)
names(top8_pm10) <- c("feature", "avgshap")
plot_pm10 <- plot_grid(dep_plot(shap_pm10, top8_pm10$feature[1], min_pm10, max_pm10), 
                       dep_plot(shap_pm10, top8_pm10$feature[2], min_pm10, max_pm10), 
                       dep_plot(shap_pm10, top8_pm10$feature[3], min_pm10, max_pm10), 
                       dep_plot(shap_pm10, top8_pm10$feature[4], min_pm10, max_pm10), 
                       dep_plot(shap_pm10, top8_pm10$feature[5], min_pm10, max_pm10),  
                       dep_plot(shap_pm10, top8_pm10$feature[6], min_pm10, max_pm10), 
                       dep_plot(shap_pm10, top8_pm10$feature[7], min_pm10, max_pm10),  
                       dep_plot(shap_pm10, top8_pm10$feature[8], min_pm10, max_pm10),  
                       ncol = 3)
cowplot::save_plot("figures/manuscript/shap_pm10.png", plot_pm10, 
                   base_height = 8, base_asp = 1, bg = "white")
rm("shap_pm10", "top8_pm10", "plot_pm10", "min_pm10", "max_pm10")

# no2 ----
shap_no2 <- readRDS("outputs/expo_mod2/no2_shap.rds")
colnames(shap_no2$S) <- fix_prednames(colnames(shap_no2$S))
colnames(shap_no2$X) <- fix_prednames(colnames(shap_no2$X))
top8_no2 <- sapply(as.data.frame(shap_no2$S), function(x) mean(abs(x)))
top8_no2 <- top8_no2[order(top8_no2, decreasing=TRUE)][1:8]
top8_no2 <- rownames_to_column(as.data.frame(top8_no2))
min_no2 <- min(shap_no2$S)
max_no2 <- max(shap_no2$S)
names(top8_no2) <- c("feature", "avgshap")
plot_no2 <- plot_grid(dep_plot(shap_no2, top8_no2$feature[1], min_no2, max_no2), 
                       dep_plot(shap_no2, top8_no2$feature[2], min_no2, max_no2), 
                       dep_plot(shap_no2, top8_no2$feature[3], min_no2, max_no2), 
                       dep_plot(shap_no2, top8_no2$feature[4], min_no2, max_no2), 
                       dep_plot(shap_no2, top8_no2$feature[5], min_no2, max_no2),  
                       dep_plot(shap_no2, top8_no2$feature[6], min_no2, max_no2), 
                       dep_plot(shap_no2, top8_no2$feature[7], min_no2, max_no2),  
                       dep_plot(shap_no2, top8_no2$feature[8], min_no2, max_no2),  
                       ncol = 3)
cowplot::save_plot("figures/manuscript/shap_no2.png", plot_no2, 
                   base_height = 8, base_asp = 1, bg = "white")
rm("shap_no2", "top8_no2", "plot_no2", "min_no2", "max_no2")

# o3 ----
shap_o3 <- readRDS("outputs/expo_mod2/o3_shap.rds")
colnames(shap_o3$S) <- fix_prednames(colnames(shap_o3$S))
colnames(shap_o3$X) <- fix_prednames(colnames(shap_o3$X))
top8_o3 <- sapply(as.data.frame(shap_o3$S), function(x) mean(abs(x)))
top8_o3 <- top8_o3[order(top8_o3, decreasing=TRUE)][1:8]
top8_o3 <- rownames_to_column(as.data.frame(top8_o3))
min_o3 <- min(shap_o3$S)
max_o3 <- max(shap_o3$S)
names(top8_o3) <- c("feature", "avgshap")
plot_o3 <- plot_grid(dep_plot(shap_o3, top8_o3$feature[1], min_o3, max_o3), 
                       dep_plot(shap_o3, top8_o3$feature[2], min_o3, max_o3), 
                       dep_plot(shap_o3, top8_o3$feature[3], min_o3, max_o3), 
                       dep_plot(shap_o3, top8_o3$feature[4], min_o3, max_o3), 
                       dep_plot(shap_o3, top8_o3$feature[5], min_o3, max_o3),  
                       dep_plot(shap_o3, top8_o3$feature[6], min_o3, max_o3), 
                       dep_plot(shap_o3, top8_o3$feature[7], min_o3, max_o3),  
                       dep_plot(shap_o3, top8_o3$feature[8], min_o3, max_o3),  
                       ncol = 3)
cowplot::save_plot("figures/manuscript/shap_o3.png", plot_o3, 
                   base_height = 8, base_asp = 1, bg = "white")
rm("shap_o3", "top8_o3", "plot_o3", "min_o3", "max_o3")
