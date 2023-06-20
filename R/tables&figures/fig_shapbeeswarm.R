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


# temp ----
shap_temp <- readRDS("outputs/expo_mod2/temp_shap.rds")
colnames(shap_temp$S) <- fix_prednames(colnames(shap_temp$S))
colnames(shap_temp$X) <- fix_prednames(colnames(shap_temp$X))
top8_temp <- sapply(as.data.frame(shap_temp$S), function(x) mean(abs(x)))
top8_temp <- top8_temp[order(top8_temp, decreasing=TRUE)][1:8]
top8_temp <- round(top8_temp, 1)
top8_temp <- rownames_to_column(as.data.frame(top8_temp))
names(top8_temp) <- c("feature", "avgshap")
plot_temp <- sv_importance(shap_temp, kind = "beeswarm", show_numbers = F, max_display = 9,
                           alpha = 0.5, width = 0.3, size = 0.5)
plot_legend <- cowplot::get_legend(plot_temp +  theme_half_open(12) + theme(legend.justification = "center"))
plot_temp <- plot_temp + theme_bw() +
  theme(legend.position = "none") + ggtitle("Temperature (ºC)")
plot_temp <- plot_temp %+% plot_temp$data[!grepl("Sum", plot_temp$data$feature),]
plot_temp <- plot_temp +
  geom_text(data = top8_temp, aes(x = feature, y = -12, label = avgshap), size = 3)

# pm25 ----
shap_pm25 <- readRDS("outputs/expo_mod2/pm25_shap.rds")
colnames(shap_pm25$S) <- fix_prednames(colnames(shap_pm25$S))
colnames(shap_pm25$X) <- fix_prednames(colnames(shap_pm25$X))
top8_pm25 <- sapply(as.data.frame(shap_pm25$S), function(x) mean(abs(x)))
top8_pm25 <- top8_pm25[order(top8_pm25, decreasing=TRUE)][1:8]
top8_pm25 <- round(top8_pm25, 1)
top8_pm25 <- rownames_to_column(as.data.frame(top8_pm25))
names(top8_pm25) <- c("feature", "avgshap")
plot_pm25 <- sv_importance(shap_pm25, kind = "beeswarm", show_numbers = F, max_display = 9,
                           alpha = 0.5, width = 0.3, size = 0.5)
plot_pm25 <- plot_pm25 + theme_bw() +
  theme(legend.position = "none") + ggtitle(expression("PM"[2.5]~"("*mu*"g/m"^3*")"))
plot_pm25 <- plot_pm25 %+% plot_pm25$data[!grepl("Sum", plot_pm25$data$feature),]
plot_pm25 <- plot_pm25 + 
  geom_text(data = top8_pm25, aes(x = feature, y = -10.5, label = avgshap), size = 3)

# pm10 ----
shap_pm10 <- readRDS("outputs/expo_mod2/pm10_shap.rds")
colnames(shap_pm10$S) <- fix_prednames(colnames(shap_pm10$S))
colnames(shap_pm10$X) <- fix_prednames(colnames(shap_pm10$X))
top8_pm10 <- sapply(as.data.frame(shap_pm10$S), function(x) mean(abs(x)))
top8_pm10 <- top8_pm10[order(top8_pm10, decreasing=TRUE)][1:8]
top8_pm10 <- round(top8_pm10, 1)
top8_pm10 <- rownames_to_column(as.data.frame(top8_pm10))
names(top8_pm10) <- c("feature", "avgshap")
plot_pm10 <- sv_importance(shap_pm10, kind = "beeswarm", show_numbers = F, max_display = 9,
                           alpha = 0.5, width = 0.3, size = 0.5)
plot_pm10 <- plot_pm10 + theme_bw() +
  theme(legend.position = "none") + ggtitle(expression("PM"[10]~"("*mu*"g/m"^3*")"))
plot_pm10 <- plot_pm10 %+% plot_pm10$data[!grepl("Sum", plot_pm10$data$feature),]
plot_pm10 <- plot_pm10 + 
  geom_text(data = top8_pm10, aes(x = feature, y = -20, label = avgshap), size = 3)

# no2 ----
shap_no2 <- readRDS("outputs/expo_mod2/no2_shap.rds")
colnames(shap_no2$S) <- fix_prednames(colnames(shap_no2$S))
colnames(shap_no2$X) <- fix_prednames(colnames(shap_no2$X))
top8_no2 <- sapply(as.data.frame(shap_no2$S), function(x) mean(abs(x)))
top8_no2 <- top8_no2[order(top8_no2, decreasing=TRUE)][1:8]
top8_no2 <- round(top8_no2, 1)
top8_no2 <- rownames_to_column(as.data.frame(top8_no2))
names(top8_no2) <- c("feature", "avgshap")
plot_no2 <- sv_importance(shap_no2, kind = "beeswarm", show_numbers = F, max_display = 9,
                          alpha = 0.5, width = 0.3, size = 0.5)
plot_no2 <- plot_no2 + theme_bw() +
  theme(legend.position = "none") + ggtitle(expression("NO"[2]~"("*mu*"g/m"^3*")"))
plot_no2 <- plot_no2 %+% plot_no2$data[!grepl("Sum", plot_no2$data$feature),]
plot_no2 <- plot_no2 +
  geom_text(data = top8_no2, aes(x = feature, y = -16, label = avgshap), size = 3)

# o3 ----
shap_o3 <- readRDS("outputs/expo_mod2/o3_shap.rds")
colnames(shap_o3$S) <- fix_prednames(colnames(shap_o3$S))
colnames(shap_o3$X) <- fix_prednames(colnames(shap_o3$X))
top8_o3 <- sapply(as.data.frame(shap_o3$S), function(x) mean(abs(x)))
top8_o3 <- top8_o3[order(top8_o3, decreasing=TRUE)][1:8]
top8_o3 <- round(top8_o3, 1)
top8_o3 <- rownames_to_column(as.data.frame(top8_o3))
names(top8_o3) <- c("feature", "avgshap")
plot_o3 <- sv_importance(shap_o3, kind = "beeswarm", show_numbers = F, max_display = 9,
                         alpha = 0.5, width = 0.3, size = 0.5)
plot_o3 <- plot_o3 + theme_bw() +
  theme(legend.position = "none") + ggtitle(expression("O"[3]~"("*mu*"g/m"^3*")"))
plot_o3 <- plot_o3 %+% plot_o3$data[!grepl("Sum", plot_o3$data$feature),]
plot_o3 <- plot_o3 +
  geom_text(data = top8_o3, aes(x = feature, y = -60, label = avgshap), size = 3)

# Compose figure ----
plot_all <- plot_grid(plot_temp, plot_pm25, plot_pm10, plot_no2, plot_o3, plot_legend,
                      nrow = 2, ncol = 3)
cowplot::save_plot("figures/manuscript/shap_beeswarm.png", plot_all, 
                   base_height = 6, base_asp = 1.7, bg = "white")
