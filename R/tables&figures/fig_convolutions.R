#-----------------------------------------------------------------------------#
#                          Convolutional predictors                           #
#-----------------------------------------------------------------------------#

library("targets")
library("tidyverse")
library("lubridate")
library("stars")
library("gridExtra")

# Convolutional weights - Gaussian kernel
# Spatial predictors: Land use, road density, TCD and IMD
tar_load("spstack")
roads <- read_rds(spstack)["primary_dens"]

# Original primary roads
proad1 <- ggplot() +
  geom_stars(data = log(roads+1)) +
  scale_fill_viridis_c(na.value = "white") +
  labs(fill = "Log(x+1)") +
  theme_minimal() +   
  theme(panel.grid = element_blank(), axis.text = element_blank(), 
        legend.position = "bottom", legend.key.width = unit(0.8, 'cm')) +
  xlab("") + ylab("") + coord_equal() + ggtitle("A)")

# Convoluted primary roads
tar_load("focal_preds")
roadsf <- read_rds(focal_preds$spstack_focal)["primary_dens_focal"]
proad2 <- ggplot() +
  geom_stars(data = log(roadsf+1)) +
  scale_fill_viridis_c(na.value = "white") +
  labs(fill = "Log(x+1)") +
  theme_minimal() + 
  theme(panel.grid = element_blank(), axis.text = element_blank(), 
        legend.position = "bottom", legend.key.width = unit(0.8, 'cm')) +
  xlab("") + ylab("") + coord_equal() + ggtitle("B)")

# focal mean weights
focalw <- raster::focalWeight(as(roads, "Raster"), 1000, "Gaus")
focalw <- as.data.frame(focalw)
focalw$row <- 1:25
focalw <- pivot_longer(focalw, -row, names_to = "col", values_to = "w")
focalw$col <- as.integer(gsub("V", "", focalw$col))
pweights <- ggplot(focalw) +
  geom_tile(aes(x = row, y = col, fill = w)) +
  coord_equal() +
  labs(fill = "Weights") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        legend.position = "bottom", legend.key.width = unit(0.8, 'cm'),
        legend.text = element_text(size = 7)) +
  xlab("") + ylab("") + ggtitle("C)")

# compose
pall <- grid.arrange(proad1, proad2, pweights, nrow = 1)
ggsave("figures/manuscript/convolutions.png", pall, width = 8, height = 4)