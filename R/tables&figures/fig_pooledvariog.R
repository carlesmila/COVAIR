#-----------------------------------------------------------------------------#
#                        Pooled variograms figure                             #
#-----------------------------------------------------------------------------#

library("tidyverse")
library("magick")

temp <- image_read("outputs/expo_mod2/temp_pooledvar.png") %>%
  image_annotate(text = "Temperature", size = 80)
pm25 <- image_read("outputs/expo_mod2/pm25_pooledvar.png") %>%
  image_annotate(text = "PM2.5", size = 80)
pm10 <- image_read("outputs/expo_mod2/pm10_pooledvar.png") %>%
  image_annotate(text = "PM10", size = 80)
no2 <- image_read("outputs/expo_mod2/no2_pooledvar.png") %>%
  image_annotate(text = "NO2", size = 80)
o3 <- image_read("outputs/expo_mod2/o3_pooledvar.png") %>%
  image_annotate(text = "O3", size = 80)
c(temp, pm25, pm10, no2, o3) %>%
  magick::image_montage(tile = "2", geometry = "1200x650") %>%
  magick::image_convert("png") %>%
  magick::image_write(path = "figures/manuscript/pooledvariog.png", quality = 100)