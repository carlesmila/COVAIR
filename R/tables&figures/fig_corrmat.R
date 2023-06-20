#-----------------------------------------------------------------------------#
#                 Correlation matrices for predicted exposures                #
#-----------------------------------------------------------------------------#

library("tidyverse")
library("lubridate")
library("GGally")
library("cowplot")

# Read data, stratify by year ----
predexpos <- read_csv("database/correlation/correlation.csv") |> 
  mutate(year = year(date))
names(predexpos) <- c("ID", "date", "Temp.",
                      "PM2.5",
                      "PM10",
                      "NO2",
                      "O3",
                      "year")

# 2019 ----
data_spatial <- predexpos |> 
  group_by(ID) |> 
  summarise(across(2:6, mean))
p1 <- ggcorr(predexpos[predexpos,3:7], method = c("pairwise", "spearman"), 
             # nbreaks = 7, palette = "PRGn",
       label = TRUE, label_size = 4, label_round = 2) +
  theme(legend.position = "none")
p2 <- ggcorr(data_spatial[,2:6], method = c("pairwise", "spearman"), 
             # nbreaks = 7, palette = "PRGn",
             label = TRUE, label_size = 4, label_round = 2) +
  theme(legend.position = "none")
pboth <- plot_grid(p1, p2, nrow = 1,
                   labels = c("Short-term exposures", "Long-term exposures"))
cowplot::save_plot("figures/manuscript/corrplot.png", pboth,
                   base_width = 8, base_height = 4)
