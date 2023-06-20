#-----------------------------------------------------------------------------#
#                       RS imputation train/test split                        #
#-----------------------------------------------------------------------------#

library("targets")
library("tidyverse")
library("sf")

tar_load("cat")
cat <- read_rds(cat)
gapsplit <- read_rds("database/split_areas.rds")
tsamp <- read_rds("database/split_indicators.rds") %>%
  dplyr::filter(date == as.Date("2020-01-01"))
gapsplit$use <- ifelse(gapsplit$gapID %in% tsamp$gapID, "Test", "Train")
gapsplit <- st_intersection(gapsplit, cat) %>% 
  arrange(use) %>%
  mutate(use = fct_rev(fct_inorder(as.factor(use))))
pcv <- ggplot(gapsplit) +
  geom_sf(aes(fill=use), alpha = 0.5, col = "black", size = 0.2) +
  scale_fill_viridis_d(option = "A") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  labs(fill = "Data split")
ggsave("figures/manuscript/RSimpCV.png", pcv, dpi = 300, width = 5, height = 4)
