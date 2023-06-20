#-----------------------------------------------------------------------------#
#                           CV scatterplots figure                            #
#-----------------------------------------------------------------------------#

library("tidyverse")
library("colorspace")
library("lubridate")
library("gridExtra")

# temp ----
temp <- read_csv("outputs/expo_mod2/temp_perf.csv") %>%
  rename(predicted = pred2) %>%
  ggplot() +
  geom_hex(aes(y = tavg, x = predicted)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_fill_continuous_sequential(palette = "Mint") +
  coord_equal() +
  theme_bw() + labs(fill="Count") +
  ylab("Measured temperature (ºC)") + 
  xlab("Out-of-sample predicted temperature (ºC)") +
  theme(aspect.ratio = 1)

# PM2.5 ----
pm25 <- read_csv("outputs/expo_mod2/pm25_perf.csv") %>%
  rename(predicted = pred1) %>%
  ggplot() +
  geom_hex(aes(y = PM2.5, x = predicted)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_fill_continuous_sequential(palette = "Mint") +
  coord_equal() +
  theme_bw() + labs(fill="Count") +
  ylab(expression("Measured PM"[2.5]~"("*mu*"g/m"^3*")")) + 
  xlab(expression("Out-of-sample predicted PM"[2.5]~"("*mu*"g/m"^3*")")) +
  theme(aspect.ratio = 1)

# PM10 ----
pm10 <- read_csv("outputs/expo_mod2/pm10_perf.csv") %>%
  rename(predicted = pred2) %>%
  ggplot() +
  geom_hex(aes(y = PM10, x = predicted)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_fill_continuous_sequential(palette = "Mint") +
  coord_equal() +
  theme_bw() + labs(fill="Count") +
  ylab(expression("Measured PM"[10]~"("*mu*"g/m"^3*")")) + 
  xlab(expression("Out-of-sample predicted PM"[10]~"("*mu*"g/m"^3*")")) +
  theme(aspect.ratio = 1)

# NO2 ----
no2 <- read_csv("outputs/expo_mod2/no2_perf.csv") %>%
  rename(predicted = pred2) %>%
  ggplot() +
  geom_hex(aes(y = NO2, x = predicted)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_fill_continuous_sequential(palette = "Mint") +
  coord_equal() +
  theme_bw() + labs(fill="Count") +
  ylab(expression("Measured NO"[2]~"("*mu*"g/m"^3*")")) + 
  xlab(expression("Out-of-sample predicted NO"[2]~"("*mu*"g/m"^3*")")) +
  theme(aspect.ratio = 1)

# O3 ----
o3 <- read_csv("outputs/expo_mod2/o3_perf.csv") %>%
  rename(predicted = pred2) %>%
  ggplot() +
  geom_hex(aes(y = O3, x = predicted)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_fill_continuous_sequential(palette = "Mint") +
  coord_equal() +
  theme_bw() + labs(fill="Count") +
  ylab(expression("Measured O"[3]~"("*mu*"g/m"^3*")")) + 
  xlab(expression("Out-of-sample predicted O"[3]~"("*mu*"g/m"^3*")")) +
  theme(aspect.ratio = 1)


# Composite ----
scatter <- grid.arrange(temp, pm25, pm10, no2, o3, ncol = 2)
ggsave("figures/manuscript/CVscatter.png", scatter, dpi = 300, width = 9, height = 10)
