#-----------------------------------------------------------------------------#
#               Station exposure distribution by year figure                  #
#-----------------------------------------------------------------------------#

library("targets")
library("tidyverse")
library("lubridate")
library("ggokabeito") # Okabe-Ito discrete colourblind-safe palette
library("sf")

# Load station locations
tar_load(ap_stations)
ap_readings <- bind_rows(read_rds(ap_stations$readings18),
                         read_rds(ap_stations$readings19),
                         read_rds(ap_stations$readings20)) %>%
  mutate(Year = as.character(year(as.Date(date))),
         pollutant = case_when(
           pollutant == "NO2" ~ "NO[2]",
           pollutant == "PM2.5" ~ "PM[2.5]",
           pollutant == "PM10" ~ "PM[10]",
           pollutant == "O3" ~ "O[3]"
         ),
         pollutant = fct_relevel(pollutant, "PM[2.5]", "PM[10]","NO[2]","O[3]"))

# Averages
yearly_avgs <- ap_readings %>%
  group_by(Year, pollutant) %>%
  summarise(lab = paste0("Mean (SD): ", round(mean(conc),1), " (", round(sd(conc),1), ")"),
            .groups="keep") %>%
  ungroup() %>%
  mutate(x = case_when(pollutant == "PM[2.5]" ~ 35,
                       pollutant == "PM[10]" ~ 100,
                       pollutant == "NO[2]" ~ 60,
                       pollutant == "O[3]" ~ 110),
         y = case_when(pollutant == "PM[2.5]" ~ 0.075-0.011*(as.integer(Year)-2018),
                       pollutant == "PM[10]" ~ 0.048-0.008*(as.integer(Year)-2018),
                       pollutant == "NO[2]" ~ 0.0375-0.006*(as.integer(Year)-2018),
                       pollutant == "O[3]" ~ 0.019-0.003*(as.integer(Year)-2018)))

# Figure
p <- ggplot(ap_readings) +
  geom_density(aes(x = conc, color = Year, fill = Year), alpha = 0.4, size = 0.1) +
  geom_text(data = yearly_avgs, aes(x = x, y = y, label = lab, col = Year), hjust = 0, show.legend = FALSE) +
  scale_fill_okabe_ito() +   scale_color_okabe_ito() +
  facet_wrap(~ pollutant, scales = "free", labeller = label_parsed) +
  xlab(expression("Concentration"~"("*mu*"g/m"^3*")")) + ylab("Density") +
  theme_bw() + theme(legend.position = "bottom") 
ggsave("figures/manuscript/expodist.png", p, width = 9, height = 6)
