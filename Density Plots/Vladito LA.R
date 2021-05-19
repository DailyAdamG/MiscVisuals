#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Density Plots")

#Loading necessary libraries

library(tidyverse)
library(ggplot2)


#Grabbing csv file

la_data <- read.csv(file = "C:/Users/daily/Desktop/Repositories/MiscVisuals/Density Plots/Vladito LA.csv")

#Season column breakdown

la_data <- la_data %>%
  mutate(season_break_down = ifelse(game_year == 2021, "2021 Season", "2019-2020 Season"))

#Create color vector

colors <- c("2019-2020 Season" = "#134A8E",
            "2021 Season" = "#1D2D5C")

#Create visual

la_data %>%
  ggplot(aes(x = launch_angle, fill = season_break_down)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = colors)+
  scale_x_continuous(breaks = seq(-90, 90, 10)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1.25) +
  labs(title = "Vladimir Guerrero Jr. Launch Angle Breakdown", x = "Launch Angles", y = "Density", fill = "Seasons") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(hjust = 0.5))