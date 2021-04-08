#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Nelson Cruz HR Bar Chart")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

#Import data

hr_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Nelson Cruz HR Bar Chart/Nelson Cruz HR.csv")

#Create color scheme for fill

fill_color <- c("Baltimore Orioles" = "#DF4601",
                "Milwaukee Brewers" = "#FFC52F",
                "Minnesota Twins" = "#002B5C",
                "Seattle Mariners" = "#005C5C",
                "Texas Rangers" = "#C0111F")

#Create visual

hr_data %>%
  ggplot(aes(x = Opp, y = HR, fill = Team)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = fill_color) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Nelson Cruz Home Runs by Opponent",
       x = "Opponent",
       y = "Total Home Runs",
       fill = "Nelson Cruz's Team")