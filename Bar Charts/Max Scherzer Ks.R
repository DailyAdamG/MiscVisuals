#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

#Import data

scherzer_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts/Max Scherzer Ks.csv")

#Create color scheme for fill

fill_color <- c("Arizona Diamondbacks" = "#E3D4AD",
                "Detroit Tigers" = "#FA4616",
                "Washington Nationals" = "#AB0003")

#Create visual

scherzer_data %>%
  ggplot(aes(x = Opp, y = SO, fill = Team)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = fill_color) +
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Max Scherzer Strikeouts by Opponent",
       x = "Opponent",
       y = "Total Strikeouts",
       fill = "Max Scherzer's Team")