#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(weights)
theme_set(theme_bw())

#Import data

velo_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts/Thor Velo.csv")

#Creating a highlight for Noah Syndergaard

velo_data <- velo_data %>%
  mutate(Highlight = ifelse(Name == "Noah Syndergaard", "yes", "no"))

#Combine season and player columns

velo_data$SeasonName <- paste(velo_data$Season, velo_data$Name, sep = " ")

#Create visual

velo_data %>%
  ggplot(aes(x = reorder(SeasonName, desc(vFA)), y = vFA, fill = Highlight)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = rd(vFA, 1)), fontface = "bold", vjust = -1) +
  coord_cartesian(ylim = c(95, 99)) +
  scale_y_continuous(breaks = seq(95, 99, 0.5)) +
  scale_fill_manual(values = c("yes" = "#FD5A1E", "no" = "dark blue")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Highest Average Fastball Velocity",
       subtitle = "*Minimum of 100 IP to Qualify*",
       x = "",
       y = "Average Fastball Velocity (MPH)",
       fill = "Player")