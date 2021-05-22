#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

#Import data

greinke_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts/Greinke Ks.csv")

#Create color scheme for fill

fill_color <- c("Kansas City Royals" = "#004687",
                "Milwaukee Brewers" = "#FFC52F",
                "Los Angeles Angels" = "#BA0021",
                "Los Angeles Dodgers" = "#005A9C",
                "Arizona Diamondbacks" = "#E3D4AD",
                "Houston Astros" = "#EB6E1F")

greinke_data$Team <- factor(greinke_data$Team, levels = c("Kansas City Royals",
                                                          "Milwaukee Brewers",
                                                          "Los Angeles Angels",
                                                          "Los Angeles Dodgers",
                                                          "Arizona Diamondbacks",
                                                          "Houston Astros"))

#Create visual

greinke_data %>%
  ggplot(aes(x = Opp, y = SO, fill = Team)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = fill_color) +
  scale_y_continuous(breaks = seq(0, 225, 25)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Zack Greinke Strikeouts by Opponent",
       x = "Opponent",
       y = "Total Strikeouts",
       fill = "Zack Greinke's Team")