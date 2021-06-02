#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(weights)
theme_set(theme_bw())

#Import data

yadi_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts/Yadier Molina.csv")

#Creating a highlight for Carlos Zambrano

yadi_data <- yadi_data %>%
  mutate(Highlight = ifelse(Name == "Yadier Molina", "yes", "no"))

#Create visual

yadi_data %>%
  ggplot(aes(x = reorder(Name, Inn), y = Inn, fill = Highlight)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Inn), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(1000, 18000)) + 
  scale_y_continuous(breaks = seq(1000, 18000, 2000)) +
  scale_fill_manual(values = c("yes" = "#FEDB00", "no" = "#C41E3A")) +
  theme(legend.position = "none") +
  labs(title = "Most Innings Caught by Active Catchers",
       subtitle = "Minimum of 1500 Innings Caught",
       x = "",
       y = "Innings",
       fill = "Player")