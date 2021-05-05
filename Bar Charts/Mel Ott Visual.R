#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

#Import data

hr_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts/Mel Ott Visual.csv")

#Creating a highlight for Mel Ott

hr_data <- hr_data %>%
  mutate(Highlight = ifelse(fullName == "Mel Ott", "yes", "no"))

#Create visual

hr_data %>%
  ggplot(aes(x = reorder(fullName, desc(LedLeagueHR)), y = LedLeagueHR, fill = Highlight)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = CareerHR), fontface = "bold", vjust = -1) +
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  scale_fill_manual(values = c("yes" = "#FD5A1E", "no" = "dark blue")) +
  theme(legend.position = "none") +
  labs(title = "Most Times Leading their League in HR",
       subtitle = "Player's career HR total appears above bar",
       x = "Player",
       y = "Times Leading the League in HR",
       fill = "Player")