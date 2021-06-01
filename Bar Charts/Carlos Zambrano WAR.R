#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(weights)
theme_set(theme_bw())

#Import data

zambrano_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts/Carlos Zambrano WAR.csv")

#Creating a highlight for Carlos Zambrano

zambrano_data <- zambrano_data %>%
  mutate(Highlight = ifelse(Name == "Carlos Zambrano", "yes", "no"))

#Combine season and player columns

zambrano_data$SeasonName <- paste(zambrano_data$Season, zambrano_data$Name, sep = " ")

#Create visual

zambrano_data %>%
  ggplot(aes(x = reorder(SeasonName, WAR), y = WAR, fill = Highlight)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = rd(WAR, 1)), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(0, 35)) + 
  scale_y_continuous(breaks = seq(0, 35, 5)) +
  scale_fill_manual(values = c("yes" = "#CC3433", "no" = "#0E3386")) +
  theme(legend.position = "none") +
  labs(title = "Most WAR by NL Central Pitcher from 2000-2009",
       subtitle = "WAR from FanGraphs.com",
       x = "",
       y = "Season WAR",
       fill = "Player")