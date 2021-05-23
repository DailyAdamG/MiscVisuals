#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(weights)
theme_set(theme_bw())

#Import data

zobrist_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts/Zobrist WAR.csv")

#Creating a highlight for Ben Zobrist

zobrist_data <- zobrist_data %>%
  mutate(Highlight = ifelse(Name == "Ben Zobrist", "yes", "no"))

#Combine season and player columns

zobrist_data$SeasonName <- paste(zobrist_data$Season, zobrist_data$Name, sep = " ")

#Create visual

zobrist_data %>%
  ggplot(aes(x = reorder(SeasonName, WAR), y = WAR, fill = Highlight)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = rd(WAR, 1)), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(5, 9)) + 
  scale_y_continuous(breaks = seq(5, 9, 0.5)) +
  scale_fill_manual(values = c("yes" = "#8FBCE6", "no" = "#092C5C")) +
  theme(legend.position = "none") +
  labs(title = "Highest Single Season Position Player WAR in Rays History",
       subtitle = "WAR from FanGraphs.com",
       x = "",
       y = "Season WAR",
       fill = "Player")