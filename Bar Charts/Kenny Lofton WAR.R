#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(weights)
theme_set(theme_bw())

#Import data

lofton_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts/Kenny Lofton WAR.csv")

#Creating a highlight for Kenny Lofton

lofton_data <- lofton_data %>%
  mutate(Highlight = ifelse(Name == "Kenny Lofton", "yes", "no"))

#Combine season and player columns

lofton_data$SeasonName <- paste(lofton_data$Season, lofton_data$Name, sep = " ")

#Create visual

lofton_data %>%
  ggplot(aes(x = reorder(SeasonName, WAR), y = WAR, fill = Highlight)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = rd(WAR,1)), fontface = "bold", hjust = -0.5) +
  coord_flip(ylim = c(4,8)) + 
  scale_y_continuous(breaks = seq(4, 8, 0.5)) +
  scale_fill_manual(values = c("yes" = "#E31937", "no" = "#0C2340")) +
  theme(legend.position = "none") +
  labs(title = "Highest Single Season WAR for the 1990s Indians",
       x = "",
       y = "WAR",
       fill = "Player") +
  theme(plot.title = element_text(hjust = 0.5))