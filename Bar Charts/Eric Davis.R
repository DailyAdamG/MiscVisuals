#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(weights)
theme_set(theme_bw())

#Import data

davis_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts/Eric Davis HR_SB.csv")

#Creating a highlight for Eric Davis

davis_data <- davis_data %>%
  mutate(Highlight = ifelse(fullName == "Eric Davis", "yes", "no"))

#Combine season and player columns

davis_data$SeasonName <- paste(davis_data$yearID, davis_data$fullName, sep = " ")

#Create visual

davis_data %>%
  ggplot(aes(x = reorder(SeasonName, HR), y = HR, fill = Highlight)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = HR), fontface = "bold", hjust = -0.5) +
  coord_flip(ylim = c(25, 40)) + 
  scale_y_continuous(breaks = seq(25, 40, 1)) +
  scale_fill_manual(values = c("yes" = "#C6011F", "no" = "#000000")) +
  theme(legend.position = "none") +
  labs(title = "Most Home Runs in a Season With 50 or More SB",
       x = "",
       y = "Home Runs",
       fill = "Player") +
  theme(plot.title = element_text(hjust = 0.5))