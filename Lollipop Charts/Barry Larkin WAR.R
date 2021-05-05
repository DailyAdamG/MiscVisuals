#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Lollipop Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(weights)

#Import data

larkin_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Lollipop Charts/Barry Larkin WAR.csv")

#Create visual

larkin_data %>%
  ggplot(aes(x = Season, y = WAR)) +
  geom_point(size = 7, color = "#C6011F") +
  geom_text(aes(label = rd(WAR,1)), color = "white", fontface = "bold", size = 9/.pt, hjust = .5, vjust = .5) +
  geom_segment(aes(x = Season,
                   xend = Season,
                   y = 0,
                   yend = WAR - 0.1), size = 1, color = "#C6011F") +
  scale_x_continuous(breaks = seq(1986, 2004, 1)) +
  scale_y_continuous(breaks = seq(0, 7, 1)) +
  labs(title = "Barry Larkin Career WAR by Season") +
  theme(legend.position = "none")