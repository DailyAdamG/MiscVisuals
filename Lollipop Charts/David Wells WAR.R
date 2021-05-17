#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Lollipop Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(weights)
theme_set(theme_bw())

#Import data

wells_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Lollipop Charts/David Wells WAR.csv")

#Create color scheme

team_colors <- c("TOR" = "#134A8E",
                   "DET" = "#FA4616",
                   "2 Tms" = "forest green",
                   "BAL" = "#DF4601",
                   "NYY" = "#0C2340",
                   "CHW" = "#C4CED4",
                   "SDP" = "#FFC425",
                   "BOS" = "#BD3039")

#Create visual

wells_data %>%
  ggplot(aes(x = Season, y = WAR, fill = Team)) +
  geom_point(size = 7, aes(col = Team)) +
  scale_color_manual(values = team_colors) +
  geom_text(aes(label = rd(WAR,1)), color = "white", fontface = "bold", size = 9/.pt, hjust = .5, vjust = .5) +
  geom_segment(aes(x = Season,
                   xend = Season,
                   y = 0,
                   yend = WAR - 0.1, color = Team), size = 1) +
  scale_x_continuous(breaks = seq(1987, 2007, 1)) +
  scale_y_continuous(breaks = seq(-1, 7, 1)) +
  labs(title = "David Wells WAR by Season and Team")