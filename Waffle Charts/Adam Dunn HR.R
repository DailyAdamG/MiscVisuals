#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create HR by team vector

vector_for_chart <- c("Cincinnati Reds" = 270,
                      "Chicago White Sox" = 106,
                      "Washington Nationals" = 76,
                      "Arizona Diamondbacks" = 8,
                      "Oakland Athletics" = 2)

#Create colors for chart

chart_colors <- c("Cincinnati Reds" = "#C6011F",
                  "Chicago White Sox" = "#27251F",
                  "Washington Nationals" = "#14225A",
                  "Arizona Diamondbacks" = "#E3D4AD",
                  "Oakland Athletics" = "#003831")

#Create visual

waffle(vector_for_chart, 
       rows = 20,
       colors = chart_colors,
       title = "Adam Dunn HR by Team",
       xlab = "1 Box = 1 Home Run")