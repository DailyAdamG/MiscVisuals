#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create HR by team vector

vector_for_chart <- c("Cleveland Indians" = 242,
                      "Chicago White Sox" = 79,
                      "Baltimore Orioles" = 60)

#Create colors for chart

chart_colors <- c("Cleveland Indians" = "#E31937",
                  "Chicago White Sox" = "#27251F",
                  "Baltimore Orioles" = "#DF4601")

#Create visual

waffle(vector_for_chart, 
       rows = 20, 
       colors = chart_colors, 
       title = "Albert Belle HR by Team",
       xlab = "1 Box = 1 Home Run")