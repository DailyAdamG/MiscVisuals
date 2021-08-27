#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create HR by team vector

vector_for_chart <- c("Cleveland Indians" = 337,
                      "Philadelphia Phillies" = 101,
                      "Chicago White Sox" = 134,
                      "Minnesota Twins" = 37,
                      "Baltimore Orioles" = 3)

#Create colors for chart

chart_colors <- c("Cleveland Indians" = "#0C2340",
                  "Philadelphia Phillies" = "#E81828",
                  "Chicago White Sox" = "#27251F",
                  "Minnesota Twins" = "#002B5C",
                  "Baltimore Orioles" = "#DF4601")

#Create visual

waffle(vector_for_chart, 
       rows = 25, 
       colors = chart_colors, 
       title = "Jim Thome HR by Team",
       xlab = "1 Box = 1 Home Run")