#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create HR by team vector

vector_for_chart <- c("Texas Rangers" = 157,
                      "Baltimore Orioles" = 40,
                      "Seattle Mariners" = 163,
                      "Minnesota Twins" = 76,
                      "Tampa Bay Rays" = 12)

#Create colors for chart

chart_colors <- c("Texas Rangers" = "#C0111F",
                  "Baltimore Orioles" = "#DF4601",
                  "Seattle Mariners" = "#005C5C",
                  "Minnesota Twins" = "#002B5C",
                  "Tampa Bay Rays" = "#8FBCE6")

#Create visual

waffle(vector_for_chart, 
       rows = 20, 
       colors = chart_colors, 
       title = "Nelson Cruz HR by Team",
       xlab = "1 Box = 1 Home Run")