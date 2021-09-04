#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create HR by team vector

vector_for_chart <- c("Miami Marlins" = 607,
                      "Boston Red Sox" = 1108,
                      "Los Angeles Dodgers" = 186)

#Create colors for chart

chart_colors <- c("Miami Marlins" = "#00A3E0",
                  "Boston Red Sox" = "#BD3039",
                  "Los Angeles Dodgers" = "#005A9C")

#Create visual

waffle(vector_for_chart / 5, 
       rows = 19, 
       colors = chart_colors, 
       title = "Josh Beckett Strikeouts by Team",
       xlab = "1 Box = 5 Strikeouts")