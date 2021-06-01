#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create HR by team vector

vector_for_chart <- c("Cleveland Indians" = 236,
                      "Boston Red Sox" = 274,
                      "Los Angeles Dodgers" = 44,
                      "Chicago White Sox" = 1)

#Create colors for chart

chart_colors <- c("Cleveland Indians" = "#0C2340",
                  "Boston Red Sox" = "#BD3039",
                  "Los Angeles Dodgers" = "#005A9C",
                  "Chicago White Sox" = "#27251F")

#Create visual

waffle(vector_for_chart, 
       rows = 25, 
       colors = chart_colors, 
       title = "Manny Ramirez HR by Team",
       xlab = "1 Box = 1 Home Run")