#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create HR by team vector

vector_for_chart <- c("Oakland Athletics" = 254,
                      "Texas Rangers" = 45,
                      "Boston Red Sox" = 52,
                      "Toronto Blue Jays" = 46,
                      "Tampa Bay Rays" = 43,
                      "New York Yankees" = 6,
                      "Chicago White Sox" = 16)

#Create colors for chart

chart_colors <- c("Oakland Athletics" = "#003831",
                  "Texas Rangers" = "#003278",
                  "Boston Red Sox" = "#BD3039",
                  "Toronto Blue Jays" = "#134A8E",
                  "Tampa Bay Rays" = "#8FBCE6",
                  "New York Yankees" = "#C4CED3",
                  "Chicago White Sox" = "#27251F")

#Create visual

waffle(vector_for_chart, 
       rows = 20, 
       colors = chart_colors, 
       title = "Jose Canseco HR by Team",
       xlab = "1 Box = 1 Home Run")