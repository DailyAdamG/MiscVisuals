#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create HR by team vector

vector_for_chart <- c("New York Mets" = 493,
                      "Los Angeles Angels" = 2416,
                      "Houston Astros" = 1866,
                      "Texas Rangers" = 939)

#Create colors for chart

chart_colors <- c("New York Mets" = "#FF5910",
                  "Los Angeles Angels" = "#BF0D3E",
                  "Houston Astros" = "#FF8200",
                  "Texas Rangers" = "#002D72")

#Create visual

waffle(vector_for_chart / 10, 
       rows = 20, 
       colors = chart_colors, 
       title = "Nolan Ryan Strikeouts by Team",
       xlab = "1 Box = 10 Strikeouts")