#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create HR by team vector

vector_for_chart <- c("Montreal Expos" = 51,
                      "Seattle Mariners" = 2162,
                      "Houston Astros" = 116,
                      "Arizona Diamondbacks" = 2077,
                      "New York Yankees" = 383,
                      "San Francisco Giants" = 86)

#Create colors for chart

chart_colors <- c("Montreal Expos" = "#003087",
                  "Seattle Mariners" = "#005C5C",
                  "Houston Astros" = "#FF8200",
                  "Arizona Diamondbacks" = "#5F259F",
                  "New York Yankees" = "#0C2340",
                  "San Francisco Giants" = "#FD5A1E")

#Create visual

waffle(vector_for_chart / 10, 
       rows = 20, 
       colors = chart_colors, 
       title = "Randy Johnson Strikeouts by Team",
       xlab = "1 Box = 10 Strikeouts")