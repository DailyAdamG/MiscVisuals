#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create HR by team vector

vector_for_chart <- c("Houston Astros" = 225,
                      "Philadelphia Phillies" = 59,
                      "New York Mets" = 101,
                      "Atlanta Braves" = 37)

#Create colors for chart

chart_colors <- c("Houston Astros" = "#EB6E1F",
                  "Philadelphia Phillies" = "#E81828",
                  "New York Mets" = "#002D72",
                  "Atlanta Braves" = "#CE1141")

#Create visual

waffle(vector_for_chart, 
       rows = 18, 
       colors = chart_colors, 
       title = "Billy Wagner by Team",
       xlab = "1 Box = 1 Saves")