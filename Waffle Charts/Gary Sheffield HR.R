#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create HR by team vector

vector_for_chart <- c("Milwaukee Brewers" = 21,
                      "San Diego Padres" = 43,
                      "Miami Marlins" = 122,
                      "Los Angeles Dodgers" = 129,
                      "Atlanta Braves" = 64,
                      "New York Yankees" = 76,
                      "Detroit Tigers" = 44,
                      "New York Mets" = 10)

#Create colors for chart

chart_colors <- c("Milwaukee Brewers" = "#FFC52F",
                  "San Diego Padres" = "#473729",
                  "Miami Marlins" = "#00A3E0",
                  "Los Angeles Dodgers" = "#005A9C",
                  "Atlanta Braves" = "#CE1141",
                  "New York Yankees" = "#C4CED3",
                  "Detroit Tigers" = "#FA4616",
                  "New York Mets" = "#002D72")

#Create visual

waffle(vector_for_chart, 
       rows = 20, 
       colors = chart_colors, 
       title = "Gary Sheffield HR by Team",
       xlab = "1 Box = 1 Home Run")