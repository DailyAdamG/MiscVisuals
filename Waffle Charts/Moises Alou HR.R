#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create HR by team vector

vector_for_chart <- c("Montreal Expos" = 84,
                      "Florida Marlins" = 23,
                      "Houston Astros" = 95,
                      "Chicago Cubs" = 76,
                      "San Francisco Giants" = 41,
                      "New York Mets" = 13)

#Create colors for chart

chart_colors <- c("Montreal Expos" = "#E4002B",
                  "Florida Marlins" = "#00A3E0",
                  "Houston Astros" = "#EB6E1F",
                  "Chicago Cubs" = "#0E3386",
                  "San Francisco Giants" = "#27251F",
                  "New York Mets" = "#FF5910")

#Create visual

waffle(vector_for_chart, 
       rows = 18,
       colors = chart_colors,
       title = "Moises Alou HR by Team",
       xlab = "1 Box = 1 Home Run")