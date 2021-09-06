#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create HR by team vector

vector_for_chart <- c("San Diego Padres" = 1,
                      "Miami Marlins" = 129,
                      "Chicago Cubs" = 179,
                      "Atlanta Braves" = 3,
                      "Baltimore Orioles" = 12,
                      "Pittsburgh Pirates" = 7)

#Create colors for chart

chart_colors <- c("San Diego Padres" = "#473729",
                  "Miami Marlins" = "#00A3E0",
                  "Chicago Cubs" = "#0E3386",
                  "Atlanta Braves" = "#CE1141",
                  "Baltimore Orioles" = "#DF4601",
                  "Pittsburgh Pirates" = "#FDB827")

#Create visual

waffle(vector_for_chart, 
       rows = 17, 
       colors = chart_colors, 
       title = "Derrek Lee HR by Team",
       xlab = "1 Box = 1 Home Run")