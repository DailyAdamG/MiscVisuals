#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create HR by team vector

vector_for_chart <- c("Los Angeles Angels" = 121,
                      "St. Louis Cardinals" = 241,
                      "San Diego Padres" = 1,
                      "Chicago Cubs" = 19,
                      "Milwaukee Brewers" = 8,
                      "Cincinnati Reds" = 3)

#Create colors for chart

chart_colors <- c("Los Angeles Angels" = "#6CACE4",
                  "St. Louis Cardinals" = "#C41E3A",
                  "San Diego Padres" = "#473729",
                  "Chicago Cubs" = "#0E3386",
                  "Milwaukee Brewers" = "#FFC52F",
                  "Cincinnati Reds" = "#C6011F")

#Create visual

waffle(vector_for_chart, 
       rows = 17,
       colors = chart_colors,
       title = "Jim Edmonds HR by Team",
       xlab = "1 Box = 1 Home Run")