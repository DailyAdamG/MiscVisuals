#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)


#Create vectors for chart

vector_for_chart <- c("Toronto Blue Jays" = 125,
                      "San Diego Padres" = 84,
                      "Atlanta Braves" = 130,
                      "Tampa Bay Rays" = 99,
                      "Chicago Cubs" = 42,
                      "Los Angeles Dodgers" = 13)

#Create colors for chart

chart_colors <- c("Toronto Blue Jays" = "#006BA6",
                  "San Diego Padres" = "#E35205",
                  "Atlanta Braves" = "#CE1141",
                  "Tampa Bay Rays" = "#004637",
                  "Chicago Cubs" = "#0E3386",
                  "Los Angeles Dodgers" = "#A5ACAF")

#Create visual

waffle(vector_for_chart/5, rows = 10, colors = chart_colors,
       title = "Fred McGriff Career Home Runs by Team", xlab = "1 Box = 5 Home Runs")
