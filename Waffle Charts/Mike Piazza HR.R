#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create vector for visual

vector_for_chart <- c("Los Angeles Dodgers" = 177,
                      "New York Mets" = 220,
                      "San Diego Padres" = 22,
                      "Oakland Athetics" = 8)

#Create colors for chart

chart_colors <- c("Los Angeles Dodgers" = "#005A9C",
                  "New York Mets" = "#FF5910",
                  "San Diego Padres" = "#FFC425",
                  "Oakland Athletics" = "#003831")
#Create visual

waffle(vector_for_chart, rows = 20, colors = chart_colors, title = "Mike Piazza Career HR by Team", xlab = "1 Box = 1 HR")