#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create vector for visual

vector_for_chart <- c("Kansas City Royals" = 894,
                      "Oakland Athletics" = 165,
                      "Boston Red Sox" = 730,
                      "New York Yankees" = 636,
                      "Detroit Tigers" = 146,
                      "Tampa Bay Rays" = 152,
                      "Cleveland Indians" = 46)

#Create colors for chart

chart_colors <- c("Kansas City Royals" = "#004687",
                  "Oakland Athletics" = "#003831",
                  "Boston Red Sox" = "#BD3039",
                  "New York Yankees" = "#C4CED3",
                  "Detroit Tigers" = "#FA4616",
                  "Tampa Bay Rays" = "#8FBCE6",
                  "Cleveland Indians" = "#E31937")
#Create visual

waffle(vector_for_chart/ 5, rows = 20, colors = chart_colors, title = "Johnny Damon Career Hits by Team", xlab = "1 Box = 5 Hits")