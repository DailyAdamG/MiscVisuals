#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create vector for visual

vector_for_chart <- c("Florida Marlins" = 450,
                      "St. Louis Cardinals" = 973,
                      "Boston Red Sox" = 172,
                      "Atlanta Braves" = 339,
                      "Detroit Tigers" = 136,
                      "San Francisco Giants" = 182,
                      "Cincinnati Reds" = 75)

#Create colors for chart

chart_colors <- c("Florida Marlins" = "#009CA6",
                  "St. Louis Cardinals" = "#C41E3A",
                  "Boston Red Sox" = "#BD3039",
                  "Atlanta Braves" = "#002855",
                  "Detroit Tigers" = "#FA4616",
                  "San Francisco Giants" = "#27251F",
                  "Cincinnati Reds" = "#C6011F")
#Create visual

waffle(vector_for_chart/10, rows = 15, colors = chart_colors, title = "Edgar Renteria Career Hits by Team", xlab = "1 Box = 10 Hits")