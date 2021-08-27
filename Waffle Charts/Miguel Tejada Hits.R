#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create vector for visual

vector_for_chart <- c("Oakland Athetics" = 968,
                      "Baltimore Orioles" = 876,
                      "Houston Astros" = 378,
                      "San Diego Padres" = 63,
                      "San Francisco Giants" = 77,
                      "Kansas City Royals" = 45)

#Create colors for chart

chart_colors <- c("Oakland Athletics" = "#003831",
                  "Baltimore Orioles" = "#DF4601",
                  "Houston Astros" = "#9A3324",
                  "San Diego Padres" = "#FFC425",
                  "San Francisco Giants" = "#27251F",
                  "Kansas City Royals" = "#004687")
#Create visual

waffle(vector_for_chart/ 5, rows = 20, colors = chart_colors, title = "Miguel Tejada Career Hits by Team", xlab = "1 Box = 5 Hits")