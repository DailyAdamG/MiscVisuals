#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create vector for visual

vector_for_chart <- c("Minnesota Twins" = 788,
                      "Chicago White Sox" = 58,
                      "Pittsburgh Pirates" = 722,
                      "Toronto Blue Jays" = 126,
                      "Houston Astros" = 11,
                      "Detroit Tigers" = 110)

#Create colors for chart

chart_colors <- c("Minnesota Twins" = "#D31145",
                  "Chicago White Sox" = "#C4CED4",
                  "Pittsburgh Pirates" = "#FDB827",
                  "Toronto Blue Jays" = "#134A8E",
                  "Houston Astros" = "#EB6E1F",
                  "Detroit Tigers" = "#0C2340")
#Create visual

waffle(vector_for_chart/5, rows = 18, 
       colors = chart_colors, 
       title = "Francisco Liriano Career Strikeouts by Team", 
       xlab = "1 Box = 5 Strikeouts")