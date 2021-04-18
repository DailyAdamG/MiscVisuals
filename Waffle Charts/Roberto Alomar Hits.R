#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Import data

hit_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts/Roberto Alomar Hits.csv")

#Grab only team and hit columns

vector_for_chart <- c("San Diego Padres" = 497,
                      "Toronto Blue Jays" = 832,
                      "Baltimore Orioles" = 496,
                      "Cleveland Indians" = 564,
                      "New York Mets" = 226,
                      "Chicago White Sox" = 75,
                      "Arizona Diamondbacks" = 34)

#Create colors for chart

chart_colors <- c("San Diego Padres" = "#462425",
                  "Toronto Blue Jays" = "#006BA6",
                  "Baltimore Orioles" = "#FC4C02",
                  "Cleveland Indians" = "#E31937",
                  "New York Mets" = "#002D72",
                  "Chicago White Sox" = "#27251F",
                  "Arizona Diamondbacks" = "#5F259F")

#Create visual

waffle(vector_for_chart/25, rows = 10, colors = chart_colors, title = "Roberto Alomar Career Hits by Team", xlab = "1 Box = 25 Hits")
