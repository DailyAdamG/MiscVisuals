#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Loading libraries

library(tidyverse)
library(waffle)

#Create team and WAR vectors

vector_for_chart <- c("Cleveland Indians" = 19.2,
                      "Montreal Expos" = 2.3,
                      "Chicago White Sox" = 4.9,
                      "Los Angeles Angels" = 6.8,
                      "Boston Red Sox" = 0.5,
                      "New York Yankees" = 2.3,
                      "Oakland Athletics" = 6.6,
                      "New York Mets" = 7.7,
                      "Braves/Twins/Rangers" = 0.7)

#Create colors for chart

chart_colors <- c("Cleveland Indians" = "#E31937",
                  "Montreal Expos" = "#003087",
                  "Chicago White Sox" = "#27251F",
                  "Los Angeles Angels" = "#6CACE4",
                  "Boston Red Sox" = "#BD3039",
                  "New York Yankees" = "#0C2340",
                  "Oakland Athletics" = "#003831",
                  "New York Mets" = "#FF5910",
                  "Braves/Twins/Rangers" = "tan")

#Create visual

waffle(vector_for_chart/0.5, rows = 10, 
       colors = chart_colors, 
       title = "Bartolo Colon WAR by Team", 
       xlab = "1 Box = 0.5 WAR")