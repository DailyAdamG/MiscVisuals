#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts")

#Load necessary libraries

library(tidyverse)
library(waffle)

#Import data

hr_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Waffle Charts/Aaron Judge HR.csv")

#Highlight Aaron Judge HR

hr_data <- hr_data %>%
  mutate(Highlight = ifelse(player_name == "Judge, Aaron", "Aaron Judge", "Other"))

#Summarize data

hr_data_summary <- hr_data %>%
  group_by(Highlight) %>%
  tally() %>%
  ungroup()

#Vector for chart

chart_vector <- c("Aaron Judge" = 17,
                  "Everyone Else" = 106)

#Colors for chart

chart_colors <- c("Aaron Judge" = "#0C2340",
                  "Everyone Else" = "grey")

#Create visual

waffle(chart_vector, rows = 10, colors = chart_colors, 
       title = "Home Runs with Exit Velocity of 115 MPH or More \n in the Statcast Era",
       xlab = "One HR per Box")