#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X Before Age X Season/MLB")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(Lahman)

#Import data from csv

data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X Before Age X Season/MLB/Most WAR Before Age 23 Season.csv")

#Limit to top 10 spots

top_data <- data %>%
  mutate(Rank = rank(-WAR, ties.method = "min")) %>%
  filter(Rank <= 10)

#Combine columns for display

top_data <- top_data %>%
  mutate(chart_label = paste(Name,"\n", Team))

#Team Colors for display

fill_color <- c("Arizona Diamondbacks" = "#5F259F",
                "Atlanta Braves" = "#CE1141",
                "Baltimore Orioles" = "#DF4601",
                "Boston Red Sox" = "#BD3039",
                "BOS" = "#BD3039",
                "Chicago Cubs" = "#0E3386",
                "Chicago White Sox" = "#27251F",
                "Cincinnati Reds" = "#C6011F",
                "Cleveland Indians" = "#E31937",
                "Colorado Rockies" = "#33006F",
                "Detroit Tigers" = "#FA4616",
                "DET" = "#FA4616",
                "Houston Astros" = "#EB6E1F",
                "Kansas City Royals" = "#004687",
                "Los Angeles Angels of Anaheim" = "#BA0021",
                "LAA" = "#BA0021",
                "Los Angeles Dodgers" = "#005A9C",
                "Miami Marlins" = "#00A3E0",
                "Milwaukee Brewers" = "#FFC52F",
                "Minnesota Twins" = "#002B5C",
                "New York Mets" = "#FF5910",
                "New York Yankees" = "#0C2340",
                "NYY" = "#0C2340",
                "Oakland Athletics" = "#003831",
                "PHA" = "#003831",
                "Philadelphia Phillies" = "#E81828",
                "Pittsburgh Pirates" = "#FDB827",
                "St. Louis Cardinals" = "#C41E3A",
                "STL" = "#C41E3A",
                "San Diego Padres" = "#2F241D",
                "San Francisco Giants" = "#FD5A1E",
                "NYG" = "#27251F",
                "Seattle Mariners" = "#005C5C",
                "SEA" = "#005C5C",
                "Tampa Bay Rays" = "#8FBCE6",
                "Texas Rangers" = "#C0111F",
                "Toronto Blue Jays" = "#134A8E",
                "Washington Nationals" = "#AB0003",
                "Multiple Teams" = "seashell")

#Create visual

top_data %>%
  ggplot(aes(x = reorder(chart_label, WAR), y = WAR, fill = Team)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = format(round(WAR,1))), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(15, 30)) +
  scale_y_continuous(breaks = seq(15, 30, 1)) +
  scale_fill_manual(values = fill_color) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most WAR Before Age 23 Season",
       subtitle = "Using July 1st as Season Age Cutoff",
       x = "",
       y = "Total WAR Before Age 23 Season")