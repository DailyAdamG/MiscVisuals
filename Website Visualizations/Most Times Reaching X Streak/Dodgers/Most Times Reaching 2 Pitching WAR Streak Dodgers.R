#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most Times Reaching X Streak/Dodgers")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)
library(Lahman)

#Import data

data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most Times Reaching X Streak/Dodgers/Pitcher WAR Through 2020.csv")

#Get data from Lahman Database

data <- sqldf(' WITH sub1 AS(SELECT playerid, Season, Name, WAR,
RANK () OVER(PARTITION BY playerid ORDER BY Season) AS SeasonNumber
                    FROM data
                    GROUP BY playerid, Season),
              sub2 AS (SELECT *,
              SeasonNumber - RANK() OVER(PARTITION BY playerid ORDER BY Season) AS streakID
              FROM sub1
              WHERE WAR >= 2)
              SELECT MIN(Season) AS MIN,
              MAX(Season) AS MAX,
              Name,
              COUNT(streakID) AS Times
              FROM sub2
              GROUP BY playerid, streakID
              ORDER BY Times DESC')

#Combine Columns and Filter Data

top_data <- data %>%
  arrange(-Times, desc(Name)) %>%
  mutate(Rank = rank(-Times, ties.method = "min"))  %>%
  filter(Rank <= 10) %>%
  mutate(chart_label = paste(Name, "\n", MIN, "-", MAX))

#Create a column for alternating row colors in graph

top_data <- top_data %>%
  mutate(EvenOdd = ifelse(row_number() %% 2 == 1, "Odd", "Even"))

#Team Colors for display

fill_color <- c("Arizona Diamondbacks" = "#5F259F",
                "Atlanta Braves" = "#CE1141",
                "Baltimore Orioles" = "#DF4601",
                "Boston Red Sox" = "#BD3039",
                "Chicago Cubs" = "#0E3386",
                "Chicago White Sox" = "#27251F",
                "Cincinnati Reds" = "#C6011F",
                "Cleveland Indians" = "#E31937",
                "Colorado Rockies" = "#33006F;",
                "Detroit Tigers" = "#FA4616",
                "Houston Astros" = "#EB6E1F",
                "Kansas City Royals" = "#004687",
                "Los Angeles Angels" = "#BA0021",
                "Los Angeles Dodgers" = "#005A9C",
                "Miami Marlins" = "#00A3E0",
                "Milwaukee Brewers" = "#FFC52F",
                "Minnesota Twins" = "#002B5C",
                "New York Mets" = "#FF5910",
                "New York Yankees" = "#0C2340",
                "Oakland Athletics" = "#003831",
                "Philadelphia Phillies" = "#E81828",
                "Pittsburgh Pirates" = "#FDB827",
                "St. Louis Cardinals" = "#C41E3A",
                "San Diego Padres" = "#2F241D",
                "San Francisco Giants" = "#FD5A1E",
                "Seattle Mariners" = "#005C5C",
                "Tampa Bay Rays" = "#8FBCE6",
                "Texas Rangers" = "#C0111F",
                "Toronto Blue Jays" = "#134A8E",
                "Washington Nationals" = "#AB0003",
                "Multiple Teams" = "seashell",
                "Barry Bonds" = "#FD5A1E",
                "Alex Rodriguez" = "#0C2340",
                "Jimmie Foxx" = "#003831",
                "Albert Pujols" = "#C41E3A",
                "Carlos Delgado" = "#134A8E",
                "Sammy Sosa" = "#0E3386",
                "Lou Gehrig" = "#0C2340",
                "Eddie Mathews" = "#13274F",
                "Rafael Palmeiro" = "#DF4601",
                "Manny Ramirez" = "#BD3039",
                "Mike Schmidt" = "#E81828",
                "Jim Thome" = "#E31937")

#Create visual

top_data %>%
  ggplot(aes(x = reorder(chart_label, Times), y = Times, fill = EvenOdd)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Times), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(0, 13)) +
  scale_y_continuous(breaks = seq(0, 13, 1)) +
  scale_fill_manual(values = c("Odd" = "#005A9C", "Even" = "#A5ACAF")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most Consecutive Seasons with 2+ Pitching WAR in Dodgers History",
       x = "",
       y = "Consecutive 2+ WAR Seasons")