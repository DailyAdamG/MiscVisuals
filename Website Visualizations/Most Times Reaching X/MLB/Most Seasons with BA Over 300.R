#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most Times Reaching X/MLB")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)
library(Lahman)

#Get data from Lahman Database

data <- sqldf('SELECT People.playerID, Batting.yearID, People.nameFirst, People.nameLast,
GROUP_CONCAT(TeamsFranchises.franchName) AS Teams, TeamsFranchises.franchID, SUM(Batting.H) AS H, SUM(Batting.AB) AS AB, 
SUM(Batting.BB) AS BB, SUM(Batting.HBP) AS HBP, SUM(Batting.SF) AS SF, SUM(Batting.SH) AS SH, AVG(Teams.G) AS TeamG
                    FROM People
                    INNER JOIN Batting
                    ON People.playerID = Batting.PlayerID
                    INNER JOIN Teams
                    ON Batting.teamID = Teams.teamID
                    AND Batting.yearID = Teams.yearID
                    INNER JOIN TeamsFranchises
                    ON Teams.franchID = TeamsFranchises.franchID
                    GROUP BY Batting.playerID, Batting.yearID')

#Fill null values with 0s

data[is.na(data)] <- 0

#Filter to only include qualified hitters and BA of .300 or higher

data <- data %>%
  mutate(PA = AB + BB + HBP + SF + SH) %>%
  mutate(BA = round((H / AB),3)) %>%
  filter(PA >= 3.1 * TeamG) %>%
  filter(BA >= .300)

#Get the count of .300 seasons by player

season_count_data <- data %>%
  group_by(playerID, nameFirst, nameLast) %>%
  count(playerID) %>%
  ungroup()

#Combine Columns and Filter Data

top_data <- season_count_data %>%
  arrange(-n) %>%
  mutate(Rank = rank(-n, ties.method = "min"))  %>%
  filter(Rank <= 9) %>%
  mutate(chart_label = paste(nameFirst, nameLast))

#Team Colors for display

fill_color <- c("Cap Anson" = "#0E3386",
                "Ty Cobb" = "#FA4616",
                "Tris Speaker" = "#E31937",
                "Stan Musial" = "#C41E3A",
                "Eddie Collins" = "#003831",
                "Pete Rose" = "#C6011F",
                "Tony Gwynn" = "#2F241D",
                "Babe Ruth" = "#0C2340",
                "Honus Wagner" = "#FDB827",
                "Arizona Diamondbacks" = "#5F259F",
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
                "Multiple Teams" = "seashell")

#Create visual

top_data %>%
  ggplot(aes(x = reorder(chart_label, n), y = n, fill = chart_label)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = n), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(10, 22)) +
  scale_y_continuous(breaks = seq(10, 22, 1)) +
  scale_fill_manual(values = fill_color) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most Seasons with Batting Average of .300 or Higher",
       subtitle = "Must have 3.1 PA/Team G or More to Qualify",
       x = "",
       y = "Seasons with Batting Average of .300 or Higer")