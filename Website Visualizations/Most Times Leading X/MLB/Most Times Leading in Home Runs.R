#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most Times Leading X/MLB")

#Load necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)
library(Lahman)

#Get data from Lahman Database

data <- sqldf('WITH sub1 AS (SELECT People.playerID, Batting.yearID, People.nameFirst, People.nameLast,
GROUP_CONCAT(TeamsFranchises.franchName) AS Teams, TeamsFranchises.franchID, SUM(Batting.HR) AS HR,
                    RANK() OVER (PARTITION BY Batting.yearID ORDER BY SUM(Batting.HR) DESC) AS Rank
                    FROM People
                    INNER JOIN Batting
                    ON People.playerID = Batting.PlayerID
                    INNER JOIN Teams
                    ON Batting.teamID = Teams.teamID
                    AND Batting.yearID = Teams.yearID
                    INNER JOIN TeamsFranchises
                    ON Teams.franchID = TeamsFranchises.franchID
                    GROUP BY Batting.playerID, Batting.yearID)
                    SELECT nameFirst, nameLast, GROUP_CONCAT(DISTINCT Teams) AS Teams, COUNT(HR) AS Times, COUNT(DISTINCT Teams) AS Franchises
                    FROM sub1
                    WHERE Rank = 1
                    GROUP BY playerID
                    ORDER BY Times DESC')

#Limit to top 10 spots

top_data <- data %>%
  mutate(Rank = rank(-Times, ties.method = "min")) %>%
  filter(Rank <= 10)

#Create Team Name for Players with More Than 1 Franchise

top_data <- top_data %>%
  mutate(DisplayFranchises = ifelse(Franchises >= 2, "Multiple Teams", Teams))

#Combine columns for display

top_data <- top_data %>%
  mutate(chart_label = paste(nameFirst,nameLast, "\n",DisplayFranchises))

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
                "Multiple Teams" = "seashell")

#Create visual

top_data %>%
  ggplot(aes(x = reorder(chart_label, Times), y = Times, fill = DisplayFranchises)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Times), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(0, 12)) +
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  scale_fill_manual(values = fill_color) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most Times Leading MLB in Home Runs",
       x = "",
       y = "Times Player Led the League in HR")