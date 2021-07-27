#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X After Age X Season/MLB")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(Lahman)

#Get data from Lahman Database

data <- sqldf('WITH sub1 AS (
SELECT People.playerID, Batting.yearID, CASE WHEN birthMonth <= 6 THEN Batting.yearID - birthYear ELSE Batting.yearID - birthYear - 1 END AS SeasonAge, People.nameFirst, People.nameLast,
TeamsFranchises.franchName AS Teams, TeamsFranchises.franchID, SUM(Batting.HR) AS HR
                    FROM People
                    INNER JOIN Batting
                    ON People.playerID = Batting.PlayerID
                    INNER JOIN Teams
                    ON Batting.teamID = Teams.teamID
                    AND Batting.yearID = Teams.yearID
                    INNER JOIN TeamsFranchises
                    ON Teams.franchID = TeamsFranchises.franchID
                    GROUP BY Batting.playerID, Batting.yearID
                    HAVING (CASE WHEN birthMonth <= 6 THEN Batting.yearID - birthYear ELSE Batting.yearID - birthYear - 1 END > 35))
              SELECT playerID,
              nameFirst,
              nameLast,
              GROUP_CONCAT(DISTINCT Teams) AS Teams,
              GROUP_CONCAT(DISTINCT franchID) AS TeamIDs,
              SUM(HR) AS HR_Total
              FROM sub1
              GROUP BY playerID
              ORDER BY HR_Total DESC')

#Limit to top 10 spots

top_data <- data %>%
  mutate(Rank = rank(-HR_Total, ties.method = "min")) %>%
  filter(Rank <= 10)

#Combine columns for display

top_data <- top_data %>%
  mutate(Name = paste(nameFirst, nameLast)) %>%
  mutate(chart_label = paste(nameFirst,nameLast,"\n", TeamIDs))

#Team Colors for display

fill_color <- c("Arizona Diamondbacks" = "#5F259F",
                "Atlanta Braves" = "#CE1141",
                "Baltimore Orioles" = "#DF4601",
                "Boston Red Sox" = "#BD3039",
                "Chicago Cubs" = "#0E3386",
                "Chicago White Sox" = "#27251F",
                "Cincinnati Reds" = "#C6011F",
                "Cleveland Indians" = "#E31937",
                "Colorado Rockies" = "#33006F",
                "Detroit Tigers" = "#FA4616",
                "Houston Astros" = "#EB6E1F",
                "Kansas City Royals" = "#004687",
                "Los Angeles Angels of Anaheim" = "#BA0021",
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
                "Hank Aaron" = "#13274F",
                "Darrell Evans" = "#FA4616",
                "Rafael Palmeiro" = "#C0111F",
                "Carlton Fisk" = "#27251F",
                "David Ortiz" = "#BD3039",
                "Ted Williams" = "#BD3039",
                "Andres Galarraga" = "#33006F",
                "Babe Ruth" = "#0C2340",
                "Raul Ibanez" = "#005C5C")

#Create visual

top_data %>%
  ggplot(aes(x = reorder(chart_label, HR_Total), y = HR_Total, fill = Name)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = HR_Total), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(140, 270)) +
  scale_y_continuous(breaks = seq(140, 270, 10)) +
  scale_fill_manual(values = fill_color) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most Home Runs After Age 35 Season",
       subtitle = "Using July 1st as Season Age Cutoff",
       x = "",
       y = "Total Home Runs Hit After Age 35 Season")