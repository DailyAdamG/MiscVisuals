#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)
library(Lahman)

#Get data from Lahman Database

data <- sqldf('SELECT People.playerID, GROUP_CONCAT(BattingPost.yearID) AS Seasons, People.nameFirst, People.nameLast,
GROUP_CONCAT(DISTINCT TeamsFranchises.franchName) AS Teams, GROUP_CONCAT(DISTINCT TeamsFranchises.franchID) AS Franchises, COUNT(BattingPost.yearID) AS Rings
                    FROM People
                    INNER JOIN BattingPost
                    ON People.playerID = BattingPost.PlayerID
                    INNER JOIN Teams
                    ON BattingPost.teamID = Teams.teamID
                    AND BattingPost.yearID = Teams.yearID
                    INNER JOIN TeamsFranchises
                    ON Teams.franchID = TeamsFranchises.franchID
                    INNER JOIN SeriesPost
                    ON BattingPost.yearID = SeriesPost.yearID
                    AND BattingPost.teamID = SeriesPost.teamIDWinner
                    WHERE BattingPost.round = "WS"
                    AND SeriesPost.round = "WS"
                    AND BattingPost.AB + BattingPost.BB + BattingPost.IBB + BattingPost.HBP +BattingPost.SH + BattingPost.SF >= 1
                    GROUP BY BattingPost.playerID')

#Combine Columns and Filter Data

top_data <- data %>%
  mutate(Rank = rank(-Rings, ties.method = "min"))  %>%
  filter(Rank <= 10) %>%
  mutate(chart_label = paste(nameFirst, nameLast, '\n', Franchises))

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
                "Boston Red Sox,New York Yankees" = 'red')

#Create visual

top_data %>%
  ggplot(aes(x = reorder(chart_label, Rings), y = Rings, fill = Teams)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Rings), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  scale_fill_manual(values = fill_color) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most World Series Wins",
       subtitle = "Minimum of 1 PA in the World Series to Qualify",
       x = "",
       y = "World Series Wins")