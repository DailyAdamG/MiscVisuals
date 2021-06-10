#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X in Span to Start Career/MLB")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)
library(Lahman)

#Get data from Lahman Database

data <- sqldf('SELECT People.playerID, Batting.yearID, People.nameFirst, People.nameLast,
GROUP_CONCAT(TeamsFranchises.franchName) AS Teams, TeamsFranchises.franchID, SUM(Batting.HR) AS HR
                    FROM People
                    INNER JOIN Batting
                    ON People.playerID = Batting.PlayerID
                    INNER JOIN Teams
                    ON Batting.teamID = Teams.teamID
                    AND Batting.yearID = Teams.yearID
                    INNER JOIN TeamsFranchises
                    ON Teams.franchID = TeamsFranchises.franchID
                    GROUP BY Batting.playerID, Batting.yearID
                    ORDER BY SUM(Batting.HR) DESC')

#Calculate the totals and find the season the span started

data <- data %>%
  arrange(playerID, yearID) %>%
  group_by(playerID) %>%
  mutate(HR_Total = rollapply(HR, width = 3, FUN = sum, align = "left",partial = TRUE)) %>%
  mutate(firstYear = lead(yearID, 1)) %>%
  mutate(secondYear = lead(yearID,2)) %>%
  mutate(Range = secondYear - yearID) %>%
  mutate(firstYearTeam = lead(Teams,1)) %>%
  mutate(secondYearTeam = lead(Teams,2)) %>%
  mutate(OneFranchise = ifelse(Teams == firstYearTeam & Teams == secondYearTeam, "Yes", "No")) %>%
  ungroup()

#Choose only the columns needed for visual

display_data <- sqldf('SELECT MIN(yearID) AS MIN,
      secondYear AS MAX,
      Range,
      playerID,
      nameFirst,
      nameLast,
      Teams,
      HR_Total,
      OneFranchise
      FROM data
      GROUP BY playerID
      ORDER BY HR_Total DESC')


#Limit to top 10 spots

top_data <- display_data %>%
  mutate(Rank = rank(-HR_Total, ties.method = "min")) %>%
  filter(Rank <= 10)

#Create Team Name for Players with More Than 1 Franchise

top_data <- top_data %>%
  mutate(DisplayFranchises = ifelse(OneFranchise == "No", "Multiple Teams", Teams))

#Combine columns for display

top_data <- top_data %>%
  mutate(chart_label = paste(nameFirst,nameLast, "\n", MIN, "-", MAX, "\n",DisplayFranchises))

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
  ggplot(aes(x = reorder(chart_label, HR_Total), y = HR_Total, fill = DisplayFranchises)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = HR_Total), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(80, 120)) +
  scale_y_continuous(breaks = seq(80, 120, 5)) +
  scale_fill_manual(values = fill_color) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most Home Runs in a 3 Season Span \nto Start Career",
       x = "",
       y = "Home Runs")