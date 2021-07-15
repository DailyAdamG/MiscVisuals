#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)
library(Lahman)

#Get data from Lahman Database

data <- sqldf("WITH sub1 AS (SELECT People.playerID, Batting.yearID, People.nameFirst, People.nameLast,
SUM(Batting.HR) AS HR, SUM(Batting.RBI) AS RBI, SUM(Batting.H * 1.0)/SUM(Batting.AB * 1.0) AS BA
                    FROM People
                    INNER JOIN Batting
                    ON People.playerID = Batting.PlayerID
                    INNER JOIN Teams
                    ON Batting.teamID = Teams.teamID
                    AND Batting.yearID = Teams.yearID
                    INNER JOIN TeamsFranchises
                    ON Teams.franchID = TeamsFranchises.franchID
                    WHERE TeamsFranchises.franchName = 'Houston Astros'
                    GROUP BY Batting.playerID, Batting.yearID
                    HAVING SUM(Batting.HR) >= 25
                    AND SUM(Batting.RBI) >= 100
                    AND BA >= .2895
              ORDER BY Batting.yearID)
              SELECT playerID, nameFirst, nameLast, COUNT(yearID) AS Seasons, GROUP_CONCAT(yearID) AS YearsID
              FROM sub1
              GROUP BY playerID
              ORDER BY Seasons DESC")

#Combine columns for display

data <- data %>%
  mutate(display_name = paste(nameFirst, nameLast,"\n", YearsID))

#Create highlight for Lance Berkman

data <- data %>%
  mutate(Highlight = ifelse(playerID == 'berkmla01', "yes", "no"))

#Create visual

data %>%
  ggplot(aes(x = reorder(display_name, Seasons), y = Seasons, fill = Highlight)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Seasons), fontface = "bold", hjust = -1) +
  coord_flip(ylim = c(0, 6)) +
  scale_y_continuous(breaks = seq(0, 6, 1)) +
  scale_fill_manual(values = c("yes" = "#EB6E1F", "no" = "#002D62")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Houston Astros with 25+ HR, 100+ RBI & .290+ Batting Average\nIn a Single Season",
       x = "",
       y = "Seasons")