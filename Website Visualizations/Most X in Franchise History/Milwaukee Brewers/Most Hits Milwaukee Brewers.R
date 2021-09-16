#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X in Franchise History/Milwaukee Brewers")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)
library(Lahman)

#Get data from Lahman Database

data <- sqldf("SELECT People.playerID, People.nameFirst, People.nameLast,
TeamsFranchises.franchName AS Team, TeamsFranchises.franchID, SUM(Batting.H) AS H, GROUP_CONCAT(DISTINCT Batting.yearID) AS Seasons
                    FROM People
                    INNER JOIN Batting
                    ON People.playerID = Batting.PlayerID
                    INNER JOIN Teams
                    ON Batting.teamID = Teams.teamID
                    AND Batting.yearID = Teams.yearID
                    INNER JOIN TeamsFranchises
                    ON Teams.franchID = TeamsFranchises.franchID
                    WHERE TeamsFranchises.franchName = 'Milwaukee Brewers'
                    GROUP BY Batting.playerID
                    ORDER BY SUM(Batting.H) DESC")

#Limit to top 10 spots

top_data <- data %>%
  mutate(Rank = rank(-H, ties.method = "min")) %>%
  filter(Rank <= 10)

#Create SeasonRange Column for Display

top_data <- top_data %>%
  mutate(SeasonRange = ifelse(nameFirst == 'Robin' & nameLast == 'Yount', '1974-1993', 
                              ifelse(nameFirst == 'Paul' & nameLast == 'Molitor', '1978-1992',
                                     ifelse(nameFirst == 'Ryan' & nameLast == 'Braun', '2007-2020',
                                            ifelse(nameFirst == 'Cecil' & nameLast == 'Cooper', '1977-1987',
                                                   ifelse(nameFirst == 'Jim' & nameLast == 'Gantner', '1976-1992',
                                                          ifelse(nameFirst == 'Geoff' & nameLast == 'Jenkins', '1998-2007',
                                                                 ifelse(nameFirst == 'Don' & nameLast == 'Money', '1973-1983',
                                                                        ifelse(nameFirst == 'Ben' & nameLast == 'Oglivie', '1978-1986',
                                                                               ifelse(nameFirst == 'B. J.' & nameLast == 'Surhoff', '1987-1995','1973-1986'))))))))))

#Combine columns for display

top_data <- top_data %>%
  mutate(chart_label = paste(nameFirst,nameLast, "\n", SeasonRange))

#Create a column for alternating row colors in graph

top_data <- top_data %>%
  mutate(EvenOdd = ifelse(row_number() %% 2 == 1, "Odd", "Even"))

#Create visual

top_data %>%
  ggplot(aes(x = reorder(chart_label, H), y = H, fill = EvenOdd)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = H), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(1000, 3200)) +
  scale_y_continuous(breaks = seq(1000, 3200, 100)) +
  scale_fill_manual(values = c("Odd" = "#FFC52F", "Even" = "#12284B")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most Hits in Milwaukee Brewers Franchise History",
       x = "",
       y = "Hits")