#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X in Franchise History/Cleveland Indians")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)
library(Lahman)

#Get data from Lahman Database

data <- sqldf("SELECT People.playerID, People.nameFirst, People.nameLast,
TeamsFranchises.franchName AS Team, TeamsFranchises.franchID, SUM(Batting.HR) AS HR, GROUP_CONCAT(DISTINCT Batting.yearID) AS Seasons
                    FROM People
                    INNER JOIN Batting
                    ON People.playerID = Batting.PlayerID
                    INNER JOIN Teams
                    ON Batting.teamID = Teams.teamID
                    AND Batting.yearID = Teams.yearID
                    INNER JOIN TeamsFranchises
                    ON Teams.franchID = TeamsFranchises.franchID
                    WHERE TeamsFranchises.franchName = 'Cleveland Indians'
                    GROUP BY Batting.playerID
                    ORDER BY SUM(Batting.HR) DESC")

#Limit to top 10 spots

top_data <- data %>%
  mutate(Rank = rank(-HR, ties.method = "min")) %>%
  filter(Rank <= 10)

#Create SeasonRange Column for Display

top_data <- top_data %>%
  mutate(SeasonRange = ifelse(nameFirst == 'Jim' & nameLast == 'Thome', '1991-2002, 2011', 
                              ifelse(nameFirst == 'Albert' & nameLast == 'Belle', '1989-1996',
                                     ifelse(nameFirst == 'Manny' & nameLast == 'Ramirez', '1993-2000',
                                            ifelse(nameFirst == 'Earl' & nameLast == 'Averill', '1929-1939',
                                                   ifelse(nameFirst == 'Hal' & nameLast == 'Trosky', '1933-1941',
                                                          ifelse(nameFirst == 'Carlos' & nameLast == 'Santana', '2010-2017, 2019-2020',
                                                                 ifelse(nameFirst == 'Larry' & nameLast == 'Doby', '1947-1955, 1958',
                                                                        ifelse(nameFirst == 'Andre' & nameLast == 'Thornton', '1977-1987',
                                                                               ifelse(nameFirst == 'Travis' & nameLast == 'Hafner', '2003-2012','1947-1956'))))))))))

#Combine columns for display

top_data <- top_data %>%
  mutate(chart_label = paste(nameFirst,nameLast, "\n", SeasonRange))

#Create a column for alternating row colors in graph

top_data <- top_data %>%
  mutate(EvenOdd = ifelse(row_number() %% 2 == 1, "Odd", "Even"))

#Create visual

top_data %>%
  ggplot(aes(x = reorder(chart_label, HR), y = HR, fill = EvenOdd)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = HR), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(150, 350)) +
  scale_y_continuous(breaks = seq(150, 350, 25)) +
  scale_fill_manual(values = c("Odd" = "#0C2340", "Even" = "#E31937")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most Home Runs in Cleveland Indians Franchise History",
       x = "",
       y = "Home Runs")