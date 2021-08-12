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
TeamsFranchises.franchName AS Team, TeamsFranchises.franchID, SUM(Batting.H) AS H, GROUP_CONCAT(DISTINCT Batting.yearID) AS Seasons
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
                    ORDER BY SUM(Batting.H) DESC")

#Limit to top 10 spots

top_data <- data %>%
  mutate(Rank = rank(-H, ties.method = "min")) %>%
  filter(Rank <= 10)

#Create SeasonRange Column for Display

top_data <- top_data %>%
  mutate(SeasonRange = ifelse(nameFirst == 'Nap' & nameLast == 'Lajoie', '1902-1914', 
                              ifelse(nameFirst == 'Tris' & nameLast == 'Speaker', '1916-1926',
                                     ifelse(nameFirst == 'Earl' & nameLast == 'Averill', '1929-1939',
                                            ifelse(nameFirst == 'Joe' & nameLast == 'Sewell', '1920-1930',
                                                   ifelse(nameFirst == 'Charlie' & nameLast == 'Jamieson', '1919-1932',
                                                          ifelse(nameFirst == 'Lou' & nameLast == 'Boudreau', '1938-1950',
                                                                 ifelse(nameFirst == 'Omar' & nameLast == 'Vizquel', '1994-2004',
                                                                        ifelse(nameFirst == 'Ken' & nameLast == 'Keltner', '1937-1944, 1946-1949',
                                                                               ifelse(nameFirst == 'Kenny' & nameLast == 'Lofton', '1992-1996, 1998-2001, 2007','1904-1918'))))))))))

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
  coord_flip(ylim = c(1400, 2100)) +
  scale_y_continuous(breaks = seq(1400, 2100, 50)) +
  scale_fill_manual(values = c("Odd" = "#0C2340", "Even" = "#E31937")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most Hits in Cleveland Indians Franchise History",
       x = "",
       y = "Hits")