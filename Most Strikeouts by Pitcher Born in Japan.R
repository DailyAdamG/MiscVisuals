#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X in Span/MLB")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)
library(weights)
library(Lahman)

#Get data from Lahman Database

data <- sqldf('SELECT People.nameFirst, People.nameLast, GROUP_CONCAT(DISTINCT Pitching.teamID) AS Teams, 
MIN(yearID) AS MIN, MAX(yearID) AS MAX, SUM(Pitching.SO) AS Strikeouts
              FROM People
              INNER JOIN Pitching
              ON People.playerID = Pitching.playerID
              WHERE People.birthCountry = "Japan"
              GROUP BY People.playerID
              ORDER BY Strikeouts DESC')

#Limit to top 10 spots

top_data <- data %>%
  mutate(Rank = rank(-Strikeouts, ties.method = "min")) %>%
  filter(Rank <= 10)

#Combine columns for display

top_data <- top_data %>%
  mutate(chart_label = paste(nameFirst,nameLast, "\n", MIN, "-", MAX))

#Team Colors for display

fill_color <- c("Nomo" = "#005A9C",
                "Darvish" = "#FFC425",
                "Tanaka" = "#0C2340",
                "Kuroda" = "#005A9C",
                "Maeda" = "#002B5C",
                "Matsuzaka" = "#BD3039",
                "Iwakuma" = "#005C5C",
                "Ohka" = "#67ABE5",
                "Uehara" = "#BD3039",
                "Yoshii" = "#FF5910",
                "Hasegawa" = "#005C5C")
                

#Create visual

top_data %>%
  ggplot(aes(x = reorder(chart_label, Strikeouts), y = Strikeouts, fill = nameLast)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Strikeouts), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(0, 2000)) +
  scale_y_continuous(breaks = seq(0, 2000, 200)) +
  scale_fill_manual(values = fill_color) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most Career Strikeouts in MLB by a Pitcher born in Japan",
       subtitle = "Data only includes completed seasons",
       x = "",
       y = "Strikeouts")