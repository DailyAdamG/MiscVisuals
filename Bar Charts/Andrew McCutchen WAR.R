#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(weights)
theme_set(theme_bw())
library(Lahman)
library(sqldf)

#Import data

cutch_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts/Andrew McCutchen WAR.csv")


WAR_Leaders <- cutch_data %>%
  arrange(-Season, Team, -WAR) %>%
  group_by(Season, Team) %>%
  mutate(Rank = dense_rank(-WAR)) %>%
  group_by(playerid) %>%
  mutate(Season_number = dense_rank(-Season)) %>%
  filter(Rank == 1) %>%
  mutate(StreakID = Season_number - dense_rank(-Season))

chart_data <- sqldf('SELECT MIN(Season) AS MIN,
      MAX(Season) AS MAX,
      COUNT(StreakID) AS Times,
      Name
      FROM WAR_Leaders
      GROUP BY playerID, streakID
      HAVING COUNT(StreakID) >= 7
      AND MIN(Season) >= 1947
      ORDER BY Times DESC,
      MIN DESC')

#Creating a highlight for Andrew McCutchen

chart_data <- chart_data %>%
  mutate(Highlight = ifelse(Name == "Andrew McCutchen", "yes", "no"))

#Combine season and player columns

chart_data <- chart_data %>%
  mutate(Range = paste(chart_data$MIN, chart_data$MAX, sep = "-")) %>%
  mutate(Label = paste(Range, Name, sep = ' '))

#Create visual

chart_data %>%
  ggplot(aes(x = reorder(Label, Times), y = Times, fill = Highlight)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Times), fontface = "bold", hjust = -0.3) +
  coord_flip() + 
  scale_y_continuous(breaks = seq(0, 15, 1)) +
  scale_fill_manual(values = c("yes" = "#FDB827", "no" = "#27251F")) +
  theme(legend.position = "none") +
  labs(title = "Most Consecutive Times Leading Team in Position Player WAR",
       subtitle = "Since Integration in 1947",
       x = "",
       y = "Consecutive Times Leading Team in WAR")