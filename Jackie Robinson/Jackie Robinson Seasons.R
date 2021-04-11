#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Jackie Robinson")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

#Import data

dodger_season_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Jackie Robinson/Jackie Robinson Seasons.csv")

#Show top seasons

top_seasons <- top_n(dodger_season_data, 10)

#Add Jackie Robinson highlight column

top_seasons <- top_seasons %>%
  mutate(Highlight = ifelse(playerid == "1011070", "yes", "no"))

#Create visual

top_seasons %>%
  ggplot(aes(x = reorder(PlayerSeason, -WAR), y = WAR, fill = Highlight)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  scale_fill_manual(values = c("yes" = "#005A9C", "no" = "#A5ACAF")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Best Position Player Seasons in Dodgers Franchise History",
       subtitle = "Jackie Robinson seasons highlighted in blue",
       caption = "WAR figures from FanGraphs.com",
       x = "Player and Season",
       y = "WAR") +
  theme(legend.position = "none")