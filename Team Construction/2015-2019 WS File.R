#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Team Construction")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

#Import data

team_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Team Construction/2010-2019 World Series Winners.csv")

#Filter data

team_data <- team_data %>%
filter(Team == "2015 Kansas City Royals" | 
         Team == "2016 Chicago Cubs" | 
         Team == "2017 Houston Astros" | 
         Team == "2018 Boston Red Sox" |
         Team == "2019 Washington Nationals")

#Re-ordering season for bar chart

ordered_position <- factor(team_data$Position_Group, levels = c("Starting Pitcher", "Bullpen", "Catcher",
                                                           "Corner Infield", "Middle Infield", "Outfield", "DH/Top Reserve", "Bench"))

#Create color scheme for fill

fill_color <- c("2015 Kansas City Royals" = "#004687",
                  "2016 Chicago Cubs" = "#0E3386",
                  "2017 Houston Astros" = "#EB6E1F",
                  "2018 Boston Red Sox" = "#BD3039",
                  "2019 Washington Nationals" = "#FFFFFF")

#Create color scheme for outline

outline_color <- c("2015 Kansas City Royals" = "#BD9B60",
                   "2016 Chicago Cubs" = "#CC3433",
                   "2017 Houston Astros" = "#002D62",
                   "2018 Boston Red Sox" = "#0C2340",
                   "2019 Washington Nationals" = "#AB0003")

#Create visual

team_data %>%
ggplot(aes(x = ordered_position, y = WAR, fill = Team, color = Team)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = fill_color) +
  scale_color_manual(values = outline_color) +
  scale_y_continuous(limits = c(-2,11), breaks = seq(-2, 11, 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Roster Construction of World Series Champions from 2015-2019",
       subtitle = "Positions determined from Baseball Reference Rosters",
       x = "Position Group")