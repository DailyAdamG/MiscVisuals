#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most Times Leading X/MLB")

#Load necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)

#Import data

data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most Times Leading X/MLB/Position Player WAR Through 2020.csv")

#Find the leaders for each season

leaders <- data %>%
  group_by(Season) %>%
  arrange(-WAR) %>%
  mutate(Rank = rank(-WAR, ties.method = "min")) %>%
  filter(Rank == 1) %>%
  ungroup()

#Find the number of times a player led MLB in WAR

display_data <- leaders %>%
  group_by(Name,playerid) %>%
  count(Rank) %>%
  arrange(-n)

#Limit to players to lead 4 or more times

display_data <- display_data %>%
  filter(n >= 5)

#Create visual

display_data %>%
  ggplot(aes(x = reorder(Name, n), y = n, fill = Name)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = n), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  scale_fill_brewer(palette = "Set1") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most Times Leading MLB in WAR",
       subtitle = "Considered Tied for Lead if Equal to the Tenth Decimal Place",
       x = "",
       y = "Times Player Led the League in WAR")