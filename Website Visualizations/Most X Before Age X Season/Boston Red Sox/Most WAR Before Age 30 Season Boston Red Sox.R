
#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X Before Age X Season/Boston Red Sox")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(Lahman)

#Import data from csv

data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X Before Age X Season/Boston Red Sox/Most WAR Before Age 30 Season Boston Red Sox.csv")

#Limit to top 10 spots

top_data <- data %>%
  mutate(Rank = rank(-WAR, ties.method = "min")) %>%
  filter(Rank <= 10)

#Create a column for alternating row colors in graph

top_data <- top_data %>%
  mutate(EvenOdd = ifelse(row_number() %% 2 == 1, "Odd", "Even"))

#Create visual

top_data %>%
  ggplot(aes(x = reorder(Name, WAR), y = WAR, fill = EvenOdd)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = format(round(WAR,1))), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(30, 70)) +
  scale_y_continuous(breaks = seq(30, 70, 2)) +
  scale_fill_manual(values = c("Odd" = "#BD3039", "Even" = "#0C2340")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most WAR Before Age 30 Season\nBoston Red Sox Franchise History",
       subtitle = "Using July 1st as Season Age Cutoff",
       x = "",
       y = "WAR")