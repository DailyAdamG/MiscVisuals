#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(weights)
theme_set(theme_bw())

#Import data

war_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts/Gehringer WAR.csv")

#Creating a highlight for Gehringer

war_data <- war_data %>%
  mutate(Highlight = ifelse(Name == "Charlie Gehringer", "yes", "no"))

#Create visual

war_data %>%
  ggplot(aes(x = reorder(Name, desc(WAR)), y = WAR, fill = Highlight)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = rd(WAR, 1)), fontface = "bold", vjust = -0.5) +
  scale_y_continuous(breaks = seq(0, 80, 5)) +
  scale_fill_manual(values = c("yes" = "#FA4616", "no" = "#0C2340")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Most WAR by a Position Player in the 1930s",
       x = "Player",
       y = "WAR",
       fill = "Player")