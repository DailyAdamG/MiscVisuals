#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Diverging Dot Plots")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(weights)
theme_set(theme_bw())

#Import data

def_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Diverging Dot Plots/Grandal Framing.csv")

#Filter to only include above 1500 Innings Caught

def_data <- def_data %>%
  filter(Inn >= 1500)

#Create a new column for above and below average FRM

def_data <- def_data %>%
  mutate(def_type = ifelse(FRM > 0, "Above Average", "Below Average")) %>%
  arrange(-FRM)

#Change to factor to keep order

def_data$Name <- factor(def_data$Name, levels = def_data$Name)

#Create visual for FRM

def_data %>%
  ggplot(aes(x = Name, y = FRM, label = rd(FRM,1))) +
  geom_point(stat = "identity", aes(col = def_type), size = 9) +
  geom_text(color = "white", size = 2.5) +
  scale_color_manual(name = "Framing Runs", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("Above Average"="#00ba38", "Below Average"="#f8766d")) +
  scale_x_discrete(limits = rev) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-150, 150, 30)) +
  labs(title = "Active Catcher Framing Totals (Minimum 1500 Innings Caught)",
       caption = "Figures from FanGraphs",
       x = "Player")