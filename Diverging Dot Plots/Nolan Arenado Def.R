#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Diverging Dot Plots")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

#Import data

def_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Diverging Dot Plots/Nolan Arenado Def.csv")

#Create a new column for above and below average DRS

def_data <- def_data %>%
  mutate(def_type = ifelse(DRS > 0, "Above Average", "Below Average")) %>%
  arrange(-DRS)

#Change to factor to keep order

def_data$Name <- factor(def_data$Name, levels = def_data$Name)

#Create visual for UZR

def_data %>%
  ggplot(aes(x = Name, y = DRS, label = DRS)) +
  geom_point(stat = "identity", aes(col = def_type), size = 7) +
  geom_text(color = "white", size = 3) +
  scale_color_manual(name = "DRS", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("Above Average"="#00ba38", "Below Average"="#f8766d")) +
  scale_x_discrete(limits = rev) +
  coord_flip() +
  labs(title = "3B DRS totals since 2013",
       caption = "Minimum of 3000 Innings to Qualify")