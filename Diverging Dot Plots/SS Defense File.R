#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Diverging Dot Plots")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

#Import data

def_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Diverging Dot Plots/SS Def 2012-2020.csv")

#Create a new column for above and below average UZR

def_data <- def_data %>%
  mutate(def_type = ifelse(UZR > 0, "Above Average", "Below Average")) %>%
  arrange(-UZR)

#Change to factor to keep order

def_data$Name <- factor(def_data$Name, levels = def_data$Name)

#Create visual for UZR

chart_1 <- def_data %>%
  ggplot(aes(x = Name, y = UZR, label = UZR)) +
  geom_point(stat = "identity", aes(col = def_type), size = 7) +
  geom_text(color = "white", size = 2) +
  scale_color_manual(name = "UZR", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("Above Average"="#00ba38", "Below Average"="#f8766d")) +
  scale_x_discrete(limits = rev) +
  coord_flip() +
  labs(title = "Shortstop UZR totals since 2012",
       caption = "Minimum of 3000 Innings to Qualify")

#Create a new column for above and below average DRS

def_data <- def_data %>%
  mutate(def_type_DRS = ifelse(DRS > 0, "Above Average", "Below Average")) %>%
  arrange(-DRS)

#Change to factor to keep order

def_data$Name <- factor(def_data$Name, levels = def_data$Name)

#Create visual for UZR

chart_2 <- def_data %>%
  ggplot(aes(x = Name, y = DRS, label = DRS)) +
  geom_point(stat = "identity", aes(col = def_type_DRS), size = 7) +
  geom_text(color = "white", size = 2.5) +
  scale_color_manual(name = "DRS", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("Above Average"="#00ba38", "Below Average"="#f8766d")) +
  scale_x_discrete(limits = rev) +
  coord_flip() +
  labs(title = "Shortstop DRS totals since 2012",
       caption = "Minimum of 3000 Innings to Qualify")