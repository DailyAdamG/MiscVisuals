#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Population Pyramids")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(ggthemes)


#Import data

career_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Population Pyramids/MLB Career WAR.csv")

#Create WAR breakdown groups

career_data <- career_data %>%
  mutate(WARGroup = ifelse(WAR < 1, "Less Than 1 WAR",
                           ifelse(WAR < 5, "1 to 5 WAR",
                              ifelse(WAR < 10, "5 to 10 WAR",
                                  ifelse(WAR < 20, "10 to 20 WAR",
                                         ifelse(WAR < 30, "20 to 30 WAR",
                                                ifelse(WAR < 40, "30 to 40 WAR",
                                                       ifelse(WAR < 50, "40 to 50 WAR", 
                                                              ifelse(WAR < 60, "50 to 60 WAR", "60 or More WAR")))))))))

#Get count of values for each group

data_for_graph <- career_data %>%
  group_by(WARGroup, Position) %>%
  count(WARGroup) %>%
  ungroup()

#Create negative count for pitchers

data_for_graph <- data_for_graph %>%
  mutate(n = ifelse(Position == "Pitchers", -1 * n, n))

#Ordering position for visual

ordered_WARGroup <- factor(data_for_graph$WARGroup, levels = c("Less Than 1 WAR",
                                                               "1 to 5 WAR",
                                                               "5 to 10 WAR",
                                                               "10 to 20 WAR",
                                                               "20 to 30 WAR",
                                                               "30 to 40 WAR",
                                                               "40 to 50 WAR",
                                                               "50 to 60 WAR",
                                                               "60 or More WAR"))

#Create visual

data_for_graph %>%
  ggplot(aes(x = ordered_WARGroup, y = n, fill = Position)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(-7000, 7000, 1000),
                     labels = c(seq(7000, 0, -1000), seq(1000, 7000, 1000))) +
  coord_flip() +
  theme_tufte() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Career WAR of Major Leaguers",
       x = "WAR Group",
       y = "Number of Players") +
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank())