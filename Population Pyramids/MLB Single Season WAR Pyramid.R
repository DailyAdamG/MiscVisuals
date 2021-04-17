#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Population Pyramids")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(ggthemes)


#Import data

career_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Population Pyramids/MLB Single Season WAR.csv")

#Create WAR breakdown groups

career_data <- career_data %>%
  mutate(WARGroup = ifelse(WAR < 0, "Below Replacement Season",
                           ifelse(WAR < .5, "0 to 0.5 WAR Season",
                              ifelse(WAR < 1, "0.5 to 1 WAR Season",
                                ifelse(WAR < 1.5, "1 to 1.5 WAR Season",
                                    ifelse(WAR < 2, "1.5 to 2 WAR Season",
                                         ifelse(WAR < 2.5, "2 to 2.5 WAR Season",
                                                ifelse(WAR < 3, "2.5 to 3 WAR Season",
                                                       ifelse(WAR < 3.5, "3 to 3.5 WAR Season",
                                                              ifelse(WAR < 4, "3.5 to 4 WAR Season", 
                                                                     ifelse(WAR < 4.5, "4 to 4.5 WAR Season", 
                                                                            ifelse(WAR < 5, "4.5 to 5 WAR Season", 
                                                                                   ifelse(WAR < 5.5, "5 to 5.5 WAR Season",
                                                                                          ifelse(WAR < 6, "5.5 to 6 WAR Season","6 or More WAR Season"))))))))))))))

#Get count of values for each group

data_for_graph <- career_data %>%
  group_by(WARGroup, Position) %>%
  count(WARGroup) %>%
  ungroup()

#Create negative count for pitchers

data_for_graph <- data_for_graph %>%
  mutate(n = ifelse(Position == "Pitchers", -1 * n, n))

#Ordering position for visual

ordered_WARGroup <- factor(data_for_graph$WARGroup, levels = c("Below Replacement Season",
                                                               "0 to 0.5 WAR Season",
                                                               "0.5 to 1 WAR Season",
                                                               "1 to 1.5 WAR Season",
                                                               "1.5 to 2 WAR Season",
                                                               "2 to 2.5 WAR Season",
                                                               "2.5 to 3 WAR Season",
                                                               "3 to 3.5 WAR Season",
                                                               "3.5 to 4 WAR Season",
                                                               "4 to 4.5 WAR Season",
                                                               "4.5 to 5 WAR Season",
                                                               "5 to 5.5 WAR Season",
                                                               "5.5 to 6 WAR Season",
                                                               "6 or More WAR Season"))

#Create visual

data_for_graph %>%
  ggplot(aes(x = ordered_WARGroup, y = n, fill = Position)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(-20000, 20000, 5000),
                     labels = c(seq(20000, 0, -5000), seq(5000, 20000, 5000))) +
  coord_flip() +
  theme_tufte() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Modern Era Single Season WAR Pyramid",
       x = "WAR Group",
       y = "Number of Player Seasons") +
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank())