#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X in Franchise History/Houston Astros")

#Import csv

data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X in Franchise History/Houston Astros/Pitching WAR by Season Astros.csv")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)
library(Lahman)

#Get data from Lahman Database

data <- sqldf("SELECT playerid, Name, GROUP_CONCAT(DISTINCT Season) AS Seasons, SUM(WAR) AS WAR
                    FROM data
                    GROUP BY playerid
                    ORDER BY SUM(WAR) DESC")

#Limit to top 10 spots

top_data <- data %>%
  mutate(Rank = rank(-WAR, ties.method = "min")) %>%
  filter(Rank <= 10)

#Create SeasonRange Column for Display

top_data <- top_data %>%
  mutate(SeasonRange = ifelse(Name == 'Roy Oswalt', '2001-2010', 
                              ifelse(Name == 'Nolan Ryan', '1980-1988',
                                     ifelse(Name == 'J.R. Richard', '1971-1980',
                                            ifelse(Name == 'Larry Dierker', '1964-1976',
                                                   ifelse(Name == 'Shane Reynolds', '1992-2002',
                                                          ifelse(Name == 'Mike Scott', '1983-1991',
                                                                 ifelse(Name == 'Don Wilson', '1966-1974',
                                                                        ifelse(Name == 'Joe Niekro', '1975-1985',
                                                                               ifelse(Name == 'Wandy Rodriguez', '2005-2012','1970-1980'))))))))))

#Combine columns for display

top_data <- top_data %>%
  mutate(chart_label = paste(Name,"\n", SeasonRange))

#Create a column for alternating row colors in graph

top_data <- top_data %>%
  mutate(EvenOdd = ifelse(row_number() %% 2 == 1, "Odd", "Even"))

#Create visual

top_data %>%
  ggplot(aes(x = reorder(chart_label, WAR), y = WAR, fill = EvenOdd)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = format(round(WAR,1))), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(15, 50)) +
  scale_y_continuous(breaks = seq(15, 50, 5)) +
  scale_fill_manual(values = c("Odd" = "#002D62", "Even" = "#EB6E1F")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most Pitching WAR in Houston Astros Franchise History",
       x = "",
       y = "WAR")