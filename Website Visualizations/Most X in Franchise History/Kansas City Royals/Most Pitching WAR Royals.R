#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X in Franchise History/Kansas City Royals")

#Import csv

data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X in Franchise History/Kansas City Royals/Pitching WAR by Season Royals.csv")

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
  mutate(SeasonRange = ifelse(Name == 'Kevin Appier', '1989-1999, 2003-2004', 
                              ifelse(Name == 'Mark Gubicza', '1984-1996',
                                     ifelse(Name == 'Bret Saberhagen', '1984-1991',
                                            ifelse(Name == 'Dennis Leonard', '1974-1983, 1985-1986',
                                                   ifelse(Name == 'Paul Splittorff', '1970-1984',
                                                          ifelse(Name == 'Zack Greinke', '2004-2010',
                                                                 ifelse(Name == 'Charlie Leibrandt', '1984-1989',
                                                                        ifelse(Name == 'Dick Drago', '1969-1973',
                                                                               ifelse(Name == 'Danny Duffy', '2011-2021','1988-1995'))))))))))

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
  coord_flip(ylim = c(15, 45)) +
  scale_y_continuous(breaks = seq(15, 45, 5)) +
  scale_fill_manual(values = c("Odd" = "#004687", "Even" = "#BD9B60")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most Pitching WAR in Kansas City Royals Franchise History",
       x = "",
       y = "WAR")