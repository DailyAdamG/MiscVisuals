#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X in Span to Start Career/MLB")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(weights)
library(zoo)
library(data.table)
library(Lahman)

#Import Pitching WAR Data

data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X in Span/MLB/Most Position Player WAR in 3 Season Span.csv")

#Calculate the totals and find the season the span started

data <- data %>%
  arrange(playerid, Season) %>%
  group_by(playerid) %>%
  mutate(WAR_Total = rollapply(WAR, width = 3, FUN = sum, align = "left",partial = TRUE)) %>%
  mutate(firstYear = lead(Season, 1)) %>%
  mutate(secondYear = lead(Season,2)) %>%
  mutate(Range = secondYear - Season) %>%
  mutate(firstYearTeam = lead(Team,1)) %>%
  mutate(secondYearTeam = lead(Team,2)) %>%
  mutate(OneFranchise = ifelse(Team == firstYearTeam & Team == secondYearTeam, "Yes", "No")) %>%
  ungroup()

#Choose only the columns needed for visual

display_data <- sqldf('SELECT MIN(Season) AS MIN,
      secondYear AS MAX,
      Range,
      playerid,
      Name,
      Team,
      WAR_Total,
      OneFranchise
      FROM data
      GROUP BY playerid
      ORDER BY WAR_Total DESC')


#Limit to top 10 spots

top_data <- display_data %>%
  mutate(Rank = rank(-WAR_Total, ties.method = "min")) %>%
  filter(Rank <= 10)

#Create Team Name for Players with More Than 1 Franchise

top_data <- top_data %>%
  mutate(DisplayFranchises = ifelse(OneFranchise == "No", "---", Team))

################################################################################

#Fix for players that have two different teams but one franchise

top_data <- top_data %>%
  mutate(DisplayFranchises = ifelse(Team == "BSN", "MIL", DisplayFranchises))

################################################################################

#Combine columns for display

top_data <- top_data %>%
  mutate(chart_label = paste(Name, "\n", MIN, "-", MAX, "\n",DisplayFranchises))

#Team Colors for display

fill_color <- c("Arizona Diamondbacks" = "#5F259F",
                "ARI" = "#5F259F",
                "Atlanta Braves" = "#CE1141",
                "MIL" = "#CE1141",
                "Baltimore Orioles" = "#DF4601",
                "Boston Red Sox" = "#BD3039",
                "BOS" = "#BD3039",
                "Chicago Cubs" = "#0E3386",
                "CHI" = "#0E3386",
                "CHC" = "#0E3386",
                "Chicago White Sox" = "#27251F",
                "Cincinnati Reds" = "#C6011F",
                "Cleveland Indians" = "#E31937",
                "Colorado Rockies" = "#33006F;",
                "Detroit Tigers" = "#FA4616",
                "DET" = "#FA4616",
                "Houston Astros" = "#EB6E1F",
                "Kansas City Royals" = "#004687",
                "Los Angeles Angels" = "#BA0021",
                "LAA" = "#BA0021",
                "Los Angeles Dodgers" = "#005A9C",
                "Miami Marlins" = "#00A3E0",
                "Milwaukee Brewers" = "#FFC52F",
                "Minnesota Twins" = "#002B5C",
                "New York Mets" = "#FF5910",
                "New York Yankees" = "#0C2340",
                "NYY" = "#0C2340",
                "Oakland Athletics" = "#003831",
                "Philadelphia Phillies" = "#E81828",
                "PHI" = "#E81828",
                "Pittsburgh Pirates" = "#FDB827",
                "PIT" = "#FDB827",
                "St. Louis Cardinals" = "#C41E3A",
                "STL" = "#C41E3A",
                "San Diego Padres" = "#2F241D",
                "San Francisco Giants" = "#FD5A1E",
                "SFG" = "#FD5A1E",
                "Seattle Mariners" = "#005C5C",
                "Tampa Bay Rays" = "#8FBCE6",
                "TBR" = "#8FBCE6",
                "Texas Rangers" = "#C0111F",
                "Toronto Blue Jays" = "#134A8E",
                "Washington Nationals" = "#AB0003",
                "BUF" = "seashell",
                "LOU" = "seashell",
                "---" = "Seashell",
                "Multiple Teams" = "seashell")

#Create visual

top_data %>%
  ggplot(aes(x = reorder(chart_label, WAR_Total), y = WAR_Total, fill = DisplayFranchises)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = rd(WAR_Total,1)), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(15, 25)) +
  scale_y_continuous(breaks = seq(15, 25, 1)) +
  scale_fill_manual(values = fill_color) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Most Position Player WAR in a 3 Season Span\nto Start Career",
       x = "",
       y = "Position Player WAR")