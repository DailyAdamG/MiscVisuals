#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X in Span/Cleveland Indians")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)
library(Lahman)

#Get data from Lahman Database

data <- sqldf('SELECT People.playerID, Batting.yearID, People.nameFirst, People.nameLast,
TeamsFranchises.franchName AS Teams, TeamsFranchises.franchID, SUM(Batting.R) AS R
                    FROM People
                    INNER JOIN Batting
                    ON People.playerID = Batting.PlayerID
                    INNER JOIN Teams
                    ON Batting.teamID = Teams.teamID
                    AND Batting.yearID = Teams.yearID
                    INNER JOIN TeamsFranchises
                    ON Teams.franchID = TeamsFranchises.franchID
                    WHERE TeamsFranchises.franchName = "Cleveland Indians"
                    GROUP BY Batting.playerID, Batting.yearID, TeamsFranchises.franchID
                    ORDER BY SUM(Batting.R) DESC')

#Calculate the totals and find the season the span started

data <- data %>%
  arrange(playerID, yearID) %>%
  group_by(playerID) %>%
  mutate(R_Total = rollapply(R, width = 5, FUN = sum, align = "right",partial = TRUE)) %>%
  mutate(firstYear_rd = lag(yearID, 4)) %>%
  fill(firstYear_rd, .direction = "up") %>%
  mutate(first_year_2_rd = lag(yearID,3)) %>%
  fill(first_year_2_rd, .direction = "up") %>%
  mutate(first_year_3_rd = lag(yearID,2)) %>%
  fill(first_year_3_rd, .direction = "up") %>%
  mutate(first_year_4_rd = lag(yearID,1)) %>%
  fill(first_year_4_rd, .direction = "up") %>%
  mutate(firstYear = ifelse(is.na(firstYear_rd) & is.na(first_year_2_rd) & is.na(first_year_3_rd) & is.na(first_year_4_rd), yearID,
                            ifelse(is.na(firstYear_rd) & is.na(first_year_2_rd) & is.na(first_year_3_rd), first_year_4_rd,
                                   ifelse(is.na(firstYear_rd) & is.na(first_year_2_rd), first_year_3_rd,
                                          ifelse(is.na(firstYear_rd), first_year_2_rd, firstYear_rd))))) %>%
  mutate(Range = yearID - firstYear) %>%
  ungroup()

#Choose only the columns needed for visual

display_data <- sqldf('SELECT firstYear AS MIN,
      yearID AS MAX,
      Range,
      playerID,
      nameFirst,
      nameLast,
      Teams,
      R_Total
      FROM data
      GROUP BY playerID, yearID
      ORDER BY R_Total DESC')

#Add index number to find overlapping values (Only finds overlapping values for top value in group)

setDT(display_data)
display_data$index <- display_data[display_data, on = .(playerID, MAX>=MIN, MIN<=MAX), mult ="first", which = TRUE]

#Keep only top value from index grouping

no_dupes <- display_data %>%
  group_by(index) %>%
  slice_head() %>%
  group_by(playerID,MIN) %>%
  slice_head() %>%
  arrange(-R_Total) %>%
  ungroup()

#Limit to top 10 spots

top_data <- no_dupes %>%
  arrange(-R_Total, desc(nameFirst)) %>%
  mutate(Rank = rank(-R_Total, ties.method = "min")) %>%
  filter(Rank <= 10)

#Combine columns for display

top_data <- top_data %>%
  mutate(chart_label = paste(nameFirst,nameLast, "\n", MIN, "-", MAX))

#Create a column for alternating row colors in graph

top_data <- top_data %>%
  mutate(EvenOdd = ifelse(row_number() %% 2 == 1, "Odd", "Even"))

#Team Colors for display

fill_color <- c("Arizona Diamondbacks" = "#5F259F",
                "Atlanta Braves" = "#CE1141",
                "Baltimore Orioles" = "#DF4601",
                "Boston Red Sox" = "#BD3039",
                "Chicago Cubs" = "#0E3386",
                "Chicago White Sox" = "#27251F",
                "Cincinnati Reds" = "#C6011F",
                "Cleveland Indians" = "#E31937",
                "Colorado Rockies" = "#33006F",
                "Detroit Tigers" = "#FA4616",
                "Houston Astros" = "#EB6E1F",
                "Kansas City Royals" = "#004687",
                "Los Angeles Angels" = "#BA0021",
                "Los Angeles Dodgers" = "#005A9C",
                "Miami Marlins" = "#00A3E0",
                "Milwaukee Brewers" = "#FFC52F",
                "Minnesota Twins" = "#002B5C",
                "New York Mets" = "#FF5910",
                "New York Yankees" = "#0C2340",
                "Oakland Athletics" = "#003831",
                "Philadelphia Phillies" = "#E81828",
                "Pittsburgh Pirates" = "#FDB827",
                "St. Louis Cardinals" = "#C41E3A",
                "San Diego Padres" = "#2F241D",
                "San Francisco Giants" = "#FD5A1E",
                "Seattle Mariners" = "#005C5C",
                "Tampa Bay Rays" = "#8FBCE6",
                "Texas Rangers" = "#C0111F",
                "Toronto Blue Jays" = "#134A8E",
                "Washington Nationals" = "#AB0003",
                "Multiple Teams" = "seashell")

#Create visual

top_data %>%
  ggplot(aes(x = reorder(chart_label, R_Total), y = R_Total, fill = EvenOdd)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = R_Total), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(500, 600)) +
  scale_y_continuous(breaks = seq(500, 600, 5)) +
  scale_fill_manual(values = c("Odd" = "#0C2340", "Even" = "#E31937")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Cleveland Indians Franchise History\nMost Runs in a 5 Season Span",
       subtitle = "Excluding Overlapping Seasons",
       x = "",
       y = "Runs")