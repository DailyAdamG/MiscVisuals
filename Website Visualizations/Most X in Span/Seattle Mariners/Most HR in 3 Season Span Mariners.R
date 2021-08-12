#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X in Span/Seattle Mariners")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)
library(Lahman)

#Get data from Lahman Database

data <- sqldf('SELECT People.playerID, Batting.yearID, People.nameFirst, People.nameLast,
GROUP_CONCAT(TeamsFranchises.franchName) AS Teams, TeamsFranchises.franchID, SUM(Batting.HR) AS HR
                    FROM People
                    INNER JOIN Batting
                    ON People.playerID = Batting.PlayerID
                    INNER JOIN Teams
                    ON Batting.teamID = Teams.teamID
                    AND Batting.yearID = Teams.yearID
                    INNER JOIN TeamsFranchises
                    ON Teams.franchID = TeamsFranchises.franchID
                    WHERE TeamsFranchises.franchName = "Seattle Mariners"
                    GROUP BY Batting.playerID, Batting.yearID, TeamsFranchises.franchID
                    ORDER BY SUM(Batting.HR) DESC')

#Calculate the totals and find the season the span started

data <- data %>%
  arrange(playerID, yearID) %>%
  group_by(playerID) %>%
  mutate(HR_Total = rollapply(HR, width = 3, FUN = sum, align = "right",partial = TRUE)) %>%
  mutate(firstYear_rd = lag(yearID, 2)) %>%
  fill(firstYear_rd, .direction = "up") %>%
  mutate(first_year_2_rd = lag(yearID,1)) %>%
  fill(first_year_2_rd, .direction = "up") %>%
  mutate(firstYear = ifelse(is.na(firstYear_rd) & is.na(first_year_2_rd), yearID,
                            ifelse(is.na(firstYear_rd), first_year_2_rd, firstYear_rd))) %>%
  mutate(Range = yearID - firstYear) %>%
  ungroup()

#Choose only the columns needed for visual

display_data <- sqldf('SELECT firstYear AS MIN,
      yearID AS MAX,
      Range,
      playerID,
      nameFirst,
      nameLast,
      HR_Total
      FROM data
      GROUP BY playerID, yearID
      ORDER BY HR_Total DESC')

#Add index number to find overlapping values (Only finds overlapping values for top value in group)

setDT(display_data)
display_data$index <- display_data[display_data, on = .(playerID, MAX>=MIN, MIN<=MAX), mult ="first", which = TRUE]

#Keep only top value from index grouping

no_dupes <- display_data %>%
  group_by(index) %>%
  slice_head() %>%
  group_by(playerID,MIN) %>%
  slice_head() %>%
  arrange(-HR_Total) %>%
  ungroup()

#################################################################################

#DOUBLE CHECK TO MAKE SURE VALUES IN DATA SET DO NOT OVERLAP!!!

#Remove any strays that show up in no_dupes even after logic (May not be necessary for all files)

no_dupes <- no_dupes %>%
  filter(nameLast != 'Griffey' | MIN != 1993)


#################################################################################

#Limit to top 10 spots

top_data <- no_dupes %>%
  arrange(-HR_Total, desc(nameFirst)) %>%
  mutate(Rank = rank(-HR_Total, ties.method = "min")) %>%
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
                "Colorado Rockies" = "#33006F;",
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
  ggplot(aes(x = reorder(chart_label, HR_Total), y = HR_Total, fill = EvenOdd)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = HR_Total), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(80, 170)) +
  scale_y_continuous(breaks = seq(80, 170, 5)) +
  scale_fill_manual(values = c("Odd" = "#0C2C56", "Even" = "#005C5C")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Seattle Mariners Franchise History\nMost Home Runs in a 3 Season Span",
       subtitle = "Excluding Overlapping Seasons",
       x = "",
       y = "Home Runs")