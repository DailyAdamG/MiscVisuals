#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Website Visualizations/Most X in Span/Los Angeles Angels")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)
library(Lahman)

#Get data from Lahman Database

data <- sqldf('SELECT People.playerID, Batting.yearID, People.nameFirst, People.nameLast,
TeamsFranchises.franchName AS Teams, TeamsFranchises.franchID, SUM(Batting.H) AS H,
SUM(Batting.AB) AS AB, SUM(Batting.BB) AS BB, SUM(Batting.HBP) AS HBP, SUM(Batting.SF) AS SF, SUM(Batting.SH) AS SH,
SUM(Batting.H) + SUM(Batting.X2B) + SUM(Batting.X3B) * 2 + SUM(Batting.HR) * 3 AS TB
                    FROM People
                    INNER JOIN Batting
                    ON People.playerID = Batting.PlayerID
                    INNER JOIN Teams
                    ON Batting.teamID = Teams.teamID
                    AND Batting.yearID = Teams.yearID
                    INNER JOIN TeamsFranchises
                    ON Teams.franchID = TeamsFranchises.franchID
                    WHERE TeamsFranchises.franchName = "Los Angeles Angels of Anaheim"
                    GROUP BY Batting.playerID, Batting.yearID')

#Fill null values with 0s

data[is.na(data)] <- 0

#Calculate the totals and find the season the span started

data <- data %>%
  arrange(playerID, yearID) %>%
  group_by(playerID) %>%
  mutate(H_Total = rollapply(H, width = 5, FUN = sum, align = "right",partial = TRUE)) %>%
  mutate(AB_Total = rollapply(AB, width = 5, FUN = sum, align = "right",partial = TRUE)) %>%
  mutate(BB_Total = rollapply(BB, width = 5, FUN = sum, align = "right",partial = TRUE)) %>%
  mutate(HBP_Total = rollapply(HBP, width = 5, FUN = sum, align = "right",partial = TRUE)) %>%
  mutate(SF_Total = rollapply(SF, width = 5, FUN = sum, align = "right",partial = TRUE)) %>%
  mutate(SH_Total = rollapply(SH, width = 5, FUN = sum, align = "right",partial = TRUE)) %>%
  mutate(TB_Total = rollapply(TB, width = 5, FUN = sum, align = "right",partial = TRUE)) %>%
  mutate(PA_Total = AB_Total + BB_Total + HBP_Total + SF_Total + SH_Total) %>%
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
      H_Total,
      AB_Total,
      PA_Total,
      TB_Total,
      (cast(H_Total AS real) + cast(BB_Total AS real) + cast(HBP_Total AS real)) / (cast(AB_Total AS real) + cast(BB_Total AS real) + cast(HBP_Total AS real) + cast(SF_Total AS real)) AS OBP,
      cast(TB_Total AS real) / cast(AB_Total AS real) AS SLG,
      (cast(H_Total AS real) + cast(BB_Total AS real) + cast(HBP_Total AS real)) / (cast(AB_Total AS real) + cast(BB_Total AS real) + cast(HBP_Total AS real) + cast(SF_Total AS real)) +
      cast(TB_Total AS real) / cast(AB_Total AS real) AS OPS
      FROM data
      GROUP BY playerID, yearID
      HAVING PA_Total >= 2500
      AND Range >= 4
      ORDER BY OPS DESC')

#Add index number to find overlapping values (Only finds overlapping values for top value in group)

setDT(display_data)
display_data$index <- display_data[display_data, on = .(playerID, MAX>=MIN, MIN<=MAX), mult ="first", which = TRUE]

#Keep only top value from index grouping

no_dupes <- display_data %>%
  group_by(index) %>%
  slice_head() %>%
  group_by(playerID,MIN) %>%
  slice_head() %>%
  arrange(-OPS) %>%
  ungroup()

#################################################################################

#DOUBLE CHECK TO MAKE SURE VALUES IN DATA SET DO NOT OVERLAP!!!

#Remove any strays that show up in no_dupes even after logic (May not be necessary for all files)

no_dupes <- no_dupes %>%
  filter(nameFirst != 'Tim' & nameLast != 'Salmon' | MIN != 2000)

#################################################################################

#Limit to top 10 spots

top_data <- no_dupes %>%
  mutate(Rank = rank(-OPS, ties.method = "min")) %>%
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
                "Cleveland Indians" = "#0C2340",
                "Cleveland Spiders" = "#0C2340",
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
                "Multiple Franchises" = "seashell")

#Create a function to drop 0s from label

numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.3f", val)) }

#Create visual

top_data %>%
  ggplot(aes(x = reorder(chart_label, OPS), y = OPS, fill = EvenOdd)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = rd(OPS,3)), fontface = "bold", hjust = -0.3) +
  coord_flip(ylim = c(.820, 1.060)) +
  scale_y_continuous(breaks = seq(.820, 1.060, .020), labels = numformat) +
  scale_fill_manual(values = c("Odd" = "#003263", "Even" = "#BA0021")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, face ="bold"),
        legend.position = "none") +
  labs(title = "Los Angeles Angels Franchise History\nHighest OPS in a 5 Season Span\n(Minimum of 2500 Plate Appearances)",
       subtitle = "Excluding Overlapping Seasons",
       x = "",
       y = "On-base Plus Slugging Percentage")