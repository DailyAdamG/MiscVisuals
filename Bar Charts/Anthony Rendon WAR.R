#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)

#Import data

data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts/Anthony Rendon WAR.csv")

data <- data %>%
  arrange(playerid, Season) %>%
  group_by(playerid) %>%
  mutate(WAR_Total = rollapply(WAR, width = 3, FUN = sum, align = "right",partial = TRUE)) %>%
  mutate(firstYear_rd = lag(Season, 2)) %>%
  fill(firstYear_rd, .direction = "up") %>%
  mutate(first_year_2_rd = lag(Season,1)) %>%
  fill(first_year_2_rd, .direction = "up") %>%
  mutate(firstYear = ifelse(is.na(firstYear_rd) & is.na(first_year_2_rd), Season,
                            ifelse(is.na(firstYear_rd), first_year_2_rd, firstYear_rd))) %>%
  mutate(Range = Season - firstYear)


display_data <- sqldf('SELECT firstYear AS MIN,
      Season AS MAX,
      Range,
      playerid,
      Name,
      WAR_Total
      FROM data
      GROUP BY playerid, Season
      ORDER BY WAR_Total DESC')

setDT(display_data)
display_data$index <- display_data[display_data, on = .(playerid, MAX>=MIN, MIN<=MAX), mult ="first", which = TRUE]

no_dupes <- display_data %>%
  group_by(index) %>%
  slice_head() %>%
  group_by(playerid,MIN) %>%
  slice_head() %>%
  arrange(-WAR_Total)