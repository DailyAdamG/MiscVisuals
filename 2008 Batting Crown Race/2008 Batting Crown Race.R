#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/2008 Batting Crown Race")

#Loading libraries

library(tidyverse)
library(lubridate)
library(gganimate)
library(hrbrthemes)
library(weights)

#Import data

batting_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/2008 Batting Crown Race/Batting Crown Race.csv")

#Ensure that date is in correct format

batting_data$Date <- mdy(batting_data$Date)

#Get running BA

batting_data <- batting_data %>%
  arrange(Player, Date) %>%
  group_by(Player) %>%
  mutate(SeasonHits = cumsum(H),
         SeasonABs = cumsum(AB),
         RunningBA = round(SeasonHits / SeasonABs,3))

#Filter to ignore the first two Pedroia games (makes the chart look nicer)

batting_data <- batting_data %>%
  filter(Date >= as.Date("2008-03-31"))

#Create color scheme

cols <- c("Dustin Pedroia" = "#BD3039", "Joe Mauer" = "#002B5C")

#Create visual

p <- batting_data %>%
  ggplot(aes(x = Date, y = RunningBA, group = Player, color = Player)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_color_manual(values = cols) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  scale_y_continuous(breaks = seq(0, .400, .025)) +
  hrbrthemes::theme_ipsum(plot_title_size = 24, base_size = 16) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.x = element_text(angle = 65)) +
  transition_reveal(Date) +
  ease_aes("linear") +
  view_follow() +
  labs(title = "2008 AL Batting Crown Race", x = "", y = "")

#Animate the visual

anim <- animate(p, nframes = 210, fps = 5, width = 608, height = 365, end_pause = 20, renderer = magick_renderer())

anim_save("2008 Batting Crown Race.gif", anim)