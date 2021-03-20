#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/1998 HR Chase")

#Loading libraries

library(tidyverse)
library(lubridate)
library(gganimate)
library(hrbrthemes)

#Import data

data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/1998 HR Chase/1998 HR Chase.csv")

#Get running HR Total

data <- data %>%
  group_by(Player) %>%
  mutate(SeasonHR = cumsum(HR)) %>%
  ungroup()

#Ensure that date is in correct format

data$Date <- mdy(data$Date)

#Create color scheme

cols <- c("Mark McGwire" = "#C41E3A", "Ken Griffey Jr." = "#005C5C", "Sammy Sosa" = "#0E3386")

#Create visual

p <- data %>%
  ggplot(aes(x = Date, y = SeasonHR, group = Player, color = Player, label = as.character(SeasonHR))) +
  geom_line(size = 2) +
  geom_point(size = 8) +
  geom_text(color = "white", fontface = "bold", size = 14/.pt, hjust = .5, vjust = .5) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  scale_color_manual(values = cols) +
  hrbrthemes::theme_ipsum(plot_title_size = 32, base_size = 16) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.margin = margin(1,1,1,2,"cm"),
        legend.position = "right",
        axis.text.y=element_blank(),
        axis.text.x = element_text(angle = 65)) +
  transition_reveal(Date) +
  ease_aes("linear") +
  labs(title = "1998 Home Run Chase", subtitle = "Record of 61 was set by Roger Maris in 1961 ", x = "", y = "") +
  view_follow()

#Animate the visual

anim <- animate(p, nframes = 190, fps = 5, width = 608, height = 365, renderer = magick_renderer())

anim_save("1998 HR Race.gif", anim)