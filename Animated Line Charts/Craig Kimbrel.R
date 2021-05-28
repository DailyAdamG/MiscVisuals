#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Line Charts")

#Loading libraries

library(tidyverse)
library(gganimate)
library(hrbrthemes)
library(lubridate)
library(weights)

#Import data

data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Line Charts/Kimbrel Career ERA.csv")

#Ensure that date is in correct format

data$Date <- mdy(data$Date)

#Get running ERA

data <- data %>%
  arrange(Date) %>%
  mutate(Game_Number = row_number(),
         Career_IPOUTS = cumsum(IPOUTS),
         Career_ER = cumsum(ER),
         RunningERA = round(Career_ER * 9 / (Career_IPOUTS / 3),2))

#function for labels

scaleFUN <- function(x) sprintf("%.2f", x)


#Create visual

p <- data %>%
  ggplot(aes(x = Game_Number, y = RunningERA)) +
  geom_line(size = 2, color = "#0E3386") +
  geom_point(size = 12, color = "#0E3386") +
  geom_text(aes(label = rd(RunningERA, digits = 2)), color = "#CC3433", fontface = "bold", size = 12/.pt, hjust = .5, vjust = .5) +
  hrbrthemes::theme_ipsum(plot_title_size = 24, base_size = 16) +
  scale_x_continuous(breaks = seq(0, 700, 50)) +
  scale_y_continuous(breaks = seq(0, 4, 0.5), labels = scaleFUN) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.y = element_text(),
        axis.text.x = element_text()) +
  transition_reveal(Game_Number) +
  view_follow() +
  ease_aes("linear") +
  labs(title = "Craig Kimbrel Running Career ERA", x = "Games Pitched", y = "")

#Animate the visual

anim <- animate(p, nframes = 300, fps = 10, width = 608, height = 365, end_pause = 20, renderer = magick_renderer())

anim_save("Craig Kimbrel.gif", anim)