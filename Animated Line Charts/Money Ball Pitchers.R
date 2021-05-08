#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Line Charts")

#Loading libraries

library(tidyverse)
library(gganimate)
library(hrbrthemes)
library(weights)

#Import data

pitching_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Line Charts/Moneyball Pitchers.csv")

#Get running WAR

pitching_data <- pitching_data %>%
  arrange(Name, Season) %>%
  group_by(Name) %>%
  mutate(CareerWAR = cumsum(WAR))

#Create color scheme

cols <- c("Mark Mulder" = "#A2AAAD", "Barry Zito" = "#EFB21E", "Tim Hudson" = "#003831")

#Create visual

p <- pitching_data %>%
  ggplot(aes(x = Season, y = CareerWAR, group = Name, color = Name)) +
  geom_line(size = 2) +
  geom_point(size = 12) +
  geom_text(aes(label = rd(CareerWAR, digits = 1)), color = "white", fontface = "bold", size = 12/.pt, hjust = .5, vjust = .5) +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = seq(1999, 2015, 1)) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  hrbrthemes::theme_ipsum(plot_title_size = 24, base_size = 16) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.x = element_text(angle = 65)) +
  transition_reveal(Season) +
  ease_aes("linear") +
  view_follow() +
  labs(title = "Moneyball Pitcher Career WAR", x = "", y = "")

#Animate the visual

anim <- animate(p, nframes = 25, fps = 1, width = 800, height = 482, end_pause = 5, renderer = magick_renderer())

anim_save("Moneyball Pitchers.gif", anim)