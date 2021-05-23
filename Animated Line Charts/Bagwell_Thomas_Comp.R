#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Line Charts")

#Loading libraries

library(tidyverse)
library(gganimate)
library(hrbrthemes)
library(weights)

#Import data

bagwell_thomas_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Line Charts/Bagwell_Thomas_Comp.csv")

#Get running WAR

bagwell_thomas_data <- bagwell_thomas_data %>%
  arrange(Name, Season) %>%
  group_by(Name) %>%
  mutate(CareerWAR = cumsum(WAR))

#Create color scheme

cols <- c("Jeff Bagwell" = "#EB6E1F", "Frank Thomas" = "#27251F")

#Create visual

p <- bagwell_thomas_data %>%
  ggplot(aes(x = Season, y = CareerWAR, group = Name, color = Name)) +
  geom_line(size = 2) +
  geom_point(size = 12) +
  geom_text(aes(label = rd(CareerWAR, digits = 1)), color = "white", fontface = "bold", size = 12/.pt, hjust = .5, vjust = .5) +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = seq(1990, 2008, 1)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  hrbrthemes::theme_ipsum(plot_title_size = 24, base_size = 14) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.x = element_text(angle = 25)) +
  transition_reveal(Season) +
  ease_aes("linear") +
  #view_follow() +
  labs(title = "Jeff Bagwell vs. Frank Thomas Career WAR", x = "", y = "")

#Animate the visual

anim <- animate(p, nframes = 24, fps = 1, width = 800, height = 482, end_pause = 5, renderer = magick_renderer())

anim_save("Bagwell_Thomas_Comp.gif", anim)