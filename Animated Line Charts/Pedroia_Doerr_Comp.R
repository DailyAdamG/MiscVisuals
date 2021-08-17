#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Line Charts")

#Loading libraries

library(tidyverse)
library(gganimate)
library(hrbrthemes)
library(weights)

#Import data

pedroia_doerr_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Line Charts/Pedroia_Doerr_Comp.csv")

#Get running WAR

pedroia_doerr_data <- pedroia_doerr_data %>%
  arrange(Name, Season) %>%
  group_by(Name) %>%
  mutate(SeasonNumber = rank(Season)) %>%
  mutate(CareerWAR = cumsum(WAR))

#Create color scheme

cols <- c("Bobby Doerr" = "#0C2340", "Dustin Pedroia" = "#BD3039")

#Create visual

p <- pedroia_doerr_data %>%
  ggplot(aes(x = SeasonNumber, y = CareerWAR, group = Name, color = Name)) +
  geom_line(size = 2) +
  geom_point(size = 12) +
  geom_text(aes(label = rd(CareerWAR, digits = 1)), color = "white", fontface = "bold", size = 12/.pt, hjust = .5, vjust = .5) +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = seq(1, 14, 1)) +
  scale_y_continuous(breaks = seq(0, 55, 5)) +
  hrbrthemes::theme_ipsum(plot_title_size = 24, base_size = 14) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.x = element_text(angle = 25)) +
  transition_reveal(SeasonNumber) +
  ease_aes("linear") +
  #view_follow() +
  labs(title = "Dustin Pedroia vs. Bobby Doerr Career WAR", x = "Season in MLB", y = "Career WAR")

#Animate the visual

anim <- animate(p, nframes = 19, fps = 1, width = 800, height = 482, end_pause = 5, renderer = magick_renderer())

anim_save("Pedroia_Doerr_Comp.gif", anim)