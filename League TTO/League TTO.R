#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/League TTO")

#Loading libraries

library(tidyverse)
library(gganimate)
library(hrbrthemes)
library(scales)

#Import data

league_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/League TTO/League TTO.csv")


#Create visual

p <- league_data %>%
  ggplot(aes(x = Season, y = TTO_Pct)) +
  geom_line(size = 2, color = "blue") +
  scale_x_continuous(breaks = seq (1910,2021,5)) +
  scale_y_continuous(breaks = seq(0,.4,.05), labels = percent) +
  hrbrthemes::theme_ipsum(plot_title_size = 20, base_size = 12) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.x = element_text(angle = 65)) +
  transition_reveal(Season) +
  ease_aes("linear") +
  labs(title = "MLB's Three True Outcome Percentage by Season", x = "", y = "")

#Animate the visual

anim <- animate(p, nframes = 112, fps = 4, width = 608, height = 365, end_pause = 10, renderer = magick_renderer())

anim_save("League TTO.gif", anim)