#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Line Charts")

#Loading libraries

library(tidyverse)
library(gganimate)
library(hrbrthemes)
library(weights)

#Import data

kershawzer_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Line Charts/Kershaw_Scherzer_Comp.csv")

#Get running WAR

kershawzer_data <- kershawzer_data %>%
  arrange(Name, Season) %>%
  group_by(Name) %>%
  mutate(CareerWAR = cumsum(WAR))

#Create color scheme

cols <- c("Clayton Kershaw" = "#005A9C", "Max Scherzer" = "#AB0003")

#Create visual

p <- kershawzer_data %>%
  ggplot(aes(x = Season, y = CareerWAR, group = Name, color = Name)) +
  geom_line(size = 2) +
  geom_point(size = 12) +
  geom_text(aes(label = rd(CareerWAR, digits = 1)), color = "white", fontface = "bold", size = 12/.pt, hjust = .5, vjust = .5) +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = seq(2008, 2020, 1)) +
  scale_y_continuous(breaks = seq(0, 70, 5)) +
  hrbrthemes::theme_ipsum(plot_title_size = 24, base_size = 14) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.x = element_text(angle = 25)) +
  transition_reveal(Season) +
  ease_aes("linear") +
  labs(title = "Clayton Kershaw vs. Max Scherzer Pitcher Career WAR", x = "", y = "")

#Animate the visual

anim <- animate(p, nframes = 18, fps = 1, width = 800, height = 482, end_pause = 5, renderer = magick_renderer())

anim_save("Kershaw_Scherzer_Comp.gif", anim)