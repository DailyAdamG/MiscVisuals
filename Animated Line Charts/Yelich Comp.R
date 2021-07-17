#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Line Charts")

#Loading libraries

library(tidyverse)
library(gganimate)
library(hrbrthemes)
library(weights)

#Import data

data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Line Charts/Yelich Comp.csv")

#Get running WAR

data <- data %>%
  arrange(Name, Season) %>%
  group_by(Name) %>%
  mutate(CareerWAR = cumsum(WAR))

#Create color scheme

cols <- c("Billy Williams" = "#0E3386", "Gary Matthews" = "#FD5A1E", "Jack Clark" = "#27251F",
          "Matt Kemp" = "#005A9C", "Reggie Smith" = "#BD3039", "Christian Yelich" = "#FFC52F")

#Create visual

p <- data %>%
  ggplot(aes(x = Age, y = CareerWAR, group = Name, color = Name)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  #geom_text(aes(label = rd(CareerWAR, digits = 1)), color = "white", fontface = "bold", size = 12/.pt, hjust = .5, vjust = .5) +
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = seq(19, 38, 1)) +
  scale_y_continuous(breaks = seq(0, 75, 5)) +
  hrbrthemes::theme_ipsum(plot_title_size = 24, base_size = 16) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.x = element_text(angle = 65)) +
  transition_reveal(Age) +
  ease_aes("linear") +
  view_follow() +
  labs(title = "Christian Yelich WAR vs. Top 5 Bill James Similarity Scores", subtitle = "Only Includes Players Who Have Finished Their Career", x = "Season Age", y = "Career WAR")

#Animate the visual

anim <- animate(p, nframes = 25, fps = 1, width = 800, height = 482, end_pause = 5, renderer = magick_renderer())

anim_save("Yelich Comp.gif", anim)