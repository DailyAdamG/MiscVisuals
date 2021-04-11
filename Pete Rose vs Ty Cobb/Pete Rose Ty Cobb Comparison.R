#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Pete Rose vs Ty Cobb")

#Loading libraries

library(tidyverse)
library(lubridate)
library(gganimate)
library(hrbrthemes)
library(weights)

#Import data

hit_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Pete Rose vs Ty Cobb/Pete Rose Ty Cobb Comparison.csv")

#Get running hit toal

hit_data <- hit_data %>%
  arrange(Player, Season) %>%
  group_by(Player) %>%
  mutate(SeasonNumber = row_number(),
         CareerHits = cumsum(H))

#Color groupings

colors <- c("Pete Rose" = "#C6011F", "Ty Cobb" = "#0C2340")

#Create visual

p <- hit_data %>%
  ggplot(aes(x = SeasonNumber, y = CareerHits, color = Player)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(1,24,1)) +
  scale_y_continuous(breaks = seq(0,5000,250)) +
  hrbrthemes::theme_ipsum(plot_title_size = 20, base_size = 16) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.margin = margin(1,1,1,1,"cm"),
        axis.text.x = element_text(angle = 65)) +
  transition_reveal(SeasonNumber) +
  ease_aes("linear") +
  labs(title = "Pete Rose vs. Ty Cobb Career Hits by MLB Season", x = "Year in Majors", y = "") +
  view_follow()

#Animate the visual

anim <- animate(p, nframes = 34, fps = 1, width = 608, height = 365, end_pause = 10, renderer = magick_renderer())

anim_save("Pete Rose Ty Cobb Comparison.gif", anim)