#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(gganimate)
library(hrbrthemes)

#Import data

bj_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Bar Charts/Blue Jays Pitchers.csv")

#Make data wide so that I can replace null values with zeros

bj_data_wide <- bj_data %>%
  spread(Season, WAR)

#Replace null values with zeros to create WAR totals for years where player is no longer active

bj_data_wide[is.na(bj_data_wide)] <- 0.0

#Go back to long data format

bj_data <- bj_data_wide %>%
  gather(Season, WAR, "1977":"2021")

#Change Year column to integer

bj_data$Season <- as.integer(bj_data$Season)

#Rearrange data and group by player to calculate running total of career WAR by year

bj_data <- bj_data %>%
  arrange(playerid, Season) %>%
  group_by(playerid) %>%
  mutate(CareerWAR = round(cumsum(WAR),1)) %>%
  ungroup()

#Rename columns

bj_data <- bj_data %>%
  rename(SeasonWAR = WAR, WAR = CareerWAR)

#Create career WAR ranking by year and limit it to the top 10 spots

top_data <- bj_data %>%
  group_by(Season) %>%
  arrange(Season, -WAR) %>%
  mutate(Rank = row_number()) %>%
  mutate(Rank = ifelse(Rank > 10, 11, Rank)) %>%
  filter((Rank <= 10 | Name == "Roy Halladay") & Season >= 1998) %>%
  ungroup()

#Create highlight for Roy Halladay

top_data <- top_data %>%
  mutate(Highlight = ifelse(Name == "Roy Halladay", "yes", "no"))

#Create visual

p <- top_data %>%
  ggplot(aes(x = -Rank, y = WAR, group = Name)) +
  geom_tile(aes(y = WAR/2, height = WAR, fill = Highlight), width = .9) +
  geom_text(aes(label = paste(Name, formatC(WAR, format = "f", digits = 1))), hjust = "left", color = "black", nudge_y = 2) +
  coord_flip(clip="off") +
  scale_x_discrete("") +
  scale_y_continuous("", labels = scales::comma, limits = c(0,60)) +
  hrbrthemes::theme_ipsum(plot_title_size = 20, subtitle_size = 16, caption_size = 10, base_size = 12) +
  scale_fill_manual(values = c("yes" = "#1D2D5C", "no" = "#134A8E")) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = "none",
        plot.margin = margin(1,1,1,2,"cm")) +
  transition_time(Season) +
  ease_aes("cubic-in-out") +
  labs(title = "Top Blue Jays Pitchers by Career WAR",
       subtitle = "{frame_time} Season",
       caption = "WAR figures from FanGraphs.com")

#Animate the visual

anim <- animate(p, nframes = 29, fps = 1, end_pause = 5, renderer = magick_renderer())

#Save the visual

anim_save("Blue Jays Pitchers.gif", anim)