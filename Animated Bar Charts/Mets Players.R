#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(gganimate)
library(hrbrthemes)

#Import data

mets_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Bar Charts/Mets Players.csv")

#Make data wide so that I can replace null values with zeros

mets_data_wide <- mets_data %>%
  spread(Season, WAR)

#Replace null values with zeros to create WAR totals for years where player is no longer active

mets_data_wide[is.na(mets_data_wide)] <- 0.0

#Go back to long data format

mets_data <- mets_data_wide %>%
  gather(Season, WAR, "1962":"2021")

#Change Year column to integer

mets_data$Season <- as.integer(mets_data$Season)

#Rearrange data and group by player to calculate running total of career WAR by year

mets_data <- mets_data %>%
  arrange(playerid, Season) %>%
  group_by(playerid) %>%
  mutate(CareerWAR = round(cumsum(WAR),1)) %>%
  ungroup()

#Rename columns

mets_data <- mets_data %>%
  rename(SeasonWAR = WAR, WAR = CareerWAR)

#Create career WAR ranking by year and limit it to the top 10 spots

top_data <- mets_data %>%
  group_by(Season) %>%
  arrange(Season, -WAR) %>%
  mutate(Rank = row_number()) %>%
  mutate(Rank = ifelse(Rank > 10, 11, Rank)) %>%
  filter((Rank <= 10 | Name == "David Wright") & Season >= 2004) %>%
  ungroup()

#Create highlight for David Wright

top_data <- top_data %>%
  mutate(Highlight = ifelse(Name == "David Wright", "yes", "no"))

#Create visual

p <- top_data %>%
  ggplot(aes(x = -Rank, y = WAR, group = Name)) +
  geom_tile(aes(y = WAR/2, height = WAR, fill = Highlight), width = .9) +
  geom_text(aes(label = paste(Name, formatC(WAR, format = "f", digits = 1))), hjust = "left", color = "black", nudge_y = 2) +
  coord_flip(clip="off") +
  scale_x_discrete("") +
  scale_y_continuous("", labels = scales::comma, limits = c(0,80)) +
  hrbrthemes::theme_ipsum(plot_title_size = 20, subtitle_size = 16, caption_size = 10, base_size = 12) +
  scale_fill_manual(values = c("yes" = "#FF5910", "no" = "#002D72")) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = "none",
        plot.margin = margin(1,1,1,2,"cm")) +
  transition_time(Season) +
  ease_aes("cubic-in-out") +
  labs(title = "Top Mets Position Players by Career WAR",
       subtitle = "{frame_time} Season",
       caption = "WAR figures from FanGraphs.com")

#Animate the visual

anim <- animate(p, nframes = 23, fps = 1, end_pause = 5, renderer = magick_renderer())

#Save the visual

anim_save("Mets Players.gif", anim)