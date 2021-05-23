#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(gganimate)
library(hrbrthemes)

#Import data

rivera_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Bar Charts/Mariano Rivera Saves.csv")

#Make data wide so that I can replace null values with zeros

rivera_data_wide <- rivera_data %>%
  spread(Season, SV)

#Replace null values with zeros to create Win totals for years where player is no longer active

rivera_data_wide[is.na(rivera_data_wide)] <- 0.0

#Go back to long data format

rivera_data <- rivera_data_wide %>%
  gather(Season, SV, "1871":"2020")

#Change Year column to integer

rivera_data$Season <- as.integer(rivera_data$Season)

#Rearrange data and group by player to calculate running total of career Saves by year

rivera_data <- rivera_data %>%
  arrange(playerid, Season) %>%
  group_by(playerid) %>%
  mutate(CareerSaves = cumsum(SV)) %>%
  ungroup()

#Rename columns

rivera_data <- rivera_data %>%
  rename(SeasonSaves = SV, Saves = CareerSaves)

#Create career Saves ranking by year and limit it to the top 10 spots

top_data <- rivera_data %>%
  group_by(Season) %>%
  arrange(Season, -Saves) %>%
  mutate(Rank = row_number()) %>%
  mutate(Rank = ifelse(Rank > 10, 11, Rank)) %>%
  filter((Rank <= 10 | Name == "Mariano Rivera") & Season >= 1995 & Season <= 2013) %>%
  ungroup()

#Create highlight for Mariano Rivera

top_data <- top_data %>%
  mutate(Highlight = ifelse(Name == "Mariano Rivera", "yes", "no"))

#Create visual

p <- top_data %>%
  ggplot(aes(x = -Rank, y = Saves, group = Name)) +
  geom_tile(aes(y = Saves/2, height = Saves, fill = Highlight), width = .9, color = "#0C2340") +
  geom_text(aes(label = paste(Name, Saves)), hjust = "left", color = "black", nudge_y = 10) +
  coord_flip(clip="off") +
  scale_x_discrete("") +
  scale_y_continuous("", labels = scales::comma, limits = c(0,800)) +
  hrbrthemes::theme_ipsum(plot_title_size = 20, subtitle_size = 16, caption_size = 10, base_size = 12) +
  scale_fill_manual(values = c("yes" = "#0C2340", "no" = "grey")) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = "none",
        plot.margin = margin(1,1,1,2,"cm")) +
  transition_time(Season) +
  ease_aes("cubic-in-out") +
  labs(title = "Mariano Rivera Career Saves by Season",
       subtitle = "{frame_time} Season")

#Animate the visual

anim <- animate(p, nframes = 24, fps = 1, end_pause = 5, renderer = magick_renderer())

#Save the visual

anim_save("Mariano Rivera Saves.gif", anim)