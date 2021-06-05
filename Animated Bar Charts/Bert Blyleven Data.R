#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(gganimate)
library(hrbrthemes)

#Import data

bert_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Bar Charts/Bert Blyleven Data.csv")

#Select only relevant data

bert_data <- bert_data %>%
select(Season, Name, playerid, SO)

#Make data wide so that I can replace null values with zeros

bert_data_wide <- bert_data %>%
  spread(Season, SO)

#Replace null values with zeros to create SO totals for years where player is no longer active

bert_data_wide[is.na(bert_data_wide)] <- 0.0

#Go back to long data format

bert_data <- bert_data_wide %>%
  gather(Season, SO, "1871":"1992")

#Change Year column to integer

bert_data$Season <- as.integer(bert_data$Season)

#Rearrange data and group by player to calculate running total of career SO by year

bert_data <- bert_data %>%
  arrange(playerid, Season) %>%
  group_by(playerid) %>%
  mutate(CareerSO = cumsum(SO)) %>%
  ungroup()

#Rename columns

bert_data <- bert_data %>%
  rename(SeasonSO = SO, SO = CareerSO)

#Create career SO ranking by year and limit it to the top 10 spots

top_data <- bert_data %>%
  group_by(Season) %>%
  arrange(Season, -SO) %>%
  mutate(Rank = row_number()) %>%
  mutate(Rank = ifelse(Rank > 10, 11, Rank)) %>%
  filter((Rank <= 10 | Name == "Bert Blyleven") & Season >= 1970 & Season <= 1992) %>%
  ungroup()

#Create highlight for Mariano Rivera

top_data <- top_data %>%
  mutate(Highlight = ifelse(Name == "Bert Blyleven", "yes", "no"))

#Create visual

p <- top_data %>%
  ggplot(aes(x = -Rank, y = SO, group = Name)) +
  geom_tile(aes(y = SO/2, height = SO, fill = Highlight), width = .9, color = "black") +
  geom_text(aes(label = paste(Name, SO)), hjust = "left", color = "black", nudge_y = 50) +
  coord_flip(clip="off") +
  scale_x_discrete("") +
  scale_y_continuous("", labels = scales::comma, limits = c(0, 7000), breaks = seq(0, 7000, 1000)) +
  hrbrthemes::theme_ipsum(plot_title_size = 20, subtitle_size = 16, caption_size = 10, base_size = 12) +
  scale_fill_manual(values = c("yes" = "#D31145", "no" = "#002B5C")) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = "none",
        plot.margin = margin(1,1,1,2,"cm")) +
  transition_time(Season) +
  ease_aes("cubic-in-out") +
  labs(title = "Bert Blyleven Career Strikeouts by Season",
       subtitle = "{frame_time} Season")

#Animate the visual

anim <- animate(p, nframes = 28, width = 750, fps = 1, end_pause = 5, renderer = magick_renderer())

#Save the visual

anim_save("Bert Blyleven SO.gif", anim)