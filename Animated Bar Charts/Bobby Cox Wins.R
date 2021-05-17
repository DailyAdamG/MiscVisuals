#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(gganimate)
library(hrbrthemes)

#Import data

cox_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Bar Charts/Bobby Cox Wins.csv")

#Make data wide so that I can replace null values with zeros

cox_data_wide <- cox_data %>%
  spread(yearID, W)

#Replace null values with zeros to create Win totals for years where manager is no longer active

cox_data_wide[is.na(cox_data_wide)] <- 0.0

#Go back to long data format

cox_data <- cox_data_wide %>%
  gather(yearID, W, "1871":"2010")

#Change Year column to integer

cox_data$yearID <- as.integer(cox_data$yearID)

#Rearrange data and group by player to calculate running total of career Wins by year

cox_data <- cox_data %>%
  arrange(playerID, yearID) %>%
  group_by(playerID) %>%
  mutate(CareerWins = cumsum(W)) %>%
  ungroup()

#Rename columns

cox_data <- cox_data %>%
  rename(SeasonWins = W, Wins = CareerWins)

#Create career WAR ranking by year and limit it to the top 10 spots

top_data <- cox_data %>%
  group_by(yearID) %>%
  arrange(yearID, -Wins) %>%
  mutate(Rank = row_number()) %>%
  mutate(Rank = ifelse(Rank > 10, 11, Rank)) %>%
  filter((Rank <= 10 | Name == "Bobby Cox") & yearID >= 1978 & yearID <= 2010) %>%
  ungroup()

#Create highlight for Bobby Cox

top_data <- top_data %>%
  mutate(Highlight = ifelse(Name == "Bobby Cox", "yes", "no"))

#Create visual

p <- top_data %>%
  ggplot(aes(x = -Rank, y = Wins, group = Name)) +
  geom_tile(aes(y = Wins/2, height = Wins, fill = Highlight), width = .9) +
  geom_text(aes(label = paste(Name, Wins)), hjust = "left", color = "black", nudge_y = 50) +
  coord_flip(clip="off") +
  scale_x_discrete("") +
  scale_y_continuous("", labels = scales::comma, limits = c(0,4000)) +
  hrbrthemes::theme_ipsum(plot_title_size = 20, subtitle_size = 16, caption_size = 10, base_size = 12) +
  scale_fill_manual(values = c("yes" = "#CE1141", "no" = "#13274F")) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = "none",
        plot.margin = margin(1,1,1,2,"cm")) +
  transition_time(yearID) +
  ease_aes("cubic-in-out") +
  labs(title = "Bobby Cox Managerial Career Wins by Season and His Rank in History",
       subtitle = "{frame_time} Season")

#Animate the visual

anim <- animate(p, height = 600, width = 800, nframes = 38, fps = 1, end_pause = 5, renderer = magick_renderer())

#Save the visual

anim_save("Bobby Cox Wins.gif", anim)