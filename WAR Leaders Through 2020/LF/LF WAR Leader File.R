#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/WAR Leaders Through 2020/LF")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(gganimate)
library(hrbrthemes)

#Import data

left_field_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/WAR Leaders Through 2020/LF/LF WAR Leaders Bar Chart Race.csv")

#Make data wide so that I can replace null values with zeros

left_field_data <- left_field_data %>%
  arrange(playerid, Season) %>%
  group_by(playerid) %>%
  mutate(FirstYear = min(Season)) %>%
  mutate(Era = ifelse(FirstYear <= 1900, "19th Century",
                      ifelse(FirstYear <= 1919, "Dead Ball",
                             ifelse(FirstYear <= 1941, "Live Ball",
                                    ifelse(FirstYear <= 1960, "Integration",
                                           ifelse(FirstYear <= 1976, "Expansion",
                                                  ifelse(FirstYear <= 1993, "Free Agency",
                                                         ifelse(FirstYear <= 2005, "Steroid", "Modern"))))))))

#Reorder the eras to be chronological

left_field_data$Era <- factor(left_field_data$Era, levels = c("19th Century", "Dead Ball", "Live Ball", "Integration", "Expansion", "Free Agency", "Steroid", "Modern"))

left_field_data_wide <- left_field_data %>%
  spread(Season, WAR)

#Replace null values with zeros to create WAR totals for years where player is no longer active

left_field_data_wide[is.na(left_field_data_wide)] <- 0.0

#Go back to long data format

left_field_data <- left_field_data_wide %>%
  gather(Season, WAR, "1871":"2020")

#Change Year column to integer

left_field_data$Season <- as.integer(left_field_data$Season)

#Rearrange data and group by player to calculate running total of career WAR by year

left_field_data <- left_field_data %>%
  arrange(playerid, Season) %>%
  group_by(playerid) %>%
  mutate(CareerWAR = round(cumsum(WAR),1)) %>%
  ungroup()

#Rename columns

left_field_data <- left_field_data %>%
  rename(SeasonWAR = WAR, WAR = CareerWAR)

#Create career WAR ranking by year and limit it to the top 10 spots

top_data <- left_field_data %>%
  group_by(Season) %>%
  arrange(Season, -WAR) %>%
  mutate(Rank = row_number()) %>%
  filter(Rank <= 10) %>%
  filter(WAR > 0) %>%
  ungroup()

#Create visual using top_data

p <- top_data %>%
  ggplot(aes(x = -Rank, y = WAR, group = Name)) +
  geom_tile(aes(y = WAR/2, height = WAR, fill = Era), width = .9) +
  geom_text(aes(label = paste(Name, formatC(WAR, format = "f", digits = 1))), hjust = "left", color = "black", nudge_y = 2) +
  coord_flip(clip="off") +
  scale_x_discrete("") +
  scale_y_continuous("", labels = scales::comma, limits = c(0,200)) +
  hrbrthemes::theme_ipsum(plot_title_size = 20, subtitle_size = 16, caption_size = 10, base_size = 12) +
  scale_fill_brewer(palette = "Spectral") +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = c(.9, .3),
        legend.title.align = 0.5,
        plot.margin = margin(1,1,1,2,"cm")) +
  transition_time(Season) +
  ease_aes("cubic-in-out") +
  labs(title = "Top 10 Left Fielders by Career WAR",
       subtitle = "{frame_time} Season",
       caption = "WAR figures from FanGraphs.com")

#Animate the visual

anim <- animate(p, nframes = 160, fps = 2, end_pause = 10, renderer = magick_renderer())

#Save the visual

anim_save("LF_WAR_Bar_Race.gif", anim)