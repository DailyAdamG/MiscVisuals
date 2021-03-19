#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/HR Leader Bar Race")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(gganimate)
library(hrbrthemes)

#Import data

data1 <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/HR Leader Bar Race/HR Leaders through 1960.csv")

data2 <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/HR Leader Bar Race/HR Leaders since 1961.csv")

#Combine data

data <- rbind(data1,data2)

#Make data wide so that I can replace null values with zeros

data_wide <- data %>%
  spread(yearID, HR)

#Replace null values with zeros to create HR totals for years where player is no longer active

data_wide[is.na(data_wide)] <- 0

#Go back to long data format

data <- data_wide %>%
  gather(Year, HR, "1871":"2020")

#Change Year column to integer

data$Year <- as.integer(data$Year)

#Rearrange data and group by player to calculate running total of career HR by year

data <- data %>%
  arrange(playerID, Year) %>%
  group_by(playerID) %>%
  mutate(CareerHR = cumsum(HR)) %>%
  ungroup()

#Rename columns

data <- data %>%
  rename(Name = fullName, SingleHR = HR, HR = CareerHR)

#Create career HR ranking by year and limit it to the top 20 spots

top_data <- data %>%
  group_by(Year) %>%
  arrange(Year, -HR) %>%
  mutate(Rank = row_number()) %>%
  filter(Rank <= 20) %>%
  ungroup()

#Create visual using top_data

p <- top_data %>%
  ggplot(aes(x = -Rank, y = HR, group = Name)) +
  geom_tile(aes(y = HR/2, height = HR, fill = Name), width = .9, show.legend = FALSE) +
  geom_text(aes(label = paste(Name, HR)), hjust = "left", color = "black", nudge_y = 10) +
  coord_flip(clip="off") +
  scale_x_discrete("") +
  scale_y_continuous("", labels = scales::comma, limits = c(0,1000)) +
  hrbrthemes::theme_ipsum(plot_title_size = 32, subtitle_size = 24, caption_size = 20, base_size = 20) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = c(0.4, 0.2),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.y=element_blank()) +
  transition_time(Year) +
  ease_aes("cubic-in-out") +
  labs(title = "Career Home Runs",
       subtitle = "{frame_time} Season")

#Animate the visual

anim <- animate(p, nframes = 150, fps = 1.25, renderer = magick_renderer())

#Save the visual

anim_save("HR_Leader_Bar_Race.gif", anim)