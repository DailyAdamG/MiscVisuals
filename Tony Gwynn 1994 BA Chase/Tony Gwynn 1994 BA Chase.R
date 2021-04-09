#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Tony Gwynn 1994 BA Chase")

#Loading libraries

library(tidyverse)
library(lubridate)
library(gganimate)
library(hrbrthemes)
library(weights)

#Import data

data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Tony Gwynn 1994 BA Chase/Tony Gwynn 1994 BA Chase.csv")

#Ensure that date is in correct format

data$Date <- mdy(data$Date)

#Get running BA

data <- data %>%
  arrange(Date) %>%
  mutate(SeasonHits = cumsum(H),
         SeasonABs = cumsum(AB),
         RunningBA = round(SeasonHits / SeasonABs,3))


#Create visual

p <- data %>%
  ggplot(aes(x = Date, y = RunningBA)) +
  geom_line(size = 2, color = "#2F241D") +
  geom_point(size = 12, color = "#2F241D") +
  geom_text(aes(label = rd(RunningBA, digits = 3)), color = "#FFC425", fontface = "bold", size = 12/.pt, hjust = .5, vjust = .5) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  geom_hline(yintercept = .400, linetype = "dashed", size = 1.25) +
  hrbrthemes::theme_ipsum(plot_title_size = 24, base_size = 16) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 65)) +
  transition_reveal(Date) +
  ease_aes("linear") +
  labs(title = "Tony Gwynn's 1994 Chase for .400", x = "", y = "")

#Animate the visual

anim <- animate(p, nframes = 150, fps = 5, width = 608, height = 365, end_pause = 10, renderer = magick_renderer())

anim_save("Tony Gwynn 1994 BA Chase.gif", anim)