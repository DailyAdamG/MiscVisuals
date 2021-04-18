#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts")

#Load necessary libraries

library(tidyverse)
library(ggplot2)
library(scales)
theme_set(theme_bw())

#Import data

hr_data <- read.csv("C:/Users/daily/Desktop/Repositories/MiscVisuals/Bar Charts/HR_PA_IBB.csv")

#Limit to top 10

top_ten <- hr_data %>%
  arrange(desc(HR_PA_IBB), Season ) %>%
  mutate(Rank = row_number()) %>%
  filter(Rank <= 10) %>%
  ungroup()

#Create visual

top_ten %>%
  ggplot(aes(x = reorder(Season_Name, desc(HR_PA_IBB)), y = HR_PA_IBB, fill = Name)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks = seq(0, .12, .01), labels = percent) +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Highest Home Run per Plate Appearance Rates in a MLB Season",
       subtitle = "*Not including intentional walks as a plate appearance*",
       x = "",
       y = "",
       fill = "Player")