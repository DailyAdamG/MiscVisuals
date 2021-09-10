#Set working directory and loading libraries

setwd("C:/Users/daily/Desktop/Repositories/MiscVisuals/Animated Game Reports/09_08_21 Sandy Alcantara")

library(ggplot2)
library(ggrepel)
library(gganimate)
library(dplyr)

#Import csv file

data <- read.csv("09_08_21 Sandy Alcantara.csv")

################################################################################

#IMPORTANT NOTE!!!

#For any ABs that last only 1 pitch, change the transition_time to transition_states function.

#Perform a find and replace for transition_states before doing any other code to 
#prevent any unwanted errors.


################################################################################

#Create a pitch result description

data <- data %>%
  mutate(play_desc = ifelse(type == "B", "Ball",
                            ifelse(type == "X", "Ball In Play",
                                   ifelse(type == "S" & description == "called_strike", "Called Strike",
                                          ifelse(type == "S" & (description == "bunt_foul_tip" | description == "foul" | description == "foul_bunt" | description == "foul_pitchout" | description == "foul_tip"), "Foul", "Swinging Strike")))))

#Create a batted ball type

data <- data %>%
  mutate(my_bbe = ifelse(bb_type == "fly_ball", "Fly Ball",
                         ifelse(bb_type == "ground_ball", "Ground Ball",
                                ifelse(bb_type == "line_drive", "Line Drive",
                                       ifelse(bb_type == "popup", "Pop-up", "")))))

#Create a plate appearance result

data <- data %>%
  mutate(my_events = ifelse(events == "caught_stealing_2b" | events == "caught_stealing_3b" | events == "caught_stealing_home" | events == "pickoff_caught_stealing_2b" | events == "pickoff_caught_stealing_3b", "Runner Caught Stealing",
                            ifelse(events == "double", "Double",
                                   ifelse(events == "double_play" | events == "grounded_into_double_play" | events == "sac_fly_double_play", "Double Play",
                                          ifelse(events == "field_error", "Reach on Error",
                                                 ifelse(events == "field_out" | events == "fielders_choice_out" | events == "force_out" | events == "other_out", "Out",
                                                        ifelse(events == "fielders_choice", "Reach Safely on Fielder's Choice",
                                                               ifelse(events == "hit_by_pitch", "Hit by Pitch",
                                                                      ifelse(events == "home_run", "Home Run",
                                                                             ifelse(events == "interf_def", "Reached on Interference",
                                                                                    ifelse(events == "pickoff_1b" | events == "pickoff_2b" | events == "pickoff_3b", "Runner Picked Off but Safe",
                                                                                           ifelse(events == "sac_bunt" | events == "sac_fly", "Sacrifice",
                                                                                                  ifelse(events == "single", "Single",
                                                                                                         ifelse(events == "strikeout", "Strikeout",
                                                                                                                ifelse(events == "strikeout_double_play", "Strikeout and Caught Stealing",
                                                                                                                       ifelse(events == "triple", "Triple",
                                                                                                                              ifelse(events == "triple_play", "Triple Play",
                                                                                                                                     ifelse(events == "walk", "Walk", ""))))))))))))))))))

#Combine batted ball events and plate appearance result

data <- data %>%
  mutate(my_des = paste(my_bbe,my_events))

#Convert movement into inches instead of feet

data <- data %>%
  mutate(h_break = pfx_x*12) %>%
  mutate(v_break = pfx_z*12)

#Create label

data <- data %>%
  mutate(my_label = paste(inning_topbot, inning, away_team, away_score, "-", home_team, home_score, "\n",
                          "Count:", balls, "-", strikes, "Outs:", outs_when_up,"\n",
                          "Batter:", batter_name, "\n",
                          pitch_name,format(round(release_speed,1)),"MPH","\n",
                          "Spin Rate:",release_spin_rate,"\n",
                          "VM:",format(round(v_break,1)), "HM:",format(round(h_break,1)),"\n",my_des))

#Create at bat number for pitcher

data <- data %>%
  arrange(at_bat_number, pitch_number) %>%
  group_by(at_bat_number) %>%
  mutate(my_ab = cur_group_id()) %>%
  ungroup()

#Create strike zone

topKzone <- 3.5
botKzone <- 1.6
leftKzone <- -.95
rightKzone <- .95
kZone <- data.frame(
  x=c(leftKzone,leftKzone,rightKzone,rightKzone,leftKzone),
  y=c(botKzone,topKzone,topKzone,botKzone,botKzone))

#Assign colors to pitch result

cols <- c("Ball" = "dark green", "Called Strike" = "coral", "Foul" = "orange", "Ball In Play" = "blue", "Swinging Strike" = "red")


#Create visual for AB 1

viz1 <- data %>%
  filter(my_ab == 1) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_states(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 1

anim1 <- animate(viz1, nframes = nrow(data %>% filter(my_ab == 1)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 2

viz2 <- data %>%
  filter(my_ab == 2) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 2

anim2 <- animate(viz2, nframes = nrow(data %>% filter(my_ab == 2)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 3

viz3 <- data %>%
  filter(my_ab == 3) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 3

anim3 <- animate(viz3, nframes = nrow(data %>% filter(my_ab == 3)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 4

viz4 <- data %>%
  filter(my_ab == 4) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 4

anim4 <- animate(viz4, nframes = nrow(data %>% filter(my_ab == 4)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 5

viz5 <- data %>%
  filter(my_ab == 5) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 5

anim5 <- animate(viz5, nframes = nrow(data %>% filter(my_ab == 5)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 6

viz6 <- data %>%
  filter(my_ab == 6) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 6

anim6 <- animate(viz6, nframes = nrow(data %>% filter(my_ab == 6)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 7

viz7 <- data %>%
  filter(my_ab == 7) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_states(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 7

anim7 <- animate(viz7, nframes = nrow(data %>% filter(my_ab == 7)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 8

viz8 <- data %>%
  filter(my_ab == 8) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 8

anim8 <- animate(viz8, nframes = nrow(data %>% filter(my_ab == 8)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 9

viz9 <- data %>%
  filter(my_ab == 9) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 9

anim9 <- animate(viz9, nframes = nrow(data %>% filter(my_ab == 9)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 10

viz10 <- data %>%
  filter(my_ab == 10) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_states(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 10

anim10 <- animate(viz10, nframes = nrow(data %>% filter(my_ab == 10)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 11

viz11 <- data %>%
  filter(my_ab == 11) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 11

anim11 <- animate(viz11, nframes = nrow(data %>% filter(my_ab == 11)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 12

viz12 <- data %>%
  filter(my_ab == 12) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 12

anim12 <- animate(viz12, nframes = nrow(data %>% filter(my_ab == 12)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 13

viz13 <- data %>%
  filter(my_ab == 13) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 13

anim13 <- animate(viz13, nframes = nrow(data %>% filter(my_ab == 13)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 14

viz14 <- data %>%
  filter(my_ab == 14) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 14

anim14 <- animate(viz14, nframes = nrow(data %>% filter(my_ab == 14)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 15

viz15 <- data %>%
  filter(my_ab == 15) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 15

anim15 <- animate(viz15, nframes = nrow(data %>% filter(my_ab == 15)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 16

viz16 <- data %>%
  filter(my_ab == 16) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 16

anim16 <- animate(viz16, nframes = nrow(data %>% filter(my_ab == 16)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 17

viz17 <- data %>%
  filter(my_ab == 17) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 17

anim17 <- animate(viz17, nframes = nrow(data %>% filter(my_ab == 17)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 18

viz18 <- data %>%
  filter(my_ab == 18) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 18

anim18 <- animate(viz18, nframes = nrow(data %>% filter(my_ab == 18)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 19

viz19 <- data %>%
  filter(my_ab == 19) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 19

anim19 <- animate(viz19, nframes = nrow(data %>% filter(my_ab == 19)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 20

viz20 <- data %>%
  filter(my_ab == 20) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 20

anim20 <- animate(viz20, nframes = nrow(data %>% filter(my_ab == 20)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 21

viz21 <- data %>%
  filter(my_ab == 21) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 21

anim21 <- animate(viz21, nframes = nrow(data %>% filter(my_ab == 21)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 22

viz22 <- data %>%
  filter(my_ab == 22) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 22

anim22 <- animate(viz22, nframes = nrow(data %>% filter(my_ab == 22)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 23

viz23 <- data %>%
  filter(my_ab == 23) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 23

anim23 <- animate(viz23, nframes = nrow(data %>% filter(my_ab == 23)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 24

viz24 <- data %>%
  filter(my_ab == 24) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 24

anim24 <- animate(viz24, nframes = nrow(data %>% filter(my_ab == 24)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 25

viz25 <- data %>%
  filter(my_ab == 25) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 25

anim25 <- animate(viz25, nframes = nrow(data %>% filter(my_ab == 25)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 26

viz26 <- data %>%
  filter(my_ab == 26) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 26

anim26 <- animate(viz26, nframes = nrow(data %>% filter(my_ab == 26)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 27

viz27 <- data %>%
  filter(my_ab == 27) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 27

anim27 <- animate(viz27, nframes = nrow(data %>% filter(my_ab == 27)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 28

viz28 <- data %>%
  filter(my_ab == 28) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 28

anim28 <- animate(viz28, nframes = nrow(data %>% filter(my_ab == 28)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 29

viz29 <- data %>%
  filter(my_ab == 29) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 29

anim29 <- animate(viz29, nframes = nrow(data %>% filter(my_ab == 29)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 30

viz30 <- data %>%
  filter(my_ab == 30) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 30

anim30 <- animate(viz30, nframes = nrow(data %>% filter(my_ab == 30)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 31

viz31 <- data %>%
  filter(my_ab == 31) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 31

anim31 <- animate(viz31, nframes = nrow(data %>% filter(my_ab == 31)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Create visual for AB 32

viz32 <- data %>%
  filter(my_ab == 32) %>%
  ggplot(aes(x = plate_x, y = plate_z, color=play_desc, label = pitch_number, group = pitch_number))+
  geom_path(aes(x,y), inherit.aes = FALSE, data=kZone, lty= 2, col="black")+
  geom_point(size=7, shape = 19)+
  scale_color_manual(values = cols, name = "Pitch Result", limits = c("Swinging Strike", "Called Strike", "Foul", "Ball", "Ball In Play")) +
  geom_text(aes(label = pitch_number, group = pitch_number), color = "white", fontface = "bold", hjust = 0.5, vjust = 0.5) +
  geom_label_repel(aes(label = my_label, group = my_label), color = "black", fontface = "bold", nudge_y = 1) +
  coord_equal()+
  xlim(-2.2,2.2)+
  ylim(0,5)+
  labs(title = paste(data$game_date, data$away_team, "at", data$home_team,"\nStarting Pitcher:",data$pitcher_name, ifelse(data$inning_topbot == "Bot", data$away_team, data$home_team)),
       caption = "*From Hitter Perspective",
       x = "Horizontal Distance (ft)",
       y = "Vertical Distance (ft)") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.text = element_text(size = 14)) +
  transition_time(pitch_number) +
  ease_aes("cubic-in-out") +
  shadow_mark(alpha = 0.3)

#Create animation for AB 32

anim32 <- animate(viz32, nframes = nrow(data %>% filter(my_ab == 32)), fps = .25, width = 600, height = 600, renderer = magick_renderer())

#Combine animations into 1 GIF

anim_save("09_08_21 Sandy Alcantara.gif", c(anim1, anim2, anim3, anim4, anim5,
                                        anim6, anim7, anim8, anim9, anim10,
                                        anim11, anim12, anim13, anim14, anim15,
                                        anim16, anim17, anim18, anim19, anim20,
                                        anim21, anim22, anim23, anim24, anim25,
                                        anim26, anim27, anim28, anim29, anim30,
                                        anim31, anim32
))