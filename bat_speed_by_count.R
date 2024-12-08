library(readr)
library(tidyverse)
library(gt)

statcast_data2024 <- read_csv("C:/Users/david/Downloads/statcast_data2024.csv")

summary(statcast_data2024$bat_speed)

df1 <- statcast_data2023 %>%
  arrange(game_pk, at_bat_number, pitch_number) %>%
  group_by(game_pk, at_bat_number) %>%
  mutate(lag_bat_speed = lag(bat_speed),
         lag_pitch_type = lag(pitch_type),
         count = paste0(balls, "-", strikes)) %>%
  filter(strikes == 2) %>%
  select(game_pk, at_bat_number, pitch_number, count, bat_speed, lag_bat_speed, pitch_type, lag_pitch_type) %>%
  drop_na(bat_speed, lag_bat_speed) %>%
  mutate(diff_bat_speed = lag_bat_speed - bat_speed, 
         pitch_combo = paste0(lag_pitch_type, "-", pitch_type)) %>%
  filter(lag_pitch_type == "FF") %>%
  group_by(pitch_combo) %>%
  summarise(avg_bat_speed_diff = round(mean(diff_bat_speed),2),
            num_seq = n())

df1 %>%
  arrange(-avg_bat_speed_diff) %>%
  gt() %>%
  tab_header(title = md("**Bat Speed Differences by Sequence**"),
             subtitle = md("Two Strike Counts Only. Positive difference means that previous swing was faster.")) %>%
  cols_label(pitch_combo = "Pitch Combo",
             avg_bat_speed_diff = "Average Bat Speed Difference (MPH)",
             num_seq = "Total Instances")


df2 <- statcast_data2023 %>%
  arrange(game_pk, at_bat_number, pitch_number) %>%
  group_by(game_pk, at_bat_number) %>%
  mutate(lag_bat_speed = lag(bat_speed),
         lag_pitch_type = lag(pitch_type),
         count = paste0(balls, "-", strikes)) %>%
  filter(strikes == 2) %>%
  select(game_pk, at_bat_number, pitch_number, count, bat_speed, lag_bat_speed, pitch_type, lag_pitch_type) %>%
  drop_na(bat_speed, lag_bat_speed) %>%
  mutate(diff_bat_speed = lag_bat_speed - bat_speed, 
         pitch_combo = paste0(lag_pitch_type, "-", pitch_type))
