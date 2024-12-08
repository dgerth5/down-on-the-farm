library(tidyverse)
library(readr)
library(zoo)
library(ggplot2)

single_a_pbp2023 <- read_csv("single_a_pbp2023.csv")

bad_events <- c("Pickoff Caught Stealing 2B","Runner Out","Caught Stealing 3B","Catcher Interference",
                "Wild Pitch","Pickoff 1B","Caught Stealing 2B","Pickoff Caught Stealing 3B",
                "Caught Stealing Home","Pickoff 3B","Stolen Base 3B","Pickoff Caught Stealing Home",
                "Pickoff 2B")

finished_ab <- single_a_pbp2023 %>%
  filter(isPitch == TRUE) %>%
  group_by(game_pk) %>%
  arrange(atBatIndex, pitchNumber, .by_group = TRUE) %>%
  mutate(ld = lead(matchup.batter.id, 1),
         last_pitch_ab = if_else(is.na(ld) == TRUE, 1, 
                                 if_else(matchup.batter.id != ld, 1, 0)),
         strikeout = if_else(result.eventType == "strikeout" | result.eventType == "strikeout_double_play", 1, 0),
         walk = if_else(result.eventType == "walk", 1, 0),
         home_run = if_else(result.eventType == "home_run", 1, 0))  %>%
  filter(last_pitch_ab == 1) %>%
  filter(!result.event %in% bad_events) %>%
  ungroup() %>%
  select(game_date, game_pk, atBatIndex, home_level_name, matchup.pitcher.fullName, walk, strikeout) %>%
  arrange(game_date) %>%
  filter(matchup.pitcher.fullName == "Jackson Ferris") 

n <- 30

fa2 <- finished_ab %>%
  mutate(batters = row_number(),
         rolling_n_k = c(rep(0,n-1),rollmean(strikeout, n)),
         rolling_n_bb = c(rep(0,n-1),rollmean(walk, n))) %>%
  select(game_date, rolling_n_k, rolling_n_bb)

# Assuming your dataframe is named df
ggplot(fa2, aes(x = game_date)) + 
  geom_line(aes(y = rolling_n_k, colour = "rolling_n_k")) + 
  geom_line(aes(y = rolling_n_bb, colour = "rolling_n_bb")) +
  scale_colour_manual(values = c("rolling_n_k" = "blue", "rolling_n_bb" = "red")) +
  labs(colour = "Metric") +
  ggtitle("Jackson Ferris Rolling K% and BB%") +
  xlab("Game Date") +
  ylab("Values")

