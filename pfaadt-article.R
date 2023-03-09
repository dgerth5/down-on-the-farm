library(baseballr)
library(tidyverse)
library(furrr)
library(readr)

dates <- data.frame(day = rep(seq(as.Date("2023-02-24"),as.Date("2023-03-07"), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map(function(x) mlb_game_pks(dates$day[x],
                                      level_ids = 1))

ml_game_pks <- minor_league_game_pk_lst %>%
  bind_rows() %>%
  dplyr::filter(status.codedGameState == "F",
                !is.na(game_pk)) %>%
  pull(game_pk)

plan("multisession", workers = 3)

safe_pbp <- safely(mlb_pbp)

ml_pbp <- 1:length(ml_game_pks) %>%
  furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
  purrr::map("result") %>%
  bind_rows()

ml_pbp <- ml_pbp %>% as.data.frame()

# 2023

pfaadt <- scrape_statcast_savant(
  start_date = "2023-02-24",
  end_date = "2023-03-07",
  playerid = 694297,
  player_type = "pitcher"
)

library(gt)
library(gtExtras)

pfaadt %>%
  group_by(pitch_type) %>%
  summarise(mean_velo = mean(release_speed),
            mean_pfx_x = mean(pfx_x)*12,
            mean_pfx_z = mean(pfx_z)*12,
            mean_spin_rate = mean(release_spin_rate),
            mean_release_x = mean(release_pos_x),
            mean_release_z = mean(release_pos_z),
            mean_extension = mean(release_extension),
            count = n()) %>%
  arrange(-count) %>%
  gt() %>%
  tab_header(title = md("**Brandon Pfaadt Statcast Data**"),
             subtitle = md("Spring Training 2023")) %>%
  fmt_number(columns = c("mean_velo", "mean_pfx_x", "mean_pfx_z", "mean_spin_rate", "mean_release_z", "mean_extension"), decimals = 1) %>%
  cols_label(pitch_type = "Pitch Type",
             mean_velo = "Average Velocity",
             mean_pfx_x = "Average Horizontal Break",
             mean_pfx_z = "Average Vertical Break",
             mean_spin_rate = "Average Spin Rate",
             mean_release_z = "Average Release Height",
             mean_extension = "Average Extension",
             count = "Pitches Thrown")
