library(tidyverse)
library(baseballr)
library(lubridate)
library(purrr)
library(furrr)

# get data

leagues = mlb_league(2023)
leagues2 = leagues %>%
  select(league_id, sport_id, league_name, season_date_info_regular_season_start_date, season_date_info_regular_season_end_date)

dates <- data.frame(day = rep(seq(as.Date("2023-03-31"),Sys.Date(), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map_df(function(x) mlb_game_pks(dates$day[x],
                                         level_ids = c(11,12,13,14)))

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

# get season stats

# apply stabilization method
# copying from post: https://downonthefarm.substack.com/p/estimating-minor-league-stabilization

pad_numbers <- data.frame(lvl = c("A", "A+", "AA", "AAA"),
                          k = c(72,66,69,66),
                          bb = c(133,179,174,174),
                          hr = c(240,165,218,220))

