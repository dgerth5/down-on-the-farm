library(baseballr)
library(tidyverse)
library(furrr)

leagues <- mlb_league(2024) %>% select(league_name, sport_id, season_date_info_season_start_date)

colnames(leagues)

dates <- data.frame(day = rep(seq(as.Date("2024-03-29"),as.Date("2024-09-22"), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map(function(x) mlb_game_pks(dates$day[x],
                                      level_ids = c(11,12,13,14)))

saveRDS(minor_league_game_pk_lst, "milb_game.RDS")


ml_game_pks <- minor_league_game_pk_lst %>%
  bind_rows() %>%
  dplyr::filter(status.codedGameState == "F",
                !is.na(game_pk)) %>%
  pull(game_pk)

plan("multisession", workers = 4)

safe_pbp <- safely(mlb_pbp)

ml_pbp <- 1:length(ml_game_pks) %>%
  furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
  purrr::map("result") %>%
  bind_rows()

ml_pbp <- ml_pbp %>% as.data.frame()

write_csv(ml_pbp, "milb-pbp-2024.csv")
