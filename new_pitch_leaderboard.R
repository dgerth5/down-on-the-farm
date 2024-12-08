library(tidyverse)
library(baseballr)
library(readr)


triple_a_pbp2023 <- read_csv("triple_a_pbp2023.csv")

pitches <- triple_a_pbp2023 %>%
  group_by(matchup.pitcher.id, matchup.pitcher.fullName, details.type.description) %>%
  summarise(mean_velo = mean(pitchData.startSpeed),
            mean_ivb = mean(pitchData.breaks.breakVerticalInduced),
            mean_hb = mean(pitchData.breaks.breakHorizontal),
            mean_sr = mean(pitchData.breaks.spinRate),
            n = n()) %>%
  drop_na()


dates <- data.frame(day = rep(seq(as.Date("2024-3-25"),as.Date("2024-4-1"), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map(function(x) mlb_game_pks(dates$day[x],
                                      level_ids = 11))

ml_game_pks <- minor_league_game_pk_lst %>%
  bind_rows() %>%
  dplyr::filter(status.codedGameState == "F",
                !is.na(game_pk)) %>%
  pull(game_pk)

plan("multisession", workers = 4)

safe_pbp <- safely(mlb_pbp)

milb_pbp <- 1:length(ml_game_pks) %>%
  furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
  purrr::map("result") %>%
  bind_rows()

milb_pbp <- milb_pbp %>% as.data.frame()


pitches2 <- milb_pbp %>%
  group_by(matchup.pitcher.id, matchup.pitcher.fullName, details.type.description) %>%
  summarise(mean_velo = mean(pitchData.startSpeed),
            mean_ivb = mean(pitchData.breaks.breakVerticalInduced),
            mean_hb = mean(pitchData.breaks.breakHorizontal),
            mean_sr = mean(pitchData.breaks.spinRate)) %>%
  drop_na()

new_filtered <- pitches2 %>% 
  semi_join(pitches, by = "matchup.pitcher.id") %>%
  anti_join(pitches, by = c("matchup.pitcher.id", "details.type.description"))

