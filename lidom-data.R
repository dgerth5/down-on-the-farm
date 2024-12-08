library(baseballr)
library(tidyverse)
library(furrr)
library(readr)

leagues <- mlb_league(2023) 
dates <- data.frame(day = rep(seq(as.Date("2023-12-01"),Sys.Date(), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map(function(x) mlb_game_pks(dates$day[x],
                                      level_ids = 17))

?mlb_game_pks

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

unique(ml_pbp$home_league_name)

ml_pbp <- ml_pbp %>% as.data.frame()

ml_pbp2 <- ml_pbp %>%
  filter(home_league_name == "Liga de Beisbol Dominicano")

ml2 <- ml_pbp %>%
  drop_na(pitchData.startSpeed) %>%
  group_by(matchup.pitcher.fullName, details.type.description) %>%
  summarise(Velo = mean(pitchData.startSpeed),
            H_Mov = mean(pitchData.breaks.breakHorizontal),
            V_Mov = mean(pitchData.breaks.breakVerticalInduced),
            SpinRate = mean(pitchData.breaks.spinRate),
            Pitches = n())

unique(ml2$home_league_name)
sort(unique(ml2$home_team))
sort(unique(ml_pbp2$home_team))
