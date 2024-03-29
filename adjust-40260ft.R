library(baseballr)
library(tidyverse)
library(furrr)
library(readr)

leagues <- mlb_league(2022) 
dates <- data.frame(day = rep(seq(as.Date("2022-04-07"),as.Date("2022-10-05"), by = "days"), times = 1))
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

#readr::write_csv(ml_pbp, "mlb2022-40ft.csv")
ml_pbp <- readr::read_csv("mlb2022-40ft.csv")

just_pitches <- ml_pbp %>% filter(isPitch == TRUE)

just2 <- just_pitches %>% 
  select(game_date, matchup.pitcher.id, matchup.batter.id, details.type.code, pitchData.startSpeed, about.atBatIndex, pitchNumber, pitchData.coordinates.pfxX, pitchData.coordinates.pfxZ) %>%
  mutate(atBatIndex2 = about.atBatIndex + 1,
         concat = paste0(game_date, matchup.pitcher.id, matchup.batter.id, details.type.code, pitchData.startSpeed, atBatIndex2	, pitchNumber))

statcast22 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast22.csv")

statcast2 <- statcast22 %>%
  select(game_date, pitcher, batter, pitch_type, release_speed, at_bat_number, pitch_number, pfx_x, pfx_z) %>%
  mutate(concat = paste0(game_date, pitcher, batter, pitch_type, release_speed, at_bat_number, pitch_number))

master <- inner_join(statcast2, just2, by = "concat") %>%
  drop_na()

master2 <- master %>%
  mutate(adj_x = pfx_x*12,
         adj_z = pfx_z*12) %>%
  select(adj_x, adj_z, pitchData.coordinates.pfxX, pitchData.coordinates.pfxZ,release_speed)

mod_x <- lm(adj_x ~ pitchData.coordinates.pfxX , data = master2)
summary(mod_x)

mod_z <- lm(adj_z ~ pitchData.coordinates.pfxZ , data = master2)
summary(mod_z)
