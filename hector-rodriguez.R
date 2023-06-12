library(baseballr)
library(tidyverse)
library(furrr)
library(readr)

leagues <- mlb_league(2023) 
dates <- data.frame(day = rep(seq(as.Date("2023-04-07"),Sys.Date(), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map(function(x) mlb_game_pks(dates$day[x],
                                      level_ids = 14))

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

ev <- ml_pbp %>%
  drop_na(hitData.launchSpeed)

ev_smry <- ev %>%
  group_by(matchup.batter.fullName, batting_team) %>%
  summarise(ev = quantile(hitData.launchSpeed,0.95)) %>%
  ungroup() %>%
  mutate(perc_ev = percent_rank(ev) * 100)

la <- ml_pbp %>%
  drop_na(hitData.launchAngle)

tightness <- la %>%
  group_by(matchup.batter.fullName, batting_team) %>%
  summarise(sd_la = sd(hitData.launchAngle),
            pa = n()) %>%
  ungroup() %>%
  mutate(perc_la = percent_rank(sd_la) * 100)

batter <- left_join(tightness, ev_smry, by = "matchup.batter.fullName")

whiff_event <- c("Swinging Strike","Swinging Strike (Blocked)", "Missed Bunt")

swing_event <- c("In play, out(s)","Swinging Strike","In play, run(s)","In play, no out",
                 "Foul Tip","Foul Bunt","Swinging Strike (Blocked)", "Missed Bunt")

whiff <- ml_pbp %>%
  mutate(swing = if_else(details.call.description %in% swing_event, 1, 0),
         whiff = if_else(details.call.description %in% whiff_event, 1, 0)) %>%
  filter(type == "pitch") %>%
  filter(pitchData.zone > 9) %>%
  group_by(matchup.batter.fullName) %>%
  summarise(mean_swing = mean(swing),
            mean_whiff = mean(whiff)) %>%
  ungroup() %>%
  mutate(perc_sw = percent_rank(mean_swing) * 100,
         perc_whiff = percent_rank(mean_whiff) * 100)

  
