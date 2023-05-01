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

plan("multisession", workers = 4)

safe_pbp <- safely(mlb_pbp)

ml_pbp <- 1:length(ml_game_pks) %>%
  furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
  purrr::map("result") %>%
  bind_rows()

library(readr)

write_csv(ml_pbp, "milb2023-4-30.csv")

ml_pbp <- read_csv("milb2023-4-30.csv")


# get season stats



# get batter team name 
# these filter out the annoying things that end an event but not a plate appearance
bad_events <- c("Pickoff Caught Stealing 2B","Runner Out","Caught Stealing 3B","Catcher Interference",
                "Wild Pitch","Pickoff 1B","Caught Stealing 2B","Pickoff Caught Stealing 3B",
                "Caught Stealing Home","Pickoff 3B","Stolen Base 3B","Pickoff Caught Stealing Home",
                "Pickoff 2B")

finished_ab <- ml_pbp %>%
  filter(isPitch == TRUE) %>%
  group_by(game_pk) %>%
  arrange(atBatIndex, pitchNumber, .by_group = TRUE) %>%
  mutate(ld = lead(matchup.batter.id, 1),
         last_pitch_ab = if_else(is.na(ld) == TRUE, 1, 
                                 if_else(matchup.batter.id != ld, 1, 0)))  %>%
  filter(last_pitch_ab == 1) %>%
  filter(!result.event %in% bad_events) %>%
  ungroup()

# add batter team name
agg_sts <- finished_ab %>%
  mutate(strikeout = if_else(result.eventType == "strikeout" | result.eventType == "strikeout_double_play", 1, 0),
         walk = if_else(result.eventType == "walk", 1, 0),
         home_run = if_else(result.eventType == "home_run", 1, 0),
         adj_name = paste0(matchup.batter.fullName, matchup.batter.id)) %>%
  group_by(adj_name,home_level_name, batting_team) %>%
  summarise(pas = sum(last_pitch_ab),
            tot_bbs = sum(walk),
            tot_ks = sum(strikeout),
            tot_hrs = sum(home_run))


# apply stabilization method
# copying from post: https://downonthefarm.substack.com/p/estimating-minor-league-stabilization

pad_numbers <- data.frame(lvl = c("A", "A+", "AA", "AAA"),
                          k = c(72,66,69,66),
                          bb = c(133,179,174,174),
                          hr = c(240,165,218,220))

