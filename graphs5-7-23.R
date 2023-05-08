library(tidyverse)
library(baseballr)
library(lubridate)
library(purrr)
library(furrr)

# get data

leagues = mlb_league(2023)
leagues2 = leagues %>%
  select(league_id, sport_id, league_name, season_date_info_regular_season_start_date, season_date_info_regular_season_end_date)

dates <- data.frame(day = rep(seq(as.Date("2023-05-07"),Sys.Date(), by = "days"), times = 1))
minor_league_game_pks_lst <- 1:nrow(dates) %>%
  purrr::map_df(function(x) mlb_game_pks(dates$day[x],
                                         level_ids = c(11,14)))

ml_game_pks <- minor_league_game_pks_lst %>%
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

pbp2 <- ml_pbp %>%
  filter(isPitch == TRUE) %>%
  mutate(adjz = -0.25 + 1.73*pitchData.coordinates.pfxZ,
         adjx = if_else(matchup.pitchHand.code == "R", .41 + 1.69*pitchData.coordinates.pfxX, -1*(.41 + 1.69*pitchData.coordinates.pfxX))) %>%
  drop_na(adjz, adjx) 

vert_mvmt <- pbp2 %>%
  filter(details.type.code %in% c("FF","SI","FC")) %>%
  group_by(matchup.pitcher.fullName, details.type.code) %>%
  summarise(mean_mvt = mean(abs(adjx)),
            thrown = n()) %>%
  ungroup() %>%
  arrange(-mean_mvt) %>%
  slice(1:10) 

fb_velo <- pbp2 %>%
  filter(details.type.code %in% c("FF","SI","FC")) %>%
  group_by(matchup.pitcher.fullName, details.type.code) %>%
  summarise(mean_velo = mean(pitchData.startSpeed),
            thrown = n()) %>%
  ungroup() %>%
  arrange(-mean_velo) %>%
  slice(1:10)

ev_df <- pbp2 %>%
  filter(is.na(hitData.launchSpeed) == FALSE)

max_ev <- ev_df %>%
  group_by(matchup.batter.fullName) %>%
  summarise(max_ev = max(hitData.launchSpeed)) %>%
  ungroup() %>%
  arrange(-max_ev) %>%
  slice(1:10)

library(gt)

gt(vert_mvmt) %>%
  tab_header(title = md("**Fastball Vertical Break Leaderboard - 5/7/23**")) %>%
  fmt_number(mean_mvt, decimals = 1) %>%
  cols_label(matchup.pitcher.fullName = "Pitcher",
             details.type.code = "Fastball Type",
             mean_mvt = "Average IVB",
             thrown = "Pitches Thrown")

gt(fb_velo) %>%
  tab_header(title = md("**Fastball Velocity Leaderboard - 5/7/23**")) %>%
  fmt_number(mean_velo, decimals = 1) %>%
  cols_label(matchup.pitcher.fullName = "Pitcher",
             details.type.code = "Fastball Type",
             mean_velo = "Average Velocity",
             thrown = "Pitches Thrown")

gt(max_ev) %>%
  tab_header(title = md("**Max Exit Velocity Leaderboard - 5/7/23**")) %>%
  fmt_number(max_ev, decimals = 1) %>%
  cols_label(matchup.batter.fullName = "Batter",
             max_ev = "Max Exit Velocity")
