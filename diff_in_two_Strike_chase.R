library(baseballr)
library(tidyverse)
library(furrr)
library(readr)
library(gt)

leagues <- mlb_league(2024) 
dates <- data.frame(day = rep(seq(as.Date("2024-04-05"),Sys.Date(), by = "days"), times = 1))
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

milb_pbp <- 1:length(ml_game_pks) %>%
  furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
  purrr::map("result") %>%
  bind_rows()

milb_pbp <- milb_pbp %>% as.data.frame()


se_milb <- c("In play, out(s)","Swinging Strike","Foul","In play, run(s)","In play, no out",
             "Foul Tip","Foul Bunt","Swinging Strike (Blocked)","Foul Pitchout", "Missed Bunt")

we_milb <- c("Swinging Strike","Swinging Strike (Blocked)", "Missed Bunt")


fsl <- milb_pbp %>%
  filter(home_league_name == "Florida State League") %>%
  filter(type == "pitch") %>%
  mutate(se = if_else(details.description %in% se_milb, 1, 0),
         chase = if_else(pitchData.zone	> 9, 1, 0),
         two_strikes = if_else(count.strikes.start == 2, 1, 0),
         bat_team = if_else(batting_team == home_team, home_parentOrg_name, away_parentOrg_name)) %>%
  filter(chase == 1) %>%
  group_by(bat_team, two_strikes) %>%
  summarise(chase_rate = mean(se)) %>%
  ungroup()

df <- fsl %>%
  filter(two_strikes == 1) %>%
  left_join(fsl %>% filter(two_strikes == 0), by = "bat_team") %>%
  mutate(Diff = chase_rate.x - chase_rate.y) %>%
  select(bat_team, chase_rate.x, chase_rate.y, Diff) %>%
  rename("Org" = bat_team,
         "TS_Chase" = chase_rate.x,
         "NTS_Chase" = chase_rate.y)

df %>%
  arrange(-Diff) %>%
  gt() %>%
  tab_header(md("**Differences in Chase Rate by Strikes**"),
             md("FSL 2024")) %>%
  fmt_percent(c("TS_Chase","NTS_Chase","Diff"), decimals = 1) %>%
  cols_label(TS_Chase = "Two Strike Chase%",
             NTS_Chase = "Non Two Strike Chase%")

gtsave("approach_fsl.png")  
