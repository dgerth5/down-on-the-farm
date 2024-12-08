library(baseballr)
library(tidyverse)
library(furrr)
library(readr)

leagues <- mlb_league(2023) 
dates <- data.frame(day = rep(seq(as.Date("2023-04-06"),Sys.Date(), by = "days"), times = 1))
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

df <- ml_pbp %>%
  drop_na(hitData.launchSpeed, hitData.launchAngle)

ev <- df %>%
  group_by(hitData.launchAngle) %>%
  summarise(ev80 = quantile(hitData.launchSpeed,0.8))

df2 <- left_join(df, ev, by = "hitData.launchAngle")

df3 <- df2 %>%
  mutate(dhh = if_else(hitData.launchSpeed >= ev80, 1, 0),
         good_la = if_else(hitData.trajectory == "fly_ball" | hitData.trajectory == "line_drive", 1, 0)) %>%
  group_by(matchup.batter.fullName) %>%
  summarise(dhh = mean(dhh),
            good_la = mean(good_la),
            n = n()) %>%
  filter(n > 50) %>%
  mutate(dhh_perc = percent_rank(dhh),
         good_la_perc = percent_rank(good_la)) %>%
  filter(dhh_perc > .5 & good_la_perc > .5)
