library(baseballr)
library(tidyverse)
library(furrr)
library(readr)

leagues <- mlb_league(2024) %>% select(sport_id, season_date_info_regular_season_start_date,season_date_info_regular_season_end_date)
dates <- data.frame(day = rep(seq(as.Date("2024-03-29"),as.Date("2024-05-22"), by = "days"), times = 1))
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


se_milb <- c("In play, out(s)","Swinging Strike","Foul","In play, run(s)","In play, no out",
             "Foul Tip","Foul Bunt","Swinging Strike (Blocked)","Foul Pitchout", "Missed Bunt")
we_milb <- c("Swinging Strike","Swinging Strike (Blocked)", "Missed Bunt")
abs_days <- c("Tue", "Wed", "Thu")

# shadow zone 

df_milb <- milb_pbp %>%
  mutate(
    horizontal = ifelse(abs(pitchData.coordinates.pX) < 1.33 & abs(pitchData.coordinates.pX) > 0.56, 1, 0),
    upper_vertical = ifelse(pitchData.coordinates.pZ < pitchData.strikeZoneTop	+ 4 & pitchData.coordinates.pZ > pitchData.strikeZoneTop	 - 4, 1, 0),
    lower_vertical = ifelse(pitchData.coordinates.pZ < pitchData.strikeZoneBottom	 + 4 & pitchData.coordinates.pZ > pitchData.strikeZoneBottom - 4, 1, 0),
    shadow_zone = ifelse(horizontal == 1 & (upper_vertical == 1 | lower_vertical == 1), 1, 0),
    se = if_else(details.description %in% se_milb, 1, 0),
    day_of_week = lubridate::wday(game_date, label = TRUE),
    abs_day = if_else(day_of_week %in% abs_days, 1, 0)
  ) %>%
  filter(shadow_zone == 1)

mod <- glm(se ~ abs_day,
           data = df_milb,
           family = "binomial")
summary(mod)


smry <- df_milb %>%
  group_by(matchup.batter.fullName, abs_day) %>%
  summarise(mean_swing = mean(se),
            n = n()) 

abs1 <- smry %>% filter(abs_day == 1) 
abs0 <- smry %>% filter(abs_day == 0)

final <- left_join(abs1, abs0, by = "matchup.batter.fullName")
final2 <- final %>%
  mutate(diff = mean_swing.x - mean_swing.y, 
         tot = n.x + n.y) %>%
  filter(tot > 100)

summary(final2$tot)
