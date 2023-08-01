library(tidyverse)
library(readr)

statcast22 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast22.csv")

swing_events <- c(
  "foul_tip", "swinging_strike", "swinging_strike_blocked", 
  "missed_bunt", "foul", "hit_into_play", "foul_bunt", "bunt_foul_tip"
)

whiff_events <- c(
  "swinging_strike", "foul_tip", "foul_bunt", 
  "missed_bunt", "swinging_strike_blocked"
)

hits <- c("single","double","triple","home_run")

full_mlb <- statcast22 %>%
  # drop any missing rows
  mutate(
    is_swing = if_else(description %in% swing_events, 1, 0), # binary indicator for a swing
    is_whiff = if_else(description %in% whiff_events, 1, 0), # binary indicator for a whiff
    is_in_zone = if_else(zone %in% 1:9, 1, 0), # binary indicator for in-zone
    is_out_zone = if_else(zone > 9, 1, 0), # binary indicator for out-of-zone
    is_chase = if_else(
      is_swing == 1 & is_out_zone == 1, 1, 0
    ), #binary indicator for swing
    is_inplay = if_else(
      description %in% c("hit_into_play"), 1, 0
    ), # binary indicator for contact
    is_hit = if_else(events %in% hits, 1, 0),
    hitting_team = if_else(
      inning_topbot == "Top", away_team, home_team
    ), # column for batting team
    pitching_team = if_else(
      inning_topbot == "Top", home_team, away_team
    ), # column for pitching team
  ) %>%
  drop_na(is_swing, plate_x , plate_z , strikes)

swing_mod <- glm(is_swing ~ plate_x * plate_z + strikes,
                 family = binomial(link = "probit"),
                 data = full_mlb)
summary(swing_mod)
probit_lp1 <- predict(swing_mod)
full_mlb$mills0_1 <- dnorm(probit_lp1) / pnorm(probit_lp1)

step2_1 <- full_mlb %>%
  filter(is_inplay == 1)

hit_mod <- glm(is_hit ~  plate_x * plate_z + pfx_x*pfx_z + release_speed, 
               family = binomial(link = "probit"),
               data = step2_1)

summary(hit_mod)
