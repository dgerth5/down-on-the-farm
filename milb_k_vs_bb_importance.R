library(tidyverse)
library(relaimpo)
library(purrr)
library(readr)

get_minor_lg_stats <- function(season){
  df <- data.frame()
  for (i in seq(11,14,1)){
    r <- baseballr::mlb_stats(
      stat_type = "season",
      sport_id = i,
      season = season,
      stat_group = "pitching",
      player_pool = "All",
      limit = 10000) %>%
      mutate(k_per = strike_outs / at_bats, 
             bb_per = base_on_balls / at_bats,
             babip = (hits - home_runs) / (at_bats + sac_flies - strike_outs - home_runs)) %>%
      dplyr::select(season, sport_id, player_id, player_full_name, innings_pitched, era, strike_outs, k_per, bb_per, babip)
    df <- rbind(df, r)
  }
  return(df)
}

milb_df <- map_dfr(2010:2019, ~ get_minor_lg_stats(season = .x))

career <- read_csv("C:/Users/david/Downloads/fangraphs-leaderboards (29).csv")
c2 <- career %>%
  dplyr::select(MLBAMID, Name, ERA)

d1 <- left_join(milb_df, c2, by = c("player_id"="MLBAMID"))

d2 <- d1 %>%
  filter(sport_id == 11) %>%
  filter(innings_pitched > 40) %>%
  mutate(is_mlb = if_else(is.na(ERA)==TRUE, FALSE, TRUE)) %>%
  drop_na(is_mlb, k_per, bb_per)

probit <- glm(is_mlb ~ k_per + bb_per,
              data = d2,
              family = binomial(link = 'probit'))

summary(probit)

probit_lp = predict(probit)
d2$mills0 = dnorm(probit_lp)/pnorm(probit_lp)

d3 <- d2 %>%
  filter(is_mlb == TRUE)

lm_select <- lm(ERA ~ k_per + bb_per, data = d3)
summary(lm_select)

calc.relimp(lm_select, rela=TRUE)

