library(tidyverse)
library(purrr)
library(relaimpo)
library(gt)

leagues <- baseballr::mlb_league(2024) %>% dplyr::select(league_name, league_abbreviation, sport_id)

get_rel_imp <- function(sport_id){
  
  df <- map_df(c(2021,2022,2023), ~baseballr::mlb_stats(
    stat_type = "season",
    sport_id = sport_id,
    season = .x,
    stat_group = "pitching",
    player_pool = "All",
    limit = 10000)) %>%
    dplyr::select(league_name, team_name, player_full_name, batters_faced, base_on_balls, strike_outs, hit_by_pitch, home_runs, era) %>%
    mutate_at(c("strike_outs","base_on_balls","hit_by_pitch","home_runs","batters_faced"), as.numeric) %>%
    mutate(k_inn = strike_outs / batters_faced,
           bb_inn = base_on_balls / batters_faced,
           hbp_inn = hit_by_pitch / batters_faced,
           hr_inn = home_runs / batters_faced) %>%
    filter(batters_faced > 50)
  
  df$era <- as.numeric(df$era)
  
  mod <- lm(era ~ k_inn + bb_inn + hbp_inn + hr_inn, data = df)
  
  rw <- c(calc.relimp(mod, rela=TRUE)$lmg, summary(mod)$r.squared)
  
  return(rw)
  
}


get_rel_imp2 <- function(sport_id, lm){
  
  df <- map_df(c(2021,2022,2023), ~baseballr::mlb_stats(
    stat_type = "season",
    sport_id = sport_id,
    season = .x,
    stat_group = "pitching",
    player_pool = "All",
    limit = 10000)) %>%
    dplyr::select(league_name, team_name, player_full_name, batters_faced, base_on_balls, strike_outs, hit_by_pitch, home_runs, era) %>%
    filter(league_name == lm) %>%
    mutate_at(c("strike_outs","base_on_balls","hit_by_pitch","home_runs","batters_faced"), as.numeric) %>%
    mutate(k_inn = strike_outs / batters_faced,
           bb_inn = base_on_balls / batters_faced,
           hbp_inn = hit_by_pitch / batters_faced,
           hr_inn = home_runs / batters_faced) %>%
    filter(batters_faced > 50)
  
  df$era <- as.numeric(df$era)
  
  mod <- lm(era ~ k_inn + bb_inn + hbp_inn + hr_inn, data = df)
  
  rw <- c(calc.relimp(mod, rela=TRUE)$lmg, summary(mod)$r.squared)
  
  return(rw)
  
}



smry <- as.data.frame(rbind(get_rel_imp(1), 
              get_rel_imp(11), 
              get_rel_imp(12),
              get_rel_imp(13),
              get_rel_imp(14),
              get_rel_imp(16),
              get_rel_imp2(23, "MEX")))

smry$level <- c("MLB","AAA","AA","A+","A","R","MEX")

smry %>%
  dplyr::select(level, everything()) %>%
  gt() %>%
  tab_header(title = md("**Relative Importance of FIP Factors By Level**"),
             subtitle = md("Season: 2021-2023")) %>%
  fmt_percent(decimals = 0) %>%
  cols_label(level = "Level",
             k_inn = "K%",
             bb_inn = "BB%",
             hbp_inn = "HBP%",
             hr_inn = "HR%",
             `V5` = "R^2")

m <- lm(k_inn ~ bb_inn, data = smry)
summary(m)$r.squared
