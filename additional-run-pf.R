library(baseballr)
library(tidyverse)
library(mixedup)
library(lme4)

leagues = mlb_league(2018) %>% select(league_name, sport_id, season_date_info_season_start_date, season_date_info_season_end_date)

get_games = function(league_id, start, end){
  
  dates = data.frame(day = rep(seq(as.Date(start),as.Date(end), by = "days"), times = 1))
  
  gmes = data.frame()
  
  for (i in 1:length(dates$day)){
    
    gm = get_game_pks_mlb(date = dates$day[i], level_ids = league_id)
    
    gmes = rbind(gmes, gm, fill = TRUE)
    
  }
  
  return(gmes)
}

triple_a_gms = get_games(11, "2018-04-05", "2018-09-18")
double_a_gms = get_games(12, "2018-04-05", "2018-09-18")
high_a_gms = get_games(13, "2018-04-05", "2018-09-18")
single_a_gms = get_games(14, "2018-04-05", "2018-09-18")

all_gms = rbind(triple_a_gms, double_a_gms, high_a_gms, single_a_gms)

edit_gms = all_gms %>%
  filter(gameType != TRUE & gameType == "R") %>%
  mutate(tot_runs = teams.home.score + teams.away.score)

edit_gms$teams.home.team.name = as.factor(edit_gms$teams.home.team.name)
edit_gms$teams.away.team.name = as.factor(edit_gms$teams.away.team.name)

run_mod = lmer(tot_runs ~ 1 + (1|teams.home.team.name) + (1|teams.away.team.name), 
               data = edit_gms)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

runs_park_effect = extract_random_coefficients(run_mod) %>%
  filter(group_var == "teams.home.team.name") %>%
  mutate(park_factor = value / mean(value)*100,
         park_factor_mult = (park_factor / 100 - 1) / 2 + 1) %>%
#  select(group, park_factor, park_factor_mult) %>%
  rename(team = group)

write_csv(runs_park_effect, "runs_park_effect18.csv")