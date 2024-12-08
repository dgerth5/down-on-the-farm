library(baseballr)
library(tidyverse)
library(mixedup)
library(lme4)

leagues = mlb_league(2023) %>% select(league_name, sport_id, season_date_info_season_start_date, season_date_info_season_end_date)

get_games = function(league_id, start, end){
  
  dates = data.frame(day = rep(seq(as.Date(start),as.Date(end), by = "days"), times = 1))
  
  gmes = data.frame()
  
  for (i in 1:length(dates$day)){
    
    gm = get_game_pks_mlb(date = dates$day[i], level_ids = league_id)
    
    gmes = rbind(gmes, gm, fill = TRUE)
    
  }
  
  return(gmes)
}

triple_a_gms = get_games(11, "2023-03-31", "2023-09-30")
double_a_gms = get_games(12, "2023-04-06", "2023-09-27")
high_a_gms = get_games(13, "2023-04-06", "2023-09-20")
single_a_gms = get_games(14, "2023-04-06", "2023-09-20")

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
  select(group, park_factor, park_factor_mult) %>%
  rename(team = group)

write_csv(runs_park_effect, "runs_park_effect23.csv")


df9 <- read_csv("runs_park_effect23.csv")


# run
top_r <- df9 %>%
  mutate(league_lvl = paste0(tools::toTitleCase(league), " (", toupper(level), ")")) %>%
  select(team, league_lvl, park_factor) %>%
  arrange(-park_factor) %>%
  head(5)

bot_r <- df9 %>%
  mutate(league_lvl = paste0(tools::toTitleCase(league), " (", toupper(level), ")")) %>%
  select(team, league_lvl, park_factor) %>%
  arrange(park_factor) %>%
  head(5) 

colnames(bot_r) <- paste0(colnames(bot_r),"2")

cb <- cbind(top_r, bot_r)

r_pk <- cb %>%
  gt() %>%
  tab_header(title = md("**Top and Bottom 5 Run Parks**"),
             subtitle =  md("Season: 2023")) %>%
  gt_add_divider("park_factor") %>%
  fmt_number(c("park_factor", "park_factor2"), decimals = 0) %>%
  cols_label(team = "Team",
             league_lvl = "League (Level)",
             park_factor = "PF",
             team2 = "Team",
             league_lvl2 = "League (Level)",
             park_factor2 = "PF")

gtsave(r_pk, "r_pk23.png")
