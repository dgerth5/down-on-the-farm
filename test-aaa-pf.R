library(baseballr)
library(tidyverse)
library(furrr)
library(readr)
library(lme4)
library(mixedup)

leagues <- mlb_league(2022) %>% select(league_name, sport_id)
dates <- data.frame(day = rep(seq(as.Date("2022-04-08"),as.Date("2022-10-18"), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map(function(x) mlb_game_pks(dates$day[x],
                                      level_ids = c(11,12,13,14)))

ml_game_pks <- minor_league_game_pk_lst %>%
  bind_rows() %>%
  dplyr::filter(status.codedGameState == "F",
                !is.na(game_pk)) %>%
  pull(game_pk)

plan("multisession", workers = 3)

safe_pbp <- safely(mlb_pbp)

ml_pbp <- 1:length(ml_game_pks) %>%
  furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
  purrr::map("result") %>%
  bind_rows()

ml_pbp <- ml_pbp %>% as.data.frame()

write_csv(ml_pbp, file = "milb_pbp_2022.csv")

unique(ml_pbp$home_level_name)

triple_a <- ml_pbp %>%
  filter(home_level_name == "Triple-A")

write_csv(triple_a, file = "triple_a_pbp2022.csv")

double_a <- ml_pbp %>%
  filter(home_level_name == "Double-A")

write_csv(double_a, file = "double_a_pbp2022.csv")

high_a <- ml_pbp %>%
  filter(home_level_name == "High-A")

write_csv(double_a, file = "high_a_pbp2022.csv")

single_a <- ml_pbp %>%
  filter(home_level_name == "Single-A")

write_csv(double_a, file = "single_a_pbp2022.csv")


events <- list(single=c("single"), double=c("double"), triple=c("triple"), home_run = c("home_run"),
               out=c("force_out","field_out","grounded_into_double_play","grounded_into_triple_play","sac_fly",
                     "fielders_choice_out","sac_fly_double_play","sac_fly_triple_play","sac_bunt_double_play","sac_bunt_triple_play"))

calculate_re <- function(df, event_name){
  
  in_play_desc = c("In play, out(s)", "In play, no out", "In play, run(s)")
  
  event_df <- df %>%
    filter(details.description %in% in_play_desc) %>%
    filter(result.eventType %in% c(events[[event_name]],events[["out"]])) %>%
    mutate(event = if_else(result.eventType == events[[event_name]], 1, 0),
           batter_name = paste0(matchup.batter.fullName, matchup.batter.id),
           pitcher_name = paste0(matchup.pitcher.fullName, matchup.pitcher.id)) %>%
    drop_na(home_team, batter_name)
  
  event_df$batter_name <- as.factor(event_df$batter_name)
  event_df$pitcher_name <- as.factor(event_df$pitcher_name)
  event_df$home_team <- as.factor(event_df$home_team)
  
  event_mod <- glmer(event ~ 1 + (1|home_team) + (1|batter_name),
                     data = event_df, 
                     family = binomial(),
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl=list(maxfun=2e5)))
  
  event_re <- extract_random_effects(event_mod) 
  event_stad_re <- event_re %>%
    filter(group_var == "home_team")
  
  logit2prob <- function(logit){
    odds <- exp(logit)
    prob <- odds / (1 + odds)
    return(prob)
  }
  
  event_stad_re$index = logit2prob(event_stad_re$value)
  event_stad_re$index2 = ((event_stad_re$index - mean(event_stad_re$index)) / mean(event_stad_re$index) + 1)*100
  
  return(event_stad_re)
}

single_re = calculate_re(ml_pbp, "single")
single_re = single_re %>% rename(single_pf = index2)
single_re$type = "single"
double_re = calculate_re(ml_pbp, "double")
double_re = double_re %>% rename(double_pf = index2)
double_re$type = "double"
triple_re = calculate_re(ml_pbp, "triple")
triple_re = triple_re %>% rename(triple_pf = index2)
triple_re$type = "triple"
homer_re = calculate_re(ml_pbp, "home_run")
homer_re = homer_re %>% rename(home_run_pf = index2)
homer_re$type = "home_run"

milb_park_effects = cbind(single_re[,c(3,9)], double_re[,c(3,9)], triple_re[,c(3,9)], homer_re[,c(3,9)])[,-c(3,5,7)] %>%
  mutate(single_mult = (single_pf / 100 - 1) / 2 + 1,
         double_mult = (double_pf / 100 - 1) / 2 + 1,
         triple_mult = (triple_pf / 100 - 1) / 2 + 1,
         home_run_mult = (home_run_pf / 100 - 1) / 2 + 1,
         year = 2022) %>%
  select(group, year, single_pf, single_mult, double_pf, double_mult, triple_pf, triple_mult, home_run_pf, home_run_mult) %>%
  rename(team = group)

write_csv(milb_park_effects, "milb_park_effects22.csv")
