library(readr)
library(tidyverse)

triple_a_pbp <- read_csv("triple-a-pbp.csv")

#### single ####

single_events <- c("single", "force_out","field_out","grounded_into_double_play","sac_fly",
                   "fielders_choice_out","sac_fly_double_play","sac_bunt_double_play")

single_df <- triple_a_pbp %>%
  filter(last.pitch.of.ab == TRUE) %>%
  filter(result.eventType %in% single_events) %>%
  mutate(single = if_else(result.eventType == "single", 1, 0),
         batter_name = paste0(matchup.batter.fullName, matchup.batter.id),
         pitcher_name = paste0(matchup.pitcher.fullName, matchup.pitcher.id)) %>%
  filter(home_league_name == "International League")

single_df$batter_name <- as.factor(single_df$batter_name)
single_df$pitcher_name <- as.factor(single_df$pitcher_name)
single_df$home_team <- as.factor(single_df$home_team)

library(lme4)

single_mod <- glmer(single ~ 1 + (1|home_team) + (1|batter_name),
                    data = single_df, 
                    family = binomial(),
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=2e5)))
                    
library(mixedup)

single_re <- extract_random_effects(single_mod) 
single_stad_re <- single_re %>%
  filter(group_var == "home_team")
summary(single_mod)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

single_stad_re$index = logit2prob(single_stad_re$value)
single_stad_re$index2 = ((single_stad_re$index - mean(single_stad_re$index)) / mean(single_stad_re$index) + 1)*100


#### doubles ####

double_events <- c("double", "force_out","field_out","grounded_into_double_play","sac_fly",
                   "fielders_choice_out","sac_fly_double_play","sac_bunt_double_play")

double_df <- triple_a_pbp %>%
  filter(last.pitch.of.ab == TRUE) %>%
  filter(result.eventType %in% double_events) %>%
  mutate(double = if_else(result.eventType == "double", 1, 0),
         batter_name = paste0(matchup.batter.fullName, matchup.batter.id),
         pitcher_name = paste0(matchup.pitcher.fullName, matchup.pitcher.id)) %>%
  filter(home_league_name == "International League")

double_df$batter_name <- as.factor(double_df$batter_name)
double_df$pitcher_name <- as.factor(double_df$pitcher_name)
double_df$home_team <- as.factor(double_df$home_team)


double_mod <- glmer(double ~ 1 + (1|home_team) + (1|batter_name),
                    data = double_df, 
                    family = binomial(),
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=2e5)))

double_re <- extract_random_effects(double_mod) 
double_stad_re <- double_re %>%
  filter(group_var == "home_team")

double_stad_re$index = logit2prob(double_stad_re$value)
double_stad_re$index2 = ((double_stad_re$index - mean(double_stad_re$index)) / mean(double_stad_re$index) + 1)*100


#### triple ####

triple_events <- c("triple", "force_out","field_out","grounded_into_triple_play","sac_fly",
                   "fielders_choice_out","sac_fly_triple_play","sac_bunt_triple_play")

triple_df <- triple_a_pbp %>%
  filter(last.pitch.of.ab == TRUE) %>%
  filter(result.eventType %in% triple_events) %>%
  mutate(triple = if_else(result.eventType == "triple", 1, 0),
         batter_name = paste0(matchup.batter.fullName, matchup.batter.id),
         pitcher_name = paste0(matchup.pitcher.fullName, matchup.pitcher.id)) %>%
  filter(home_league_name == "International League")

triple_df$batter_name <- as.factor(triple_df$batter_name)
triple_df$pitcher_name <- as.factor(triple_df$pitcher_name)
triple_df$home_team <- as.factor(triple_df$home_team)

triple_mod <- glmer(triple ~ 1 + (1|home_team) + (1|batter_name),
                    data = triple_df, 
                    family = binomial(),
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=2e5)))

triple_re <- extract_random_effects(triple_mod) 
triple_stad_re <- triple_re %>%
  filter(group_var == "home_team")

triple_stad_re$index = logit2prob(triple_stad_re$value)
triple_stad_re$index2 = ((triple_stad_re$index - mean(triple_stad_re$index)) / mean(triple_stad_re$index) + 1)*100


#### home run ####


homer_events <- c("home_run", "force_out","field_out","grounded_into_homer_play","sac_fly",
                   "fielders_choice_out","sac_fly_homer_play","sac_bunt_homer_play")

homer_df <- triple_a_pbp %>%
  filter(last.pitch.of.ab == TRUE) %>%
  filter(result.eventType %in% homer_events) %>%
  mutate(homer = if_else(result.eventType == "home_run", 1, 0),
         batter_name = paste0(matchup.batter.fullName, matchup.batter.id),
         pitcher_name = paste0(matchup.pitcher.fullName, matchup.pitcher.id)) %>%
  filter(home_league_name == "International League")

homer_df$batter_name <- as.factor(homer_df$batter_name)
homer_df$pitcher_name <- as.factor(homer_df$pitcher_name)
homer_df$home_team <- as.factor(homer_df$home_team)

homer_mod <- glmer(homer ~ 1 + (1|home_team) + (1|batter_name),
                    data = homer_df, 
                    family = binomial(),
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=2e5)))

homer_re <- extract_random_effects(homer_mod) 
homer_stad_re <- homer_re %>%
  filter(group_var == "home_team")

homer_stad_re$index = logit2prob(homer_stad_re$value)
homer_stad_re$index2 = ((homer_stad_re$index - mean(homer_stad_re$index)) / mean(homer_stad_re$index) + 1)*100

# add types

single_stad_re$type = "single"
double_stad_re$type = "double"
triple_stad_re$type = "triple"
homer_stad_re$type = "homer"

int_league_pf = rbind(single_stad_re, double_stad_re, triple_stad_re, homer_stad_re)
