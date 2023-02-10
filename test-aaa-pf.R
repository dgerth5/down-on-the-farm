library(readr)
library(tidyverse)

triple_a_pbp <- read_csv("triple-a-pbp.csv")

events <- list(single=c("single"), double=c("double"), triple=c("triple"), home_run = c("home_run"),
               out=c("force_out","field_out","grounded_into_double_play","grounded_into_triple_play","sac_fly",
                     "fielders_choice_out","sac_fly_double_play","sac_fly_triple_play","sac_bunt_double_play","sac_bunt_triple_play"))

calculate_re <- function(df, event_name){
  event_df <- triple_a_pbp %>%
    filter(last.pitch.of.ab == TRUE) %>%
    filter(result.eventType %in% c(events[[event_name]],events[["out"]])) %>%
    mutate(event = if_else(result.eventType == events[[event_name]], 1, 0),
           event_name = event_name,
           batter_name = paste0(matchup.batter.fullName, matchup.batter.id),
           pitcher_name = paste0(matchup.pitcher.fullName, matchup.pitcher.id)) %>%
    filter(home_league_name == "International League")
  
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

single_re = calculate_re(triple_a_pbp, "single")
single_re$type = "single"
double_re = calculate_re(triple_a_pbp, "double")
double_re$type = "double"
triple_re = calculate_re(triple_a_pbp, "triple")
triple_re$type = "triple"
homer_re = calculate_re(triple_a_pbp, "home_run")
homer_re$type = "home_run"

intl_park_effects = rbind(single_re, double_re, triple_re, homer_re)
