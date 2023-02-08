library(readr)
library(tidyverse)

triple_a_pbp <- read_csv("triple-a-pbp.csv")
hd <- triple_a_pbp[1:3,] # just to get an idea of the column names


hit_situations <- c("In play, out(s)", "In play, run(s)", "In play, no out")

just_hit_sits <- triple_a_pbp %>%
  filter(isPitch == TRUE) %>%
  filter(details.call.description %in% hit_situations) %>%
  mutate(single = if_else(result.eventType == "single", 1, 0),
         double = if_else(result.eventType == "double", 1, 0),
         triple = if_else(result.eventType == "triple", 1, 0),
         hr = if_else(result.eventType == "home_run", 1, 0),
         batter_name = paste0(matchup.batter.fullName, matchup.batter.id),
         pitcher_name = paste0(matchup.pitcher.fullName, matchup.pitcher.id))

just_hit_sits$batter_name <- as.factor(just_hit_sits$batter_name)
just_hit_sits$pitcher_name <- as.factor(just_hit_sits$pitcher_name)
just_hit_sits$home_team <- as.factor(just_hit_sits$home_team)

library(lme4)

single_mod <- glmer(single ~ 1 + (1|batter_name) + (1|pitcher_name) + (1|home_team), data = just_hit_sits, family = binomial())
double_mod <- glmer(double ~ 1 + (1|batter_name) + (1|pitcher_name) + (1|home_team), data = just_hit_sits, family = binomial())
triple_mod <- glmer(triple ~ 1 + (1|batter_name) + (1|pitcher_name) + (1|home_team), data = just_hit_sits, family = binomial()) # didnt converge 
homer_mod <- glmer(hr ~ 1 + (1|batter_name) + (1|pitcher_name) + (1|home_team), data = just_hit_sits, family = binomial()) # assuming this wont converge

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


m <- logit2prob(coef(single_mod)$home_team)


single_stad_re$index = logit2prob(single_stad_re$value)
single_stad_re$index2 = ((single_stad_re$index - mean(single_stad_re$index)) / mean(single_stad_re$index) + 1)*100

singles <- just_hit_sits %>%
  group_by(home_team) %>%
  summarise(singles = sum(single))

singles$per = ((singles$singles - mean(singles$singles)) / mean(singles$singles) + 1)*100


double_re <- extract_random_effects(double_mod)
triple_re <- extract_random_effects(triple_mod)
homer_re <- extract_random_effects(homer_mod)