library(tidyverse)

milb_pbp_2022 <- read_csv("milb_pbp_2022.csv")

df2 <- milb_pbp_2022 %>%
  filter(home_level_name == "High-A") %>%
  filter(isPitch == TRUE)

df1 <- df2 %>%
  group_by(game_pk) %>%
  arrange(atBatIndex, .by_group = TRUE) %>%
  mutate(ld = lead(matchup.batter.id, 1),
         last_pitch_ab = if_else(is.na(ld) == TRUE, 1, 
                                 if_else(matchup.batter.id != ld, 1, 0))) %>%
  filter(last_pitch_ab == 1) %>%
  ungroup()

events <- c("single","field_out","strikeout","double","hit_by_pitch","home_run","double_play","walk",
            "triple","force_out","sac_fly","field_error","grounded_into_double_play","fielders_choice_out","other_out",
            "fielders_choice","sac_bunt","sac_fly_double_play","strikeout_double_play","triple_play")

df2 <- df1 %>%
  filter(result.eventType %in% events) %>%
  mutate(strikeout = if_else(result.eventType == "strikeout" | result.eventType == "strikeout_double_play", 1, 0),
         walk = if_else(result.eventType == "walk", 1, 0),
         single = if_else(result.eventType == "single", 1, 0),
         double = if_else(result.eventType == "double", 1, 0),
         triple = if_else(result.eventType == "triple", 1, 0),
         home_run = if_else(result.eventType == "home_run", 1, 0),
         adj_name = paste0(matchup.batter.fullName, matchup.batter.id))

df3 <- df2[,c(2,1,46,166:173)]

df4 <- df3 %>%
  arrange(game_date, game_pk, adj_name, atBatIndex) %>%
  group_by(game_date, game_pk,adj_name) %>%
  summarise(pas = sum(last_pitch_ab),
            walks = sum(walk))

df4 <- df4 %>%
  group_by(adj_name) %>%
  mutate(name_count = n()) %>%
  ungroup() %>%
  filter(name_count > 1) %>%
  select(-name_count) 

ewma <- function(v, alpha){
  pred <- vector(length = length(v))
  pred[1] <- .1
  for (i in 2:length(v)){
    pred[i] <- v[i-1]*alpha + pred[i-1]*(1-alpha)
  }
  return(pred)
}

library(purrr)

dfroll <- df4 %>%
  group_by(adj_name) %>%
  mutate(roll_walk = cumsum(walks),
         roll_pa = cumsum(pas), 
         bb_rate = roll_walk / roll_pa)

# Function to calculate RMSE for a given alpha
calc_rmse <- function(alpha) {
  rmse <- dfroll %>%
    group_by(adj_name) %>%
    summarize(rmse = sqrt(mean((bb_rate - ewma(bb_rate, alpha))^2))) %>%
    summarize(total_rmse = mean(rmse)) %>%
    pull(total_rmse)
  return(rmse)
}

# Find optimal alpha by minimizing RMSE
opt_alpha <- optimize(calc_rmse, c(0, 1), tol = 1e-6)$minimum

opt_alpha

dfroll1 <- dfroll %>%
  group_by(adj_name) %>%
  mutate(ewma_bbrate = ewma(bb_rate, opt_alpha))

can <- dfroll1 %>%
  filter(adj_name == "Alexander Canario672744")

plot(can$game_date, can$bb_rate, type="l") # Plotting a line
lines(can$game_date, can$ewma_bbrate, col="red") # Adding another line with red color

