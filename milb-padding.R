library(readr)
library(tidyverse)

double_a_pbp2022 <- read_csv("double_a_pbp2022.csv")

filt <- double_a_pbp2022 %>%
  filter(isPitch == TRUE) %>%
  filter(last.pitch.of.ab == TRUE) 

events <- c("single","field_out","strikeout","double","hit_by_pitch","home_run","double_play","walk",
            "triple","force_out","sac_fly","field_error","grounded_into_double_play","fielders_choice_out","other_out",
            "fielders_choice","sac_bunt","sac_fly_double_play","strikeout_double_play","triple_play")

df2 <- filt %>%
  filter(result.eventType %in% events) %>%
  mutate(strikeout = if_else(result.eventType == "strikeout" | result.eventType == "strikeout_double_play", 1, 0),
         walk = if_else(result.eventType == "walk", 1, 0),
         adj_name = paste0(matchup.batter.fullName, matchup.batter.id))

df3 <- df2 %>%
  group_by(game_date, adj_name) %>%
  summarise(sum_walk = sum(walk),
            sum_k = sum(strikeout),
            sum_pa = n())

ct_player_apps <- df3 %>% group_by(adj_name) %>% summarise(ct = n())

df3 <- left_join(df3, ct_player_apps, by = "adj_name")

dfroll <- df3 %>%
  arrange(game_date) %>%
  group_by(adj_name) %>%
  mutate(roll_walk = cumsum(sum_walk),
         roll_k = cumsum(sum_k),
         roll_pa = cumsum(sum_pa))

df_tot <- df2 %>%
  group_by(adj_name) %>%
  summarise(tot_walk = sum(walk),
            tot_k = sum(strikeout),
            tot_pa = n(),
            year = 2022)

df_final <- left_join(dfroll, df_tot, by = "adj_name")

double_a_pbp2021 <- read_csv("double_a_pbp2021.csv")

filt21 <- double_a_pbp2021 %>%
  filter(isPitch == TRUE) %>%
  filter(last.pitch.of.ab == TRUE) 

events <- c("single","field_out","strikeout","double","hit_by_pitch","home_run","double_play","walk",
            "triple","force_out","sac_fly","field_error","grounded_into_double_play","fielders_choice_out","other_out",
            "fielders_choice","sac_bunt","sac_fly_double_play","strikeout_double_play","triple_play")

df2_21 <- filt21 %>%
  filter(result.eventType %in% events) %>%
  mutate(strikeout = if_else(result.eventType == "strikeout" | result.eventType == "strikeout_double_play", 1, 0),
         walk = if_else(result.eventType == "walk", 1, 0),
         adj_name = paste0(matchup.batter.fullName, matchup.batter.id))

df3_21 <- df2_21 %>%
  group_by(game_date, adj_name) %>%
  summarise(sum_walk = sum(walk),
            sum_k = sum(strikeout),
            sum_pa = n())

ct_player_apps <- df3_21 %>% group_by(adj_name) %>% summarise(ct = n())

df3_21 <- left_join(df3_21, ct_player_apps, by = "adj_name")

dfroll_21 <- df3_21 %>%
  arrange(game_date) %>%
  group_by(adj_name) %>%
  mutate(roll_walk = cumsum(sum_walk),
         roll_k = cumsum(sum_k),
         roll_pa = cumsum(sum_pa))

df_tot_21 <- df2_21 %>%
  group_by(adj_name) %>%
  summarise(tot_walk = sum(walk),
            tot_k = sum(strikeout),
            tot_pa = n())

df_final_21 <- left_join(dfroll_21, df_tot_21, by = "adj_name")

la <- c(rep(mean(df_tot$tot_k) / mean(df_tot$tot_pa), length(df_final$game_date)),
        rep(mean(df_tot_21$tot_k) / mean(df_tot_21$tot_pa), length(df_final_21$game_date)))
        

dff_final <- rbind(df_final, df_final_21)

fn <- function(pad, roll_k, roll_pa, k_per, la){
  
  pad = (roll_k + pad*la) / (roll_pa + pad)
  
  resid = pad - k_per
  
  return(sqrt(sum(resid^2)))
  
}

optimize(fn, c(0,100), roll_k = dff_final$roll_k, roll_pa = dff_final$roll_pa, k_per = dff_final$tot_k / dff_final$tot_pa, la = la)

