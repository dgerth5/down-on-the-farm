library(readr)
library(tidyverse)

milb_pbp_2022 <- read_csv("milb_pbp_2022.csv")

double_a_pbp2022 <- milb_pbp_2022 %>%
  filter(home_level_name == "Single-A") %>%
  filter(isPitch == TRUE)

filt <- double_a_pbp2022 %>%
  group_by(game_pk) %>%
  arrange(atBatIndex, .by_group = TRUE) %>%
  mutate(ld = lead(matchup.batter.id, 1),
         last_pitch_ab = if_else(is.na(ld) == TRUE, 1, 
                                 if_else(matchup.batter.id != ld, 1, 0))) %>%
  filter(last_pitch_ab == 1)

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
            tot_pa = n())

df_final <- left_join(dfroll, df_tot, by = "adj_name")

milb_pbp_2021 <- read_csv("milb_pbp_2021.csv")

double_a_pbp2021 <- milb_pbp_2021 %>%
  filter(home_level_name == "Single-A") %>%
  filter(isPitch == TRUE)

filt21 <- double_a_pbp2021 %>%
  group_by(game_pk) %>%
  arrange(atBatIndex, .by_group = TRUE) %>%
  mutate(ld = lead(matchup.batter.id, 1),
         last_pitch_ab = if_else(is.na(ld) == TRUE, 1, 
                                 if_else(matchup.batter.id != ld, 1, 0))) %>%
  filter(last_pitch_ab == 1)

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

la_bb <- c(rep(mean(df_tot$tot_walk) / mean(df_tot$tot_pa), length(df_final$game_date)),
          rep(mean(df_tot_21$tot_walk) / mean(df_tot_21$tot_pa), length(df_final_21$game_date)))

la_k <- c(rep(mean(df_tot$tot_k) / mean(df_tot$tot_pa), length(df_final$game_date)),
          rep(mean(df_tot_21$tot_k) / mean(df_tot_21$tot_pa), length(df_final_21$game_date)))
        

dff_final <- rbind(df_final, df_final_21)

#write_csv(dff_final, file = "padding_test2.csv")


fn <- function(pad, roll_stat, roll_pa, stat_per, la){
  
  pad2 = (roll_stat + pad*la) / (roll_pa + pad)
  
  rmse = MLmetrics::RMSE(pad2, stat_per)

}

k_pad <- optimize(fn, c(0,100), roll_stat = dff_final$roll_k, roll_pa = dff_final$roll_pa, stat_per = dff_final$tot_k / dff_final$tot_pa, la = la_k)$minimum
bb_pad <- optimize(fn, c(0,100), roll_stat = dff_final$roll_walk, roll_pa = dff_final$roll_pa, stat_per = dff_final$tot_walk / dff_final$tot_pa, la = la_bb)$minimum

