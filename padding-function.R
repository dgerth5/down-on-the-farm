library(readr)
library(tidyverse)

# load data
milb_pbp_2022 <- read_csv("milb_pbp_2022.csv")
milb_pbp_2021 <- read_csv("milb_pbp_2021.csv")


fn <- function(df22, df21, lvl) {
  
  milb_pbp_2022 <- df22
  
  double_a_pbp2022 <- milb_pbp_2022 %>%
    filter(home_level_name == lvl) %>%
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
           single = if_else(result.eventType == "single", 1, 0),
           double = if_else(result.eventType == "double", 1, 0),
           triple = if_else(result.eventType == "triple", 1, 0),
           home_runs = if_else(result.eventType == "home_run", 1, 0),
           adj_name = paste0(matchup.batter.fullName, matchup.batter.id))
  
  df3 <- df2 %>%
    group_by(game_date, adj_name) %>%
    summarise(sum_walk = sum(walk),
              sum_k = sum(strikeout),
              sum_sing = sum(single),
              sum_dbl = sum(double),
              sum_tri = sum(triple),
              sum_hr = sum(home_runs),
              sum_pa = n())
  
  ct_player_apps <- df3 %>% group_by(adj_name) %>% summarise(ct = n())
  
  df3 <- left_join(df3, ct_player_apps, by = "adj_name")
  
  dfroll <- df3 %>%
    arrange(game_date) %>%
    group_by(adj_name) %>%
    mutate(roll_walk = cumsum(sum_walk),
           roll_k = cumsum(sum_k),
           roll_sing = cumsum(sum_sing),
           roll_dbl = cumsum(sum_dbl),
           roll_tri = cumsum(sum_tri),
           roll_hr = cumsum(sum_hr),
           roll_pa = cumsum(sum_pa))
  
  df_tot <- df2 %>%
    group_by(adj_name) %>%
    summarise(tot_walk = sum(walk),
              tot_k = sum(strikeout),
              tot_sing = sum(single),
              tot_dbl = sum(double),
              tot_tri = sum(triple),
              tot_hr = sum(home_runs),
              tot_pa = n())
  
  df_final <- left_join(dfroll, df_tot, by = "adj_name") %>% 
    mutate(ry_walk = tot_walk - roll_walk,
           ry_k = tot_k - roll_k,
           ry_sing = tot_sing - roll_sing,
           ry_dbl = tot_dbl - roll_dbl,
           ry_tri = tot_tri - roll_tri,
           ry_hr = tot_hr - roll_hr,
           ry_pa = tot_pa - roll_pa) %>%
    filter(tot_pa > 300) %>%
    filter(ry_pa != 0)
  
  milb_pbp_2021 <- df21
  
  lvl2 <- if_else(lvl == "Single-A", "Low-A", lvl)
  
  double_a_pbp2021 <- milb_pbp_2021 %>%
    filter(home_level_name == lvl2) %>%
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
           single = if_else(result.eventType == "single", 1, 0),
           double = if_else(result.eventType == "double", 1, 0),
           triple = if_else(result.eventType == "triple", 1, 0),
           home_runs = if_else(result.eventType == "home_run", 1, 0),
           adj_name = paste0(matchup.batter.fullName, matchup.batter.id))
  
  df3_21 <- df2_21 %>%
    group_by(game_date, adj_name) %>%
    summarise(sum_walk = sum(walk),
              sum_k = sum(strikeout),
              sum_sing = sum(single),
              sum_dbl = sum(double),
              sum_tri = sum(triple),
              sum_hr = sum(home_runs),
              sum_pa = n())
  
  ct_player_apps <- df3_21 %>% group_by(adj_name) %>% summarise(ct = n())
  
  df3_21 <- left_join(df3_21, ct_player_apps, by = "adj_name")
  
  dfroll_21 <- df3_21 %>%
    arrange(game_date) %>%
    group_by(adj_name) %>%
    mutate(roll_walk = cumsum(sum_walk),
           roll_k = cumsum(sum_k),
           roll_sing = cumsum(sum_sing),
           roll_dbl = cumsum(sum_dbl),
           roll_tri = cumsum(sum_tri),
           roll_hr = cumsum(sum_hr),
           roll_pa = cumsum(sum_pa))
  
  df_tot_21 <- df2_21 %>%
    group_by(adj_name) %>%
    summarise(tot_walk = sum(walk),
              tot_k = sum(strikeout),
              tot_sing = sum(single),
              tot_dbl = sum(double),
              tot_tri = sum(triple),
              tot_hr = sum(home_runs),
              tot_pa = n())
  
  df_final_21 <- left_join(dfroll_21, df_tot_21, by = "adj_name") %>%
    mutate(ry_walk = tot_walk - roll_walk,
           ry_k = tot_k - roll_k,
           ry_sing = tot_sing - roll_sing,
           ry_dbl = tot_dbl - roll_dbl,
           ry_tri = tot_tri - roll_tri,
           ry_hr = tot_hr - roll_hr,
           ry_pa = tot_pa - roll_pa) %>%
    filter(tot_pa > 300) %>%
    filter(ry_pa != 0)
  
  la_bb <- c(rep(mean(df_tot$tot_walk) / mean(df_tot$tot_pa), length(df_final$game_date)),
             rep(mean(df_tot_21$tot_walk) / mean(df_tot_21$tot_pa), length(df_final_21$game_date)))
  
  la_k <- c(rep(mean(df_tot$tot_k) / mean(df_tot$tot_pa), length(df_final$game_date)),
            rep(mean(df_tot_21$tot_k) / mean(df_tot_21$tot_pa), length(df_final_21$game_date)))
  
  la_1b <- c(rep(mean(df_tot$tot_sing) / mean(df_tot$tot_pa), length(df_final$game_date)),
             rep(mean(df_tot_21$tot_sing) / mean(df_tot_21$tot_pa), length(df_final_21$game_date)))
  
  la_2b <- c(rep(mean(df_tot$tot_dbl) / mean(df_tot$tot_pa), length(df_final$game_date)),
             rep(mean(df_tot_21$tot_dbl) / mean(df_tot_21$tot_pa), length(df_final_21$game_date)))
  
  la_3b <- c(rep(mean(df_tot$tot_tri) / mean(df_tot$tot_pa), length(df_final$game_date)),
             rep(mean(df_tot_21$tot_tri) / mean(df_tot_21$tot_pa), length(df_final_21$game_date)))
  
  la_hr <- c(rep(mean(df_tot$tot_hr) / mean(df_tot$tot_pa), length(df_final$game_date)),
             rep(mean(df_tot_21$tot_hr) / mean(df_tot_21$tot_pa), length(df_final_21$game_date)))
  
  dff_final <- rbind(df_final, df_final_21)
  
  #write_csv(dff_final, file = "padding_test2.csv")
  
  
  fn <- function(pad, roll_stat, roll_pa, stat_per, la){
    
    pad2 = (roll_stat + pad*la) / (roll_pa + pad)
    
    rmse = MLmetrics::RMSE(pad2, stat_per)
    
  }
  
  k_pad <- optimize(fn, c(0,5000), roll_stat = dff_final$roll_k, roll_pa = dff_final$roll_pa, stat_per = dff_final$ry_k / dff_final$ry_pa, la = la_k)$minimum
  bb_pad <- optimize(fn, c(0,5000), roll_stat = dff_final$roll_walk, roll_pa = dff_final$roll_pa, stat_per = dff_final$ry_walk / dff_final$ry_pa, la = la_bb)$minimum
  sing_pad <- optimize(fn, c(0,5000), roll_stat = dff_final$roll_sing, roll_pa = dff_final$roll_pa, stat_per = dff_final$ry_sing / dff_final$ry_pa, la = la_1b)$minimum
  hr_pad <- optimize(fn, c(0,5000), roll_stat = dff_final$roll_hr, roll_pa = dff_final$roll_pa, stat_per = dff_final$ry_hr / dff_final$ry_pa, la = la_hr)$minimum


  pad_df <- data.frame(Level = lvl,
                       K_Pad = k_pad,
                       BB_Pad = bb_pad,
                       Single_Pad = sing_pad,
                       Home_Run_Pad = hr_pad)

  return(pad_df)
  

}

df1 <- fn(milb_pbp_2022, milb_pbp_2021, "Single-A")
df2 <- fn(milb_pbp_2022, milb_pbp_2021, "High-A")
df3 <- fn(milb_pbp_2022, milb_pbp_2021, "Double-A")
df4 <- fn(milb_pbp_2022, milb_pbp_2021, "Triple-A")


library(janitor)
pad_df <- rbind(df1,df2,df3,df4) %>%
  adorn_totals(name = "Average") %>%
  mutate(across(where(is.numeric), 
                ~ replace(., n(), .[n()]/(n()-1)))) 

library(gt)

pad_df %>% 
  gt() %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = Level == "Average")) %>%
  tab_header(title = md("**Padding Values By Stat Type and Level**"),
             subtitle = md("Padding Value Units are Plate Appearances")) %>%
  fmt_number(columns = -"Level", decimals = 0)

