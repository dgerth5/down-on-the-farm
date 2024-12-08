library(baseballr)
library(tidyverse)

get_minor_lg_stats <- function(season){
  
  df <- data.frame()
  
  for (i in c(1,seq(11,14,1))){
    
    r <- baseballr::mlb_stats(
      stat_type = "season",
      sport_id = i,
      season = season,
      stat_group = "hitting",
      player_pool = "All",
      limit = 10000) 
    
    df <- rbind(df, r)
    
  }
  
  df2 <- df %>%
    group_by(player_id,player_full_name, sport_id, league_name) %>%
    summarise(tot_pa = sum(plate_appearances),
              tot_hit = sum(hits),
              tot_doubles = sum(doubles),
              tot_triples = sum(triples),
              tot_hr = sum(home_runs),
              tot_go = sum(ground_outs),
              tot_bb = sum(base_on_balls),
              tot_hbp = sum(hit_by_pitch),
              tot_sb = sum(stolen_bases),
              tot_k = sum(strike_outs)) %>%
    mutate(year = season,
           tot_singles = tot_hit - tot_doubles - tot_triples - tot_hr)
  
  return(df2)
  
}

df <- map_df(c(2018:2019, 2021:2023), ~ get_minor_lg_stats(season = .x))

clean_fn <- function(df, yr, id_high_lvl, id_lo_lvl){
  
  id1 <- df %>% 
    filter(year == yr) %>%
    filter(sport_id == id_high_lvl) %>% 
    select(player_id, player_full_name, league_name, tot_pa, tot_singles)
  
  id2 <- df %>% 
    filter(year == yr) %>%
    filter(sport_id == id_lo_lvl) %>% 
    select(player_id, player_full_name, league_name, tot_pa, tot_singles)
  
  means <- rbind(id1, id2) %>%
    mutate(sing_per = tot_singles / tot_pa) %>%
    filter(tot_pa > 50) %>%
    group_by(league_name) %>%
    summarise(mean_sing = mean(sing_per, na.rm = TRUE),
              sd_sing = sd(sing_per, na.rm = TRUE)) 
  
  df1 <- left_join(id2, id1, by = "player_id") %>%
    mutate(sing_perlo = tot_singles.x / tot_pa.x, 
           sing_perhi = tot_singles.y / tot_pa.y,
           play_next_lvl = if_else(is.na(tot_pa.y)==TRUE, 0, 1)) %>%
    left_join(means, by = c("league_name.x"="league_name")) %>%
    left_join(means, by = c("league_name.y"="league_name")) %>%
    mutate(sing_perhi_z = if_else(is.na(sing_perhi)==TRUE, NA, (sing_perhi - mean_sing.y) / sd_sing.y),
           sing_perlo_z = if_else(is.na(sing_perlo)==TRUE, NA, (sing_perlo - mean_sing.x) / sd_sing.x)) %>%
    filter(play_next_lvl == 1) %>%
    filter(tot_pa.x > 50) %>% filter(tot_pa.y > 50) %>% ungroup()
  
  return(df1)
}

# 1 MLB, 11 Triple-A, 12 Double-A, 13 High-A, 14 Single-A
s18mlb <- clean_fn(df, 2018, 1, 11)
s19mlb <- clean_fn(df, 2019, 1, 11)
s21mlb <- clean_fn(df, 2021, 1, 11)
s22mlb <- clean_fn(df, 2022, 1, 11)
s23mlb <- clean_fn(df, 2023, 1, 11)

combined_df_mlb <- bind_rows(s18mlb, s19mlb, s21mlb, s22mlb, s23mlb) %>%
  mutate(sing_change_per = sing_perhi_z - sing_perlo_z)


s18trip <- clean_fn(df, 2018, 11, 12)
s19trip <- clean_fn(df, 2019, 11, 12)
s21trip <- clean_fn(df, 2021, 11, 12)
s22trip <- clean_fn(df, 2022, 11, 12)
s23trip <- clean_fn(df, 2023, 11, 12)

combined_df_trip <- bind_rows(s18trip, s19trip, s21trip, s22trip, s23trip) %>%
  mutate(sing_change_per = sing_perhi_z - sing_perlo_z)


s18dbl <- clean_fn(df, 2018, 12, 13)
s19dbl <- clean_fn(df, 2019, 12, 13)
s21dbl <- clean_fn(df, 2021, 12, 13)
s22dbl <- clean_fn(df, 2022, 12, 13)
s23dbl <- clean_fn(df, 2023, 12, 13)

combined_df_dbl <- bind_rows(s18dbl, s19dbl, s21dbl, s22dbl, s23dbl) %>%
  mutate(sing_change_per = sing_perhi_z - sing_perlo_z)

s18hi_a <- clean_fn(df, 2018, 13, 14)
s19hi_a <- clean_fn(df, 2019, 13, 14)
s21hi_a <- clean_fn(df, 2021, 13, 14)
s22hi_a <- clean_fn(df, 2022, 13, 14)
s23hi_a <- clean_fn(df, 2023, 13, 14)

combined_df_hi_a <- bind_rows(s18hi_a, s19hi_a, s21hi_a, s22hi_a, s23hi_a)  %>%
  mutate(sing_change_per = sing_perhi_z - sing_perlo_z)

# lo-a hi-a 16% increase
# hi-a dbl-a 9% increase
# dbl-a triple-a 5% increase
# trpl-a mlb 31% increase

tbl_df <- data.frame(Lvl = c("Triple A -> MLB", 
                             "Double-A -> Triple-A",
                             "High-A -> Double-A",
                             "Single-A -> High-A"),
                     PercentIncrease = c(round(mean(combined_df_mlb$sing_change_per),2), 
                                         round(mean(combined_df_trip$sing_change_per),2), 
                                         round(mean(combined_df_dbl$sing_change_per),2), 
                                         round(mean(combined_df_hi_a$sing_change_per),2)))

head(tbl_df)


get_z_score_dat <- function(df, yr){
  
  id1 <- df %>% 
    filter(year == yr) %>%
    select(player_id, player_full_name, league_name, sport_id, tot_pa, tot_singles) %>%
    mutate(sing_per = tot_singles / tot_pa)
  
  means <- id1 %>%
    filter(tot_pa > 50) %>%
    group_by(sport_id) %>%
    summarise(mean_sing = mean(sing_per, na.rm = TRUE),
              sd_sing = sd(sing_per, na.rm = TRUE)) }

zsdf <- get_z_score_dat(df, 2023)

head(zsdf)
