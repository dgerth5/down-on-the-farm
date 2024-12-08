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
    mutate(k_per = tot_k / tot_pa,
           bb_per = tot_bb / tot_pa,
           hr_per = tot_hr / tot_pa) %>%
    filter(tot_pa > 50)
  
  id2 <- df %>% 
    filter(year == yr) %>%
    filter(sport_id == id_lo_lvl) %>%
    mutate(k_per = tot_k / tot_pa,
           bb_per = tot_bb / tot_pa,
           hr_per = tot_hr / tot_pa) %>%
    filter(tot_pa > 50)
  
  means <- rbind(id1, id2) %>%
    mutate(sing_per = tot_singles / tot_pa) %>%
    filter(tot_pa > 50) %>%
    group_by(league_name) %>%
    summarise(mean_sing = mean(sing_per, na.rm = TRUE),
              sd_sing = sd(sing_per, na.rm = TRUE)) 
  
  df1 <- left_join(id2, id1, by = "player_id") %>%
    mutate(made_lvl = if_else(is.na(tot_pa.y)==TRUE, 0, 1))
  
  return(df1) }


s18dbl <- clean_fn(df, 2018, 12, 13)
s19dbl <- clean_fn(df, 2019, 12, 13)
s21dbl <- clean_fn(df, 2021, 12, 13)
s22dbl <- clean_fn(df, 2022, 12, 13)
s23dbl <- clean_fn(df, 2023, 12, 13)

combined_df_dbl <- bind_rows(s18dbl, s19dbl, s21dbl, s22dbl, s23dbl) 

library(sampleSelection)

mod <- sampleSelection::selection(made_lvl ~ k_per.x + bb_per.x + hr_per.x,
                                  hr_per.y ~ k_per.x + bb_per.x + hr_per.x,
                                  data = combined_df_dbl)

summary(mod)

combined_df_dbl$p <- predict(mod, combined_df_dbl)

just_made <- combined_df_dbl %>%
  filter(made_lvl == 1) %>%
  drop_na(k_per.y, p) %>%
  select(k_per.y, p)

MLmetrics::MAE(just_made$p, just_made$k_per.y)


