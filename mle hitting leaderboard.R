library(tidyverse)

get_minor_lg_stats_h <- function(season){
  
  df <- data.frame()
  
  for (i in seq(11,14,1)){
    
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
    select(player_full_name, sport_abbreviation, league_name, team_name,
           plate_appearances, at_bats, sac_flies, hits, doubles, triples, home_runs, base_on_balls, strike_outs, hit_by_pitch) %>%
    mutate(k_per = strike_outs / plate_appearances,
           bb_per = base_on_balls / plate_appearances,
           s_per = (hits - doubles - triples - home_runs) / plate_appearances,
           d_per = doubles / plate_appearances, t_per = triples / plate_appearances,
           xbh_per = (doubles + triples) / plate_appearances,
           hr_per = home_runs / plate_appearances) %>%
    filter(plate_appearances > 0)
  
  milb <- data.frame(Level = c("AAA","AA","A+","A"),
                     k_per1 = c(.039,.065,.091,.12),
                     bb_per1 = c(-.009,-.021,-.034,-.047),
                     s_per1 = c(-.01,-.018,-.024,-.033),
                     xbh_per1 = c(-.006,-.01,-.018,-.026),
                     hr_per1 = c(-.009,-.018,-.025,-.031))
  
  df3 <- df2 %>%
    left_join(milb, by = c("sport_abbreviation"="Level")) %>%
    mutate(MLE_K_Per = k_per + k_per1,
           MLE_BB_Per = bb_per + bb_per1,
           MLE_S_Per = s_per + s_per1,
           MLE_XBH_Per = xbh_per + xbh_per1,
           MLE_HR_Per = hr_per + hr_per1,
           MLE_BA = (MLE_S_Per*plate_appearances + MLE_XBH_Per*plate_appearances + MLE_HR_Per*plate_appearances) / at_bats,
           MLE_OBP = (MLE_S_Per*plate_appearances + MLE_XBH_Per*plate_appearances + MLE_HR_Per*plate_appearances + MLE_BB_Per*plate_appearances + hit_by_pitch) / (at_bats + base_on_balls + hit_by_pitch + sac_flies),
           MLE_SLG = (MLE_S_Per*plate_appearances + (d_per + xbh_per1)*plate_appearances*2 + (t_per + xbh_per1)*plate_appearances*3 + MLE_HR_Per*plate_appearances*4) / at_bats) %>%
    select(-c("sac_flies","k_per1","bb_per1","s_per1","xbh_per1","hr_per1"))

  return(df3)
}

milbh2024 <- get_minor_lg_stats_h(2024)

write_csv(milbh2024, "milb_h_leaderboard.csv")
