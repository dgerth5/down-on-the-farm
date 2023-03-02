library(readr)
library(tidyverse)

milb_pbp_2022 <- read_csv("milb_pbp_2022.csv")


smry <- milb_pbp_2022 %>%
  group_by(game_pk) %>%
  arrange(atBatIndex, .by_group = TRUE) %>%
  mutate(ld = lead(matchup.batter.id, 1),
         last_pitch_ab = if_else(is.na(ld) == TRUE, 1, 
                                 if_else(matchup.batter.id != ld, 1, 0)),
         batterid_name = paste0(matchup.batter.fullName, matchup.batter.id),
         strikeout = if_else(result.eventType == "strikeout" | result.eventType == "strikeout_double_play", 1, 0),
         walk = if_else(result.eventType == "walk", 1, 0),
         single = if_else(result.eventType == "single", 1, 0),
         double = if_else(result.eventType == "double", 1, 0),
         triple = if_else(result.eventType == "triple", 1, 0),
         home_runs = if_else(result.eventType == "home_run", 1, 0)) %>%
  filter(last_pitch_ab == 1) %>% 
  group_by(batterid_name, batting_team) %>%
  summarise(pa = n(),
            singles = sum(single),
            doubles = sum(double),
            triples = sum(triple),
            homers = sum(home_runs),
            strikeouts = sum(strikeout),
            walks = sum(walk)) %>%
  rename(team = batting_team)

library(readxl)
milbpf_wlgs_2022 <- read_excel("~/milbpf-wlgs-2022.xlsx")

get_pf_wo_tm <- function(df, tm, type){
  
  lg <- as.character(df[which(df[,1] == tm), 11])
  spec_df <- df %>%
    filter(team != tm & league == lg)
  
  if (type == "single") {
    avg_pf <- mean(unlist(spec_df[,3]))
    avg_pm <- mean(unlist(spec_df[,4]))
    
  } else if (type == "double") {
    avg_pf <- mean(unlist(spec_df[,5]))
    avg_pm <- mean(unlist(spec_df[,6]))
    
  } else if (type == "triple") {
    avg_pf <- mean(unlist(spec_df[,7]))
    avg_pm <- mean(unlist(spec_df[,8]))
    
  } else if (type == "homerun") {
    avg_pf <- mean(unlist(spec_df[,9]))
    avg_pm <- mean(unlist(spec_df[,10]))
    
  } else {
    print("type is one of single, double, triple, homerun")
  }
  
  lst <- data.frame(pf_wo_team = avg_pf, 
                    pm_wo_team = avg_pm)
  
  return(lst)
  
}

empy_df_single <- data.frame()
empy_df_double <- data.frame()
empy_df_triple <- data.frame()
empy_df_home_run <- data.frame()


tms <- milbpf_wlgs_2022$team[41]


for (i in 1:length(tms)){
  
  tm <- get_pf_wo_tm(milbpf_wlgs_2022, tms[i], "single") %>% 
    rename(single_pf_wo_team = pf_wo_team,
           single_pm_wo_team = pm_wo_team)
  
  empy_df_single <- rbind(empy_df_single, tm)
  
  tm2 <- get_pf_wo_tm(milbpf_wlgs_2022, tms[i], "single") %>% 
    rename(double_pf_wo_team = pf_wo_team,
           double_pm_wo_team = pm_wo_team)
  
  empy_df_double <- rbind(empy_df_double, tm2)
  
  tm3 <- get_pf_wo_tm(milbpf_wlgs_2022, tms[i], "single") %>% 
    rename(triple_pf_wo_team = pf_wo_team,
           triple_pm_wo_team = pm_wo_team)
  
  empy_df_triple <- rbind(empy_df_triple, tm3)
  
  tm4 <- get_pf_wo_tm(milbpf_wlgs_2022, tms[i], "single") %>% 
    rename(hr_pf_wo_team = pf_wo_team,
           hr_pm_wo_team = pm_wo_team)
  
  empy_df_home_run <- rbind(empy_df_home_run, tm4)
  
}


fnl_milb_pf <- cbind(milbpf_wlgs_2022, empy_df_single, empy_df_double, empy_df_triple, empy_df_home_run)

adding_pf <- left_join(smry, fnl_milb_pf, by = "team") %>%
  mutate(adj_single = singles / (.5*(single_pf/100) + .5*(single_pf_wo_team/100)),
         single_dff = singles - adj_single,
         adj_double = doubles / (.5*(double_pf/100) + .5*(double_pf_wo_team/100)),
         double_dff = doubles - adj_double,
         adj_triple = triples / (.5*(triple_pf/100) + .5*(triple_pf_wo_team/100)),
         triple_dff = triples - adj_triple,
         adj_homer = homers / (.5*(home_run_pf/100) + .5*(hr_pf_wo_team/100)),
         homer_dff = homers - adj_homer)

