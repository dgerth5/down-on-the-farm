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
         home_runs = if_else(result.eventType == "home_run", 1, 0),
         sac = if_else(result.eventType == "sac_fly" | result.eventType == "sac_bunt", 1, 0),
         hpb = if_else(result.eventType == "Hit By Pitch", 1, 0)) %>%
  filter(last_pitch_ab == 1) %>% 
  group_by(batterid_name, batting_team) %>%
  summarise(pa = n(),
            singles = sum(single),
            doubles = sum(double),
            triples = sum(triple),
            homers = sum(home_runs),
            strikeouts = sum(strikeout),
            walks = sum(walk),
            sacs = sum(sac),
            hbps = sum(hpb)) %>%
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
  
 # lst <- lst[!duplicated(lst)]
  
  return(lst)
  
}

empy_df_single <- data.frame()
empy_df_double <- data.frame()
empy_df_triple <- data.frame()
empy_df_home_run <- data.frame()

tms <- milbpf_wlgs_2022$team

for (i in 1:length(tms)){
  
  tm <- get_pf_wo_tm(milbpf_wlgs_2022, tms[i], "single") %>% 
    rename(single_pf_wo_team = pf_wo_team,
           single_pm_wo_team = pm_wo_team)
  
  empy_df_single <- rbind(empy_df_single, tm)
  
  tm2 <- get_pf_wo_tm(milbpf_wlgs_2022, tms[i], "double") %>% 
    rename(double_pf_wo_team = pf_wo_team,
           double_pm_wo_team = pm_wo_team)
  
  empy_df_double <- rbind(empy_df_double, tm2)
  
  tm3 <- get_pf_wo_tm(milbpf_wlgs_2022, tms[i], "triple") %>% 
    rename(triple_pf_wo_team = pf_wo_team,
           triple_pm_wo_team = pm_wo_team)
  
  empy_df_triple <- rbind(empy_df_triple, tm3)
  
  tm4 <- get_pf_wo_tm(milbpf_wlgs_2022, tms[i], "homerun") %>% 
    rename(hr_pf_wo_team = pf_wo_team,
           hr_pm_wo_team = pm_wo_team)
  
  empy_df_home_run <- rbind(empy_df_home_run, tm4)
  
}


fnl_milb_pf <- cbind(milbpf_wlgs_2022, empy_df_single, empy_df_double, empy_df_triple, empy_df_home_run)

adding_pf <- left_join(smry, fnl_milb_pf, by = "team") %>%
  mutate(adj_single = singles / (.5*(single_pf/100) + .5*(single_pf_wo_team/100)),
         single_dff = adj_single - singles,
         adj_double = doubles / (.5*(double_pf/100) + .5*(double_pf_wo_team/100)),
         double_dff = adj_double - doubles,
         adj_triple = triples / (.5*(triple_pf/100) + .5*(triple_pf_wo_team/100)),
         triple_dff = adj_triple - triples,
         adj_homer = homers / (.5*(home_run_pf/100) + .5*(hr_pf_wo_team/100)),
         homer_dff = adj_homer - homers)

combin <- adding_pf %>%
  group_by(batterid_name) %>%
  summarise(tot_pa = sum(pa),
            tot_singles = sum(singles),
            tot_doubles = sum(doubles),
            tot_triples = sum(triples),
            tot_homers = sum(homers),
            tot_adj_singles = sum(adj_single),
            tot_adj_doubles = sum(adj_double),
            tot_adj_triples = sum(adj_triple),
            tot_adj_homers = sum(adj_homer),
            tot_walks = sum(walks),
            tot_strikeouts = sum(strikeouts),
            tot_sacs = sum(sacs),
            tot_hbp = sum(hbps)) %>%
  mutate(tot_single_diff = tot_adj_singles - tot_singles,
         tot_double_diff = tot_adj_doubles - tot_doubles,
         tot_triple_diff = tot_adj_triples - tot_triples,
         tot_homer_diff = tot_adj_homers - tot_homers,
         single_per = tot_singles / tot_pa,
         adj_single_per = tot_adj_singles / tot_pa,
         single_per_diff = adj_single_per - single_per,
         double_per = tot_doubles / tot_pa,
         adj_double_per = tot_adj_doubles / tot_pa,
         double_per_diff = adj_double_per - double_per,
         triple_per = tot_triples / tot_pa,
         adj_triple_per = tot_adj_triples / tot_pa,
         triple_per_diff = adj_triple_per - triple_per,
         homer_per = tot_homers / tot_pa*100,
         adj_homer_per = tot_adj_homers / tot_pa*100,
         homer_per_diff = adj_homer_per - homer_per,
         Name = substr(batterid_name,1, nchar(batterid_name)-6),
         adj_batting_avg = (tot_adj_singles + tot_adj_doubles + tot_adj_triples + tot_adj_homers) /
           (tot_pa - tot_walks - tot_sacs - tot_hbp),
         adj_obp = (tot_adj_singles + tot_adj_doubles + tot_adj_triples + tot_adj_homers + tot_walks + tot_hbp) /
           (tot_pa - tot_sacs),
         adj_slg = (tot_adj_singles + tot_adj_doubles*2 + tot_adj_triples*3 + tot_adj_homers*4) /
           (tot_pa - tot_walks - tot_sacs - tot_hbp),
         batting_avg = (tot_singles + tot_doubles + tot_triples + tot_homers) /
           (tot_pa - tot_walks - tot_sacs - tot_hbp),
         obp = (tot_singles + tot_doubles + tot_triples + tot_homers + tot_walks + tot_hbp) /
           (tot_pa - tot_sacs),
         slg = (tot_singles + tot_doubles*2 + tot_triples*3 + tot_homers*4) /
           (tot_pa - tot_walks - tot_sacs - tot_hbp)
         ) %>%
  filter(tot_pa >= 200)

library(gt)
library(stringr)

top_hr <- combin %>%
  arrange(-homer_per_diff) %>%
  mutate(Name = substr(batterid_name,1, nchar(batterid_name)-6)) %>%
  select(Name, homer_per, adj_homer_per, homer_per_diff) %>%
  rename(`Home_Run%` = homer_per,
         `Adj_Home_Run%` = adj_homer_per,
         `Diff%` = homer_per_diff) %>%
  slice(1:5) 

bot_hr <- combin %>%
  arrange(homer_per_diff) %>%
  mutate(Name = substr(batterid_name,1, nchar(batterid_name)-6)) %>%
  select(Name, homer_per, adj_homer_per, homer_per_diff) %>%
  rename(`Home_Run%` = homer_per,
         `Adj_Home_Run%` = adj_homer_per,
         `Diff%` = homer_per_diff) %>%
  slice(1:5) 

gt(top_hr) %>%
  tab_header(title = md("**Players Who Were Hurt The Most**"),
             subtitle = md("Season: 2022, Min 200 PA")) %>%
  fmt_number(columns = c("Home_Run%","Adj_Home_Run%","Diff%"), decimals = 1) %>%
  cols_label(`Home_Run%` = "Home Run %",
             `Adj_Home_Run%` = "Adj. Home Run%")

combin$name = ifelse(combin$Name %in% c(top_hr$Name,bot_hr$Name), combin$Name, "")

library(ggplot2)
library(ggrepel)

ggplot(combin, aes(x = homer_per, y = adj_homer_per, label = name)) +
  geom_point() +
  geom_text_repel(min.segment.length = 0, max.overlaps = 10000) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Home Run %", y = "Park Factor Adjusted Home Run %",
       title = "Home Run % v Adjusted Home Run %",
       subtitle = "Season: 2022, Min 200 PA")

df <- data.frame(Hit_Type = c("Single","Double","Triple","Home Run"),
                 Spread = c(11.9,19.7,49.0,63.1)) %>%
  gt() %>%
  tab_header(title = md("**Difference in Park Factor Between Highest and Lowest**"),
             subtitle = md("Season: 2022"))

top_name <- c("Gunnar Henderson",
         "Corbin Carroll",
         "Elly De La Cruz",
         "Jackson Chourio",
         "Jordan Walker",
         "Francisco Álvarez",
         "Pete Crow-Armstrong",
         "Jordan Lawlar",
         "Marcelo Mayer",
         "Diego Cartaya",
         "Masyn Winn")

library(gtExtras)

combin %>%
  filter(Name %in% top_name) %>%
  mutate(ops = obp + slg,
         pfops = adj_obp + adj_slg,
         ops_diff = pfops - ops) %>%
  select(Name, batting_avg, obp, slg, adj_batting_avg, adj_obp, adj_slg, ops, pfops, ops_diff) %>%
  gt() %>%
  tab_header(title = md("**Raw Slash Line vs. Park Factor Adj. Slash Line**"),
           subtitle = md("Selected Top Prospects")) %>%
  fmt_number(columns = c("batting_avg", "adj_batting_avg", "obp", "adj_obp", "slg", "adj_slg", "ops", "pfops", "ops_diff"), decimals = 3) %>%
  gt_add_divider(columns = "slg", style = "solid") %>%
  gt_add_divider(columns = "adj_slg", style = "solid") %>%
  gt_color_rows(ops_diff, palette = c("blue", "white", "red"), domain = c(-.200, .200), na.color = "#FFFFFF") %>%
  cols_label(batting_avg = "BA",
             adj_batting_avg = "pfBA",
             obp = "OBP",
             adj_obp = "pfOBP",
             slg = "SLG",
             adj_slg = "pfSLG",
             ops = "OPS",
             pfops = "pfOPS",
             ops_diff = "OPS Diff")

leader <- adding_pf %>%
  group_by(batterid_name) %>%
  summarise(tot_pa = sum(pa),
            tot_singles = sum(singles),
            tot_doubles = sum(doubles),
            tot_triples = sum(triples),
            tot_homers = sum(homers),
            tot_adj_singles = sum(adj_single),
            tot_adj_doubles = sum(adj_double),
            tot_adj_triples = sum(adj_triple),
            tot_adj_homers = sum(adj_homer),
            tot_walks = sum(walks),
            tot_strikeouts = sum(strikeouts),
            tot_sacs = sum(sacs),
            tot_hbp = sum(hbps)) %>%
  mutate(tot_single_diff = tot_adj_singles - tot_singles,
         tot_double_diff = tot_adj_doubles - tot_doubles,
         tot_triple_diff = tot_adj_triples - tot_triples,
         tot_homer_diff = tot_adj_homers - tot_homers,
         single_per = tot_singles / tot_pa,
         adj_single_per = tot_adj_singles / tot_pa,
         single_per_diff = adj_single_per - single_per,
         double_per = tot_doubles / tot_pa,
         adj_double_per = tot_adj_doubles / tot_pa,
         double_per_diff = adj_double_per - double_per,
         triple_per = tot_triples / tot_pa,
         adj_triple_per = tot_adj_triples / tot_pa,
         triple_per_diff = adj_triple_per - triple_per,
         homer_per = tot_homers / tot_pa*100,
         adj_homer_per = tot_adj_homers / tot_pa*100,
         homer_per_diff = adj_homer_per - homer_per,
         Name = substr(batterid_name,1, nchar(batterid_name)-6),
         adj_batting_avg = (tot_adj_singles + tot_adj_doubles + tot_adj_triples + tot_adj_homers) /
           (tot_pa - tot_walks - tot_sacs - tot_hbp),
         adj_obp = (tot_adj_singles + tot_adj_doubles + tot_adj_triples + tot_adj_homers + tot_walks + tot_hbp) /
           (tot_pa - tot_sacs),
         adj_slg = (tot_adj_singles + tot_adj_doubles*2 + tot_adj_triples*3 + tot_adj_homers*4) /
           (tot_pa - tot_walks - tot_sacs - tot_hbp),
         batting_avg = (tot_singles + tot_doubles + tot_triples + tot_homers) /
           (tot_pa - tot_walks - tot_sacs - tot_hbp),
         obp = (tot_singles + tot_doubles + tot_triples + tot_homers + tot_walks + tot_hbp) /
           (tot_pa - tot_sacs),
         slg = (tot_singles + tot_doubles*2 + tot_triples*3 + tot_homers*4) /
           (tot_pa - tot_walks - tot_sacs - tot_hbp),
         diff_ops = adj_slg + adj_obp - slg - obp) %>%
  select(Name, tot_pa, batting_avg, obp, slg, adj_batting_avg, adj_obp, adj_slg, diff_ops)

leader[,-c(1,2)] <- round(leader[,-c(1,2)],3)

write_csv(leader, "adj_leaderboard22.csv")
