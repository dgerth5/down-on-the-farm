library(readr)
library(tidyverse)

milb_pbp_2024 <- read_csv("~/R/basic-files/milb-pbp-2024.csv")

unique(milb_pbp_2024$home_league_name)
unique(milb_pbp_2024$home_level_id)

# triple a leaderboard

triple_a <- milb_pbp_2024 %>%
  filter(home_level_id == 11) 

ev90_triple_a <- triple_a %>%
  filter(is.na(hitData.launchSpeed) == FALSE) %>%
  group_by(matchup.batter.fullName) %>%
  summarise(ev90 = quantile(hitData.launchSpeed,0.90),
            n = n()) %>%
  mutate(ev90_per = percent_rank(ev90)*100)

ss_triple_a <- triple_a %>%
  filter(is.na(hitData.launchAngle) == FALSE) %>%
  mutate(sweetspot = if_else(hitData.launchAngle <= 32 & hitData.launchAngle >= 8, 1, 0)) %>%
  group_by(matchup.batter.fullName) %>%
  summarise(ss = mean(sweetspot),
            n = n()) %>%
  mutate(ss_per = percent_rank(ss)*100)

wh_triple_a <- triple_a %>%
  mutate(we = if_else(details.description %in% c("Swinging Strike","Swinging Strike (Blocked)", "Missed Bunt" ), 1, 0),
         se = if_else(details.description %in% c("In play, out(s)","Swinging Strike","Foul","In play, run(s)","In play, no out",
                                                 "Foul Tip","Foul Bunt","Swinging Strike (Blocked)","Foul Pitchout", "Missed Bunt"), 1, 0),
         chase_zone = if_else(pitchData.zone > 9, 1, 0)) %>%
  filter(se == 1) %>%
  group_by(matchup.batter.fullName) %>%
  summarise(whiff_rate = mean(we),
            n = n()) %>%
  mutate(wh_per = 100 - 100*percent_rank(whiff_rate))  
  

triple_a_board <- ev90_triple_a %>%
  select(matchup.batter.fullName, ev90, ev90_per) %>%
  left_join(ss_triple_a %>% select(-n), by = "matchup.batter.fullName") %>%
  left_join(wh_triple_a, by = "matchup.batter.fullName")

triple_a_filter <- triple_a_board %>%
  filter(n > 100,
         ss_per > 75,
         ev90_per < 45)


# fsl leaderboard

fsl <- milb_pbp_2024 %>%
  filter(home_league_name == "Florida State League")

ev90_fsl <- fsl %>%
  filter(is.na(hitData.launchSpeed) == FALSE) %>%
  group_by(matchup.batter.fullName) %>%
  summarise(ev90 = quantile(hitData.launchSpeed,0.90),
            n = n()) %>%
  mutate(ev90_per = percent_rank(ev90)*100)

ss_fsl <- fsl %>%
  filter(is.na(hitData.launchAngle) == FALSE) %>%
  mutate(sweetspot = if_else(hitData.launchAngle <= 32 & hitData.launchAngle >= 8, 1, 0)) %>%
  group_by(matchup.batter.fullName) %>%
  summarise(ss = mean(sweetspot),
            n = n()) %>%
  mutate(ss_per = percent_rank(ss)*100)

wh_fsl <- fsl %>%
  mutate(we = if_else(details.description %in% c("Swinging Strike","Swinging Strike (Blocked)", "Missed Bunt" ), 1, 0),
         se = if_else(details.description %in% c("In play, out(s)","Swinging Strike","Foul","In play, run(s)","In play, no out",
                                                 "Foul Tip","Foul Bunt","Swinging Strike (Blocked)","Foul Pitchout", "Missed Bunt"), 1, 0),
         chase_zone = if_else(pitchData.zone > 9, 1, 0)) %>%
  filter(se == 1) %>%
  group_by(matchup.batter.fullName) %>%
  summarise(whiff_rate = mean(we),
            n = n()) %>%
  mutate(wh_per = 100 - 100*percent_rank(whiff_rate))  

fsl_board <- ev90_fsl %>%
  select(matchup.batter.fullName, ev90, ev90_per) %>%
  left_join(ss_fsl %>% select(-n), by = "matchup.batter.fullName") %>%
  left_join(wh_fsl, by = "matchup.batter.fullName")


fsl_filter <- fsl_board %>%
  filter(n > 100,
         ss_per > 75,
         ev90_per < 50)
