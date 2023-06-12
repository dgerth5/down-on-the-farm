library(baseballr)
library(tidyverse)
library(furrr)
library(readr)

leagues <- mlb_league(2023) 
dates <- data.frame(day = rep(seq(as.Date("2023-04-07"),Sys.Date(), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map(function(x) mlb_game_pks(dates$day[x],
                                      level_ids = 11))

ml_game_pks <- minor_league_game_pk_lst %>%
  bind_rows() %>%
  dplyr::filter(status.codedGameState == "F",
                !is.na(game_pk)) %>%
  pull(game_pk)

plan("multisession", workers = 4)

safe_pbp <- safely(mlb_pbp)

ml_pbp <- 1:length(ml_game_pks) %>%
  furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
  purrr::map("result") %>%
  bind_rows()

ml_pbp <- ml_pbp %>% as.data.frame()

ev <- ml_pbp %>%
  drop_na(hitData.launchSpeed)

ev_perc <- ntile(ev$hitData.launchSpeed, 100)
ev_smry <- ev %>%
  group_by(matchup.batter.fullName, batting_team) %>%
  summarise(ev = quantile(hitData.launchSpeed,0.95))

ev_smry_cubs <- ev_smry %>%
  filter(batting_team == "Iowa Cubs") %>%
  arrange(-ev) %>%
  select(-batting_team) %>%
  ungroup() 

evv <- ev_smry %>%
  select(ev)  %>%
  mutate(perc = ntile(ev, 100)) %>%
  group_by(perc) %>%
  summarise(ev2 = mean(ev))

la <- ml_pbp %>%
  drop_na(hitData.launchAngle)

tightness <- la %>%
  group_by(matchup.batter.fullName, batting_team) %>%
  summarise(sd_la = sd(hitData.launchAngle),
            pa = n())

batter <- left_join(tightness, ev_smry, by = "matchup.batter.fullName")

write_csv(batter, "iowa_cubs_hit.csv")

pitcher <- ml_pbp %>%
  mutate(pfx_x = 0.4129406 + 1.6943550*pitchData.coordinates.pfxX,
         pfx_z = -0.2487306 + 1.7272317*pitchData.coordinates.pfxZ,
         vy_f = -sqrt(pitchData.coordinates.vY0^2 - (2 * pitchData.coordinates.aY	 * (50 - 17/12))),
         t = (vy_f - pitchData.coordinates.vY0) / pitchData.coordinates.aY,
         vz_f = pitchData.coordinates.vZ0 + (pitchData.coordinates.aZ	 * t),
         VAA = -atan(vz_f/vy_f) * (180 / 3.1415)) %>%
  group_by(matchup.pitcher.fullName, details.type.code, fielding_team) %>%
  summarise(mean_velo = mean(pitchData.startSpeed),
            mean_xmov = mean(pfx_x),
            mean_zmov = mean(pfx_z),
            mean_spin_rate = mean(pitchData.breaks.spinRate),
            mean_vaa = mean(VAA),
            n = n()) %>%
  drop_na()

whiff_event <- c("Swinging Strike","Swinging Strike (Blocked)", "Missed Bunt")

swing_event <- c("In play, out(s)","Swinging Strike","In play, run(s)","In play, no out",
                 "Foul Tip","Foul Bunt","Swinging Strike (Blocked)", "Missed Bunt")

whiff <- ml_pbp %>%
  mutate(swing = if_else(details.call.description %in% swing_event, 1, 0),
         whiff = if_else(details.call.description %in% whiff_event, 1, 0)) %>%
  filter(type == "pitch") %>%
  # ilter(swing == 1) %>%
  group_by(details.type.code) %>%
  summarise(whiff_Rate = mean(whiff))

write_csv(pitcher, "iowa_cubs_dat.csv")

fbs <- pitcher %>%
  filter(fielding_team == "Iowa Cubs") %>%
  filter(details.type.code == "FF" | details.type.code == "SI") %>%
  filter(n > 55) %>%
  select(-c(fielding_team, n)) %>%
  ungroup()

library(gt)
gt(fbs) %>%
  tab_header(title = md("**Selected Iowa Cubs Fastballs**"),
             subtitle = md("Season: 2023. Through 6-7-2023")) %>%
  cols_label(matchup.pitcher.fullName = "Name",
             details.type.code = "Pitch Type",
             mean_velo = "Velo",
             mean_xmov = "HB",
             mean_zmov = "IVB",
             mean_spin_rate = "Spin Rate",
             mean_vaa = "VAA") %>%
  fmt_number(c(mean_velo,mean_spin_rate), decimals = 0, sep_mark = "") %>%
  fmt_number(c(mean_xmov,mean_zmov,mean_vaa), decimals = 1)

gt(ev_smry_cubs[1:10,]) %>%
  tab_header(title = md("**Iowa Cubs 95th Percentile Exit Velocity**"),
             subtitle = md("Season: 2023. Through 6-7-2023")) %>%
  cols_label(matchup.batter.fullName = "Name",
             ev = "EV 95th")
  
