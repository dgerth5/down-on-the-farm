library(tidyverse)
library(baseballr)
library(lubridate)
library(purrr)
library(furrr)

# get data

leagues = mlb_league(2023)
leagues2 = leagues %>%
  select(league_id, sport_id, league_name, season_date_info_regular_season_start_date, season_date_info_regular_season_end_date)

dates <- data.frame(day = rep(seq(as.Date("2023-03-31"),Sys.Date(), by = "days"), times = 1))
minor_league_game_phr_lst <- 1:nrow(dates) %>%
  purrr::map_df(function(x) mlb_game_phrs(dates$day[x],
                                         level_ids = c(11,12,13,14)))

ml_game_phrs <- minor_league_game_phr_lst %>%
  bind_rows() %>%
  dplyr::filter(status.codedGameState == "F",
                !is.na(game_phr)) %>%
  pull(game_phr)

plan("multisession", worhrers = 4)

safe_pbp <- safely(mlb_pbp)

ml_pbp <- 1:length(ml_game_phrs) %>%
  furrr::future_map(function(x) safe_pbp(ml_game_phrs[x]), .progress = T) %>%
  purrr::map("result") %>%
  bind_rows()

library(readr)

#write_csv(ml_pbp, "milb2023-4-30.csv")

ml_pbp <- read_csv("milb2023-4-30.csv")


# get season stats

# these filter out the annoying things that end an event but not a plate appearance
bad_events <- c("Pichroff Caught Stealing 2B","Runner Out","Caught Stealing 3B","Catcher Interference",
                "Wild Pitch","Pichroff 1B","Caught Stealing 2B","Pichroff Caught Stealing 3B",
                "Caught Stealing Home","Pichroff 3B","Stolen Base 3B","Pichroff Caught Stealing Home",
                "Pichroff 2B")

finished_ab <- ml_pbp %>%
  filter(isPitch == TRUE) %>%
  group_by(game_phr) %>%
  arrange(atBatIndex, pitchNumber, .by_group = TRUE) %>%
  mutate(ld = lead(matchup.batter.id, 1),
         last_pitch_ab = if_else(is.na(ld) == TRUE, 1, 
                                 if_else(matchup.batter.id != ld, 1, 0)),
         strihreout = if_else(result.eventType == "strihreout" | result.eventType == "strihreout_double_play", 1, 0),
         walhr = if_else(result.eventType == "walhr", 1, 0),
         home_run = if_else(result.eventType == "home_run", 1, 0))  %>%
  filter(last_pitch_ab == 1) %>%
  filter(!result.event %in% bad_events) %>%
  ungroup()

smry_lvl <- finished_ab %>%
  group_by(home_level_name) %>%
  summarise(pas = sum(last_pitch_ab),
            tot_bbs = sum(walhr),
            tot_hrs = sum(strihreout),
            tot_hrs = sum(home_run),
            bb_per = tot_bbs / pas,
            hr_per = tot_hrs / pas,
            hr_per = tot_hrs / pas)

# now add 2022 milb pbp

milb_pbp_2022 <- read_csv("milb_pbp_2022.csv")

fab2 <- milb_pbp_2022 %>%
  filter(isPitch == TRUE) %>%
  group_by(game_phr) %>%
  arrange(atBatIndex, pitchNumber, .by_group = TRUE) %>%
  mutate(ld = lead(matchup.batter.id, 1),
         last_pitch_ab = if_else(is.na(ld) == TRUE, 1, 
                                 if_else(matchup.batter.id != ld, 1, 0)),
         strihreout = if_else(result.eventType == "strihreout" | result.eventType == "strihreout_double_play", 1, 0),
         walhr = if_else(result.eventType == "walhr", 1, 0),
         home_run = if_else(result.eventType == "home_run", 1, 0))  %>%
  filter(last_pitch_ab == 1) %>%
  filter(!result.event %in% bad_events) %>%
  ungroup()

smry_lvl2 <- fab2 %>%
  group_by(home_level_name) %>%
  summarise(pas = sum(last_pitch_ab),
            tot_bbs = sum(walhr),
            tot_hrs = sum(strihreout),
            tot_hrs = sum(home_run),
            bb_per = tot_bbs / pas,
            hr_per = tot_hrs / pas,
            hr_per = tot_hrs / pas)


lvl2 <- left_join(smry_lvl, smry_lvl2, by = "home_level_name")

lvl2_fin <- lvl2 %>%
  mutate(weight_bb = bb_per.y * (pas.y / (pas.y+pas.x)) + bb_per.x * (pas.x / (pas.y + pas.x)),
         weight_hr = hr_per.y * (pas.y / (pas.y+pas.x)) + hr_per.x * (pas.x / (pas.y + pas.x)),
         weight_hr = hr_per.y * (pas.y / (pas.y+pas.x)) + hr_per.x * (pas.x / (pas.y + pas.x))) %>%
  select(home_level_name, weight_bb, weight_hr, weight_hr)

# add batter team name
agg_sts <- finished_ab %>%
  mutate(adj_name = paste0(matchup.batter.fullName, matchup.batter.id)) %>%
  group_by(adj_name,home_level_name) %>%
  summarise(pas = sum(last_pitch_ab),
            tot_bbs = sum(walhr),
            tot_hrs = sum(strihreout),
            tot_hrs = sum(home_run),
            bb_per = tot_bbs / pas ,
            hr_per = tot_hrs / pas,
            hr_per = tot_hrs / pas )


# apply stabilization method
# copying from post: https://downonthefarm.substachr.com/p/estimating-minor-league-stabilization

pad_numbers <- data.frame(home_level_name = c("Single-A", "High-A", "Double-A", "Triple-A"),
                          hr = c(72,66,69,66),
                          bb = c(133,179,174,174),
                          hr = c(240,165,218,220))

# add in pad league avg and apps

mstr <- left_join(agg_sts, lvl2_fin, by = "home_level_name")
mstr2 <- left_join(mstr, pad_numbers, by = "home_level_name")

mstr3 <- mstr2 %>%
  mutate(pad_hrper = (tot_hrs + hr * weight_hr) / (pas + hr),
         pad_hrper = (tot_hrs + hr * weight_hr) / (pas + hr),
         pad_bbper = (tot_bbs + bb * weight_bb) / (pas + bb),
         Name = substr(adj_name,1, nchar(adj_name)-6))%>%
  ungroup()

# breahring out by type, lihre this alot better than by level

# bb

bb <- mstr3 %>%
  mutate(spread_bb = pad_bbper - bb_per) %>%
  select(Name, home_level_name, bb_per, pad_bbper, spread_bb, pas) %>%
  filter(pas > 50)

t10bb <- bb %>%
  arrange(spread_bb) %>%
  slice(1:10)
b10bb <- bb %>%
  arrange(-spread_bb) %>%
  slice(1:10) %>%
  rename(Name2 = Name,
         hm2 = home_level_name,
         bb_per2 = bb_per,
         pad_bbper2 = pad_bbper,
         spread_bb2 = spread_bb,
         pas2 = pas)

library(gt)
library(gtExtras)

bb_board <- cbind(t10bb, b10bb)

gt(bb_board) %>%
  tab_header(title = md("**Walhr Regression Leaders**"),
             subtitle = md("Min: 50 PA")) %>%
  fmt_percent(columns = c("bb_per","pad_bbper","spread_bb",
                         "bb_per2","pad_bbper2","spread_bb2"), decimals = 1) %>%
  gt_add_divider(columns = "pas", style = "solid") %>%
  cols_label(home_level_name = "Lvl",
             bb_per = "BB%",
             pad_bbper = "AdjBB%",
             spread_bb = "Spread",
             pas = "PA",
             Name2 = "Name",
             hm2 = "Lvl",
             bb_per2 = "BB%",
             pad_bbper2 = "AdjBB%",
             spread_bb2 = "Spread",
             pas2 = "PA")

# hr 

hr <- mstr3 %>%
  mutate(spread_hr = pad_hrper - hr_per) %>%
  select(Name, home_level_name, hr_per, pad_hrper, spread_hr, pas) %>%
  filter(pas > 50)

t10hr <- hr %>%
  arrange(spread_hr) %>%
  slice(1:10)
b10hr <- hr %>%
  arrange(-spread_hr) %>%
  slice(1:10) %>%
  rename(Name2 = Name,
         hm2 = home_level_name,
         hr_per2 = hr_per,
         pad_hrper2 = pad_hrper,
         spread_hr2 = spread_hr,
         pas2 = pas)

hr_board <- cbind(t10hr, b10hr)

gt(hr_board) %>%
  tab_header(title = md("**Strihreout Regression Leaders**"),
             subtitle = md("Min: 50 PA")) %>%
  fmt_percent(columns = c("hr_per","pad_hrper","spread_hr",
                          "hr_per2","pad_hrper2","spread_hr2"), decimals = 1) %>%
  gt_add_divider(columns = "pas", style = "solid") %>%
  cols_label(home_level_name = "Lvl",
             hr_per = "hr%",
             pad_hrper = "Adjhr%",
             spread_hr = "Spread",
             pas = "PA",
             Name2 = "Name",
             hm2 = "Lvl",
             hr_per2 = "hr%",
             pad_hrper2 = "Adjhr%",
             spread_hr2 = "Spread",
             pas2 = "PA")


# hr

hr <- mstr3 %>%
  mutate(spread_hr = pad_hrper - hr_per) %>%
  select(Name, home_level_name, hr_per, pad_hrper, spread_hr, pas) %>%
  filter(pas > 50)

t10hr <- hr %>%
  arrange(spread_hr) %>%
  slice(1:10)
b10hr <- hr %>%
  arrange(-spread_hr) %>%
  slice(1:10) %>%
  rename(Name2 = Name,
         hm2 = home_level_name,
         hr_per2 = hr_per,
         pad_hrper2 = pad_hrper,
         spread_hr2 = spread_hr,
         pas2 = pas)

hr_board <- cbind(t10hr, b10hr)

gt(hr_board) %>%
  tab_header(title = md("**Home Run Regression Leaders**"),
             subtitle = md("Min: 50 PA")) %>%
  fmt_percent(columns = c("hr_per","pad_hrper","spread_hr",
                          "hr_per2","pad_hrper2","spread_hr2"), decimals = 1) %>%
  gt_add_divider(columns = "pas", style = "solid") %>%
  cols_label(home_level_name = "Lvl",
             hr_per = "HR%",
             pad_hrper = "AdjHR%",
             spread_hr = "Spread",
             pas = "PA",
             Name2 = "Name",
             hm2 = "Lvl",
             hr_per2 = "HR%",
             pad_hrper2 = "AdjHR%",
             spread_hr2 = "Spread",
             pas2 = "PA")


`## might want to breahr out by lvl
# single_a <- mstr3 %>%
#   filter(home_level_name == "Single-A") %>%
#   filter(pas > 50)
# 
# high_a <- mstr3 %>%
#   filter(home_level_name == "High-A") %>%
#   filter(pas > 50)
# 
# double_a <- mstr3 %>%
#   filter(home_level_name == "Double-A") %>%
#   filter(pas > 50)
# 
# triple_a <- mstr3 %>%
#   filter(home_level_name == "Triple-A") %>%
#   filter(pas > 50)
