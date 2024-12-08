library(readr)
library(mgcv)
library(tidyverse)
library(catboost)
library(gt)
library(gtExtras)
library(mlbplotR)
library(mixedup)

statcast23 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast23.csv")
statcast22 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast22.csv")
statcast21 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast21.csv")
statcast20 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast20.csv")
statcast19 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast19.csv")
statcast18 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast18.csv")

sc <- bind_rows(statcast23, statcast22, statcast21, statcast20, statcast19, statcast18)

# build RE288 matrix

re288_df <- sc %>%
  mutate(id = paste0(game_pk,"-",inning,"-",inning_topbot)) %>%
  group_by(id) %>%
  summarise(max_run = max(post_bat_score))

re288_df2 <- sc %>%
  mutate(id = paste0(game_pk,"-",inning,"-",inning_topbot)) %>%
  left_join(re288_df, by = "id") %>%
  mutate(diff_max_run = max_run - post_bat_score,
         br = paste0(if_else(is.na(on_1b)==TRUE,0,1), "-",
                     if_else(is.na(on_2b)==TRUE,0,1), "-",
                     if_else(is.na(on_3b)==TRUE,0,1)),
         count = paste0(balls,"-",strikes)) %>%
  filter(balls != 4) %>%
  filter(strikes != 3) %>%
  group_by(br, count, outs_when_up) %>%
  summarise(re = mean(diff_max_run))

# build BIP model

bip_df <- sc %>%
  drop_na(launch_angle, launch_speed) %>%
  filter(description == "hit_into_play") %>%
  mutate(adj_event = if_else(events %in% c("single","double","triple","home_run"), events, "out")) %>%
  select(adj_event, home_team, launch_angle, launch_speed) 

bip_df$home_team <- as.integer(as.factor(bip_df$home_team))

mod_pool <- catboost.load_pool(bip_df[,-1],
                               label = as.integer(as.factor(bip_df$adj_event)))

pms <- list(iterations = 1000,
            loss_function = "MultiClass")

bip_mod <- catboost.train(mod_pool, params = pms)

bip2 <- bip_df %>%
  mutate(bip_label = as.integer(as.factor(adj_event)))

# build xEV & xLA model
# just 2023

just_bb <- statcast23 %>%
  drop_na(launch_speed, launch_angle) %>%
  select(launch_speed, launch_angle, batter, plate_x, plate_z, release_pos_z, release_speed) %>%
  mutate_at("batter", factor)

mod_ev <- bam(launch_speed ~ s(batter, bs = "re") + s(plate_x, plate_z) + release_pos_z + release_speed,
              data = just_bb)

ev_re <- extract_random_effects(mod_ev) 
ev_re$group <- as.numeric(ev_re$group)

fg_id <- read_csv("C:/Users/david/Downloads/fangraphs-leaderboards (10).csv")
fg_id2 <- fg_id %>%
  select(MLBAMID, Name)

ev_re2 <- left_join(ev_re, fg_id2, by = c("group"="MLBAMID")) %>%
  select(Name, value)

l <- mlbplotR::load_headshots()

head <- mlbplotR::load_headshots() %>%
  select(espn_name, espn_headshot)
 
top5_ev_re <- ev_re2 %>%
  arrange(-value) %>%
  slice(1:5) %>%
  left_join(head, by = c("Name"= "espn_name"))

top5_ev_re$espn_headshot[3] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/36185.png"

bot5_ev_re <- ev_re2 %>%
  arrange(value) %>%
  slice(1:5) %>%
  rename("Name2" = Name,
         "EV_Influence" = value) %>%
  left_join(head, by = c("Name2"= "espn_name")) %>%
  rename("espn_hd2" = espn_headshot)

bot5_ev_re$espn_hd2[5] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/41326.png"


table <- cbind(top5_ev_re, bot5_ev_re)

tbl2 <- table %>%
  select(Name, espn_headshot, value, Name2, espn_hd2, EV_Influence) %>%
  gt() %>%
  tab_header(title = md("**Best and Worst EV Influencers**"),
             subtitle = md("Season: 2023")) %>%
  gt_add_divider(columns = "value", style = "solid") %>%
  gt_img_rows(espn_headshot, height = 40) %>%
  gt_img_rows(espn_hd2, height = 40) %>%
  fmt_number(columns = c("value","EV_Influence"), decimals = 1) %>%
  cols_label(Name2 = "Name",
             value = "EV Influence (MPH)",
             EV_Influence = "EV Influence (MPH)",
             espn_headshot = "",
             espn_hd2 = "")

gtsave(tbl2, "ev_influence.png")

mod_la <- bam(launch_angle ~ s(batter, bs = "re") + s(plate_x, plate_z) + release_pos_z + release_speed,
              data = just_bb)

la_re <- extract_random_effects(mod_la) 



statcast23$xEV <- predict.bam(mod_ev, statcast23)
statcast23$xLA <- predict.bam(mod_la, statcast23)

# get predicted event probs of every pitch assuming swung at

sc23 <- statcast23 %>%
  select(home_team, xLA, xEV) %>%
  rename("launch_speed" = xEV,
         "launch_angle" = xLA)

sc23$home_team <- as.integer(as.factor(sc23$home_team))

statcast23_pool <- catboost.load_pool(sc23)

sc23_preds <- catboost.predict(bip_mod, statcast23_pool, prediction_type = "Probability")

# predicted called_strike model
# rv of take = p_strike*rv_strike_ct + p_ball*rv_ball_ct

called_str_df <- sc %>%
  filter(description %in% c("called_strike","ball","blocked_ball")) %>%
  mutate(called_str = if_else(description == "called_strike", 1, 0)) %>%
  select(called_str, plate_x, plate_z, release_speed, release_pos_z)

called_str_pool <- catboost.load_pool(called_str_df[,-1],
                                      label = called_str_df$called_str)

pms2 <- list(iterations = 1000,
            loss_function = "Logloss")

cs_mod <- catboost.train(called_str_pool, params = pms2)
