library(baseballr)
library(tidyverse)
library(furrr)
library(readr)

leagues <- mlb_league(2023) 
dates <- data.frame(day = rep(seq(as.Date("2023-04-06"),Sys.Date(), by = "days"), times = 1))
#dates <- data.frame(day = rep(seq(as.Date("2023-08-15"),as.Date("2023-08-15"), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map(function(x) mlb_game_pks(dates$day[x],
                                      level_ids = 14))

ml_game_pks <- minor_league_game_pk_lst %>%
  bind_rows() %>%
  dplyr::filter(status.codedGameState == "F",
                !is.na(game_pk)) %>%
  pull(game_pk)

plan("multisession", workers = 4)

safe_pbp <- safely(mlb_pbp)

milb_pbp <- 1:length(ml_game_pks) %>%
  furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
  purrr::map("result") %>%
  bind_rows()

milb_pbp <- milb_pbp %>% as.data.frame()

# just pitches
# look at termarr.R if whiff needed

pbp2 <- milb_pbp %>% 
  filter(isPitch == TRUE) %>%
  filter(home_league_name == "Florida State League") %>%
  select(details.description, matchup.pitchHand.code, matchup.batter.fullName, pitchData.zone, count.strikes.start, count.balls.start, details.type.code,
         pitchData.startSpeed, pitchData.breaks.breakHorizontal, pitchData.breaks.breakVerticalInduced,
         pitchData.extension, pitchData.coordinates.x0, pitchData.coordinates.z0,
         pitchData.coordinates.x, pitchData.coordinates.y, hitData.launchAngle, hitData.launchSpeed, batting_team, matchup.batter.id) %>%
  mutate(se = if_else(details.description %in% c("In play, out(s)","Swinging Strike","Foul","In play, run(s)","In play, no out",
                                                 "Foul Tip","Foul Bunt","Swinging Strike (Blocked)","Foul Pitchout", "Missed Bunt"), 1, 0),
         chase_zone = if_else(pitchData.zone > 9, 1, 0),
         pfx_xadj = if_else(matchup.pitchHand.code == "R", pitchData.breaks.breakHorizontal, pitchData.breaks.breakHorizontal*-1 ),
         relx_ajd = if_else(matchup.pitchHand.code == "R", pitchData.coordinates.x0, pitchData.coordinates.x0*-1))
pbp2$count.strikes.start <- as.factor(pbp2$count.strikes.start)

library(catboost)

train_pool <- catboost.load_pool(data = pbp2[,-c(1:4,6:7,10,12,16:21)],
                                 label = pbp2$se)

mod <- catboost.train(train_pool,
                      params = list(
                        loss_function = "Logloss",
                        eval_metric = "Logloss",
                        iterations = 1000))

pbp2$predicted_probs <- catboost.predict(mod,train_pool,prediction_type = "Probability")

MLmetrics::LogLoss(pbp2$predicted_probs, pbp2$se)

# croe

croe <- pbp2 %>%
  filter(chase_zone == 1) %>%
  group_by(matchup.batter.fullName, matchup.batter.id) %>%
  summarise(xchase = mean(predicted_probs),
            chase = mean(se),
            n = n()) %>%
  mutate(croe = round(chase - xchase,3)) # lower / better

croe$croe_pr <- percent_rank(croe$croe)

# 90th ev
ev90 <- pbp2 %>%
  filter(is.na(hitData.launchSpeed) == FALSE) %>%
  group_by(matchup.batter.fullName) %>%
  summarise(ev90 = quantile(hitData.launchSpeed,0.90),
            n = n()) %>%
  mutate(ev90_per = percent_rank(ev90)*100)

# dhh

ev80 <- pbp2 %>%
  filter(is.na(hitData.launchSpeed) == FALSE) %>%
  group_by(hitData.launchAngle) %>%
  summarise(ev80 = quantile(hitData.launchSpeed,0.8))

# smooth out
library(mgcv)
m1 <- bam(ev80 ~ s(hitData.launchAngle), data = ev80)
ev80$smooth_ev <- predict(m1, ev80)

plot(ev80$hitData.launchAngle, ev80$ev80, type = "l")
lines(ev80$hitData.launchAngle, ev80$smooth_ev, col = "red")

dhh1 <- left_join(pbp2, ev80, by = "hitData.launchAngle")

dhh_smry <- dhh1 %>%
  filter(is.na(hitData.launchSpeed) == FALSE) %>%
  mutate(dhh = if_else(hitData.launchSpeed > smooth_ev, 1, 0)) %>%
  group_by(matchup.batter.fullName) %>%
  summarise(dhh = mean(dhh),
            n = n()) %>%
  mutate(dhh_per = percent_rank(dhh)*100)

# sweet spot percentage
ss <- pbp2 %>%
  filter(is.na(hitData.launchAngle) == FALSE) %>%
  mutate(sweetspot = if_else(hitData.launchAngle <= 32 & hitData.launchAngle >= 8, 1, 0)) %>%
  group_by(matchup.batter.fullName) %>%
  summarise(ss = mean(sweetspot),
            n = n()) %>%
  mutate(ss_per = percent_rank(ss)*100)

# whiff rate
wh <- pbp2 %>%
  mutate(we = if_else(details.description %in% c("Swinging Strike","Swinging Strike (Blocked)", "Missed Bunt" ), 1, 0)) %>%
  filter(se == 1) %>%
  group_by(matchup.batter.fullName) %>%
  summarise(whiff_rate = mean(we),
            n = n()) %>%
  mutate(wh_per = 100 - 100*percent_rank(whiff_rate))

# swing rate
sr <- pbp2  %>%
  group_by(matchup.batter.fullName) %>%
  summarise(swing_rate = mean(se),
            pitches_seen = n()) %>%
  mutate(sw_per = percent_rank(swing_rate)*100)

merged_df <- croe %>%
  left_join(ev90, by = "matchup.batter.fullName") %>%
  left_join(dhh_smry, by = "matchup.batter.fullName") %>%
  left_join(ss, by = "matchup.batter.fullName") %>%
  left_join(wh, by = "matchup.batter.fullName") %>%
  left_join(sr, by = "matchup.batter.fullName") %>%
  select(matchup.batter.fullName, matchup.batter.id, croe, croe_pr, ev90, ev90_per, dhh, dhh_per, ss, ss_per, whiff_rate, wh_per, swing_rate, sw_per, pitches_seen) %>%
  drop_na()

write_csv(merged_df, "fsl-batted-ball-0925.csv")


filt <- merged_df %>%
  filter(croe_pr <= .4) %>%
  filter(ev90_per >= 80) %>%
  #filter(dhh >= .5) %>%
#  filter(ss_per <= 50) %>%
  filter(wh_per >= 30) %>%
  filter(sw_per >= 30) %>%
  filter(pitches_seen > 400) 

# get fangraphs id

razzball <- read_csv("C:/Users/david/Downloads/razzball.csv")
r2 <- razzball %>%
  select(MLBAMID, FangraphsID)

md3 <- left_join(merged_df, r2, by = c("matchup.batter.id"="MLBAMID"))


fangraphs_minor_league_leaders_29_ <- read_csv("C:/Users/david/Downloads/fangraphs-minor-league-leaders (29).csv")

summary(merged_df$pitches_seen)

nm <- c("Cam Collier", "Won-Bin Cho", "Ricardo Olivar", "Brenny Escanio", "Gabriel Rincones Jr.")

md2 <- merged_df %>%
  filter(matchup.batter.fullName %in% nm) %>%
  mutate(croe_2 = croe*-1)

md2$team = c("Yankees", "Reds", "Phillies", "Twins", "Cardinals")

m3 <- mlbplotR::load_mlb_teams() %>% select(team_mascot, team_logo_espn)

md2 <- md2 %>%
  left_join(m3, by = c("team" = "team_mascot")) %>%
  select(matchup.batter.fullName, team_logo_espn, croe_2, ev90, dhh, ss, whiff_rate) %>%
  ungroup() 

library(gt)
library(gtExtras)
  
g2 <- md2 %>%
  arrange(matchup.batter.fullName) %>%
  gt() %>%
  tab_header(title = md("**FSL Leaders**")) %>%
  fmt_number("ev90", decimals = 1) %>%
  fmt_percent(columns = c("croe_2","dhh","ss","whiff_rate"), decimals = 1) %>%
  gt_img_rows(team_logo_espn, height = 35) %>%
  cols_label(matchup.batter.fullName = "Name",
             team_logo_espn = "",
             croe_2 = "CROE",
             ev90 = "EV90",
             dhh = "DHH%",
             ss = "SS%",
             whiff_rate = "Whiff%")  

gtsave(g2, "fsl-plot-925.png")

m2 <- merged_df %>%
  select(matchup.batter.fullName, croe, ev, dhh_mean, ss_mean, whiff_rate) %>% 
  mutate(across(c(croe, ev, dhh_mean, ss_mean, whiff_rate), ~ ( . - mean(.)) / sd(.))) 

write_csv(milb_pbp, "fsl2023.csv")

### overall
# swing decision: croe
# power: 90th ev
# batted ball quality: dhh , ss
# contact: whiff%