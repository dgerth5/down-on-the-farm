library(readr)
library(tidyverse)
mlb2022_40ft <- read_csv("mlb2022-40ft.csv")

unique(mlb2022_40ft$details.call.description)

swing_event <- c("In play, out(s)","Swinging Strike","In play, run(s)","In play, no out","Foul",
                 "Foul Tip","Foul Bunt","Swinging Strike (Blocked)", "Missed Bunt")

mlb2022_mvmt <- mlb2022_40ft %>%
  filter(type == "pitch") %>%
  mutate(swing = if_else(details.call.description %in% swing_event, 1, 0),
         chase_zone = if_else(pitchData.zone > 9, 1, 0),
         chase_swing = if_else(swing == 1 & chase_zone == 1, 1, 0),
         pfx_x = 0.4129406 + 1.6943550*pitchData.coordinates.pfxX,
         pfx_z = -0.2487306 + 1.7272317*pitchData.coordinates.pfxZ)  %>%
  select(swing, chase_zone, chase_swing, pfx_x, pfx_z, pitchData.coordinates.x, pitchData.coordinates.y, pitchData.zone,
         matchup.batter.fullName, pitchData.breaks.spinRate, pitchData.extension, count.strikes.start, home_team,
         pitchData.coordinates.pX, pitchData.coordinates.pZ, matchup.batSide.code)

mlb2022_mvmt$count.strikes.start <- as.factor(mlb2022_mvmt$count.strikes.start)
mlb2022_mvmt$matchup.batSide.code <- as.factor(mlb2022_mvmt$matchup.batSide.code)

mean(mlb2022_mvmt$swing)


library(readxl)
mlb_triplea_elev <- read_excel("~/mlb-triplea-elev.xlsx")
elev <- data.frame(team = c(mlb_triplea_elev$`MLB Team`, mlb_triplea_elev$`AAA Team`),
                   elev = c(mlb_triplea_elev$Elevation...2, mlb_triplea_elev$Elevation...5))

combin1 <- left_join(mlb2022_mvmt, elev, by = c("home_team"="team")) %>% drop_na()

library(mgcv)

m0 <- bam(swing ~ s(pitchData.coordinates.x, pitchData.coordinates.y) +
          + s(pfx_x, pfx_z, elev) + s(pitchData.coordinates.pX, pitchData.coordinates.pZ) 
          + pitchData.breaks.spinRate + pitchData.extension + count.strikes.start,
          data = combin1, 
          family = binomial(),
          nthreads = 4,
          discrete = TRUE)

summary(m0)

combin1$p <- predict(m0, combin1, type = "response")
combin1$p0 <- rep(0, length(combin1$p))
combin1$p_dec <- ifelse(combin1$p > .5, 1, 0)

summary(combin1$p)
MLmetrics::LogLoss(combin1$p, combin1$swing)
MLmetrics::Accuracy(combin1$p_dec, combin1$swing)
MLmetrics::Accuracy(combin1$p, combin1$swing)

summary(combin1$swing)

library(pROC)

roc_obj <- roc(combin1$swing, combin1$p)

plot(roc_obj, main="ROC Curve")

# Add AUC in legend
legend("bottomright", legend=paste("AUC =", round(auc(roc_obj), 2)))

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

milb_pbp <- 1:length(ml_game_pks) %>%
  furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
  purrr::map("result") %>%
  bind_rows()

milb_pbp <- milb_pbp %>% as.data.frame()


unique(milb_pbp$pitchData.zone)

milb2023 <- milb_pbp %>%
  filter(isPitch == TRUE) %>%
  mutate(swing = if_else(details.call.description %in% swing_event, 1, 0),
         chase_zone = if_else(pitchData.zone > 9, 1, 0),
         chase_swing = if_else(swing == 1 & chase_zone == 1, 1, 0),
         pfx_x = 0.4129406 + 1.6943550*pitchData.coordinates.pfxX,
         pfx_z = -0.2487306 + 1.7272317*pitchData.coordinates.pfxZ)  %>%
  select(swing, chase_zone, chase_swing, pfx_x, pfx_z, pitchData.coordinates.x, pitchData.coordinates.y, pitchData.zone,
         matchup.batter.fullName, pitchData.breaks.spinRate, pitchData.extension, count.strikes.start, home_team,
         pitchData.coordinates.pX, pitchData.coordinates.pZ, matchup.batSide.code, matchup.batter.fullName, batting_team) %>%
  drop_na()


milb2023$count.strikes.start <- as.factor(milb2023$count.strikes.start)
milb2023$matchup.batSide.code <- as.factor(milb2023$matchup.batSide.code)
combin2 <- left_join(milb2023, elev, by = c("home_team"="team")) %>% drop_na()

combin2$p <- predict(m0, combin2, type = "response")

# get prob of chase swing btw mlb and aaa

cmlb <- combin1 %>% filter(chase_zone == 1) 
mean(cmlb$p)
mean(cmlb$swing)
caaa <- combin2 %>% filter(chase_zone == 1)
mean(caaa$p)

croe_smry <- combin2 %>%
  group_by(matchup.batter.fullName, batting_team) %>%
  filter(chase_zone == 1) %>%
  summarise(mean_chase = mean(swing),
            mean_p = mean(p),
            n = n()) %>%
  mutate(croe = round(mean_p - mean_chase,3))

summary(croe_smry$n)
summary(croe_smry$croe)

ovr_swing <- combin2 %>%
  group_by(matchup.batter.fullName) %>%
  summarise(mean_swing = mean(swing)) %>%
  ungroup() %>%
  mutate(swing_per = round(percent_rank(mean_swing),2)*100)

croe_w_swing <- left_join(croe_smry, ovr_swing, by = "matchup.batter.fullName")

just_omaha <- croe_w_swing %>% filter(batting_team == "Omaha Storm Chasers")

g50 <- croe_w_swing %>% filter(swing_per > 50)

write_csv(just_omaha, "just_omaha.csv")

library(gt)

tbl <- croe_w_swing %>%
  select(matchup.batter.fullName, mean_chase, croe, swing_per) %>%
  arrange(croe) %>%
  slice(1:10)

gt(tbl) %>%
  tab_header(title = md("**Top 10 CROE**"),
             subtitle = md("Season: 2023")) %>%
  fmt_percent(c("mean_chase","croe"), decimals = 1) %>%
  cols_label(matchup.batter.fullName = "Name",
             mean_chase = "Chase%",
             croe = "CROE",
             swing_per = "Swing Percentile")
