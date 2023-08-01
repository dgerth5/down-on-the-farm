library(baseballr)
library(tidyverse)
library(furrr)
library(readr)

leagues <- mlb_league(2023) 
dates <- data.frame(day = rep(seq(as.Date("2023-04-06"),Sys.Date(), by = "days"), times = 1))
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

ml_pbp <- 1:length(ml_game_pks) %>%
  furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
  purrr::map("result") %>%
  bind_rows()

ml_pbp <- ml_pbp %>% as.data.frame()

ev <- ml_pbp %>%
  drop_na(hitData.launchSpeed)

ev_smry <- ev %>%
  group_by(matchup.batter.fullName) %>%
  summarise(ev = quantile(hitData.launchSpeed,0.95))

ev_smry$ev_rank <- percent_rank(ev_smry$ev)

library(readxl)
termarr_rolling <- read_excel("~/termarr-rolling.xlsx")

# Load ggplot2 package
library(ggplot2)

termarr_rolling$Date <- as.Date(termarr_rolling$Date)

# Create the plot
ggplot(termarr_rolling, aes(x = Date)) +
  geom_line(aes(y = RollingBB, color = "BB%")) +
  geom_line(aes(y = RollingK, color = "K%")) +
  scale_color_manual(values = c("BB%" = "blue", "K%" = "red")) +
  labs(x = "Date", y = "Value", color = "Legend", 
       title = "Termarr Johnson 2023 Rolling BB% and K%") 

termarr <- ev %>%
  filter(matchup.batter.fullName == "Termarr Johnson")

termarr_la <- termarr %>%
  select(hitData.launchAngle)

summary(termarr_la$hitData.launchAngle)

ggplot(termarr_la, aes(x = hitData.launchAngle, fill = ..count..)) +
  geom_histogram(bins = 15, color = "black") +
  scale_fill_gradient(low = "white", high = "black") +
  labs(x = "Launch Angle", y = "Frequency", 
       title = "Termarr Johnson Launch Angle Distribution",
       subtitle = "Average Launch Angle: 9.6 degrees")

# in zone contact rate

unique(ml_pbp$hitData.hardness)

unique(z_con$details.call.description)

se <- c("In play, out(s)","Swinging Strike","Foul","In play, run(s)","In play, no out",
        "Foul Tip","Foul Bunt","Swinging Strike (Blocked)","Foul Pitchout", "Missed Bunt" )

we <- c("Swinging Strike","Swinging Strike (Blocked)", "Missed Bunt" )

z_con <- ml_pbp %>%
  filter(isPitch == TRUE) %>%
  filter(pitchData.zone < 10) %>%
  mutate(swing = if_else(details.call.description %in% se, 1, 0),
         whiff = if_else(details.call.description %in% we, 1, 0)) %>%
  filter(swing == 1) %>%
  group_by(matchup.batter.fullName) %>%
  summarise(zone_whiff = mean(whiff),
            n = n())

z_con$zw_per <- percent_rank(z_con$zone_whiff)

library(lubridate)

sw <- ml_pbp %>%
  filter(isPitch == TRUE) %>%
  #filter(pitchData.zone < 10) %>%
  mutate(swing = if_else(details.call.description %in% se, 1, 0),
         whiff = if_else(details.call.description %in% we, 1, 0),
         month = month(game_date)) %>%
  #filter(month > 5) %>%
  group_by(matchup.batter.fullName) %>%
  summarise(swing_rate = mean(swing),
            n = n())

sw$sw_per <- percent_rank(sw$swing_rate)


# pre-draft table
library(gt)

df <- data.frame(Name = c("Jackson Holliday", "Druw Jones", "Termarr Johnson", "Elijah Greene"),
                 Whiff = c(.2,.2,.26,.32),
                 Chase = c(.15,.21,.22,.27),
                 Pitches = c(515,450,657,530))  

gt(df) %>%
  fmt_percent(c("Whiff","Chase"), decimals = 0)
