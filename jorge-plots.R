library(baseballr)
library(tidyverse)
library(furrr)
library(readr)

leagues <- mlb_league(2023) 
dates <- data.frame(day = rep(seq(as.Date("2023-04-07"),Sys.Date(), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map(function(x) mlb_game_pks(dates$day[x],
                                      level_ids = 14))

ml_game_pks <- minor_league_game_pk_lst %>%
  bind_rows() %>%
  dplyr::filter(status.codedGameState == "F",
                !is.na(game_pk)) %>%
  pull(game_pk)

plan("multisession", workers = 3)

safe_pbp <- safely(mlb_pbp)

ml_pbp <- 1:length(ml_game_pks) %>%
  furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
  purrr::map("result") %>%
  bind_rows()

ml_pbp <- ml_pbp %>% as.data.frame()

# ev / la

ev <- ml_pbp %>%
  drop_na(hitData.launchSpeed)

ev_perc <- ntile(ev$hitData.launchSpeed, 100)
ev_smry <- ev %>%
  group_by(matchup.batter.fullName) %>%
  summarise(ev = quantile(hitData.launchSpeed,0.95))

evv <- ev_smry %>%
  select(ev)  %>%
  mutate(perc = ntile(ev, 100)) %>%
  group_by(perc) %>%
  summarise(ev2 = mean(ev))

tightness <- ev %>%
  group_by(matchup.batter.fullName) %>%
  summarise(sd_ev = sd(hitData.launchAngle))

summary(tightness$sd_ev)

jorge <- ev %>%
  filter(matchup.batter.fullName == "Carlos Jorge")

jorge_la <- jorge %>%
  select(hitData.launchAngle)

sd(jorge_la$hitData.launchAngle)

summary(jorge_la$hitData.launchAngle)

ggplot(jorge_la, aes(x = hitData.launchAngle, fill = ..count..)) +
  geom_histogram(bins = 15, color = "black") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Launch Angle", y = "Frequency", 
       title = "Carlos Jorge Launch Angle Distribution",
       subtitle = "Average Launch Angle: 26.16 degrees")


df <- data.frame(Level = c("R", "A"),
                 Pull = c(.372,.462),
                 Cent = c(.103,.264),
                 Oppo = c(.526,.274))

library(gt)

gt(df) %>%
  tab_header(title = md("**Carlos Jorge Spray Distribution**"),
             subtitle = md("Data: FanGraphs")) %>%
  fmt_percent(columns = c("Pull","Cent","Oppo"), decimals = 1) %>%
  cols_align(align = "center")
