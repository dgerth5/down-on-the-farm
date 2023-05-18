library(baseballr)
library(tidyverse)
library(furrr)
library(readr)

leagues <- mlb_league(2022) 
dates <- data.frame(day = rep(seq(as.Date("2023-04-07"),as.Date("2023-05-16"), by = "days"), times = 1))
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

ev_perc <- ntile(ev$hitData.launchSpeed, 100)
ev_smry <- ev %>%
  group_by(matchup.batter.fullName) %>%
  summarise(ev = quantile(hitData.launchSpeed,0.95))

evv <- ev_smry %>%
  select(ev)  %>%
  mutate(perc = ntile(ev, 100)) %>%
  group_by(perc) %>%
  summarise(ev2 = mean(ev))

jett <- ev %>%
  filter(matchup.batter.fullName == "Jett Williams")

mean(jett$hitData.launchAngle)
sd(jett$hitData.launchAngle)

quantile(jett$hitData.launchSpeed, .95)


library(ggplot2)

jett_la <- jett %>%
  select(hitData.launchAngle)

summary(jett_la$hitData.launchAngle)

ggplot(jett_la, aes(x = hitData.launchAngle, fill = ..count..)) +
  geom_histogram(bins = 15, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(x = "Launch Angle", y = "Frequency", 
       title = "Jett Williams Launch Angle Distribution",
       subtitle = "Average Launch Angle: 14.6 degrees")