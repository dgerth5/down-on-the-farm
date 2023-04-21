library(ggplot2)
library(baseballr)
library(tidyverse)
library(furrr)

leagues <- mlb_league(2022) 
dates <- data.frame(day = rep(seq(as.Date("2023-04-20"),as.Date("2023-04-20"), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map(function(x) mlb_game_pks(dates$day[x],
                                      level_ids = 11))

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

bradford <- ml_pbp %>%
  filter(matchup.pitcher.fullName == "Cody Bradford") %>%
  mutate(facet = "Cody Bradford") %>%
  select(details.type.code, pitchData.coordinates.pfxX, pitchData.coordinates.pfxZ, facet) %>%
  rename(pitch_type = details.type.code,
         pfx_x = pitchData.coordinates.pfxX,
         pfx_z = pitchData.coordinates.pfxZ) %>%
  drop_na()

harrison <- ml_pbp %>%
  filter(matchup.pitcher.fullName == "Kyle Harrison") %>%
  mutate(facet = "Kyle Harrison") %>%
  select(details.type.code, pitchData.coordinates.pfxX, pitchData.coordinates.pfxZ, facet) %>%
  rename(pitch_type = details.type.code,
         pfx_x = pitchData.coordinates.pfxX,
         pfx_z = pitchData.coordinates.pfxZ) %>%
  drop_na()

df <- rbind(bradford, harrison)
ggplot(df, aes(x = pfx_x, y = pfx_z, color = pitch_type)) +
  geom_point() +
  ggtitle("Matchup of the Day: Cody Bradford vs. Kyle Harrison") +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  xlim(-15,15) + ylim(-15,15) +
  xlab("Horizontal Mvmt") + ylab("Vertical Mvmt") +
  facet_wrap(~facet, ncol = 2, scales = "fixed") +
  labs(facet_bradford = "Cody Bradford",
       facet_harrison = "Kyle Harrison")