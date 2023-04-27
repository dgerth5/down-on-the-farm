library(ggplot2)
library(baseballr)
library(tidyverse)
library(furrr)

leagues <- mlb_league(2022) 
dates <- data.frame(day = rep(seq(as.Date("2023-04-25"),as.Date("2023-04-25"), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map_df(function(x) mlb_game_pks(dates$day[x],
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
  filter(matchup.pitcher.fullName == "Robbie Erlin") %>%
  mutate(facet = "Robbie Erlin",
         adj_x = 1.443 + 1.681*pitchData.coordinates.pfxX - 0.012*pitchData.startSpeed,
         adj_z = 2.268 + 1.753*pitchData.coordinates.pfxZ - 0.030*pitchData.startSpeed) %>%
  select(details.type.code, adj_x, adj_z, 
         pitchData.coordinates.pX, pitchData.coordinates.pZ,
         facet) %>%
  rename(pitch_type = details.type.code,
         pfx_x = adj_x,
         pfx_z = adj_z,
         pX = pitchData.coordinates.pX,
         pZ = pitchData.coordinates.pZ) 

harrison <- ml_pbp %>%
  filter(matchup.pitcher.fullName == "Kyle Harrison") %>%
  mutate(facet = "Kyle Harrison",
         adj_x = 1.443 + 1.681*pitchData.coordinates.pfxX - 0.012*pitchData.startSpeed,
         adj_z = 2.268 + 1.753*pitchData.coordinates.pfxZ - 0.030*pitchData.startSpeed) %>%
  select(details.type.code, adj_x, adj_z, 
         pitchData.coordinates.pX, pitchData.coordinates.pZ,
         facet) %>%
  rename(pitch_type = details.type.code,
         pfx_x = adj_x,
         pfx_z = adj_z,
         pX = pitchData.coordinates.pX,
         pZ = pitchData.coordinates.pZ)

df <- rbind(bradford, harrison) %>% drop_na()
ggplot(df, aes(x = pfx_x, y = pfx_z, color = pitch_type)) +
  geom_point() +
  ggtitle("Robbie Erlin vs. Kyle Harrison") +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point(size = 4, shape = 18) +
  xlim(-21,21) + ylim(-21,21) +
  xlab("Horizontal Mvmt") + ylab("Vertical Mvmt") +
  facet_wrap(~facet, ncol = 2, scales = "fixed") +
  labs(facet_erlin = "Robbie Erlin",
       facet_harrison = "Kyle Harrison")

# location
ggplot(df, aes(x = pX, y = pZ, color = pitch_type)) +
  geom_point() +
  ggtitle("Robbie Erlin vs. Kyle Harrison") +
  geom_rect(xmin = -0.75, xmax = 0.75, ymin = 1.75, ymax = 3.75, fill = "transparent", color = "black") +
  xlim(-3,3) + ylim(0,5) +
  xlab("X Location") + ylab("Y Location") +
  facet_wrap(~facet, ncol = 2, scales = "fixed") +
  labs(facet_erlin = "Robbie Erlin",
       facet_harrison = "Kyle Harrison")
