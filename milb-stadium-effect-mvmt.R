library(tidyverse)
library(baseballr)
library(lubridate)
library(purrr)
library(furrr)

# get data

leagues = mlb_league(2023)
leagues2 = leagues %>%
  select(league_id, sport_id, league_name, season_date_info_regular_season_start_date, season_date_info_regular_season_end_date)

dates <- data.frame(day = rep(seq(as.Date("2023-03-30"),Sys.Date(), by = "days"), times = 1))
minor_league_game_pks_lst <- 1:nrow(dates) %>%
  purrr::map_df(function(x) mlb_game_pks(dates$day[x],
                                          level_ids = c(1,11,14)))

ml_game_pks <- minor_league_game_pks_lst %>%
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

library(tidyverse)
library(readr)

#write_csv(ml_pbp, "mlb-AAA-FSL-statcast.csv")

ml_pbp <- read_csv("mlb2022-40ft.csv")

pbp2 <- ml_pbp %>%
  filter(isPitch == TRUE) %>%
  filter(game_date < "2022-05-03") %>%
  mutate(adjz = -0.25 + 1.73*pitchData.coordinates.pfxZ,
         adjx = if_else(matchup.pitchHand.code == "R", .41 + 1.69*pitchData.coordinates.pfxX, -1*(.41 + 1.69*pitchData.coordinates.pfxX)),
         adjrelx = if_else(matchup.pitchHand.code == "R", pitchData.coordinates.x0, pitchData.coordinates.x0*-1),
         adjspindir = if_else(matchup.pitchHand.code == "R", pitchData.breaks.spinDirection, 360 - pitchData.breaks.spinDirection)) %>%
  select(adjz, adjx, adjrelx, pitchData.coordinates.z0, pitchData.extension, pitchData.breaks.spinRate, adjspindir, pitchData.startSpeed, matchup.pitcher.id, home_team, home_league_name, home_level_name,details.type.code) %>%
  drop_na() %>%
  mutate_at(c("matchup.pitcher.id", "home_team"), factor) %>%
  filter(details.type.code == "FF") %>%
  filter(home_level_name == "Major League Baseball") 

unique(pbp2$home_level_name)

library(mgcv)

mod_hmov <- bam(adjx ~ s(adjrelx, pitchData.coordinates.z0, k = 100) + adjspindir + pitchData.extension + pitchData.breaks.spinRate + pitchData.startSpeed + 
                  s(matchup.pitcher.id, bs = "re") + s(home_team, bs = "re"),
                discrete = TRUE,
                nthreads = 4,
                data = pbp2)

library(mixedup)
home_h <- extract_random_effects(mod_hmov) %>%
  filter(group_var == "home_team") %>%
  mutate(adj_val = value*-1) %>%
  select(group, adj_val) %>%
  rename(Park = group, Stadium_Effect = adj_val)


asdf <- inner_join(home_h, home_h2, by = "Park") %>%
  rename(Stadium_Effect22 = "Stadium_Effect.x",
         Stadium_Effect23 = "Stadium_Effect.y")

cor(home_h$Stadium_Effect, home_h2$Stadium_Effect)

m <- lm(Stadium_Effect23 ~ Stadium_Effect22, data = asdf)
summary(m)
