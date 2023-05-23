library(baseballr)
library(tidyverse)
library(furrr)
library(readr)

leagues <- mlb_league(2023) 
dates <- data.frame(day = rep(seq(as.Date("2023-03-30"),Sys.Date(), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map(function(x) mlb_game_pks(dates$day[x],
                                      level_ids = c(1,11))) 

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

# just horizontal and vertical release point, spin axis, extension, spin rate, and pitch speed 
# https://downonthefarm.substack.com/p/whos-throwing-the-wrong-fastball?utm_source=profile&utm_medium=reader2

library(readr) 
library(tidyverse)
#write_csv(ml_pbp, file = "mlb-triple-a-mvmt052023.csv")

ml_pbp2 <- read_csv("mlb-triple-a-mvmt052023.csv")
ml_pbp3 <- ml_pbp2 %>% 
  select(details.type.code,matchup.pitchHand.code, pitchData.coordinates.pfxX, pitchData.coordinates.pfxZ, home_team, pitchData.breaks.spinDirection, pitchData.breaks.spinRate, pitchData.extension, pitchData.startSpeed,pitchData.coordinates.x0, pitchData.coordinates.z0)
mlb2022_40ft <- read_csv("mlb2022-40ft.csv")
mlb2022_40ft2 <- mlb2022_40ft %>%
  select(details.type.code,matchup.pitchHand.code, pitchData.coordinates.pfxX, pitchData.coordinates.pfxZ, home_team, pitchData.breaks.spinDirection, pitchData.breaks.spinRate, pitchData.extension, pitchData.startSpeed,pitchData.coordinates.x0, pitchData.coordinates.z0)
milb_pbp_2022 <- read_csv("milb_pbp_2022.csv")
milb_pbp_20222 <- milb_pbp_2022 %>%
  select(details.type.code,matchup.pitchHand.code, pitchData.coordinates.pfxX, pitchData.coordinates.pfxZ, home_team, pitchData.breaks.spinDirection, pitchData.breaks.spinRate, pitchData.extension, pitchData.startSpeed,pitchData.coordinates.x0, pitchData.coordinates.z0)

ml_pbp <- rbind(ml_pbp3, mlb2022_40ft2, milb_pbp_20222)

mvmt_ff_df <- ml_pbp %>%
  filter(details.type.code == "FF") %>%
  mutate(pfx_x = if_else(matchup.pitchHand.code == "R", 0.4129406 + 1.6943550*pitchData.coordinates.pfxX, -1*(0.4129406 + 1.6943550*pitchData.coordinates.pfxX)),
         pfx_z = -0.2487306 + 1.7272317*pitchData.coordinates.pfxZ,
         adj_sd = if_else(matchup.pitchHand.code == "R", pitchData.breaks.spinDirection, 
                          ifelse(pitchData.breaks.spinDirection <= 180, 180 - pitchData.breaks.spinDirection, pitchData.breaks.spinDirection - 180)),
         adj_x0 = if_else(matchup.pitchHand.code == "R", pitchData.coordinates.x0, -1*pitchData.coordinates.x0)) %>%
  select(pfx_x, home_team, adj_sd, pitchData.breaks.spinRate, pitchData.extension, pitchData.startSpeed,adj_x0, pitchData.coordinates.z0) %>%
  drop_na()

summary(mvmt_ff_df)

library(readxl)
mlb_stadium_elevation <- read_excel("~/mlb-stadium-elevation.xlsx")

df_w_elev <- inner_join(mvmt_ff_df, mlb_stadium_elevation, by = c("home_team"="Team")) 

library(catboost)

train_pool <- catboost.load_pool(data = df_w_elev[,-c(1,2)],
                                 label = df_w_elev$pfx_x)

mod2 <- catboost.train(train_pool,
                       params = list(
                         iterations = 1000,
                         loss_function = "RMSE",
                         depth = 6,
                         custom_metric = list('RMSE'),
                         eval_metric = 'RMSE',
                         random_seed = 123,
                         od_type = 'Iter',
                         metric_period = 50,
                         od_wait = 20))

# mvmt_ff_df_r <- ml_pbp %>%
#   filter(details.type.code == "FF") %>%
#   filter(matchup.pitchHand.code == "R") %>%
#   mutate(pfx_x = 0.4129406 + 1.6943550*pitchData.coordinates.pfxX,
#          pfx_z = -0.2487306 + 1.7272317*pitchData.coordinates.pfxZ) %>%
#   select(pitchData.breaks.spinDirection, pitchData.breaks.spinRate, pitchData.extension, pitchData.startSpeed,pitchData.coordinates.x0, pitchData.coordinates.z0) %>%
#   drop_na()

# summary(mvmt_ff_df_r)

# helsley <- mvmt_ff_df %>%
#   filter(matchup.pitcher.fullName == "Ryan Helsley") %>%
#   summarise(mean_sd = mean(pitchData.breaks.spinDirection), 
#             mean_sr = mean(pitchData.breaks.spinRate), 
#             mean_ext = mean(pitchData.extension), 
#             mean_velo = mean(pitchData.startSpeed), 
#             mean_x0 = mean(pitchData.coordinates.x0), 
#             mean_z0 = mean(pitchData.coordinates.z0))
# 
# head(helsley)



p_df <- data.frame(adj_sd = rep(197,60),
                   pitchData.breaks.spinRate = rep(2659,60),
                   pitchData.extension = rep(6.63,60),
                   pitchData.startSpeed = rep(99.6,60),
                   adj_x0 = rep(-0.252,60),
                   pitchData.coordinates.z0 = rep(5.85,60),
                   Elevation = mlb_stadium_elevation$Elevation)

p_df_pool <- catboost.load_pool(p_df)

p_df$p_ivb <- catboost.predict(mod2, p_df_pool) 
p_df <- p_df %>% arrange(Elevation)

plot(p_df$Elevation, p_df$p_ivb, type = "l",
     main = "Predicted IVB: Pitch Metrics Held Constant, Elevation Varies")

?

# elevation table
library(gt)
library(readxl)
mlb_triplea_elev <- read_excel("~/mlb-triplea-elev.xlsx") %>%
  mutate(Elevation_Diff = `Elevation...2` - `Elevation...5`) %>%
  arrange(Elevation_Diff)

gt(mlb_triplea_elev) %>%
  tab_header(title = md("**MLB vs. Triple A Farm Team Elevation**")) %>%
  cols_label(`Elevation...2` = "Elevation",
             `Elevation...5` = "Elevation") %>%
  cols_align(align = c("center"))

med_elev <- mlb_triplea_elev %>%
  group_by(`AAA League`) %>%
  summarise(med_elev = median(`Elevation...5`),
            med_elev2 = median(Elevation_Diff))

gt(med_elev) %>%
  tab_header(title = md("**Triple A Elevation By League**")) %>%
  cols_label(`AAA League` = "League",
             med_elev = "Median Elevation",
             med_elev2 = "MLB to AAA Median Elevation Change") %>%
  cols_align(align = c("center"))
