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
library(lubridate)
library(tidyverse)
#write_csv(ml_pbp, file = "mlb-triple-a-mvmt052023.csv")

ml_pbp2 <- read_csv("mlb-triple-a-mvmt052023.csv")
ml_pbp3 <- ml_pbp2 %>% 
  select(details.type.code, game_date,matchup.pitchHand.code, matchup.pitcher.fullName, pitchData.coordinates.pfxX, pitchData.coordinates.pfxZ, home_team, pitchData.breaks.spinDirection, pitchData.breaks.spinRate, pitchData.extension, pitchData.startSpeed,pitchData.coordinates.x0, pitchData.coordinates.z0)
mlb2022_40ft <- read_csv("mlb2022-40ft.csv")
mlb2022_40ft2 <- mlb2022_40ft %>%
  select(details.type.code, game_date,matchup.pitchHand.code, matchup.pitcher.fullName, pitchData.coordinates.pfxX, pitchData.coordinates.pfxZ, home_team, pitchData.breaks.spinDirection, pitchData.breaks.spinRate, pitchData.extension, pitchData.startSpeed,pitchData.coordinates.x0, pitchData.coordinates.z0)
milb_pbp_2022 <- read_csv("milb_pbp_2022.csv")
milb_pbp_20222 <- milb_pbp_2022 %>%
  select(details.type.code, game_date,matchup.pitchHand.code, matchup.pitcher.fullName, pitchData.coordinates.pfxX, pitchData.coordinates.pfxZ, home_team, pitchData.breaks.spinDirection, pitchData.breaks.spinRate, pitchData.extension, pitchData.startSpeed,pitchData.coordinates.x0, pitchData.coordinates.z0)

ml_pbp <- rbind(ml_pbp3, mlb2022_40ft2, milb_pbp_20222)

mvmt_ff_df <- ml_pbp %>%
  filter(details.type.code == "FF") %>%
  mutate(pfx_x = if_else(matchup.pitchHand.code == "R", 0.4129406 + 1.6943550*pitchData.coordinates.pfxX, -1*(0.4129406 + 1.6943550*pitchData.coordinates.pfxX)),
         pfx_z = -0.2487306 + 1.7272317*pitchData.coordinates.pfxZ,
         adj_sd = if_else(matchup.pitchHand.code == "R", pitchData.breaks.spinDirection, 360 - pitchData.breaks.spinDirection),
         adj_x0 = if_else(matchup.pitchHand.code == "R", pitchData.coordinates.x0, -1*pitchData.coordinates.x0)) %>%
  select(pfx_z, home_team, matchup.pitcher.fullName, adj_sd, pitchData.breaks.spinRate, pitchData.extension, pitchData.startSpeed,adj_x0, pitchData.coordinates.z0) %>%
  mutate_at(c("home_team"), factor) %>%
  drop_na()


library(readxl)
mlb_stadium_elevation <- read_excel("~/mlb-stadium-elevation.xlsx")

df_w_elev <- inner_join(mvmt_ff_df, mlb_stadium_elevation, by = c("home_team"="Team")) 

library(mgcv)

mod1 <- bam(pfx_z ~ s(adj_sd) + pitchData.breaks.spinRate + pitchData.extension + pitchData.startSpeed + s(adj_x0, pitchData.coordinates.z0) + s(Elevation),
            data = df_w_elev,
            nthreads = 4,
            discrete = TRUE)

saveRDS(mod1, "elev_mod.rds")

summary(mod1)


p_df <- data.frame(Home_Team = mlb_stadium_elevation$Team,
                   adj_sd = rep(212.6,60),
                   pitchData.breaks.spinRate = rep(2262,60),
                   pitchData.extension = rep(6.374,60),
                   pitchData.startSpeed = rep(93.39,60),
                   adj_x0 = rep(-1.587,60),
                   pitchData.coordinates.z0 = rep(5.739,60),
                   Elevation = mlb_stadium_elevation$Elevation)

head(cecconi)

p_df$bam_p <- predict(mod1, p_df[,-1])

p_df2 <- p_df %>% arrange(Elevation) 

plot(p_df2$Elevation, p_df2$bam_p, type = "l",
     main = "Elevation Effects on IVB",
     xlab = "Elevation", ylab = "xIVB")


cecconi <- mvmt_ff_df %>%
  filter(matchup.pitcher.fullName == "Slade Cecconi") %>%
  summarise(mean_sd = mean(adj_sd),
            mean_sr = mean(pitchData.breaks.spinRate),
            mean_ext = mean(pitchData.extension),
            mean_velo = mean(pitchData.startSpeed),
            mean_x0 = mean(adj_x0),
            mean_z0 = mean(pitchData.coordinates.z0),
            mean_mvmt = mean(pfx_z)) %>%
  rename(adj_sd = mean_sd ,
         pitchData.breaks.spinRate = mean_sr,
         pitchData.extension = mean_ext,
         pitchData.startSpeed = mean_velo,
         adj_x0 = mean_x0,
         pitchData.coordinates.z0 = mean_z0)

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

mlb_tripa_avgz <- read_excel("~/mlb_tripa_avgz.xlsx")

gt(mlb_tripa_avgz) %>%
  tab_header(title = md("**MLB vs. Triple A Farm Team xIVB**")) %>%
  fmt_number(columns = c(`xIVB...3`,`xIVB...7`, `xIVB Diff`)) %>%
  cols_label(`Elevation...2` = "Elevation",
             `xIVB...3` = "xIVB",
             `Elevation...6` = "Elevation",
             `xIVB...7` = "xIVB")

gt(cecconi) %>%
  tab_header(title = md("**Slade Cecconi FF Pitch Characteristics**"),
             subtitle = md("Season: 2023")) %>%
  cols_label(adj_sd = "Spin Direction",
             pitchData.breaks.spinRate = "Spin Rate",
             pitchData.extension = "Extension",
             pitchData.startSpeed = "Velocity",
             adj_x0 = "Horizontal Release Point",
             pitchData.coordinates.z0 = "Vertical Release Point",
             mean_mvmt = "Average IVB") %>%
  fmt_number(columns = c("pitchData.extension", "adj_x0", "pitchData.coordinates.z0", "mean_mvmt")) %>%
  fmt_number(columns = c("adj_sd", "pitchData.breaks.spinRate","pitchData.startSpeed"),decimals = 0, sep_mark = "") %>%
  cols_align(align = "center") 
