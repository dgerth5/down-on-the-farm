library(baseballr)
library(tidyverse)
library(readr)
library(mlbplotR)
library(gt)
library(gtExtras)
library(purrr)

oaa <- read_csv("C:/Users/david/Downloads/outs_above_average (1).csv")
pitch_lb <- read_csv("C:/Users/david/Downloads/fangraphs-leaderboards (3).csv")
team_map <- read_csv("C:/Users/david/Downloads/SFBB MLB Team Map - SFBB Team Map.csv")
fg_pf <- fg_park(2022) %>%
  select(home_team, `3yr`)

oaa2 <- oaa %>%
  mutate(oaa_runs = outs_above_average*0.8,
         name = paste0(first_name, " ", last_name)) %>%
  select(name, oaa_runs)

fg_fn <- function(id){
  Sys.sleep(1)
  df <- fg_pitcher_game_logs(id, year = 2023)
  return(df)
}

fg_fn2 <- safely(fg_fn)

df <- map_df(pitch_lb$PlayerId, fg_fn2)

df2 <- df$result
tm2 <- team_map %>%
  select(FANGRAPHSTEAM, FANGRAPHSABBR)

df3 <- df2 %>%
  mutate(park = if_else(HomeAway == "H", Team, str_sub(Opp, -3, -1))) %>%
  select(PlayerName, R, park)  %>%
  left_join(tm2, by = c("park"="FANGRAPHSABBR")) %>%
  left_join(fg_pf, by = c("FANGRAPHSTEAM"="home_team")) %>%
  mutate(adjR = R / (`3yr` / 100)) %>%
  group_by(PlayerName) %>%
  summarise(padjR = sum(adjR),
            sumR = sum(R)) %>%
  inner_join(oaa2, by = c("PlayerName"="name")) %>%
  mutate(pdadjR = padjR + oaa_runs) %>%
  inner_join(pitch_lb, by = c("PlayerName"="Name")) %>%
  select(PlayerName, pdadjR, sumR, IP) %>%
  mutate(pdRA9 = pdadjR / IP * 9,
         RA9 = sumR / IP * 9)

df4 <- inner_join(df3, load_headshots() %>% select(player_name, espn_headshot), by = c("PlayerName"="player_name"))

df4 %>%
  arrange(pdRA9) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, PlayerName, espn_headshot, IP, RA9, pdRA9) %>%
  slice(6:10) %>%
  gt() %>%
  tab_header(title = md("**Park and Defense Adjusted RA/9 Leaders**"),
             subtitle = md("Qualified Pitchers. Season: 2023")) %>%
  fmt_number(columns = c("RA9","pdRA9")) %>%
  gt_img_rows(espn_headshot, height = 35) %>%
  cols_label(espn_headshot = "",
             PlayerName = "Name")

