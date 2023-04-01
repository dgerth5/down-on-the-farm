library(readxl)
dotf_2023_top_prospects <- read_excel("~/dotf 2023 top prospects.xlsx") %>%
  rename(team_name = Org)

library(tidyverse)
library(mlbplotR)

teams_colors_logos <- load_mlb_teams() %>%
  select(team_name, team_cap_logo_on_light)

df <- left_join(dotf_2023_top_prospects, teams_colors_logos, by = "team_name")

library(gt)
library(gtExtras)

df2 <- df %>%
  select(Player, team_cap_logo_on_light, `Pipeline Rank`, Level, `MiLB City`)

df2 %>%
  gt() %>%
  tab_header(title = md("**Prospects To Watch and Where to Watch Them**")) %>%
  gt_img_rows(team_cap_logo_on_light, height = 25) %>%
  cols_label(team_cap_logo_on_light = "Org")
