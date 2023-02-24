library(readr)
milb_park_effects22 <- read_csv("milb_park_effects22.csv")

library(gt)
library(tidyverse)

just_hr <- milb_park_effects22 %>%
  select(team, home_run_pf, home_run_mult) %>%
  arrange(-home_run_pf) %>%
  rename(Team = team,
         `Home Run Park Factor` = home_run_pf,
         `Home Run Multiplier` = home_run_mult) %>%
  slice(1:10)

gt(just_hr) %>%
  tab_header(title = md("**Top 10 Home Run Parks**"),
             subtitle = md("Season: 2022")) %>%
  cols_align(everything(), align = "center")
