library(baseballr)

beeter <- scrape_statcast_savant(
  start_date = "2023-02-24",
  end_date = "2023-03-20",
  playerid = 690925,
  player_type = "pitcher"
)

library(dplyr)
library(gt)
library(gtExtras)

beeter %>%
  group_by(pitch_type) %>%
  summarise(mean_velo = mean(release_speed),
            mean_pfx_x = mean(pfx_x)*12,
            mean_pfx_z = mean(pfx_z)*12,
            mean_spin_rate = round(mean(release_spin_rate),0),
            count = n()) %>%
  arrange(-count) %>%
  gt() %>%
  tab_header(title = md("**Clayton Beeter Statcast Data**"),
             subtitle = md("Spring Training 2023")) %>%
  fmt_number(columns = c("mean_velo", "mean_pfx_x", "mean_pfx_z", "mean_spin_rate"), decimals = 0, sep_mark = "") %>%
  cols_label(pitch_type = "Pitch Type",
             mean_velo = "Average Velocity",
             mean_pfx_x = "Average Horizontal Break",
             mean_pfx_z = "Average Vertical Break",
             mean_spin_rate = "Average Spin Rate",
             count = "Pitches Thrown")
