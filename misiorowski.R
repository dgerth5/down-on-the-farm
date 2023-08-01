library(baseballr)
library(tidyverse)

data <- baseballr::mlb_pbp(game_pk = 742367)

clean <- data %>%
  filter(isPitch == TRUE) %>%
  filter(matchup.pitcher.fullName == "Jacob Misiorowski") %>%
  mutate(pfx_x = 0.4129406 + 1.6943550*pitchData.coordinates.pfxX,
         pfx_z = -0.2487306 + 1.7272317*pitchData.coordinates.pfxZ)  %>%
  group_by(details.type.description) %>%
  summarise(mean_velo = round(mean(pitchData.startSpeed),0),
            mean_xmov = round(mean(pfx_x),1),
            mean_zmov = round(mean(pfx_z),1),
            mean_sr = round(mean(pitchData.breaks.spinRate),0),
            mean_height = round(mean(pitchData.coordinates.z0),1),
            thrown = n()) %>%
  arrange(-mean_velo) %>%
  ungroup()

library(gt)

gt(clean) %>%
  tab_header(title = md("**Jacob Misiorowski Futures Game Averages**")) %>%
  cols_label(details.type.description = "Pitch",
             mean_velo = "Velo",
             mean_xmov = "Horz. Break",
             mean_zmov = "Vert. Break",
             mean_sr = "Spin Rate",
             mean_height = "Release Height",
             thrown = "Times Thrown"
             )

?cols_label
