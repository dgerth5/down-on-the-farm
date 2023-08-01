library(baseballr)
library(tidyverse)

data <- baseballr::mlb_pbp(game_pk = 742367)

clean <- data %>%
  filter(isPitch == TRUE) %>%
  filter(matchup.pitcher.fullName == "Carson Whisenhunt") %>%
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
  tab_header(title = md("**Carson Whisenhunt Futures Game Averages**")) %>%
  cols_label(details.type.description = "Pitch",
             mean_velo = "Velo",
             mean_xmov = "Horz. Break",
             mean_zmov = "Vert. Break",
             mean_sr = "Spin Rate",
             mean_height = "Release Height",
             thrown = "Times Thrown"
  )


df_usage <- data.frame(Level = c("A","A+","AA"),
                       FB_Whiff = c(.21,.18,.20),
                       FB_Chase = c(.15,.21,.16),
                       FB_Usage = c(.56,.50,.45),
                       CH_Whiff = c(.59,.62,.51),
                       CH_Chase = c(.66,.47,.35),
                       CH_Usage = c(.29,.41,.32),
                       CB_Whiff = c(.50,.20,.41),
                       CB_Chase = c(.25,.11,.22),
                       CB_Usage = c(.14,.08,.14))
gt(df_usage) %>%
  tab_header(title = md("**Carson Whisenhunt Season: 2023**")) %>%
  fmt_percent(-Level, decimals = 0)

