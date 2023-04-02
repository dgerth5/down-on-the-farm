library(readr)
library(dplyr)
df2 <- read_csv("milb_pbp_2022.csv")

bin_hits22 <- df2 %>%
  group_by(game_pk) %>%
  arrange(atBatIndex, .by_group = TRUE) %>%
  mutate(strikeout = if_else(result.eventType == "strikeout" | result.eventType == "strikeout_double_play", 1, 0),
         walk = if_else(result.eventType == "walk", 1, 0),
         single = if_else(result.eventType == "single", 1, 0),
         double = if_else(result.eventType == "double", 1, 0),
         triple = if_else(result.eventType == "triple", 1, 0),
         home_run = if_else(result.eventType == "home_run", 1, 0),
         stolen_base = if_else(grepl("Stolen", details.event), 1, 0),
         ld = lag(atBatIndex),
         last_pitch_ab = if_else(is.na(ld) == TRUE, 1, 
                                 if_else(atBatIndex != ld, 1, 0))) %>%
  filter(last_pitch_ab == 1)
  
bin_hits22$stolen_base[is.na(bin_hits22$stolen_base)] <- 0

bin_events_by_team22 <- bin_hits22 %>%
  group_by(batting_team) %>%
  summarise(sum_k = sum(strikeout),
            sum_bb = sum(walk),
            sum_1b = sum(single),
            sum_2b = sum(double),
            sum_3b = sum(triple),
            sum_hr = sum(home_run),
            sum_sb = sum(stolen_base))

df22 <- read_csv("milb_park_effects22.csv")

unique(df2$details.event)


rp <- df2 %>%
  mutate(sb = if_else(grep("steal", details.description) | 
                      grep("steal", result.description), 1, 0)) %>%
  filter(sb == 1)


steal_details <- grep("steal", df2$details.description, ignore.case = TRUE, value = TRUE)

sdu <- unique(steal_details)
View(sdu)

steal_result <- grep("steal", df2$result.description, ignore.case = TRUE, value = TRUE)
