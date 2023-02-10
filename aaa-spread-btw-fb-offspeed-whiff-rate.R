library(baseballr)
library(tidyverse)
# library(furrr)
# 
# leagues <- mlb_league(2022) # just 2022 for now
# dates <- data.frame(day = rep(seq(as.Date("2022-04-08"),as.Date("2022-09-28"), by = "days"), times = 1))
# minor_league_game_pk_lst <- 1:nrow(dates) %>%
#   purrr::map(function(x) mlb_game_pks(dates$day[x],
#                                       level_ids = 11)) # just triple a data
# 
# ml_game_pks <- minor_league_game_pk_lst %>%
#   bind_rows() %>%
#   dplyr::filter(status.codedGameState == "F",
#                 !is.na(game_pk)) %>%
#   pull(game_pk)
# 
# plan("multisession", workers = 3)
# 
# safe_pbp <- safely(mlb_pbp)
# 
# ml_pbp <- 1:length(ml_game_pks) %>%
#   furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
#   purrr::map("result") %>%
#   bind_rows()
# 
# ml_pbp <- ml_pbp %>% as.data.frame()
# 
# getwd()
# write.csv(ml_pbp, "triple-a-pbp.csv") # might want to find something faster than this

## next time import csv, prob take a min or two

library(readr)
triple_a_pbp <- read_csv("triple-a-pbp.csv")


hd <- triple_a_pbp[1:3,] # just to get an idea of the column names

# counting up the na's in the pitch types
ct <- triple_a_pbp   %>%
  group_by(home_league_name, details.type.code) %>%
  summarise(ct = n(), 
            per = round(ct / 725955*100,3))

fb_code <- c("FF", "SI", "FT", "FC")

swing_situations <- c("In play, out(s)", "Swinging Strike", "In play, runs(s)","In play, no out",
                      "Foul Tip", "Foul Bunt", "Swinging Strike (Blocked)", "Missed Bunt")
  
triple_a_pbp2 <- triple_a_pbp %>%
  filter(details.call.description %in% swing_situations) %>%
  mutate(fb = if_else(details.type.code %in% fb_code, 1, 0),
         Miss = if_else(details.call.description %in%
                         c("Swinging Strike",
                           "Swinging Strike (Blocked)"), 1, 0),
         namecode = paste0(matchup.batter.fullName, matchup.batter.id))

smry <- triple_a_pbp2 %>%
  group_by(namecode, fb) %>%
  summarise(whiff_rate = mean(Miss),
            count = n())
summary(smry$count)
