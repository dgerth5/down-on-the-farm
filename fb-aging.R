library(readr)
library(purrr)
library(baseballr)
library(tidyverse)

statcast22 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast22.csv")
statcast21 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast21.csv")
statcast20 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast20.csv")
statcast19 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast19.csv")
statcast18 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast18.csv")

statcast <- rbind(statcast22, statcast21, statcast20, statcast19, statcast18)

f <- unique(statcast$batter)
dff <- map_dfr(f, ~ mlb_people(.x))

id_bday <- dff %>%
  select(id, birth_date) %>%
  rename(batter = id)

id_bday$birth_date <- as.Date(id_bday$birth_date)

statcast_wbday <- left_join(statcast, id_bday, by = "batter")
statcast_wbday2 <- statcast_wbday %>%
  mutate(age_day_of_game = as.numeric(game_date - birth_date) / 365,
         same_hand = if_else(p_throws == stand, 1, 0),
         adjx = if_else(release_pos_x < 0, pfx_x, pfx_x*-1),
         adjplate_x = if_else(release_pos_x < 0, plate_x, plate_x*-1),
         count = paste0(balls, "-", strikes),
         swing = if_else(description %in% c("hit_into_play", "foul", "swinging_strike", "foul_tip", "swinging_strike_blocked"), 1, 0),
         whiff = if_else(description %in% c("swinging_strike",
                                            "swinging_strike_blocked"),1,0))


mod_df <- statcast_wbday2 %>% filter(swing == 1 & pitch_type == "FF")

library(mgcv)

m0 <- bam(whiff ~ s(adjplate_x,plate_z, k = 100) + s(adjx,pfx_z, k = 100) + s(release_speed,age_day_of_game, k = 100) + release_spin_rate + same_hand, 
          data = mod_df,
          family = binomial(),
          discrete = TRUE,
          nthreads = 4)     

pred_df <- data.frame(adjplate_x = rep(-.34,10),
                      plate_z = rep(2.98,10),
                      adjx = rep(-1.06,10),
                      pfx_z = rep(1.49,10),
                      release_speed = rep(93,10),
                      age_day_of_game = 25:34,
                      release_spin_rate = rep(2500,10),
                      same_hand = rep(1,10))

pred_df$p <- predict(m0, pred_df, type = "response")
