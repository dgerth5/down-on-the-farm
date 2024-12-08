library(baseballr)
library(tidyverse)
library(furrr)
library(readr)
library(gt)

leagues <- mlb_league(2024)  %>% select(league_id, league_name, season_date_info_season_start_date, season_date_info_season_end_date)
dates <-  data.frame(day = rep(seq(as.Date("2024-4-6"),as.Date("2024-4-18"), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map(function(x) mlb_game_pks(dates$day[x],
                                      level_ids = 14))

ml_game_pks <- minor_league_game_pk_lst %>%
  bind_rows() %>%
  dplyr::filter(status.codedGameState == "F",
                !is.na(game_pk)) %>%
  pull(game_pk)

plan("multisession", workers = 4)

safe_pbp <- safely(mlb_pbp)

milb_pbp <- 1:length(ml_game_pks) %>%
  furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
  purrr::map("result") %>%
  bind_rows()

milb_pbp <- milb_pbp %>% as.data.frame()

chase_rate <- milb_pbp %>%
  mutate(se = if_else(details.description %in% c("In play, out(s)","Swinging Strike","Foul","In play, run(s)","In play, no out",
                                                 "Foul Tip","Foul Bunt","Swinging Strike (Blocked)","Foul Pitchout", "Missed Bunt"), 1, 0),
         chase_zone = if_else(pitchData.zone > 9, 1, 0)) %>%
  filter(chase_zone == 1) %>%
  group_by(matchup.pitcher.id, matchup.pitcher.fullName, details.type.description) %>%
  summarise(chase_rate = mean(se),
            n = n()) %>%
  drop_na() %>%
  filter(matchup.pitcher.fullName == "George Klassen")

times_in_chase_zone <- milb_pbp %>%
  mutate(chase_zone = if_else(pitchData.zone > 9, 1, 0)) %>%
  group_by(matchup.pitcher.id, matchup.pitcher.fullName, details.type.description) %>%
  summarise(cz_per = mean(chase_zone),
            n = n()) %>%
  drop_na() %>%
  filter(matchup.pitcher.fullName == "George Klassen")

whiff_rate <- milb_pbp %>%
  mutate(se = if_else(details.description %in% c("In play, out(s)","Swinging Strike","Foul","In play, run(s)","In play, no out",
                                                 "Foul Tip","Foul Bunt","Swinging Strike (Blocked)","Foul Pitchout", "Missed Bunt"), 1, 0),
         we = if_else(details.description %in% c("Swinging Strike","Swinging Strike (Blocked)", "Missed Bunt", "Foul Tip" ), 1, 0)) %>%
  filter(se == 1) %>%
  group_by(matchup.pitcher.id, matchup.pitcher.fullName, details.type.description) %>%
  summarise(whiff_rate = mean(we),
            n = n()) %>%
  drop_na() %>%
  filter(matchup.pitcher.fullName == "George Klassen")


unique(milb_pbp$details.call.description)

pitch_metrics <- milb_pbp %>%
  group_by(matchup.pitcher.fullName, details.type.description) %>%
  summarise(mean_velo = mean(pitchData.startSpeed),
            mean_ivb = mean(pitchData.breaks.breakVerticalInduced),
            mean_hb = mean(pitchData.breaks.breakHorizontal),
            mean_sr = mean(pitchData.breaks.spinRate),
            n = n()) %>%
  drop_na() %>%
  filter(matchup.pitcher.fullName == "George Klassen")


# Joining chase_rate with times_in_chase_zone
combined_data <- left_join(chase_rate, times_in_chase_zone, 
                           by = c("details.type.description"))

# Joining the above result with whiff_rate
combined_data <- left_join(combined_data, whiff_rate, 
                           by = c("details.type.description"))

# Joining the result with pitch_metrics
final_combined_data <- left_join(combined_data, pitch_metrics, 
                                 by = c("details.type.description")) %>%
  mutate(usage = n.y.y / (37+45+129),
         zone_per = 1-cz_per) %>%
  select(details.type.description, mean_velo, mean_ivb, mean_hb, mean_sr, whiff_rate, zone_per, chase_rate, usage) %>%
  arrange(-usage)

klassen_pro <- final_combined_data %>%
  gt() %>%
  tab_header(title = md("**George Klassen Pitch Data Summary**"),
             subtitle = md("FSL 2024")) %>%
  fmt_percent(c("whiff_rate", "zone_per", "chase_rate", "usage"), decimals = 0) %>%
  fmt_number(c("mean_velo", "mean_ivb", "mean_hb"), decimals = 1) %>%
  fmt_number(c("mean_sr"), decimals = 0, use_seps = FALSE) %>%
  cols_label(details.type.description = "Pitch Type",
             mean_velo = "Velocity",
             mean_ivb = "IVB",
             mean_hb = "Horizontal Break",
             mean_sr = "Spin Rate",
             whiff_rate = "Whiff%",
             zone_per = "Zone%",
             chase_rate = "Chase%",
             usage = "Usage%")
  
gtsave(klassen_pro, "klassen_pro.png")

 
## college


df <- data.frame(Pitch = c("Fastball", "Curveball", "Slider"),
                 Strike = c(.57, .50, .40),
                 Whiff = c(.17, .30, .50),
                 Zone = c(.43, .39, .20),
                 Chase = c(.18, .16, .16),
                 Usage = c(.73, .21, .06))

klassen_college2 <- df %>%
  gt() %>%
  tab_header(title = md("**George Klassen Pitch Summary**"),
             subtitle = md("2023 Season at Minnesota")) %>%
  fmt_percent(c("Strike", "Whiff", "Chase", "Usage", "Zone"), decimals = 0) %>%
  cols_label(Pitch = "Pitch Type",
             Strike = "Strike%",
             Zone = "Zone%",
             Whiff = "Whiff%",
             Chase = "Chase%",
             Usage = "Usage%")

gtsave(klassen_college2, "klassen_college2.png")


library(readr)
library(tidyverse)
statcast23 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast23.csv")
single_a_pbp2023 <- read_csv("single_a_pbp2023.csv")

# estimating release position model

release_point_mod <- lm(cbind(release_pos_x, release_pos_z) ~ plate_x + plate_z + vx0 + vy0 + vz0 + ax + ay + az, data = statcast23)
summary(release_point_mod)

klassen4 <- milb_pbp %>%
  filter(home_league_name == "Florida State League") %>%
  filter(matchup.pitcher.fullName == "George Klassen") %>%
  rename("plate_x" = pitchData.coordinates.pX,
         "plate_z" = pitchData.coordinates.pZ,
         "vx0" = pitchData.coordinates.vX0,
         "vy0" = pitchData.coordinates.vY0,
         "vz0" = pitchData.coordinates.vZ0,
         "ax" = pitchData.coordinates.aX,
         "ay" = pitchData.coordinates.aY,
         "az" = pitchData.coordinates.aZ)

pred_release_pos_milb <- predict(release_point_mod, klassen4)

klassen4$est_x <- pred_release_pos_milb[,1]
klassen4$est_z <- pred_release_pos_milb[,2]

klassen5 <- klassen4 %>%
  select(details.type.description, est_x, est_z, pitchData.extension) %>% 
  drop_na()

library(ggplot2)

ggplot(klassen5, aes(x = est_x, y = est_z, color = details.type.description)) +
  geom_point() +
  xlim(0,-4) + ylim(0,7) +
  labs(title = "George Klassen Release Points",
       x = "Horizontal Release Point",
       y = "Vertical Release Point",
       color = "Pitch Type") 


asfd <- statcast23 %>%
  group_by(player_name) %>%
  summarise(sd_X = sd(abs(release_pos_x)),
            sd_Z = mean(release_pos_z)) %>%
  drop_na()

mean(asfd$sd_X)
mean(asfd$sd_Z)

mean(klassen5$pitchData.extension)

mean(statcast23$release_extension, na.rm = TRUE)
