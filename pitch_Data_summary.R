library(tidyverse)
library(baseballr)
library(readr)
library(purrr)
library(furrr)
library(gt)

dates <- data.frame(day = rep(seq(as.Date("2024-6-18"),as.Date("2024-6-18"), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map(function(x) mlb_game_pks(dates$day[x],
                                      level_ids = 12))

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
summary(milb_pbp$pitchData.breaks.spinRate)


pitches2 <- milb_pbp %>%
  group_by(matchup.pitcher.id, matchup.pitcher.fullName, details.type.description) %>%
  summarise(mean_velo = mean(pitchData.startSpeed),
            mean_ivb = mean(pitchData.breaks.breakVerticalInduced),
            mean_hb = mean(pitchData.breaks.breakHorizontal),
            mean_sr = mean(pitchData.breaks.spinRate),
            n = n()) %>%
  drop_na()

wilcox <- pitches2 %>%
  ungroup() %>%
  filter(matchup.pitcher.fullName == "Cole Wilcox") %>%
  select(-matchup.pitcher.id, -matchup.pitcher.fullName) %>%
  arrange(-n) %>%
  gt() %>%
  tab_header(title = md("**Cole Wilcox Pitch Data Summary**"),
             subtitle = md("6/18/2024")) %>%
  fmt_number(c("mean_velo", "mean_ivb", "mean_hb"), decimals = 1) %>%
  fmt_number(c("mean_sr"), decimals = 0, use_seps = FALSE) %>%
  cols_label(details.type.description = "Pitch",
             mean_velo = "Velocity",
             mean_ivb = "IVB",
             mean_hb = "Horizontal Break",
             mean_sr = "Spin Rate",
             n = "Pitches Thrown")

gtsave(wilcox, "wilcox0618.png")
