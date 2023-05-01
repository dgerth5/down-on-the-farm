library(ggplot2)
library(baseballr)
library(tidyverse)
library(furrr)

leagues <- mlb_league(2022) 
dates <- data.frame(day = rep(seq(as.Date("2023-04-30"),as.Date("2023-04-30"), by = "days"), times = 1))
minor_league_game_pk_lst <- 1:nrow(dates) %>%
  purrr::map_df(function(x) mlb_game_pks(dates$day[x],
                                         level_ids = 11))

ml_game_pks <- minor_league_game_pk_lst %>%
  bind_rows() %>%
  dplyr::filter(status.codedGameState == "F",
                !is.na(game_pk)) %>%
  pull(game_pk)

plan("multisession", workers = 3)

safe_pbp <- safely(mlb_pbp)

ml_pbp <- 1:length(ml_game_pks) %>%
  furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
  purrr::map("result") %>%
  bind_rows()


waino <- ml_pbp %>%
  filter(matchup.pitcher.fullName == "Adam Wainwright") %>%
  mutate(adj_x = 1.443 + 1.681*pitchData.coordinates.pfxX - 0.012*pitchData.startSpeed,
         adj_z = 2.268 + 1.753*pitchData.coordinates.pfxZ - 0.030*pitchData.startSpeed) %>%
  select(details.type.code, pitchData.startSpeed, adj_x, adj_z) %>%
  rename(pitch_type = details.type.code) %>%
  drop_na()


library(gtable)
library(gt)
library(ggplot2)


p <- ggplot(waino, aes(x = adj_x, y = adj_z, col = pitch_type)) +
  geom_point() +
  ggtitle("Adam Wainwright Pitch Movement", 
          subtitle = "4-30-2023") + 
  xlab("Horizontal Movement") + ylab("Vertical Movement") +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0)

waino_smry <- waino %>% 
  group_by(pitch_type) %>%
  summarise(mean_velo = mean(pitchData.startSpeed), 
            mean_horz = mean(adj_x), 
            mean_vert = mean(adj_z),
            ct = n()) %>%
  drop_na()

t <- gt(waino_smry) %>%
  tab_header(title = md("**Adam Wainwright vs Durham Bulls**"),
             subtitle = md("4-30-2023. IP: 5.2 K: 9 BB: 1 ER: 4 H: 7")) %>%
  fmt_number(columns = c("mean_velo", "mean_horz", "mean_vert"), decimals = 0) %>%
  cols_label(pitch_type = "Pitch Type", 
             mean_velo = "Velocity", 
             mean_horz = "Horz Mvmt", mean_vert = "Vert Mvmt",
             ct = "Pitches Thrown")

library(grid)
library(gridExtra)

p2 <- ggplotGrob(p)
t2 <- grid.table(waino_smry)

?tableGrob

library(cowplot)

plot_grid(p2,t2)
grid.arrange(p2, t2)
