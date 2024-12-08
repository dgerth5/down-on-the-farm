library(relaimpo)
library(purrr)
library(tidyverse)
library(baseballr)

get_woba_fn <- function(year){
 pitch_df <- mlb_stats(stat_type = "season",
                       sport_id = 1,
                       season = year,
                       stat_group = "pitching",
                       player_pool = "All") %>%
             mutate(uBB = base_on_balls - intentional_walks,
                    X1B = hits - doubles - triples - home_runs) %>%
             rename(HBP = hit_batsmen,
                    X2B = doubles,
                    X3B = triples,
                    HR = home_runs,
                    AB = at_bats,
                    SO = strike_outs,
                    SH = sac_bunts,
                    SF = sac_flies) %>%
              dplyr::select(season, uBB, HBP, X1B, X2B, X3B, HR, AB, SH, SO, SF, player_id, player_full_name, batters_faced)
                        
  add_woba <- woba_plus(pitch_df)
  
  return(add_woba)
}


all_data <- map_df(c(2018:2023),
                     ~ get_woba_fn(year = .x))

mod_df <- all_data %>%
  arrange(player_id, season) %>%
  group_by(player_id) %>%
  mutate(next_year_woba = lead(wOBA),
         k_per = SO / batters_faced,
         bb_per = uBB / batters_faced) %>%
  drop_na(next_year_woba) 

mod <- lm(next_year_woba ~ k_per + bb_per,
          data = mod_df,
          weights = batters_faced)

summary(mod)

relative_imp <- calc.relimp(mod, rela=TRUE)
relative_imp



