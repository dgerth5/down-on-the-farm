library(magrittr)

compare_fn <- function(year){
  
  prev_year_df <- baseballr::mlb_stats(
    stat_type = "season",
    sport_id = 1,
    season = year - 1,
    stat_group = "hitting",
    player_pool = "All",
    limit = 10000) %>%
    dplyr::mutate(singles_per_pa = (hits - doubles - triples - home_runs)/plate_appearances,
                  walks_per_pa = base_on_balls/plate_appearances,
                  singles = (hits - doubles - triples - home_runs)) %>%
    dplyr::select(season, player_id, plate_appearances, singles, base_on_balls, singles_per_pa, walks_per_pa)
  
  curr_year_df <- baseballr::mlb_stats(
    stat_type = "season",
    sport_id = 1,
    season = year,
    stat_group = "hitting",
    player_pool = "All",
    limit = 10000) %>%
    dplyr::mutate(singles_per_pa = (hits - doubles - triples - home_runs)/plate_appearances,
                  walks_per_pa = base_on_balls/plate_appearances,
                  singles = (hits - doubles - triples - home_runs)) %>%
    dplyr::select(season, player_id, plate_appearances, singles, base_on_balls, singles_per_pa, walks_per_pa)
  
  mod_df <- dplyr::left_join(prev_year_df, curr_year_df, by = "player_id") %>%
    tidyr::drop_na()
  
  return(mod_df)
  
  
}

full_df <- purrr::map_dfr(c(2018:2019, 2021:2023), ~compare_fn(year = .x))

singles_mod <- lm(singles_per_pa.y ~ singles_per_pa.x , data = full_df)
summary(singles_mod)

walks_mod <- lm(walks_per_pa.y ~ walks_per_pa.x , data = full_df)
summary(walks_mod)

