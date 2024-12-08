library(tidyverse)
library(purrr)
library(readxl)
library(readr)
library(survival)

# load statcast data
statcast23 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast23.csv")
statcast22 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast22.csv")
statcast21 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast21.csv")
statcast20 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast20.csv")
statcast19 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast19.csv")
statcast18 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast18.csv")

# load tj data
tommy_john_data <- read_excel("~/tommy_john_data.xlsx")

# get pitch data

get_pitch_count_data <- function(year){
  
  # get pitches thrown across all levels, but then keep just the major leaguers
  df <- map_df(c(1,11,12,13,14), ~baseballr::mlb_stats(
    stat_type = "season",
    sport_id = .x,
    season = year,
    stat_group = "pitching",
    player_pool = "All"),
    limit = 10000) %>%
    dplyr::select(season, player_id, player_full_name, number_of_pitches, sport_id)
  
  pitch_df <- df %>%
    group_by(player_id, player_full_name) %>%
    summarise(tot_pitches = sum(number_of_pitches),
              min_lg = min(sport_id)) %>%
    filter(min_lg == 1)
  
  pitch_df$season = year
  
  df_t1 <- map_df(c(1,11,12,13,14), ~baseballr::mlb_stats(
    stat_type = "season",
    sport_id = .x,
    season = year+1,
    stat_group = "pitching",
    player_pool = "All"),
    limit = 10000) %>%
    dplyr::select(season, player_id, player_full_name, number_of_pitches, sport_id)
  
  pitch_df_t1 <- df_t1 %>%
    group_by(player_id) %>%
    summarise(tot_pitchest1 = sum(number_of_pitches))
  
  final_df <- left_join(pitch_df, pitch_df_t1, by = "player_id")
  
  return(final_df)
}

all_pitch_data <- map_df(c(2018,2021,2022),
                         ~ get_pitch_count_data(year = .x))

primary_pos <- map_df(unique(all_pitch_data$player_id),
                      ~baseballr::mlb_people(person_ids = .x))

remove_pos <- left_join(all_pitch_data,
                        primary_pos %>% dplyr::select(id, current_age, primary_position_code),
                        by = c("player_id"="id")) %>%
  filter(primary_position_code == 1) %>%
  mutate(next_yr = season + 1,
         age = current_age - (2024 - season)) %>%
  dplyr::select(-primary_position_code, current_age)

# join to get if tj next year

df_w_tj <- left_join(remove_pos,
                     tommy_john_data,
                     by = c("player_id"="MLBAMID", "next_yr"="Year"))

excl_retired <- df_w_tj %>%
  mutate(pitch_nxt_yr = if_else(is.na(tot_pitchest1)==TRUE, 0, 1),
         had_tj = if_else(is.na(Player)==TRUE, 0, 1)) %>%
  filter(pitch_nxt_yr == 1) # currently excluding pitchers who had tj in off season #%>% filter(had_tj == 0)

mean(excl_retired$had_tj)

# get instances of 25+ pitch innings

o25_fn <- function(df){
  df_25 <- df %>%
    arrange(game_pk, at_bat_number) %>%
    mutate(
      new_pitcher = if_else(pitcher != lag(pitcher, default = 0), 1, 0),
      pitch_count = cumsum(new_pitcher)  
    ) %>%
    group_by(game_pk) %>% 
    mutate(pitch_count = row_number() - cummax((new_pitcher == 1) * row_number())+1,
           over25 = if_else(pitch_count >= 25, 1, 0)) %>%
    group_by(pitcher) %>%
    mutate(change_in_over_25 = over25 != lag(over25, default = FALSE)) %>%
    summarise(times_over_25 = sum(over25 & change_in_over_25)) %>%
    ungroup()
  
  df_25$year <- unique(df$game_year)
  
  return(df_25)
}

o25_2023 <- o25_fn(statcast23)
o25_2022 <- o25_fn(statcast22)
o25_2021 <- o25_fn(statcast21)
o25_2019 <- o25_fn(statcast19)
o25_2018 <- o25_fn(statcast18)

o_master <- bind_rows(o25_2023, o25_2022, o25_2021, o25_2019, o25_2018)

# get highest avg spin rate from non FB

max_avg_sr_fn <- function(df){
  
  fb <- c("4-Seam Fastball","Cutter","Sinker","Fastball")
  
  max_avg_sr <- df %>%
    filter(!pitch_name %in% fb) %>%
    drop_na(release_spin_rate) %>%
    group_by(pitcher, pitch_name) %>%
    summarise(avg_sr = mean(release_spin_rate),
              times_thrown = n()) %>%
    group_by(pitcher) %>%
    slice_max(order_by = times_thrown, n = 1)
  
  max_avg_sr$year <- unique(lubridate::year(df$game_date))
  
  return(max_avg_sr)
}

off_max_sr23 <- max_avg_sr_fn(statcast23)
off_max_sr22 <- max_avg_sr_fn(statcast22)
off_max_sr21 <- max_avg_sr_fn(statcast21)
off_max_sr19 <- max_avg_sr_fn(statcast19)
off_max_sr18 <- max_avg_sr_fn(statcast18)

off_max_sr_master <- bind_rows(off_max_sr23, off_max_sr22, off_max_sr21, off_max_sr19, off_max_sr18)

# get avg fb velo, and check if it's greater than 1 mph

avg_fb_velo <- function(df, last_year_df){
  
  fb <- c("4-Seam Fastball","Cutter","Sinker","Fastball")
  
  max_avg_velo <- df %>%
    filter(pitch_name %in% fb) %>%
    drop_na(release_speed) %>%
    group_by(pitcher) %>%
    summarise(avg_velo = max(release_speed)) 
  
  max_avg_velo$year = unique(lubridate::year(df$game_date))
  
  max_avg_velo_prev_yr <- last_year_df %>%
    filter(pitch_name %in% fb) %>%
    drop_na(release_speed) %>%
    group_by(pitcher) %>%
    summarise(avg_velo_prev = mean(release_speed)) 
  
  df <- left_join(max_avg_velo, max_avg_velo_prev_yr, by = "pitcher")
  
  df2 <- df %>%
    mutate(over1 = if_else(is.na(avg_velo_prev)==TRUE, 0, 
                           if_else(avg_velo > avg_velo_prev + 1, 1, 0)))
  
  return(max_avg_velo)
  
}

velo23 <- avg_fb_velo(statcast23, statcast22)
velo22 <- avg_fb_velo(statcast22, statcast21)
velo21 <- avg_fb_velo(statcast21, statcast20)
velo19 <- avg_fb_velo(statcast19, statcast18)
velo18 <- avg_fb_velo(statcast18, statcast18)

velo_master <- bind_rows(velo23, velo22, velo21, velo19, velo18)


model_df1 <- left_join(excl_retired, o_master,
                       by = c("player_id"="pitcher", "season"="year"))
model_df2 <- left_join(model_df1, off_max_sr_master, 
                       by = c("player_id"="pitcher", "season"="year"))  # some dupes here come back
model_df3 <- left_join(model_df2, velo_master,
                       by = c("player_id"="pitcher", "season"="year"))

final_df <- model_df3 %>%
  filter(tot_pitches > 500) %>%
  dplyr::select(player_id, player_full_name, had_tj, age, tot_pitchest1, tot_pitches, times_over_25, avg_sr, avg_velo)

# model 
l <- Surv(final_df$tot_pitchest1, final_df$had_tj, type = "right")
head(l)

cox_model <- coxph(Surv(tot_pitchest1, had_tj, type = "right") ~ age + tot_pitches + times_over_25 + avg_velo + avg_sr, data = final_df)
summary(cox_model)

final_surve <- predict(cox_model, final_df, type = "survival")
head(final_surve)
predicted_survival <- survfit(cox_model, newdata = final_df[1:100,])

# Plot predicted survival curves
plot(predicted_survival, col = 1:100, 
     lty = 1, xlab = "Days", ylab = "Survival Probability", main = "Predicted Survival Curves for New Pitchers",
     ylim = c(0.85,1))
# legend("bottomleft", legend = paste("Pitcher", 1:2030), col = 1:2030, lty = 1)

cox.zph(cox_model)
ggcoxzph(cox_model)


survminer::ggsurvplot(survfit(cox_model),
                      color = "#2E9FDF", 
                      data = final_df, 
                      ggtheme = theme_minimal(),
                      ylim = c(0.85, 1))
