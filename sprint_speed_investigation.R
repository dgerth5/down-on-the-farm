library(tidyverse)
library(baseballr)
library(purrr)
library(caTools)
library(MLmetrics)
library(ggplot2)
library(mgcv)
library(mlbplotR)
library(gt)
library(gtExtras)



# get sprint speed

df <- map_df(2015:2023, ~ statcast_leaderboards(leaderboard = "sprint_speed", year = .x))

get_stat_leaders <- function(level_id, season) {
  
  lb <- baseballr::mlb_stats(
    stat_type = "season",
    sport_id = level_id,
    season = season,
    stat_group = "hitting",
    player_pool = "All",
    limit = 10000) %>%
    filter(plate_appearances > 50) %>%
    mutate(single = hits - doubles - triples - home_runs,
           single_to_gbo_ratio = single / ground_outs,
           sba = (caught_stealing + stolen_bases) / (single + base_on_balls + hit_by_pitch), 
           triple_rate = triples / plate_appearances) %>%
    mutate(norm_sba = scales::rescale(sba, to=c(0,1)),
           norm_tr = scales::rescale(triple_rate, to=c(0,1)),
           norm_sgr = scales::rescale(single_to_gbo_ratio, to=c(0,1)),
           norm_gidp = scales::rescale(ground_into_double_play, to=c(0,1))) %>%
    select(season, player_id, player_full_name, caught_stealing, plate_appearances, stolen_bases, single, hit_by_pitch, base_on_balls, sba, norm_sba, norm_tr, norm_sgr, norm_gidp, position_name) 

  return(lb)
  
}

s <- map_df(2015:2023, ~ get_stat_leaders(level_id = 1, season = .x))
s$season <- as.numeric(s$season)

summary(s)

df2 <- left_join(df, s, by = c("player_id", "year"="season"))

df3 <- df2 %>%
  arrange(player_id, year) %>%
  select(player_id, year, sprint_speed, norm_sba, norm_tr, norm_sgr, norm_gidp, plate_appearances, age) %>%
  group_by(player_id) %>%
  mutate(lag_sba = lag(norm_sba),
         lag_tr = lag(norm_tr),
         lag_s2gbo = lag(norm_sgr),
         lag_gdp = lag(norm_gidp),
         lag_pa = lag(plate_appearances),
         tot_pa = plate_appearances + lag_pa,
         curr_pa_weight = plate_appearances / tot_pa,
         prev_pa_weight = lag_pa / tot_pa) %>%
  drop_na() 

set.seed(4129)
split <- sample.split(df3$player_id, SplitRatio = 0.75)
training_set <- subset(df3, split == TRUE)
test_set <- subset(df3, split == FALSE)

get_best_rmse <- function(w){
  
  weight <- w
  
  ts2 <- training_set %>%
    mutate(wa_sba = norm_sba*weight*curr_pa_weight + lag_sba*(1-weight)*prev_pa_weight,
           wa_triple_rate = norm_tr*weight*curr_pa_weight + lag_tr*(1-weight)*prev_pa_weight,
           wa_s2gbo = norm_sgr*weight*curr_pa_weight + lag_s2gbo*(1-weight)*prev_pa_weight)

  # mod <- lm(sprint_speed ~ wa_sba + wa_triple_rate + wa_s2gbo + age,
  #           data = ts2)
  
  mod <- gam(sprint_speed ~ s(wa_sba) + s(wa_triple_rate) + s(wa_s2gbo) + s(age),
             data = ts2)
  
  test2 <- test_set %>%
    mutate(wa_sba = norm_sba*weight*curr_pa_weight + lag_sba*(1-weight)*prev_pa_weight,
           wa_triple_rate = norm_tr*weight*curr_pa_weight + lag_tr*(1-weight)*prev_pa_weight,
           wa_s2gbo = norm_sgr*weight*curr_pa_weight + lag_s2gbo*(1-weight)*prev_pa_weight)
  
  p <- predict(mod, test2)
  
  r <- round(RMSE(p, test2$sprint_speed), 4)
  
  return(r)
  
}

x <- seq(0,1,.05)

for (i in x){
  print(paste0("Weight ", i, " RMSE: ", get_best_rmse(i)))
}


weight <- 0.55

df4 <- df3 %>%
  mutate(wa_sba = norm_sba*weight*curr_pa_weight + lag_sba*(1-weight)*prev_pa_weight,
         wa_triple_rate = norm_tr*weight*curr_pa_weight + lag_tr*(1-weight)*prev_pa_weight,
         wa_s2gbo = norm_sgr*weight*curr_pa_weight + lag_s2gbo*(1-weight)*prev_pa_weight)

mod2 <- gam(sprint_speed ~ s(wa_sba) + s(wa_triple_rate) + s(wa_s2gbo) + s(age),
            data = df4)

test3 <- test_set %>%
  mutate(wa_sba = norm_sba*weight*curr_pa_weight + lag_sba*(1-weight)*prev_pa_weight,
         wa_triple_rate = norm_tr*weight*curr_pa_weight + lag_tr*(1-weight)*prev_pa_weight,
         wa_s2gbo = norm_sgr*weight*curr_pa_weight + lag_s2gbo*(1-weight)*prev_pa_weight)

# test fit

fitted_values <- predict(mod2, test3)

plot_data <- data.frame(SprintSpeed = test3$sprint_speed, FittedValues = fitted_values)

ggplot(plot_data, aes(x = SprintSpeed, y = FittedValues)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  
  theme_minimal() +
  labs(x = "Sprint Speed", y = "Predicted Sprint Speed", 
       title = "Sprint Speed vs Predicted Sprint Speed - Out of Sample")

ggsave("sprint_speed_pred.png")

get_minor_lg_stats <- function(season){
  
  df <- data.frame()
  
  for (i in seq(11,14,1)){
    
    r <- baseballr::mlb_stats(
                              stat_type = "season",
                              sport_id = i,
                              season = season,
                              stat_group = "hitting",
                              player_pool = "All",
                              limit = 10000) 
    
    df <- rbind(df, r)
    
  }
  
  df2 <- df %>%
    group_by(player_id,player_full_name) %>%
    summarise(tot_pa = sum(plate_appearances),
              tot_hit = sum(hits),
              tot_doubles = sum(doubles),
              tot_triples = sum(triples),
              tot_hr = sum(home_runs),
              tot_go = sum(ground_outs),
              tot_bb = sum(base_on_balls),
              tot_hbp = sum(hit_by_pitch),
              tot_sb = sum(stolen_bases),
              tot_cs = sum(caught_stealing)) %>%
    ungroup() %>%
    filter(tot_pa > 50) %>%
    mutate(single = tot_hit - tot_doubles - tot_triples - tot_hr,
           single_to_gbo_ratio = single / tot_go,
           sba = (tot_cs + tot_sb) / (single + tot_bb + tot_hbp), 
           triple_rate = tot_triples / tot_pa) %>%
    mutate(norm_sba = scales::rescale(sba, to = c(0,1)),
           norm_tr = scales::rescale(triple_rate, to = c(0,1)),
           norm_sgr = scales::rescale(single_to_gbo_ratio, to = c(0,1)),
           season = season)
  
  return(df2)
  
}


milb_stats <- map_df(2022:2023, ~ get_minor_lg_stats(season = .x))


clean_lookup <- function(df) {
  
  lookup = df %>% 
    select(key_person, 
           key_mlbam, 
           key_fangraphs,
           name_first, 
           name_last,
           birth_year, 
           birth_month, 
           birth_day) %>% 
    filter(!is.na(key_mlbam)) %>%
    filter(birth_year > 1980, !is.na(key_mlbam)) %>%
    mutate(
      dob = ifelse(!is.na(birth_day), 
                   paste0(birth_month, "-", birth_day, "-", birth_year), NA),
      player_name = paste0(name_first, " ", name_last)
    ) %>%
    select(mlbam_id = key_mlbam, fg_id = key_fangraphs, player_name, dob) %>%
    mutate(age2023 = lubridate::year(lubridate::as.period(lubridate::interval(start = lubridate::mdy(dob), end = lubridate::mdy("03-30-2023")))))
  
  return(lookup)
  
}

lookup = baseballr::chadwick_player_lu() %>% clean_lookup()

milb_stats2 <- left_join(milb_stats, lookup, by = c("player_id"="mlbam_id"))

milb_stats3 <- milb_stats2 %>%
  arrange(player_id, season) %>%
  select(player_id, player_name, season, norm_sba, norm_tr, norm_sgr, tot_pa, age2023) %>%
  group_by(player_id) %>%
  mutate(lag_sba = lag(norm_sba),
         lag_tr = lag(norm_tr),
         lag_s2gbo = lag(norm_sgr),
         lag_pa = lag(tot_pa),
         cum_pa = tot_pa + lag_pa,
         curr_pa_weight = tot_pa / cum_pa,
         prev_pa_weight = lag_pa / cum_pa) %>%
  mutate(wa_sba = norm_sba*weight*curr_pa_weight + lag_sba*(1-weight)*prev_pa_weight,
         wa_triple_rate = norm_tr*weight*curr_pa_weight + lag_tr*(1-weight)*prev_pa_weight,
         wa_s2gbo = norm_sgr*weight*curr_pa_weight + lag_s2gbo*(1-weight)*prev_pa_weight) %>%
  rename(age = age2023) %>%
  filter(season == 2023) %>%
  drop_na()

pred_sprint_speed <- predict.gam(mod2, milb_stats3)  

final_df <- milb_stats3
final_df$pred_sprint_speed <- as.vector(pred_sprint_speed)

final_df <- final_df %>%
  select(player_name, player_id, pred_sprint_speed) %>%
  arrange(-pred_sprint_speed)

x <- df %>%
  filter(year == 2023) %>%
  select(player_id, sprint_speed, competitive_runs) 

just_milb <- left_join(final_df, x, by = "player_id") %>% 
  filter(is.na(sprint_speed) == TRUE) %>%
  select(player_name, player_id, pred_sprint_speed)

sapply(just_milb, class)

table1 <- just_milb %>%
  ungroup() %>%
  slice(1:10) %>%
  mutate(team = c("STL", "TB", "WSH", "TOR", "KC", "NYM", "TB", "BAL", "BOS", "HOU"),
         player_image = paste0("https://img.mlbstatic.com/mlb-photos/image/upload/c_fill,g_auto/w_360/v1/people/", player_id, "/headshot/milb/current")) %>%
  left_join(mlbplotR::load_mlb_teams(), by = c("team"="team_abbr")) %>%
  select(player_image, player_name,team_logo_espn, pred_sprint_speed) %>%
  gt() %>%
  tab_header(title = md("**Fastest Estimated Minor Leaguers**"),
             subtitle = md("2023 Season")) %>%
  fmt_number("pred_sprint_speed", decimals = 1) %>%
  gt_img_rows(player_image, height = 25) %>%
  gt_img_rows(team_logo_espn, height = 25) %>%
  cols_label(pred_sprint_speed = "Sprint Speed (ft/sec)",
             player_image = "",
             team_logo_espn = "",
             player_name = "Name")

gtsave(table1, "fastest_milb2.png")

# compare to mlb data 

comb_df <- left_join(final_df, x, by = "player_id") %>% 
  drop_na(sprint_speed) 

comb_df$diff <- round(comb_df$sprint_speed - comb_df$pred_sprint_speed,4)

MLmetrics::RMSE(comb_df$pred_sprint_speed, comb_df$sprint_speed)

plot_data2 <- data.frame(SprintSpeed = comb_df$sprint_speed, FittedValues = comb_df$pred_sprint_speed,
                         Name = comb_df$player_name)

selected_names <- c("Elly De La Cruz", "Isan Diaz", "Vidal Bruján", "Isan Díaz")  
selected_names2 <- c("Brenton Doyle") 

ggplot(plot_data2, aes(x = SprintSpeed, y = FittedValues)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_label_repel(data = subset(plot_data2, Name %in% selected_names), 
                   aes(label = Name), nudge_x = -0.35, nudge_y = 0.4, 
                   arrow = arrow(type = "closed", length = unit(0.02, "npc")), 
                   box.padding = 0.5, 
                   point.padding = 0.5) +
  geom_label_repel(data = subset(plot_data2, Name %in% selected_names2), 
                   aes(label = Name), nudge_x = 0.35, nudge_y = -0.35, 
                   arrow = arrow(type = "closed", length = unit(0.02, "npc")), 
                   box.padding = 0.5, 
                   point.padding = 0.5) +
  ggtitle("Sprint Speed vs Estimated Sprint Speed", 
          subtitle = "Players Who Played Triple A and MLB in 2023") +
  xlab("Sprint Speed") + ylab("Estimated Sprint Speed")

ggsave("sprint_speed_pred2.png")

