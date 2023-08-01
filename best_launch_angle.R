library(readr)
library(tidyverse)
statcast23 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast23.csv")

swing_events <- c(
  "foul_tip", "swinging_strike", "swinging_strike_blocked", 
  "missed_bunt", "foul", "hit_into_play", "foul_bunt", "bunt_foul_tip"
)

whiff_events <- c(
  "swinging_strike", "foul_tip", "foul_bunt", 
  "missed_bunt", "swinging_strike_blocked"
)

full_mlb <- statcast23 %>%
  mutate(is_swing = if_else(description %in% swing_events, 1, 0)) %>%
  filter(is_swing == 1) %>%
  mutate(clean_event = if_else(description == "foul", "foul",
                               if_else(description == "hit_into_play", bb_type, description)),
         clean_event2 = if_else(clean_event == "swinging_strike_blocked", "swinging_strike", 
                                if_else(clean_event == "foul_tip", "foul", clean_event))) %>%
  filter(!str_detect(clean_event2, "bunt")) %>%
  drop_na(clean_event2, delta_run_exp, launch_angle)

run_vals <- full_mlb %>%
  group_by(clean_event2) %>%
  summarise(mean_delta_rv = mean(delta_run_exp))

# average la_by zone

la_by_zone <- statcast23 %>%
  drop_na(launch_angle, zone) %>%
  group_by(zone) %>%
  summarise(mean_la = mean(launch_angle))

relevant2 <- full_mlb %>%
  select(launch_angle, zone, clean_event2)

train_pool <- catboost.load_pool(relevant2[,-3], label = as.integer(as.factor(relevant2$clean_event2)))

pms <- list(iterations = 1000,
            od_type = "IncToDec",
            od_wait = 50,
            loss_function = "MultiClass")

mod1 <- catboost.train(train_pool, params = pms)

j <- levels(as.factor(relevant2$clean_event2))

print(data.frame(event = j, lvl = 1:length(j)))


pred_df <- left_join(relevant2, la_by_zone, by = "zone") 
pred_df2 <- pred_df %>%
  select(mean_la, zone) %>%
  rename(launch_angle = mean_la)

pred_pool <- catboost.load_pool(pred_df2)

preds <- catboost.predict(mod1, pred_pool, prediction_type = 'Probability')

p_rv_xla <- preds[,1]*0.09145082 + preds[,2]*-0.03745866 + preds[,3]*-0.06044488 + preds[,4]*0.30251165 + preds[,5]*-0.23849855

pred_df3 <- pred_df %>%
  select(launch_angle, zone)

pred_pool2 <- catboost.load_pool(pred_df3)

preds2 <- catboost.predict(mod1, pred_pool2, prediction_type = 'Probability')

p_rv_ala <- preds2[,1]*0.09145082 + preds2[,2]*-0.03745866 + preds2[,3]*-0.06044488 + preds2[,4]*0.30251165 + preds2[,5]*-0.23849855

pred_df$xlarv <- p_rv_xla
pred_df$larv <- p_rv_ala 

mean(pred_df$xlarv - pred_df$larv)

pred_df$hitter <- full_mlb$batter

# something off here

best_swings <- pred_df %>%
  mutate(rv_above_av = larv - xlarv) %>%
  group_by(hitter) %>%
  summarise(mean_rv_above_avg = mean(rv_above_av),
            n = n())

summary(best_swings$mean_rv_above_avg)
