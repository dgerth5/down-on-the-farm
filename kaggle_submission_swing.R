library(data.table)
library(tidyverse)

dt <- read_csv("C:\\Users\\david\\Downloads\\nwds-xswing\\train.csv")

just_vars <- dt %>%
  group_by(game_pk, at_bat_number) %>%
  arrange(pitch_number) %>%
  mutate(adj_plate_x = if_else(p_throws == "R", plate_x, plate_x*-1),
         lag_plate_x = lag(adj_plate_x),
         lag_plate_z = lag(plate_z),
         lag_plate_z = lag(plate_z),
         lag_rel_speed = lag(release_speed),
         diff_speed1 = release_speed - lag_rel_speed,
         distance_btw_prev = sqrt((adj_plate_x - lag_plate_x)^2 + (plate_z - lag_plate_z)^2),
         adj_pfx_x = if_else(p_throws == "R", pfx_x, pfx_x*-1)) %>%
  mutate_at(c("pitcher","batter","strikes"), factor) %>%
  drop_na(distance_btw_prev, diff_speed1)


library(lme4)

swing_mod <- glm(swing ~  strikes + adj_plate_x*plate_z + distance_btw_prev + release_speed*diff_speed1 + adj_pfx_x*pfx_z,
                   data = just_vars,
                   family = "binomial")

summary(swing_mod)

swing_mod2 <- glmer(swing ~ strikes + adj_plate_x*plate_z + distance_btw_prev + release_speed*diff_speed1 + adj_pfx_x*pfx_z + (1|batter) + (1|pitcher), 
                   data = just_vars,
                   family = "binomial",
                   nAGQ=0,
                   control=glmerControl(optimizer = "nloptwrap"),
                   verbose = TRUE)

summary(swing_mod2)

library(catboost)

df2 <- just_vars %>%
  ungroup() %>%
  select(swing, strikes , adj_plate_x, plate_z , distance_btw_prev , release_speed, diff_speed1 , adj_pfx_x, pfx_z, batter, pitcher)

mm <- model.matrix(swing ~., data = df2)
y <- as.integer(as.factor(df2$swing))

train_pool <- catboost.load_pool(mm, y)

cat_mod <- catboost.train(train_pool,
                          params = list(loss_function = 'Logloss',
                                        iterations = 100, metric_period=1))

fi <- catboost.get_feature_importance(cat_mod)
