library(readr)
milb_pbp_2023 <- read_csv("milb_pbp_2023.csv")

dbl_a <- milb_pbp_2023 %>%
  filter(home_level_name == "Double-A") %>%
  select(pitchData.coordinates.x)

summary(dbl_a)


milb_pbp_2023$pitchData.coordinates.x

unique(milb_pbp_2023$details.event)
unique(milb_pbp_2023$details.description)


"Called Strike"  
"Ball" 

cs_df <- milb_pbp_2023 %>%
  filter(details.description %in% c("Called Strike", "Ball")) %>%
  mutate(is_strike = if_else(details.description == "Called Strike", 1, 0)) %>%
  select(is_strike, pitchData.coordinates.x, pitchData.coordinates.y, matchup.batter.id) 

cs_df$matchup.batter.id <- as.factor(cs_df$matchup.batter.id)

mod <- glm(is_strike ~  pitchData.coordinates.x * pitchData.coordinates.y,
           data = cs_df,
           family = "binomial")

p <- predict(mod, cs_df, type = "response")
p2 <- ifelse(p > .5, 1, 0)

Metrics::accuracy(cs_df$is_strike, p2)

train_pool <- catboost.load_pool(data = cs_df[, -1], label = cs_df$is_strike)

params <- list(
  iterations = 500,
  loss_function = "Logloss",
  eval_metric = "Logloss",
  verbose = 250
)

cb_model <- catboost.train(learn_pool = train_pool, params = params)
cb_predictions <- catboost.predict(cb_model, train_pool, prediction_type = "Probability")

p3 <- ifelse(cb_predictions > .5, 1, 0)

Metrics::accuracy(cs_df$is_strike, p3)


