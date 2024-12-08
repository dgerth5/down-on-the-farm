library(tidyverse)
library(baseballr)
library(rstan)

df <- mlb_stats(stat_type = "season",
                sport_id = 1,
                season = 2024,
                stat_group = "pitching",
                player_pool = "All")

?mlb_stats

colnames(df)

df1 <- df %>%
  select(player_full_name, player_id, batters_faced, strike_outs, base_on_balls, earned_runs, innings_pitched)

df1$innings_pitched <- ceiling(as.numeric(df1$innings_pitched))

df2 <- df1 %>% 
  rename("k" = strike_outs,
         "bb" = base_on_balls,
         "er" = earned_runs,
         "bf" = batters_faced,
         "ip"= innings_pitched)

summary(df2)

# mean for K 17%
# mean for BB 10%

mod_data <- list(N = length(df1$player_full_name),
                 ip = df1$innings_pitched,
                 bf = df1$batters_faced,
                 er = df1$earned_runs,
                 k = df1$strike_outs,
                 bb = df1$base_on_balls,
                 alpha1 = 10,
                 beta1 = 47,
                 alpha2 = 6,
                 beta2 = 49)

summary(df1)


mod_stan <- stan_model("er_model_stan.stan")

samples <- sampling(mod_stan,
                    mod_data,
                    control = list(max_treedepth = 15))

print(samples)


bayesplot::mcmc_trace(samples)

shinystan::launch_shinystan(samples)
