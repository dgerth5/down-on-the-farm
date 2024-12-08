library(tidyverse)
library(lubridate)
library(readr)
library(baseballr)
library(stringr)

statcast18 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast18.csv")
statcast19 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast19.csv")
statcast20 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast20.csv")
statcast21 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast21.csv")
statcast22 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast22.csv")
statcast23 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast23.csv")

sc <- bind_rows(statcast18, statcast19, statcast20, statcast21, statcast22, statcast23)

smry1 <- sc %>%
  mutate(yr = year(game_date)) %>%
  drop_na(launch_speed) %>%
  group_by(batter, yr) %>%
  summarise(ev90 = quantile(launch_speed, 0.9),
            bbe = n()) %>%
  filter(bbe > 30)

r <- read_csv("C:/Users/david/Downloads/SFBB Player ID Map - PLAYERIDMAP (2).csv")
p <- read_csv("C:/Users/david/Downloads/SFBB Player ID Map - PLAYERIDMAP (2).csv") %>%
  select(MLBID,BIRTHDATE) %>%
  mutate(yr = str_sub(BIRTHDATE,-4,-1)) 

p$yr <- as.numeric(p$yr)
p2 <- p %>% filter(is.na(yr) == FALSE) %>% select(MLBID, yr)

smry2 <- left_join(smry1, p2, by = c("batter"="MLBID"))

smry3 <- smry2 %>%
  arrange(batter, yr.x) %>%
  group_by(batter) %>%
  mutate(age = yr.x - yr.y,
         age2 = yr.x - yr.y - 1,
         rolling_count = row_number(),
         lag_ev = lag(ev90)) %>%
  ungroup() %>%
  drop_na()

msadf <- lm(ev90 ~ lag_ev, data = smry3)
summary(msadf)

smry3$batter2 <- as.integer(as.factor(smry3$batter))

smry4 <- smry3 %>%
  select(ev90, lag_ev, age2, batter, batter2)

library(splines)

b <- bs(smry4$age2,
        knots = seq(from = min(smry4$age2), to = max(smry4$age2), length = 10),
        degree = 3,
        intercept = FALSE)
b <- b[,-13]

library(rstan)

dat <- list(N = length(smry4$ev90),
            B = ncol(b),
            P = length(unique(smry4$batter)),
            y = smry4$ev90,
            ev90_t1 = smry4$lag_ev,
            spline_mat = b,
            batter = smry4$batter2)

stan_mod <- stan_model("ev-aging-stan.stan")

fit <- sampling(object = stan_mod,
                data = dat,
                chains = 4, iter = 2000,
                refresh = 1000)

?sampling

library(bayesplot)

posterior_samples <- extract(fit)
yrep <- posterior_samples$y_sim

# yrep3 <- pmax(yrep,0)
# yrep3 <- pmin(yrep3,115)
# 
# summary(yrep3[,3])

sim_mean <- colMeans(yrep)
summary(sim_mean)
MLmetrics::RMSE(sim_mean, smry4$ev90)

ppc_dens_overlay(y = smry4$ev90, yrep = yrep)

print(fit, pars = c("alpha","beta1"))

max(yrep)
min(yrep)

mean(smry4$lag_ev)
38.32 + .61*99.82

write_csv(smry4, "ev-aging-data.csv")
