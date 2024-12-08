library(tidyverse)
library(readr)
library(splines)
library(sampleSelection)

statcast18 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast18.csv")
statcast19 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast19.csv")
statcast20 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast20.csv")
statcast21 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast21.csv")
statcast22 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast22.csv")
statcast23 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast23.csv")

fg_war <- read_csv("C:/Users/david/Downloads/fangraphs-leaderboards (5).csv")

sc <- bind_rows(statcast18, 
                statcast19,
                statcast20,
                statcast21,
                statcast22,
                statcast23)

bbe <- sc %>%
  mutate(year = lubridate::year(game_date)) %>%
  drop_na(launch_angle, launch_speed)  %>%
  filter(description == "hit_into_play") %>%
  group_by(year, batter) %>%
  summarise(ev90 = quantile(launch_speed, .9),
            bbe = n()) %>%
  ungroup() %>%
  arrange(year) %>%
  group_by(batter) %>%
  mutate(lag_ev = lag(ev90),
         lead_ev = lead(ev90),
         join_yr = year - 1,
         key = paste0(batter, join_yr),
         next_yr = if_else(is.na(lead_ev) == TRUE, 0, 1)) %>%
  filter(year < 2023)

fg2 <- fg_war %>%
  select(Season, Name, MLBAMID, WAR, Age) %>%
  mutate(key = paste0(MLBAMID, Season))

age_spline <- bs(fg2$Age, 
                 degree = 3,
                 df = 12)

colnames(age_spline) <- paste0("age", 1:12)

spline_df <- cbind(fg2[,-5], age_spline)

mod_df <- inner_join(bbe, spline_df, by = "key")

mod <- heckit(
  selection = next_yr ~ WAR + age1 + age2 + age3 + age4 + age5 + age6 + age7 + age8 + age9 + age10 + age11 + age12,
  outcome = lead_ev ~ ev90,
  method = "2step",
  data = mod_df
)

summary(mod)

mod1 <- lm(lead_ev ~ ev90, data = mod_df)

p_heck <- predict(mod, mod_df)
p_lm <- predict(mod1, mod_df)

p_df <- data.frame(mod_df$lead_ev, p_heck, p_lm) 
p_df2 <- p_df %>% drop_na()

MLmetrics::RMSE(p_df2$p_heck, p_df2$mod_df.lead_ev)
MLmetrics::RMSE(p_df2$p_lm, p_df2$mod_df.lead_ev)

