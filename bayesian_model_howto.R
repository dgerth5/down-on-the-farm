library(dplyr)
library(ggplot2)
library(bayesrules)
library(baseballr)

# get pitcher stats for all single a players in 2023

pitcher_stats <- mlb_stats(
  stat_type = "season",
  sport_id = 14,
  season = 2023,
  stat_group = "pitching",
  player_pool = "All") %>% 
  mutate(k_rate = strike_outs / batters_faced) %>%
  filter(batters_faced > 30) # limit to over 30 batters faced

summary(pitcher_stats$k_rate) # mean: 25%

mean(pitcher_stats$strike_outs) 
mean(pitcher_stats$batters_faced)

plot_beta(1,3, mean=TRUE) + ggtitle("Beta(1,3)")
plot_beta(44,133, mean=TRUE) + ggtitle("Beta(44,133)")
plot_beta(8,24, mean=TRUE) + ggtitle("Beta(8,24)")

#alpha, beta, y, and N. N is the total number of batters faced, not the number of non strikeouts like it is with alpha/beta

plot_beta_binomial(8, 24, 10, 15, posterior = FALSE)
plot_beta_binomial(8, 24, 10, 15, posterior = TRUE)
plot_beta_binomial(8+10, 24+5, 14, 50, posterior = TRUE)

