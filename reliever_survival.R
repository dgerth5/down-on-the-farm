library(readr)
library(tidyverse)
library(baseballr)
library(ggsurvfit)
library(survival)
library(fitdistrplus)

fg_data <- read_csv("C:/Users/david/Downloads/fangraphs-leaderboards (36).csv")

get_current_players <- function(year){
  df <- map_df(c(1,11,12,13,14), ~baseballr::mlb_stats(
    stat_type = "season",
    sport_id = .x,
    season = year,
    stat_group = "pitching",
    player_pool = "All",
    limit = 2000))
  
  return(unique(df$player_id))
}

seq(2019, 2021)

get_career_stats <- function(year){
  
  yrs <- seq(year, 2024)
  
  df <- map_df(yrs, ~baseballr::mlb_stats(
    stat_type = "season",
    sport_id = 1,
    season = .x,
    stat_group = "pitching",
    player_pool = "All",
    limit = 2000))
  
  df$innings_pitched <- as.numeric(df$innings_pitched)
  
  career_summary <- df %>%
    group_by(player_id,player_full_name) %>%
    summarise(career_ip = sum(innings_pitched),
              started = sum(games_started) / sum(games_played))
  
}

curr_players <- get_current_players(2024)
career_stats <- get_career_stats(2011)

mod_df <- fg_data %>%
  mutate(curr_player = if_else(MLBAMID %in% curr_players, 1, 0),
         censor = if_else(curr_player == 1, 0, 1),
         fb_usage = coalesce(`FA% (sc)`,0) + coalesce(`FC% (sc)`,0) + coalesce(`SI% (sc)`,0),
         weighted_fb = (coalesce(`FA% (sc)`,0)/fb_usage)*coalesce(`vFA (sc)`,0) + (coalesce(`FC% (sc)`,0)/fb_usage)*coalesce(`vFC (sc)`,0) + (coalesce(`SI% (sc)`,0)/fb_usage)*coalesce(`vSI (sc)`,0)) %>%
  dplyr::select(Season, Name, PlayerId, MLBAMID, Age, IP, `ERA-`, `FIP-`, `BABIP+`, `LOB%+`, `K%+`, `BB%+`, weighted_fb, curr_player, censor) %>%
  rename(k = `K%+`, b = `BB%+`) %>%
  left_join(career_stats, by = c("MLBAMID"="player_id")) %>%
  mutate(surv_innings = career_ip - IP,
         starter = if_else(started < .3, 0 , 1)) %>%
  filter(surv_innings >0)

mod_df <- mod_df[!duplicated(mod_df[c("MLBAMID")]), ] 

not_censored = mod_df %>%
  filter(censor == 1) %>%
  filter(surv_innings > 0)
# mutate(surv_innings = surv_innings + 0.03)

fit_weibull <- fitdist(not_censored$surv_innings, "weibull")
fit_exponential <- fitdist(not_censored$surv_innings, "exp")
fit_gamma <- fitdist(not_censored$surv_innings, "gamma")

print(fit_weibull)

summary(mod_df)

#  Age (25) + IP (40) + k (107) + b (118) + weighted_fb (93)

exp(1.5 + 25*-.15 + 40*.02 + 107*.01 + 118*-.01 + 93*.07)
log(104)


comp_table <- rbind(
  Weibull = c(AIC = fit_weibull[["aic"]], BIC = fit_weibull[["bic"]]),
  Exponential = c(AIC = fit_exponential[["aic"]], BIC = fit_exponential[["bic"]]),
  Gamma = c(AIC = fit_gamma[["aic"]], BIC = fit_gamma[["bic"]])
)

print(comp_table)

par(mfrow = c(1,1))
plot(fit_weibull)
plot(fit_exponential)
plot(fit_gamma)

# weibull has best fit


mod <- survreg(Surv(surv_innings, censor, type = "right") ~ Age + IP + k + b + weighted_fb + starter,
               data = mod_df,
               dist = "weibull")

summary(mod)
anova(mod)
concordance(mod)

mm <- model.matrix( ~ Age + IP + k + b + weighted_fb + starter,
                    data = mod_df)


plot(residuals(mod, type = "deviance"))
qqnorm(residuals(mod, type = "deviance"))
qqline(residuals(mod, type = "deviance"))

new_data <- data.frame(Age = 25, IP = 30, k = 110, b = 110, weighted_fb = 93, starter = 0)
predict(mod, newdata = new_data, type = "quantile", p = c(0.1, 0.5, 0.99))

mod_df$p50 <- predict(mod, newdata = mod_df, type = "quantile", p = .5)
MLmetrics::MAE(mod_df$p50, mod_df$surv_innings)

library(survival)
library(ggplot2)

# Assuming 'mod' is your fitted model and 'new_data' is your data frame with predictors

# Step 1: Create a sequence of time points
time_seq <- seq(0, max(mod_df$surv_innings), by = 10)  # Adjust the range and step as needed

# Step 2: Calculate the survival probabilities
scale <- exp(predict(mod, newdata = new_data, type = "lp"))
shape <- 1 / mod$scale

surv_prob <- exp(-(time_seq / scale)^shape)

# Step 3: Create a data frame for plotting
plot_data <- data.frame(time = time_seq, probability = surv_prob)

# Step 4: Plot the survival curve using ggplot2
ggplot(plot_data, aes(x = time, y = probability)) +
  geom_line() +
  geom_ribbon(aes(ymin = probability, ymax = 1), alpha = 0.2) +
  labs(x = "Survival Innings", y = "Survival Probability",
       title = "Survival Curve for Weibull Regression Model") +
  theme_minimal()


# Fit Cox proportional hazards model 
cox_model <- coxph(Surv(surv_innings, censor) ~ Age + IP + k + b + weighted_fb, 
                   data = mod_df)

library(survival)
library(ggplot2)

# Create new data for prediction
# new_data <- data.frame(Age = 22, IP = 51, k = 193, b = 87, weighted_fb = 98)

# Create survival function
surv_func <- survfit(cox_model, newdata = new_data)

# Create data frame for plotting
plot_data <- data.frame(time = surv_func$time,
                        survival = surv_func$surv,
                        lower = surv_func$lower,
                        upper = surv_func$upper)

# Plot the survival curve
ggplot(plot_data, aes(x = time, y = survival)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "Survival Innings", y = "Survival Probability",
       title = "Survival Curve for Cox Proportional Hazards Model") +
  theme_minimal()


surv_curves <- survfit(cox_model, newdata = mod_df)

# Step 2: Extract the median survival time (50th percentile)
median_survival <- quantile(surv_curves, probs = 0.5)$quantile

MLmetrics::RMSE(median_survival, mod_df$surv_innings)





library(rstan)

# Prepare your data
stan_data <- list(
  N = length(mod_df$Season),
  P = 6,
  surv_innings = mod_df$surv_innings,
  censor = mod_df$censor,
  X = mm #,
#  prior_only = 1  # Set to 1 for prior predictive check
)

# Compile and run the model
fit_prior <- stan(file = "weibull_mod1.stan", 
                  data = stan_data, iter = 2000, chains = 4)

print(fit_prior, digits = 4)

# Extract prior predictive samples
y_pred_prior <- extract(fit_prior)$y_rep
ncol(y_pred_prior)
nrow(y_pred_prior)

hist(mod_df$surv_innings)

library(ggplot2)

# Convert to data frame
df_pred <- data.frame(y = c(y_pred_prior), type = "Prior Predictive")
df_obs <- data.frame(y = mod_df$surv_innings, type = "Observed")
df_all <- rbind(df_pred, df_obs)

# Plot
ggplot(df_all, aes(x = y, fill = type)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Prior Predictive Check",
       x = "y", y = "Density")
