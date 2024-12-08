library(readr)
library(tidyverse)
statcast23 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast23.csv")
single_a_pbp2023 <- read_csv("single_a_pbp2023.csv")

# estimating release position model

release_point_mod <- lm(cbind(release_pos_x, release_pos_z) ~ plate_x + plate_z + vx0 + vy0 + vz0 + ax + ay + az, data = statcast23)
summary(release_point_mod)

pred_release_pos <- predict(release_point_mod, statcast23)

statcast23$est_release_x <- pred_release_pos[,1]
statcast23$est_release_z <- pred_release_pos[,2]

#milb
# plate_x -> pitchData.coordinates.pX, plate_z -> pitchData.coordinates.pZ
# vx0 -> pitchData.coordinates.vX0, vy0 -> pitchData.coordinates.vY0, vz0 -> pitchData.coordinates.vZ0	
# ax -> pitchData.coordinates.aX, ay -> pitchData.coordinates.aY, az -> pitchData.coordinates.aZ

single_a1 <- single_a_pbp2023 %>%
  filter(home_league_name == "Florida State League") %>%
  rename("plate_x" = pitchData.coordinates.pX,
         "plate_z" = pitchData.coordinates.pZ,
         "vx0" = pitchData.coordinates.vX0,
         "vy0" = pitchData.coordinates.vY0,
         "vz0" = pitchData.coordinates.vZ0,
         "ax" = pitchData.coordinates.aX,
         "ay" = pitchData.coordinates.aY,
         "az" = pitchData.coordinates.aZ)

pred_release_pos_milb <- predict(release_point_mod, single_a1)

single_a1$est_release_x <- pred_release_pos_milb[,1]
single_a1$est_release_z <- pred_release_pos_milb[,2]


# events

se_mlb <- c("swinging_strike", "foul", "hit_into_play", "foul_tip", "swinging_strike_blocked",
            "foul_bunt", "missed_bunt")

we_mlb <- c("swinging_strike", "swinging_Strike_blocked", "missed_bunt")

se_milb <- c("In play, out(s)","Swinging Strike","Foul","In play, run(s)","In play, no out",
        "Foul Tip","Foul Bunt","Swinging Strike (Blocked)","Foul Pitchout", "Missed Bunt")

we_milb <- c("Swinging Strike","Swinging Strike (Blocked)", "Missed Bunt")

# model

unique(statcast23$pitch_type)
unique(single_a1$details.type.code)

fb <- c("FF","FC","SI","FS")

mod_df_mlb <- statcast23 %>%
  mutate(se = if_else(description %in% se_mlb, 1, 0),
         we = if_else(description %in% we_mlb, 1, 0),
         pfx_x = pfx_x*12,
         pfx_z = pfx_z*12) %>%
  filter(pitch_type %in% fb) %>%
  select(description, se, we, release_pos_x, release_pos_z, plate_x, plate_z, pfx_x, pfx_z, release_speed, release_extension, release_spin_rate) %>%
  filter(se == 1) %>%
  drop_na()

mod_df_milb <- single_a1 %>%
  mutate(se = if_else(details.description %in% se_milb, 1, 0),
         we = if_else(details.description %in% we_milb, 1, 0)) %>%
  filter(details.type.code %in% fb) %>%
  select(matchup.pitcher.fullName, details.description, se, we, est_release_x, est_release_z, plate_x, plate_z, 
         pitchData.breaks.breakHorizontal, pitchData.breaks.breakVerticalInduced, pitchData.startSpeed, pitchData.extension, pitchData.breaks.spinRate) %>%
  rename("pfx_x" = pitchData.breaks.breakHorizontal,
         "pfx_z" = pitchData.breaks.breakVerticalInduced,
         "release_speed" = pitchData.startSpeed,
         "release_extension" = pitchData.extension,
         "release_spin_rate" = pitchData.breaks.spinRate) %>%
  filter(se == 1) %>%
  drop_na()

mod_mlb <- glm(we ~ pfx_x*pfx_z + release_speed + release_extension + release_spin_rate,
               data = mod_df_mlb,
               family = "binomial")
summary(mod_mlb)

mod_milb <- glm(we ~ pfx_x*pfx_z + release_speed + release_extension + release_spin_rate,
                data = mod_df_milb,
                family = "binomial")

summary(mod_milb)


milb_p <- mod_df_milb
milb_p$pred_mlb_mod <- predict(mod_mlb, milb_p, type = "response")
milb_p$pred_milb_mod <- predict(mod_milb, milb_p, type = "response")
milb_p$diff <- milb_p$pred_mlb_mod - milb_p$pred_milb_mod

smry <- milb_p %>%
  group_by(matchup.pitcher.fullName) %>%
  summarise(avg_diff = mean(diff), n = n())




# Extract coefficients
coef_mlb <- coef(summary(mod_mlb))
coef_milb <- coef(summary(mod_milb))

# Calculate odds ratios
odds_ratios_mlb <- exp(coef(summary(mod_mlb))[, "Estimate"])
odds_ratios_milb <- exp(coef(summary(mod_milb))[, "Estimate"])

# Comparison of odds ratios
odds_ratio_comparison <- data.frame(odds_ratios_mlb, odds_ratios_milb)
odds_ratio_comparison$Difference = round(odds_ratio_comparison$odds_ratios_mlb - odds_ratio_comparison$odds_ratios_milb, 2)



# Install and load the rms package
install.packages("rms")
library(rms)

# Fit the model using lrm function from rms for better calibration support
model_mlb <- lrm(we ~ plate_x*plate_z + pfx_x*pfx_z + release_speed + release_extension + release_spin_rate, data = mod_df_mlb, x=TRUE, y=TRUE)
model_milb <- lrm(we ~ plate_x*plate_z + pfx_x*pfx_z + release_speed + release_extension + release_spin_rate, data = mod_df_milb, x=TRUE, y=TRUE)

# Calibration curves
cal_mlb <- calibrate(model_mlb, method="boot", B=100)
cal_milb <- calibrate(model_milb, method="boot", B=100)

# Plot calibration curves
plot(cal_mlb)
plot(cal_milb)

