library(readr)
library(tidyverse)
library(brms)

single_a_pbp2023 <- read_csv("single_a_pbp2023.csv")

se_milb <- c("In play, out(s)","Swinging Strike","Foul","In play, run(s)","In play, no out",
             "Foul Tip","Foul Bunt","Swinging Strike (Blocked)","Foul Pitchout", "Missed Bunt")

we_milb <- c("Swinging Strike","Swinging Strike (Blocked)", "Missed Bunt")

fsl <- single_a_pbp2023 %>%
  filter(home_league_name == "Florida State League") %>%
  filter(type == "pitch") %>%
  filter(details.type.code == "FF") %>%
  mutate(se = if_else(details.description %in% se_milb, 1, 0),
         we = if_else(details.description %in% we_milb, 1, 0),
         hh = if_else(is.na(hitData.launchSpeed)==TRUE, 0, 
                      if_else(hitData.launchSpeed < 95 , 0 , 1))) %>%
  group_by(matchup.batter.fullName) %>%
  summarise(pitches_thrown = n(),
            sum_swing = sum(se),
            sum_whiff = sum(we),
            sum_hh = sum(hh),
            mean_velo = mean(pitchData.startSpeed, na.rm = TRUE),
            mean_spin_rate = mean(pitchData.breaks.spinRate, na.rm = TRUE),
            mean_vmov = mean(pitchData.breaks.breakVerticalInduced, na.rm = TRUE),
            mean_hmov = mean(pitchData.breaks.breakHorizontal, na.rm = TRUE)) %>%
  mutate(no_swing = pitches_thrown - sum_swing,
         sum_contact = sum_swing - sum_whiff,
         sum_weak_hit = sum_contact - sum_hh) %>%
  filter(pitches_thrown > 110)

sum(fsl$sum_hh) / sum(fsl$sum_contact)


mod <- brm(sum_whiff | trials(sum_swing) ~ mean_velo + mean_spin_rate + mean_vmov*mean_hmov + sum_swing,
           data = fsl,
           family = binomial("logit"),
           chains = 1,
           iter = 1000)

print(mod, digits = 4)


fsl$response <- with(fsl, cbind(sum_whiff, sum_swing - sum_whiff))

model <- glm(response ~ mean_velo + mean_spin_rate + mean_vmov*mean_hmov + sum_swing,
             family = binomial(link = "logit"), 
             data = fsl)
summary(model)
