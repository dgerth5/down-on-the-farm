library(mgcv)
library(tidyverse)
library(readr)

triple_a_pbp2023 <- read_csv("triple_a_pbp2023.csv")

bbe <- triple_a_pbp2023 %>%
  filter(is.na(hitData.trajectory) == FALSE) %>%
  filter(hitData.trajectory %in% c("fly_ball","ground_ball","popup","line_drive")) %>%
  mutate(gb = if_else(hitData.trajectory == "ground_ball", 1, 0),
         sh = if_else(matchup.batSide.code == matchup.pitchHand.code, 1, 0)) %>%
  select(gb, sh, pitchData.breaks.breakVertical, pitchData.breaks.breakHorizontal, pitchData.startSpeed)

gb_mod <- bam(gb ~ sh + s(pitchData.startSpeed) + s(pitchData.breaks.breakVertical, pitchData.breaks.breakHorizontal),
              data = bbe,
              family = "binomial")

df <- data.frame(pitcher = c(rep("A", 6), rep("B", 6)),
                 sh = c(rep(1,3),rep(0,3),rep(1,3),rep(0,3)),
                 pitchData.startSpeed = c(96,87.5,83.9,96,87.5,83.9,99.7,89,82.1,99.7,89,82.1),
                 pitchData.breaks.breakHorizontal = c(14,-2,-16,14,-2,-16, 2, -5, -5, 2, -5, -5),
                 pitchData.breaks.breakVertical = c(-28,-40,-43,-28,-40,-43,-8,-33,-50,-8,-33,-50))

p_gb <- predict(gb_mod, df, type = "response")
df$p_gb <- p_gb

se <- c("In play, out(s)", "In play, no out", "Foul",
        "Swinging Strike","In play, run(s)","Foul Tip","Swinging Strike (Blocked)",
        "Foul Bunt")

we <- c("Swinging Strike", "Swinging Strike (Blocked)")

whiff <- triple_a_pbp2023 %>%
  filter(isPitch == TRUE) %>%
  filter(details.call.description %in% se) %>%
  mutate(whiff = if_else(details.call.description %in% we, 1, 0),
         sh = if_else(matchup.batSide.code == matchup.pitchHand.code, 1, 0)) %>%
  select(whiff, sh, pitchData.breaks.breakVertical, pitchData.breaks.breakHorizontal, pitchData.startSpeed)

we_mod <- bam(whiff ~ sh + s(pitchData.startSpeed) + s(pitchData.breaks.breakVertical, pitchData.breaks.breakHorizontal),
              data = whiff,
              family = "binomial")

p_wh <- predict(we_mod, df, type = "response")

df$p_wh <- p_wh

oh <- df %>% filter(sh == 0)

# popups now
oh$p_gb[1]*.69 + oh$p_gb[2]*.21 + oh$p_gb[3]*.096
oh$p_gb[4]*.56 + oh$p_gb[5]*.36 + oh$p_gb[6]*.07

# a = 1.9
# b = 6.5


oh$p_wh[1]*.69 + oh$p_wh[2]*.21 + oh$p_wh[3]*.096
oh$p_wh[4]*.56 + oh$p_wh[5]*.36 + oh$p_wh[6]*.07
