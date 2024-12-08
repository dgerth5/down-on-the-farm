library(readr)
library(tidyverse)

statcast_data2024 <- read_csv("C:/Users/david/Downloads/statcast_data2024.csv")

mod_ff <- statcast_data2024 %>%
  filter(pitch_type == "FF") %>%
  mutate(release_pos_x = if_else(p_throws == "R", release_pos_x, release_pos_x*-1)) %>%
  select(pfx_z, release_pos_x, release_pos_z) 

m <- lm(pfx_z ~ release_pos_x*release_pos_z, data = mod_ff)

ff <- statcast_data2024 %>% 
  filter(pitch_type == "FF") %>%
  mutate(pred_pfx_z = predict(m, mod_ff)*12,
         pfx_z = pfx_z*12) %>%
  select(player_name, pitcher, pfx_z, release_pos_z, pred_pfx_z) %>%
  drop_na() %>%
  mutate(pfx_z_ovr_exp = pfx_z - pred_pfx_z) %>%
  group_by(pitcher, player_name) %>%
  summarise(avg_ivb = mean(pfx_z),
            avg_rel_height = mean(release_pos_z),
            avg_diff = mean(pfx_z_ovr_exp),
            n = n()) %>%
  filter(n > 50)

ff$ivb_perc <- round(percent_rank(ff$avg_ivb)*100,0)
ff$rel_height_perc <- round(percent_rank(ff$avg_rel_height)*100,0)
ff$diff_perc <- round(percent_rank(ff$avg_diff)*100,0)
#ff$ivb_vs_diff <- abs(ff$ivb_perc - ff$diff_perc)

summary(ff$diff_from_avg)
