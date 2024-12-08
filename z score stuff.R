r <- baseballr::mlb_stats(
  stat_type = "season",
  sport_id = 11,
  season = 2023,
  stat_group = "pitching",
  player_pool = "All",
  limit = 10000) 


yr <- 2023
id_high_lvl = 1
id_lo_lvl = 11

id1 <- df %>% 
  filter(year == yr) %>%
  filter(sport_id == id_high_lvl) %>% 
  select(player_id, player_full_name, league_name, tot_pa, tot_k)

id2 <- df %>% 
  filter(year == yr) %>%
  filter(sport_id == id_lo_lvl) %>% 
  select(player_id, player_full_name, league_name, tot_pa, tot_k)

means <- rbind(id1, id2) %>%
  mutate(k_per = tot_k / tot_pa) %>%
  group_by(league_name) %>%
  summarise(mean_k = mean(k_per, na.rm = TRUE),
            sd_k = sd(k_per, na.rm = TRUE))

df1 <- left_join(id2, id1, by = "player_id") %>%
  mutate(k_perlo = tot_k.x / tot_pa.x, 
         k_perhi = tot_k.y / tot_pa.y,
         play_next_lvl = if_else(is.na(tot_pa.y)==TRUE, 0, 1)) %>%
  left_join(means, by = c("league_name.x"="league_name")) %>%
  left_join(means, by = c("league_name.y"="league_name")) %>%
  mutate(k_perhi_z = if_else(is.na(k_perhi)==TRUE, NA, (k_perhi - mean_k.y) / sd_k.y),
         k_perlo_z = if_else(is.na(k_perlo)==TRUE, NA, (k_perlo - mean_k.x) / sd_k.x)) %>%
  filter(play_next_lvl == 1) %>%
  filter(tot_pa.x > 50) %>% filter(tot_pa.y > 50)
