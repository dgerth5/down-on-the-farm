library(readr)
library(tidyverse)
library(mixedup)
library(lme4)

hit_triple_a_21_23 <- read_csv("C:/Users/david/Downloads/hit-triple-a-21-23.csv")
hit_double_a_21_23 <- read_csv("C:/Users/david/Downloads/hit-double-a-21-23.csv")
hit_high_a_21_23 <- read_csv("C:/Users/david/Downloads/hit-high-a-21-23.csv")
hit_single_a_21_23 <- read_csv("C:/Users/david/Downloads/hit-single-a-21-23.csv")
hit_cpx_21_23 <- read_csv("C:/Users/david/Downloads/hit-cpx-21-23.csv")

master_df <- bind_rows(hit_triple_a_21_23 ,
                       hit_double_a_21_23 ,
                       hit_high_a_21_23 ,
                       hit_single_a_21_23 ,
                       hit_cpx_21_23 )

age <- master_df %>%
  group_by(Season, Level) %>%
  summarise(avg_age = mean(Age))

master_df2 <- left_join(master_df, age, by = c("Season", "Level"))

master_df3 <- master_df2 %>%
  group_by(Season, Name, PlayerId, Level) %>%
  summarise(sum1b = sum(`1B`),
            sum2b = sum(`2B`),
            sum3b = sum(`3B`),
            sumHR = sum(HR),
            sumK = sum(SO),
            sumBB = sum(BB),
            sumPA = sum(PA)) %>%
  ungroup() %>%
  mutate(single_per = sum1b / sumPA,
         double_per = sum2b / sumPA,
         triple_per = sum3b / sumPA,
         homer_per = sumHR / sumPA,
         k_per = sumK / sumPA,
         bb_per = sumBB / sumPA,
         next_season = Season + 1,
         next_lvl = case_when(Level == "AAA" ~ "MLB",
                              Level == "AA" ~ "AAA",
                              Level == "A+" ~ "AA",
                              Level == "A" ~ "A+")) %>%
  filter(sumPA > 0)

s2021 <- master_df3 %>% filter(Season == 2021) %>% 
  group_by(Level) %>% 
  mutate(scale_single_per = scale(single_per),
         scale_double_per = scale(double_per),
         scale_triple_per = scale(triple_per),
         scale_homer_per = scale(homer_per),
         scale_k_per = scale(k_per),
         scale_bb_per = scale(bb_per))
s2022 <- master_df3 %>% filter(Season == 2022) %>% 
  group_by(Level) %>% 
  mutate(scale_single_per = scale(single_per),
         scale_double_per = scale(double_per),
         scale_triple_per = scale(triple_per),
         scale_homer_per = scale(homer_per),
         scale_k_per = scale(k_per),
         scale_bb_per = scale(bb_per))
s2023 <- master_df3 %>% filter(Season == 2023) %>% 
  group_by(Level) %>% 
  mutate(scale_single_per = scale(single_per),
         scale_double_per = scale(double_per),
         scale_triple_per = scale(triple_per),
         scale_homer_per = scale(homer_per),
         scale_k_per = scale(k_per),
         scale_bb_per = scale(bb_per))

j1 <- left_join(s2021, s2022, by = c("PlayerId", "next_season"="Season","next_lvl"="Level")) %>% drop_na()
j2 <- left_join(s2022, s2023, by = c("PlayerId", "next_season"="Season","next_lvl"="Level")) %>% drop_na()

j_master <- bind_rows(j1, j2) %>%
  filter(sumPA.x > 80) %>%
  filter(sumPA.y > 80) %>%
  mutate(lvl_change = paste0(Level, "-", next_lvl)) 
#  filter(lvl_change == "AA-AAA") %>%
 # select(k_per.y, k_per.x, sumPA.x, lvl_change) 

j_master2 <- bind_rows(j1, j2) %>%
  filter(sumPA.x > 80) %>%
  filter(sumPA.y > 80) %>%
  mutate(lvl_change = paste0(Level, "-", next_lvl),
         diff_1b = single_per.y / single_per.x - 1,
         diff_2b = double_per.y / double_per.x - 1, 
         diff_3b = triple_per.y / triple_per.x - 1, 
         diff_hr = homer_per.y / homer_per.x - 1, 
         diff_bb = bb_per.y / bb_per.x - 1,
         diff_k = k_per.y  / k_per.x - 1) %>%
  group_by(lvl_change) %>%
  summarise(m1b = mean(diff_1b),
            # m2b = mean(diff_2b),
            # m3b = mean(diff_3b),
            # mhr = mean(diff_hr),
            mbb = mean(diff_bb),
            mk = mean(diff_k),
            n = n())

#j_master$lvl_change <- as.factor(j_master$lvl_change)

mod <- lm(bb_per.y ~ bb_per.x*lvl_change, data = j_master, weights = sumPA.x)
summary(mod)

library(readr)
library(tidyverse)
library(mixedup)
library(lme4)

hit_triple_a_21_23 <- read_csv("C:/Users/david/Downloads/hit-triple-a-21-23.csv")
hit_double_a_21_23 <- read_csv("C:/Users/david/Downloads/hit-double-a-21-23.csv")
hit_high_a_21_23 <- read_csv("C:/Users/david/Downloads/hit-high-a-21-23.csv")
hit_single_a_21_23 <- read_csv("C:/Users/david/Downloads/hit-single-a-21-23.csv")
hit_cpx_21_23 <- read_csv("C:/Users/david/Downloads/hit-cpx-21-23.csv")

master_df <- bind_rows(hit_triple_a_21_23 ,
                       hit_double_a_21_23 ,
                       hit_high_a_21_23 ,
                       hit_single_a_21_23 ,
                       hit_cpx_21_23 )

age <- master_df %>%
  group_by(Season, Level) %>%
  summarise(avg_age = mean(Age))

master_df2 <- left_join(master_df, age, by = c("Season", "Level"))

master_df3 <- master_df2 %>%
  mutate(adj_age = Age - avg_age,
         single_per = `1B` / PA,
         double_per = `2B` / PA,
         triple_per = `3B` / PA,
         homer_per = HR / PA,
         k_per = SO / PA,
         bb_per = BB / PA,
         next_season = Season + 1,
         next_lvl = case_when(Level == "AAA" ~ "MLB",
                              Level == "AA" ~ "AAA",
                              Level == "A+" ~ "AA",
                              Level == "A" ~ "A+")) %>%
  filter(PA > 0)

s2021 <- master_df3 %>% filter(Season == 2021)
s2022 <- master_df3 %>% filter(Season == 2022)
s2023 <- master_df3 %>% filter(Season == 2023)

j1 <- left_join(s2021, s2022, by = c("PlayerId", "next_season"="Season","next_lvl"="Level")) %>% drop_na()
j2 <- left_join(s2022, s2023, by = c("PlayerId", "next_season"="Season","next_lvl"="Level")) %>% drop_na()

j_master <- bind_rows(j1, j2) %>%
  filter(PA.x > 200) %>%
  filter(PA.y > 200) %>%
  mutate(lvl_change = paste0(Level, "-", next_lvl))
  #select(homer_per.y, homer_per.x, PA.x)

#j_master$lvl_change <- as.factor(j_master$lvl_change)

mod <- lm(cbind(single_per.y, double_per.y, triple_per.y, homer_per.y, bb_per.y, k_per.y) ~ 
            single_per.x+ double_per.x+ triple_per.x+ homer_per.x+ bb_per.x+ k_per.x, 
          data = j_master, weights = PA.x)
summary(mod)

wt <- c(5,  5,  4,  1)/15
x <- c(3.7,3.3,3.5,2.8)
xm <- weighted.mean(x, wt)


