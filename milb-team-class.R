library(readxl)
library(tidyverse)
dotf_team_class2 <- read_excel("~/dotf-team-class2.xlsx")

df <- dotf_team_class2 %>%
  mutate(pow_rat = HR / H,
         pat_rat = SO / BB,
         sba_rat = (SB + CS) / (H - `2B` - `3B` - HR + BB + IBB + HBP))

y2022 <- df %>% filter(Yr == 2022)
y2021 <- df %>% filter(Yr == 2021)
y2019 <- df %>% filter(Yr == 2019)
y2018 <- df %>% filter(Yr == 2018)

library(mclust)

just_rats <- df %>% select(pow_rat, pat_rat, sba_rat) %>% drop_na()

mod <- Mclust(just_rats, G = 2)
summary(mod)

plot(mod)

just_rats_p <- predict(mod, just_rats)

just_rats$p <- as.factor(just_rats_p[["classification"]])

smry <- just_rats %>%
  group_by(p) %>%
  summarise(mean_pow = mean(pow_rat),
            mean_pat = mean(pat_rat),
            mean_sba = mean(sba_rat))

just_rats2 <- df %>% select(Aff, pow_rat, pat_rat, sba_rat) %>% drop_na()
just_rats2$p <- as.factor(just_rats_p[["classification"]])

smry2 <- just_rats2 %>%
  group_by(Aff, p) %>%
  summarise(count = n())

df2 <- smry2 %>% 
  group_by(Aff) %>% # group data by team
  mutate(total_count = sum(count)) %>% # calculate total count for each team
  mutate(factor_percentage = count / total_count) %>% # calculate percentage for each factor
  select(Aff, p, factor_percentage) %>% # select relevant columns
  spread(key = p, value = factor_percentage, fill = 0) # spread data by factor 

df2$scale1 <- abs(as.vector(scale(df2$`1`)))

