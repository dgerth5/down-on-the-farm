library(readr)
library(tidyverse)
library(lme4)
library(sampleSelection)

fg_aaa <- read_csv("C:/Users/david/Downloads/fangraphs-minor-league-leaders (33).csv")
fg_aa <- read_csv("C:/Users/david/Downloads/fangraphs-minor-league-leaders (34).csv")
fg_higha <- read_csv("C:/Users/david/Downloads/fangraphs-minor-league-leaders (35).csv")
fg_a <- read_csv("C:/Users/david/Downloads/fangraphs-minor-league-leaders (36).csv")

df <- left_join(fg_aa, fg_aaa, by = "PlayerId")
df2 <- df %>%
  select(Name.x, PlayerId, `wRC+.x`, PA.x, HR.x, PA.y, HR.y) %>%
  mutate(next_lvl = if_else(is.na(PA.y) == TRUE, 0, 
                            if_else(PA.y < 100, 0, 1)),
         single_per = HR.y / PA.y,
         single_per1 = HR.x / PA.x) %>%
  filter(PA.x > 100)

mod_prob <- glm(next_lvl ~ `wRC+.x`,
                data = df2,
                family = binomial(link = "probit"),
                weights = round(PA.x^(1/2),0))

df2$mills <- dnorm(predict(mod_prob)) / pnorm(predict(mod_prob))

df3 <- df2 %>% 
  filter(next_lvl == 1)

mod_lm <- lm(single_per ~ single_per1, data = df3)
summary(mod_lm)

df3$p <- predict(mod_lm, df3)


