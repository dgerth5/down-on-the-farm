library(readr)
library(readxl)
library(tidyverse)

milb_park_effects22 <- read_csv("milb_park_effects22.csv")
milb_park_effects21 <- read_csv("milb_park_effects21.csv")
milb_park_effects19 <- read_csv("milb_park_effects19.csv")
milb_park_effects18 <- read_csv("milb_park_effects18.csv")

dotf_team_class2 <- read_excel("~/dotf-team-class2.xlsx")
dotf_team_class2$IBB[160] <- 3
dotf_team_class2$IBB[256] <- 8

y2022 <- dotf_team_class2 %>% filter(Yr == 2022)
y2021 <- dotf_team_class2 %>% filter(Yr == 2021)
y2019 <- dotf_team_class2 %>% filter(Yr == 2019)
y2018 <- dotf_team_class2 %>% filter(Yr == 2018)

df22 <- left_join(milb_park_effects22, y2022, by = c("team" = "Team"))
df21 <- left_join(milb_park_effects21, y2021, by = c("team" = "Team"))
df19 <- left_join(milb_park_effects19, y2019, by = c("team" = "Team"))
df18 <- left_join(milb_park_effects18, y2018, by = c("team" = "Team"))

final <- rbind(df22, df21, df19, df18) %>% drop_na()

final2 <- final %>%
  mutate(`1B` = H - `2B` - `3B` - HR,
         adj1B = `1B` / (single_mult*.5 + .5),
         adj2B = `2B` / (double_mult*.5 + .5),
         adj3B = `3B` / (triple_mult*.5 + .5),
         adjHR = HR / (home_run_mult*.5 + .5),
         pow_rat = adjHR / (adj1B + adj2B + adj3B + adjHR),
         pat_rat = BB / SO,
         sba_rat = (SB + CS) / (adj1B + BB + IBB + HBP))

sum(final2$adjHR)
sum(final2$HR)

library(mclust)

just_rats <- final2 %>% select(pow_rat, pat_rat, sba_rat)

mod <- Mclust(just_rats)
summary(mod)

#plot(mod)

just_rats_p <- predict(mod, just_rats)

just_rats$p <- as.factor(just_rats_p[["classification"]])

smry <- just_rats %>%
  group_by(p) %>%
  summarise(mean_pow = mean(pow_rat),
            mean_pat = mean(pat_rat),
            mean_sba = mean(sba_rat))

just_rats2 <- final2 %>% select(Aff, pow_rat, pat_rat, sba_rat) %>% drop_na()
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


xgrid <- expand.grid(df2$Aff, df2$Aff)

xgrid2 <- left_join(xgrid, df2, by = c("Var1" = "Aff")) %>%
  rename(Var1_1 = `1`, Var1_2 = `2`, Var1_3 = `3`)

xgrid3 <- left_join(xgrid2, df2, by = c("Var2" = "Aff")) %>%
  rename(Var2_1 = `1`, Var2_2 = `2`, Var2_3 = `3`)

xgrid4 <- xgrid3 %>%
  filter(Var1 != Var2)

cosine_sim <- function(x,y){
  
  num <- x %*% y 
  denom <- sqrt(sum(x^2)*sum(y^2)) 
  return(num / denom)
  
}

var1 <- xgrid4[,3:5]
var2 <- xgrid4[,6:8]

sim_score <- c()

for (i in 1:length(xgrid4$Var1)){
  
  ss <- cosine_sim(as.numeric(var1[i,]), as.numeric(var2[i,]))
  
  sim_score[i] <- ss
  
}

xgrid4$sim_score <- sim_score

library(gt)

cleaner_ss <- xgrid4 %>%
  filter(Var1 == "LAD") %>%
  select(Var1, Var2, sim_score) %>%
  arrange(-sim_score) %>%
  rename(Team1 = Var1, Team2 = Var2, `Similarity Score` = sim_score)

gt(head(cleaner_ss,5)) %>%
  tab_header(title = md("**Orgs Furthest From the Dodgers**")) %>%
  fmt_number(columns = "Similarity Score", decimals = 2)
#
#

  
  
