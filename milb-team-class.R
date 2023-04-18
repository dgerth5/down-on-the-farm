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

mod <- Mclust(just_rats)
summary(mod)

#plot(mod)

just_rats_p <- predict(mod, just_rats)

just_rats$p <- as.factor(just_rats_p[["classification"]])

smry <- just_rats %>%
  group_by(p) %>%
  summarise(mean_pow = mean(pow_rat),
            mean_pat = mean(pat_rat),
            mean_sba = mean(sba_rat),
            teams = n()) %>%
  mutate(group = c("A","B","C")) %>%
  ungroup() %>%
  select(group, mean_pow, mean_pat, mean_sba, teams)

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

mean(df2$`1`)
mean(df2$`2`)
mean(df2$`3`)


xgrid <- expand.grid(df2$Aff, df2$Aff)

xgrid2 <- left_join(xgrid, df2, by = c("Var1" = "Aff")) %>%
  rename(Var1_1 = `1`, Var1_2 = `2`, Var1_3 = `3`)

xgrid3 <- left_join(xgrid2, df2, by = c("Var2" = "Aff")) %>%
  rename(Var2_1 = `1`, Var2_2 = `2`, Var2_3 = `3`)

xgrid4 <- xgrid3 %>%
  filter(Var1 != Var2)

# google share
#just_sim_scores <- xgrid4 %>% select(Var1, Var2, sim_score)
#write_csv(just_sim_scores, "dotf_sim_scores.csv")

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

cleaner_ss2 <- xgrid4 %>%
  filter(Var1 == "LAD") %>%
  select(Var2, sim_score) %>%
  arrange(sim_score) %>%
  rename(Team2 = Var2, `Similarity Score` = sim_score) %>%
  head(5)

cleaner_ss3 <- xgrid4 %>%
  filter(Var1 == "LAD") %>%
  select(Var2, sim_score) %>%
  arrange(-sim_score) %>%
  rename(Team3 = Var2, `Similarity Score1` = sim_score) %>%
  head(5)

ss3 <- cbind(cleaner_ss3, cleaner_ss2)

gt(ss3) %>%
  tab_header(title = md("**Organizations Closest and Farthest From the Dodgers**"),
             subtitle = md("Seasons: 2018-2022")) %>%
  fmt_number(columns = c("Similarity Score","Similarity Score1"), decimals = 2) %>%
  gt_add_divider(columns = c("Similarity Score1"), weight = px(3)) %>%
  cols_label(Team2 = "Closest Orgs",
             Team3 = "Furthest Orgs",
             `Similarity Score1` = "Similarity Score")
  
xgt(smry) %>%
  tab_header(title = md("**Group Averages**")) %>%
  fmt_number(columns = c("mean_pow","mean_pat","mean_sba"), decimals = 2) %>%
  cols_label(group = "Group",
             mean_pow = "Average Power",
             mean_pat = "Average Patience",
             mean_sba = "Average Speed",
             teams = "Teams")

fastest_orgs <- df2 %>% 
  ungroup() %>%
  select(Aff, `1`) %>% 
  arrange(-`1`)

power_orgs <- df2 %>% 
  ungroup() %>%
  select(Aff, `2`) %>% 
  arrange(-`2`)

pat_orgs <- df2 %>% 
  ungroup() %>%
  select(Aff, `3`) %>% 
  arrange(-`3`)

combin_df <- cbind(head(fastest_orgs,5) %>% rename(Org = Aff), 
                   head(power_orgs,5) %>% rename(Org. = Aff), 
                   head(pat_orgs,5) %>% rename(Org.. = Aff))

gt(combin_df) %>%
  tab_header(title = md("**Organization Leaders By Hitting Style**")) %>%
  gt_add_divider(columns = c("1","2"), weight = px(3)) %>%
  cols_label(Org. = "Org",
             Org.. = "Org",
             `1` = "A %",
             `2` = "B %",
             `3` = "C %") %>%
  fmt_number(columns = c(`3`,`1`,`2`), decimals = 2)

stl <- df2 %>%
  filter(Aff == "STL") %>%
  ungroup()

gt(stl) %>%
  tab_header(title = md("**St. Louis Cardinals Hitting Style**")) %>%
  fmt_number(columns = c(`1`,`2`,`3`), decimals = 2) %>%
  cols_label(Aff = "Org",
             `1` = "A",
             `2` = "B",
             `3` = "C")
