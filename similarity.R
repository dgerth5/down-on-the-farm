library(readr)
stuff_leader <- read_csv("C:/Users/david/Downloads/FanGraphs Leaderboard (14).csv")

stuff_leader[is.na(stuff_leader)] <- 0

stuff2 <- stuff_leader %>%
  select(Name, `botStf FA`, `botStf SI`, `botStf FC`, `botStf FS`, `botStf SL`, `botStf CU`, `botStf CH`, `botStf KC`,
         botERA)

stuff3 <- expand.grid(stuff2, stuff2)

stuff2 <- stuff_leader %>%
  select(Name, `botStf FA`, `botStf SI`, `botStf FC`, `botStf FS`, `botStf SL`, `botStf CU`, `botStf CH`, `botStf KC`,
         botERA)

stuff2_list <- split(stuff2, seq(nrow(stuff2)))

stuff3 <- expand.grid(stuff2_list, stuff2_list)

typeof(stuff3)
names(stuff3) <- c('Row1', 'Row2')

stuff3$Row1 <- do.call(rbind, stuff3$Row1)
stuff3$Row2 <- do.call(rbind, stuff3$Row2)

stuff3_df <- cbind(stuff3$Row1, stuff3$Row2)

colnames(stuff3_df) <- c(paste0("Name1_", colnames(stuff3$Row1)), paste0("Name2_", colnames(stuff3$Row2)))


stuff4 <- stuff3_df %>% 
  filter(Name1_Name != Name2_Name) %>%
  mutate(cosine_sim = (`Name1_botStf FA`* `Name2_botStf FA` + `Name1_botStf SI`* `Name2_botStf SI` + `Name1_botStf FC`* `Name2_botStf FC` + `Name1_botStf FS`* `Name2_botStf FS` + `Name1_botStf SL`* `Name2_botStf SL` + `Name1_botStf CU`* `Name2_botStf CU` + `Name1_botStf CH`* `Name2_botStf CH` + `Name1_botStf KC`* `Name2_botStf KC`) / (sqrt(`Name1_botStf FA`^2 + `Name1_botStf SI`^2 + `Name1_botStf FC`^2 + `Name1_botStf FS`^2 + `Name1_botStf SL`^2 + `Name1_botStf CU`^2 + `Name1_botStf CH`^2 + `Name1_botStf KC`^2)*sqrt(`Name2_botStf FA`^2 + `Name2_botStf SI`^2 + `Name2_botStf FC`^2 + `Name2_botStf FS`^2 + `Name2_botStf SL`^2 + `Name2_botStf CU`^2 + `Name2_botStf CH`^2 + `Name2_botStf KC`^2)))

stuff5 <- stuff4 %>% filter(cosine_sim > .9)
cor(stuff5$Name2_botERA, stuff5$Name1_botERA)