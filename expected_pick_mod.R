library(readxl)
library(tidyverse)
library(survival)

fangraphs_grades <- read_excel("~/fangraphs_grades_db.xlsx")

clean_grades <- fangraphs_grades %>%
  filter(Hit != 0) %>%
  mutate(f_hit = as.factor(substr(Hit, nchar(Hit) - 1, nchar(Hit))),
         f_gpow = as.factor(substr(Game, nchar(Game) - 1, nchar(Game))),
         f_rpow = as.factor(substr(Raw, nchar(Raw) - 1, nchar(Raw))),
         f_spd = as.factor(substr(Spd, nchar(Spd) - 1, nchar(Spd))),
         f_fld = as.factor(substr(Fld, nchar(Fld) - 1, nchar(Fld))),
         school_type = if_else(substr(`Player Type`, 1, 1) == "C", "Col", "HS"),
         pick_num = as.numeric(substring(Pick, 1, regexpr("/", Pick) - 1)),
         adj_pick_num = if_else(is.na(pick_num)==TRUE, 101, if_else(pick_num > 100, 101, pick_num)), 
         ovr100 = if_else(is.na(pick_num)==TRUE, 1, if_else(pick_num > 100, 1, 0))) 

summary(lm(adj_pick_num ~ FV + Age, data = clean_grades))

surv_mod = coxph(Surv(adj_pick_num, event = ovr100, type = "right") ~ FV + Age, data = clean_grades)
summary(surv_mod)
