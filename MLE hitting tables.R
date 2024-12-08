library(gt)
library(gtExtras)
library(readxl)

dotf_hitter_mle <- read_excel("~/dotf_hitter_mle.xlsx")


hitter_mle <- dotf_hitter_mle %>%
  select(-c("k_sd_hi_lvl", "bb_sd_hi_lvl", "sing_sd_hi_lvl", "xbh_sd_hi_lvl", "hr_sd_hi_lvl")) %>%
  gt() %>%
  tab_header(title = md("**Hitter MLEs**"),
             subtitle = md("Standard Deviations are from 2023")) %>%
  fmt_percent(c("k_increase", "bb_increase", "sing_increase", "xbh_increase", "hr_increase"), decimals = 1) %>%
  gt_add_divider(level) %>%
  cols_label(level = "Level",
             k_z_score_diff = "K% Z-Score Diff",
             k_increase = "K% Diff",
             bb_z_score_diff = "BB% Z-Score Diff",
             bb_increase = "BB% Diff",
             sing_z_score_diff = "1B% Z-Score Diff",
             sing_increase = "1B% Diff",
             xbh_z_score_diff = "XBH% Z-Score Diff",
             xbh_increase = "XBH% Diff",
             hr_z_score_diff = "HR% Z-Score Diff",
             hr_increase = "HR% Diff")

gtsave(hitter_mle, "hitter_mle.png")  
            
