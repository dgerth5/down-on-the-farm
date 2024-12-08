library(gt)
library(gtExtras)

df_pitchers <- data.frame(lc = c("AAA -> MLB", "AA -> AAA", "A+ -> AA", "A -> A+"),
                          k_z = c(-0.73, -0.56, -0.61, -0.67),
                          k_sd = c(.058, .062, .064, .065),
                          k_change = c(-0.73, -0.56, -0.61, -0.67) * c(.058, .062, .064, .065),
                          bb_z = c(.49, .46, .32, .30),
                          bb_sd = c(.033, .045, .044, .049),
                          bb_change = c(.49, .46, .32, .30) * c(.033, .045, .044, .049))

pitcher_mle <- df_pitchers %>%
  select(-c("k_sd", "bb_sd")) %>%
  gt() %>%
  tab_header(title = md("**Pitcher MLEs**"),
             subtitle = md("Standard Deviations are from 2023")) %>%
  fmt_percent(c("k_change", "bb_change"), decimals = 1) %>%
  gt_add_divider(lc) %>%
  cols_label(lc = "Level",
             k_z = "K% Z-Score Diff",
            # k_sd = "K% SD",
             k_change = "K% Diff",
             bb_z = "BB% Z-Score Diff",
            # bb_sd = "BB% SD",
             bb_change = "BB% Diff")

gtsave(pitcher_mle, "pitcher_mle.png")  
