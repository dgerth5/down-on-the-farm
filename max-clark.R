library(gt)

df_power <- data.frame(Stat = c("EV90", "Max EV", "DHH%"),
                       Rate = c(100.7, 106.7, "19.7%"),
                       Percentile = c(31, 39, 44))

tbl1 <- df_power %>%
  gt() %>%
  tab_header(title = md("**Max Clark Power Numbers**"),
             subtitle = md("Season: 2024. League: Florida State League"))

gtsave(tbl1, "max-clark-power.png")

df_hit <- data.frame(Stat = c("Swing Rate", "Chase%", "Whiff%"),
                       Rate = c("38.3%", "19.7%", "18.5%"),
                       Percentile = c(13, 97, 90))

tbl2 <- df_hit %>%
  gt() %>%
  tab_header(title = md("**Max Clark Hit Numbers**"),
             subtitle = md("Season: 2024. League: Florida State League"))

gtsave(tbl2, "max-clark-hit.png")
