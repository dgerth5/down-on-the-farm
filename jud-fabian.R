library(gt)

fabian <- data.frame(Year = c("2019", "2019-Cape", "2020", "2021", "2022"),
                     Whiff = c(.24,.30,.25,.36,.28),
                     Chase = c(.14,.23,.19,.21,.16),
                     zWhiff = c(.17,.21,.18,.27,.22))

gt(fabian) %>%
  tab_header(title = md("**Jud Fabian College Swing Stats**"),
             subtitle = md("Seasons: 2019-2022")) %>%
  fmt_percent(c("Whiff","Chase","zWhiff"), decimals = 0)
