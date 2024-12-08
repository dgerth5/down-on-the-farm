library(tidyverse)
library(gt)

df <- data.frame(Pitch = c("FB", "CH", "SL"),
                 Velo = c(95.0, 83.3, 85.9),
                 Usage = c(0.44, .29, .26),
                 Chase = c(.24, .40, .41),
                 Whiff = c(.22, .46, .36))
bremner <- df %>%
  gt() %>%
  tab_header(md("**Tyler Bremner Arsenal Stats**"),
             md("Season: 2024")) %>%
  fmt_percent(c("Usage","Chase","Whiff"), decimals = 0) %>%
  cols_label(Chase = "Chase%",
             Whiff = "Whiff%")

gtsave(bremner, "bremner.png")
