library(gt)
library(tidyr)

df <- data.frame(GS = 12,
                 IP = 34.1,
                 ERA = 5.50,
                 K = 39/148,
                 BB = 21/148,
                 WP = 10)

plt <- df %>%
  gt() %>%
  tab_header(title = md("**Jaxon Wiggins 2024 Summary**")) %>%
  fmt_number(ERA, decimals = 2) %>%
  fmt_percent(c("K", "BB"), decimals = 0) %>%
  cols_label(K = "K%", BB = "BB%")

gtsave(plt, "wiggins.png")
