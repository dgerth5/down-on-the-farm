library(readxl)
library(gt)
library(gtExtras)

victor_scott_tbl <- read_excel("~/victor_scott_tbl.xlsx")


tbl1 <- victor_scott_tbl %>%
  mutate(wRC = c(117,119,"-")) %>%
  gt() %>%
  tab_header(title = md("**Victor Scott II Statline**"),
             subtitle = md("2023 Season")) %>%
  gt_add_divider(c("Level","SLG","K%","SB%")) %>%
  fmt_percent(c("BB%","K%","SB%"), decimals = 1)

gtsave(tbl1, "victor_scott_tbl.png")
