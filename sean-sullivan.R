library(gt)


df <- data.frame(Statistic = c("IP", "ERA", "FIP", "K%", "BB%", "GB%"),
                 Percentage = c("20.2", "2.61", "0.44", "38.4%", "1.2%", "41.2%"))

df %>%
  gt()

df2 <- data.frame(IP = c(20.2),
                  ERA = c(2.61),
                  FIP = c(0.44),
                  K = c("38.4%"),
                  BB = c("1.2%"),
                  GB = c("41.2%"),
                  BABIP = c(0.451))

sullivan <- df2 %>%
  gt() %>%
  tab_header(md("**Sean Sullivan 2024 Season Summary**")) %>%
  cols_label(K = "K%",
             BB = "BB%",
             GB = "GB%")

gtsave(sullivan, "sean-sullivan.png")
