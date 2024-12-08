library(gt)


df1 <- data.frame(Year = c(1,2,3, "Total"),
                  Cash = c(10,10,10,30),
                  PVCash = c(9.52,9.07,8.64,27.23))

gt(df1) %>%
  tab_header(title = md("**Cash Flow Analysis**"),
             subtitle = md("Discount Rate: 5%")) %>%
  fmt_currency(c("Cash","PVCash")) %>%
  cols_label(Cash = "Cash Flow",
             PVCash = "PV(Cash Flow)")

df2 <- data.frame(Year = c(1:8,"Total"),
                  WAR = c(rep(0,2),rep(2.5,6),15),
                  WARDol = c(rep(0,2),rep(22.5,6),135),
                  WARDolPV = c(rep(0,2), 16.90, 15.37, 13.97, 12.70, 11.55, 10.50, 80.99))

gt(df2) %>%
  tab_header(title = md("**Prospect X Production**")) %>%
  fmt_currency(c("WARDolPV","WARDol")) %>%
  cols_label(WARDol = "WAR$",
             WARDolPV = "PV(WAR$)") 

df3 <- data.frame(Year = c(1:9,"Total"),
                  WARA = c(rep(0,3),rep(1,6),6),
                  WARDolA = c(rep(0,3),rep(9,6),54),
                  WARPVA = c(0,0,0,6.15,5.59,5.08,4.62,4.20,3.82,29.45),
                  WARB = c(0,rep(1,6),rep(0,2),6),
                  WARDolB = c(0,rep(9,6), rep(0,2),54),
                  WARPVB = c(0,7.44,6.76,6.15,5.59,5.08,4.62,0,0,35.63))

gt(df3) %>%
  tab_header(title = md("**Prospect A vs. Prospect B**")) %>%
  fmt_currency(-c("Year","WARA","WARB")) %>%
  cols_label(WARA = "A-WAR",
             WARDolA = "A-WAR$",
             WARPVA = "PV(A-WAR$)",
             WARB = "B-WAR",
             WARDolB = "B-WAR$",
             WARPVB = "PV(B-WAR$)")
