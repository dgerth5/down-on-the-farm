library(gt)
library(dplyr)

df <- data.frame(pt = c("FB","CH","SL","CB"),
                 BA = c(.358, .323, .481, .320),
                 SLG = c(.541, .790, .815, .720),
                 Chase = c(21,29,28,31),
                 Whiff = c(9,30,28,32),
                 n = c(447,232,109,98))

gt(df) %>%
  tab_header(title = md("**Brooks Lee vs. Selected Pitch Types**"),
             subtitle = md("Season: 2022. Team: Cal Poly (NCAA-D1)")) %>%
  cols_label(pt = "Pitch Type",
             Chase = "Chase%",
             Whiff = "Whiff%",
             n = "Pitches Seen")


df2 <- data.frame(pt = c("FB","CH","SL","CB"),
                 BA = c(.257,.636,.316,.250),
                 SLG = c(.429,.636,.474,.250),
                 Chase = c(30,25,41,29),
                 Whiff = c(11,24,44,33),
                 n = c(162,39,56,27))

gt(df2) %>%
  tab_header(title = md("**Brooks Lee vs. Selected Pitch Types**"),
             subtitle = md("Season: 2022-2023. Team: Wichita Wing Surge (MIN-AA)")) %>%
  cols_label(pt = "Pitch Type",
             Chase = "Chase%",
             Whiff = "Whiff%",
             n = "Pitches Seen")
