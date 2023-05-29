library(gt)
library(dplyr)

df <- data.frame(pt = c("FF","CH","SL","CB"),
                 BA = c(.325, .225, .302, .350),
                 SLG = c(.735, .400, .698, .850),
                 Chase = c(20, 23, 18, 17),
                 Whiff = c(15, 37, 25, 21),
                 n = c(529,209,200,115))

gt(df) %>%
  tab_header(title = md("**Dalton Rushing vs. Selected Pitch Types**"),
             subtitle = md("Season: 2022")) %>%
  cols_label(pt = "Pitch Type",
             Chase = "Chase%",
             Whiff = "Whiff%",
             n = "Pitches Seen")
               