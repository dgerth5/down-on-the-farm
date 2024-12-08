library(ggplot2)

# High-A

df <- data.frame(statistic = c("EV90","SweetSpot%","Whiff/Swing Rate","Swing Rate","Sprint Speed"),
                 percentile = c(90,7,4,79,98),
                 percentile_lbl = c("106.4 MPH (90)", "24.5% (7)", "43.8% (4)", "50.9% (79)", "28.8 ft/sec (98)"))

df$statistic <- factor(df$statistic, levels = unique(df$statistic))

ggplot(df, aes(x = statistic, y = percentile)) + 
  geom_bar(stat = "identity", fill = "#333366") +
  geom_text(aes(label = percentile_lbl), position = position_stack(vjust = 0.5), color = "white") +
  ggtitle("Greg Jones Triple-A Batted Ball Data",
          "2023 Season") +
  xlab("Statistic") + ylab("Percentile") 

ggsave("greg-jones-percentiles.png")
