library(ggplot2)

# High-A

df <- data.frame(statistic = c("K%","BB%","GB%","Oppo%"),
                 percentile = c(100,32,24,99),
                 percentile_lbl = c("34.9% (100)", "10.3% (32)", "37.1% (24)", "38.1% (99)"))

df$statistic <- factor(df$statistic, levels = unique(df$statistic))

ggplot(df, aes(x = statistic, y = percentile)) + 
  geom_bar(stat = "identity", fill = "#FD5A1E") +
  geom_text(aes(label = percentile_lbl), position = position_stack(vjust = 0.5), color = "black") +
  ggtitle("Hayden Birdsong",
          "Season: 2023. Min 100 IP") +
  xlab("Statistic") + ylab("Percentile") 

ggsave("birdsong-percentiles.png")
