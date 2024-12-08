library(ggplot2)

df <- data.frame(statistic = c("K%","BB%","GB%","HR/FB","Oppo%"),
                 percentile = c(91, 18, 86, 87, 98))

df$statistic <- factor(df$statistic, levels = unique(df$statistic))

f <- ggplot(df, aes(x = statistic, y = percentile)) + 
  geom_bar(stat = "identity", fill = "#0E3386") +
  geom_text(aes(label = percentile), position = position_stack(vjust = 0.5), color = "white") +
  ggtitle("Jackson Ferris vs Carolina League (A)",
          "Season: 2023. Min 50 IP") +
  xlab("Statistic") + ylab("Percentile") + 
  theme_minimal()

ggsave("ferris.png")
