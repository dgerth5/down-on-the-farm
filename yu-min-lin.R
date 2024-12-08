library(ggplot2)

# High-A

df <- data.frame(statistic = c("K%","BB%","GB%","HR/FB","Oppo%"),
                 percentile = c(96, 55, 76, 92, 51))

df$statistic <- factor(df$statistic, levels = unique(df$statistic))

ggplot(df, aes(x = statistic, y = percentile)) + 
  geom_bar(stat = "identity", fill = "#A71930") +
  geom_text(aes(label = percentile), position = position_stack(vjust = 0.5), color = "white") +
  ggtitle("Yun-Min Lin vs Northwest League (A+)",
          "Season: 2023. Min 50 IP") +
  xlab("Statistic") + ylab("Percentile") + 
  theme_minimal()

ggsave("lin-high-a.png")


df2 <- data.frame(statistic = c("K%","BB%","GB%","HR/FB","Oppo%"),
                 percentile = c(60, 41, 69, 44, 54))

df2$statistic <- factor(df$statistic, levels = unique(df$statistic))

ggplot(df2, aes(x = statistic, y = percentile)) + 
  geom_bar(stat = "identity", fill = "#e3D4AD") +
  geom_text(aes(label = percentile), position = position_stack(vjust = 0.5), color = "black") +
  ggtitle("Yun-Min Lin vs Texas League (AA)",
          "Season: 2023. Min 50 IP") +
  xlab("Statistic") + ylab("Percentile") + 
  theme_minimal()

ggsave("lin-double-a.png")
