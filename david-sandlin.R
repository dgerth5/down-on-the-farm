library(ggplot2)

df <- data.frame(statistic = c("K%","BB%","GB%","HR/FB","Oppo%"),
                 percentile = c(93, 92, 19, 3, 80))

df$statistic <- factor(df$statistic, levels = unique(df$statistic))

ggplot(df, aes(x = statistic, y = percentile)) + 
  geom_bar(stat = "identity", fill = "#7AB2DD") +
  geom_text(aes(label = percentile), position = position_stack(vjust = 0.5), color = "black") +
  ggtitle("David Sandlin vs Carolina League (A)",
          "Season: 2023. Min 50 IP") +
  xlab("Statistic") + ylab("Percentile") + 
  theme_minimal()

ggsave("sandlin-single-a.png")

?geom_text
x