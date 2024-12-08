library(ggplot2)

# High-A

df <- data.frame(statistic = c("K%","BB%","GB%","HR/FB","Spd"),
                 percentile = c(66, 85, 43, 91, 63))

df$statistic <- factor(df$statistic, levels = unique(df$statistic))

ggplot(df, aes(x = statistic, y = percentile)) + 
  geom_bar(stat = "identity", fill = "#092C5C") +
  geom_text(aes(label = percentile), position = position_stack(vjust = 0.5), color = "#F5D130") +
  ggtitle("Xavier Isaac vs Carolina League (A)",
          "Season: 2023. Min 200 PA") +
  xlab("Statistic") + ylab("Percentile") 

ggsave("xavier-isaac-single-a.png")
