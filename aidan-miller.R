library(ggplot2)

df <- data.frame(statistic = c("EV90", "MaxEV", "Chase%", "SwStr%", "Pull%"),
                 Percentile = c(94, 53, 86, 80, 73),
                 Percentile_lbl = c("106.3 (94)", "108.1 (53)", "24.7% (86)", "9.9% (80)", "51.4% (73)"))

df$statistic <- factor(df$statistic, levels = unique(df$statistic))

ggplot(df, aes(x = statistic, y = Percentile)) + 
  geom_bar(stat = "identity", fill = "#6F263D") +
  geom_text(aes(label = Percentile_lbl), position = position_stack(vjust = 0.5), color = "#6BACE4") +
  ggtitle("Aidan Miller FSL Summary",
          "Season: 2024") +
  xlab("Statistic") + ylab("Percentile") 

parchment_color <- "#F5E6CC"  # This is an example of a light tan, resembling parchment

ggplot(df, aes(x = statistic, y = Percentile)) + 
  geom_bar(stat = "identity", fill = "#6F263D") +
  geom_text(aes(label = Percentile_lbl), position = position_stack(vjust = 0.5), color = "#6BACE4") +
  ggtitle("Aidan Miller FSL Summary", "Season: 2024") +
  xlab("Statistic") + ylab("Percentile") +
  theme(
    panel.background = element_rect(fill = parchment_color),  # Set the background color of the plot area
    plot.background = element_rect(fill = parchment_color, color = NA)  # Set the background color of the entire plot
  )

ggsave("aidanmiller.png")
