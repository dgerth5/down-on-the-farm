library(gt)

df <- data.frame(Statistic = c("CROE","EV90 (MPH)", "DHH", "SS%", "Whiff%", "Swing%"),
                 Rate = c("4.6%", "102.1", "27.1%", "35.7%", "21.4%", "41.9%"),
                 Percentile = c(75, 44, 73, 53, 70, 26))

g <- df %>%
  gt() %>%
  tab_header(title = md("**Jackson Holliday Triple A Data**"),
             subtitle = md("Season: 2023")) 

gtsave(g, filename = "holliday.png")

# ev90 is ~70th percentile
  

library(ggplot2)
library(readr)
library(ggrepel)

fg <- read_csv("C:/Users/david/Downloads/fangraphs-minor-league-leaders (38).csv")

fg2 <- fg %>%
  dplyr::select(Name, `hr%per`, `hr/fbper`)

ggplot(fg2, aes(x = `hr%per`, y = `hr/fbper`)) +
  geom_point(color = "black") +
  geom_point(data = subset(fg2, Name == "Jackson Holliday"), aes(x = `hr%per`, y = `hr/fbper`), color = "orange", size = 3) +
  ggtitle("HR% Percentile vs HR/FB Percentile", subtitle = "Season: 2023. Minimum 200 PA") +
  xlab("HR% Percentile") + ylab("HR/FB Percentile") +
  scale_color_manual(values = c("black", "orange")) +
  geom_smooth(method = "lm", color = "blue") +
  geom_text_repel(
    data = subset(fg2, Name == "Jackson Holliday"),
    aes(label = Name, x = `hr%per`, y = `hr/fbper`),
    segment.color = "black",
    arrow = arrow(length = unit(0.02, "npc")),
    nudge_x = -0.05,
    nudge_y = 0.025# Adjust this value as needed for better label placement
  )

ggsave("hr-percentile-holliday.png")



df2 <- data.frame(Name = c("Jackson Holliday", "Druw Jones", "Termarr Johnson", "Elijah Greene"),
                 Whiff = c(.2,.2,.26,.32),
                 Chase = c(.15,.21,.22,.27),
                 Pitches = c(515,450,657,530))  

g2 <- df2 %>%
  gt() %>%
  tab_header(title = md("**'Big 4' Showcase Hit Tool Data**")) %>%
  fmt_percent(c("Whiff","Chase"), decimals = 0)

gtsave(g2, filename = "prep-hitter-pd.png")
?gtsave
