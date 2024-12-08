library(tidyverse)
library(readxl)
library(readr)

dotf_colson_plot1 <- read_excel("~/dotf-colson-plot1.xlsx")
dotf_colson_plot2 <- read_csv("C:/Users/david/Downloads/fangraphs-minor-league-leaders (15).csv")

c1 <- dotf_colson_plot1 %>%
  mutate(bb_per = percent_rank(`BB%`)*100,
         k_per = 100 - percent_rank(`K%`)*100,
         slg_per = percent_rank(SLG)*100,
         gb_per = percent_rank(`GB%`)*100,
         swstr_per = 100 - percent_rank(`SwStr%`)*100) %>%
  select(Name, bb_per, k_per, slg_per, gb_per, swstr_per) %>%
  rename("BB%" = bb_per,
         "GB%" = gb_per,
         "K%" = k_per,
         "SLG" = slg_per,
         "SwStr%" = swstr_per) %>%
  filter(Name == "Colson Montgomery") %>%
  select(-Name) %>%
  gather(key = "variable", value = "value") 

c2 <- dotf_colson_plot2 %>%
  mutate(bb_per = percent_rank(`BB%`)*100,
         k_per = 100 - percent_rank(`K%`)*100,
         slg_per = percent_rank(SLG)*100,
         gb_per = percent_rank(`GB%`)*100,
         swstr_per = 100 - percent_rank(`SwStr%`)*100) %>%
  select(Name, bb_per, k_per, slg_per, gb_per, swstr_per) %>%
  rename("BB%" = bb_per,
         "GB%" = gb_per,
         "K%" = k_per,
         "SLG" = slg_per,
         "SwStr%" = swstr_per) %>%
  filter(Name == "Colson Montgomery") %>%
  select(-Name) %>%
  gather(key = "variable", value = "value") 


p1 <- ggplot(c1, aes(x = variable, y = value)) +
  geom_bar(stat = "identity") + 
  coord_cartesian(ylim = c(0, 100)) + # Adjust visible range without removing data
  labs(x = "Statistic", y = "Percentile", title = "Colson Montgomery vs Carolina League (A)", subtitle = "Season: 2022, Min 150 PA") +
  theme_minimal()

p2 <- ggplot(c2, aes(x = variable, y = value)) +
  geom_bar(stat = "identity") + 
  coord_cartesian(ylim = c(0, 100)) + # Adjust visible range without removing data
  labs(x = "Statistic", y = "Percentile", title = "Colson Montgomery vs South Atlantic League (A+)", subtitle = "Season: 2022, Min 150 PA") +
  theme_minimal()

library(gridExtra)

?grid.arrange
grid.arrange(p1, p2, nrow = 1)
