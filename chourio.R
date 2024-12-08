library(baseballr)
library(readr)
library(gt)
library(mlbplotR)
library(gtExtras)
library(tidyverse)
library(gridExtra) # optional

chourio <- fg_milb_batter_game_logs("sa3015704", 2023)

jc_slash <- chourio %>%
  filter(Level == "(AA)") %>%
  arrange(Date) %>%
  mutate(cumba = cumsum(H) / cumsum(AB),
         cumobp = (cumsum(H) + cumsum(BB) + cumsum(HBP)) / (cumsum(AB) + cumsum(BB) + cumsum(HBP) + cumsum(SF)),
         cumslg = (cumsum(`1B`) + 2*cumsum(`2B`) + 3*cumsum(`3B`) + 4*cumsum(HR))/cumsum(AB)) %>%
  select(Date, cumba, cumobp, cumslg)

jc_slash$Date <- as.Date(jc_slash$Date)

p1 <- ggplot(jc_slash, aes(x = Date)) +
  geom_line(aes(y = cumba, color = "BA"), size = 1) +
  geom_line(aes(y = cumobp, color = "OBP"), size = 1) +
  geom_line(aes(y = cumslg, color = "SLG"), size = 1) +
  scale_color_manual(values = c("BA" = "black", "OBP" = "blue", "SLG" = "red")) +
  labs(title = "Jackson Chourio Cumulative Slash Line",
       x = "Month",
       y = "Percentage",
       color = "Stat") +
  theme_minimal()

jc_bbk <- chourio %>%
  filter(Level == "(AA)") %>%
  arrange(Date) %>%
  mutate(cumbb = cumsum(BB),
         cumk = cumsum(SO),
         cumpa = cumsum(PA),
         rollbb = cumbb / cumpa,
         rollk = cumk / cumpa) %>%
  select(Date, rollbb, rollk)

jc_bbk$Date <- as.Date(jc_bbk$Date)

p2 <- ggplot(jc_bbk, aes(x = Date)) +
  geom_line(aes(y = rollbb, color = "BB%"), size = 1) +
  geom_line(aes(y = rollk, color = "K%"), size = 1) +
  scale_color_manual(values = c("BB%" = "blue", "K%" = "red")) +
  labs(title = "Jackson Chourio BB% and K%",
       x = "Month",
       y = "Percentage",
       color = "Stat") +
  theme_minimal()

grid.arrange(p1,p2)

ggsave("chourio_grid.png", plot = grid.arrange(p1,p2))

doublea20 <- read_csv("C:/Users/david/Downloads/fangraphs-minor-league-leaders (30).csv")

table1 <- doublea20 %>%
  mutate(tm = c("TEX","NYY","AZ","CHC","LAA","AZ","CWS","MIL","TB","MIL","WSH")) %>%
  left_join(mlbplotR::load_mlb_teams(), by = c("tm"="team_abbr")) %>%
  select(Name, team_logo_espn, Age, PA, BABIP, `BB%`, `K%`, `GB/FB`, `SwStr%`, Spd, ISO, `wRC+`) %>%
  arrange(-`wRC+`) %>%
  gt() %>%
  tab_header(title = md("**Double-A Hitters Age 20 and Under**"),
             subtitle = md("Min: 300 PA. Season: 2023")) %>%
  fmt_percent(c("BB%","K%","SwStr%"), decimals = 0) %>%
  fmt_number(c("BABIP","ISO"), decimals = 3) %>%
  fmt_number(c("wRC+"), decimals = 0) %>%
  fmt_number(c("GB/FB","Spd"), decimals = 2) %>%
  gt_img_rows(team_logo_espn, height = 30) %>%
  gt_highlight_rows(rows = 7, font_weight = "bold", fill = "gold") %>%
  cols_label(team_logo_espn = "") 

gtsave(table1, "chourio_tbl.png")

gtsave()