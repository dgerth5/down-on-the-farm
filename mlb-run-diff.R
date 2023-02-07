# testing out the repo / r-project and working on my graphics from the post
# https://downonthefarm.substack.com/p/how-to-plotting-run-differentials

library(tidyverse)
library(mlbplotR)
library(ggrepel)

theme_scott <- function () {
  
  theme_minimal(base_size=9) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(
        fill = "aliceblue", color = "aliceblue"
      ),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      plot.margin = margin(10, 10, 20, 10) 
    )
  
}

df = read_csv("https://raw.githubusercontent.com/svanlent/run_differential/main/run_diff_22.csv", show_col_types = FALSE)
teams_colors_logos = load_mlb_teams()
df = df %>% 
  left_join(., teams_colors_logos, by = 'team_name') %>%
  mutate(record = paste0("(",wins,"-", losses, ")"))

head(df)         

teams_to_label = c("AZ", "WSH", "BOS", "OAK", "PIT", "COL", "LAD", "HOU", "NYY", "CLE", "NYM", "STL")

df %>% 
  ggplot(., aes(x = rs, y =  ra, 
                label = ifelse(team_abbr %in% teams_to_label, record, ''))) +
  geom_mlb_logos(aes(team_abbr = team_abbr), width = 0.06) +
  scale_x_continuous(breaks = seq(450, 900, 50), limits=c(450, 900)) +
  scale_y_reverse(breaks = seq(450, 900, 50), limits=c(900, 450)) +
  geom_text_repel(
    min.segment.length = .1, 
    nudge_x = .15, 
    box.padding = 0.9, 
    max.overlaps = Inf, 
    alpha = .9
  ) +
  geom_hline(yintercept=670, linetype="dashed", size = .2) +
  geom_vline(xintercept=670, linetype="dashed", size = .2) +
  labs(
    title="2022 MLB Run Differentials by Club",
    x ="Runs Scored", 
    y = "Runs Scored Against") +
  theme_scott() +
  theme(
    legend.position = 'none', 
    plot.title.position = 'plot', 
    plot.title = element_text(
      face = 'bold', 
      size = 18, 
      color = "#CB4C4E", 
      hjust = 0.5
    ), 
    plot.subtitle = element_text(
      size = 12, 
      face = 'bold', 
      hjust = 0.5
    )
  ) 

# extracting pythag

df$wp = df$wins / (df$wins + df$losses)

# correct algebra 
# https://mail.google.com/mail/u/0/?tab=rm&ogbl#search/conor+mcquiston/FMfcgzGrcFpNmkDnnFgHnnSLlhdHtmQV

mod2 = df %>% lm(log(wp) ~ log(rs/(rs+ra)), .) # this is incorrect algebra but better fit than the correct algebra
summary(mod2)

df$x_winper = exp(predict(mod2, df, type = "response"))
df$x_wins = df$x_winper*162

df_fit = df %>% 
  mutate(win_d = round(wins - x_wins, 0)) %>%
  as_tibble

team_colors_long = c(
  'Texas Rangers' = '#002D72',
  'Detroit Tigers' = '#0C2340',
  'Los Angeles Dodgers' = '#002F6C',
  'Los Angeles Angels' = '#BA0021',
  'Minnesota Twins' ='#0C2340',
  'New York Mets' = '#002D72',
  'Arizona Diamondbacks' = '#A71930',
  'St. Louis Cardinals' = '#BA0C2F',
  'Cleveland Guardians' = '#D50032',
  'Baltimore Orioles' = '#FC4C02',
  'Toronto Blue Jays' = '#134A8E',
  'Houston Astros' = '#EB6E1F',
  'New York Yankees' = '#0C2340',
  'Atlanta Braves' = '#BA0C2F',
  'Colorado Rockies' = '#330072',
  'Seattle Mariners' = '#00685E',
  'Milwaukee Brewers' = '#13294B',
  'Kansas City Royals' = '#1A4784',
  'Pittsburgh Pirates' = '#FFC72C',
  'Washington Nationals' = '#BA122B',
  'Chicago Cubs' = '#002F6C',
  'San Francisco Giants' = '#FA4616',
  'San Diego Padres' = '#041E42',
  'Boston Red Sox' = '#C8102E',
  'Miami Marlins' = '#ED6F2E',
  'Cincinnati Reds' = '#D50032',
  'Oakland Athletics' = '#034638',
  'Tampa Bay Rays' = '#8FBCE6',
  'Philadelphia Phillies' = '#BA0C2F',
  'Chicago White Sox' = '#27251F'
)

df_fit %>%
  mutate(team_abbr = paste0(team_abbr, " ", win_d)) %>%
  ggplot(., aes(x = wins, y =  x_wins, 
                label = team_abbr, fill = team_name)) +
  scale_x_continuous(breaks = seq(50, 120, 5), limits=c(50, 120)) +
  scale_y_continuous(breaks = seq(50, 120, 5), limits=c(50, 120)) +
  geom_point(size = 3) + 
  scale_fill_manual(values=team_colors_long) +
  geom_point(shape = 21, size = 5, colour = "black") +
  geom_text_repel(
    min.segment.length = .1, 
    nudge_x = .15, 
    box.padding = 0.9, 
    max.overlaps = Inf, 
    alpha = .9
  ) +
  theme_scott() +
  theme(
    legend.position = 'none', 
    plot.title.position = 'plot', 
    plot.title = element_text(
      face = 'bold', 
      size = 18, 
      color = "#CB4C4E", 
      hjust = 0.5
    ), 
    plot.subtitle = element_text(
      size = 12, 
      face = 'bold', 
      hjust = 0.5
    )
  ) +
  labs(
    title="2022 MLB Wins vs. Expected Wins",
    subtitle = "Expected Wins Based on Run Differential",
    x ="Actual Wins", 
    y = "Expected Wins") +
  geom_smooth(
    method="lm", 
    se=FALSE, 
    formula=y~x, 
    colour="red", 
    linetype = "dashed", 
    fill = NA
  )
