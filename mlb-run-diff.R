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
