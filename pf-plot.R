library(readr)
milb_park_effects22 <- read_csv("milb_park_effects22.csv")

library(gt)
library(tidyverse)

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

just_hr <- milb_park_effects22 %>%
  select(team, home_run_pf, home_run_mult) %>%
  arrange(-home_run_pf) %>%
  rename(Team = team,
         `Home Run Park Factor` = home_run_pf,
         `Home Run Multiplier` = home_run_mult) %>%
  slice(1:5)

gt(just_hr) %>%
  tab_header(title = md("**Top 10 Home Run Parks**"),
             subtitle = md("Season: 2022")) %>%
  cols_align(everything(), align = "center")