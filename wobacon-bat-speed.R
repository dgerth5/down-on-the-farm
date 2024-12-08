library(readr)
library(tidyverse) 
library(lubridate)
library(catboost)
library(ggrepel)
library(mlbplotR)
library(gt)
library(gtExtras)
library(webshot2)

# load data

statcast21 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast21.csv")
statcast22 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast22.csv")
statcast23 <- read_csv("C:/Users/david/PycharmProjects/pythonProject/statcast23.csv")

sc <- bind_rows(statcast21, statcast22, statcast23)

# data cleaning

wanted_events <- c("home_run","double","triple","single")

bbs <- sc %>%
  drop_na(launch_angle, launch_speed)  %>%
  filter(description == "hit_into_play") %>%
  mutate(bbe = if_else(!events %in% wanted_events, "out", events),
         vy_f = -sqrt(vy0^2 - (2*ay*(50-17/12))),
         t = (vy_f - vy0) / ay,
         vz_f = vz0 + (az*t),
         vaa = -atan(vz_f/vy_f)*(180/pi)) %>%
  select(bbe, launch_speed, launch_angle, pfx_x, pfx_z, plate_x, plate_z, release_pos_x, release_pos_z,
         release_spin_rate, release_speed, vaa, home_team)

summary(bbs$launch_speed)

bbs$home_team <- as.integer(as.factor(bbs$home_team))
bbs$bbe <- as.integer(as.factor(bbs$bbe))

# hr = 2
# out = 3
# single = 4
# double = 1 
# triple = 5

# modeling 

train_pool <- catboost.load_pool(bbs[,-1], label = bbs$bbe)

pms <- list(iterations = 1000,
            loss_function = "MultiClass")

cat_mod <- catboost.train(train_pool, params = pms)

feature_importance <- catboost.get_feature_importance(cat_mod, pool = train_pool, type = 'FeatureImportance')
feature_importance_df <- as.data.frame(feature_importance)
feature_importance_df$Variable <- rownames(feature_importance_df)
ggplot(feature_importance_df, aes(x = Variable, y = V1)) +
  geom_bar(stat = 'identity') +
  coord_flip() + 
  theme_minimal() +
  ggtitle('Feature Importance')

# test pool 

test_df <- sc %>%
  mutate(yr = year(game_date)) %>%
  filter(yr == 2023) %>%
  drop_na(launch_angle, launch_speed)  %>%
  filter(description == "hit_into_play") %>%
  mutate(bbe = if_else(!events %in% wanted_events, "out", events),
         vy_f = -sqrt(vy0^2 - (2*ay*(50-17/12))),
         t = (vy_f - vy0) / ay,
         vz_f = vz0 + (az*t),
         vaa = -atan(vz_f/vy_f)*(180/pi)) %>%
  select(bbe, launch_speed, launch_angle, pfx_x, pfx_z, plate_x, plate_z, release_pos_x, release_pos_z,
         release_spin_rate, release_speed, vaa, home_team, batter)

test_df$home_team <- as.integer(as.factor(test_df$home_team))
test_df$bbe <- as.integer(as.factor(test_df$bbe))

test_pool <- catboost.load_pool(test_df[,-c(1,14)])
preds <- catboost.predict(cat_mod, test_pool, prediction_type = "Probability")

test_df$xwobacon <- preds[,1]*1.25 + preds[,2]*2 + preds[,3]*0 + preds[,4]*.9 + preds[,5]*1.6

# test pool w 2.22 mph extra

test_df2 <- sc %>%
  mutate(yr = year(game_date)) %>%
  filter(yr == 2023) %>%
  drop_na(launch_angle, launch_speed)  %>%
  filter(description == "hit_into_play") %>%
  mutate(bbe = if_else(!events %in% wanted_events, "out", events),
         vy_f = -sqrt(vy0^2 - (2*ay*(50-17/12))),
         t = (vy_f - vy0) / ay,
         vz_f = vz0 + (az*t),
         vaa = -atan(vz_f/vy_f)*(180/pi),
         ls2 = launch_speed + 2.22) %>%
  select(bbe, ls2, launch_angle, pfx_x, pfx_z, plate_x, plate_z, release_pos_x, release_pos_z,
         release_spin_rate, release_speed, vaa, home_team, batter) %>%
  rename("launch_speed" = ls2)

test_df2$home_team <- as.integer(as.factor(test_df2$home_team))
test_df2$bbe <- as.integer(as.factor(test_df2$bbe))

test_pool2 <- catboost.load_pool(test_df2[,-c(1,14)])
preds2 <- catboost.predict(cat_mod, test_pool2, prediction_type = "Probability")

test_df2$xwobacon <- preds2[,1]*1.25 + preds2[,2]*2 + preds2[,3]*0 + preds2[,4]*.9 + preds2[,5]*1.6

# summarize predictions and group together

smry1 <- test_df %>%
  group_by(batter) %>%
  summarise(xwobacon = mean(xwobacon),
            n = n())

summary(smry1$n)

smry2 <- test_df2 %>%
  group_by(batter) %>%
  summarise(xwobacon2 = mean(xwobacon))

smry <- inner_join(smry1, smry2, by = "batter") %>%
  select(batter, xwobacon, xwobacon2, n) %>%
  mutate(diff_xwobacon = xwobacon2 / xwobacon - 1) %>%
  arrange(-diff_xwobacon) %>%
  filter(n > 100)

# plot 

baseball_name <- read_csv("C:/Users/david/Downloads/razzball.csv") %>%
  select(MLBAMID, Name)

label_data <- smry %>%
  arrange(-diff_xwobacon) %>%
  slice(c(1:10, (n()-(9:0))))

ld2 <- inner_join(label_data, baseball_name, by = c("batter"="MLBAMID"))

fbld <- sc %>%
  mutate(yr = year(game_date)) %>%
  filter(yr == 2023) %>%
  drop_na(launch_angle, launch_speed)  %>%
  filter(description == "hit_into_play") %>%
  mutate(fb = if_else(bb_type == "fly_ball", 1, 0),
         ld = if_else(bb_type == "line_drive", 1, 0)) %>%
  drop_na(fb,ld) %>%
  group_by(batter) %>%
  summarise(fb_per = mean(fb),
            ld_per = mean(ld)) %>%
  mutate(fbld_dif = fb_per - ld_per)

ld3 <- inner_join(ld2, fbld, by = "batter")
ld3 <- left_join(ld3, mlbplotR::load_headshots(), by = c("batter" = "savant_id"))
ld3$team <- c("Blue Jays","Reds","Marlins","Yankees","Brewers","Giants","Dodgers","Blue Jays","Royals","Mariners",
              "Red Sox","Brewers","Blue Jays","Rangers","Reds","Braves","Cubs","Guardians","Guardians","Royals")
ld3 <- left_join(ld3, mlbplotR::load_mlb_teams(), by = c("team" = "team_mascot")) 

# table

ld3 %>%
  head(10) %>%
  select(Name, espn_headshot, team_logo_espn, xwobacon, xwobacon2, diff_xwobacon, fb_per, n) %>%
  gt() %>%
  tab_header(title = md("**Players Who Would Have Benefited the Most From More Exit Velo**"),
             subtitle = md("Season: 2023. Min 100 BBE")) %>%
  fmt_number(columns = c("xwobacon","xwobacon2"), decimals = 3) %>%
  fmt_percent(columns = c("diff_xwobacon", "fb_per"), decimals = 0) %>%
  gt_img_rows(espn_headshot, height = 40) %>%
  gt_img_rows(team_logo_espn, height = 40) %>%
  gt_color_rows("diff_xwobacon", palette = c("blue", "white", "red"), domain = c(.04, .20),  na.color = "#FFFFFF") %>%
  cols_label(espn_headshot = "",
             team_logo_espn = "",
             xwobacon = "xwOBAcon",
             xwobacon2 = "xwOBAcon +2.2 EV",
             diff_xwobacon = "%Change",
             fb_per = "FB%",
             n = "BBE") 

t2 <- ld3 %>%
  tail(10) %>%
  select(Name, espn_headshot, team_logo_espn, xwobacon, xwobacon2, diff_xwobacon, fb_per, n) %>%
  gt() %>%
  tab_header(title = md("**Players Who Would Have Benefited the Least From More Exit Velo**"),
             subtitle = md("Season: 2023. Min 100 BBE")) %>%
  fmt_number(columns = c("xwobacon","xwobacon2"), decimals = 3) %>%
  fmt_percent(columns = c("diff_xwobacon", "fb_per"), decimals = 0)  %>%
  gt_img_rows(espn_headshot, height = 40) %>%
  gt_img_rows(team_logo_espn, height = 40) %>%
  gt_color_rows("diff_xwobacon", palette = c("blue", "white", "red"), domain = c(.04, .20),  na.color = "#FFFFFF") %>%
  cols_label(espn_headshot = "",
             team_logo_espn = "",
             xwobacon = "xwOBAcon",
             xwobacon2 = "xwOBAcon +2.2 EV",
             diff_xwobacon = "%Change",
             fb_per = "FB%",
             n = "BBE") 

gtsave(t1, "t1.png")
gtsave(t2, "t2.png")

# fb vs diff xwOBAcon plot
inner_join(smry, fbld, by = "batter") %>%
  ggplot(aes(x = fb_per, y = diff_xwobacon)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'red', se = TRUE) +
  labs(title = "FB% vs Percent Change in xwOBAcon", subtitle = "Season: 2023. Min 100 BBE") +
  xlab("FB%") + ylab("%Change in xwOBAcon") +
  theme_minimal()

ggsave("g1.png")

# open driveline

driveline <- read_csv("https://raw.githubusercontent.com/drivelineresearch/openbiomechanics/main/baseball_hitting/data/poi/poi_metrics.csv")
driveline2 <- driveline %>%
  select(blast_bat_speed_mph_x, exit_velo_mph_x)

ggplot(data = driveline2, aes(x = blast_bat_speed_mph_x, y = exit_velo_mph_x)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'red', se = TRUE) +
  labs(title = "Effect of Bat Speed on Exit Velocity", subtitle = "Data from Driveline OpenBiomechanics") +
  xlab("Bat Speed") + ylab("Exit Velocity") +
  theme_minimal()

ggsave("g2.png")
