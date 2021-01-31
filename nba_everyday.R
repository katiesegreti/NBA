###EVERY DAY!!!
library(ballr)
library(tidyverse)

setwd("~/work/nets_republic/NBA")

#get the saved history
player_history0130 <- read_csv("nba_history_013021.csv") %>%
  mutate(season = as.numeric(paste0(substr(season, 1, 2), substr(season, 6, 7)))) %>%
  select(player, pos, age, tm, g, gs, mp, fg, fga, fgpercent, x3p, x3pa, x3ppercent, x2p, x2pa, x2ppercent, efgpercent, ft, fta, ftpercent, orb, drb, trb, ast, stl, blk, tov, pf, pts, link, season)



#every day, get the current season stats.
current_season <- NBAPerGameStatistics(season = 2021)

#COMBINE TODAY'S DATA (2021 SEASON) WITH HISTORY
today_with_history <- current_season %>%
  mutate(season = 2021) %>%
  select(player, pos, age, tm, g, gs, mp, fg, fga, fgpercent, x3p, x3pa, x3ppercent, 
         x2p, x2pa, x2ppercent, efgpercent, ft, fta, ftpercent, orb, drb, trb, ast, 
         stl, blk, tov, pf, pts, link, season) %>%
  bind_rows(player_history0130) %>%
  mutate(season_team = paste0(season, " ", tm)) %>% unique()

#need another dataset that's just the current season and only one row per player
#filter to TOT for players on >1 team in 2021
player_counts_2021 <- today_with_history %>% filter(season == 2021) %>%
  count(player)

#filter to "TOT" for players who were on more than one team
players_2021 <- today_with_history %>% 
  filter(season == "2020-21") %>%
  left_join(player_counts_2021) %>%
  filter(n == 1 | tm == "TOT") %>%
  mutate(hilite = 0)

###### SO MY DATASETS TO USE WITH CHARTS AND TABLES ARE today_with_history and players_2021
##### today_with_history for stats history charts
##### players_2021 with only current season data for league compare charts

