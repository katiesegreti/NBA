library(httr)
library(tidyverse)
library(xml2)
library(rvest)
library(stringr)
library(rebus)
library(lubridate)
library(ggthemes)
library(ggdark)
library(scales)
library(extrafont)
library(shiny)
library(shinythemes)
library(reactable)
library(ggplot2)
library(extrafont)
#library(ggchicklet)
library(ballr)

#############################################################
### read in bio details
nba_bio_2021 <- read_csv("https://raw.githubusercontent.com/katiesegreti/NBA/main/nba_bios.csv") %>% unique()

#update bios for early march and fix this soon
nba_player_teams <- read_csv("https://raw.githubusercontent.com/katiesegreti/NBA/main/NBA_player_teams_0302.csv") %>% select(player, current_team) %>%
  mutate(current_team = if_else(current_team == "BKN", "BRK", current_team))

### read in history
player_history <- read_csv("https://raw.githubusercontent.com/katiesegreti/NBA/main/nba_history_013021.csv") %>%
   mutate(season = as.numeric(paste0(substr(season, 1, 2), substr(season, 6, 7)))) %>%
   select(player, pos, age, tm, g, gs, mp, fg, fga, fgpercent, x3p, x3pa, x3ppercent, x2p, x2pa, x2ppercent, efgpercent, ft, fta, ftpercent, orb, drb, trb, ast, stl, blk, tov, pf, pts, link, season)

### read in shooting data
# wnba_shooting <- read_csv("https://raw.githubusercontent.com/katiesegreti/WNBA/master/WNBA_2020_shooting_players.csv") %>%
#   mutate( player = as.factor(player),
#           tm = as.factor(tm),
#           zone = as.factor(zone),
#           stat = as.factor(stat),
#           shots = as.factor(shots)
#           )


######################################################
###############CONSOLODATE THE NUMBER OF DATASETS!!!!!
######################################################

# get the current 2020 stats 
#every day, get the current season stats.
current_season <- NBAPerGameStatistics(season = 2021)

#combine today's 2020 stats with history (also combine season and team into a column for chart making)
#COMBINE TODAY'S DATA (2021 SEASON) WITH HISTORY
today_with_history <- current_season %>%
  mutate(season = 2021) %>%
  select(player, pos, age, tm, g, gs, mp, fg, fga, fgpercent, x3p, x3pa, x3ppercent, 
         x2p, x2pa, x2ppercent, efgpercent, ft, fta, ftpercent, orb, drb, trb, ast, 
         stl, blk, tov, pf, pts, link, season) %>%
  bind_rows(player_history) %>%
  mutate(season_team = paste0(season, " ", tm)) %>% unique()

#JOIN THE LATEST STATS WITH BIO DETAILS
# wnba_today <- current_stats %>%
#   left_join(wnba_bio_2020)
# 
# wnba_today1 <- wnba_today %>%
#   mutate(season_team = paste0(season, " ", tm))

#filter to TOT for players on >1 team in 2021
player_counts_2021 <- today_with_history %>% filter(season == 2021) %>%
  count(player)

get_teams <- function(playername) {
  x <- current_season %>% filter(player == playername) %>% select(tm)
  data.frame(player = playername, team = last(x$tm))
}








#filter to "TOT" for players who were on more than one team
players_2021 <- today_with_history %>% 
  filter(season == 2021) %>%
  left_join(player_counts_2021) %>%
  filter(n == 1 | tm == "TOT") %>%
  mutate(hilite = 0)

#get the current team for each player
# 
#player_teams <- players_2021$player %>% map(function(x) get_teams(x)) %>% bind_rows() 
player_teams <- players_2021$player %>% map(function(x) get_teams(x)) %>% bind_rows() %>% 
  left_join(nba_player_teams, by = "player") %>%
  mutate(team = if_else(!is.na(current_team), current_team, team)) %>% select(player, team)



# players_2021a <- today_with_history %>%
#   filter(season == 2021) %>%
#   mutate(hilite = 0)

players_2021a <- players_2021 %>%
  left_join(player_teams) %>%
  mutate(tm = team) %>%
  select(-team)

# nba team colors
ATL <- "#e03a3e"
BRK <- "#000000"
BOS <- "#008348"
CHO <- "#00788c"
CHI <- "#ce1141"
CLE <- "#6f263d"
DAL <- "#bbc4ca"
DEN <- "#fec524"
DET <- "#c8102e"
GSW <- "#fdb927"
HOU <- "#ce1141"
IND <- "#fdbb30"
LAC <- "#1d428a"
LAL <- "#552583"
MEM <- "#5d76a9"
MIA <- "#98002e"
MIL <- "#00471b"
MIN <- "#78be20"
NOP <- "#b4975a"
NYK <- "#f58426"
OKC <- "#007ac1"
ORL <- "#c4ced4"
PHI <- "#006bb6"
PHO <- "#1d1160"
POR <- "#e03a3e"
SAC <- "#5a2b81"
SAS <- "#000000"
TOR <- "#ce1141"
UTA <- "#00471b"
WAS <- "#002b5c"

team_colors <- c("ATL" = ATL, "BRK" = BRK , "BOS" = BOS,  "CHO" = CHO, 
                 "CHI" = CHI, "CLE" = CLE, "DAL" = DAL, "DEN" = DEN, "DET" = DET,
                 "GSW" = GSW, "HOU" = HOU, "IND" = IND, "LAC" = LAC, "LAL" = LAL, 
                 "MEM" = MEM, "MIA" = MIA, "MIL" = MIL,  "MIN" = MIN, "NOP" = NOP,
                 "NYK" = NYK, "OKC" = OKC, "ORL" = ORL, "PHI" = PHI,  "PHO" = PHO, 
                 "POR" = POR, "SAC" = SAC, "SAS" = SAS, "TOR" = TOR, "UTA" = UTA, "WAS" = WAS)

team_names <- c("", "Atlanta Hawks" = "ATL", "Brooklyn Nets" = "BRK",
                "Boston Celtics" = "BOS", "Charlotte Hornets" = "CHO", "Chicago Bulls" = "CHI",
                "Cleveland Caveliers" = "CLE", "Dallas Mavericks" = "DAL", "Denver Nuggets" = "DEN",
                "Detroit Pistons" = "DET", "Golden State Warriors" = "GSW", "Houston Rockets" = "HOU",
                "Indiana Pacers" = "IND", "Los Angeles Clippers" = "LAC", "Los Angeles Lakers" = "LAL",
                "Memphis Grizzlies" = "MEM", "Miami Heat" = "MIA", "Milwaukee Bucks" = "MIL",
                "Minnesota Timberwolves" = "MIN", "New Orleans Pelicans" = "NOP", "New York Knicks" = "NYK",
                "Oklahoma City Thunder" = "OKC", "Orlando Magic" = "ORL", "Philadelphia 76ers" = "PHI",
                "Phoenix Suns" = "PHO", "Portland Trailblazers" = "POR", "Sacramento Kings" = "SAC",
                "San Antonio Spurs" = "SAS", "Toronto Raptors" = "TOR", "Utah Jazz" = "UTA", "Washington Wizards" = "WAS"
)

#colors
darkslateblue <- "#2e3c81"
crimson <- "#e73a3c"

#non-dark theme
bg_color <- "#f2f2f2"
nr_theme <- theme_wsj() +
  theme(
    text = element_text(family = "Arial"),
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    legend.position = "none",
    axis.line.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.text.x = element_text(face = "plain", family = "Arial"),
    panel.grid.major.y = element_line( colour = darkslateblue),
    plot.title = element_text(size = 22, family = "Arial"),
    legend.background = element_rect(fill = bg_color),
    plot.subtitle = element_text(size = 18, family = "Arial")
  )

#sticky style for reactable
sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                     borderRight = "1px solid #eee")

# write a function to make avg_point charts by season for a player
avgpoint_chart <- function(player_name) {
  df <- today_with_history %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist() %>% as.numeric() %>% sort()
  # get number of seasons for conditional formatting on the label sizes?
  n_seasons = length(season_labs)
  max_value = max(df$pts)
  df %>% ggplot(aes(x = season_team, y = pts)) +
    #geom_chicklet(radius = grid::unit(2, 'mm'), fill = darkslateblue) +
    geom_col(fill = darkslateblue) +
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(round(pts, 1)), 
                   y = pts + max_value / 25 ), size = 4, fill = crimson) +
    geom_text(aes(label = paste(g,"\n games")), y = max_value / 6, color = "white", size = 3) +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 4, color = "white") +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("Average Points: ", player_name),
         x = "season",
         y = "") +
    nr_theme
}

#fg % chart
fgpct_chart <- function(player_name) {
  df <- today_with_history %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist() %>% as.numeric() %>% sort()
  max_value = max(df$fgpercent)
  df %>% ggplot(aes(x = season_team, y = fgpercent)) +
    geom_col(fill = darkslateblue) +
    #geom_chicklet(radius = grid::unit(2, 'mm'), fill = darkslateblue) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + 
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(round(fgpercent, 2) * 100, "%"), 
                   y = fgpercent + max_value / 25 ), size = 4, fill = crimson) +
    geom_text(aes(label = paste0(g,"\n games")), y = max_value / 6, color = "white", size = 3) +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 4, color = "white") +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("Field Goal Percentage: ", player_name),
         x = "",
         y = "") +
    nr_theme
}

#fg % chart
threepct_chart <- function(player_name) {
  df <- today_with_history %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist() %>% as.numeric() %>% sort()
  max_value = max(df$x3ppercent)
  df %>% ggplot(aes(x = season_team, y = x3ppercent)) +
    geom_col(fill = darkslateblue) +
    #geom_chicklet(radius = grid::unit(2, 'mm'), fill = darkslateblue) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + 
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(x3ppercent * 100, "%"), 
                   y = x3ppercent + max_value / 25 ), size = 4, fill = crimson) +
    geom_text(aes(label = paste0(g,"\n games")), y = max_value / 6, color = "white", size = 3) +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 4, color = "white") +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("3 Point Percentage: ", player_name),
         x = "",
         y = "") +
    nr_theme
}



###### SO MY DATASETS TO USE WITH CHARTS AND TABLES ARE today_with_history and players_2021
##### today_with_history for stats history charts
##### players_2021 with only current season data for league compare charts


#rebound chart
rebound_chart <- function(player_name) {
  df <- today_with_history %>% filter(player == player_name & tm != "TOT") 
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist() %>% as.numeric() %>% sort()
  max_value = max(df$trb)
  df %>% ggplot(aes(x = season_team, y = trb)) +
    geom_col(fill = darkslateblue) +
    #geom_chicklet(radius = grid::unit(2, 'mm'), fill = darkslateblue) +
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(round(trb, 1),""), 
                   y = trb + max_value / 25 ), size = 4, fill = crimson) +
    geom_text(aes(label = paste0(g,"\n games")), y = max_value / 6, color = "white", size = 3) +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 4, color = "white") +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("Average Rebounds: ", player_name),
         x = "season",
         y = "") +
    nr_theme
}



#assist chart
assist_chart <-  function(player_name) {
  df <- today_with_history %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist() %>% as.numeric() %>% sort()
  max_value = max(df$ast)
  df %>% ggplot(aes(x = season_team, y = ast)) +
    geom_col(fill = darkslateblue) +
    #geom_chicklet(radius = grid::unit(2, 'mm'), fill = darkslateblue) +
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(round(ast, 1),""), 
                   y = ast + max_value / 25 ), size = 4, fill = crimson) +
    geom_text(aes(label = paste0(g,"\n games")), y = max_value / 6, color = "white", size = 3) +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, color = "white", size = 4) +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("Average Assists: ", player_name),
         x = "season",
         y = "") +
    nr_theme
}



#block chart
block_chart <-  function(player_name) {
  df <- today_with_history %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist() %>% as.numeric() %>% sort()
  max_value = max(df$blk)
  df %>% ggplot(aes(x = season_team, y = blk)) +
    geom_col(fill = darkslateblue) +
    #geom_chicklet(radius = grid::unit(2, 'mm'), fill = darkslateblue) +
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(round(blk, 1),""), 
                   y = blk + max_value / 25 ), size = 4, fill = crimson) +
    geom_text(aes(label = paste0(g,"\n games")), y = max_value / 6, color = "white", size = 3) +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 4, color = "white") +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("Average Blocks: ", player_name),
         x = "season",
         y = "") +
    nr_theme
}



#steal chart
steal_chart <-  function(player_name) {
  df <- today_with_history %>% filter(player == player_name & tm != "TOT")
  season_labs = df$season_team %>% map(function(x) str_split(x, " ")[[1]][1]) %>% unlist() %>% as.numeric() %>% sort()
  max_value = max(df$stl)
  df %>% ggplot(aes(x = season_team, y = stl)) +
    geom_col(fill = darkslateblue) +
    #geom_chicklet(radius = grid::unit(2, 'mm'), fill = darkslateblue) +
    scale_x_discrete(labels = season_labs) +
    geom_label(aes(label = paste0(round(stl, 1),""), 
                   y = stl + max_value / 25 ), size = 4, fill = crimson) +
    geom_text(aes(label = paste0(g,"\n games")), y = max_value / 6, color = "white", size = 3) +
    geom_label(aes(label = tm, fill = tm), y = max_value / 15, size = 4, color = "white") +
    scale_fill_manual(values = team_colors) +
    labs(title = paste0("Average Steals: ", player_name),
         x = "season",
         y = "") +
    nr_theme
}


#team comparison chart for pts
team_compare_player_pts <- function(player_name, team, team_fullname) {
  df <- players_2021a %>% filter(tm == team ) %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite))
  max_value <- max(df$pts, na.rm =  TRUE)
  df %>% ggplot(aes(x = reorder(player, pts), y = pts, fill = hilite)) +
    #geom_chicklet(radius = grid::unit(2, 'mm'), fill = darkslateblue) +
    geom_col(width = .7) +
    geom_text(aes(label = pts), y = max_value / 25, color = "white") +
    scale_fill_manual(values = c(darkslateblue, crimson)) +
    coord_flip() +
    labs(title = player_name,
         subtitle = paste("Average points compared to", team_fullname),
         x = "",
         y = "") +
    nr_theme 
}


#league comparison chart for pts
league_compare_player_pts <- function(player_name) {
  players_2021 %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, pts), y = pts, fill = hilite)) +
    geom_col(aes(width = if_else(hilite == 1, 2, 0.3))) +
    scale_fill_manual(values = c(darkslateblue, crimson)) +
    scale_x_discrete(labels = "") +
    labs(title = player_name,
         subtitle = "Average points compared to all NBA",
         x = "",
         y = "") +
    nr_theme + theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
}


#team comparison chart for fg%
team_compare_player_fg <- function(player_name, team, team_fullname) {
  df <- players_2021a %>%
    filter(tm == team, fgpercent < 1 ) %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) 
  max_value <- max(df$fgpercent, na.rm =  TRUE)
  df %>% ggplot(aes(x = reorder(player, fgpercent), y = fgpercent, fill = hilite)) +
    geom_col(width = .7) +
    geom_text(aes(label = paste0(fgpercent * 100, "%")), y = max_value / 25, color = "white") +
    scale_fill_manual(values = c(darkslateblue, crimson)) +
    scale_y_continuous(labels = percent) +
    coord_flip() +
    labs(title = player_name,
         subtitle = paste0("Field goal percentage compared to ", team_fullname),
         x = "",
         y = "") +
    nr_theme 
}



#leage comparison chart for fg%
league_compare_player_fg <- function(player_name) {
  players_2021 %>% filter(fgpercent < 1) %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, fgpercent), y = fgpercent, fill = hilite)) +
    geom_col(aes(width = if_else(hilite == 1, 2, 0.3))) +
    scale_fill_manual(values = c(darkslateblue, crimson)) +
    scale_x_discrete(labels = "") +
    scale_y_continuous(labels = percent) +
    labs(title =  player_name,
         subtitle = "Field goal percentage compared to all NBA",
         x = "",
         y = "") +
    nr_theme + theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
}


#team comparison chart for 3p%
team_compare_player_3p <- function(player_name, team, team_fullname) {
  df <- players_2021a %>%
    filter(tm == team & x3ppercent < 1) %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) 
  max_value = max(df$x3ppercent, na.rm = TRUE)
  df %>% ggplot(aes(x = reorder(player, x3ppercent), y = x3ppercent, fill = hilite)) +
    geom_col(width = .7) +
    geom_text(aes(label = paste0(x3ppercent * 100, "%")), y = max_value / 25, color = "white") +
    scale_fill_manual(values = c(darkslateblue, crimson)) +
    scale_y_continuous(labels = percent) +
    coord_flip() +
    labs(title = player_name,
         subtitle = paste0("3 point percentage compared to ", team_fullname),
         x = "",
         y = "") +
    nr_theme 
}



#leage comparison chart for 3p%
league_compare_player_3p <- function(player_name) {
  players_2021 %>% filter(x3ppercent < 1 & x3ppercent > 0) %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, x3ppercent), y = x3ppercent, fill = hilite)) +
    geom_col(aes(width = if_else(hilite == 1, 2, 0.3))) +
    scale_fill_manual(values = c(darkslateblue, crimson)) +
    scale_x_discrete(labels = "") +
    scale_y_continuous(labels = percent) +
    labs(title = player_name,
         subtitle = "3 point percentage compared to all NBA",
         x = "",
         y = "") +
    nr_theme + theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
}



#team comparison for rebounds
team_compare_player_rebounds <- function(player_name, team, team_fullname) {
  df <- players_2021a %>% 
    filter(tm == team ) %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) 
  max_value <- max(df$trb, na.rm = TRUE)
  df %>% ggplot(aes(x = reorder(player, trb), y = trb, fill = hilite)) +
    geom_col(width = .7) +
    geom_text(aes(label = trb), y = max_value / 25, color = "white") +
    scale_fill_manual(values = c(darkslateblue, crimson)) +
    coord_flip() +
    labs(title = player_name,
         subtitle = paste0("Average rebounds compared to ", team_fullname),
         x = "",
         y = "") +
    nr_theme 
}



#league comparison chart for rebounds
league_compare_player_rebounds <- function(player_name) {
  players_2021 %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, trb), y = trb, fill = hilite)) +
    geom_col(aes(width = if_else(hilite == 1, 2, 0.3))) +
    scale_fill_manual(values = c(darkslateblue, crimson)) +
    scale_x_discrete(labels = "") +
    labs(title = player_name,
         subtitle = "Average rebounds compared to all NBA",
         x = "",
         y = "") +
    nr_theme + theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
}



#team comparison chart for assists
team_compare_player_assists <- function(player_name, team, team_fullname) {
  df <- players_2021a %>%
    filter(tm == team ) %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) 
  max_value <- max(df$ast, na.rm = TRUE)
  df %>% ggplot(aes(x = reorder(player, ast), y = ast, fill = hilite)) +
    geom_col(width = .7) +
    geom_text(aes(label = trb), y = max_value / 25, color = "white") +
    scale_fill_manual(values = c(darkslateblue, crimson)) +
    coord_flip() +
    labs(title = player_name,
         subtitle = paste0("Average assists compared to ", team_fullname),
         x = "",
         y = "") +
    nr_theme 
}
#league comparison chart for assists
league_compare_player_assists <- function(player_name) {
  players_2021 %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, ast), y = ast, fill = hilite)) +
    geom_col(aes(width = if_else(hilite == 1, 2, 0.3))) +
    scale_fill_manual(values = c(darkslateblue, crimson)) +
    scale_x_discrete(labels = "") +
    labs(title = player_name,
         subtitle = "Average assists compared to all NBA",
         x = "",
         y = "") +
    nr_theme + theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
}


#team comparison chart for blocks
team_compare_player_blocks <- function(player_name, team, team_fullname) {
  df <- players_2021a %>%
    filter(tm == team ) %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) 
  max_value <- max(df$blk)
  df %>% ggplot(aes(x = reorder(player, blk), y = blk, fill = hilite)) +
    geom_col(width = .7) +
    geom_text(aes(label = blk), y = max_value / 25, color = "white") +
    scale_fill_manual(values = c(darkslateblue, crimson)) +
    coord_flip() +
    labs(title = player_name,
         subtitle = paste0("Average blocks compared to ", team_fullname),
         x = "",
         y = "") +
    nr_theme 
}
#league comparison chart for blocks
league_compare_player_blocks <- function(player_name) {
  players_2021 %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, blk), y = blk, fill = hilite)) +
    geom_col(width = .7) +
    scale_fill_manual(values = c(darkslateblue, crimson)) +
    scale_x_discrete(labels = "") +
    labs(title = player_name,
         subtitle = "Average blocks compared to all NBA",
         x = "",
         y = "") +
    nr_theme + theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
}


#team comparison chart for steals
team_compare_player_steals <- function(player_name, team, team_fullname) {
  df <- players_2021a %>%
    filter(tm == team ) %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) 
  max_value <- max(df$stl)
  df %>% ggplot(aes(x = reorder(player, stl), y = stl, fill = hilite)) +
    geom_col(width = .7) +
    scale_fill_manual(values = c(darkslateblue, crimson)) +
    coord_flip() +
    labs(title = player_name,
         subtitle = paste0("Average steals compared to ", team_fullname),
         x = "",
         y = "") +
    nr_theme 
}
#league comparison chart for steals
league_compare_player_steals <- function(player_name) {
  players_2021 %>%
    mutate(hilite = if_else(player == player_name, 1, 0)) %>%
    mutate(hilite = as.factor(hilite)) %>%
    ggplot(aes(x = reorder(player, stl), y = stl, fill = hilite)) +
    geom_col(size = 10) +
    scale_fill_manual(values = c(darkslateblue, crimson)) +
    scale_x_discrete(labels = "") +
    labs(title = player_name,
         subtitle = "Average steals compared to all NBA",
         x = "",
         y = "") +
    nr_theme + theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
}

#make it a function that takes player name
# shooting_chart <- function(player_name) {
#   df <- wnba_shooting %>%
#     filter(player == toupper(player_name) ) %>%
#     mutate(shots = ordered(shots, levels = c("missed", "made"))) 
#   max_fga <- max(df$FGA, na.rm = TRUE)
#   df %>%
#     ggplot(aes(x = reorder(zone, FGA), y = value, fill = shots)) +
#     geom_col(width = .7) +
#     geom_text(aes(label = ifelse(value > 1, value, ""), y = ifelse(shots == "made", value / 2, (FGA - value / 2)))) +
#     geom_label(aes(label = ifelse(FGA > 0, paste0(percent, "%"), "-"), 
#                    y = ifelse(FGA > 0, FGA + max_fga * 0.05, 1)), 
#                fill = "#385097", color = "white") +
#     scale_x_discrete(labels = c("right" = "right corner 3", "restricted" = "restricted area",
#                                 "paint" = "in the paint", "mid" = "mid range", "left" = "left corner 3",
#                                 "break" = "above the break 3")) +
#     scale_fill_manual(values = c("grey", "#dd1f22")) +
#     coord_flip() +
#     nyl_theme + theme(
#       legend.position = "top"
#     ) +
#     labs(
#       title = paste0("Shooting by Zone - ", player_name),
#       subtitle = "Field Goal Attempts - 2020 Season",
#       x = "",
#       y = ""
#     )
# }



