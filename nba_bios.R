library(httr)
library(tidyverse)
library(xml2)
library(rvest)
library(stringr)
library(rebus)
library(lubridate)


nba_links <- current_season$link


p1 <- GET("https://www.basketball-reference.com/players/h/hardeja01.html")
p1_details <- content(p1, "parsed")

ok <- read_html("https://www.basketball-reference.com/players/h/hardeja01.html")
ok1 <- ok %>% html_node('#meta')

# get player id from their player pages
player_ids <- c()
for (k in 1:length(players)) {
  url <- paste0("https://www.wnba.com/player/", players[k], "/#/stats")
  player_id <- read_html(url) %>%
    html_nodes('.player-profile') %>%
    html_attr("data-playerid")
  player_ids <- c(player_ids, player_id)
}

bio_info <- data.frame(player = wnba2020_names$player, 
                       snake_name = players, 
                       player_id = player_ids,
                       dob = 0,
                       height = 0,
                       weight = 0,
                       college = 0)


# loop throught all players to get their details
for(j in 1:nrow(bio_info)) {
  p_url <- paste0("https://a.data.nba.com/wnba/player/", bio_info$player_id[j])
  p <- GET(p_url)
  details <- content(p, "parsed")
  bio_info$dob[j] <- ifelse(length(details$data$info$dob)>0, details$data$info$dob, NA)
  bio_info$height[j] <- ifelse(length(details$data$info$ht)>0, details$data$info$ht, NA)
  bio_info$weight[j] <- ifelse(length(details$data$info$wt)>0, details$data$info$wt, NA)
  bio_info$college[j] <- ifelse(length(details$data$info$hcc)>0, details$data$info$hcc, NA)
}



players_2021a %>% 
  filter(tm == "BRK") %>%
  select(player, pos)


player_teams4 <- players_2021$player %>% map(function(x) get_teams(x)) %>% bind_rows() %>% 
  left_join(nba_player_teams, by = "player") %>%
  mutate(team = if_else(!is.na(current_team), current_team, team)) %>% select(player, team)


player_teams3 <- player_teams1 %>% left_join(nba_player_teams, by = "player") %>%
  mutate(team = if_else(!is.na(current_team), current_team, team)) %>% select(player, team)


#save player teams 3
write_csv(player_teams3, "NBA_player_teams_0302.csv")










nba_player_teams <- read_csv("nba_bios_0302.csv") %>%
  filter(!is.na(COUNTRY) & PLAYER != "PLAYER") %>%
  mutate(PLAYER = str_remove(PLAYER, " Headshot")) %>%
  rename(player = PLAYER,
         current_team = TEAM) %>%
  select(player, current_team)

names(nba_bios_0302) <- tolower(names(nba_bios_0302))


sum(nba_bios_0302$player %in% nba_bio_2021$player)

str(nba_bio_2021)
str(nba_bios_0302)

test_bios <- nba_bios_0302 %>% anti_join(nba_bio_2021, by = "player")

bios <- bios %>%
  mutate(ht = str_replace(ht, "/", "-"),
         wt = str_remove(wt, "???lbs"),
         player = paste(first_name, last_name)
         )

#save the clean version
write_csv(bios, "nba_bios.csv")


players_bio <- players_2021 %>% left_join(bios, by = "player") %>%
  select(player, age, tm.x, number, position, ht, wt, college, country)





# bio and history needs to go on github
write_csv(players_bio, "NBA_bios.csv")



### 3/8


