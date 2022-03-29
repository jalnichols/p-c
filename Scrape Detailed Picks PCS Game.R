

library(tidyverse)
library(lubridate)
library(rvest)
library(RMySQL)

dbDisconnect(con)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#
#
#

game_race_list <- vector("list", 7)

pull_in_game <- seq(2022,2022,1)

for(g in 1:length(pull_in_game)) {
  
  page <- paste0('https://www.procyclingstats.com/game.php?s=activated-games&season=',
                 pull_in_game[[g]],
                 '&filter=Filter') %>%
    read_html()
  
  res <- cbind(
    
    page %>%
      html_nodes('table') %>%
      html_table() %>%
      .[[1]] %>%
      as_tibble(),
    
    page %>%
      html_nodes('table') %>%
      html_nodes('a') %>%
      html_attr(name = "href") %>%
      enframe(name = NULL) %>%
      rename(url = value))
  
  game_race_list[[g]] <- res
  
}

#

all_games <- bind_rows(game_race_list) %>%
  
  inner_join(
    
    dbReadTable(con, "pcs_all_stages") %>%
      select(url, value, year, s) %>%
      mutate(match_url = paste0(url, "/game")), by = c("url" = "match_url")
    
  ) %>%
  select(Date, Race, Url = value, year, Stage = s) %>%

  inner_join(dbGetQuery(con, "SELECT stage, race, year FROM pcs_game_picks GROUP BY stage, race, year"), by = c("Stage" = "stage", "Race" = "race", "year"))

#
#
# SCRAPE GAME PREDICTED RIDERS
#
#

each_race <- all_games %>%
  
  anti_join(dbGetQuery(con, "SELECT DISTINCT Race, Year as year, stage as Stage FROM pcs_game_team_picks")) %>%
  
  group_by(Race, year) %>%
  filter(rank(Url, ties.method = "first") == 1) %>%
  ungroup() %>%
  select(Race, year, Url) %>%
  unique()

#
#
#

list_of_game_picks <- vector("list", length(each_race$Url))

for(g in 1:length(list_of_game_picks)) {
  
  page <- paste0(each_race$Url[[g]], '/game/stats-per-stage') %>%
    read_html()
  
  res <- cbind(
    
    page %>%
      html_nodes('table') %>%
      html_table() %>%
      .[[1]] %>%
      as_tibble(),
    
    page %>%
      html_nodes('table') %>%
      html_nodes('a') %>%
      html_attr(name = "href") %>%
      enframe(name = NULL) %>%
      rename(url = value)) %>%
    
    janitor::clean_names() %>%
    
    select(url, number_players) %>%
    mutate(number_players = as.character(number_players))
  
  list_of_game_picks[[g]] <- res %>%
    
    mutate(Race = each_race$Race[[g]],
           year = each_race$year[[g]])
  
  print(g)
  
  Sys.sleep(runif(1,0.5,2.5))
  
}

#

players_in_game <- bind_rows(list_of_game_picks) %>%
  
  mutate(url = paste0("https://www.procyclingstats.com/", url)) %>%
  mutate(url = str_replace(url, "/result", "")) %>%
  
  left_join(
    all_games %>%
      rename(url = Url), by = c("url", "Race", "year")
  ) %>%
  
  mutate(number_players = ifelse(number_players == "-", 1001, as.numeric(number_players))) %>%
  
  mutate(more_1000 = ifelse(number_players > 1000, 1, 0)) %>%
  
  anti_join(dbGetQuery(con, "SELECT DISTINCT Race, Year as year, stage as Stage FROM pcs_game_team_picks"))

#

scrape_1000 <- players_in_game %>%
  filter(more_1000 == 0) %>%
  mutate(url = str_sub(url, 38, nchar(url))) %>%
  separate(url, c("R", "Y", "S"), sep = "/", remove = FALSE) %>%
  
  mutate(S = ifelse(is.na(S), "result", S)) %>%
  filter(S != "gc")

#

scrape_ab_1000 <- players_in_game %>%
  filter(more_1000 != 0) %>%
  mutate(url = str_sub(url, 38, nchar(url))) %>%
  separate(url, c("R", "Y", "S"), sep = "/", remove = FALSE) %>%
  
  mutate(S = ifelse(is.na(S), "result", S)) %>%
  filter(S != "gc") %>%
  
  arrange(desc(R == "tour-de-france")) %>%
  filter(!is.na(Date))
  
#
#
# Initial writing of new tables
#
#

for(g in 1:nrow(scrape_1000)) {
  
  URL <- paste0("https://www.procyclingstats.com/race.php?limit=1000&xnation=&my_league=&league=&filter=Filter&id1=",
                scrape_1000$R[[g]], 
                "&id2=", 
                scrape_1000$Y[[g]],  
                "&id3=", 
                scrape_1000$S[[g]], 
                "&id4=game&id5=dayranking")
  
  #
  
  page <- URL %>%
    read_html()
  
  #
  
  res <- cbind(
    
    page %>%
      html_nodes('table') %>%
      html_table() %>%
      .[[1]] %>%
      as_tibble() %>%
      janitor::clean_names() %>%
      select(player, view_team, points),
    scrape_1000[g,]) %>%
    select(team = player, view_team, points,
           url, Race, Year = year, Date, number_players,
           Stage)
  
  #
  
  dbWriteTable(con, "pcs_game_team_picks", res, append = TRUE, row.names = FALSE)
  
  print(g)
  
  Sys.sleep(runif(1,0.5,2.5))
  
}

#
#
#

ctry <- c("be", "fr", "it", "gb", "us", "pt", "nl", "pl", "es",
          "cz", "de", "jp", "si", "ch", "dk", "ie", "sk", "tr",
          "no", "ro", "ca", "hu", "hr", "za", "se", "lu", "co",
          "ar", "au", "bg", "lv", "rs", "ad")

for(g in 1:nrow(scrape_ab_1000)) {
  
  for(c in ctry) {
    
    URL <- paste0("https://www.procyclingstats.com/race.php?limit=1000&xnation=",
                  c,
                  "&my_league=&league=&filter=Filter&id1=",
                  scrape_ab_1000$R[[g]], 
                  "&id2=", 
                  scrape_ab_1000$Y[[g]],  
                  "&id3=", 
                  scrape_ab_1000$S[[g]], 
                  "&id4=game&id5=dayranking")
    
    #
    
    page <- URL %>%
      read_html()
    
    #
    
    if(nrow(page %>%
            html_nodes('table') %>%
            html_table() %>%
            .[[1]] %>%
            as_tibble()) > 0) {
      
      res <- cbind(
        
        page %>%
          html_nodes('table') %>%
          html_table() %>%
          .[[1]] %>%
          as_tibble() %>%
          janitor::clean_names() %>%
          select(player, view_team, points),
        scrape_ab_1000[g,]) %>%
        select(team = player, view_team, points,
               url, Race, Year = year, Date, number_players,
               Stage)
      
      #
      
      dbWriteTable(con, "pcs_game_team_picks", res, append = TRUE, row.names = FALSE)
      
      print(g)
      
      Sys.sleep(runif(1,0.5,2.5))
      
    }
    
  }
  
}

#
#
#

dbGetQuery(con, "SELECT * FROM pcs_game_team_picks") %>% 
  
  separate(view_team, c("r1", "r2", "r3", "r4", "r5"), sep = "\\)") %>%
  gather(stat, value, r1:r5) %>%
  filter(!is.na(value)) %>%
  separate(value, c("rider", "select_points"), sep = "\\(") %>%
  filter(str_trim(rider) != "") %>%
  
  group_by(url, Race, Year, Stage) %>%
  mutate(pickers = n_distinct(team)) %>%
  ungroup() %>%
  
  group_by(url, Race, Year, Stage, rider = str_trim(rider), pickers) %>%
  summarize(picks = n(), points = sum(as.numeric(select_points), na.rm = T)) %>%
  ungroup() %>%
  
  mutate(pts_of_5 = points / (pickers*5)) %>%
  
  filter(!is.na(Stage)) %>%
  
  mutate(race = iconv(Race, from="UTF-8", to = "ASCII//TRANSLIT"),
         rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT")) -> rider_point_percentages_df

#
#
#

picks_aggr <- dbGetQuery(con, "SELECT * FROM pcs_game_picks") %>%
  mutate(url = str_replace(url, "https://www.procyclingstats.com/race/", ""),
         url = str_replace(url, "/game/results-vs-picks", ""))

#
#
#

combined_picks <- picks_aggr %>%
  
  select(-race, -year, -stage, -result) %>%
  
  filter(number_picks > 0) %>%
  
  left_join(rider_point_percentages_df, by = c("url")) %>%
  filter(str_detect(picked_rider, str_to_title(rider))) %>%
  filter(str_sub(picked_rider,1,3) == str_sub(str_to_title(rider), 1, 3)) %>%
  filter((((number_picks / picks) > 0.90 & (number_picks / picks) < 1.1)) |
           (abs(number_picks - picks) <= 5)) %>%
  
  select(-rider, -picks, -points, -race) %>%
  
  rbind(
    
    picks_aggr %>%
      
      select(-race, -year, -stage, -result) %>%
      
      filter(number_picks == 0) %>%
      
      left_join(rider_point_percentages_df %>%
                  select(-rider, -picks, -points, -race) %>%
                  mutate(pts_of_5 = 0) %>%
                  unique(), by = c("url"))
    
  ) %>%

  unique() %>%
  
  filter(Year >= 2016) %>%
  
  mutate(Race = str_to_lower(Race),
         Stage = as.character(Stage)) %>%
  
  inner_join(
    dbGetQuery(con, "SELECT DISTINCT stage, race, year, date, pred_climb_difficulty, sof, bunch_sprint, one_day_race
               FROM stage_data_perf
               WHERE year >= 2016 AND time_trial = 0"), by = c("Stage" = "stage", "Race" = "race", "Year" = "year")
  ) %>%
  mutate(date = as.Date(date))

#
#
#

DATES <- dbGetQuery(con, "SELECT DISTINCT date FROM lme4_rider_logranks
                    WHERE date > '2016-12-31'") %>%
  arrange(desc(date)) %>%
  
  anti_join(dbGetQuery(con, "SELECT DISTINCT date FROM lme4_rider_pcsgamepicks"))

#
#
#

for(p in 1:nrow(DATES)) {
  
  ed <- as.Date(DATES$date[[p]])
  sd <- ed - 365 
  
  picks_model <- lme4::lmer(pts_of_5 ~ (1 + pred_climb_difficulty | picked_rider) + 
                              (0 + bunch_sprint | picked_rider) + 
                              #(0 + one_day_race | picked_rider) +
                              sof,
                            
                            data = combined_picks %>% 
                              
                              filter(between(date, sd, ed)) %>%
                              
                              group_by(picked_rider) %>%
                              filter(n() >= 15) %>% 
                              ungroup())
  
  #
  
  # picks_model shows impact of SOF as -0.044 and intercept at 0.035
  # so average rider in 0.50 race would be 0.012 with 75% of riders with negative intercepts
  
  #
  
  random_effects <- lme4::ranef(picks_model)[[1]] %>% rownames_to_column() %>% mutate(date = ed-1)

  dbWriteTable(con, "lme4_rider_pcsgamepicks", random_effects, append = TRUE, row.names = FALSE)
  
  print(DATES$date[[p]])
    
}
  