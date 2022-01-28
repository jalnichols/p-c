
library(tidyverse)
library(rvest)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

YEARS <- seq(2022,2022,1)

for(y in YEARS) {
  
  teams_for_year <- paste0('https://www.procyclingstats.com/teams.php?year=', y, '&filter=Filter&s=worldtour') %>% 
    read_html() %>% 
    html_nodes('div.mt20') %>% 
    html_nodes('a') %>% 
    html_attr(name = "href") %>%
    enframe(name = NULL) %>% 
    rename(team_url = value)
  
  tm_list <- vector("list", length(teams_for_year$team_url))
  
  for(t in 1:length(teams_for_year$team_url)) {
    
    page <- paste0("https://www.procyclingstats.com/", teams_for_year$team_url[[t]]) %>%
      read_html()
    
    tm <- page %>%
      html_nodes('h1') %>%
      html_text() %>% 
      str_replace(paste0('»',y), '')
    
    df <- page %>%
      html_nodes('ul.list') %>%
      html_nodes('li')
    
    r1 <- df %>% html_nodes('a') %>% html_attr(name = "href")
    r2 <- df %>% html_nodes('a') %>% html_text()
    
    riders_team <- cbind(r1,r2) %>%
      as_tibble() %>%
      mutate(team_url = teams_for_year$team_url[[t]],
             team = tm) %>%
      filter(str_detect(r1, "rider/")) %>%
      unique()
    
    tm_list[[t]] <- riders_team
    
  }
  
  #
  
  riders_for_year <- bind_rows(tm_list) %>%
    
    mutate(rider = iconv(r2, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
    separate(team, c("team", "level"), sep = '\\(') %>%
    mutate(level = str_replace(level, "\\)", "")) %>%
    mutate(team = stringi::stri_trans_general(str = team, id = "Latin-ASCII")) %>%
    
    select(team, level, rider, rider_url = r1) %>%
    
    mutate(rider = str_to_title(tolower(rider)),
           Season = y)
  
  dbWriteTable(con, "team_rosters_season", riders_for_year, append = TRUE, row.names = FALSE)
  
}
