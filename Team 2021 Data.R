
library(tidyverse)
library(rvest)
library(DBI)

dbDisconnect(con)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      port = 5432,
                      host = 'localhost',
                      dbname = "cycling",
                      user = "postgres",
                      password = "braves")

#


years = seq(2011,2011,1)

for(y in 1:length(years)) {
  
  teams_2021 <- paste0('https://www.procyclingstats.com/teams.php?year=', years[[y]], '&filter=Filter&s=worldtour') %>%
    read_html() %>%
    html_nodes('div.mt20') %>%
    html_nodes('a') %>%
    html_attr(name = "href") %>%
    enframe(name = NULL) %>%
    rename(team_url = value) %>%
    unique()
  
  teams_2021 <- paste0('https://www.procyclingstats.com/teams.php?year=', years[[y]], '&name=&pname=contains&level=&filter=Filter&s=alphabetical') %>% 
    read_html()
  
  table <- teams_2021 %>%
    html_nodes('table') %>%
    .[[1]] %>%
    html_table()
  
  links <- teams_2021 %>%
    html_nodes('table') %>%
    .[[1]] %>%
    html_nodes('a') %>%
    html_attr(name = "href")  %>%
    enframe(name = NULL) %>% 
    rename(team_url = value) %>%
    filter(str_detect(team_url, "team/"))
  
  names <- teams_2021 %>%
    html_nodes('table') %>%
    .[[1]] %>%
    html_nodes('a') %>%
    html_text()  %>%
    enframe(name = NULL) %>%
    rename(team_name = value)
  
  together <- cbind(links, names, table) %>%
    janitor::clean_names() %>%
    filter(number_riders > 1) %>%
    select(team_name, team_url, class) %>%
    filter(!class %in% c("WTW", "UCI", "P-CRO")) %>%
    mutate(team_name = stringi::stri_trans_general(str = team_name, id = "Latin-ASCII"),
           year = years[[y]]) %>%
    anti_join(dbGetQuery(con, "SELECT DISTINCT team as team_name, season as year FROM pcs_linked_teams") %>%
                mutate(team_name = str_trim(team_name)), by = c("team_name", "year"))
  
  teams_2021 <- together
  
  tm_list <- vector("list", length(teams_2021$team_url))
  
  for(t in 1:length(teams_2021$team_url)) {
    
    page <- paste0("https://www.procyclingstats.com/", teams_2021$team_url[[t]]) %>%
      read_html()
    
    tm <- page %>%
      html_nodes('h1') %>%
      html_text() %>% 
      str_replace('»', '') %>%
      str_replace(paste0(years[[y]]), "")
    
    linked_teams <- page %>%
      html_nodes("form") %>%
      html_nodes("option") %>%
      html_attr(name = "value")
    
    linked_teams_names <- page %>%
      html_nodes("form") %>%
      html_nodes("option") %>%
      html_text()
    
    team <- linked_teams %>%
      enframe(name = NULL) %>%
      cbind(linked_team_name = linked_teams_names) %>%
      rename(linked_team_url = value) %>%
      mutate(team_url = teams_2021$team_url[[t]],
             team = tm)
    
    tm_list[[t]] <- team
    
    Sys.sleep(4)
    
  }
  
  #
  
  all_links <- bind_rows(tm_list) %>%
    separate(linked_team_name, c("linked_season", "linked_team_name"), sep = "\\|") %>%
    separate(team, c("team", "level"), sep = '\\(') %>%
    mutate(level = str_replace(level, "\\)", ""),
           linked_team_name = str_trim(linked_team_name)) %>%
    mutate(team = stringi::stri_trans_general(str = team, id = "Latin-ASCII"),
           linked_team_name = stringi::stri_trans_general(str = linked_team_name, id = "Latin-ASCII")) %>%
    
    select(team, level, team_url, linked_team_url, linked_team_name, linked_season) %>%
    
    mutate(season = years[[y]])
  
  dbWriteTable(con, "pcs_linked_teams", all_links, append = TRUE, row.names = FALSE)
  
  print(years[[y]])
  
}


#
#
#
#
#

years = seq(2023,2011,-1)

for(y in 3:length(years)) {
  
  teams_2021 <- paste0('https://www.procyclingstats.com/teams.php?year=', years[[y]], '&filter=Filter&s=worldtour') %>% 
    read_html() %>% 
    html_nodes('div.mt20') %>% 
    html_nodes('a') %>% 
    html_attr(name = "href") %>%
    enframe(name = NULL) %>% 
    rename(team_url = value) %>%
    unique()
  
  tm_list <- vector("list", length(teams_2021$team_url))
  
  for(t in 1:length(teams_2021$team_url)) {
    
    page <- paste0("https://www.procyclingstats.com/", teams_2021$team_url[[t]]) %>%
      read_html()
    
    tm <- page %>%
      html_nodes('h1') %>%
      html_text() %>% 
      str_replace('»', '') %>%
      str_replace(paste0(years[[y]]), "")
    
    df <- page %>%
      html_nodes('ul.list') %>%
      html_nodes('li')
    
    r1 <- df %>% html_nodes('a') %>% html_attr(name = "href")
    r2 <- df %>% html_nodes('a') %>% html_text()
    r3 <- df %>% html_nodes('div.fs11') %>% html_text() %>% enframe(name = NULL)
    r4 <- r3[1:(nrow(r3)/2),]
    r5 <- r3[(nrow(r3)/2+1):nrow(r3),]
    
    riders_team <- cbind(r1,r2) %>%
      as_tibble() %>%
      mutate(team_url = teams_2021$team_url[[t]],
             team = tm) %>%
      filter(str_detect(r1, "rider/")) %>%
      unique() %>%
      arrange(r2) %>%
      cbind(r5) %>%
      rename(notes = value)
    
    tm_list[[t]] <- riders_team
    
    Sys.sleep(4)
    
  }
  
  #
  
  riders_2021 <- bind_rows(tm_list) %>%
    
    mutate(rider = iconv(r2, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
    separate(team, c("team", "level"), sep = '\\(') %>%
    mutate(level = str_replace(level, "\\)", "")) %>%
    mutate(team = stringi::stri_trans_general(str = team, id = "Latin-ASCII")) %>%
    
    select(team, level, rider, rider_url = r1, notes, team_url) %>%
    
    mutate(rider = str_to_title(tolower(rider)))
  
  riders_season <- riders_2021 %>% mutate(season = years[[y]])
  
  dbWriteTable(con, "pcs_season_team_riders", riders_season, append = TRUE, row.names = FALSE)
  
  print(years[[y]])
  
}

#

years = seq(2021,2016,-1)

for(y in 1:length(years)) {
  
  teams_2021 <- paste0('https://www.procyclingstats.com/teams.php?season=', years[[y]], '&filter=Filter&s=continental') %>% 
    read_html() %>%
    html_nodes('div.content') %>%
    html_nodes('a.black')
  
  links <- teams_2021 %>%
    html_attr(name = "href")  %>%
    enframe(name = NULL) %>% 
    rename(team_url = value) %>%
    filter(str_detect(team_url, "team/"))
  
  names <- teams_2021 %>%
    html_text()  %>%
    enframe(name = NULL) %>%
    rename(team_name = value)
  
  together <- cbind(links, names)
  
  with_jerseys <- paste0('https://www.procyclingstats.com/teams.php?season=', years[[y]], '&filter=Filter&s=continental') %>% 
    read_html() %>%
    html_nodes('div.content') %>%
    html_nodes('a') %>%
    html_nodes('img') %>%
    html_attr(name = "title")
  
  teams_2021 <- together %>%
    filter(team_name %in% with_jerseys)
  
  tm_list <- vector("list", length(teams_2021$team_url))
  
  # for(t in 1:length(teams_2021$team_url)) {
  #   
  #   page <- paste0("https://www.procyclingstats.com/", teams_2021$team_url[[t]], "/season/points-per-rider") %>%
  #     read_html()
  #   
  #   tm <- page %>%
  #     html_nodes('h1') %>%
  #     html_text() %>% 
  #     str_replace('»', '') %>%
  #     str_replace(paste0(years[[y]]), "")
  #   
  #   r1 <- page %>%
  #     html_nodes('table') %>%
  #     html_table() %>%
  #     .[[1]] %>%
  #     janitor::clean_names() %>%
  #     filter(rider != "")
  #   
  #   r2 <- page %>%
  #     html_nodes('table') %>%
  #     html_nodes('a') %>% 
  #     html_attr(name = "href")
  #   
  #   riders_team <- cbind(r1, rider_url = r2) %>%
  #     as_tibble() %>%
  #     mutate(team_url = teams_2021$team_url[[t]],
  #            team = tm) %>%
  #     unique() %>%
  #     janitor::clean_names()
  #   
  #   if(nrow(riders_team) > 0) {
  #     tm_list[[t]] <- riders_team
  #   }
  # 
  #   Sys.sleep(4)
  #   
  # }
  # 
  # #
  # 
  # riders_2021 <- bind_rows(tm_list) %>%
  #   
  #   mutate(rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
  #   separate(team, c("team", "level"), sep = '\\(') %>%
  #   mutate(level = str_replace(level, "\\)", "")) %>%
  #   mutate(team = stringi::stri_trans_general(str = team, id = "Latin-ASCII")) %>%
  #   
  #   select(team, level, rider, rider_url, pcs_points = points) %>%
  #   
  #   mutate(rider = str_to_title(tolower(rider)))
  # 
  # riders_season <- riders_2021 %>% mutate(season = years[[y]])
  # 
  # dbWriteTable(con, "pcs_points_season_rider", riders_season, append = TRUE, row.names = FALSE)
  
  for(t in 1:length(teams_2021$team_url)) {

    page <- paste0("https://www.procyclingstats.com/", teams_2021$team_url[[t]]) %>%
      read_html()

    tm <- page %>%
      html_nodes('h1') %>%
      html_text() %>%
      str_replace('»', '') %>%
      str_replace(paste0(years[[y]]), "")

    df <- page %>%
      html_nodes('ul.list') %>%
      html_nodes('li')

    r1 <- df %>% html_nodes('a') %>% html_attr(name = "href")
    r2 <- df %>% html_nodes('a') %>% html_text()

    riders_team <- cbind(r1,r2) %>%
      as_tibble() %>%
      mutate(team_url = teams_2021$team_url[[t]],
             team = tm) %>%
      filter(str_detect(r1, "rider/")) %>%
      unique()

    tm_list[[t]] <- riders_team

    Sys.sleep(4)

  }

  #

  riders_2021 <- bind_rows(tm_list) %>%

    mutate(rider = iconv(r2, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
    separate(team, c("team", "level"), sep = '\\(') %>%
    mutate(level = str_replace(level, "\\)", "")) %>%
    mutate(team = stringi::stri_trans_general(str = team, id = "Latin-ASCII")) %>%

    select(team, level, rider, rider_url = r1) %>%

    mutate(rider = str_to_title(tolower(rider)))

  riders_season <- riders_2021 %>% mutate(season = years[[y]])

  dbWriteTable(con, "pcs_season_team", riders_season, append = TRUE, row.names = FALSE)
  
  print(years[[y]])
  
}

#

years = seq(2024,2024,1)

for(y in 1:length(years)) {
  
  teams_2021 <- paste0('https://www.procyclingstats.com/teams.php?year=', years[[y]], '&name=&pname=contains&level=&filter=Filter&s=alphabetical') %>% 
    read_html()
  
  table <- teams_2021 %>%
    html_nodes('table') %>%
    .[[1]] %>%
    html_table()
  
  links <- teams_2021 %>%
    html_nodes('table') %>%
    .[[1]] %>%
    html_nodes('a') %>%
    html_attr(name = "href")  %>%
    enframe(name = NULL) %>% 
    rename(team_url = value) %>%
    filter(str_detect(team_url, "team/"))
  
  names <- teams_2021 %>%
    html_nodes('table') %>%
    .[[1]] %>%
    html_nodes('a') %>%
    html_text()  %>%
    enframe(name = NULL) %>%
    rename(team_name = value)
  
  together <- cbind(links, names, table) %>%
    janitor::clean_names() %>%
    filter(number_riders > 1) %>%
    select(team_name, team_url, class) %>%
    filter(!class %in% c("WTW", "UCI", "P-CRO")) %>%
    mutate(team_name = stringi::stri_trans_general(str = team_name, id = "Latin-ASCII"),
           year = years[[y]]) %>%
    anti_join(dbGetQuery(con, "SELECT DISTINCT team as team_name, season as year FROM pcs_points_season_rider") %>%
                mutate(team_name = str_trim(team_name)), by = c("team_name", "year"))
  
  teams_2021 <- together %>%
    filter(class %in% c("WT", "PRT"))
  
  tm_list <- vector("list", length(teams_2021$team_url))
  
  for(t in 1:length(teams_2021$team_url)) {

    page <- paste0("https://www.procyclingstats.com/", teams_2021$team_url[[t]], "/season/points-per-rider") %>%
      read_html()

    tm <- page %>%
      html_nodes('h1') %>%
      html_text() %>%
      str_replace('»', '') %>%
      str_replace(paste0(years[[y]]), "")

    r1 <- page %>%
      html_nodes('table') %>%
      html_table() %>%
      .[[1]] %>%
      janitor::clean_names() %>%
      filter(rider != "")

    r2 <- page %>%
      html_nodes('table') %>%
      html_nodes('a') %>%
      html_attr(name = "href")

    riders_team <- cbind(r1, rider_url = r2) %>%
      as_tibble() %>%
      mutate(team_url = teams_2021$team_url[[t]],
             team = tm) %>%
      unique() %>%
      janitor::clean_names()

    if(nrow(riders_team) > 0) {
      tm_list[[t]] <- riders_team
    }

    Sys.sleep(7)

  }

  #

  riders_2021 <- bind_rows(tm_list) %>%

    mutate(rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
    separate(team, c("team", "level"), sep = '\\(') %>%
    mutate(level = str_replace(level, "\\)", "")) %>%
    mutate(team = stringi::stri_trans_general(str = team, id = "Latin-ASCII")) %>%

    select(team, level, rider, rider_url, pcs_points = points) %>%

    mutate(rider = str_to_title(tolower(rider)))

  riders_season <- riders_2021 %>% mutate(season = years[[y]])

  dbWriteTable(con, "pcs_points_season_rider", riders_season, append = TRUE, row.names = FALSE)
  
  #
  #
  #
  
  together <- cbind(links, names, table) %>%
    janitor::clean_names() %>%
    filter(number_riders > 1) %>%
    select(team_name, team_url, class) %>%
    filter(class %in% c("WT", "PRT")) %>%
    mutate(team_name = stringi::stri_trans_general(str = team_name, id = "Latin-ASCII"),
           year = years[[y]]) %>%
    anti_join(dbGetQuery(con, "SELECT DISTINCT team as team_name, season as year FROM pcs_season_team") %>%
                mutate(team_name = str_trim(team_name)), by = c("team_name", "year"))
  
  teams_2021 <- together
  
  for(t in 1:length(teams_2021$team_url)) {

    page <- paste0("https://www.procyclingstats.com/", teams_2021$team_url[[t]]) %>%
      read_html()

    tm <- page %>%
      html_nodes('h1') %>%
      html_text() %>%
      str_replace('»', '') %>%
      str_replace(paste0(years[[y]]), "")

    df <- page %>%
      html_nodes('ul.list') %>%
      html_nodes('li')

    r1 <- df %>% html_nodes('a') %>% html_attr(name = "href")
    r2 <- df %>% html_nodes('a') %>% html_text()

    riders_team <- cbind(r1,r2) %>%
      as_tibble() %>%
      mutate(team_url = teams_2021$team_url[[t]],
             team = tm) %>%
      filter(str_detect(r1, "rider/")) %>%
      unique()

    tm_list[[t]] <- riders_team

    Sys.sleep(7)

  }

  #

  riders_2021 <- bind_rows(tm_list) %>%

    mutate(rider = iconv(r2, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
    separate(team, c("team", "level"), sep = '\\(') %>%
    mutate(level = str_replace(level, "\\)", "")) %>%
    mutate(team = stringi::stri_trans_general(str = team, id = "Latin-ASCII")) %>%

    select(team, level, rider, rider_url = r1) %>%

    mutate(rider = str_to_title(tolower(rider)))

  riders_season <- riders_2021 %>% mutate(season = years[[y]])

  dbWriteTable(con, "pcs_season_team", riders_season, append = TRUE, row.names = FALSE)
  
  print(years[[y]])
  
}






#
#
#

years = seq(2017,2021,1)

for(y in 1:length(years)) {
  
  teams_2021 <- paste0('https://www.procyclingstats.com/teams.php?year=', years[[y]], '&filter=Filter&s=worldtour') %>% 
    read_html() %>% 
    html_nodes('div.mt20') %>% 
    html_nodes('a') %>% 
    html_attr(name = "href") %>%
    enframe(name = NULL) %>% 
    rename(team_url = value) %>%
    unique()
  
  tm_list <- vector("list", length(teams_2021$team_url))
  
  for(t in 1:length(teams_2021$team_url)) {
    
    page <- paste0("https://www.procyclingstats.com/", teams_2021$team_url[[t]], "/season/points-per-rider") %>%
      read_html()
    
    tm <- page %>%
      html_nodes('h1') %>%
      html_text() %>% 
      str_replace('»', '') %>%
      str_replace(paste0(years[[y]]), "")
    
    r1 <- page %>%
      html_nodes('table') %>%
      html_table() %>%
      .[[1]] %>%
      janitor::clean_names() %>%
      filter(rider != "")
    
    r2 <- page %>%
      html_nodes('table') %>%
      html_nodes('a') %>% 
      html_attr(name = "href")
    
    riders_team <- cbind(r1, rider_url = r2) %>%
      as_tibble() %>%
      mutate(team_url = teams_2021$team_url[[t]],
             team = tm) %>%
      unique() %>%
      janitor::clean_names()
    
    tm_list[[t]] <- riders_team
    
    Sys.sleep(4)
    
  }
  
  #
  
  riders_2021 <- bind_rows(tm_list) %>%
    
    mutate(rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
    separate(team, c("team", "level"), sep = '\\(') %>%
    mutate(level = str_replace(level, "\\)", "")) %>%
    mutate(team = stringi::stri_trans_general(str = team, id = "Latin-ASCII")) %>%
    
    select(team, level, rider, rider_url, pcs_points = points) %>%
    
    mutate(rider = str_to_title(tolower(rider)))
  
  riders_season <- riders_2021 %>% mutate(season = years[[y]])
  
  dbWriteTable(con, "pcs_points_season_rider", riders_season, append = TRUE, row.names = FALSE)
  
  print(years[[y]])
  
}

#

kmeans_model <- read_rds('Stored models/rider-clusters-kmeans-model.rds')

centers <- kmeans_model$centers %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(CL = rowname) %>%
  gather(stat, center, -CL)

#

prc <- dbGetQuery(con, "SELECT * FROM performance_rider_clustering WHERE Date = '2023-06-19'") %>%
  rename(Date = date)

dist <- prc %>%
  select(-in_final_group, -rel_sof) %>%
  
  # filter out riders without enough races (max will normally be around 75 so 15+ races needed)
  group_by(Date) %>%
  filter(races > (max(races)/7)) %>%
  ungroup() %>%
  
  gather(stat, value, -rider, -Date, -races) %>%
  
  # scale each stat
  group_by(stat) %>%
  mutate(value = (value - mean(value, na.rm = T)) / sd(value, na.rm = T)) %>%
  ungroup() %>%
  
  # join with centers
  inner_join(centers, by = c("stat")) %>%
  
  # take sum squared euclidean distance
  group_by(rider, Date, races, CL) %>%
  summarize(SED = sum((value - center)^2, na.rm = T)) %>%
  ungroup() %>%
  
  # filter out smallest distance
  group_by(rider, Date, races) %>%
  filter(SED == min(SED, na.rm = T)) %>%
  ungroup() %>%
  
  select(-SED) %>%
  
  # match with correct cluster labels
  inner_join(tibble(CL = c("1","2","3","4","5","6"), type = c("Domestique", "Sprint Helper", "Mtn Helper", "Sprinter", "Puncheur", "Climber")), by = c("CL"))

#
#
#
#
#

team_clusters <- riders_allseasons %>%
  
  mutate(match_rider = tolower(rider)) %>%
  
  left_join(dist %>%
              mutate(rider = tolower(rider)) %>%
              select(-Date), by = c("match_rider" = "rider")) %>%
  
  left_join(prc %>%
              mutate(rider = tolower(rider)) %>%
              select(-Date, -races), by = c("match_rider" = "rider")) %>%
  
  select(-match_rider) %>%
  
  mutate(team = str_trim(team)) %>%
  
  mutate(prior_team = ifelse(Season == 2021, "",
                             case_when(team == "AG2R Citroen Team" ~ "AG2R Citroen Team",
                                       team == "Astana Qazaqstan Team" ~ "Astana - Premier Tech",
                                       team == "Bahrain - Victorious" ~ "Bahrain - Victorious",
                                       team == "BORA - hansgrohe" ~ "BORA - hansgrohe",
                                       team == "Cofidis" ~ "Cofidis", 
                                       team == "EF Education - Nippo" ~ "EF Education - Nippo",
                                       team == "Groupama - FDJ" ~ "Groupama - FDJ",
                                       team == "INEOS Grenadiers" ~ "INEOS Grenadiers",
                                       team == "Intermarche - Wanty - Gobert Materiaux" ~ "Intermarche - Wanty - Gobert Materiaux", 
                                       team == "Israel Start-Up Nation" ~ "Israel Start-Up Nation",
                                       team == "Lotto Soudal" ~ "Lotto Soudal",
                                       team == "Movistar Team" ~ "Movistar Team",
                                       team == "Quick Step-Alpha Vinyl" ~ "Deceuninck - Quick Step", 
                                       team == "Team BikeExchange Jayco" ~ "Team BikeExchange",
                                       team == "Team DSM" ~ "Team DSM",
                                       team == "Team Jumbo-Visma" ~ "Team Jumbo-Visma", 
                                       team == "Team Qhubeka NextHash" ~ "Team Qhubeka NextHash",
                                       team == "Trek - Segafredo" ~ "Trek - Segafredo",
                                       team == "UAE-Team Emirates" ~ "UAE-Team Emirates",
                                       team == "Alpecin-Fenix" ~ "Alpecin-Fenix",
                                       team == "B&B Hotels - KTM" ~ "B&B Hotels p/b KTM",
                                       team == "Bardiani-CSF-Faizane" ~ "Bardiani-CSF-Faizane",
                                       team == "Bingoal Pauwels Sauces WB" ~ "Bingoal - Wallonie Bruxelles",
                                       team == "Burgos-BH" ~ "Burgos-BH",
                                       team == "Caja Rural - Seguros RGA" ~ "Caja Rural - Seguros RGA",
                                       team == "Drone Hopper -  Androni Giocattoli" ~ "Androni Giocattoli - Sidermec", 
                                       team == "EOLO-Kometa" ~ "EOLO-Kometa",
                                       team == "Equipo Kern Pharma" ~ "Equipo Kern Pharma", 
                                       team == "Euskaltel - Euskadi" ~ "Euskaltel - Euskadi",
                                       team == "Gazprom - RusVelo" ~ "Gazprom - RusVelo",
                                       team == "Human Powered Health" ~ "Rally Cycling",
                                       team == "Sport Vlaanderen - Baloise" ~ "Sport Vlaanderen - Baloise",
                                       team == "Team Arkea Samsic" ~ "Team Arkea Samsic",
                                       team == "Team Novo Nordisk" ~ "Team Novo Nordisk",
                                       team == "Team TotalEnergies" ~ "Team TotalEnergies",
                                       team == "Uno-X Pro Cycling Team" ~ "Uno-X Pro Cycling Team",
                                       TRUE ~ ""))) %>%
  
  filter(!(Season == 2021 & ((rider == "Hirschi Marc" & team == "Team DSM") |
                               (rider == "Van Wilder Ilan" & team == "Deceuninck - Quick Step")))) %>%
  
  filter(!(Season == 2021 & team == "Team Qhubeka ASSOS")) %>%
  
  #left_join(
  #  
  #  dbReadTable(con, "pcs_pts_riders_season") %>%
  #     mutate(year = year+1) %>%
  #     select(rider, year, pcs_pts_per, pcs_pts), by = c("rider", "Season" = "year")) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT * FROM lme4_rider_logranks_sq
               WHERE Date = '2022-10-18'"), by = c("rider")
    
  ) %>%
  
  mutate(pcd_impact = ifelse(sqpcd_impact > 0.02, 0.02, sqpcd_impact)) %>%
  
  mutate(rider_match = str_to_title(rider)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, date as dob FROM rider_attributes") %>%
      filter(!is.na(dob)) %>%
      mutate(rider = str_to_title(rider)), by = c("rider_match" = "rider")) %>%
  
  mutate(age = as.numeric(as.Date(paste0(Season,"-06-30"))-as.Date(dob))/365.25) %>%
  
  select(-rider_match, -dob) %>%
  
  mutate(actual_rider = rider,
         rider = "Hansen Adam",
         actual_age = age) %>%
  mutate(age = age-1) %>%
  mutate(predicted_aging = mgcv::predict.gam(read_rds("Stored models/aging_curve_gam.rds"), .)) %>%
  mutate(age = age+1) %>%
  mutate(predicted_aging2 = mgcv::predict.gam(read_rds("Stored models/aging_curve_gam.rds"), .)) %>%
  mutate(aging_delta = (predicted_aging2+100)/(predicted_aging+100)) %>%
  select(-age, -rider) %>%
  rename(age = actual_age,
         rider = actual_rider) %>%
  
  unique()

#
#
#

team_clusters %>% 
  unique() %>%
  filter(Season == 2022) %>%
  select(-CL) %>%
  left_join(
    team_clusters %>% 
      unique() %>%
      filter(Season == 2021) %>%
      select(rider, team2021 = team), by = c("rider")
  ) %>%
  mutate(type = ifelse(is.na(type), "Domestique", type),
         NewToTeam = ifelse(is.na(team2021), "New to Team",
                            ifelse(team2021 == prior_team, "From Last Year", "New to Team"))) %>% 
  
  group_by(team, NewToTeam, level) %>% 
  summarize(riders=  n(), 
            pcs_pts_per = mean(pcs_pts_per, na.rm = T),
            Pts = sum((exp(points)-0.135), na.rm = T), 
            PCD = mean(weighted_pcd, na.rm = T),
            LDR = mean(leader, na.rm = T)) %>%
  ungroup() -> team_stats

#
#
#
#
#

ggplot(team_clusters %>% 
         unique() %>%
         filter(str_trim(team) == 'Jumbo-Visma' & Season == 2022) %>%
         select(-CL) %>%
         left_join(
           team_clusters %>% 
             unique() %>%
             filter(Season == 2021) %>%
             select(rider, team2021 = team), by = c("rider")
         ) %>%
         mutate(type = ifelse(is.na(type), "Domestique", type),
                NewToTeam = ifelse(is.na(team2021), "New to Team",
                                   ifelse(team2021 == prior_team, "From Last Year", "New to Team"))) %>%
         rbind(
           
           team_clusters %>% 
             unique() %>%
             filter(str_trim(team) == 'Team DSM' & Season == 2021) %>%
             select(-CL) %>%
             left_join(
               team_clusters %>% 
                 unique() %>%
                 filter(Season == 2022) %>%
                 select(rider, team2021 = team), by = c("rider")
             ) %>%
             mutate(type = ifelse(is.na(type), "Domestique", type),
                    NewToTeam = ifelse(is.na(team2021), "Left Team",
                                       ifelse(team2021 == team, "From Last Year", "Left Team"))) %>%
             filter(NewToTeam == "Left Team")
           
         ),
       
       aes(x = leader, y = weighted_pcd, label = rider, 
           color = type, #size = (exp(points)-0.13)^2,
           size = 1^1.25))+
  
  # scale sizes correctly
  geom_blank(data = team_clusters %>%
               filter(rider %in% c('Van Aert Wout', 'Carretero Hector')))+

  geom_point(stroke = 0.75)+
  
  ggrepel::geom_label_repel(size=3, color = "black", fill = "white")+

  scale_y_continuous(breaks = seq(0,20,4))+
  scale_x_continuous(labels = scales::percent)+
  theme(plot.title = element_text(face = "bold", size = 18), 
        axis.text = element_text(size = 15))+
  
  labs(x = "Leader: % of races as #1 on team", 
       y = "Parcours fit: climbing difficulty of better performances", 
       size = "Success points",
       subtitle = "how often is rider the team leader / which parcours fit a rider\nSize of circle indicates PCS Points")+
  expand_limits(y = c(0,16), x = c(0,0.6))+
  
  scale_fill_manual(values = c("#F02108", "gray40", "#F0A608",
                                "#37B36C", "#16BEF2", "#162CF2"), name = "Rider Type")+
  
  scale_color_manual(values = c("#F02108", "gray40", "#F0A608",
                               "#37B36C", "#16BEF2", "#162CF2"), name = "Rider Type")+
  
  scale_size_continuous(range = c(2,15))+
  guides(size = FALSE)+
  
  facet_wrap(~NewToTeam)

#
#
#

all_teams <- team_clusters %>% filter(Season == 2022) %>% select(team) %>% unique() %>% .[[1]]

for(x in 1:length(all_teams)) {

  TN <- all_teams[[x]]
  
  data_for <- team_clusters %>% 
    unique() %>%
    filter(str_trim(team) == TN & Season == 2022) %>%
    select(-CL) %>%
    left_join(
      team_clusters %>% 
        unique() %>%
        filter(Season == 2021) %>%
        select(rider, team2021 = team), by = c("rider")
    ) %>%
    mutate(type = ifelse(is.na(type), "Domestique", type),
           NewToTeam = ifelse(is.na(team2021), "New to Team",
                              ifelse(team2021 == prior_team, "From Last Year", "New to Team"))) %>%
    rbind(
      
      team_clusters %>% 
        unique() %>%
        filter(str_trim(team) == TN & Season == 2021) %>%
        select(-CL) %>%
        left_join(
          team_clusters %>% 
            unique() %>%
            filter(Season == 2022) %>%
            select(rider, team2021 = team), by = c("rider")
        ) %>%
        mutate(type = ifelse(is.na(type), "Domestique", type),
               NewToTeam = ifelse(is.na(team2021), "Left Team",
                                  ifelse(team2021 == team, "From Last Year", "Left Team"))) %>%
        filter(NewToTeam == "Left Team")) %>%
    
    mutate(CL = ((random_intercept+(4.5*pcd_impact)+3.8)),
           BS = ((random_intercept+(1*pcd_impact)+bunchsprint_impact+3.8)),
           MT = ((random_intercept+(10*pcd_impact)+3.8))) %>%
    mutate(hm = 3 / ((1/CL)+(1/BS)+(1/MT))) %>% rowwise() %>% mutate(best = min(CL,BS,MT))
  
  new <- (data_for %>% filter(NewToTeam == "New to Team") %>% nrow()) / data_for %>% nrow()
  
  if(new < 0.33) {
    new=0.5
  } else{
    new=new/0.5
  }
  
  p <- ggplot(data_for %>%
                mutate(ability = hm),

              aes(x = bunchsprint_impact*-1, y = pcd_impact*-10, label = rider, 
                  fill = ability))+
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    
    annotate("text", x = 1.6, y = -1.25, label = "better\nsprinters", size=8, alpha = 0.5)+
    annotate("text", x = -0.6, y = 0.75, label = "better\nclimbers", size=8, alpha = 0.5)+ 
    annotate("text", x = -0.6, y = -1.25, label = "better\nclassics", size=8, alpha = 0.5)+ 
    geom_point(size=6, shape = 21, stroke=0.75, color="black", alpha=0.75)+
    geom_blank(data = team_clusters %>%
                 filter(rider %in% c('Evenepoel Remco', 'Bennett Sam',
                                     'Rivera Kevin', 'Merlier Tim',
                                     'Van Aert Wout')) %>%
                 mutate(CL = ((random_intercept+(4.5*pcd_impact)+3.8)),
                        BS = ((random_intercept+(1*pcd_impact)+bunchsprint_impact+3.8)),
                        MT = ((random_intercept+(10*pcd_impact)+3.8))) %>% 
                 mutate(hm = 3 / ((1/CL)+(1/BS)+(1/MT))) %>%
                 rowwise() %>% mutate(ability = min(CL,BS,MT),
                                      ability = hm))+
    ggrepel::geom_label_repel(size=3, color = "black", fill = "white", alpha=0.75)+
    
    scale_y_continuous(breaks = seq(-1.5,1,0.5))+
    scale_x_continuous(breaks = seq(-1,2,0.5))+
    scale_fill_gradientn(colors = c("#FF005C",
                                    "#FF0000",
                                    "#FF4500",
                                    "#FF6800",
                                    "#FF9600",
                                    #"#FFC100",
                                    "#FFE000",
                                    #"#FFFE00",
                                    "#FFFFB8",
                                    "#888888",
                                    "#000000"))+
    
    theme(plot.title = element_text(face = "bold", size = 18),
          axis.title = element_text(size = 14),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          strip.text = element_text(size=14, color = "white", face = "bold"),
          strip.background = element_rect(fill = "black"),
          panel.grid.major = element_line(color = "gray80", size = 0.25),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "gray90"),
          legend.position = "bottom",
          legend.text = element_blank())+
    
    labs(x = "Bunch Sprint Ability", 
         y = "Climbing Ability",
         fill = "Overall\nAbility",
         title = paste0(TN, " 2022 team plot"))+
    
    facet_grid(~NewToTeam, space = "free")
  
  #
  
  ggplotGrob(p) -> gp
  facet.columns <- gp$layout$l[grepl("panel-1-2", gp$layout$name)]
  gp$widths[facet.columns] <- gp$widths[facet.columns] * new
  
  ggsave(filename=paste0(TN, "-2022.png"), plot=gp, height = 7.5, width = 11)
 
  print(TN)
   
}
  
#
#
#
#
#

success_model <- dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, bunchsprint_impact
                                  FROM lme4_rider_success WHERE DATE = '2020-12-04'") %>%
  
  mutate(random_intercept = random_intercept - 5.2) %>%
  
  mutate(flat_BS = (random_intercept + bunchsprint_impact + (2 * pcd_impact)),
         flats = (random_intercept + (2 * pcd_impact)),
         hills = (random_intercept + (5 * pcd_impact)),
         climbs = (random_intercept + (10 * pcd_impact)),
         high_mtns = (random_intercept + (18 * pcd_impact))
         ) %>%
  
  mutate(flat_BS = exp(flat_BS) / (1+exp(flat_BS)),
         flats = exp(flats) / (1+exp(flats)),
         hills = exp(hills) / (1+exp(hills)),
         climbs = exp(climbs) / (1+exp(climbs)),
         high_mtns = exp(high_mtns) / (1+exp(high_mtns))) %>%
  
  select(rider, flat_BS:high_mtns)

#
#
#

teams_success_2021 <- riders_2021 %>%
  
  mutate(match_rider = tolower(rider)) %>%
  
  left_join(success_model %>%
              mutate(rider = tolower(rider)), by = c("match_rider" = "rider")) %>%
  
  select(-match_rider)

#
#
#
#
#

leader_model <- dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, bunchsprint_impact
                                  FROM lme4_rider_teamleader WHERE DATE = '2020-12-04'") %>%
  
  mutate(random_intercept = random_intercept - 2) %>%
  
  mutate(flat_BS = (random_intercept + bunchsprint_impact + (2 * pcd_impact)),
         flats = (random_intercept + (2 * pcd_impact)),
         hills = (random_intercept + (5 * pcd_impact)),
         climbs = (random_intercept + (10 * pcd_impact)),
         high_mtns = (random_intercept + (18 * pcd_impact))
  ) %>%
  
  mutate(flat_BS = exp(flat_BS) / (1+exp(flat_BS)),
         flats = exp(flats) / (1+exp(flats)),
         hills = exp(hills) / (1+exp(hills)),
         climbs = exp(climbs) / (1+exp(climbs)),
         high_mtns = exp(high_mtns) / (1+exp(high_mtns))) %>%
  
  select(rider, flat_BS:high_mtns)

#
#
#

teams_leader_2021 <- riders_2021 %>%
  
  mutate(match_rider = tolower(rider)) %>%
  
  left_join(leader_model %>%
              mutate(rider = tolower(rider)), by = c("match_rider" = "rider")) %>%
  
  select(-match_rider)

#
#
#
#
#
#
#
#
#
#

success_model_all <- dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, bunchsprint_impact, Date
                                  FROM lme4_rider_success") %>%
  
  mutate(random_intercept = random_intercept - 5) %>%
  
  mutate(flat_BS = (random_intercept + bunchsprint_impact + (2 * pcd_impact)),
         flats = (random_intercept + (2 * pcd_impact)),
         hills = (random_intercept + (5 * pcd_impact)),
         climbs = (random_intercept + (10 * pcd_impact)),
         high_mtns = (random_intercept + (18 * pcd_impact))
  ) %>%
  
  mutate(flat_BS = exp(flat_BS) / (1+exp(flat_BS)),
         flats = exp(flats) / (1+exp(flats)),
         hills = exp(hills) / (1+exp(hills)),
         climbs = exp(climbs) / (1+exp(climbs)),
         high_mtns = exp(high_mtns) / (1+exp(high_mtns))) %>%
  
  select(rider, Date, flat_BS:high_mtns) %>%
  
  gather(parcours, success, flat_BS:high_mtns) %>%
  
  group_by(Date, parcours) %>%
  mutate(rank = rank(-success, ties.method = "min")) %>%
  ungroup()

#
#
#

teams_success_all_2021 <- riders_2021 %>%
  
  mutate(match_rider = tolower(rider)) %>%
  
  left_join(success_model_all %>%
              mutate(rider = tolower(rider)), by = c("match_rider" = "rider")) %>%
  
  select(-match_rider)

#

ggplot(teams_success_all_2021 %>%
         filter(rider %in% c("Froome Chris", "Thomas Geraint", "Roglic Primoz",
                             "Pogacar Tadej", "Pinot Thibaut", "Bernal Egan")),
       aes(x = as.Date(Date), y = climbs, color = rider))+
  
  geom_line(size=1)+
  
  scale_y_continuous(labels = scales::percent)+
  
  labs(x = "",
       y = "Bunch Sprint Success Model",
       title = "Bunch Sprint Expected Performance")

#
#
#

rider_averages <- dbGetQuery(con, "SELECT * FROM performance_rider_clustering WHERE Date = '2020-12-04'") %>%
  
  gather(stat, value, rel_sof:in_final_group)

#

rider_stat_averages_2021 <- riders_2021 %>%
  
  mutate(match_rider = tolower(rider)) %>%
  
  left_join(rider_averages %>%
              mutate(rider = tolower(rider)), by = c("match_rider" = "rider")) %>%
  
  select(-match_rider)

#
#
#
#
#
#
#
#
#
#
#
#

riders_2023 <- dbGetQuery(con, "SELECT * FROM pcs_season_team WHERE season = 2023") %>%
  mutate(rider = case_when(rider == "Skjelmose Mattias" ~ "Skjelmose Jensen Mattias",
                           TRUE ~ rider)) %>%
  mutate(team = str_trim(team)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT * FROM lme4_rider_logranks_sq
               WHERE Date = '2022-10-19' AND test_or_prod = 'unwt_new'"), by = c("rider")) %>%
  
  mutate(rider_match = str_to_title(rider)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, date as dob FROM rider_attributes") %>%
      filter(!is.na(dob)) %>%
      mutate(rider = str_to_title(rider)), by = c("rider_match" = "rider")) %>%
  
  mutate(age = as.numeric(as.Date(paste0(season,"-06-30"))-as.Date(dob))/365.25) %>%
  
  select(-rider_match, -dob) %>%
  
  rename(odr_impact = one_day_race) %>%
  
  expand_grid(
    
    dbGetQuery(con, "SELECT bunch_sprint, one_day_race, pred_climb_difficulty 
               FROM stage_data_perf 
               WHERE year = 2022 AND rnk = 1") %>%
      filter(!is.na(pred_climb_difficulty)) %>% 
      mutate(pred_climb_difficulty = ifelse(pred_climb_difficulty <= 0, 0, pred_climb_difficulty ^ 2)) %>%
      mutate(rand = runif(n(), 0, 1)) %>%
      filter(rank(desc(rand), ties.method = "first") <= 100)) %>%
  
  mutate(prediction = exp(4.6 + random_intercept + (pred_climb_difficulty * sqpcd_impact) + (one_day_race * odr_impact) + (bunch_sprint * bunchsprint_impact))) %>%
  
  group_by(team, level, rider, season, age, random_intercept, sqpcd_impact, odr_impact, bunchsprint_impact) %>%
  summarize(prediction = quantile(prediction, probs = 0.25, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(climbing = exp(4.6 + random_intercept + (225 * sqpcd_impact) + (0 * odr_impact) + (0 * bunchsprint_impact)),
         sprinting = exp(4.6 + random_intercept + (0 * sqpcd_impact) + (0 * odr_impact) + (1 * bunchsprint_impact)),
         classics = exp(4.6 + random_intercept + (25 * sqpcd_impact) + (1 * odr_impact) + (0 * bunchsprint_impact)))

#

riders_2023 %>% 
  filter(team == "AG2R Citroen Team") %>% 
  ggplot(aes(x = age, y = prediction, label = rider))+
  geom_text()+
  scale_y_reverse()

riders_2023 %>% 
  mutate(climbing = ifelse(climbing > 100, 100, climbing),
         sprinting = ifelse(sprinting > 100, 100, sprinting),
         classics = ifelse(classics > 100, 100, classics)) %>%
  filter(team == "Groupama - FDJ") %>% 
  ggplot(aes(x = sprinting, y = climbing, label = rider, color = classics))+
  geom_vline(xintercept=25)+
  geom_hline(yintercept=25)+
  geom_text()+
  #scale_x_log10()+
  #scale_y_log10()+
  scale_color_gradientn(colors = c("#37B36C", "#769684", "gray50", "#AD94A6", "#D56Db7"))+
  geom_blank(data = expand_grid(climbing = c(3, 100),
                                sprinting = c(3, 100),
                                classics = c(3, 100),
                                rider = "x"))

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

Y = 2018

teams_pages <- paste0('https://www.procyclingstats.com/teams.php?year=', Y, '&filter=Filter&s=worldtour') %>%
  read_html() %>%
  html_nodes('div.mt20') %>%
  html_nodes('a') %>%
  html_attr(name = "href") %>%
  enframe(name = NULL) %>%
  rename(team_url = value) %>%
  unique()

#

tm_list <- vector("list", length(teams_pages$team_url))
tm_list2 <- tm_list

for(t in 1:length(teams_pages$team_url)) {
  
  page <- paste0("https://www.procyclingstats.com/", teams_pages$team_url[[t]]) %>%
    read_html()
  
  tm <- page %>%
    html_nodes('h1') %>%
    html_text() %>% 
    str_replace('»', '') %>%
    str_replace(paste0(Y), "")
  
  h3_elements <- page %>%
    html_nodes("span.table-cont") %>%
    html_nodes("table")
  
  staff_urls <- h3_elements[[1]] %>%
    html_nodes('a') %>%
    html_attr(name = "href")
  
  h3_elements <- h3_elements %>%
    html_table()
  
  staff <- h3_elements[[1]] %>%
    cbind(url = staff_urls)%>%
    mutate(team_url = teams_pages$team_url[[t]],
           team = tm)
  
  gear <- h3_elements[[2]] %>%
    janitor::clean_names() %>%
    rename(gear = x, brand = x_2) %>%
    mutate(team_url = teams_pages$team_url[[t]],
           team = tm)
  
  tm_list[[t]] <- staff
  tm_list2[[t]] <- gear
  
  Sys.sleep(4)
  
}

#

all_staff <- bind_rows(tm_list) %>%
  separate(team, c("team", "level"), sep = '\\(') %>%
  mutate(level = str_replace(level, "\\)", ""),
         team = str_trim(team)) %>%
  mutate(team = stringi::stri_trans_general(str = team, id = "Latin-ASCII")) %>%
  
  select(team, level, team_url, staff_name = Name, staff_role = Functions, url)

all_gear <- bind_rows(tm_list2) %>%
  separate(team, c("team", "level"), sep = '\\(') %>%
  mutate(level = str_replace(level, "\\)", ""),
         team = str_trim(team)) %>%
  mutate(team = stringi::stri_trans_general(str = team, id = "Latin-ASCII"),
         brand = stringi::stri_trans_general(str = brand, id = "Latin-ASCII")) %>%
  
  select(team, level, team_url, gear, brand)

dbWriteTable(con, "pcs_all_staff", all_staff %>% mutate(season = Y), append = T, row.names = F)

dbWriteTable(con, "pcs_all_gear", all_gear %>% mutate(season = Y) %>% unique(), append = T, row.names = F)

#
#
#

all_staff <- dbGetQuery(con, "SELECT * FROM pcs_all_staff WHERE season >= 2021") %>%
  
  select(url) %>%
  unique()

#

for(s in 1:nrow(all_staff)) {
  
  staff_page <- paste0("https://www.procyclingstats.com/", all_staff$url[[s]]) %>%
    read_html()
  
  df <- staff_page %>%
    html_nodes('span.table-cont') %>%
    html_nodes('table') %>%
    html_table() %>%
    .[[3]]
  
  dbWriteTable(con, "pcs_staff_races", df %>% mutate(staff_url = all_staff$url[[s]]), append = T, row.names = F)
  
  print(df)
  
  Sys.sleep(5)
  
}

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

race_url <- 'race/tour-de-france/2023'
choose_stage = 1

URL <- paste0('https://www.procyclingstats.com/', str_replace(race_url,"/gc",""), '/startlist')

#

page <- URL %>%
  read_html() 

startlist <- page %>%
  html_nodes('ul.startlist_v4') %>%
  html_nodes('a')

riders <- startlist %>%
  html_text() %>%
  enframe(name = NULL) %>%
  mutate(value = iconv(value, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
  mutate(value = str_to_title(tolower(value)))

teams <- cbind(
  startlist %>%
    html_text() %>%
    enframe(name = NULL),
  startlist %>%
    html_attr(name = "href") %>%
    enframe(name = NULL) %>%
    rename(url = value)) %>%
  filter(value != "") %>%
  mutate(team_2nd = ifelse(str_detect(url, "team"), value, NA)) %>%
  fill(team_2nd, .direction = "down") %>%
  mutate(value = iconv(value, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
  mutate(value = str_to_title(tolower(value))) %>%
  select(-url) %>%
  rename(team = team_2nd)

riders_in_race <- cbind(riders %>% rename(rider = value)) %>%
  filter(!str_detect(rider, "(Wt)")) %>%
  filter(!str_detect(rider, "(Prt)")) %>%
  inner_join(teams, by = c("rider" = "value"))

#

kmeans_model <- read_rds('Stored models/rider-clusters-kmeans-model.rds')

centers <- kmeans_model$centers %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(CL = rowname) %>%
  gather(stat, center, -CL)

#

prc <- dbGetQuery(con, "SELECT * FROM performance_rider_clustering WHERE Date = '2023-06-19'") %>%
  rename(Date = date)

dist <- prc %>%
  select(-in_final_group, -rel_sof) %>%
  
  # filter out riders without enough races (max will normally be around 75 so 15+ races needed)
  group_by(Date) %>%
  filter(races > (max(races)/7)) %>%
  ungroup() %>%
  
  gather(stat, value, -rider, -Date, -races) %>%
  
  # scale each stat
  group_by(stat) %>%
  mutate(value = (value - mean(value, na.rm = T)) / sd(value, na.rm = T)) %>%
  ungroup() %>%
  
  # join with centers
  inner_join(centers, by = c("stat")) %>%
  
  # take sum squared euclidean distance
  group_by(rider, Date, races, CL) %>%
  summarize(SED = sum((value - center)^2, na.rm = T)) %>%
  ungroup() %>%
  
  # filter out smallest distance
  group_by(rider, Date, races) %>%
  filter(SED == min(SED, na.rm = T)) %>%
  ungroup() %>%
  
  select(-SED) %>%
  
  # match with correct cluster labels
  inner_join(tibble(CL = c("1","2","3","4","5","6"), type = c("Domestique", "Sprint Helper", "Mtn Helper", "Sprinter", "Puncheur", "Climber")), by = c("CL"))

#
#
#
#
#

team_clusters <- riders_in_race %>%
  
  mutate(match_rider = tolower(rider)) %>%
  
  left_join(dist %>%
              mutate(rider = tolower(rider)) %>%
              select(-Date), by = c("match_rider" = "rider")) %>%
  
  left_join(prc %>%
              mutate(rider = tolower(rider)) %>%
              select(-Date, -races), by = c("match_rider" = "rider")) %>%
  
  select(-match_rider) %>%
  
  mutate(team = str_trim(team)) %>%
  
  left_join(
    dbGetQuery(con, "SELECT * FROM lme4_rider_logranks_sq
               WHERE Date = '2023-06-18' AND test_or_prod = 'BS_not_ODR'") %>%
      inner_join(riders_in_race) %>%
      mutate(bs_perf = 1-percent_rank(exp(4.5 + (bunchsprint_impact) + random_intercept)),
             mtn_perf = 1-percent_rank(exp(4.5 + (sqpcd_impact * 400) + random_intercept)),
             brk_perf = 1-percent_rank(exp(4.5 + (sqpcd_impact * 64) + (bunchsprint_impact * 0.25) + (one_day_race * 0.5) + random_intercept))) %>%
      select(rider, bs_perf:brk_perf) %>%
      gather(parcours, rk, -rider) %>%
      group_by(rider) %>%
      mutate(rank = rank(desc(rk), ties.method = "first"),
             rank = ifelse(rank == 1, 0.6, ifelse(rank == 2, 0.3, 0.1)),
             rank = rank * rk) %>%
      summarize(perf = sum(rank, na.rm = T)) %>%
      ungroup(), by = c("rider")
    
  ) %>%
  
  left_join(
    dbGetQuery(con, "SELECT * FROM lme4_rider_logranks_sq
               WHERE Date = '2023-06-18' AND test_or_prod = 'BS_not_ODR'"), by = c("rider")
    
  ) %>%
  
  unique() %>%

  mutate(bs_perf = 1-percent_rank(exp(4.5 + (bunchsprint_impact) + random_intercept)),
         mtn_perf = 1-percent_rank(exp(4.5 + (sqpcd_impact * 400) + random_intercept)),
         brk_perf = 1-percent_rank(exp(4.5 + (sqpcd_impact * 64) + (bunchsprint_impact * 0.25) + (one_day_race * 0.5) + random_intercept))) %>%
  
  mutate(pctrk = percent_rank(perf)*10) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT * FROM lme4_rider_breakaway
               WHERE Date = '2023-06-18'") %>%
      mutate(stage_6 = exp(model_intercept+random_intercept+(pcd_impact*20.5)+(bunchsprint_impact*0)),
             stage_10 = exp(model_intercept+random_intercept+(pcd_impact*7.5)+(bunchsprint_impact*0.27)),
             stage_4 = exp(model_intercept+random_intercept+(pcd_impact*1)+(bunchsprint_impact*0.88))) %>%
      select(rider, stage_6:stage_4), by = c("rider")) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT * FROM lme4_rider_timetrial
               WHERE Date = '2023-06-23'") %>%
      mutate(stage_16 = exp(modelintercept + (tvg_impact * 17) + random_intercept)) %>%
      select(rider, stage_16), by = c("rider")) %>%
  
  mutate(leader = ifelse(is.na(leader), mean(leader, na.rm = T), leader)) %>%
  
  inner_join(dbGetQuery(con, "SELECT * FROM model_implied_power") %>%
               gather(stat, value, c(rel025:abs40, aboverrl_40:aboverrl_025)) %>% 
               mutate(stat = str_replace(stat, "rel", "relative_"), 
                      stat = str_replace(stat, "abs", "absolute_")) %>%
               separate(stat, c("power", "duration"), sep = "_") %>% 
               
               mutate(duration = ifelse(duration == "025", 0.25, duration),
                      power = ifelse(power == "aboverrl", "absolute over relative", power)) %>%
               
               filter(power %in% c("absolute", "relative")), by = c("rider")) %>%
  
  mutate(power = paste0(power,"_", duration)) %>% 
  select(-duration) %>%
  spread(power, value) %>%
  mutate(stage_16 = ifelse(is.na(stage_16), median(stage_16, na.rm = T), stage_16)) %>%
  
  mutate(itt = stage_16,
         brk = (stage_6+stage_10+stage_4)/3,
         abs = (absolute_5+absolute_2+absolute_0.25)/3,
         rel = (relative_5+relative_10+relative_20)/3)
  
#

set.seed(17843)

principal_components <- prcomp(team_clusters[,c(10, 21:22, 24, 45:48)], scale = TRUE, center = TRUE)

six_clusters_kmeans <- kmeans(principal_components$x, centers = 6)
seven_clusters_kmeans <- kmeans(principal_components$x, centers = 8)

centers_6 <- team_clusters %>% select(rider, team) %>% cbind(CL = six_clusters_kmeans$cluster)
centers_7 <- team_clusters %>% select(rider, team) %>% cbind(CL = seven_clusters_kmeans$cluster)

#

team_clusters_supp <- team_clusters %>% 
  select(-CL) %>% 
  inner_join(centers_7) %>%
  mutate(rider_type = case_when(CL == 1 ~ "Overall Contender",
                                CL == 2 ~ "Domestique",
                                CL == 3 ~ "Breakaways",
                                CL == 4 ~ "Climbers",
                                CL == 5 ~ "Puncheur",
                                CL == 6 ~ "Sprint Helper",
                                CL == 7 ~ "Breakaways",
                                CL == 8 ~ "Sprinter",
                                TRUE ~ NA)) %>%
  
  mutate(rider_type = case_when(rider %in% c("Simmons Quinn", "Peters Nans", "Lafay Victor",
                                             "Van Gils Maxim", "Sanchez Luis Leon", "Konrad Patrick") ~ "Breakaways",
                                rider %in% c("Haig Jack", "Landa Mikel", "Paret-Peintre Aurelien",
                                             "Pinot Thibaut", "Madouas Valentin", "Ciccone Giulio",
                                             "Guerreiro Ruben", "Barguil Warren", "Soler Marc") ~ "Climbers",
                                rider %in% c("Laengen Vegard Stake", "Bjerg Mikkel", "Van Den Berg Lars",
                                             "Castroviejo Jonathan", "Durbridge Luke", "Tejada Harold",
                                             "Fedorov Yevgeniy", "Kung Stefan", "Naesen Oliver",
                                             "Turner Ben", "Boivin Guillaume", "Arndt Nikias",
                                             "Trentin Matteo", "Craddock Lawson", "Van Baarle Dylan") ~ "Domestique",
                                rider %in% c("Teunissen Mike", "Van Poppel Danny") ~ "Sprint Helper",
                                rider %in% c("Gros?chartner Felix") ~ "Puncheur",
                                TRUE ~ rider_type))

#
#
#
#
#

ggplot(team_clusters_supp %>% 
         unique() %>%
         filter(str_trim(team) == 'Soudal - Quick Step (WT)'),
       
       aes(x = bs_perf, y = mtn_perf, label = rider, 
           fill = pctrk,
           size = leader))+
  
  # scale sizes correctly
  geom_blank(data = team_clusters_supp %>%
               unique() %>%
               filter(rider %in% c('Van Aert Wout', 'Majka Rafal', 'Haller Marco', 'Le Gac Olivier',
                                   'Vingegaard Jonas', "Pogacar Tadej", "Delaplace Anthony", 'Abrahamsen Jonas')))+
  
  geom_point(stroke = 0.75, color = "black", shape = 21)+
  geom_point(size = 1)+
  ggrepel::geom_label_repel(size=3, color = "black", fill = "white")+
  
  scale_y_continuous(breaks = seq(0,1,0.25), labels = scales::percent)+
  scale_x_continuous(breaks = seq(0,1,0.25), labels = scales::percent)+
  theme(plot.title = element_text(face = "bold", size = 18), 
        axis.text = element_text(size = 15))+
  
  labs(x = "Percentile rank in bunch sprints", 
       y = "Percentile rank in mountains", 
       size = "% of races\nas #1 on team",
       title = "Lotto Dstny")+
  expand_limits(y = c(0,1), x = c(0,1))+
  
  scale_fill_gradientn(colors = c('#ffffff','#fddbc7','#ef8a62','#b2182b'),
                        name = "Performance\nout of 10")+

  scale_size_continuous(range = c(1,16), label = scales::percent)

#
#

teams <- team_clusters %>% 
  select(team) %>% 
  unique() %>%
  mutate(team_table = team) %>%
  mutate(team = iconv(team, from="UTF-8", to = "ASCII//TRANSLIT"),
         team = str_trim(str_replace(team, "\\(.+?\\)", ""))) %>%
  mutate(team_display = case_when(team == "Intermarche - Circus - Wanty" ~ "Intermarche",
                                  team == "Team DSM - Firmenich" ~ "Team DSM",
                                  team == "Team Jayco AlUla" ~ "Jayco AlUla",
                                  team == "Team Arkea Samsic" ~ "Arkea Samsic",
                                  team == "Astana Qazaqstan Team" ~ "Astana Qazaqstan",                                  
                                  TRUE ~ team))

#

for(t in 1:nrow(teams)) {
  
  ggplot(team_clusters_supp %>% 
           unique() %>%
           filter(str_trim(team) == teams$team_table[[t]]),
         
         aes(x = bs_perf, y = mtn_perf, label = rider, 
             color = rider_type,
             size = pctrk))+
    
    # scale sizes correctly
    geom_blank(data = team_clusters_supp %>%
                 unique() %>%
                 filter(rider %in% c('Van Aert Wout', 'Majka Rafal', 'Haller Marco', 'Le Gac Olivier', 'Ewan Caleb',
                                     'Vingegaard Jonas', "Pogacar Tadej", "Delaplace Anthony", 'Abrahamsen Jonas',
                                     'Gregaard Jonas', 'Lampaert Yves', 'Politt Nils')))+
    
    geom_point(stroke = 0.75)+
    geom_point(size = 1, color = "black")+
    ggrepel::geom_label_repel(size=3, color = "black", fill = "white")+
    
    scale_y_continuous(breaks = seq(0,1,0.25), labels = scales::percent)+
    scale_x_continuous(breaks = seq(0,1,0.25), labels = scales::percent)+
    theme(plot.title = element_text(face = "bold", size = 18), 
          axis.text = element_text(size = 15))+
    
    labs(x = "Percentile rank in bunch sprints", 
         y = "Percentile rank in mountains", 
         size = "Performance\nout of 10",
         title = teams$team_display[[t]])+
    expand_limits(y = c(0,1), x = c(0,1))+
    
    scale_color_manual(values = c("#162CF2", "#F02108", "gray40",
                                  "#F0A608", "#16BEF2", "#A503EC", "#23EC03"), name = "Rider Type")+
    
    scale_size_continuous(range = c(1,16))
  
  ggsave(paste0("tdf23-", teams$team[[t]], ".png"), height = 7, width = 8)
  
}
