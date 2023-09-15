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

months_to_pull <- expand_grid(months = c(paste0("0", seq(1,9,1)), "10", "11", "12"),
                              years = seq(2014,2022,1)) %>%
  filter((years == 2022 & as.numeric(months) >= 6))

#

for(m in 1:nrow(months_to_pull)) {

  races <- paste0("https://www.cyclingnews.com/races/archive/", months_to_pull$years[[m]], "/", months_to_pull$months[[m]], "/") %>%
    
    read_html() %>%
    html_nodes('div.archive-list') %>%
    html_nodes('li.day-article')
  
  #
  
  races_df <- cbind(
    races %>% html_text() %>% enframe(name = NULL) %>% rename(race = value) %>% mutate(race = str_replace_all(race, "\n", "")),
    races %>% html_nodes('a') %>% html_attr(name = 'href') %>% enframe(name = NULL) %>% rename(url = value)
  ) %>%
    
    cbind(months_to_pull[m,])
  
  #
  
  dbWriteTable(con, "cyclingnews_races", races_df, append = TRUE, row.names = FALSE)
  
  Sys.sleep(5)
  
}

#
#
#
#
#

cn_races <- dbReadTable(con, "cyclingnews_races") %>%
  filter(years == 2022 & months %in% c("6", "7", "8")) %>%
  filter(!str_detect(race, "Cyclo-cross")) %>%
  filter(!str_detect(race, "MTB")) %>%
  filter(!str_detect(race, "Ladies")) %>%
  filter(!str_detect(race, "Women")) %>%
  filter(!str_detect(race, "Countdown")) %>%
  filter(!str_detect(race, "Hour Record attempt")) %>%
  filter(!str_detect(race, "Mountain Bike")) %>%
  filter(!str_detect(race, "National Championship")) %>%
  filter(!str_detect(race, "UCI Track")) %>%
  filter(!str_detect(race, "Supercross")) %>%
  filter(!str_detect(race, "UCI Marathon")) %>%
  filter(!str_detect(race, "XC")) %>%
  filter(!str_detect(race, "Zwift Tour")) %>%
  filter(!str_detect(race, "Zwift Class")) %>%
  filter(!str_detect(race, "X2O")) %>%
  filter(!str_detect(race, "Badkamers Trofee")) %>%
  filter(!str_detect(race, "Wisconsin Off Road Series")) %>%
  filter(!str_detect(race, "Off Road")) %>%
  filter(!str_detect(race, "Waaslandcross")) %>%
  filter(!str_detect(race, "Cyclocross")) %>%
  filter(!str_detect(race, "Feminas")) %>%
  filter(!str_detect(race, "Superprestige")) %>%
  filter(!str_detect(race, "Virtual")) %>%
  filter(!str_detect(race, "USA Crit")) %>%
  filter(!str_detect(race, "UEC Cyclo-Cross")) %>%
  filter(!str_detect(race, "UEC Elite Track")) %>%
  filter(!str_detect(race, "UCI Esports")) %>%
  filter(!str_detect(race, "Cyclo-Cross")) %>%
  filter(!str_detect(race, "Feminin")) %>%
  filter(!str_detect(race, "Trek Cup")) %>%
  filter(!str_detect(race, "CX")) %>%
  filter(!str_detect(race, "build-up")) %>%
  filter(!str_detect(race, "Féminin")) %>%
  unique()

#

for(r in 1:nrow(cn_races)) {
  
  race_url = cn_races$url[[r]]
  
  stages <- race_url %>%
    
    read_html() %>%
    
    html_nodes('a') %>%
    
    html_attr(name = "href") %>%
    
    enframe(name = NULL) %>%
    
    rename(stage_url = value) %>%
    
    filter(str_detect(stage_url, "results")) %>%
    filter(str_detect(stage_url, "races")) %>%
    unique() %>%
    mutate(race = cn_races$race[[r]],
           race_url = cn_races$url[[r]],
           year = cn_races$years[[r]])
  
  print(nrow(stages))
  
  dbWriteTable(con, "cyclingnews_stages", stages, append = TRUE, row.names = FALSE)
  
  Sys.sleep(15)
  
}

#
#
#
#
#
#

cn_stages <- dbReadTable(con, "cyclingnews_stages")

cn_stages <- cn_stages %>%
  inner_join(read_delim("cn_stages-remove.csv") %>% filter(is.na(keep)) %>%
               select(race), by = c("race")) %>%
  
  filter(!str_detect(stage_url, "mixed-relay")) %>%
  filter(!str_detect(stage_url, "women-junior")) %>%
  filter(!str_detect(stage_url, "men-junior")) %>%
  filter(!str_detect(stage_url, "womens-junior")) %>%
  filter(!str_detect(stage_url, "mens-junior")) %>%
  filter(!str_detect(stage_url, "individual-time-trial")) %>%
  filter(!str_detect(stage_url, "women-elite")) %>%
  filter(!str_detect(stage_url, "u23-time-trials")) %>%
  filter(!str_detect(stage_url, "u23-women")) %>%
  filter(!str_detect(stage_url, "mens-time-trial")) %>%
  filter(!str_detect(stage_url, "team-time-trial")) %>%
  filter(!str_detect(stage_url, "time-trial-men")) %>%
  filter(!str_detect(stage_url, "time-trial-women")) %>%
  filter(!str_detect(stage_url, "women")) %>%
  
  filter(!(str_detect(stage_url, paste0(year - 1)))) %>%
  
  group_by(race, race_url) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  
  mutate(priority = ifelse(year <= 2019, 1, 0)) %>%
  arrange(desc(priority), desc(year), desc(n))

#

for(s in 1:nrow(cn_stages)) {
  
  stage_url = cn_stages$stage_url[[s]]
  
  paragraph <- stage_url %>%
    
    read_html() %>%
    
    html_nodes('p') %>%
    
    html_text() %>%
    
    enframe(name = NULL) %>%

    rename(paragraph_text = value) %>%
    filter(paragraph_text != "") %>%
    filter(paragraph_text != "\n") %>%
    filter(!str_detect(paragraph_text, "unlimited access")) %>%
    filter(!str_detect(paragraph_text, "first month for just")) %>%
    filter(!str_detect(paragraph_text, "you for reading")) %>%
    filter(!str_detect(paragraph_text, "Full Results")) %>%
    filter(!str_detect(paragraph_text, "your first month")) %>%
    filter(!str_detect(paragraph_text, "There was a problem")) %>%
    filter(!str_detect(paragraph_text, "Cyclingnews")) %>%
    filter(!str_detect(paragraph_text, "five free articles")) %>%
    filter(!str_detect(paragraph_text, "after your trial")) %>%
    filter(!str_detect(paragraph_text, "latest race content")) %>%
    cbind(cn_stages[s,] %>% select(stage_url)) %>%
    
    mutate(breakpara1 = str_detect(paragraph_text, "break"),
           breakpara2 = str_count(paragraph_text, "\\(")) %>%
    
    filter(breakpara1 == TRUE | breakpara2 > 0)
  
  print(stage_url)
  #print(paragraph %>% arrange(desc(breakpara2)) %>% .[1,] %>% select(paragraph_text) %>% .[[1]])
  
  dbWriteTable(con, "cyclingnews_paras", paragraph, append = TRUE, row.names = FALSE)
  
  Sys.sleep(15)
  
}

#

cn_paras <- dbGetQuery(con, "SELECT * FROM cyclingnews_paras") %>% 
  inner_join(cn_stages %>% select(stage_url, year) %>% filter(year == 2022)) %>%
  
  separate(stage_url, c("j0", "j1", "j2", "j3", "race", "stage", "j4"), sep = "/") %>%
  select(-c("j1","j2","j3","j4","j0"))

#
#
#
#
#

breakaways <- read_delim("breakaway-riders-2019.csv") %>%
  rename(url = `...4`) %>%
  fill(url, .direction = "down") %>%
  
  mutate(year = ifelse(is.na(race), "", str_sub(race, nchar(race)-3, nchar(race))),
         year = ifelse(year == "", str_sub(url, nchar(url)-3, nchar(url)), year)) %>%
  
  fill(race, .direction = "down") %>%
  fill(year, .direction = "down") %>%
  fill(stage, .direction = "down") %>%

  left_join(dbGetQuery(con, "SELECT rider, COUNT(*) as N FROM pcs_stage_data WHERE year >= 2018
                       GROUP BY rider"))
