

library(tidyverse)
library(DBI)
library(RMySQL)
library(rvest)

dbDisconnect(con)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

df <- dbGetQuery(con, "SELECT rider, PCS, activity_id FROM strava_activity_data WHERE PCS = 'missing' AND Stat = 'Distance'")

unique_df <- df %>%
  
  arrange(desc(as.numeric(activity_id))) %>%
  
  group_by(rider) %>%
  mutate(total = n()) %>%
  filter(rank(activity_id, ties.method = "first") == 1) %>%
  ungroup() %>%
  
  filter(total >= 0)

#

pull_from_strava_html <- fs::dir_info("D:/Jake/Documents/Strava-Pages") %>%
  
  mutate(activity_id = str_replace(path, "D:/Jake/Documents/Strava-Pages/activity_id-", ""),
         activity_id = str_replace(activity_id, ".html", "")) %>%
  
  select(activity_id, path) %>%
  filter(activity_id %in% unique_df$activity_id)

#

data_pulled <- vector("list", length(pull_from_strava_html$activity_id))

#

for(a in 1:length(pull_from_strava_html$activity_id)) {
    
    PAGE <- read_html(pull_from_strava_html$path[[a]])
    
    # rider info
    
    data_pulled[[a]] <- tibble(rider = PAGE %>% html_nodes(xpath = '//*[@id="heading"]/header/h2/span/a') %>% html_text(),
                               strava_rider_url = PAGE %>% html_nodes(xpath = '//*[@id="heading"]/header/h2/span/a') %>% html_attr(name = "href"),
                               linked_activity_id = pull_from_strava_html$activity_id[[a]] 
                               )
    
    print(a)
  
}

#

new_rider_matches <- bind_rows(data_pulled) %>%
  
  inner_join(df %>%
               mutate(rider = str_replace(rider, "https://www.strava.com", "")) %>%
               group_by(rider) %>% count() %>% ungroup(), by = c("strava_rider_url" = "rider"))

#
#
#
#
#

missing <- dbGetQuery(con, 'SELECT DISTINCT rider FROM strava_activity_data WHERE pcs = "missing" AND Stat = "Distance"')

overwrite_these <- read_csv('pcs-to-overwrite.csv') %>%
  
  mutate(strava_rider_url = paste0('https://www.strava.com', strava_rider_url)) %>%
  filter(!str_detect(PCS, "'")) %>%
  
  filter(strava_rider_url %in% missing$rider) %>%
  
  mutate(PCS = str_to_title(tolower(PCS)))

#

for(x in 1:length(overwrite_these$strava_rider_url)) {
  
  tictoc::tic()
  
  dbSendQuery(con,
              
              paste0("UPDATE strava_activity_data SET PCS = '", overwrite_these$PCS[[x]],"' WHERE PCS = 'missing' AND rider = '", overwrite_these$strava_rider_url[[x]], "'")
              
              )
  
  print(x)
  print(overwrite_these$PCS[[x]])
  
  tictoc::toc()
  
}

#
#
#
#
#

mismatch <- readxl::read_excel("Mismatched Names.xlsx") %>%
  
  filter(!str_detect(Should_Be, "'"))

#

for(x in 1:length(mismatch$lower_case_match)) {
  
  tictoc::tic()
  
  dbSendQuery(con,
              
              paste0("UPDATE strava_activity_data SET PCS = '", mismatch$Current[[x]],"' WHERE PCS = '", mismatch$Should_Be[[x]], "'")
              
  )
  
  print(x)
  print(mismatch$lower_case_match[[x]])
  
  tictoc::toc()
  
}