

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

#

linked_from <- dbGetQuery(con, "SELECT * FROM strava_matched_activities") %>% 
  mutate(unknown_activity_id = str_replace(activity_id, "/activities/", "")) %>%
  inner_join(df, by = c("unknown_activity_id" = "activity_id")) %>%
  
  inner_join(dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Distance')") %>% 
  
  # I would like to bring in weight here so when I cut-off too low watts below it is watts/kg
  
  # clean up the dates
  mutate(Y = str_sub(DATE, nchar(DATE)-3, nchar(DATE))) %>% 
  separate(DATE, into = c("weekday", "date", "drop"), sep = ",") %>% 
  mutate(date = paste0(str_trim(date),", ", Y)) %>% 
  select(-weekday, -drop, -Y) %>% 
  
  # clean up the stat values
  mutate(VALUE = str_replace(VALUE, "mi", ""), 
         VALUE = str_replace(VALUE, "W", ""),
         VALUE = ifelse(Stat == "AvgTemperature",
                        str_sub(VALUE, 1, nchar(VALUE)-8), VALUE),
         VALUE = as.numeric(VALUE)) %>% 
  
  mutate(date = lubridate::mdy(date)) %>% 
  unique() %>% 
  spread(Stat, VALUE) %>% 
  
  janitor::clean_names() %>% 
  
  mutate(pcs = str_to_title(pcs)) %>%
  
  inner_join(dbGetQuery(con, "SELECT * FROM pcs_stage_data_we WHERE year IN (2020, 2021)") %>%
               
               mutate(date = as.Date(date),
                      rider = str_to_title(rider)), by = c("date", "pcs" = "rider")) %>%
  
  # if two results exist for same day matching distance, it's probably a recon and TT which
  # means drop the lower watts
  
  # also, many riders include distance outside the TT as part of their strava activity
  # so maybe accept any riders +/- 10 km? or maybe we just can't get accurate TT data
  
  mutate(distance = distance * 1.609) %>% 
  filter((distance / length) > 0.5) %>%
  filter((distance / length) < 1.2) %>%
  filter((time_trial == 1 & (distance / length) > 0.8) | time_trial == 0) %>%
  
  unique() %>%
  
  left_join(dbGetQuery(con, "SELECT activity_id, activity_type FROM strava_activities") %>%
              unique(), by = c("activity_id")), by = c("matched_from" = "activity_id"))

#

linked_from %>% 
  filter(year > 2018) %>%
  filter(class %in% c("1.1", "2.1", "2.Pro", "2.HC", "2.UWT", "1.Pro", "1.HC", "1.UWT", "WC", "CC", 
                      "2.2", "1.2", "2.2U", "1.2U", "1.Ncup", "2.Ncup") |
           race %in% c("tour de l'avenir", "giro ciclistico d'italia")) %>% 
  select(unknown_activity_id, activity_id, rider, PCS) %>% unique() -> sig_races

#

unique_df <- sig_races %>%
  
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
  filter(activity_id %in% unique_df$unknown_activity_id)

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