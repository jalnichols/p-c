
library(tidyverse)
library(DBI)
library(RMySQL)
library(rvest)

#

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#
#
# pull in each race and check for Live Stats

all_2019_races <- dbGetQuery(con, "SELECT DISTINCT race, stage, stage_name, year, Class, url 
                             FROM pcs_stage_data WHERE year = 2021 AND
                             Class IN ('2.1', '1.1', '2.HC', '1.HC', '1.UWT', '2.UWT', 'WC', '1.Pro', '2.Pro')") %>%
  mutate(live_stats_url = "") %>%
  
  mutate(url = ifelse(stage == 0, paste0(url, "/prologue"),
                      ifelse(Class %in% c("1.1", "1.HC", "1.Pro", "1.UWT", "WC"), 
                             paste0(url), paste0(url, "/stage-", str_trim(str_sub(stage_name, 7,8))))))

#

for(r in 1:length(all_2019_races$url)) {
  
  checker <- paste0("D:/Jake/Documents/PCS-HTML/", str_replace_all(str_replace(all_2019_races$url[[r]], "race/", ""), "/", "")) %>%
    
    read_html() %>%
    html_nodes('a') %>%
    html_text() %>%
    enframe(name = NULL) %>%
    filter(str_detect(value, "LiveStats"))
  
  if(length(checker$value)>0) {  
    
    all_2019_races$live_stats_url[[r]] <- paste0("race/",all_2019_races$url[[r]],"/today/livestats")
    
    print(all_2019_races$url[[r]])
    
  }
  
}

#
# some races are missed, so scrape any in a race that is a hit
#

possible_races <- all_2019_races %>%
  
  mutate(valid = ifelse(live_stats_url == "", 0, 1)) %>%
  
  group_by(race) %>%
  filter(max(valid)==1) %>%
  ungroup() %>%
  
  mutate(live_stats_url = paste0("race/", url,"/today/livestats"))

#
#
# this simply pulls in un-structured list of all comments in the Live Stats page on PCS

storage_list <- vector("list", length(possible_races$live_stats_url))

#

for(r in 1:length(possible_races$live_stats_url)) {
  
  #download.file(url = paste0("https://www.procyclingstats.com/", 
  #                           possible_races$live_stats_url[[r]]), 
  #              
  #              destfile = paste0("PCS-HTML/", str_replace_all(possible_races$live_stats_url[[r]], "/", "")),
  #              quiet = TRUE)
  
  page <- paste0("PCS-HTML/", str_replace_all(possible_races$live_stats_url[[r]], "/", "")) %>%
    
    read_html()
  
  tst <-     page %>%
    html_nodes('ul.timeline2') %>%
    html_nodes('li') %>%
    html_nodes('div.txt') %>%
    html_text()
  
  if(length(tst)==0) {
    
    
  } else {
    
    df <- cbind(
      
      page %>%
        html_nodes('ul.timeline2') %>%
        html_nodes('li') %>%
        html_nodes('div.txt') %>%
        html_text(),
      page %>%
        html_nodes('ul.timeline2') %>%
        html_nodes('li') %>%
        html_nodes('div.bol') %>%
        html_text()) %>%
      as_tibble() %>%
      rename(Note = V1,
             KM_Left = V2) %>%
      mutate(race = possible_races$race[[r]],
             url = possible_races$url[[r]],
             year = possible_races$year[[r]])
    
    storage_list[[r]] <- df
    
  }
  
  print(r)
  
  Sys.sleep(runif(1,5,25))
  
}

#
#
#

data_dump <- bind_rows(storage_list) %>%
  
  mutate(KM_Left = as.numeric(KM_Left),
         attack = str_detect(tolower(Note), "attack"),
         caught = str_detect(tolower(Note), "caught"),
         group = str_detect(tolower(Note), "group"),
         dropped = str_detect(tolower(Note), "dropped"),
         peloton = str_detect(tolower(Note), "peloton"),
         timegap = str_detect(tolower(Note), "timegap"),
         pulling = str_detect(tolower(Note), "pulling")) %>%
  
  filter(!is.na(KM_Left)) %>%
  
  mutate(chars = nchar(Note)) %>%
  
  filter(chars > 0) %>%
  filter(chars < 300)

#

#dbWriteTable(con, "pcs_livestats", data_dump, row.names = FALSE, overwrite = TRUE)
