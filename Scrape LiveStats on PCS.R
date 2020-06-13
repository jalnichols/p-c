
library(tidyverse)
library(rvest)
library(DBI)
library(rjson)

readRenviron("p-c/.Renviron")

con <- DBI::dbConnect(
  RMariaDB::MariaDB(), 
  user = Sys.getenv("MYSQL_U"), 
  password = Sys.getenv("MYSQL_P"), 
  db = "cycling", 
  host = Sys.getenv("MYSQL_H"))

print("connected to DB")

#
#
# NEED A WHOLE SECTION WHICH FINDS LIVE EVENTS

live_url <- 'https://web.archive.org/web/20190713125643/https://www.procyclingstats.com/race/tour-de-france/2019/stage-8/today/situation'

#
#
# scraper will stop when KM_left == 0 AND when it has done 12 more iterations (roughly 30 minutes)

KM_left = 500
Counter = 15
iter = 0

while(KM_left > 0 & Counter > 0 & iter <= 250) {
  
  # hit page, recalculate KMs left, reset Counter if necessary
  hit_page <- live_url %>%
    read_html()
  
  KM_left <- hit_page %>%
    
    html_nodes('ul') %>%
    html_nodes('div') %>%
    html_nodes(xpath = '//*[@id="kmtogo"]') %>%
    html_text()
  
  if(KM_left == 0) {
    
    Counter = Counter - 1
    
  }
  
  # pull in data
  
  groups <- hit_page %>%
    html_nodes('ul.situ2') %>%
    html_nodes('li')
  
  # dig into groups
  
  grp_list <- vector("list", length(groups))
  
  for(g in 1:length(groups)) {
    
    time <- groups[[g]] %>%
      html_nodes('div.time') %>%
      html_text()
    
    grp_no <- groups[[g]] %>%
      html_nodes('div.bol2') %>%
      html_text()
    
    riders <- cbind(
      
      groups[[g]] %>%
        html_nodes('div.riders') %>%
        html_nodes('a') %>%
        html_attr(name = "href") %>%
        enframe(name = NULL) %>%
        rename(rider_url = value),
      
      groups[[g]] %>%
        html_nodes('div.riders') %>%
        html_nodes('a') %>%
        html_text() %>%
        enframe(name = NULL) %>%
        rename(rider = value)) %>%
      
      mutate(time_gap = time,
             group = grp_no)
    
    if(length(riders$rider_url)==0) {
      
      riders <- tibble(rider_url = "none",
                       rider = "No riders listed / Peloton",
                       time_gap = time,
                       group = grp_no)
      
    }
    
    grp_list[[g]] <- riders %>%
      mutate(time_gap = ifelse(time_gap == "???", "0", str_replace(time_gap, "\\?\\?\\?", "")))
    
  }
  
  # stage info
  
  stage_info <- hit_page %>%
    html_nodes('div.entry.race') %>%
    html_nodes('span')
  
  # groups
  
  groups_on_road <- bind_rows(grp_list) %>%
    
    mutate(KM_left = as.numeric(KM_left),
           race_url = live_url,
           race_urlx = race_url) %>%
    
    separate(race_urlx, into = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10"), sep = "/") %>%
    
    select(-V1, -V2, -V3, -V4, -V8, -V9, -V10)
  
  # write to table
  
  dbWriteTable(con, "pcs_live_on_roads", groups_on_road, append = TRUE, row.names = FALSE)
  
  # Sleep and advance iterations
  
  iter = iter + 1
  Sys.sleep(runif(1, 90, 150))
  
}

#
#
#
#
#

