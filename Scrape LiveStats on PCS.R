
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

# hit pcs

front_page <- 'https://www.procyclingstats.com/' %>%
  
  read_html()

container <- front_page %>%
  
  html_nodes('div.situLiveCont')

races_live <- container %>%
  html_nodes('a.situ-live') %>%
  html_attr(name = "href") %>%
  enframe(name = NULL) %>%
  mutate(value = paste0("https://www.procyclingstats.com/", value)) %>%
  rename(url = value)

#
#
# Pull in races that need to be monitored

# this won't work properly when there's multiple live races ongoing so I will need to adjust at some point

for(r in 1:length(races_live$url)) {
  
  live_url <- races_live$url[[r]]
  
  #live_url <- 'https://web.archive.org/web/20190713125643/https://www.procyclingstats.com/race/tour-de-france/2019/stage-8/today/situation'
  
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
    
    print(KM_left)
    
    if(KM_left == 0) {
      
      Counter = Counter - 1
      
    }
    
    # pull in data
    
    groups <- hit_page %>%
      html_nodes('ul.situ3') %>%
      html_nodes('li')
    
    # dig into groups
    
    grp_list <- vector("list", length(groups))
    
    for(g in 1:length(groups)) {
      
      time <- groups[[g]] %>%
        html_nodes('span.time') %>%
        html_text()
      
      if(length(time)==0) {
        
        time = 0
        
      }
      
      grp_no <- groups[[g]] %>%
        html_nodes('div.bol') %>%
        html_text()
      
      are_there_riders <- groups[[g]] %>%
        html_nodes('table.riders2') %>%
        html_text()
      
      if(are_there_riders != "") {
        
        riders <- groups[[g]] %>%
          html_nodes('table.riders2') %>%
          html_table() %>%
          .[[1]] %>%
          gather(position, stat, -X1) %>%
          mutate(lgth = nchar(stat)) %>%
          group_by(X1) %>%
          filter(lgth == max(lgth, na.rm = T)) %>%
          ungroup() %>%
          select(rider = stat) %>%
          
          mutate(time_gap = time,
                 group = grp_no)
        
      } else {
        
        riders <- tibble(rider = "No riders listed / Peloton",
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
      
      select(-V1, -V2, -V3, -V4, -V8, -V9, -V10) %>%
      mutate(updated_at = lubridate::now())
    
    # write to table
    
    dbWriteTable(con, "pcs_live_on_roads", groups_on_road, append = TRUE, row.names = FALSE)
    
    print(groups_on_road)
    
    # Sleep and advance iterations
    
    iter = iter + 1
    Sys.sleep(runif(1, 90, 150))
    
  }
  
}

#
#
#
#
#

dbDisconnect(con)