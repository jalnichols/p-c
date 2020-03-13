
library(tidyverse)
library(rvest)

#

scraped_list19 <- vector("list",20)

for(x in 1:14) {
  
  url <- paste0('https://www.procyclingstats.com/rankings.php?id=49002&nation=&team=&page=', 
  (x-1)*200, 
  '&prev_id=prev&younger=&older=&limit=200&filter=Filter&morefilters=1')
  
  page <- url %>%
    read_html()
  
  rider_links <- page %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_attr(name = 'href') %>%
    .[seq(1,1195,6)] %>%
    enframe(name = NULL) %>%
    rename(url = value)
  
  rider_names <- page %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_text() %>%
    .[seq(1,1195,6)]  %>%
    str_trim() %>%
    enframe(name = NULL) %>%
    rename(rider = value)
  
  scraped_list19[[x]] <- cbind(rider_names, rider_links)
  
  Sys.sleep(5)
  
}

#
#
#

riders19 <- bind_rows(scraped_list19) %>%
  mutate(rider = str_trim(rider))

#
#
#

scraped_list18 <- vector("list",20)

for(x in 1:14) {
  
  url <- paste0('https://www.procyclingstats.com/rankings.php?id=31654&nation=&team=&page=', 
                (x-1)*200, 
                '&prev_id=prev&younger=&older=&limit=200&filter=Filter&morefilters=1')
  
  page <- url %>%
    read_html()
  
  rider_links <- page %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_attr(name = 'href') %>%
    .[seq(1,1195,6)] %>%
    enframe(name = NULL) %>%
    rename(url = value)
  
  rider_names <- page %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_text() %>%
    .[seq(1,1195,6)]  %>%
    enframe(name = NULL) %>%
    rename(rider = value)
  
  scraped_list18[[x]] <- cbind(rider_names, rider_links)
  
  Sys.sleep(5)
  
}

#
#
#

riders18 <- bind_rows(scraped_list18) %>%
  mutate(rider = str_trim(rider))


#
#
#

scraped_list17 <- vector("list",20)

for(x in 1:14) {
  
  url <- paste0('https://www.procyclingstats.com/rankings.php?id=14803&nation=&team=&page=', 
                (x-1)*200, 
                '&prev_id=prev&younger=&older=&limit=200&filter=Filter&morefilters=1')
  
  page <- url %>%
    read_html()
  
  rider_links <- page %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_attr(name = 'href') %>%
    .[seq(1,1195,6)] %>%
    enframe(name = NULL) %>%
    rename(url = value)
  
  rider_names <- page %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_text() %>%
    .[seq(1,1195,6)]  %>%
    enframe(name = NULL) %>%
    rename(rider = value)
  
  scraped_list17[[x]] <- cbind(rider_names, rider_links)
  
  Sys.sleep(5)
  
}

#
#
#

riders17 <- bind_rows(scraped_list17) %>%
  mutate(rider = str_trim(rider))

#

scraped_list16 <- vector("list",20)

for(x in 1:14) {
  
  url <- paste0('https://www.procyclingstats.com/rankings.php?id=273&nation=&team=&page=', 
                (x-1)*200, 
                '&prev_id=prev&younger=&older=&limit=200&filter=Filter&morefilters=1')
  
  page <- url %>%
    read_html()
  
  rider_links <- page %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_attr(name = 'href') %>%
    .[seq(1,1195,6)] %>%
    enframe(name = NULL) %>%
    rename(url = value)
  
  rider_names <- page %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_text() %>%
    .[seq(1,1195,6)]  %>%
    enframe(name = NULL) %>%
    rename(rider = value)
  
  scraped_list16[[x]] <- cbind(rider_names, rider_links)
  
  Sys.sleep(5)
  
}

#
#
#

riders16 <- bind_rows(scraped_list16) %>%
  mutate(rider = str_trim(rider))

#
#

scraped_list15 <- vector("list",20)

for(x in 1:14) {
  
  url <- paste0('https://www.procyclingstats.com/rankings.php?id=81&nation=&team=&page=', 
                (x-1)*200, 
                '&prev_id=prev&younger=&older=&limit=200&filter=Filter&morefilters=1')
  
  page <- url %>%
    read_html()
  
  rider_links <- page %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_attr(name = 'href') %>%
    .[seq(1,1195,6)] %>%
    enframe(name = NULL) %>%
    rename(url = value)
  
  rider_names <- page %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_text() %>%
    .[seq(1,1195,6)]  %>%
    enframe(name = NULL) %>%
    rename(rider = value)
  
  scraped_list15[[x]] <- cbind(rider_names, rider_links)
  
  Sys.sleep(5)
  
}

#
#
#

riders15 <- bind_rows(scraped_list15) %>%
  mutate(rider = str_trim(rider))

#
#
#

scraped_list14 <- vector("list",20)

for(x in 1:14) {
  
  url <- paste0('https://www.procyclingstats.com/rankings.php?id=82&nation=&team=&page=', 
                (x-1)*200, 
                '&prev_id=prev&younger=&older=&limit=200&filter=Filter&morefilters=1')
  
  page <- url %>%
    read_html()
  
  rider_links <- page %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_attr(name = 'href') %>%
    .[seq(1,1195,6)] %>%
    enframe(name = NULL) %>%
    rename(url = value)
  
  rider_names <- page %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_text() %>%
    .[seq(1,1195,6)]  %>%
    enframe(name = NULL) %>%
    rename(rider = value)
  
  scraped_list14[[x]] <- cbind(rider_names, rider_links)
  
  Sys.sleep(5)
  
}

#
#
#

riders14 <- bind_rows(scraped_list14) %>%
  mutate(rider = str_trim(rider))

#
#
#
#
#

riders_to_scrape <- rbind(riders19 %>% rowid_to_column(),
                          riders18  %>% rowid_to_column(),
                          riders17  %>% rowid_to_column(), 
                          riders16  %>% rowid_to_column(), 
                          riders15  %>% rowid_to_column(), 
                          riders14  %>% rowid_to_column()
                          ) %>%
  arrange(rowid) %>%
  
  select(-rowid) %>%
  
  unique() %>%
  
  rename(rider_url = url) %>%
  
  anti_join(dbReadTable(con, "rider_attributes"), by = c("rider_url"))
  
#
#
#
#
#

rider_data_list <- vector("list", length(riders_to_scrape$rider_url))

for(r in 1:length(riders_to_scrape$rider_url)) {
  
  if(r < 1) {
    
    
  } else {
  
  pg <- paste0("https://www.procyclingstats.com/", riders_to_scrape$rider_url[[r]]) %>%
    
    read_html()
  
  stg <- pg %>%
    
    html_nodes('div.rdr-info') %>%
    html_text()
  
  rider_data_list[[r]] <- tibble(
    
    dob = str_trim(str_sub(stg, str_locate(stg, "Date of birth:")[[1]] + 14, str_locate(stg, "Nationality")[[1]] - 5)),
    weight = str_trim(str_sub(stg, str_locate(stg, "Weight:")[[2]] + 1, str_locate(stg, "Height")[[1]] - 1)),
    height = str_trim(str_sub(stg, str_locate(stg, "Height:")[[2]] + 1, str_locate(stg, "Height:")[[2]] + 5))
    
  ) %>%
    
    mutate(rider = riders_to_scrape$rider[[r]],
           rider_url = riders_to_scrape$rider_url[[r]])
  
  Sys.sleep(runif(1, 10, 30))
  
  }
  
  print(r)
  
}

#
#
#

rider_attributes <- bind_rows(rider_data_list) %>%
  
  mutate(weight = as.numeric(str_trim(str_replace(weight, "kg", ""))),
         height = as.numeric(str_trim(str_replace(height, " m", "")))) %>%
  
  separate(dob, c("day", "month", "year"), sep = " ") %>%
  
  inner_join(tibble(month = c("January","February","March","April","May","June",
                              "July","August","September","October","November","December"),
                    m = c("01","02","03","04","05","06","07","08","09","10","11","12")), by = c("month")) %>%
  mutate(day = ifelse(nchar(day)==3, paste0("0",str_sub(day,1,1)), str_sub(day,1,2))) %>%
  mutate(date = as.Date(paste0(year, "-", m, "-", day))) %>%
  
  select(-month, -day, -year, -m)
  


#

rider_attributes$rider <- str_to_title(tolower(rider_attributes$rider))
rider_attributes$rider <- iconv(rider_attributes$rider, from="UTF-8", to = "ASCII//TRANSLIT")
rider_attributes$rider <- tolower(rider_attributes$rider)

#

library(DBI)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

dbWriteTable(con, "rider_attributes", rider_attributes, append = TRUE, row.names = FALSE)