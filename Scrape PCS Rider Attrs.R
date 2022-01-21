
library(tidyverse)
library(lubridate)
library(rvest)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

LIMIT = 100
DATES1 = c('2021-12-31', "2020-12-31", "2019-12-31", "2018-12-31")
PAGES = seq(0, 800, LIMIT)

pull_pages <- expand_grid(LIMIT, DATES1, PAGES)

data_list <- vector("list", length(pull_pages$LIMIT))

for(i in 1:length(pull_pages$LIMIT)) {

  DATE1 = pull_pages$DATES1[[i]]
  PAGE = pull_pages$PAGES[[i]]
  
  pg <- paste0('https://www.procyclingstats.com/rankings.php?date=',
                DATE1, 
                '&nation=&age=&zage=&page=smallerorequal&team=&offset=',
                PAGE,
                '&filter=Filter&p=we&s=individual') %>%
    
    read_html() 
  
  d <- pg %>%
    
    html_nodes("tbody") %>%
    html_nodes("tr") %>%
    html_nodes("td") %>%
    html_nodes("a") %>%
    html_attr(name = "href") %>% 
    enframe(name = NULL)
  
  d2 <- pg %>%

    html_nodes("tbody") %>%
    html_nodes("tr") %>%
    html_nodes("td") %>%
    html_nodes("a") %>%
    html_text() %>% 
    enframe(name = NULL)
  
  data_list[[i]] <- cbind(d, d2 %>% rename(rider = value)) %>%
    filter(str_detect(value, "rider/")) %>%
    mutate(rider_url = paste0('https://www.procyclingstats.com/', value),
           rider = str_trim(rider)) %>%
    select(rider_url, rider)
  
}

#

riders_to_scrape <- bind_rows(data_list) %>%
  unique() %>%
  mutate(rider_url = str_replace(rider_url, "https://www.procyclingstats.com", ""))

#
#
#
#
#

rider_data_list <- vector("list", length(riders_to_scrape$rider_url))
sites_list <- vector("list", length(rider_data_list))

for(r in 1:length(riders_to_scrape$rider_url)) {
  
  pg <- paste0("https://www.procyclingstats.com/", riders_to_scrape$rider_url[[r]]) %>%
    
    read_html()
  
  STRAVA_SITE <- pg %>%
    html_nodes('ul.list') %>%
    html_nodes('li') %>%
    html_nodes('a') %>%
    html_attr(name = "href") %>%
    enframe(name = NULL) %>%
    filter(str_detect(value, "strava"))
  
  if(nrow(STRAVA_SITE) == 0) {
    
    sites_list[[r]] <- tibble(value = "no strava")
    
  } else {
    sites_list[[r]] <- STRAVA_SITE
    
  }
    
  rider_data_list[[r]] <- pg %>%
    
    html_nodes('div.rdr-info-cont') %>%
    html_text()
  
  Sys.sleep(runif(1, 1, 5))
  
  print(r)
  
}

#

rider_attr_list <- vector("list", length(rider_data_list))

strava_rider_list <- vector("list", length(rider_attr_list))

for(x in 1:length(rider_data_list)) {
  
  stg <- rider_data_list[[x]]
  
  if(str_detect(stg, "Passed away on:")) {
    
    rider_attr_list[[x]] <- tibble(
      
      dob = str_trim(str_sub(stg, str_locate(stg, "Date of birth:")[[1]] + 14, str_locate(stg, "Passed away on:")[[1]] - 1)),
      weight = str_trim(str_sub(stg, str_locate(stg, "Weight:")[[2]] + 1, str_locate(stg, "Height")[[1]] - 1)),
      height = str_trim(str_sub(stg, str_locate(stg, "Height:")[[2]] + 1, str_locate(stg, "Height:")[[2]] + 5))
      
    )
    
  } else {
  
  rider_attr_list[[x]] <- tibble(
    
    dob = str_trim(str_sub(stg, str_locate(stg, "Date of birth:")[[1]] + 14, str_locate(stg, "Nationality")[[1]] - 5)),
    weight = str_trim(str_sub(stg, str_locate(stg, "Weight:")[[2]] + 1, str_locate(stg, "Height")[[1]] - 1)),
    height = str_trim(str_sub(stg, str_locate(stg, "Height:")[[2]] + 1, str_locate(stg, "Height:")[[2]] + 5))

  )
  
  }
  
  print(nchar(stg))
    
}

#

rider_attributes <- bind_rows(rider_attr_list) %>%
  
  cbind(riders_to_scrape) %>%
  
  mutate(weight = as.numeric(str_trim(str_replace(weight, "kg", ""))),
         height = as.numeric(str_trim(str_replace(height, " m", "")))) %>%
  mutate(date = lubridate::dmy(dob)) %>%
  select(-dob)

strava_riders_we <- bind_rows(sites_list) %>%
  filter(!value == 'https://www.strava.com/athletes/santestebana') %>%
  
  cbind(riders_to_scrape) %>%
  
  filter(!value == "no strava")

#

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

dbWriteTable(con, "rider_attributes_we", rider_attributes, append = TRUE, row.names = FALSE)