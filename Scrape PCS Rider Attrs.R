


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

LIMIT = 200
DATES1 = c(31759, 12408, 1951, 34621, 1951)
DATES2 = c(43578, 31759, 12408, 1951, 34621)
PAGES = seq(0, 1000, 200)
ITERS = length(DATES1) * length(PAGES)

data_list <- vector("list", length(ITERS))

for(i in 1:ITERS) {

  DATE1 = DATES1[[floor((i-1) / 6)+1]]
  DATE2 = DATES2[[floor((i-1) / 6)+1]]
  PAGE = PAGES[[(i %% 6)+1]]
  
  pg <- paste0('https://www.procyclingstats.com/rankings.php?id=',
                DATE1, 
                '&id=', 
                DATE2, 
                '&nation=&team=&page=', 
                PAGE, 
                '&prev_rnk_days=1&younger=&older=&limit=', 
                LIMIT, 
                '&filter=Filter&morefilters=0') %>%
    
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
  unique()

#
#
#
#
#

rider_data_list <- vector("list", length(riders_to_scrape$rider_url))

for(r in 1:length(riders_to_scrape$rider_url)) {
  
  pg <- riders_to_scrape$rider_url[[r]] %>%
    
    read_html()
  
  rider_data_list[[r]] <- pg %>%
    
    html_nodes('div.rdr-info') %>%
    html_text()
  
  Sys.sleep(runif(1, 1, 5))
  
  print(r)
  
}

#

rider_attr_list <- vector("list", length(rider_data_list))

for(x in 1:length(rider_data_list)) {
  
  stg <- rider_data_list[[x]]
  
  rider_attr_list[[x]] <- tibble(
    
    dob = str_trim(str_sub(stg, str_locate(stg, "Date of birth:")[[1]] + 14, str_locate(stg, "Nationality")[[1]] - 5)),
    weight = str_trim(str_sub(stg, str_locate(stg, "Weight:")[[2]] + 1, str_locate(stg, "Height")[[1]] - 1)),
    height = str_trim(str_sub(stg, str_locate(stg, "Height:")[[2]] + 1, str_locate(stg, "Height:")[[2]] + 5))

  )
  
  print(nchar(stg))
    
}

#

rider_attributes <- bind_rows(rider_attr_list) %>%
  
  cbind(rider = riders_to_scrape %>%
          select(rider)) %>%
  
  mutate(weight = as.numeric(str_trim(str_replace(weight, "kg", ""))),
         height = as.numeric(str_trim(str_replace(height, " m", ""))))

#

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

dbWriteTable(con, "rider_attributes", rider_attributes)