
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

urls <- c()

#

scraped_list19 <- vector("list",30)

#xs = seq(0,2500,100)

for(x in 1:30) {
  
  #url <- paste0('https://www.procyclingstats.com/rankings.php?nation=&age=&zage=&page=smallerorequal&team=&offset=',
  #(x-1)*200,
  #'&teamlevel=&filter=Filter')
  
  url = paste0("https://www.procyclingstats.com/rankings.php?date=",
               "2020-12-31",
               "&nation=&age=&zage=&page=smallerorequal&team=&offset=",
               (x-1)*100, "&filter=Filter&p=me&s=season-individual")
  
  # url <- paste0('https://www.procyclingstats.com/rankings.php?date=2021-06-14&nation=&age=&zage=&page=smallerorequal&team=&offset=',
  #               (x-1)*100, '&filter=Filter')
  # 
  #url <- paste0('https://www.procyclingstats.com/rankings.php?nation=&age=26&zage=&page=largerorequal&team=&offset=',
  #              xs[[x]], '&teamlevel=&filter=Filter')
  
  page <- url %>%
    read_html()
  
  rider_links <- page %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_attr(name = 'href') %>%
    .[seq(1,300,3)] %>%
    enframe(name = NULL) %>%
    rename(url = value)
  
  rider_names <- page %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_text() %>%
    .[seq(1,300,3)]  %>%
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
  mutate(rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
  mutate(rider = str_to_title(str_trim(rider)))
  

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
  
  anti_join(dbReadTable(con, "rider_attributes") %>%
              filter(!is.na(weight)), by = c("rider_url"))

#

riders_to_scrape <- riders_2023 %>%
  filter(is.na(age)) %>%
  
  anti_join(dbReadTable(con, "rider_attributes") %>%
              filter(!is.na(date)), by = c("rider_url"))
  
#
#
#
#
#

riders_to_scrape <- dbGetQuery(con, "SELECT DISTINCT rider_url FROM rider_attributes") %>%
  filter(!is.na(rider_url))

riders_to_scrape <- dbGetQuery(con, "SELECT DISTINCT rider_url FROM pcs_season_team WHERE season = 2023") %>%
  filter(!is.na(rider_url)) %>%
  anti_join(dbGetQuery(con, "SELECT DISTINCT rider_url FROM rider_attributes_new"))

missing_list <- vector("list", 6000)

for(r in 1:length(riders_to_scrape$rider_url)) {
  
  if(r < 1) {
  } else {
  
  pg <- paste0("https://www.procyclingstats.com/", riders_to_scrape$rider_url[[r]]) %>%
    
    read_html()
  
  stg <- pg %>%
    
    html_nodes('div.rdr-info-cont') %>%
    html_text()
  
  if(length(stg) == 0) {
    missing_list[[r]] = riders_to_scrape$rider_url[[r]]
  } else {
    
    rid_dat <- tibble(
      dob = str_trim(str_sub(stg, str_locate(stg, "Date of birth:")[[1]] + 14, str_locate(stg, "Nationality")[[1]] - 5)),
      weight = str_trim(str_sub(stg, str_locate(stg, "Weight:")[[2]] + 1, str_locate(stg, "Height")[[1]] - 1)),
      height = str_trim(str_sub(stg, str_locate(stg, "Height:")[[2]] + 1, str_locate(stg, "Height:")[[2]] + 5))
    ) %>%
      filter(dob != "" | !is.na(weight) | !is.na(height))
    
    if(nrow(rid_dat) == 0) {
      missing_list[[r]] = riders_to_scrape$rider_url[[r]]
    } else {
      
      rid_dat <- rid_dat %>%
        
        mutate(rider = pg %>% html_nodes(xpath = "/html/body/div[1]/div[1]/div[2]/div[1]/h1") %>% html_text(),
               rider_url = riders_to_scrape$rider_url[[r]],
               updated = lubridate::today()) %>%
        mutate(weight = as.numeric(str_trim(str_replace(weight, "kg", ""))),
               height = as.numeric(str_trim(str_replace(height, " m", "")))) %>%
        separate(dob, c("day", "month", "year"), sep = " ") %>%
        mutate(year = str_replace(year, "Passed", "")) %>%
        inner_join(tibble(month = c("January","February","March","April","May","June",
                                    "July","August","September","October","November","December"),
                          m = c("01","02","03","04","05","06","07","08","09","10","11","12")), by = c("month")) %>%
        mutate(day = ifelse(nchar(day)==3, paste0("0",str_sub(day,1,1)), str_sub(day,1,2))) %>%
        mutate(date = as.Date(paste0(year, "-", m, "-", day))) %>%
        select(-month, -day, -year, -m) %>%
        
        separate(rider, c("first", "last"), sep = "  ") %>%
        mutate(rider = paste0(last, " ", first),
               rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT"),
               last = iconv(last, from="UTF-8", to = "ASCII//TRANSLIT"),
               first = iconv(first, from="UTF-8", to = "ASCII//TRANSLIT"))
      
      dbWriteTable(con, "rider_attributes_new", rid_dat, append = TRUE, row.names = FALSE)
      
      
    }
  }
  
  Sys.sleep(runif(1, 2, 6))
  
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

#
#
#
#
#
#
#

# use stage_level_power

stage_level_power %>%
  
  filter(is.na(weight)) %>%
  select(rider = pcs) %>%
  group_by(rider) %>%
  count() %>%
  ungroup() %>%
  
  arrange(desc(n)) -> RIDERS

#

rider_data_list <- vector("list", length(RIDERS$rider))

#

for(r in 1:length(RIDERS$rider)) {
  
  # identify page from search results
  page <- paste0("https://www.procyclingstats.com/search.php?term=", str_replace_all(RIDERS$rider[[r]], " ", "+")) %>%
    
    read_html()
  
  first_result <- page %>%
    html_nodes('div.mt30') %>%
    html_nodes('a') %>%
    html_attr(name = "href")
  
  if(length(first_result) > 0) {
    
    first_result <- first_result[[1]]
    
    rider_page <- paste0("https://www.procyclingstats.com/", first_result) %>%
      read_html()
    
    # scrape rider info  
    stg <- rider_page %>%
      
      html_nodes('div.rdr-info-cont') %>%
      html_text()
    
    rid_dat <- tibble(
      
      dob = str_trim(str_sub(stg, str_locate(stg, "Date of birth:")[[1]] + 14, str_locate(stg, "Nationality")[[1]] - 5)),
      weight = str_trim(str_sub(stg, str_locate(stg, "Weight:")[[2]] + 1, str_locate(stg, "Height")[[1]] - 1)),
      height = str_trim(str_sub(stg, str_locate(stg, "Height:")[[2]] + 1, str_locate(stg, "Height:")[[2]] + 5))
      
    ) %>%
      
      mutate(rider = RIDERS$rider[[r]],
             rider_url = first_result)
    
    rider_data_list[[r]] <- rid_dat
    
    if(!is.na(rid_dat$weight)) {
      
      dbSendQuery(con, paste0("DELETE FROM rider_attributes WHERE rider = '", RIDERS$rider[[r]], "'"))
      
    }
    
  } else {}
  
  Sys.sleep(runif(1, 3, 12))
  
  print(r)
  
}

#
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


dbWriteTable(con, "rider_attributes", rider_attributes, append = TRUE, row.names = FALSE)

#

rider_attributes %>%
  separate(rider_url, c("first", "last"), sep = "-", remove = FALSE) %>%
  mutate(first = str_replace(first, 'rider/', '')) %>%
  mutate(yes_first = str_detect(rider, first), 
         yes_last = str_detect(rider, last)) %>%
  filter(yes_first == TRUE & yes_last == TRUE) -> rider_attributes1

rider_attributes %>% 
  separate(rider_url, c("first", "last"), sep = "-", remove = FALSE) %>%
  mutate(first = str_replace(first, 'rider/', '')) %>% 
  mutate(yes_first = str_detect(rider, first),
         yes_last = str_detect(rider, last)) %>%
  filter((yes_first == TRUE & yes_last == FALSE) | (yes_last == TRUE & yes_first == FALSE)) -> rider_attributes2

rider_attributes3 <- rbind(rider_attributes1, rider_attributes2[c(), ]) %>% select(weight, height, rider, rider_url, date)

dbWriteTable(con, "rider_attributes", rider_attributes3, append = TRUE, row.names = FALSE)


#
#
#
#
#
#
#

valid_weight_now <- dbGetQuery(con, 

'SELECT DISTINCT stat_value AS weight, PCS

FROM strava_activity_data sa

JOIN (
  
  SELECT DISTINCT stat_value, activity_id
  
  FROM strava_activity_power
  
  WHERE stat_name = "athlete_weight" AND stat_value <> "0" AND stat_value < 100
) AS wt ON sa.activity_id = wt.activity_id

WHERE Stat IN ("Distance", "Elevation")') %>%
  
  group_by(PCS) %>%
  summarize(weight = median(weight, na.rm = T)) %>%
  ungroup()

#

RA <- dbReadTable(con, "rider_attributes")

#
#
#

for(r in 1:length(valid_weight_now$PCS)) {

  if(str_to_lower(valid_weight_now$PCS[[r]]) %in% str_to_lower(RA$rider)) {
    
    dbSendQuery(con,
                
                paste0("UPDATE rider_attributes
            SET weight = ", valid_weight_now$weight[[r]],
                       " WHERE rider = '", str_to_lower(valid_weight_now$PCS[[r]]), "'"))
    
    print("updated")
    
  } else {
    
    dbWriteTable(con,
                 
                 "rider_attributes",
                 
                 valid_weight_now[r,] %>%
                   mutate(height = NA,
                          date = NA,
                          rider_url = NA,
                          PCS = str_to_lower(PCS)) %>%
                   rename(rider = PCS),
                 
                 append = TRUE,
                 row.names = FALSE
            
                 )
    
    print("new record")
    
  }
  

  
}


#
#
#
#

riders_to_scrape <- read_csv("MW.csv")

safe_weight = function(url) {
  
  remDr$navigate(url)
  
  Sys.sleep(9)
  
  JSON_xpath <- '/html/body/pre'
  
  json_data <- remDr$findElement(using = 'xpath', JSON_xpath)
  
  data_lists <- json_data$getElementText() %>%
    .[[1]] %>%
    jsonlite::fromJSON()
  
}

#

for(x in 12:nrow(riders_to_scrape)) {
  
  url <- paste0("https://www.strava.com/activities/", riders_to_scrape$activity_id[[x]], "/power_data")
  
  safely_weight <- safely(.f = safe_weight, otherwise = NULL)
  
  data_lists <- safely_weight(url)
  
  if(is.null(data_lists$result)) {
    print("failed")
  } else {
    
    df <- tibble(activity_id = riders_to_scrape$activity_id[[x]],
                 rider = riders_to_scrape$PCS[[x]],
                 athlete_ftp = data_lists$result$athlete_ftp,
                 weight = data_lists$result$athlete_weight)
    
    dbWriteTable(con, "weight_from_strava", df, append = TRUE, row.names = FALSE)
    
    print(df) 
  
  }
  
}
