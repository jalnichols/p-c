library(tidyverse)
library(rvest)
library(DBI)

dbDisconnect(con)

con <- dbConnect(RMySQL::MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

years_cx <- c(2022, 2021, 2020, 2019, 2018, 2017, 2016)

for(y in years_cx) {
  
  page <- paste0("https://firstcycling.com/cx.php?y=", y) %>%
    read_html()
  
  races <- page %>%
    html_nodes('table') %>%
    .[[2]] %>%
    html_table(fill=TRUE, convert = FALSE) %>%
    .[,1:4]
  
  races_url <- page %>%
    html_nodes('table') %>%
    .[[2]] %>%
    html_nodes('a') %>%
    html_attr(name = "href") %>%
    enframe(name = NULL) %>%
    filter(str_detect(value, "race"))

  races_df <- cbind(races %>% select(Date, CAT, Race), races_url) %>%
    mutate(Season = y,
           Date = as.character(Date)) %>%
    separate(Date, c("Day", "Month"), sep = "\\.") %>%
    mutate(Year = ifelse(Month %in% c("08", "09", "10","11","12"), Season-1, Season)) %>%
    mutate(Date = as.Date(paste0(Year,"-",Month,"-",Day))) %>%
    select(-Month, -Day) %>%
    rename(url = value)
  
  dbWriteTable(con, "firstcycling_cyclocross_races", races_df, append = TRUE, row.names = FALSE)
  
}

#
#
#
#

all_cx_races <- dbReadTable(con, "firstcycling_cyclocross_races") %>%
  anti_join(dbGetQuery(con, "SELECT DISTINCT race_url as url FROM firstcycling_cyclocross_results"))

#
#
#

for(r in 899:length(all_cx_races$url)) {
  
  page <- paste0("https://firstcycling.com/", all_cx_races$url[[r]]) %>%
    read_html()

  rider_res <- page %>%
    html_nodes('table') %>%
    html_table(convert = FALSE) %>%
    .[[3]]
  
  rider_url <- page %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_attr(name = "href") %>%
    enframe(name = NULL) %>%
    filter(str_detect(value, "rider"))
  
  rider_df <- cbind(rider_res, rider_url = rider_url) %>%
    mutate(race_url = all_cx_races$url[[r]])
  
  dbWriteTable(con, "firstcycling_cyclocross_results", rider_df, append = TRUE, row.names = FALSE)
  
  print(r)
  Sys.sleep(3)
  
}

#
#
#

all_cx_results <- dbReadTable(con, "firstcycling_cyclocross_results") %>%
  
  mutate(Pos = as.numeric(Pos),
         Secs = ifelse(Pos == 1, 0, str_replace(Time, "\\+ ", "")),
         Secs = ifelse(str_detect(Secs, ":"), as.numeric(lubridate::ms(Secs)), as.numeric(Secs)),
         Secs = ifelse(Time == "", NA, Secs),
         Lapped = ifelse(Time == "", 1, 0))

         