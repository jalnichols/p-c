
library(tidyverse)
library(rvest)
library(DBI)

con <- DBI::dbConnect(
  RMariaDB::MariaDB(), 
  user = Sys.getenv("MYSQL_U"), 
  password = Sys.getenv("MYSQL_P"), 
  db = "cycling", 
  host = Sys.getenv("MYSQL_H"))

#
#
#
#
# Identify Section --------------------------------------------------------

#
#
#
#
# Scrape all UCI World Tour races for 2011-20

scraper_list <- tibble(year = c(2011, 2011, 2012, 2012, 2013, 2013, 2014, 2014, 2015, 2015, 2016, 2016, 2017, 2017, 2018, 2018, 2019, 2019, 2020, 2020),
                       page_no = c(1,2, 1,2, 1,2, 1,2, 1,2, 1,2, 1,2, 1,2, 1,2, 1,2))

result_list <- vector("list", length(scraper_list$year))

#
#
#

for(y in 1:length(scraper_list$year)) {
  
  pg <- paste0('https://www.la-flamme-rouge.eu/maps/races?count=0&page=', scraper_list$page_no[[y]],
               '&calendar%5B0%5D=1&year%5B0%5D=', scraper_list$year[[y]], '&years=&name=') %>%
    read_html()
  
  r <- pg %>%
    html_nodes('tr') %>%
    html_nodes('td') %>%
    html_nodes('a') %>%
    html_attr(name = "href") %>%
    enframe(name = NULL) %>% 
    filter(str_detect(value, "maps/races/view/")) %>% 
    separate(value, c("url", "trash"), sep = "=") %>%
    select(-trash) %>%
    mutate(url = paste0('https://www.la-flamme-rouge.eu', str_sub(url, 1, nchar(url) - 4)))
  
  df <- pg %>%
    html_nodes('table') %>%
    html_table(header = TRUE) %>%
    .[[1]] %>%
    as_tibble() %>%
    
    # attach races URL from above
    cbind(r) %>%
    janitor::clean_names() %>%
    select(date, race = name, stages, class = clas, url) %>%
    mutate(year = scraper_list$year[[y]])
  
  result_list[[y]] <- df

  print(paste0("page:", scraper_list$page_no[[y]], " year:", scraper_list$year[[y]]))
    
}

#
# this generates a list of all World Tour races since 2011 and links to those in my DB already
# we can then scrape those that remain
# this filters out 2020 races which haven't happened yet
#

all_races <- bind_rows(result_list)

current_races <- dbGetQuery(con, "SELECT DISTINCT race_url, year FROM fr_stage_urls")

all_races <- all_races %>%
  
  anti_join(current_races, by = c("url" = "race_url", "year")) %>%
  
  filter(year < lubridate::year(lubridate::today()))

#
# NOW non-World Tour races
#

scraper_list <- tibble(year = c(2011, 2012, 2013, 2014, 2015, 2016, 
                                2017, 2017, 2017, 2017, 2017, 
                                2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018,
                                2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 
                                2020, 2020, 2020, 2020, 2020, 2020, #2
                                2017, 2018, 2019, 2020, #5
                                2017, 2018, 2019, 2020, #6
                                2017, 2018, 2019, 2020, #3
                                2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, #8
                                2017, 2018, 2018, 2019, 2020), #4
                       page_no = c(1,1,1,1,1,1,
                                   1,2,3,4,5, 
                                   1,2,3,4,5,6,7,8,9,10, 
                                   1,2,3,4,5,6,7,8,9,10,
                                   1,2,3,4,5,6,
                                   1, 1, 1, 1,
                                   1, 1, 1, 1,
                                   1, 1, 1, 1,
                                   1, 1, 1, 1, 1, 1, 1, 1, 
                                   1, 1, 2, 1, 1),
                       type = c(2,2,2,2,2,2,
                                2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                5,5,5,5,
                                6,6,6,6,
                                3,3,3,3,
                                8,8,8,8,8,8,8,8,
                                4,4,4,4,4))

#

result_list <- vector("list", length(scraper_list$year))

for(y in 1:length(scraper_list$year)) {
  
  pg <- paste0('https://www.la-flamme-rouge.eu/maps/races?count=0&page=', scraper_list$page_no[[y]],
               '&calendar%5B0%5D=', scraper_list$type[[y]], '&year%5B0%5D=', scraper_list$year[[y]], '&years=&name=') %>%
    read_html()
  
  r <- pg %>%
    html_nodes('tr') %>%
    html_nodes('td') %>%
    html_nodes('a') %>%
    html_attr(name = "href") %>%
    enframe(name = NULL) %>% 
    filter(str_detect(value, "maps/races/view/")) %>% 
    separate(value, c("url", "trash"), sep = "=") %>%
    select(-trash) %>%
    mutate(url = paste0('https://www.la-flamme-rouge.eu', str_sub(url, 1, nchar(url) - 4)))
  
  df <- pg %>%
    html_nodes('table') %>%
    html_table(header = TRUE) %>%
    .[[1]] %>%
    as_tibble() %>%
    
    # attach races URL from above
    cbind(r) %>%
    janitor::clean_names() %>%
    select(date, race = name, stages, class = clas, url) %>%
    mutate(year = scraper_list$year[[y]],
           class = as.character(class))
  
  result_list[[y]] <- df
  
  print(paste0('https://www.la-flamme-rouge.eu/maps/races?count=0&page=', scraper_list$page_no[[y]],
               '&calendar%5B0%5D=', scraper_list$type[[y]], '&year%5B0%5D=', scraper_list$year[[y]], '&years=&name='))
  
}

#

all_ME_races <- bind_rows(result_list)

all_ME_races <- all_ME_races %>%
  
  anti_join(current_races, by = c("url" = "race_url", "year")) %>%
  
  filter(!str_detect(race, "UCI Road World Championships")) %>%
  filter(!str_detect(race, "Africa Cup - TTT")) %>%
  filter(year < lubridate::year(lubridate::today()))

#
# combine
#

all_races <- rbind(all_ME_races, all_races)

#
# Now we can scrape for all stages linked to above races
#

stages_list <- vector("list", length(all_races$url))

for(x in 1:length(all_races$url)) {
  
  d <- all_races$url[[x]] %>%
    
    read_html() %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_attr(name = "href") %>% 
    enframe(name = NULL) %>%
    mutate(value = str_replace(value, "sid=", "=")) %>%
    separate(value, c("url", "trash"), sep = "=") %>% 
    mutate(url = paste0('https://www.la-flamme-rouge.eu', str_sub(url, 1, nchar(url) - 1))) %>%
    mutate(url = str_replace(url, "viewtrack", "loadtrack")) %>%
    mutate(race = all_races$race[[x]],
           race_url = all_races$url[[x]], 
           year = all_races$year[[x]])
  
  stages_list[[x]] <- d %>%
    select(-trash)
  
  Sys.sleep(runif(1,0.5,3.5))
  
  print(all_races$url[[x]])
  
}

#
# filter out races w/o data available yet (empty stages)
#

stages_list <- stages_list %>%
  discard(function(x) nrow(x) == 0)

#
# this writes a list of each stage URL
#

dbWriteTable(con, "fr_stage_urls", bind_rows(stages_list), append = TRUE, row.names = FALSE)

#
# join stages + races
#

all_stages <- all_races %>%
  
  inner_join(
    
    bind_rows(stages_list) %>%
      select(race_url) %>%
      unique(), by = c("url" = "race_url"))

#
# Write new stages 
#

dbWriteTable(con,
             
             "fr_stages",
             
             all_stages,
             
             row.names = FALSE,
             
             append = TRUE
             
)