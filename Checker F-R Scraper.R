
library(tidyverse)
library(rvest)
library(DBI)

setwd("~/p-c")

readRenviron("~/.Renviron")

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

current_races <- dbGetQuery(con, "SELECT DISTINCT race_url, year FROM fr_stages_url")

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

if(length(all_races$url)==0) {
  
  print("no races to scrape")
  
} else {

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

#
# start scraping stages
#

all_stages <- dbReadTable(con, "fr_stages") %>%
  unique() %>%
  
  inner_join(all_races %>%
               select(url), by = c("url" = "url"))

#

tictoc::tic()

#

for(y in 1:length(stages_list)) {
  
  if(!stages_list[[y]]$race_url[[1]] %in% all_stages$url) {
    
    print("didn't pull it")
    
  } else {
    
    
    # run through each race in the stages_list (denoted by y)
    # each stage in that race will be denoted by z below
    
    for(z in 1:length(stages_list[[y]]$url)) {
      
      # this pulls out the json data
      
      q = 0
      
      json <- NULL
      
      while(is.null(json) & q < 5) {
        
        q = q + 1
        
        try(
          json <- rjson::fromJSON(readLines(stages_list[[y]]$url[[z]]))
        )
        
      }
      
      # with the climbs / sprints data
      actual_climbs <- json[[3]]$stageclimbs
      
      # with the generic sprints (some of this contains climb data)
      generic_sprints <- json[[3]]$stagegenericsprints
      
      # with the generic sprints (some of this contains climb data)
      cobbles <- json[[3]]$stagecobbles
      
      # and the distance / altitude data
      other_jsons <- json[[2]]
      
      # and lat/long data
      lat_longs <- json[[1]]
      
      # if there are cobbles listed inside the JSON, scrape them
      
      if(length(cobbles) > 0) {
        
        cobbles_list <- vector('list', length(cobbles))
        
        for(c in 1:length(cobbles)) {
          
          difficulty = cobbles[[c]]$difficulty
          
          if(is.null(difficulty)) {
            
            difficulty <- as.numeric(NA)
            
          }
          
          marker = cobbles[[c]]$marker$icon
          
          if(is.null(marker)) {
            
            marker <- as.numeric(NA)
            
          }
          
          cobbles_list[[c]] <- tibble(
            
            sector_name = cobbles[[c]]$name %>% str_replace_all("%20", " "),
            start_distance = cobbles[[c]]$startdistance,
            end_distance = cobbles[[c]]$distance,
            difficulty = as.numeric(difficulty),
            icon = marker,
            source = "COBBLES"
            
          )
          
        }
        
      } else {
        
        cobbles_list <- vector('list', 1)
        
        c = 1
        
        cobbles_list[[c]] <- tibble(
          
          sector_name = "none",
          start_distance = as.numeric(NA),
          end_distance = as.numeric(NA),
          difficulty = as.numeric(NA),
          icon = "no cobbles",
          source = "MISSING"
          
        )
        
      }
      
      # if there are climbs listed inside the JSON, scrape them
      
      if(length(actual_climbs) > 0) {
        
        climbs_list <- vector('list', length(actual_climbs))
        
        for(c in 1:length(actual_climbs)) {
          
          category = actual_climbs[[c]]$category
          
          if(is.null(category)) {
            
            category <- as.numeric(NA)
            
          }
          
          climbs_list[[c]] <- tibble(
            
            climb_name = actual_climbs[[c]]$name %>% str_replace_all("%20", " "),
            start_distance = actual_climbs[[c]]$startdistance,
            end_distance = actual_climbs[[c]]$distance,
            category = as.numeric(category),
            altitude = actual_climbs[[c]]$altitude,
            source = "ACTUAL CLIMBS"
            
          )
          
        }
        
      } else {
        
        climbs_list <- vector('list', 1)
        
        c = 1
        
        climbs_list[[c]] <- tibble(climb_name = "none",
                                   start_distance = as.numeric(NA),
                                   end_distance = as.numeric(NA),
                                   altitude = as.numeric(NA),
                                   category = as.numeric(NA),
                                   source = "MISSING")
        
      }
      
      #combine all climbs for that stage
      
      df <- bind_rows(climbs_list) %>%
        mutate(url = stages_list[[y]]$url[[z]],
               race_url = stages_list[[y]]$race_url[[z]],
               race = stages_list[[y]]$race[[z]],
               year = 0,
               stage = z)
      
      # same process for cobbles
      
      cobs <- bind_rows(cobbles_list) %>%
        mutate(url = stages_list[[y]]$url[[z]],
               race_url = stages_list[[y]]$race_url[[z]],
               race = stages_list[[y]]$race[[z]],
               year = 0,
               stage = z)
      
      # clean up generic sprints
      #
      
      gen_spr_list <- vector('list', length(generic_sprints))
      
      # if generic sprints has data present (anything appearing on the F-R stage profiles) scrape them
      
      if(length(generic_sprints) > 0) {
        
        for(c in 1:length(gen_spr_list)) {
          
          category = generic_sprints[[c]]$difficulty
          
          if(is.null(category)) {
            
          } else {
            
            gen_spr_list[[c]] <- tibble(
              
              climb_name = generic_sprints[[c]]$name %>% str_replace_all("%20", " "),
              start_distance = generic_sprints[[c]]$startdistance,
              end_distance = generic_sprints[[c]]$distance,
              category = as.numeric(category),
              altitude = generic_sprints[[c]]$altitude,
              source = "GENERIC SPRINTS")
            
          }
          
        }
        
        gen_sprs <- bind_rows(gen_spr_list) %>%
          
          filter(!is.na(category)) %>%
          
          mutate(category = as.numeric(NA)) %>%
          
          mutate(url = stages_list[[y]]$url[[z]],
                 race_url = stages_list[[y]]$race_url[[z]],
                 race = stages_list[[y]]$race[[z]],
                 year = 0,
                 stage = z)
        
      } else {
        
        gen_sprs = df
        
      }
      
      # use actual_climbs, if blank use climbs from generic sprints
      
      if(length(actual_climbs) > 0) {
        
        dbWriteTable(con, "fr_climbs_scraped", df, row.names = F, append = TRUE)
        
      } else {
        
        dbWriteTable(con, "fr_climbs_scraped", gen_sprs, row.names = F, append = TRUE)
        
      }
      
      if(length(cobbles)>0) {
        
        dbWriteTable(con, "fr_cobbles", cobs, row.names = F, append = TRUE)
        
      }
      
      # Now I can pull in distance, altitude data
      
      route_list <- vector("list", length(other_jsons))
      
      for(a in 1:length(other_jsons)) {
        
        route_list[[a]] <- tibble(alt = other_jsons[[a]]$altitude,
                                  lat = other_jsons[[a]]$position$A,
                                  long = other_jsons[[a]]$position$k,
                                  dist = other_jsons[[a]]$distance
        )
        
      }
      
      # write to DB table
      
      route_data_to_write <- bind_rows(route_list) %>%
        mutate(url = stages_list[[y]]$url[[z]]
        ) %>%
        
        mutate(dist = as.numeric(dist),
               lat = as.numeric(lat),
               long = as.numeric(long))
      
      #
      #
      
      dbWriteTable(con, "fr_route_data", route_data_to_write, row.names = F, append = TRUE)
      
      #
      #
      
      Sys.sleep(runif(1, 3, 7))
      
    }

    print(y)
    
    Sys.sleep(runif(1, 5, 13))
    
  }
  
}

#

tictoc::toc()

#

print("finished scraping races")

}