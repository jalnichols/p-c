
library(tidyverse)
library(rvest)
library(RMySQL)

# this script identifies the F-R stage we want to extract (Identify Section)... it writes those stages in fr_stages
# scrapes those stages/races (Scraping Section)... inside that it writes the profile information automatically to fr_altitude
# following that it writes the raw stage climb data to fr_scraped_climb_stages
# the next section (Cleaning Section) cleans up both the climb and altitude data


# Can pull in all climbs in Lat/Long box using the following URLs

# https://www.la-flamme-rouge.eu/maps/tracks/maps/load/2/bounds/44/3/41/-3

# Eg, for this 18 squares box lat/long in the Pyrenees there are 2300 climbs
# can cover Western Europe with about 450 square boxes


# Identify Section --------------------------------------------------------



# DB

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

# Scrape all UCI World Tour races for 2013-20

scraper_list <- tibble(year = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2017, 2018, 2018, 2019, 2019, 2020, 2020),
                       page_no = c(1,1,1,1,1,1,1,2,1,2,1,2,1,2))

result_list <- vector("list", length(scraper_list$year))

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
  
  img <- pg %>%
    html_nodes('tr') %>%
    html_nodes('td') %>%
    html_nodes('a') %>%
    html_nodes('img') %>%
    html_attr(name = "alt") %>%
    enframe(name = NULL) %>% 
    filter(!is.na(value)) %>%
    rename(tour = value)
  
  df <- pg %>%
    html_nodes('table') %>%
    html_table(header = TRUE) %>%
    .[[1]] %>%
    as_tibble() %>%
    
    # attach races URL from above
    cbind(r, img) %>%
    janitor::clean_names() %>%
    select(date, race = name, stages, class = clas, url, tour) %>%
    mutate(year = scraper_list$year[[y]])
  
  result_list[[y]] <- df
  
}

all_races <- bind_rows(result_list)

# Now scraper Europe/Americas/WC/Asia Tour for 2017-19

# 2 Europe Tour
# 3 Americas Tour
# 4 Asia Tour
# 5 Africa Tour
# 6 Oceania Tour
# 8 World Champs
# 7 Olympics
# 1 UWT

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
  
  img <- pg %>%
    html_nodes('tr') %>%
    html_nodes('td') %>%
    html_nodes('a') %>%
    html_nodes('img') %>%
    html_attr(name = "alt") %>%
    enframe(name = NULL) %>% 
    filter(!is.na(value)) %>%
    rename(tour = value)
  
  df <- pg %>%
    html_nodes('table') %>%
    html_table(header = TRUE) %>%
    .[[1]] %>%
    as_tibble() %>%
    
    # attach races URL from above
    cbind(r, img) %>%
    janitor::clean_names() %>%
    select(date, race = name, stages, class = clas, url, tour) %>%
    mutate(year = scraper_list$year[[y]],
           class = as.character(class))
  
  result_list[[y]] <- df
  
}

#
#
#

# HERE I NEED TO BUILD A SECTION WHICH SCRAPES BASED ON IDs to pull in Avenir, Natl Championships, Olympics

#
#
#

intermediate_races <- all_races %>%
  rbind(
    
    bind_rows(result_list))

#

all_races <- intermediate_races %>%
  
  mutate(id = str_replace(url, 'https://www.la-flamme-rouge.eu/maps/races/view/', '')) %>%
  
  separate(id, c("trash", "id"), sep = "/") %>%
  
  select(-trash)

#

dbWriteTable(con, "fr_races", all_races, append = TRUE, row.names = FALSE)

# find data we already have

already_scraped <- dbGetQuery(con, "SELECT * FROM fr_stage_urls") %>%
  
  select(race_url) %>%
  unique()

# only scrape ones we don't have OR skip this if we want to re-scrape

 all_races <- all_races %>%
   
   filter((!(url %in% already_scraped$race_url)))

#
#
#
#
#
# Now we can pull in all URLs from all_races
#
#
#
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
  
  print(x)
  
}

# filter out races w/o data available yet (empty stages)

stages_list <- stages_list %>%
  discard(function(x) nrow(x) == 0)

# this writes a list of each stage URL

dbWriteTable(con, "fr_stage_urls", bind_rows(stages_list), append = TRUE, row.names = FALSE)

stages_list_all <- c(read_rds("stages-list-f-r.rds"), stages_list)

write_rds(stages_list_all, "stages-list-f-r.rds")

# and for the data frame

all_stages <- all_races %>%
  
  inner_join(
    
    bind_rows(stages_list) %>%
      select(race_url) %>%
      unique(), by = c("url" = "race_url"))

#

dbWriteTable(con,
             "fr_stages",
             all_stages,
             row.names = FALSE,
             append = TRUE
)


# bring in 

all_stages <- dbReadTable(con, "fr_stages") %>%
  unique() %>%
  
  inner_join(all_races %>%
               select(url), by = c("url" = "url"))

stages_list <- read_rds( "stages-list-f-r.rds")

#
#
#
#
#

# Scraping Section --------------------------------------------------------


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
             year = stages_list[[y]]$year[[z]],
             
             # this data is wrong when all_stages != stages_list in terms of length
             #year = str_sub(all_stages$date[[y]], nchar(all_stages$date[[y]]) - 3, nchar(all_stages$date[[y]])),
             year = 0,
             stage = z,
             updated = lubridate::now())
    
    # same process for cobbles
    
    cobs <- bind_rows(cobbles_list) %>%
      mutate(url = stages_list[[y]]$url[[z]],
             race_url = stages_list[[y]]$race_url[[z]],
             race = stages_list[[y]]$race[[z]],
             year = stages_list[[y]]$year[[z]],
             
             # this data is wrong when all_stages != stages_list in terms of length
             #year = str_sub(all_stages$date[[y]], nchar(all_stages$date[[y]]) - 3, nchar(all_stages$date[[y]])),
             year = 0,
             stage = z,
             updated = lubridate::now())
    
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
               #year = stages_list[[y]]$year[[z]],
               #year = str_sub(all_stages$date[[y]], nchar(all_stages$date[[y]]) - 3, nchar(all_stages$date[[y]])),
               year = 0,
               stage = z,
               updated = lubridate::now())
      
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
      mutate(url = stages_list[[y]]$url[[z]],
             #race_url = stages_list[[y]]$race_url[[z]],
             #race = stages_list[[y]]$race[[z]],
             #year = stages_list[[y]]$year[[z]],
             #year = str_sub(all_stages$date[[y]], nchar(all_stages$date[[y]]) - 3, nchar(all_stages$date[[y]])),
             #year = 0,
             #stage = z
             ) %>%
      
      mutate(dist = as.numeric(dist),
             lat = as.numeric(lat),
             long = as.numeric(long),
             updated = lubridate::now())
    
    #
    #
    
    dbWriteTable(con, "fr_route_data", route_data_to_write, row.names = F, append = TRUE)
    
    #
    #
    
    Sys.sleep(runif(1, 1, 5))
    
  }
  
  #climbs_stages_list[[y]] <- bind_rows(data_list)
  
  print(y)
  print(stages_list[[y]]$race)
  print(stages_list[[y]]$year)
  
  Sys.sleep(runif(1, 1, 5))
  
  }
  
}

#
#
#
#
#


# transformations below sourced from https://www.w3schools.com/tags/ref_urlencode.asp

clean_climbs <- dbReadTable(con, "fr_climbs_scraped") %>%
  filter(updated >= '2020-07-31') %>%
  mutate(year = str_sub(race_url, 48, 51)) %>%
  
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%2520', ' '))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%F4', 'o'))) %>%   
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25F4', 'o'))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%2527', "'"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25E9', 'e'))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%E8', 'e'))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25E8', 'e'))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%27', "'"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25EF', 'i'))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%E9', "e"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%F3', 'o'))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%EF', "i"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%E7', 'c'))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%E2', "a"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25E2', "a"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%F2', "o"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%FB', "u"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25E0', "a"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%F9', "u"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%C9', "E"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%FC', "u"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%F1', "n"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25F6', "o"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%EC', "i"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%ED', "i"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25F1', "n"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%60', "`"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%D3', "O"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%ED', "i"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%E0', "a"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%F3', "o"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25EC', "i"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25F3', "o"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%F6', "o"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25ED', "i"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25EA', "e"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%EE', "i"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%EA', "e"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25FC', "u"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%FC', "u"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%DF', "ss"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25C1', "A"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%C1', "A"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%FA', "u"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%C7', "C"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%u0131', "i"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25u2019', "'"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%u2019', "'"))) %>%
  
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%2C', ","))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25F2', "o"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25E1', "a"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25E5', "a"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25E7', "c"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%E3', "a"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%EB', "e"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%B0', ""))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%D6', "O"))) %>%
  
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25D8', "O"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25F8', "o"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25C5', "A"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25C6', "Ae"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%2525', ""))) %>%
  
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25u0160', "S"))) %>%  
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25u0150', "O"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25u010D', "c"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, 'u0160', "S"))) %>%  
  mutate(climb_name = str_trim(str_replace_all(climb_name, 'u0150', "O"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, 'u010D', "c"))) %>%
  
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25u010C', "C"))) %>%  
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25u0107', "c"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25u0153', "oe"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%u010C', "C"))) %>%  
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%u0107', "c"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%u0153', "oe"))) %>%
  
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%28', ""))) %>%  
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%29', ""))) %>% 
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%D8', "O"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%C8', "E"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%E4', "a"))) %>%  
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%AA', "a"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%22', ''))) %>%
  
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%E1', "a"))) %>%  
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%u0103', "a"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%u0219', "s"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%u0142', "l"))) %>%  
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%u0119', "e"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%u0144', "n"))) %>%
  
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%28 Souvenir Jacques Goddet%29', ''))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%28Souvenir Henri Desgrange%29', ''))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%28Souvenir Jacques Goddet%29', ''))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%28 Souvenir Henri Desgrange%29', ''))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%2528 Souvenir Jacques Goddet%2529', ''))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%2528Souvenir Henri Desgrange%2529', ''))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%2528Souvenir Jacques Goddet%2529', ''))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%2528 Souvenir Henri Desgrange%2529', ''))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, 'Bardonecchia %28Jafferau%29', 'Jafferau'))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, 'Pas de Peyrol %2528Puy Mary%2529', 'Puy Mary'))) %>%
  mutate(climb_name = ifelse(str_detect(climb_name, 'PORT DE CANTO'), "Port de Canto", climb_name)) %>%
  mutate(climb_name = ifelse(str_detect(climb_name, 'Alto de Puig'), "Alto de Puig", climb_name)) %>%
  mutate(climb_name = ifelse(str_detect(climb_name, 'Passo dello Stelvio'), "Passo dello Stelvio", climb_name)) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%2528.*%2529', ''))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%28.*%29', ''))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%2509', ''))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, "' ", "'"))) %>%
  mutate(climb_name = str_trim(str_replace_all(climb_name, '%25', ''))) %>%
  
  unique() %>%
  
  mutate(year = as.numeric(year))

# build matcher for PCS to FR race names

pcs <- dbGetQuery(con, "SELECT year, race, date, class, max(stage) as stages 
                  FROM pcs_stage_raw GROUP BY race, year, date, class") %>%
  
  filter(!is.na(class)) %>%
  
  mutate(date = as.Date(date),
         class = ifelse(class == "UWT",
                        ifelse(stages > 1, "2.UWT", "1.UWT"), class),
         class = ifelse(class == "1.2U23", "1.2U", class)) %>%
  
  group_by(race, year) %>%
  mutate(date = min(date, na.rm = T)) %>%
  ungroup() %>%
  
  unique() %>%
  
  filter(!race %in% c("World Championships WE - Road Race", "World Championships WJ - Road Race",
                      "World Championships U23 - Road Race", "World Championships MJ - ITT",
                      "World Championships WE - ITT", "World Championships WJ - ITT",
                      "World Championships MJ - Road Race", "World Championships U23 - ITT",
                      "Tour of Almaty", "Tour de Taiwan", "Tour of Japan", "Tour de Korea",
                      "Tour of Antalya", "Tour of China I", "Tour of China II", "Tour of Indonesia",
                      "Tour of Fuzhou", "Tour of Iran (Azarbaijan)", "Tour of Peninsular",
                      "Tour of Taihu Lake", "UEC Road European Championships - ITT")) %>%
  filter(year > 2012) %>%
  filter(date <= '2020-08-01') %>%
  
  filter(!class == 'NC')

#

fr <- dbGetQuery(con, "SELECT race, year, date FROM fr_stages GROUP BY race, year, date") %>%
  
  inner_join(dbReadTable(con, "fr_races")) %>%
  
  select(date, race, year, class, tour, stages) %>%
  
  mutate(date = str_replace_all(date, "Monday", ""),
         date = str_replace_all(date, "Tuesday", ""),
         date = str_replace_all(date, "Wednesday", ""),
         date = str_replace_all(date, "Thursday", ""),
         date = str_replace_all(date, "Friday", ""),
         date = str_replace_all(date, "Saturday", ""),
         date = str_replace_all(date, "Sunday", ""),
         date = str_trim(date)) %>%
  separate(date, c("day", "month", "year2"), sep = " ") %>%
  inner_join(tibble(month = c("January","February","March","April","May","June",
                              "July","August","September","October","November","December"),
                    m = c("01","02","03","04","05","06","07","08","09","10","11","12")), by = c("month")) %>%
  mutate(date = as.Date(paste0(year2,"-",m,"-",day))) %>%
  select(race, year, date, class, tour) %>%
  filter(date <= '2020-08-01') %>%
  
  mutate(date = ifelse(year == 2016 & race == "Santos Tour Down Under", as.Date('2016-01-19'), date)) %>%
  
  filter(!race %in% c("UCI Road World Championships - ITT (Men Elite)")) %>%
  
  mutate(class = ifelse(class == "CM", "WC", class))

#

matches <- fr %>%
  
  # ignore races where I know I lack matches
  #filter(!str_detect(race, "Giro Ciclistico")) %>%
  #filter(!str_detect(race, "Avenir")) %>%
  
  mutate(year = as.numeric(year)) %>%
  rename(fr_race = race) %>%
  
  inner_join(
    
    pcs %>%
      rename(pcs_race = race), by = c("year", "class")
    
  ) %>%
  
  mutate(sd_DL = stringdist::stringdist(tolower(pcs_race), tolower(fr_race), method = "dl") / nchar(pcs_race),
         sd_QG = stringdist::stringdist(tolower(pcs_race), tolower(fr_race), method = "qgram", q = 2) / nchar(pcs_race)) %>%
  
  group_by(fr_race, year) %>%
  mutate(rankDL = rank(sd_DL, ties.method = "min"),
         rankQG = rank(sd_QG, ties.method = "min")) %>%
  ungroup() %>%
  
  mutate(hm = (sd_DL + sd_QG) / 2) %>%
  
  group_by(fr_race, year) %>%
  mutate(rankHM = rank(hm, ties.method = "min")) %>%
  ungroup() %>%
  
  group_by(fr_race) %>% 
  mutate(no1 = sum(rankHM == 1, na.rm = T)) %>%
  ungroup()

#

pcs_fr_matches <- matches %>%
  
  filter(rankHM == 1 |
           ((tolower(fr_race) == "euroeyes cyclassics") & (tolower(pcs_race) == "cyclassics hamburg"))) %>%
  
  rbind(
    
    matches %>%
      mutate(hm = 0.75) %>%
      inner_join(
        read_csv('fr_pcs_matches.csv') %>%
          filter(match == TRUE) %>%
          select(-match), by = c("fr_race" = "fr", "pcs_race" = "pcs", "year")
        
      )) %>%
  
  filter(hm < 0.751) %>%
  
  anti_join(
    
    read_csv('fr_pcs_matches.csv') %>%
      filter(match == FALSE) %>%
      select(-match), by = c("fr_race" = "fr", "pcs_race" = "pcs", "year")
    
  ) %>%
  
  filter(year > 2012) %>%
  
  filter(date.x < (date.y + 5) & date.x > (date.y - 5)) %>%
  
  unique() %>%
  
  select(fr_race, pcs_race, year) %>%
  
  rbind(read_csv('fr_pcs_matches.csv') %>%
          filter(match == TRUE) %>%
          select(-match) %>%
          rename(fr_race = fr,
                 pcs_race = pcs)) %>%
  unique()

#

all_climbs <- clean_climbs %>%
  
  #link with Pro cycling stats
  
  left_join(
    
    pcs_fr_matches %>%
      select(fr_race, pcs = pcs_race, year), by = c("race" = "fr_race", "year")
    
  ) %>%
  
  rename(slug = race,
         race = pcs) %>%
  
  # removed from race
  filter(!(url == 'https://www.la-flamme-rouge.eu/maps/loadtrack/181312' & climb_name == "Sestriere")) %>%
  
  # manual error correction
  mutate(start_distance = ifelse(url == 'https://www.la-flamme-rouge.eu/maps/loadtrack/159271' &
                                   climb_name == "Picon Blanco", 165, start_distance),
         end_distance = ifelse(url == 'https://www.la-flamme-rouge.eu/maps/loadtrack/155641' &
                                 climb_name == "Cote de Berland", 32, end_distance)) %>%
  
  filter(!is.na(race)) %>%
  
  # correct for prologues
  mutate(stage = ifelse(race == "Tour de Romandie" & year %in% c(2019, 2018, 2017, 2016, 2014, 2013), stage - 1,
                        ifelse(race == "Criterium du Dauphine" & year %in% c(2016, 2018), stage - 1,
                               ifelse(race == "Tour de l'Ain" & year %in% c(2013, 2014, 2015, 2017), stage - 1,
                                      ifelse(race == "Paris - Nice" & year %in% c(2013, 2015, 2016), stage - 1, stage))))) %>%
  
  unique() %>%
  
  filter(!(str_detect(climb_name, "passage sur la"))) %>%
  filter(!(str_detect(climb_name, "Finish"))) %>%
  filter(!(str_detect(climb_name, "Lap"))) %>%
  filter(!(str_detect(tolower(climb_name), "km at"))) %>%
  filter(!(str_detect(climb_name, "km @"))) %>%
  filter(!(str_detect(tolower(climb_name), "m -"))) %>%
  filter(!(str_detect(tolower(climb_name), "circuit"))) %>%
  filter(!(str_detect(climb_name, "Avenue"))) %>%
  filter(!(str_detect(climb_name, "U-turn"))) %>%
  
  # count climbs on stage
  group_by(stage, race, year, climb_name) %>%
  mutate(time_climbed = rank(end_distance, ties.method = "first")) %>%
  ungroup()

# 
# clean routes
#

all_routes <- dbGetQuery(con, "SELECT alt, dist, url FROM fr_route_data WHERE updated > '2020-07-15'") %>%    
  mutate(distances = as.numeric(dist),
         alt = as.numeric(alt)) %>%
  
  group_by(url, dist) %>%
  summarize(alt = mean(alt, na.rm = T)) %>%
  ungroup() %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT DISTINCT url, race, race_url, year
               FROM fr_stage_urls") %>%
      group_by(race, year, race_url) %>%
      mutate(stage = rank(year, ties.method = "first")) %>%
      ungroup(), by = c("url")) %>%
  
  arrange(year, race, stage, dist) %>%
  
  group_by(stage, year, race) %>%
  mutate(points = rank(dist, ties.method = "first"),
         length = max(dist, na.rm = T),
         highest_point = max(alt, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(grades = (alt - lag(alt)) / (1000 * (dist - lag(dist))),
         grades = ifelse(dist == 0, 0, grades),
         grades = ifelse(grades > 0.25, 0.25,
                         ifelse(grades < -0.25, -0.25, grades))) %>%
  
  rename(elevations = alt) %>%
  mutate(year = as.numeric(year)) %>%
  rename(distances = dist) %>%
  
  #link with Pro cycling stats
  
  left_join(
    
    pcs_fr_matches %>%
      select(fr_race, pcs = pcs_race, year), by = c("race" = "fr_race", "year")
    
  ) %>%
  
  rename(slug = race,
         race = pcs) %>%
  
  # correct for prologues
  mutate(stage = ifelse(race == "Tour de Romandie" & year %in% c(2019, 2018, 2017, 2016, 2014, 2013), stage - 1,
                        ifelse(race == "Criterium du Dauphine" & year %in% c(2016, 2018), stage - 1,
                               ifelse(race == "Tour de l'Ain" & year %in% c(2013, 2014, 2015, 2017), stage - 1,
                                      ifelse(race == "Paris - Nice" & year %in% c(2013, 2015, 2016), stage - 1, 
                                             ifelse(race == "Tour of Utah" & year == 2018, stage - 1, stage))))))

# 
# #
# # old data model
# #
# 
# all_routes <- dbReadTable(con, "fr_altitude") %>%
#   
#   unique() %>%
#   
#   mutate(distances = as.numeric(dist)) %>%
#   
#   arrange(year, race, stage, distances) %>%
#   
#   group_by(stage, year, race, distances) %>%
#   mutate(alt = mean(alt, na.rm = T)) %>%
#   ungroup() %>%
#   
#   unique() %>%
#   
#   group_by(stage, year, race) %>%
#   mutate(points = rank(distances, ties.method = "first"),
#          length = max(distances, na.rm = T),
#          highest_point = max(alt, na.rm = T)) %>%
#   ungroup() %>%
#   
#   mutate(grades = (alt - lag(alt)) / (1000 * (distances - lag(distances))),
#          grades = ifelse(distances == 0, 0, grades),
#          grades = ifelse(grades > 0.25, 0.25,
#                          ifelse(grades < -0.25, -0.25, grades))) %>%
#   
#   select(-dist) %>%
#   rename(elevations = alt) %>%
#   mutate(year = as.numeric(year)) %>%
#   
#   #link with Pro cycling stats
# 
#   left_join(
#     
#     pcs_fr_matches %>%
#       select(fr_race, pcs = pcs_race, year), by = c("race" = "fr_race", "year")
#     
#   ) %>%
#   
#   rename(slug = race,
#          race = pcs) %>%
#   
#   # correct for prologues
#   mutate(stage = ifelse(race == "Tour de Romandie" & year %in% c(2019, 2018, 2017, 2016, 2014, 2013), stage - 1,
#                         ifelse(race == "Criterium du Dauphine" & year %in% c(2016, 2018), stage - 1,
#                                ifelse(race == "Tour de l'Ain" & year %in% c(2013, 2014, 2015, 2017), stage - 1,
#                                       ifelse(race == "Paris - Nice" & year %in% c(2013, 2015, 2016), stage - 1, 
#                                              ifelse(race == "Tour of Utah" & year == 2018, stage - 1, stage))))))

# calculate stage characteristic data

final_1km <- all_routes %>%
  
  group_by(stage, year, race) %>%
  mutate(stage_end = max(length, na.rm = T)) %>%
  ungroup() %>%
  
  filter((stage_end - distances) < 1.1) %>%
  
  group_by(race, year, stage) %>%
  summarize(final_1km_elev = mean(elevations, na.rm = T),
            max_gradient = max(grades, na.rm = T),
            med_gradient = median(grades, na.rm = T),
            avg_gradient = mean(grades, na.rm = T),
            x25th_gradient = quantile(grades, probs = 0.75, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  
  mutate(final_1km_gradient = (med_gradient + avg_gradient + x25th_gradient) / 3) %>%
  
  select(stage, year, race, final_1km_elev, final_1km_gradient)

#

final_5km <- all_routes %>%
  
  group_by(stage, year, race) %>%
  mutate(stage_end = max(length, na.rm = T)) %>%
  ungroup() %>%
  
  filter((stage_end - distances) < 5.1) %>%
  
  group_by(race, year, stage) %>%
  summarize(final_5km_elev = mean(elevations, na.rm = T),
            max_gradient = max(grades, na.rm = T),
            med_gradient = median(grades, na.rm = T),
            avg_gradient = mean(grades, na.rm = T),
            x25th_gradient = quantile(grades, probs = 0.75, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  
  mutate(final_5km_gradient = (med_gradient + avg_gradient + x25th_gradient) / 3) %>%
  
  select(stage, year, race, final_5km_elev, final_5km_gradient)

#

percentage_climbing_in_final_climb <- all_routes %>%
  
  arrange(stage, year, race, points) %>%
  
  group_by(stage, year, race) %>%
  mutate(stage_end = max(length, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(final_20km = ifelse((stage_end - distances) < 20.6, grades, NA)) %>%
  
  mutate(distance_chunks = distances - lag(distances),
         distance_chunks = ifelse(distances == 0, NA, distance_chunks)) %>%
  
  mutate(vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * final_20km, 0),
         total_vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * grades, 0)) %>%
  
  group_by(stage, year, race) %>%
  summarize(final_20km_vert_gain = sum(vert_gain, na.rm = T),
            total_vert_gain = sum(total_vert_gain, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(perc_gain_end = final_20km_vert_gain / total_vert_gain)

#

lumpiness <- all_routes %>%
  
  arrange(stage, year, race, points) %>%
  
  mutate(distance_chunks = distances - lag(distances),
         distance_chunks = ifelse(distances == 0, NA, distance_chunks)) %>%
  
  mutate(total_elev_change = ifelse(abs(grades) > 0.02, abs(elevations - lag(elevations)), 0)) %>%
  
  group_by(stage, year, race) %>%
  summarize(total_elev_change = sum(total_elev_change, na.rm = T),
            stage_end = max(length, na.rm = T),
            time_at_1500m = mean(elevations > 1499.99, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(perc_elev_change = total_elev_change / (stage_end * 1000))

#

stage_characteristics <- all_routes %>%
  
  filter(!(is.na(points))) %>%
  filter(points == 1) %>%
  select(-points, -distances, -elevations, -grades,
         -race_url, -url) %>%
  
  inner_join(
    
    lumpiness %>%
      select(-stage_end), by = c("stage", "race", "year")
    
  ) %>%
  inner_join(
    
    percentage_climbing_in_final_climb, by = c("stage", "race", "year")
    
  ) %>%
  inner_join(
    
    final_1km, by = c("stage", "race", "year")
    
  ) %>%
  inner_join(
    
    final_5km, by = c("stage", "race", "year")
    
  ) %>%
  
  filter(!is.na(race)) %>%
  filter(total_elev_change < 20000)

#
# write the altitude feature data to database
#

#dbWriteTable(con, "flamme_rouge_characteristics", stage_characteristics, row.names = FALSE, overwrite = TRUE)

#
#
#

climb_data_list <- all_climbs %>%
  
  filter(climb_name != "none")

#

climb_result_list <- vector("list", length(climb_data_list$climb_name))

#

for(c in 1:length(climb_data_list$climb_name)) {
  
  climb_result_list[[c]] <- all_routes %>%
    filter(url == climb_data_list$url[[c]] &
             distances >= climb_data_list$start_distance[[c]] &
             distances <= climb_data_list$end_distance[[c]]) %>%
    
    mutate(CLIMB = climb_data_list$climb_name[[c]],
           time_climbed = climb_data_list$time_climbed[[c]]) %>%
    
    group_by(url, stage, year, race, CLIMB, time_climbed) %>%
    summarize(low = min(elevations, na.rm = T),
              segments = n()) %>%
    ungroup() %>%
    
    mutate(summit = climb_data_list$altitude[[c]],
           length = climb_data_list$end_distance[[c]] - climb_data_list$start_distance[[c]])
  
}

#

climb_results <- bind_rows(climb_result_list) %>%
  
  rename(climb_name = CLIMB) %>%
  
  mutate(low = ifelse(race == "Tour de France" & stage == 6 & year == 2016,
                      ifelse(climb_name == "Cote d'Aubin", 264, 
                             ifelse(climb_name == "Cote de Saint-Antonin-Noble-Val", 164,
                                    ifelse(climb_name == "Col des Estaques", 202, low))), low)) %>%
  
  mutate(low = ifelse(race == "Giro d'Italia" & year == 2014 & stage == 11,
                      ifelse(climb_name == "Passo Cento Croci", 450,
                             ifelse(climb_name == "Naso di Gatto", 100, low)), low)) %>%
  
  mutate(summit = ifelse(race == "Giro d'Italia" & year == 2013 & stage == 18,
                         ifelse(climb_name == "Polsa", 1201, summit), summit))

#

all_climbs_int <- all_climbs %>%
  
  inner_join(climb_results %>%
               select(url, stage, race, year, climb_name, time_climbed,
                      length, summit, low), by = c("url", "stage", "race", "year", "climb_name", "time_climbed")) %>%
  
  mutate(start_distance = ifelse(race == "Tour de Suisse" & year == 2018 & stage == 3 & time_climbed == 3,
                                 ifelse(climb_name == "Hagenfirst", 173.1, start_distance), start_distance),
         length = ifelse(race == "Tour de Suisse" & year == 2018 & stage == 3 & time_climbed == 3,
                         ifelse(climb_name == "Hagenfirst", 3.9, length), length),
         start_distance = ifelse(race == "Vuelta a Burgos" & year == 2017 & stage == 1 & time_climbed == 2,
                                 ifelse(climb_name == "Castillo de Burgos", 154.1, start_distance), start_distance),
         length = ifelse(race == "Vuelta a Burgos" & year == 2017 & stage == 1 & time_climbed == 2,
                         ifelse(climb_name == "Castillo de Burgos", 1, length), length),
         start_distance = ifelse(race == "Paris - Nice" & year == 2017 & stage == 6,
                                 ifelse(climb_name == "Fayence", 192.2, start_distance), start_distance),
         start_distance = ifelse(race == "Paris - Nice" & year == 2017 & stage == 7,
                                 ifelse(climb_name == "Cote de Gattieres", 5, start_distance), start_distance),
         length = ifelse(race == "Paris - Nice" & year == 2017 & stage == 6,
                         ifelse(climb_name == "Fayence", 1.3, length), length),
         start_distance = ifelse(race == "Amgen Tour of California" & year == 2017 & stage == 2,
                                 ifelse(climb_name == "Del Puerto Canyon", 60.3, start_distance), start_distance),
         low = ifelse(race == "Amgen Tour of California" & year == 2017 & stage == 2,
                      ifelse(climb_name == "Del Puerto Canyon", 492, low), low),
         start_distance = ifelse(race == "Amgen Tour of California" & year == 2017 & stage == 4,
                                 ifelse(climb_name == "Ojai Santa Paula Rd", 62, start_distance), start_distance),
         end_distance = ifelse(race == "Criterium du Dauphine" & year == 2017 & stage == 7,
                               ifelse(climb_name == "Cote de Berland", 29.6, end_distance), end_distance),
         low = ifelse(race == "Criterium du Dauphine" & year == 2017 & stage == 7,
                      ifelse(climb_name == "Cote de Berland", 404, low), low)) %>%
  
  mutate(length = end_distance - start_distance) %>%
  
  mutate(gradient = (summit - low) / (1000 * length))

#

mtn_names <- all_climbs_int %>%
  filter(!is.na(category)) %>%
  mutate(mtn = str_sub(climb_name, 1, str_locate(climb_name, " ") %>% .[, 2])) %>%
  
  group_by(mtn) %>%
  summarize(n = n(), 
            len = mean(length, na.rm = T), 
            grade = mean(gradient, na.rm = T)) %>%
  arrange(-n)

#

fr_worldwide_climbs <- dbGetQuery(con, "SELECT DISTINCT TRACKNAME FROM fr_all_european_climbs") %>%
  
  mutate(TRACKNAME = str_trim(str_replace(TRACKNAME, "(via D110 - Entraigues)", ""))) %>%
  select(TRACKNAME)

# 

matching_with_every_climb <- all_climbs_int %>%
  
  filter(source == "ACTUAL CLIMBS" & is.na(category) & (gradient < 0.04 | length < 1.5)) %>%
  
  select(climb_name, url, race_url, source) %>%
  
  mutate(merge = "YES") %>%
  
  left_join(
    
    fr_worldwide_climbs %>%
      
      mutate(merge = "YES"), by = c("merge")
    
  ) %>%
  
  mutate(sd = stringdist::stringdist(tolower(climb_name), tolower(TRACKNAME), method = "qgram", q = 2) / nchar(climb_name)) %>%
  
  group_by(climb_name) %>% 
  mutate(rk = rank(sd, ties.method = "min")) %>%
  filter(rk < 4) %>% 
  ungroup() %>%
  
  filter(sd < 0.300 & rk == 1)

#

matching_with_every_climb_others <- all_climbs_int %>%
  
  select(climb_name, url, race_url, source) %>%
  
  mutate(merge = "YES") %>%
  
  filter(source != "ACTUAL CLIMBS") %>%
  
  left_join(
    
    fr_worldwide_climbs %>%
      mutate(merge = "YES"), by = c("merge")
    
  ) %>%
  
  mutate(sd = stringdist::stringdist(tolower(climb_name), tolower(TRACKNAME), method = "qgram", q = 2) / nchar(climb_name)) %>%
  
  group_by(climb_name) %>% 
  mutate(rk = rank(sd, ties.method = "min")) %>%
  filter(rk < 4) %>% 
  ungroup() %>%
  
  filter(sd < 0.251 & rk == 1)

#

actual_climbs_list <- all_climbs_int %>%
  
  filter(source == "ACTUAL CLIMBS" & ((gradient > 0.0399 | length > 1.49) | !is.na(category)))

#

valid_climbs <- actual_climbs_list %>%
  
  select(climb_name, url, race_url) %>%
  
  rbind(
    
    matching_with_every_climb_others %>%
      select(climb_name, url, race_url)
    
  ) %>%
  rbind(
    
    matching_with_every_climb %>%
      select(climb_name, url, race_url)
    
  ) %>%
  
  unique()

#

invalid_climbs <- all_climbs_int %>%
  anti_join(valid_climbs, by = c("climb_name", "url", "race_url"))

#

all_climbs_data <- all_climbs_int %>%
  
  inner_join(valid_climbs, by = c("climb_name", "url", "race_url")) %>%
  
  mutate(length = ifelse(climb_name == "Alto de Jaizkibel" & year == 2013 & race == "Clasica Ciclista San Sebastian", 7.24, length),
         length = ifelse(climb_name == "Cote D'Ereffe" & year == 2018 & race == "Baloise Belgium Tour	", 1.62, length),
         gradient = ifelse(climb_name == "Alto de Jaizkibel" & year == 2013 & race == "Clasica Ciclista San Sebastian", 0.06, gradient),
         gradient = ifelse(climb_name == "Cote D'Ereffe" & year == 2018 & race == "Baloise Belgium Tour	", 0.062, gradient)
         ) %>%
  
  unique()

#

#dbWriteTable(con, "fr_all_climbs_intermediate", all_climbs_data, row.names = F, overwrite = TRUE)

#
#
#
#
#
#

# GAM has R^2 of 0.76 vs 0.71 for LM
# including altitude improves R^2 by 0.02 or so over just VAM poly

all_climbs_data <- dbReadTable(con, "fr_all_climbs_intermediate") %>%
  unique()

#

gam_mod = mgcv::gam(category ~ alt + s(vam_poly, k = 5), 
                    
                    # the KOM point values found by measuring max efforts in total watts are
                    # HC ~20, 1st ~10, 2nd ~5, 3rd ~3, 4th ~1.5
                    # I scale up for cat 2s and up
                    
                    data = all_climbs_data %>%
                      filter(!is.na(category)) %>%
                      mutate(category = ifelse(category == 1, 12, 
                                               ifelse(category == 2, 6, 
                                                      ifelse(category == 3, 3, 
                                                             ifelse(category == 4, 1.5, 24))))) %>% 
                      filter(race %in% c("Vuelta a Espana", "Tour de France", "Giro d'Italia")) %>%
                      select(gradient, length, summit, category, time_climbed, stage, year, climb_name) %>%
                      unique() %>%
                      mutate(vam_poly = ((gradient^2) * length), 
                             alt = summit - 1000))

summary(gam_mod)

summary(read_rds("model-climb-difficulty.rds"))

write_rds(gam_mod, "model-climb-difficulty.rds")

# LM is deprecated

# lm_mod <- lm(category ~ alt + vam_poly, 
#              
#              # the KOM point values found by measuring max efforts in total watts are
#              # HC ~20, 1st ~10, 2nd ~5, 3rd ~3, 4th ~1.5
#              
#              data = all_climbs_data %>%
#                filter(!is.na(category)) %>%
#                mutate(category = ifelse(category == 1, 10, 
#                                         ifelse(category == 2, 5, 
#                                                ifelse(category == 3, 3, 
#                                                       ifelse(category == 4, 1.5, 20))))) %>% 
#                filter(race %in% c("Vuelta a Espana", "Tour de France", "Giro d'Italia")) %>%
#                select(gradient, length, summit, category, time_climbed, stage, year, climb_name) %>%
#                unique() %>%
#                mutate(vam_poly = ((gradient^2) * length), 
#                       alt = summit - 1000))

#

climbs_to_write <- all_climbs_data %>%
  
  mutate(vam_poly = ((gradient^2) * length), 
         alt = summit - 1000) %>%
  
  cbind(
    model_category = predict(gam_mod, 
                             all_climbs_data %>%
                               mutate(vam_poly = ((gradient^2) * length),
                                      alt = summit - 1000))) %>%
  
  unique() %>%
  
  filter(gradient > 0.03 | model_category > 1.15) %>%
  
  filter(gradient > 0) %>%
  
  #mutate(model_category = ifelse(model_category < 1.25, 1.25, model_category)) %>%
  
  select(climb_name, race, stage, year, start_distance, end_distance, summit, length, time_climbed, gradient, model_category,
         vam_poly, alt) %>%
  
  unique() %>%
  
  mutate(race = ifelse(race == "Vuelta a Espana" & year == 2018, "La Vuelta a Espana", race)) %>%
  
  rbind(
    
    cbind(
      
      readr::read_csv("f_r_climbs_missing.csv") %>%
        select(-model_category) %>%
        mutate(vam_poly = ((gradient^2) * length), 
               alt = summit - 1000),
      
      model_category = predict(gam_mod, 
                               readr::read_csv("f_r_climbs_missing.csv") %>%
                                 select(-model_category) %>%
                                 mutate(vam_poly = ((gradient^2) * length), 
                                        alt = summit - 1000)))
    
  ) %>%
  
  filter(!(str_detect(climb_name, "passage sur la"))) %>%
  filter(!(str_detect(climb_name, "Finish"))) %>%
  filter(!(str_detect(climb_name, "Lap"))) %>%
  filter(!(str_detect(tolower(climb_name), "km at"))) %>%
  filter(!(str_detect(climb_name, "km @"))) %>%
  filter(!(str_detect(tolower(climb_name), "m -"))) %>%
  filter(!(str_detect(tolower(climb_name), "circuit"))) %>%
  filter(!(str_detect(climb_name, "Avenue"))) %>%
  filter(!(str_detect(climb_name, "U-turn"))) %>%
  
  as_tibble()


#dbWriteTable(con, "flamme_rouge_climbs", climbs_to_write, overwrite = TRUE, row.names = FALSE)
