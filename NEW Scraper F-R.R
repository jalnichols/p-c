
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

# Scrape all UCI World Tour races for 2013-19

scraper_list <- tibble(year = c(2013, 2014, 2015, 2016, 2017, 2017, 2018, 2018, 2019, 2019, 2020),
                       page_no = c(1,1,1,1,1,2,1,2,1,2,1))

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
  
}

all_races <- bind_rows(result_list)

# Now scraper Europe/Americas/WC/Asia Tour for 2017-19

scraper_list <- tibble(year = c(2017, 2017, 2017, 2017, 2017, 
                                2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018,
                                2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 
                                2020, 2020, #2
                                2017, 2018, 2019, 2020, #3
                                2017, 2018, 2019, 2020, #8
                                2017, 2018, 2018, 2019, 2020), #4
                       page_no = c(1,2,3,4,5, 
                                   1,2,3,4,5,6,7,8,9,10, 
                                   1,2,3,4,5,6,7, 8,9,
                                   1,2,
                                   1, 1, 1,1,
                                   1, 1, 1,1, 
                                   1, 1, 2, 1,1),
                       type = c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                3,3,3,3,
                                8,8,8,8, 
                                4,4,4,4,4))

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
  
}

intermediate_races <- all_races %>%
  rbind(
    
    bind_rows(result_list))

# NON UWT events to retain data

retain_ids <- c(
  
  # 1.HC
  
  # scheldeprijs, Nokere, tre valli varesine,
  # milan torino, paris tours, brussels, gp indus, kuurne
  # brabantse, almeria, drenthe, gran piemonte, emilia
  # laigueglia
  
  69, 70, 79, 80, 81, 83, 98, 99, 100, 214, 262, 820, 75, 101,
  
  # 1.1
  
  # Meml Pantani, Toscano, Ventoux
  
  349, 86, 828,
  
  # 2.1
  
  # Occitanie, Provence, Julio Alvarez, deutschland
  # castilla/leon, madrid, adriatica ionica,
  # colombia, l'ain, austria, valencia
  # san juan, slovenia, murcia, japan cup
  
  159, 158, 274, 440, 287, 285, 505, 359, 337, 334, 210,
  204, 309, 213, 82, 227,
  
  # 2.HC
  
  # Dunkirk, belgium, wallonia, norway, arctic
  # denmark, britain, luxembourg
  # dubai, oman, alps
  # burgos, ruta del sol, algarve, utah, langkawi
  
  180, 175, 91, 168, 89, 87, 88, 264, 162, 148,
  66, 90, 92, 93, 96, 61, 173,
  
  # other HC level European races
  
  270, 173, 302, 102, 166, 167, 171, 260, 35, 76, 77, 78, 97,
  
  # added remaining 1.1 races on 2020-03-26
  
  72,74,140,146,147,156,157,267,288,319,329,689,690,759,
  
  # 1.1 Euros in 2019
  
  520, 749, 139, 145, 152, 691, 352, 353, 316, 802, 351, 85,
  346, 354, 348, 350, 347, 787, 341, 360, 344, 342, 340, 343, 
  330, 327, 328, 318, 317, 310, 315, 308, 335, 311, 304, 299, 
  286, 560, 528, 269, 271, 273, 272, 73, 268, 265, 266, 263, 
  453, 261, 326, 246, 243, 242, 207, 202, 201, 200, 199,
  
  # other multi-day Tours at 2.1 European level which are equal to weaker 2.HC European races
  
  209, 449, 71, 314, 151,
  
  # remaining 2.1s from 2020-03-26
  
  241,284,303,305,306,307,312,333,336,338,339,361,372,468,494,556,576,799,873,
  
  # tokyo, qinghai lake, hainan, colorado classic, saudi tour
  838, 95,94,183,1253,
  
  # tour de l'avenir
  185, 
  
  # #.2s which are high ranked on PCS
  539, 744, 479, 472, 518, 532, 895,
  
  # WC / ITT
  
  9, 14)

#

all_races <- intermediate_races %>%
  
  mutate(id = str_replace(url, 'https://www.la-flamme-rouge.eu/maps/races/view/', '')) %>%
  
  separate(id, c("trash", "id"), sep = "/") %>%
  
  select(-trash) %>%
  
  filter(str_detect(class, "UWT") | id %in% retain_ids)

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

write_rds(stages_list, "stages-list-f-r.rds")

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

#

all_stages <- dbReadTable(con, "fr_stages") %>%
  unique() %>%
  
  anti_join(dbGetQuery(con, "SELECT race_url FROM fr_climbs_scraped"), by = c("url" = "race_url")) %>%
  
  filter(year > 2019)

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
    
    # and the distance / altitude data
    other_jsons <- json[[2]]
    
    # and lat/long data
    lat_longs <- json[[1]]
    
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
             #year = stages_list[[y]]$year[[z]],
             
             # this data is wrong when all_stages != stages_list in terms of length
             #year = str_sub(all_stages$date[[y]], nchar(all_stages$date[[y]]) - 3, nchar(all_stages$date[[y]])),
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
               #year = stages_list[[y]]$year[[z]],
               #year = str_sub(all_stages$date[[y]], nchar(all_stages$date[[y]]) - 3, nchar(all_stages$date[[y]])),
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
             long = as.numeric(long))
    
    #
    #
    
    dbWriteTable(con, "fr_route_data", route_data_to_write, row.names = F, append = TRUE)
    
    #
    #
    
    Sys.sleep(runif(1, 3, 7))
    
  }
  
  #climbs_stages_list[[y]] <- bind_rows(data_list)
  
  print(y)
  
  Sys.sleep(runif(1, 5, 13))
  
  }
  
}
