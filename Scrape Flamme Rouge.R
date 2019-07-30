
library(tidyverse)
library(rvest)
library(RMySQL)

# Scrape all UCI World Tour races for 2013-19

scraper_list <- tibble(year = c(2013, 2014, 2015, 2016, 2017, 2017, 2018, 2018, 2019, 2019),
                       page_no = c(1,1,1,1,1,2,1,2,1,2))

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
                                2019, 2019, 2019, 2019, 2019, 2019, 2017, 2018, 2019,
                                2017, 2018, 2019, 2017, 2018, 2018, 2019),
                       page_no = c(1,2,3,4,5, 1,2,3,4,5,6,7,8,9,10, 1,2,3,4,5,6,
                                   1, 1, 1,
                                   1, 1, 1, 
                                   1, 1, 2, 1),
                       type = c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                3,3,3,
                                8,8,8, 
                                4,4,4,4))

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

retain_ids <- c(159, 309, 828, 337, 66, 61, 274, 69, 70, 99, 92,
                93, 213, 210, 14, 9, 204, 359, 96, 162, 148, 88, 90,
                # 2.HC
                180, 175, 91, 168, 89, 87, 88, 264,
                # 1.HC
                99, 100, 69, 70, 98, 214, 262, 80, 79, 81, 820, 83,
                # 1.1
                349, 86,
                # 2.1
                158, 440)

#

all_races <- intermediate_races %>%
  
  mutate(id = str_replace(url, 'https://www.la-flamme-rouge.eu/maps/races/view/', '')) %>%
  
  separate(id, c("trash", "id"), sep = "/") %>%
  
  select(-trash) %>%
  
  filter(str_detect(class, "UWT") | id %in% retain_ids)

# Now we can pull in all URLs from all_races

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
  
}

all_stages <- all_races

#

tictoc::tic()

climbs_stages_list <- vector("list", length(stages_list))
other_json_list <- vector("list", length(stages_list))

for(y in 1:length(stages_list)) {

  data_list <- vector('list', length(stages_list[[y]]))
  data_list2 <- vector('list', length(stages_list[[y]]))
  
  # run through each race in the stages_list (denoted by y)
  # each stage in that race will be denoted by z below
  
  for(z in 1:length(stages_list[[y]]$url)) {
    
    # this pulls out the json data
    
    json <- rjson::fromJSON(readLines(stages_list[[y]]$url[[z]]))
    
    # with the climbs / sprints data
    actual_climbs <- json[[3]]$stageclimbs
    
    # and the distance / altitude data
    other_jsons <- json[[2]]
    
    # and lat/long data
    lat_longs <- json[[1]]
    
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
        altitude = actual_climbs[[c]]$altitude
        
      )
      
    }
    
    } else {
    
      climbs_list <- vector('list', 1)
      
      c = 1
      
    climbs_list[[c]] <- tibble(climb_name = "none",
                          start_distance = as.numeric(NA),
                          end_distance = as.numeric(NA),
                          altitude = as.numeric(NA),
                          category = as.numeric(NA))
    
    }
    
    df <- bind_rows(climbs_list) %>%
      mutate(url = stages_list[[y]]$url[[z]],
             race_url = stages_list[[y]]$race_url[[z]],
             race = stages_list[[y]]$race[[z]],
             #year = stages_list[[y]]$year[[z]],
             year = str_sub(all_stages$date[[y]], nchar(all_stages$date[[y]]) - 3, nchar(all_stages$date[[y]])),
             stage = z)
    
    data_list[[z]] <- df
    
    # Now I can pull in distance, altitude data
    # I take only every tenth item + final item
    
    other_jsons <- c(other_jsons[seq(1,floor(length(other_jsons)/10)*10, 10)],
                     other_jsons[length(other_jsons)])
    
    route_list <- vector("list", length(other_jsons))
    
    for(a in 1:length(other_jsons)) {
      
      route_list[[a]] <- tibble(alt = other_jsons[[a]]$altitude,
                                dist = other_jsons[[a]]$distance,
                                lat = other_jsons[[a]]$position$A,
                                long = other_jsons[[a]]$position$k)
      
    }
    
    data_list2[[z]] <- bind_rows(route_list) %>%
      mutate(url = stages_list[[y]]$url[[z]],
             race_url = stages_list[[y]]$race_url[[z]],
             race = stages_list[[y]]$race[[z]],
             #year = stages_list[[y]]$year[[z]],
             year = str_sub(all_stages$date[[y]], nchar(all_stages$date[[y]]) - 3, nchar(all_stages$date[[y]])),
             stage = z)
    
  }
    
    climbs_stages_list[[y]] <- bind_rows(data_list)
    
    other_json_list[[y]] <- bind_rows(data_list2)
    
    Sys.sleep(runif(1, 1,9))
    
}

#
#
#
#

tictoc::toc()

readr::write_rds(climbs_stages_list, "R Code/Cycling/flamme-rouge-climbs-707.rds")
readr::write_rds(other_json_list, "R Code/Cycling/flamme-rouge-routes-707.rds")

#readr::write_rds(climbs_stages_list, "R Code/Cycling/flamme-rouge-climbs1.rds")
#readr::write_rds(other_json_list, "R Code/Cycling/flamme-rouge-routes.rds")

climbs_stages_list <- c(read_rds("R Code/Cycling/flamme-rouge-climbs1.rds"), 
                       read_rds("R Code/Cycling/flamme-rouge-climbs-707.rds"),
                       read_rds("R Code/Cycling/flamme-rouge-climbs.rds"))

other_json_list <- c(read_rds("R Code/Cycling/flamme-rouge-routes.rds"), read_rds("R Code/Cycling/flamme-rouge-routes-707.rds"))

# DB

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

# transformations below sourced from https://www.w3schools.com/tags/ref_urlencode.asp

all_climbs <- bind_rows(climbs_stages_list) %>%
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
  
  #link with Pro cycling stats
  
  left_join(
    
    readr::read_csv("R Code/Cycling/flamme-rouge-to-pcs.csv") %>%
      filter(!pcs == "UNK"), by = c("race" = "fr")
    
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
  
  # count climbs on stage
  group_by(stage, race, year, climb_name) %>%
  mutate(time_climbed = rank(end_distance, ties.method = "first")) %>%
  ungroup()

# 
# clean routes
#

all_routes <- bind_rows(other_json_list) %>%
  
  unique() %>%
  
  mutate(distances = as.numeric(dist)) %>%
  
  arrange(year, race, stage, distances) %>%
  
  group_by(stage, year, race, distances) %>%
  mutate(alt = mean(alt, na.rm = T)) %>%
  ungroup() %>%
  
  unique() %>%
  
  group_by(stage, year, race) %>%
  mutate(points = rank(distances, ties.method = "first"),
         length = max(distances, na.rm = T),
         highest_point = max(alt, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(grades = (alt - lag(alt)) / (1000 * (distances - lag(distances))),
         grades = ifelse(distances == 0, 0, grades),
         grades = ifelse(grades > 0.25, 0.25,
                         ifelse(grades < -0.25, -0.25, grades))) %>%
  
  select(-dist) %>%
  rename(elevations = alt) %>%
  
  left_join(
    
    readr::read_csv("R Code/Cycling/flamme-rouge-to-pcs.csv") %>%
      filter(!pcs == "UNK"), by = c("race" = "fr")
    
  ) %>%
  
  rename(slug = race,
         race = pcs) %>%
  
  # correct for prologues
  mutate(stage = ifelse(race == "Tour de Romandie" & year %in% c(2019, 2018, 2017, 2016, 2014, 2013), stage - 1,
                        ifelse(race == "Criterium du Dauphine" & year %in% c(2016, 2018), stage - 1,
                               ifelse(race == "Tour de l'Ain" & year %in% c(2013, 2014, 2015, 2017), stage - 1,
                                      ifelse(race == "Paris - Nice" & year %in% c(2013, 2015, 2016), stage - 1, 
                                             ifelse(race == "Tour of Utah" & year == 2018, stage - 1, stage))))))

# calculate stage characteristic data

final_1km <- all_routes %>%
  
  group_by(stage, year, race) %>%
  mutate(stage_end = max(length, na.rm = T)) %>%
  ungroup() %>%
  
  filter((stage_end - distances) < 1.6) %>%
  
  group_by(race, year, stage) %>%
  summarize(final_1km_elev = mean(elevations, na.rm = T),
            final_1km_gradient = max(grades, na.rm = T)) %>%
  ungroup()

#

final_5km <- all_routes %>%
  
  group_by(stage, year, race) %>%
  mutate(stage_end = max(length, na.rm = T)) %>%
  ungroup() %>%
  
  filter((stage_end - distances) < 5.6) %>%
  
  group_by(race, stage, year) %>%
  summarize(final_5km_elev = mean(elevations, na.rm = T),
            final_5km_gradient = max(grades, na.rm = T)) %>%
  ungroup()

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
         -race_url, -url, -lat, -long) %>%
  
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
  
  filter(!is.na(race))

#
#
#

dbWriteTable(con, "flamme_rouge_characteristics", stage_characteristics, row.names = FALSE, overwrite = TRUE)

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

lm(category ~ gradient + length + summit + gradient:length, 
   
   data = all_climbs_int %>%
     mutate(category = ifelse(category == 1, 10, 
                              ifelse(category == 2, 5, 
                                     ifelse(category == 3, 2, 
                                            ifelse(category == 4, 1, 20))))) %>% 
     filter(race %in% c("Vuelta a Espana", "Tour de France", "Giro d'Italia")) %>%
     select(gradient, length, summit, category, time_climbed, stage, year, climb_name) %>%
     unique())

#

climbs_to_write <- all_climbs_int %>%
  
  mutate(model_category = 
           (15.03 * gradient) + 
           (-0.419 * length) + 
           (0.001595 * summit) + 
           (15.929 * (gradient * length)) - 1.1) %>%
  
  unique() %>%
  
  filter(!(race == "Giro del Trentino")) %>%
  
  mutate(race = ifelse(race_url == "https://www.la-flamme-rouge.eu/maps/races/view/2018/159", "La Route d'Occitane",
                       ifelse(race_url == "https://www.la-flamme-rouge.eu/maps/races/view/2017/159", "Route du Sud - la Depeche du Midi",
                              ifelse(race_url == "https://www.la-flamme-rouge.eu/maps/races/view/2019/159", "La Route d'Occitanie - La Depeche du Midi", race)))) %>%
  
  mutate(model_category = ifelse(model_category < 0.1, 0.1, model_category)) %>%
  
  select(climb_name, race, stage, year, start_distance, end_distance, summit, length, time_climbed, gradient, model_category) %>%
  
  unique() %>%
  
  mutate(race = ifelse(race == "Vuelta a Espana" & year == 2018, "La Vuelta a Espana", race)) %>%
  
  rbind(
    
    readr::read_csv("R Code/Cycling/f_r_climbs_missing.csv")
    
  )
  

dbWriteTable(con, "flamme_rouge_climbs", climbs_to_write, overwrite = TRUE, row.names = FALSE)