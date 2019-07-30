
library(tidyverse)
library(rvest)
library(rjson)

# Set up to scrape cycling-stages KML JSONs of stages

stage_races <- tibble(
  stage_url = c("criterium-du-dauphine-2018-stage-",
                "paris-nice-2018-stage-",
                "tour-de-romandie-2018-stage-",
                "vuelta-spain-2018-stage-",
                "giro-italy-2019-race-",
                "tour-de-suisse-2018-stage-",
                "tour-de-france-2018-stage-",
                "volta-a-catalunya-2019-stage-",
                "tour-colombia-2019-stage-",
                "tour-down-under-2019-stage-",
                "uae-tour-2019-stage-",
                "tour-of-the-alps-2019-stage-",
                "tour-of-california-2019-race-",
                "tour-of-valencia-2019-stage-",
                "paris-nice-2018-stage-",
                "giro-italy-2018-stage-",
                "tour-de-france-2017-stage-",
                "tour-de-france-2016-stage-",
                "giro-italy-2017-stage-",
                "giro-italy-2016-stage-",
                "vuelta-spain-2017-stage-",
                "vuelta-spain-2016-stage-",
                "criterium-du-dauphine-2017-stage-",
                "tour-de-suisse-2017-stage-",
                "tour-de-romandie-2017-stage-",
                "tour-of-the-basque-country-2017-stage-",
                "volta-a-catalunya-2017-stage-",
                "paris-nice-2017-stage-",
                "ruta-del-sol-2017-stage-",
                "tirreno-adriatico-2017-stage-",
                "tour-de-france-2015-stage-"),
  
  measurements = c(419, 325, 419, 419, 419, 419,
                   419, 419, 419, 419, 419, 419,
                   419, 419, 419, 419, 419, 419,
                   419, 419, 419, 419, 419, 419,
                   419, 419, 419, 419, 419, 419,
                   419),
  
  stages = c(7, 8, 5, 21, 21, 9, 
             21, 7, 6, 6, 7, 5, 
             7, 5, 8, 21, 21, 21,
             21, 21, 21, 21, 8, 9,
             5, 6, 7, 8, 5, 7,
             21))

stage_race_stage_list <- vector("list", length(stage_races$stages))

for(i in 1:length(stage_race_stage_list)) {
  
  race_list <- vector("list", stage_races$stages[[i]])
  
  for(n in 1:stage_races$stages[[i]]) {
    
    race_list[[n]] <- tibble(url = paste0("https://cdn.cyclingstage.com/images/imap/", 
                                          
                                          stage_races$stage_url[[i]], n, 
                                          
                                          "-route.kml-", stage_races$measurements[[i]], ".json"))
    
  }
  
  stage_race_stage_list[[i]] <- bind_rows(race_list)
  
}

#

stages_list <- bind_rows(stage_race_stage_list) %>%
  .[[1]]

#

one_dayers <- tibble(
  
  stage_url = c("world-championships-2018-road-race",
                "world-championships-2018-itt-men",
                "milan-san-remo-2019-route",
                "strade-bianche-2019-route",
                "gent-wevelgem-2019-route",
                "tour-of-flanders-2019-route",
                "amstel-gold-race-2019-route",
                "paris-roubaix-2019-route",
                "la-fleche-wallonne-2019-route",
                "liege-bastogne-liege-2019-route",
                "brabantse-pijl-2019-route",
                "omloop-het-nieuwsblad-2019-route",
                "kuurne-brussels-kuurne-2019-route",
                "e3-binckbank-classic-2019-route"),
  
  measurements = c(419, 419, 419, 419, 419, 419,
                   419, 419, 419, 419, 419, 419,
                   419, 419))

#

one_day_stage_list <- vector("list", length(one_dayers$measurements))

for(i in 1:length(one_day_stage_list)) {
  
  one_day_stage_list[[i]] <- tibble(url = paste0("https://cdn.cyclingstage.com/images/imap/", 
                                                 
                                                 one_dayers$stage_url[[i]], 
                                                 
                                                 ".kml-", one_dayers$measurements[[i]], ".json"))
  
}

#

stages_list <- c(
  
  bind_rows(stage_race_stage_list) %>% .[[1]],
  
  bind_rows(one_day_stage_list)  %>% .[[1]],

  c('https://cdn.cyclingstage.com/images/imap/giro-italy-2019-rit-8-route.kml-419.json', 
    'https://cdn.cyclingstage.com/images/imap/giro-italy-2019-race-21-route.kml-325.json',
    'https://cdn.cyclingstage.com/images/imap/vuelta-spain-2018-stage-1-route.kml-325.json', 
    'https://cdn.cyclingstage.com/images/imap/criterium-du-dauphine-2018-stage-3-route.kml-325.json', 
    'https://cdn.cyclingstage.com/images/imap/tour-de-suisse-2018-stage-1-route.kml-325.json',
    'https://cdn.cyclingstage.com/images/imap/tour-de-suisse-2018-stage-8-route.kml-325.json', 
    'https://cdn.cyclingstage.com/images/imap/tour-de-suisse-2018-stage-9-route.kml-325.json', 
    'https://cdn.cyclingstage.com/images/imap/tour-colombia-2019-stage-1-route.kml-325.json', 
    'https://cdn.cyclingstage.com/images/imap/tour-of-california-2019-stage-2-route.kml-419.json',
    'https://cdn.cyclingstage.com/images/imap/tour-of-california-2019-stage-4-route.kml-419.json', 
    'https://cdn.cyclingstage.com/images/imap/tour-of-california-2019-stage-5-route.kml-419.json',
    'https://cdn.cyclingstage.com/images/imap/tour-of-california-2019-stage-7-route.kml-419.json',
    'https://cdn.cyclingstage.com/images/imap/criterium-du-dauphine-2017-stage-4-route.kml-325.json',
    'https://cdn.cyclingstage.com/images/imap/tour-de-france-2016-stage-18-route.kml-325.json',
    'https://cdn.cyclingstage.com/images/imap/tour-de-france-2017-stage-20-route.kml-325.json',
    'https://cdn.cyclingstage.com/images/imap/tour-de-france-2017-stage-21-route.kml-325.json',
    'https://cdn.cyclingstage.com/images/imap/giro-italy-2018-stage-1-route.kml-325.json',
    'https://cdn.cyclingstage.com/images/imap/giro-italy-2018-stage-16-route.kml-325.json',
    'https://cdn.cyclingstage.com/images/imap/giro-italy-2018-stage-21-route.kml-325.json',
    'https://cdn.cyclingstage.com/images/imap/vuelta-spain-2017-stage-1-route.kml-325.json',
    'https://cdn.cyclingstage.com/images/imap/vuelta-spain-2017-stage-1-route.kml-325.json',
    'https://cdn.cyclingstage.com/images/imap/vuelta-spain-2017-stage-16-route.kml-325.json',
    'https://cdn.cyclingstage.com/images/imap/vuelta-spain-2017-stage-21-route.kml-325.json'))

stages_list <- stages_list[lengths(stages_list) != 0]

# 

stages_kml_list <- vector("list", length(stages_list))
stages_obj_list <- vector("list", length(stages_list))

for(s in 1:length(stages_list)) {
  
  if(RCurl::url.exists(stages_list[[s]])) {
  
    Sys.sleep(runif(1, 3, 9))
    
    raw <- fromJSON(readLines(stages_list[[s]]))
  
    df <- tibble(
    
     points = seq(1, length(raw$elevations), 1),
      distances = raw$distances,
      elevations = raw$elevations,
      grades = raw$grades,
      length = raw$calculatedLength,
     highest_point = raw$maxElevation,
      stage_url = stages_list[[s]]
  
    )
  
  obj <- raw$itemMarkersObj
  
  } else {
    
    obj <- "blank"
    df <- tibble(
      
      points = NA,
      distances = NA,
      elevations = NA,
      grades = NA,
      length = NA,
      highest_point = NA,
      stage_url = stages_list[[s]]
      
    )
    
  }
  
  stages_kml_list[[s]] <- df
  stages_obj_list[[s]] <- obj
  
  Sys.sleep(runif(1, 1,7))
  
}

#
#
#

readr::write_rds(stages_obj_list, "R Code/Cycling/stages-obj-list.rds")
readr::write_rds(stages_kml_list, "R Code/Cycling/stages-kml-list.rds")

stages_obj_list <- readr::read_rds("R Code/Cycling/stages-obj-list.rds")
stages_kml_list <- readr::read_rds("R Code/Cycling/stages-kml-list.rds")

#

stage_kml_data <- bind_rows(stages_kml_list) %>%
  
  mutate(race = str_replace(stage_url, "https://cdn.cyclingstage.com/images/imap/", ""),
         race = str_replace(race, "route.kml-", ""),
         race = str_replace(race, ".json", ""),
         race = str_replace(race, "325", ""),
         race = str_replace(race, "419", "")) %>%
  
  group_by(race, points) %>%
  mutate(n = n()) %>%
  ungroup() %>%

  group_by(race) %>%
  mutate(pts = n()) %>%
  ungroup() %>%
  
  filter((pts < 420) | (pts > 325 & str_detect(stage_url, "419"))) %>%
  
  group_by(race, points) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  
  group_by(race) %>%
  mutate(pts = n()) %>%
  ungroup()
  
#
#
#

# handle obj list
# these have start as element [[1]], finish as element [[2]], categorized climb / cobbles other (can find icon used)
# correspond to measurement ids in first column

obj_result_list <- vector("list", length(stages_obj_list))

for(o in 1:length(stages_obj_list)) {
  
  RACE <- stages_list[[o]]
  
  obj_list <- stages_obj_list[[o]]
  
  if(length(obj_list) < 3) {
    
    obj_result_list[[o]] <- tibble(start = NA,
                                   end = NA,
                                   type = "blank",
                                   race = RACE)
    
  } else {
  
  res_list <- vector('list', length(obj_list))
  
  for(x in 3:length(obj_list)) {
    
    obj <- obj_list[[x]]
    
    df <- tibble(
      
      start = obj$startIndex,
      end = obj$index,
      type = str_replace(obj$icon, "https://cdn.cyclingstage.com/images/icons/", ""),
      race = RACE)
    
    res_list[[x]] <- df
      
  }
  
  }
  
  obj_result_list[[o]] <- bind_rows(res_list)
  
}

#

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

#

objects <- bind_rows(obj_result_list) %>%
  
  mutate(race = str_replace(race, "https://cdn.cyclingstage.com/images/imap/", ""),
         race = str_replace(race, "route.kml-", ""),
         race = str_replace(race, ".json", ""),
         race = str_replace(race, "325", ""),
         race = str_replace(race, "419", "")) %>%
  
  mutate(url = race) %>%
  
  mutate(itt = ifelse(str_detect(race, "itt"), TRUE, FALSE)) %>%
  
  mutate(race = str_replace(race, "-stage-", "/"),
         race = str_replace(race, "2019-race-", "2019/"),
         race = str_replace(race, "2018-race-", "2018/"),
         race = str_replace(race, "2017-race-", "2017/"),
         race = str_replace(race, "2016-race-", "2016/"),
         race = str_replace(race, "2015-race-", "2015/"),
         race = str_replace(race, "-2019", "/2019"),
         race = str_replace(race, "-2018", "/2018"),
         race = str_replace(race, "-2017", "/2017"),
         race = str_replace(race, "-2016", "/2016"),
         race = str_replace(race, "-2015", "/2015"),
         race = ifelse(str_sub(race, nchar(race), nchar(race)) == "-", str_sub(race, 1, nchar(race) - 1), race)) %>%
  
  separate(race, c("race", "year", "stage"), sep = "/") %>%
  
  mutate(stage = ifelse(is.na(stage), 1, stage)) %>%
  
  mutate(year = ifelse(race == "world-championships", str_sub(year, 1, 4), year),
         stage = ifelse(race == "world-championships", ifelse(itt == TRUE, "itt", "road race"), stage)) %>%
  
  mutate(stage = as.numeric(stage)) %>%
  
  select(-itt) %>%
  
  mutate(race = str_replace_all(race, "-", " "),
         race = capwords(race)) %>%
  
  left_join(
    
    readr::read_csv("R Code/Cycling/cycling-stages-to-pcs.csv"), by = c("race" = "cycst")
    
  ) %>%
  
  rename(slug = race,
         race = pcs) %>%
  
  inner_join(
    
    stage_kml_data %>%
      select(points, start_distances = distances, 
             start_elevations = elevations, race) %>%
      group_by(race, points), by = c("url" = "race", "start" = "points")
    
  ) %>%
  inner_join(
    
    stage_kml_data %>%
      select(points, end_distances = distances, 
             end_elevations = elevations, race), by = c("url" = "race", "end" = "points")
    
  ) %>%
  
  mutate(climb_length = end_distances - start_distances,
         climb_ascent = end_elevations - start_elevations,
         climb_grade = climb_ascent / (1000 * climb_length)) %>%
  
  unique()

#
#
#
#
#

# calculate stage characteristic data

final_1km <- stage_kml_data %>%
  
  group_by(stage_url, race) %>%
  mutate(stage_end = max(length, na.rm = T)) %>%
  ungroup() %>%
  
  filter((stage_end - distances) < 1.6) %>%
  
  group_by(race, stage_url) %>%
  summarize(final_1km_elev = mean(elevations, na.rm = T),
            final_1km_gradient = mean(grades, na.rm = T)) %>%
  ungroup()

#

final_5km <- stage_kml_data %>%
  
  group_by(stage_url, race) %>%
  mutate(stage_end = max(length, na.rm = T)) %>%
  ungroup() %>%
  
  filter((stage_end - distances) < 5.6) %>%
  
  group_by(race, stage_url) %>%
  summarize(final_5km_elev = mean(elevations, na.rm = T),
            final_5km_gradient = mean(grades, na.rm = T)) %>%
  ungroup()

#

percentage_climbing_in_final_climb <- stage_kml_data %>%
  
  arrange(stage_url, race, points) %>%
  
  group_by(stage_url, race) %>%
  mutate(stage_end = max(length, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(final_20km = ifelse((stage_end - distances) < 20.6, grades, NA)) %>%
  
  mutate(distance_chunks = distances - lag(distances),
         distance_chunks = ifelse(distances == 0, NA, distance_chunks)) %>%
  
  mutate(vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * final_20km, 0),
         total_vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * grades, 0)) %>%
  
  group_by(stage_url, race) %>%
  summarize(final_20km_vert_gain = sum(vert_gain, na.rm = T),
            total_vert_gain = sum(total_vert_gain, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(perc_gain_end = final_20km_vert_gain / total_vert_gain)

#

lumpiness <- stage_kml_data %>%
  
  arrange(stage_url, race, points) %>%

  mutate(distance_chunks = distances - lag(distances),
         distance_chunks = ifelse(distances == 0, NA, distance_chunks)) %>%
  
  mutate(total_elev_change = ifelse(abs(grades) > 0.02, 1000 * distance_chunks * abs(grades), 0)) %>%
  
  group_by(stage_url, race) %>%
  summarize(total_elev_change = sum(total_elev_change, na.rm = T),
            stage_end = max(length, na.rm = T),
            time_at_1500m = mean(elevations > 1499.99, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(perc_elev_change = total_elev_change / (stage_end * 1000))

#

categorized_climbs <- 

#

stage_characteristics <- stage_kml_data %>%
  
  filter(!(is.na(points))) %>%
  filter(points == 1) %>%
  select(-points, -distances, -elevations, -grades) %>%
  
  inner_join(
    
    lumpiness %>%
      select(-stage_end), by = c("stage_url", "race")
    
  ) %>%
  inner_join(
    
    percentage_climbing_in_final_climb, by = c("stage_url", "race")
    
  ) %>%
  inner_join(
    
    final_1km, by = c("stage_url", "race")
    
  ) %>%
  inner_join(
    
    final_5km, by = c("stage_url", "race")
    
  ) %>%
  
  mutate(race = str_replace(race, "-stage-", "/"),
         race = str_replace(race, "2019-race-", "2019/"),
         race = str_replace(race, "2018-race-", "2018/"),
         race = str_replace(race, "2017-race-", "2017/"),
         race = str_replace(race, "2016-race-", "2016/"),
         race = str_replace(race, "2015-race-", "2015/"),
         race = str_replace(race, "-2019", "/2019"),
         race = str_replace(race, "-2018", "/2018"),
         race = str_replace(race, "-2017", "/2017"),
         race = str_replace(race, "-2016", "/2016"),
         race = str_replace(race, "-2015", "/2015"),
         race = ifelse(str_sub(race, nchar(race), nchar(race)) == "-", str_sub(race, 1, nchar(race) - 1), race)) %>%
  
  separate(race, c("race", "year", "stage"), sep = "/") %>%
  
  mutate(stage = ifelse(is.na(stage), 1, stage)) %>%
  
  mutate(year = ifelse(race == "world-championships", str_sub(year, 1, 4), year),
         stage = ifelse(race == "world-championships", ifelse(length < 100, "itt", "road race"), stage)) %>%
  
  mutate(race = str_replace_all(race, "-", " "),
         race = capwords(race)) %>%
  
  left_join(
    
    readr::read_csv("R Code/Cycling/cycling-stages-to-pcs.csv"), by = c("race" = "cycst")
    
  ) %>%
  
  rename(slug = race,
         race = pcs)

#

library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

dbWriteTable(con, "cycling_stages_characteristics", stage_characteristics)
