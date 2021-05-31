

library(tidyverse)
library(rvest)
library(RMySQL)

dbDisconnect(con)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

all_race_activities <- dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Distance')") %>% 
  
  # clean up the dates
  mutate(Y = str_sub(DATE, nchar(DATE)-3, nchar(DATE))) %>% 
  separate(DATE, into = c("weekday", "date", "drop"), sep = ",") %>% 
  mutate(date = paste0(str_trim(date),", ", Y)) %>% 
  select(-weekday, -drop, -Y) %>% 
  
  # clean up the stat values
  mutate(VALUE = str_replace(VALUE, "mi", ""), 
         VALUE = str_replace(VALUE, "W", ""), 
         VALUE = as.numeric(VALUE)) %>% 
  
  mutate(date = lubridate::mdy(date)) %>% 
  unique() %>% 
  spread(Stat, VALUE) %>% 
  janitor::clean_names() %>% 
  
  inner_join(dbGetQuery(con, "SELECT rider, date, stage, race, year, class, length, stage_type, missing_profile_data FROM pcs_stage_data") %>%
              
               mutate(date = as.Date(date, origin = '1970-01-01')) %>%
               unique(), by = c("date", "pcs" = "rider")) %>% 
  
  # if two results exist for same day matching distance, it's probably a recon and TT which
  # means drop the lower watts
  
  # also, many riders include distance outside the TT as part of their strava activity
  # so maybe accept any riders +/- 10 km? or maybe we just can't get accurate TT data
  
  mutate(distance = distance * 1.609) %>% 
  filter((distance / length) > 0.95) %>%
  filter((distance / length) < 1.05) %>%
  
  inner_join(
    
    fs::dir_info('D:/Jake/Documents/STRAVA_JSON/') %>%
      #filter(birth_time > '2021-03-01') %>%
      select(path) %>%
      mutate(activity_id = str_replace(path, 'D:/Jake/Documents/STRAVA_JSON/strava-activity-id-', ''),
             activity_id = str_replace(activity_id, ".rds", "")), by = c("activity_id")
    
  )

# prep data

pcs_stage_data <- dbGetQuery(con, "SELECT date, rider, stage, race, year, class FROM pcs_stage_data WHERE time_trial = 0") %>%
  
  mutate(date = as.Date(date, origin = '1970-01-01')) %>%
  unique()

# strava data

strava_act_data <- dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Distance')") %>% 
  
  # clean up the dates
  mutate(Y = str_sub(DATE, nchar(DATE)-3, nchar(DATE))) %>% 
  separate(DATE, into = c("weekday", "date", "drop"), sep = ",") %>% 
  mutate(date = paste0(str_trim(date),", ", Y)) %>% 
  select(-weekday, -drop, -Y, -VALUE, -Stat) %>% 
  
  mutate(date = lubridate::mdy(date)) %>% 
  unique() %>%
  janitor::clean_names()

#
#
#
#
# DEFINE FUNCTION

extract_telemetry <- function(act_page) {
  
  #remDr$navigate(act_page)
  
  #Sys.sleep(75)
  
  # access JSON on page
  
  #JSON_xpath <- '/html/body/pre'
  
  #json_data <- remDr$findElement(using = 'xpath', JSON_xpath)
  
  # extract into a list
  
  #data_lists <- json_data$getElementText() %>%
  #  .[[1]] %>%
  #  jsonlite::fromJSON()
  
  #
  
  #write_rds(data_lists, paste0("D:/Jake/Documents/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
  
  data_lists <- read_rds(paste0("D:/Jake/Documents/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
  
  # clean this up before writing to DB
  
  df <- cbind(
    
    altitude = data_lists[["altitude"]],
    distance = data_lists[["distance"]],
    time = data_lists[["time"]],
    #watts = data_lists[["watts_calc"]],
    activity_id = ACTIVITY
    
  ) %>%
    
    #cbind(as_tibble(data_lists$latlng)) %>%
    
    #rename(latitude = V1,
    #       longitude = V2) %>%
    
    as_tibble() %>%
    mutate(altitude = as.numeric(altitude),
           distance = as.numeric(distance),
           #watts = as.numeric(watts),
           time = as.numeric(time)) %>%
    mutate(distance = round(distance, 0),
           altitude = round(altitude, 0))
  
  # set up data for stage characteristics code
  
  all_routes <- df %>%
    
    inner_join(strava_act_data, by = c("activity_id")) %>%
    
    inner_join(pcs_stage_data, by = c("date", "pcs" = "rider")) %>% 
    
    rename(dist = distance,
           alt = altitude,
           rider = pcs) %>%
    
    mutate(dist = floor(dist / 50) * 0.05) %>%
    
    group_by(stage, race, year, class, rider, dist) %>%
    summarize(alt = median(alt, na.rm = T)) %>%
    ungroup() %>%
    
    arrange(year, race, stage, class, rider, dist) %>%
    
    group_by(year, race, stage, class, rider) %>%
    mutate(points = rank(dist, ties.method = "first"),
           length = max(dist, na.rm = T),
           avg_alt = mean(alt, na.rm = T),
           highest_point = max(alt, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(grades = (alt - lag(alt)) / (1000 * (dist - lag(dist))),
           grades = ifelse(dist == 0, 0, grades),
           grades = ifelse(grades > 0.25, 0.25,
                           ifelse(grades < -0.25, -0.25, grades))) %>%
    
    rename(elevations = alt) %>%
    mutate(year = as.numeric(year)) %>%
    rename(distances = dist)
  
  # calc stage chars
  
  final_1km <- all_routes %>%
    
    group_by(stage, class, year, race, rider) %>%
    mutate(stage_end = max(length, na.rm = T)) %>%
    ungroup() %>%
    
    filter((stage_end - distances) < 1.1) %>%
    
    group_by(stage, class, year, race, rider) %>%
    summarize(final_1km_elev = mean(elevations, na.rm = T),
              max_gradient = max(grades, na.rm = T),
              med_gradient = median(grades, na.rm = T),
              avg_gradient = mean(grades, na.rm = T),
              x25th_gradient = quantile(grades, probs = 0.75, na.rm = T),
              n = n()) %>%
    ungroup() %>%
    
    mutate(final_1km_gradient = (med_gradient + avg_gradient + x25th_gradient) / 3) %>%
    
    select(stage, class, year, race, rider, final_1km_elev, final_1km_gradient)
  
  #
  
  final_5km <- all_routes %>%
    
    group_by(stage, class, year, race, rider) %>%
    mutate(stage_end = max(length, na.rm = T)) %>%
    ungroup() %>%
    
    filter((stage_end - distances) < 5.1) %>%
    
    group_by(stage, class, year, race, rider) %>%
    summarize(final_5km_elev = mean(elevations, na.rm = T),
              max_gradient = max(grades, na.rm = T),
              med_gradient = median(grades, na.rm = T),
              avg_gradient = mean(grades, na.rm = T),
              x25th_gradient = quantile(grades, probs = 0.75, na.rm = T),
              n = n()) %>%
    ungroup() %>%
    
    mutate(final_5km_gradient = (med_gradient + avg_gradient + x25th_gradient) / 3) %>%
    
    select(stage, class, year, race, rider, final_5km_elev, final_5km_gradient)
  
  #
  
  percentage_climbing_in_final_climb <- all_routes %>%
    
    arrange(stage, year, race, class, rider, points) %>%
    
    group_by(stage, class, year, race, rider) %>%
    mutate(stage_end = max(length, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(final_20km = ifelse((stage_end - distances) < 20.6, grades, NA)) %>%
    
    mutate(distance_chunks = distances - lag(distances),
           distance_chunks = ifelse(distances == 0, NA, distance_chunks)) %>%
    
    mutate(vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * final_20km, 0),
           total_vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * grades, 0)) %>%
    
    group_by(stage, class, year, race, rider) %>%
    summarize(final_20km_vert_gain = sum(vert_gain, na.rm = T),
              total_vert_gain = sum(total_vert_gain, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(perc_gain_end = final_20km_vert_gain / total_vert_gain)
  
  #
  
  percentage_climbing_start <- all_routes %>%
    
    arrange(stage, year, race, class, rider, points) %>%
    
    group_by(stage, class, year, race, rider) %>%
    mutate(stage_end = max(length, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(first_30km = ifelse((distances) < 30.6, grades, NA)) %>%
    
    mutate(distance_chunks = distances - lag(distances),
           distance_chunks = ifelse(distances == 0, NA, distance_chunks)) %>%
    
    mutate(vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * first_30km, 0),
           total_vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * grades, 0)) %>%
    
    group_by(stage, class, year, race, rider) %>%
    summarize(first_30km_vert_gain = sum(vert_gain, na.rm = T),
              total_vert_gain = sum(total_vert_gain, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(perc_gain_start = first_30km_vert_gain / total_vert_gain)
  
  # data below taken from a paper showing aerobic power at certain elevation levels (in feet) compared to sea-level
  # eg, at 14,000 feet it's about 71% of at sea-level
  
  model_data <- tibble(elev = seq(0,14000,1000), 
                       aap = c(0.999, 0.992, 0.983, 0.972, 0.959, 0.944, 0.927, 0.907, 0.886, 0.863, 0.837, 0.809, 0.78, 0.748, 0.714)) %>% 
    mutate(elev = elev * 0.3048)
  
  mgcv::gam(aap ~ s(elev, k = 5), data = model_data) -> gam_mod
  
  weighted_alt <- cbind(all_routes, pred = all_routes %>%
                          rename(elev = elevations) %>% 
                          mgcv::predict.gam(object = gam_mod)) %>% 
    group_by(stage, race, year, class, rider) %>% 
    summarize(weighted_altitude = mean(pred, na.rm = T)) %>%
    ungroup()
  
  #
  
  lumpiness <- all_routes %>%
    
    arrange(stage, year, race, class, rider, points) %>%
    
    mutate(distance_chunks = distances - lag(distances),
           distance_chunks = ifelse(distances == 0, NA, distance_chunks)) %>%
    
    mutate(total_elev_change = ifelse(abs(grades) > 0.02, abs(elevations - lag(elevations)), 0)) %>%
    
    group_by(stage, class, year, race, rider) %>%
    summarize(total_elev_change = sum(total_elev_change, na.rm = T),
              stage_end = max(length, na.rm = T),
              time_at_1500m = mean(elevations > 1499.99, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(perc_elev_change = total_elev_change / (stage_end * 1000))
  
  #
  
  stage_characteristics <- all_routes %>%
    
    filter(!(is.na(points))) %>%
    filter(points == 1) %>%
    select(-points, -distances, -elevations, -grades) %>%
    
    inner_join(
      
      lumpiness %>%
        select(-stage_end), by = c("stage", "race", "year", "class", "rider")
      
    ) %>%
    
    inner_join(
      
      weighted_alt, by = c("stage", "race", "year", "class", "rider")
      
    ) %>%
    inner_join(
      
      percentage_climbing_in_final_climb, by = c("stage", "race", "year", "class", "rider")
      
    ) %>%
    inner_join(
      
      percentage_climbing_start %>%
        select(-total_vert_gain), by = c("stage", "race", "year", "class", "rider")
      
    ) %>%
    inner_join(
      
      final_1km, by = c("stage", "race", "year", "class", "rider")
      
    ) %>%
    inner_join(
      
      final_5km, by = c("stage", "race", "year", "class", "rider")
      
    ) %>%
    
    filter(total_vert_gain >= 0) %>%
    
    gather(stat, value, -stage, -race, -year, -class, -rider) %>%
    
    group_by(stat, stage, race, year, class) %>%
    summarize(value = median(value, na.rm = T),
              metric = n()) %>%
    ungroup() %>%
    
    spread(stat, value)
  
  # write to DB
  
  dbWriteTable(con, "strava_stage_characteristics", stage_characteristics %>% select(-metric), append = T, row.names = F)
  
}

#
# 
# 

tictoc::tic()

for(a in 1:length(all_race_activities$activity_id)) {
  
  ACTIVITY <- all_race_activities$activity_id[[a]]
  
  act_page <- paste0('https://www.strava.com/activities/', 
                     
                     ACTIVITY, 
                     
                     '/streams?stream_types%5B%5D=time',
                     #'&stream_types%5B%5D=watts_calc',
                     '&stream_types%5B%5D=altitude&stream_types%5B%5D=heartrate&stream_types%5B%5D=distance')
  
  safe_segments <- safely(.f = extract_telemetry, otherwise = print("error"))
  
  safe_segments(act_page)
  
  print(a)
  
}

tictoc::toc()