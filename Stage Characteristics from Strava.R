

library(tidyverse)
library(RMySQL)

dbDisconnect(con)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

telem <- dbReadTable(con, "strava_telemetry")

#

all_routes <- telem %>%
  
  inner_join(dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Distance')") %>% 
               
               # clean up the dates
               mutate(Y = str_sub(DATE, nchar(DATE)-3, nchar(DATE))) %>% 
               separate(DATE, into = c("weekday", "date", "drop"), sep = ",") %>% 
               mutate(date = paste0(str_trim(date),", ", Y)) %>% 
               select(-weekday, -drop, -Y, -VALUE, -Stat) %>% 
               
               mutate(date = lubridate::mdy(date)) %>% 
               unique() %>%
               janitor::clean_names(), by = c("activity_id")) %>%
  
  inner_join(dbGetQuery(con, "SELECT date, rider, stage, race, year, class FROM pcs_stage_data WHERE time_trial = 0") %>%
               
               filter(year %in% c("2014", "2015", "2016", "2017")) %>%
               mutate(date = as.Date(date, origin = '1970-01-01')) %>%
               unique(), by = c("date", "pcs" = "rider")) %>% 
  
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

#

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
    
    percentage_climbing_in_final_climb, by = c("stage", "race", "year", "class", "rider")
    
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
  summarize(value = median(value, na.rm = T)) %>%
  ungroup() %>%
  
  spread(stat, value)

