

library(tidyverse)
library(RMySQL)

dbDisconnect(con)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

x2021_acts <- dbGetQuery(con, "SELECT DISTINCT activity_id
                    FROM strava_activity_data")

#

telem <- dbGetQuery(con, "SELECT *
                    
                    FROM strava_telemetry")

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
               
               filter(year %in% c("2014", "2016", "2017", "2018", "2019", "2020", "2021", "2015")) %>%
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

rm(telem)
gc()

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
  summarize(value = median(value, na.rm = T),
            metric = n()) %>%
  ungroup() %>%
  
  spread(stat, value)

#
#
#

dbWriteTable(con, "strava_stage_characteristics", stage_characteristics %>% select(-metric), overwrite = T, row.names = F)

#
#
#

gam_mod <- read_rds('Stored models/power-required-throughout-race-gam.rds')

#

preds <- cbind(all_routes %>% mutate(perc_thru = distances/length),
               
               pred = mgcv::predict.gam(gam_mod, all_routes %>% mutate(perc_thru = distances/length))) %>%
  
  group_by(stage, race, year, class, rider) %>%
  mutate(pred = pred / max(pred, na.rm = T)) %>%
  ungroup()

#

preds %>%
  
  mutate(grades = ifelse(grades <= 0, 0, grades)) %>%
  
  arrange(year, rider, class, race, stage, distances) %>%
  
  group_by(stage, race, year, class, rider) %>% 
  summarize(weighted_vert_gain = sum(pred * grades, na.rm = T) / sum(pred, na.rm = T) * mean(length * 1000, na.rm = T),
            vert_gain = mean(grades, na.rm = T) * mean(length * 1000, na.rm = T)) %>%
  ungroup() -> abc

#
#
#
#
#
#
#
#
#
#

races_pull_in <- all_routes %>%
  select(stage, race, year, class) %>%
  unique()

koms_list_tdf <- vector("list", length(races_pull_in$race))

for(S in 1:length(koms_list_tdf)) {
  
  tdf14_2020 <- all_routes %>%
    filter(year == races_pull_in$year[[S]] & race == races_pull_in$race[[S]] & stage == races_pull_in$stage[[S]]) %>%
    
    select(-highest_point, -points) %>%
    
    mutate(left_km = length - distances) %>%
    
    mutate(every_km2 = floor(left_km/0.1)/10) %>%
    
    group_by(every_km2, length, year, race, stage, rider) %>%
    summarize(
      
      left_km = min(left_km, na.rm = T),
      elevations = mean(elevations, na.rm = T)
      
    ) %>%
    ungroup() %>%
    
    arrange(rider, year, race, stage, every_km2) %>%
    
    mutate(every_km = floor(every_km2/0.25)/4) %>%
    mutate(everytenth = floor(every_km2/0.1)/10) %>%
    
    mutate(every_km = ifelse(left_km < 3, everytenth, every_km),
           gradient = (elevations - lead(elevations)) / ((lead(left_km)-left_km)*1000)) %>%
    
    group_by(every_km, length, year, race, stage, rider) %>%
    summarize(min = min(elevations, na.rm = T),
              max = max(elevations, na.rm = T),
              gradient = mean(gradient, na.rm = T),
              elevations = mean(elevations, na.rm = T),
    ) %>%
    ungroup() %>%
    
    arrange(rider, desc(every_km)) %>%
    
    mutate(change_gradient = ifelse(gradient > 0.035 | gradient < -0.035,
                                    ifelse(gradient > 0.035 & lag(gradient < 0.035), "uphill",
                                           ifelse(gradient < -0.035 & lag(gradient > -0.035), "downhill", NA)), "flat")) %>%
    
    fill(change_gradient, .direction = "down") %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    mutate(every_km3 = floor(every_km)) %>%
    mutate(everytenth = floor(every_km/0.5)/2) %>%
    
    mutate(every_km3 = ifelse(every_km < 3, everytenth, every_km3)) %>%
    
    mutate(segment_distance = every_km - lead(every_km),
           segment_distance = ifelse(is.na(segment_distance), lag(segment_distance), segment_distance)) %>%
    
    group_by(every_km = every_km3, length, year, race, stage, rider) %>%
    summarize(min = min(elevations, na.rm = T),
              max = max(elevations, na.rm = T),
              gradient = sum(gradient * segment_distance, na.rm = T) / sum(segment_distance, na.rm = T),
              distance = sum(segment_distance, na.rm = T),
              elevations = mean(elevations, na.rm = T),
    ) %>%
    ungroup() %>%
    
    arrange(rider, desc(every_km)) %>%
    
    mutate(change_gradient = ifelse(gradient > 0.03 | gradient < -0.03,
                                    ifelse(gradient > 0.03 & lag(gradient < 0.03), "uphill",
                                           ifelse(gradient < -0.03 & lag(gradient > -0.03), "downhill", NA)), "flat")) %>%
    
    fill(change_gradient, .direction = "down") %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                    ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
    
    rownames_to_column() %>%
    
    mutate(grouping = ifelse(rowname == 1, 1, ifelse(change_gradient != lag(change_gradient), 1, NA)),
           rk = rank(grouping, ties.method = "first"),
           grouping = ifelse(is.na(grouping), NA, rk)) %>%
    
    fill(grouping, .direction = "down")
  
  #
  
  different_groupings <- tdf14_2020 %>%
    
    filter(change_gradient == "uphill") %>%
    
    group_by(grouping, stage_length = length, race, stage, year, rider) %>%
    summarize(start_km = max(every_km, na.rm = T),
              end_km = min(every_km, na.rm = T),
              start_elev = min(elevations, na.rm = T),
              end_elev = max(elevations, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(length = start_km - end_km,
           gain = end_elev - start_elev,
           gradient = gain / (length * 1000)) %>%
    
    filter(length > 0) %>%
    filter(gradient > 0.035)
  
  #
  
  riders <- different_groupings %>%
    select(rider) %>%
    unique() %>%
    count() %>%
    .[[1]]
  
  unique_climbs <- different_groupings %>%
    group_by(rider) %>%
    count() %>%
    ungroup() %>%
    summarize(median = median(n)) %>%
    .[[1]] %>%
    round()
  
  if(riders > 1) {
    
    kmeans(different_groupings %>% select(end_km, end_elev, gradient, length) %>% scale(), centers = unique_climbs, nstart = 500) -> K
    
    cbind(cl = K$cluster, different_groupings) %>%
      group_by(cl, stage, race, year) %>%
      summarize(alt = mean(end_elev, na.rm = T),
                start_km = mean(start_km, na.rm = T),
                end_km = mean(end_km, na.rm = T),
                gradient = mean(gradient, na.rm = T),
                length = mean(length, na.rm = T),
                stage_length = mean(stage_length, na.rm = T),
                matches = n()) %>%
      ungroup() -> clustered_climbs
    
  } else {
    
    different_groupings %>% 
      rowid_to_column() %>% 
      rename(cl = rowid) %>%
      group_by(cl, stage, race, year) %>%
      summarize(alt = mean(end_elev, na.rm = T),
                start_km = mean(start_km, na.rm = T),
                end_km = mean(end_km, na.rm = T),
                gradient = mean(gradient, na.rm = T),
                length = mean(length, na.rm = T),
                stage_length = mean(stage_length, na.rm = T)) %>%
      ungroup() -> clustered_climbs
    
  }
  
  if(length(different_groupings$grouping) == 0) {
    
  } else {
    
    #
    
    data_climbs <- cbind(clustered_climbs,
                                 
                                 model_category = mgcv::predict.gam(read_rds('model-climb-difficulty.rds'),
                                                                    clustered_climbs %>%
                                                                      mutate(vam_poly = ((gradient^2)*length)) %>%
                                                                      mutate(alt = alt - 1000))) %>%
      mutate(perc_thru = 1 - (end_km / stage_length))
    
    #
    
    final_data_climbs <- cbind(
      
      data_climbs,
      
      power_required = mgcv::predict.gam(read_rds("Stored models/power-required-throughout-race-gam.rds"), data_climbs)) %>%
      
      cbind(best = mgcv::predict.gam(read_rds("Stored models/power-required-throughout-race-gam.rds"), tibble(perc_thru = 1))) %>%
      
      mutate(rel_to_best = power_required / best) %>%
      
      select(-best, -power_required) %>%
      
      rename(power_required = rel_to_best) %>%
      
      mutate(power_model_category = power_required * model_category)
    
    #
    
    koms_list_tdf[[S]] <- final_data_climbs
    
    print(races_pull_in$race[[S]])
    
  }
  
}

#

all_2021_koms <- bind_rows(koms_list_tdf)

#

dbWriteTable(con, "climbs_from_strava_telemetry", all_2021_koms, append = TRUE, row.names = F)
