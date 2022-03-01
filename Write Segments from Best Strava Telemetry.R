

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
#
#

all_stage_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE year > 2021") %>%
  
  mutate(date = as.Date(date))

#

all_race_activities <- dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Distance','AvgElapsed Time')") %>% 
  
  # clean up the dates
  mutate(Y = str_sub(DATE, nchar(DATE)-3, nchar(DATE))) %>% 
  separate(DATE, into = c("weekday", "date", "drop"), sep = ",") %>% 
  mutate(date = paste0(str_trim(date),", ", Y)) %>% 
  select(-weekday, -drop, -Y) %>% 
  
  # clean up the stat values
  mutate(VALUE = str_replace(VALUE, "mi", ""), 
         VALUE = str_replace(VALUE, "W", "")) %>% 
  
  mutate(date = lubridate::mdy(date)) %>% 
  unique() %>% 
  spread(Stat, VALUE) %>% 
  janitor::clean_names() %>%
  mutate(distance = as.numeric(distance)) %>%
  
  mutate(pcs = str_to_title(pcs)) %>%
  
  inner_join(dbGetQuery(con, "SELECT * FROM stage_data_perf
                        WHERE year > 2021") %>%
               
               mutate(date = as.Date(date)) %>%
               mutate(date = as.Date(date, origin = '1970-01-01')) %>%
               unique(), by = c("date", "pcs" = "rider")) %>% 
  
  # if two results exist for same day matching distance, it's probably a recon and TT which
  # means drop the lower watts
  
  # also, many riders include distance outside the TT as part of their strava activity
  # so maybe accept any riders +/- 10 km? or maybe we just can't get accurate TT data
  
  mutate(distance = distance * 1.609) %>% 
  filter((distance / length) > 0.90) %>%
  filter((distance / length) < 1.10) %>%
  
  filter(time_trial == 0)

#
#
#
#
#
#

telemetry_available <- all_race_activities %>%
  
  inner_join(
    
    fs::dir_info('D:/Jake/Documents/STRAVA_JSON/') %>%
      select(path, birth_time) %>%
      mutate(activity_id = str_replace(path, 'D:/Jake/Documents/STRAVA_JSON/strava-activity-id-', ''),
             activity_id = str_replace(activity_id, ".rds", "")), by = c("activity_id")) %>%
  
  group_by(stage, race, year, class, date) %>%
  filter(n() >= 20) %>%
  ungroup()

#
# for creating new segments from closest Strava telemetry to actual race route
#

for_creating_segments_from_strava <- telemetry_available %>%
  filter(rnk < 200) %>% 
  
  mutate(race_time = as.numeric(lubridate::hms(avg_elapsed_time)),
         race_time = ifelse(is.na(race_time), as.numeric(lubridate::ms(avg_elapsed_time)), race_time)) %>%
  
  filter(race_time < 40000) %>%
  
  
  filter(!activity_id %in% c("5028077839", "5063406337", "5125903648", "5485415918", "5026068681",
                             "5062666498", "5489444684", "5026221808", '2628306420', '2189109844',
                             '2628945164', '2624417915', '2628945164', '2722684158', '2770112067',
                             '2298569287', '2164552486', '2152995982', '2291747237', '2241381376',
                             '2768142025', '2686351242', '2428741219', '2339987833', '2575788478',
                             '2297631533', '2287342940', '4825833669', '2389816951', '5125549501', 
                             '2276954758')) %>%
  
  group_by(stage, race, year, class, date, length) %>% 
  mutate(abs_time = abs(race_time - total_seconds),
         abs_dist = abs(distance - length),
         pct_time = percent_rank(desc(abs_time)),
         pct_dist = percent_rank(desc(abs_dist)),
         hm = 2 / ((1/pct_time)+(1/pct_dist))) %>% 
  filter(rank(desc(hm), ties.method = "first") == 1) %>%
  ungroup() %>% 
  select(pcs, distance, race, stage, class, date, year, length, activity_id, stage_type) %>%
  mutate(creation_type = "TIME") %>%
  anti_join(dbGetQuery(con, "SELECT DISTINCT stage, race, year, date, creation_type FROM segments_from_strava_data") %>% 
              mutate(date = as.Date(date),
                     stage = as.character(stage))) %>%
  
  filter(!(stage == 1 & race == 'tour de romandie' & year == 2019)) %>%
  filter(!(stage == 4 & race == 'voo-tour de wallonie' & year == 2019))

#
#
#

for(g in 1:length(for_creating_segments_from_strava$race)) {
  
  correct_stage_info <- for_creating_segments_from_strava[g,]
  
  # bring in Flamme Rouge data
  
  ACTIVITY <- correct_stage_info$activity_id[[1]]
  
  data_lists <- read_rds(paste0("D:/Jake/Documents/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
  
  # clean this up before writing to DB
  
  lat <- data_lists$latlng[,1]
  long <- data_lists$latlng[,2]
  
  if(length(lat) == 0) {
    print(paste0("NO LAT-", ACTIVITY))
  } else {
    
    route <- cbind(
      
      altitude = data_lists[["altitude"]],
      distance = data_lists[["distance"]],
      time = data_lists[["time"]],
      activity_id = ACTIVITY
      
    ) %>%
      
      as_tibble() %>%
      mutate(altitude = as.numeric(altitude),
             distance = as.numeric(distance),
             time = as.numeric(time)) %>%
      mutate(distance = round(distance, 0),
             altitude = round(altitude, 0)) %>%
      
      cbind(lat = lat,
            long = long) %>%
    
      mutate(spd = (distance - lag(distance)) / (time - lag(time))) %>% 
      filter(!(spd < 2.75 & distance < 1000)) %>%
      
      rowid_to_column()
    
    #
    
    all_routes <- route %>%
      
      mutate(distance = distance/1000) %>%
      
      arrange(distance) %>%
      
      mutate(grades = (altitude - lag(altitude)) / (1000 * (distance - lag(distance))),
             grades = ifelse(distance == 0, 0, grades),
             grades = ifelse(grades > 0.25, 0.25,
                             ifelse(grades < -0.25, -0.25, grades))) %>%
      
      rename(elevations = altitude,
             distances = distance)
    
    #
    #
    
    smooth_route <- all_routes %>%
      
      mutate(length = max(distances)) %>%
      
      mutate(left_km = length - distances) %>%
      
      mutate(every_km2 = floor(left_km/0.1)/10) %>%
      
      group_by(every_km2) %>%
      summarize(
        
        left_km = min(left_km, na.rm = T),
        elevations = mean(elevations, na.rm = T),
        lat = median(lat, na.rm = T),
        long = median(long, na.rm = T)
        
      ) %>%
      ungroup() %>%
      
      arrange(every_km2) %>%
      
      #mutate(every_km = floor(every_km2/0.25)/4) %>%
      mutate(everytenth = floor(every_km2/0.1)/10) %>%
      
      mutate(every_km = ifelse(left_km < 500, everytenth, every_km),
             gradient = (elevations - lead(elevations)) / ((lead(left_km)-left_km)*1000)) %>%
      
      group_by(every_km) %>%
      summarize(min = min(elevations, na.rm = T),
                max = max(elevations, na.rm = T),
                gradient = mean(gradient, na.rm = T),
                elevations = mean(elevations, na.rm = T),
                lat = median(lat, na.rm = T),
                long = median(long, na.rm = T)
      ) %>%
      ungroup() %>%
      
      arrange(desc(every_km)) %>%
      
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
      mutate(everytenth = floor(every_km/0.25)/4) %>%
      
      mutate(every_km3 = ifelse(every_km < 500, everytenth, every_km3)) %>%
      
      mutate(segment_distance = every_km - lead(every_km),
             segment_distance = ifelse(is.na(segment_distance), lag(segment_distance), segment_distance)) %>%
      
      group_by(every_km = every_km3) %>%
      summarize(min = min(elevations, na.rm = T),
                max = max(elevations, na.rm = T),
                gradient = sum(gradient * segment_distance, na.rm = T) / sum(segment_distance, na.rm = T),
                distance = sum(segment_distance, na.rm = T),
                elevations = mean(elevations, na.rm = T),
                lat = median(lat, na.rm = T),
                long = median(long, na.rm = T)
      ) %>%
      ungroup() %>%
      
      arrange(desc(every_km)) %>%
      
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
    
    different_groupings_uphill <- smooth_route %>%
      
      filter(change_gradient == "uphill") %>%
      
      group_by(grouping) %>%
      mutate(IS_START = ifelse(every_km == max(every_km, na.rm = T), 1, 0),
             IS_END = ifelse(every_km == min(every_km, na.rm = T), 1, 0),
             start_lat = ifelse(IS_START==1, lat, NA),
             end_lat = ifelse(IS_END==1, lat, NA),
             start_long = ifelse(IS_START==1, long, NA),
             end_long = ifelse(IS_END==1, long, NA)) %>%
      summarize(start_km = max(every_km, na.rm = T),
                end_km = min(every_km, na.rm = T),
                start_elev = min(elevations, na.rm = T),
                end_elev = max(elevations, na.rm = T),
                start_lat = mean(start_lat, na.rm = T),
                end_lat = mean(end_lat, na.rm = T),
                start_long = mean(start_long, na.rm = T),
                end_long = mean(end_long, na.rm = T)) %>%
      ungroup() %>%
      
      mutate(length = start_km - end_km,
             gain = end_elev - start_elev,
             gradient = gain / (length * 1000)) %>%
      
      filter(length > 0) %>%
      filter(gradient > 0.025)
    
    #
    
    different_groupings_downhill <- smooth_route %>%
      
      filter(change_gradient == "downhill") %>%
      
      group_by(grouping) %>%
      mutate(IS_START = ifelse(every_km == max(every_km, na.rm = T), 1, 0),
             IS_END = ifelse(every_km == min(every_km, na.rm = T), 1, 0),
             start_lat = ifelse(IS_START==1, lat, NA),
             end_lat = ifelse(IS_END==1, lat, NA),
             start_long = ifelse(IS_START==1, long, NA),
             end_long = ifelse(IS_END==1, long, NA)) %>%
      summarize(start_km = max(every_km, na.rm = T),
                end_km = min(every_km, na.rm = T),
                start_elev = min(elevations, na.rm = T),
                end_elev = max(elevations, na.rm = T),
                start_lat = mean(start_lat, na.rm = T),
                end_lat = mean(end_lat, na.rm = T),
                start_long = mean(start_long, na.rm = T),
                end_long = mean(end_long, na.rm = T)) %>%
      ungroup() %>%
      
      mutate(length = start_km - end_km,
             gain =  start_elev - end_elev,
             gradient = gain / (length * 1000)) %>%
      
      filter(length > 0) %>%
      filter(gradient < -0.025)
    
    #
    
    if(length(different_groupings_downhill$grouping) > 0 | length(different_groupings_uphill$grouping) > 0) {
      
      segments <- rbind(different_groupings_downhill, different_groupings_uphill) %>%
        
        mutate(stage_length = max(route$distance)) %>%
        
        cbind(correct_stage_info %>%
                select(activity_id, stage, race, year, date, creation_type))
      
      #
      
      dbWriteTable(con, "segments_from_strava_data", segments, append = TRUE, row.names = F)
      
    } else {
      print(paste0("NO SEGS-", ACTIVITY))
    }
    
  }
  
  print(g)
}

#
#
#

segments_from_fr_data <- dbReadTable(con, "segments_from_strava_data") %>%
  mutate(date = as.Date(date),
         stage = as.character(stage)) %>%
  left_join(all_stage_data %>%
              filter(time_trial == 0) %>%
              select(stage, race, year, class, date, pcs_stage_length = length) %>%
              unique(), by = c("race", "stage", "year", "date")) %>%
  
  mutate(stage_length = stage_length / 1000) %>%
  filter(creation_type == "TIME")

#
#
#

already_calculated <- dbGetQuery(con, "SELECT DISTINCT activity_id FROM strava_new_segment_creation_interim
                                    WHERE creation_type = 'TIME'")

RRR <- segments_from_fr_data %>%
  
  filter(!is.na(pcs_stage_length)) %>%
  
  group_by(race, stage, class, year, date, creation_type) %>%
  summarize(segs= n()) %>%
  ungroup() %>%
  
  filter(date > (lubridate::today()-8)) %>%

  inner_join(telemetry_available %>%
               anti_join(already_calculated) %>%
               select(stage, race, year, class, date) %>%
               group_by(stage, race, year, class, date) %>%
               count() %>%
               ungroup() %>%
               unique()) %>%
  
  arrange(desc(class %in% c("1.UWT", "2.UWT")), desc(segs >= 25), desc(date))

#
#
#
#
#
#
#

for(g in 1:length(RRR$race)) {
  
  tictoc::tic()
  
  R=RRR$race[[g]]
  S=RRR$stage[[g]]
  Y=RRR$year[[g]]
  
  #
  
  correct_stage_info <- segments_from_fr_data %>%
    filter(!is.na(pcs_stage_length)) %>%
    
    filter(race == R &
             stage == S &
             year == Y) %>%
    select(activity_id) %>%
    unique()
  
  # bring in Power File
  
  ACTIVITY <- correct_stage_info$activity_id[[1]]
  
  data_lists <- read_rds(paste0("D:/Jake/Documents/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
  
  # clean this up before writing to DB
  
  lat <- data_lists$latlng[,1]
  long <- data_lists$latlng[,2]
  
  route <- cbind(
    
    altitude = data_lists[["altitude"]],
    distance = data_lists[["distance"]],
    time = data_lists[["time"]],
    activity_id = ACTIVITY
    
  ) %>%
    
    as_tibble() %>%
    mutate(altitude = as.numeric(altitude),
           distance = as.numeric(distance),
           time = as.numeric(time)) %>%
    mutate(distance = round(distance, 0),
           altitude = round(altitude, 0)) %>%
    
    cbind(lat = lat,
          long = long) %>%
    
    mutate(spd = (distance - lag(distance)) / (time - lag(time))) %>% 
    filter(spd > 2.75)
  
  route$distance[1] <- 0
  
  # extract finish and start data
  
  vfinish_lat <- route$lat[[length(route$distance)]]
  vfinish_long <- route$long[[length(route$distance)]]
  vfinish_dist <- route$distance[[length(route$distance)]]
  
  vstart_lat <- route$lat[[1]]
  vstart_long <- route$long[[1]]
  
  #
  # this builds segments in-between the ones programmatically found above
  #
  
  creating_new_segments <- segments_from_fr_data %>%
    
    filter(race == R & stage == S & year == Y) %>%
    
    unique() %>%
    rbind(segments_from_fr_data %>%
            
            filter(race == R & stage == S & year == Y) %>%
            
            unique() %>%
            
            arrange(start_km) %>%
            .[1,] %>%
            
            mutate(start_km = 0,
                   end_km = 0) %>%
            
            mutate(start_lat = vfinish_lat,
                   start_long = vfinish_long) %>%
            
            mutate(end_lat = vfinish_lat,
                   end_long = vfinish_long)) %>%
    
    mutate(start_km = stage_length - start_km,
           end_km = stage_length - end_km) %>%
    
    arrange(start_km) %>%
    
    rowid_to_column() %>%
    
    mutate(kind = "fill_in_gaps") %>%
    mutate(start_prior = ifelse(rowid == 1, 0, lag(end_km)),
           end_next = start_km)
  
  # and then this splits segments > 10km into smaller than 10km chunks
  
  link_with_correct_segments <- vector("list", 500)
  
  for(l in 1:nrow(creating_new_segments)) {
    
    (creating_new_segments$end_next[[l]] - creating_new_segments$start_prior[[l]]) -> length_seg
    
    if(length_seg > 10) {
      
      split_n_ways = ceiling(length_seg / 10)
      
      new_df <- vector("list", split_n_ways)
      
      for(x in 1:split_n_ways) {
        
        new_df[[x]] <- creating_new_segments[l,] %>%
          mutate(end_next = start_prior + ((x) * (length_seg / split_n_ways)),
                 start_prior = start_prior + ((x-1) * (length_seg / split_n_ways)))
        
      }
      
      link_with_correct_segments[[l]] <- bind_rows(new_df)
      
    } else {
      link_with_correct_segments[[l]] <- creating_new_segments[l,]
    }
    
  }
  
  # this joins with finish segment and all programmatic segments
  
  link_with_correct_segments <- bind_rows(link_with_correct_segments) %>%    
    
    select(start_lat, start_long, end_lat, end_long,
           stage, race, year, date, class, start_prior, end_next, creation_type, kind) %>%
    
    rbind(segments_from_fr_data %>%
            
            filter(race == R & stage == S & year == Y) %>%
            
            unique() %>%
            
            mutate(start_km = stage_length - start_km,
                   end_km = stage_length - end_km) %>%
            
            arrange(start_km) %>%
            
            mutate(kind = "generated_by_code") %>%
            
            mutate(start_prior = start_km,
                   end_next = end_km) %>%
            select(start_lat, start_long, end_lat, end_long,
                   stage, race, year, date, class, start_prior, end_next, creation_type, kind)) %>%
    
    arrange(start_prior) %>%
    
    rowid_to_column() %>%
    
    select(-kind) %>%
    
    fuzzyjoin::difference_inner_join(., route %>% mutate(distance = distance/1000),
                                     by = c("start_prior" = "distance"), max_dist = 0.015) %>%
    group_by(start_prior, end_next, stage, race, year, date, class, creation_type) %>%
    summarize(start_lat = mean(lat),
              start_long = mean(long)) %>%
    ungroup() %>%
    
    fuzzyjoin::difference_inner_join(., route %>% mutate(distance = distance/1000), 
                                     by = c("end_next" = "distance"), max_dist = 0.015) %>%
    group_by(start_prior, end_next, stage, race, year, date, class, creation_type, start_lat, start_long) %>%
    summarize(end_lat = mean(lat),
              end_long = mean(long)) %>%
    ungroup() %>%
    
    rowid_to_column()
  
  #
  #
  #
  #
  #
  
  bring_in_telem <- telemetry_available %>%
    filter(race == R & stage == S & year == Y) %>%
    left_join(
      
      dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
        
        mutate(rider = str_to_title(rider)) %>%
        filter(!is.na(weight)) %>%
        group_by(rider) %>%
        summarize(weight = median(weight)) %>%
        ungroup(), by = c("pcs" = "rider")) %>%
    anti_join(already_calculated, by = c("activity_id"))
  
  res_telem <- vector("list", length(bring_in_telem$activity_id))
  
  for(b in 1:length(bring_in_telem$activity_id)) {
    
    ACTIVITY <- bring_in_telem$activity_id[[b]]
    
    data_lists <- read_rds(paste0("D:/Jake/Documents/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
    
    # clean this up before writing to DB
    
    lat <- data_lists$latlng[,1]
    long <- data_lists$latlng[,2]
    
    if(length(lat) == 0) {
      
    } else {
      
      df <- cbind(
        
        altitude = data_lists[["altitude"]],
        distance = data_lists[["distance"]],
        time = data_lists[["time"]],
        watts = data_lists[["watts"]],
        activity_id = ACTIVITY
        
      ) %>%
        as_tibble()
      
      if(!"watts" %in% colnames(df)) {
        
        df <- cbind(df, tibble(watts = as.numeric(NA)))
        
      }
      
      df <- df %>%
        
        mutate(altitude = as.numeric(altitude),
               distance = as.numeric(distance),
               time = as.numeric(time),
               watts = as.numeric(watts)) %>%
        mutate(distance = round(distance, 0),
               altitude = round(altitude, 0)) %>%
        
        cbind(latitude = lat,
              longitude = long) %>%
        
        mutate(time_delta = time - lag(time),
               dist_delta = distance - lag(distance),
               lat_delta = latitude - lag(latitude),
               long_delta = longitude - lag(longitude),
               alt_delta = altitude - lag(altitude)) %>%
        
        mutate(speed = dist_delta/time_delta,
               gradient = alt_delta / dist_delta) %>%
        
        rowid_to_column()
      
      res_telem[[b]] <- df
      
    }
    
  }
  
  #
  # Bind all telem and then remove data before start and end points
  #
  
  df <- bind_rows(res_telem) %>%
    
    cbind(tibble(start_lat = vstart_lat,
                 start_long = vstart_long,
                 end_lat = vfinish_lat,
                 end_long = vfinish_long
    )) %>%
    
    mutate(est_start = floor(sqrt((((latitude - start_lat)*364000)^2) + (((longitude - start_long)*288000)^2))/50),
           est_finish = floor(sqrt((((latitude - end_lat)*364000)^2) + (((longitude - end_long)*288000)^2))/50)) %>%
    
    group_by(activity_id) %>%
    mutate(est_group = ifelse(est_start == min(est_start), "START", 
                              ifelse(est_finish == min(est_finish), "END", "NONE"))) %>%
    ungroup() %>%
    
    # this calculates the difference between the actual distance and expected distance of the segment
    mutate(stage_padding = ifelse(est_group == "START", (distance/1000) - 0, 
                            ifelse(est_group == "END", (distance - vfinish_dist)/1000, NA))) %>%
    
    group_by(activity_id, est_group) %>%
    mutate(estgroup_rowid = ifelse(is.na(stage_padding), 0, min(abs(stage_padding), na.rm = T)),
           estgroup_rowid = ifelse(abs(stage_padding) == estgroup_rowid, rowid, NA)) %>%
    ungroup() %>%
    
    group_by(activity_id) %>%
    mutate(finish_rowid = ifelse(est_group == "END", estgroup_rowid, NA),
           start_rowid = ifelse(est_group == "START", estgroup_rowid, NA),
           finish_rowid = mean(finish_rowid, na.rm = T),
           start_rowid = mean(start_rowid, na.rm = T)) %>%
    ungroup() %>%
    
    select(-estgroup_rowid) %>%
    
    filter(rowid >= start_rowid & rowid <= finish_rowid) %>%
    
    mutate(start_distance = ifelse(start_rowid == rowid, distance, NA),
           finish_distance = ifelse(finish_rowid == rowid, distance, NA)) %>%
    
    group_by(activity_id) %>%
    mutate(start_distance = mean(start_distance, na.rm = T),
           finish_distance = mean(finish_distance, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(distance_left = finish_distance - distance,
           distance_gone = distance - start_distance) %>%
    
    select(-est_start, -est_finish, -est_group,
           -finish_rowid, -start_rowid, -start_distance,
           -finish_distance, -start_lat, -start_long,
           -end_lat, -end_long)
  
  #
  # run thru each segment to match
  #
  
  new_segments_list <- vector("list", length(link_with_correct_segments$rowid))
  
  for(x in 1:length(link_with_correct_segments$rowid)) {
    
    clean_df <- df %>%
      
      select(-rowid) %>%
      
      cbind(link_with_correct_segments %>%
              select(rowid, race, stage, year, class, date,
                     start_lat, end_lat, start_long, end_long,
                     start_prior, end_next, creation_type) %>%
              filter(rowid == x)) %>%

      mutate(est_start = floor(sqrt((((latitude - start_lat)*364000)^2) + (((longitude - start_long)*288000)^2))/50),
             est_finish = floor(sqrt((((latitude - end_lat)*364000)^2) + (((longitude - end_long)*288000)^2))/50)) %>%
      
      group_by(activity_id) %>%
      filter((est_start == min(est_start)) | (est_finish == min(est_finish))) %>%
      mutate(est_group = ifelse(est_start == min(est_start), "START", "END")) %>%
      ungroup() %>%
      
      # this calculates the difference between the actual distance and expected distance of the segment
      mutate(padding = ifelse(est_group == "START", (distance_gone/1000) - start_prior, (distance_gone/1000) - end_next))
    
    new_segments_list[[x]] <- clean_df
    
  }
  
  created_segments <- bind_rows(new_segments_list) %>%
    
    #group_by(activity_id) %>%
    #mutate(mid_padding = median(padding)) %>% 
    #filter((abs(padding-mid_padding)) < 50) %>%
    #ungroup() %>%
    
    # we either subtract mid_padding or zero
    
    #group_by(rowid, est_group, activity_id) %>% 
    #filter(rank(abs(padding-mid_padding), ties.method = "first")==1) %>%
    #ungroup()
  
    filter(abs(padding) <= 5) %>%
    
    group_by(rowid, est_group, activity_id) %>% 
    filter(rank(abs(padding-0), ties.method = "first")==1) %>%
    ungroup()
  
  #
  
  new_segments_list2 <- vector("list", length(new_segments_list))
  
  un_segs <- created_segments$rowid %>% unique()
  
  for(x in un_segs) {
    
    clean_df <- created_segments %>%
      filter(rowid == x)
    
    # sometimes the rider did not do the segment in their telemetry
    if(length(clean_df$est_group) > length(bring_in_telem$activity_id)) {
      
      #workabovecp_of_new_segment <- clean_df %>%
        #select(est_group, distance, activity_id) %>%
        #spread(est_group, distance) %>%
        
        #inner_join(df, by = c("activity_id")) %>%
        
        #filter(distance >= START & distance <= END) %>%
        
        #arrange(time) %>%

        #left_join(bring_in_telem %>%
        #            select(activity_id, weight), by = c("activity_id")) %>%
        
        #mutate(cp = 6 * weight,
        #       AbovePower = ifelse(watts > cp, watts - cp, 0)) %>%
        
        #group_by(activity_id) %>%
        #summarize(AbovePower = sum(AbovePower)/1000) %>%
        #ungroup()
      
      power_of_new_segment <- clean_df %>%
        select(est_group, distance, activity_id) %>%
        spread(est_group, distance) %>%
        
        inner_join(df, by = c("activity_id")) %>%
        
        filter(distance >= START & distance <= END) %>%
        
        group_by(activity_id) %>%
        summarize(Power = mean(watts, na.rm = T),
                  ValidPower = sum(!is.na(watts)  & !is.nan(watts), na.rm = T),
                  ValidPoints = n()) %>%
        ungroup()
      
      distance_of_new_segment <- clean_df %>%
        select(est_group, distance, activity_id) %>%
        spread(est_group, distance) %>%
        mutate(segment_distance = END-START) %>%
        filter(!(is.na(END))) %>%
        filter(!(is.na(START))) %>%
        select(activity_id, segment_distance)
      
      position_of_new_segment <- clean_df %>%
        select(est_group, distance, activity_id) %>%
        spread(est_group, distance) %>%
        filter(!(is.na(END))) %>%
        filter(!(is.na(START))) %>%
        mutate(position_distance = (END+START)/2) %>%
        select(activity_id, position_distance)
      
      time_of_new_segment <- clean_df %>%
        select(est_group, time, activity_id) %>%
        spread(est_group, time) %>%
        mutate(segment_time = END-START) %>%
        filter(!(is.na(END))) %>%
        filter(!(is.na(START))) %>%
        select(activity_id, segment_time)
      
      alt_of_new_segment <- clean_df %>%
        select(est_group, altitude, activity_id) %>%
        spread(est_group, altitude) %>%
        mutate(segment_vertgain = END-START) %>%
        filter(!(is.na(END))) %>%
        filter(!(is.na(START))) %>%
        select(activity_id, segment_vertgain)
      
      new_segments_list2[[x]] <- link_with_correct_segments %>%
        select(rowid, race, stage, year, class, date,
               start_lat, end_lat, start_long, end_long,
               start_prior, end_next, creation_type) %>%
        filter(rowid == x) %>%
        cbind(distance_of_new_segment) %>%
        inner_join(alt_of_new_segment, by = c("activity_id")) %>%
        inner_join(time_of_new_segment, by = c("activity_id")) %>%
        inner_join(position_of_new_segment, by = c("activity_id")) %>%
        inner_join(power_of_new_segment, by = c("activity_id")) %>%
        mutate(segment_speed_kmh = (segment_distance/segment_time)/1000*3600,
               segment_gradient = segment_vertgain / segment_distance,
               Power = ifelse(is.nan(Power), NA, Power))
      
    }
  }
  
  if(nrow(bind_rows(new_segments_list2)) == 0) {} else {
    
    all_new_segments <- bind_rows(new_segments_list2) %>%
      mutate(should_be_distance = (end_next - start_prior)*1000) %>%
      
      mutate(ratio = abs(1-(should_be_distance/segment_distance)))
    
    #
    
    dbWriteTable(con, "strava_new_segment_creation_interim", all_new_segments, append = TRUE, row.names = F)

    rm(new_segments_list2)
    rm(all_new_segments)
    rm(created_segments)
    rm(new_segments_list)
    rm(clean_df)
    rm(df)
    rm(res_telem)
    
    gc()
    
  }
  
  print(paste0(R," - ", S, " - ", g))
  
  tictoc::toc()
  
}

#
#
#
#
#
#
#
#

# 250m sections -----------------------------------------------------------


already_calculated <- dbGetQuery(con, "SELECT DISTINCT activity_id FROM strava_new_segment_creation_climbs
                                    WHERE creation_type = 'TIME'")

RRR <- segments_from_fr_data %>%
  
  filter(gradient >= 0.04 & length > 0.25) %>%
  
  filter(!is.na(pcs_stage_length)) %>%
  
  group_by(race, stage, class, year, date, creation_type) %>%
  summarize(segs= n()) %>%
  ungroup() %>%
  
  inner_join(telemetry_available %>%
               anti_join(already_calculated) %>%
               select(stage, race, year, class, date) %>%
               group_by(stage, race, year, class, date) %>%
               count() %>%
               ungroup() %>%
               unique()) %>%
  
  arrange(desc(class %in% c("1.UWT", "2.UWT")), desc(segs >= 5), desc(date))

#

for(g in 1:length(RRR$race)) {
  
  tictoc::tic()
  
  R=RRR$race[[g]]
  S=RRR$stage[[g]]
  Y=RRR$year[[g]]
  
  #
  
  correct_stage_info <- segments_from_fr_data %>%
    filter(!is.na(pcs_stage_length)) %>%
    
    filter(race == R &
             stage == S &
             year == Y) %>%
    select(activity_id) %>%
    unique()
  
  # bring in Power File
  
  ACTIVITY <- correct_stage_info$activity_id[[1]]
  
  data_lists <- read_rds(paste0("D:/Jake/Documents/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
  
  # clean this up before writing to DB
  
  lat <- data_lists$latlng[,1]
  long <- data_lists$latlng[,2]
  
  route <- cbind(
    
    altitude = data_lists[["altitude"]],
    distance = data_lists[["distance"]],
    time = data_lists[["time"]],
    activity_id = ACTIVITY
    
  ) %>%
    
    as_tibble() %>%
    mutate(altitude = as.numeric(altitude),
           distance = as.numeric(distance),
           time = as.numeric(time)) %>%
    mutate(distance = round(distance, 0),
           altitude = round(altitude, 0)) %>%
    
    cbind(lat = lat,
          long = long) %>%
    
    mutate(spd = (distance - lag(distance)) / (time - lag(time))) %>% 
    filter(spd > 2.75)
  
  route$distance[1] <- 0
  
  # extract finish and start data
  
  vfinish_lat <- route$lat[[length(route$distance)]]
  vfinish_long <- route$long[[length(route$distance)]]
  vfinish_dist <- route$distance[[length(route$distance)]]
  
  vstart_lat <- route$lat[[1]]
  vstart_long <- route$long[[1]]
  
  #
  # this builds segments in-between the ones programmatically found above
  #
  
  creating_new_segments <- segments_from_fr_data %>%
    
    filter(race == R & stage == S & year == Y) %>%
    
    filter(gradient >= 0.04) %>%
    unique() %>%
    
    mutate(start_km = stage_length - start_km,
           end_km = stage_length - end_km) %>%
    
    arrange(start_km) %>%
    
    rowid_to_column() %>%
    
    mutate(kind = "expand_climbs") %>%
    mutate(start_prior = start_km,
           end_next = end_km)
  
  # and then this splits segments > 10km into smaller than 10km chunks
  
  link_with_correct_segments <- vector("list", 500)
  
  for(l in 1:nrow(creating_new_segments)) {
    
    (creating_new_segments$end_next[[l]] - creating_new_segments$start_prior[[l]]) -> length_seg
    
    if(length_seg > 0.25) {
      
      split_n_ways = ceiling(length_seg / 0.25)
      
      new_df <- vector("list", split_n_ways)
      
      for(x in 1:split_n_ways) {
        
        new_df[[x]] <- creating_new_segments[l,] %>%
          mutate(end_next = start_prior + ((x) * (length_seg / split_n_ways)),
                 start_prior = start_prior + ((x-1) * (length_seg / split_n_ways)))
        
      }
      
      link_with_correct_segments[[l]] <- bind_rows(new_df) %>%
        mutate(original_segment = creating_new_segments$rowid[[l]])
      
    } else {
      link_with_correct_segments[[l]] <- creating_new_segments[l,]
    }
    
  }
  
  # this joins with finish segment and all programmatic segments
  
  link_with_correct_segments <- bind_rows(link_with_correct_segments) %>%    
    
    select(start_lat, start_long, end_lat, end_long,original_segment,
           stage, race, year, date, class, start_prior, end_next, creation_type, kind) %>%
    
    arrange(start_prior) %>%
    
    rowid_to_column() %>%
    
    select(-kind) %>%
    
    fuzzyjoin::difference_inner_join(., route %>% mutate(distance = distance/1000),
                                     by = c("start_prior" = "distance"), max_dist = 0.015) %>%
    group_by(start_prior, end_next, stage, race, year, date, class, creation_type,original_segment) %>%
    summarize(start_lat = mean(lat),
              start_long = mean(long)) %>%
    ungroup() %>%
    
    fuzzyjoin::difference_inner_join(., route %>% mutate(distance = distance/1000), 
                                     by = c("end_next" = "distance"), max_dist = 0.015) %>%
    group_by(start_prior, end_next, stage, race, year, date, class, creation_type, start_lat, start_long,
             original_segment) %>%
    summarize(end_lat = mean(lat),
              end_long = mean(long)) %>%
    ungroup() %>%
    
    rowid_to_column()
  
  #
  #
  #
  #
  #
  
  bring_in_telem <- telemetry_available %>%
    filter(race == R & stage == S & year == Y) %>%
    anti_join(already_calculated, by = c("activity_id"))
  
  res_telem <- vector("list", length(bring_in_telem$activity_id))
  
  for(b in 1:length(bring_in_telem$activity_id)) {
    
    ACTIVITY <- bring_in_telem$activity_id[[b]]
    
    data_lists <- read_rds(paste0("D:/Jake/Documents/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
    
    # clean this up before writing to DB
    
    lat <- data_lists$latlng[,1]
    long <- data_lists$latlng[,2]
    
    if(length(lat) == 0) {
      
    } else {
      
      df <- cbind(
        
        altitude = data_lists[["altitude"]],
        distance = data_lists[["distance"]],
        time = data_lists[["time"]],
        watts = data_lists[["watts"]],
        activity_id = ACTIVITY
        
      ) %>%
        as_tibble()
      
      if(!"watts" %in% colnames(df)) {
        
        df <- cbind(df, tibble(watts = as.numeric(NA)))
        
      }
      
      df <- df %>%
        
        mutate(altitude = as.numeric(altitude),
               distance = as.numeric(distance),
               time = as.numeric(time),
               watts = as.numeric(watts)) %>%
        mutate(distance = round(distance, 0),
               altitude = round(altitude, 0)) %>%
        
        cbind(latitude = lat,
              longitude = long) %>%
        
        mutate(time_delta = time - lag(time),
               dist_delta = distance - lag(distance),
               lat_delta = latitude - lag(latitude),
               long_delta = longitude - lag(longitude),
               alt_delta = altitude - lag(altitude)) %>%
        
        mutate(speed = dist_delta/time_delta,
               gradient = alt_delta / dist_delta) %>%
        
        rowid_to_column()
      
      res_telem[[b]] <- df
      
    }
    
  }
  
  #
  # Bind all telem and then remove data before start and end points
  #
  
  df <- bind_rows(res_telem) %>%
    
    cbind(tibble(start_lat = vstart_lat,
                 start_long = vstart_long,
                 end_lat = vfinish_lat,
                 end_long = vfinish_long
    )) %>%
    
    mutate(est_start = floor(sqrt((((latitude - start_lat)*364000)^2) + (((longitude - start_long)*288000)^2))/50),
           est_finish = floor(sqrt((((latitude - end_lat)*364000)^2) + (((longitude - end_long)*288000)^2))/50)) %>%
    
    group_by(activity_id) %>%
    mutate(est_group = ifelse(est_start == min(est_start), "START", 
                              ifelse(est_finish == min(est_finish), "END", "NONE"))) %>%
    ungroup() %>%
    
    # this calculates the difference between the actual distance and expected distance of the segment
    mutate(stage_padding = ifelse(est_group == "START", (distance/1000) - 0, 
                                  ifelse(est_group == "END", (distance - vfinish_dist)/1000, NA))) %>%
    
    group_by(activity_id, est_group) %>%
    mutate(estgroup_rowid = ifelse(is.na(stage_padding), 0, min(abs(stage_padding), na.rm = T)),
           estgroup_rowid = ifelse(abs(stage_padding) == estgroup_rowid, rowid, NA)) %>%
    ungroup() %>%
    
    group_by(activity_id) %>%
    mutate(finish_rowid = ifelse(est_group == "END", estgroup_rowid, NA),
           start_rowid = ifelse(est_group == "START", estgroup_rowid, NA),
           finish_rowid = mean(finish_rowid, na.rm = T),
           start_rowid = mean(start_rowid, na.rm = T)) %>%
    ungroup() %>%
    
    select(-estgroup_rowid) %>%
    
    filter(rowid >= start_rowid & rowid <= finish_rowid) %>%
    
    mutate(start_distance = ifelse(start_rowid == rowid, distance, NA),
           finish_distance = ifelse(finish_rowid == rowid, distance, NA)) %>%
    
    group_by(activity_id) %>%
    mutate(start_distance = mean(start_distance, na.rm = T),
           finish_distance = mean(finish_distance, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(distance_left = finish_distance - distance,
           distance_gone = distance - start_distance) %>%
    
    select(-est_start, -est_finish, -est_group,
           -finish_rowid, -start_rowid, -start_distance,
           -finish_distance, -start_lat, -start_long,
           -end_lat, -end_long)
  
  #
  # run thru each segment to match
  #
  
  new_segments_list <- vector("list", length(link_with_correct_segments$rowid))
  
  for(x in 1:length(link_with_correct_segments$rowid)) {
    
    clean_df <- df %>%
      
      select(-rowid) %>%
      
      cbind(link_with_correct_segments %>%
              select(rowid, race, stage, year, class, date,
                     start_lat, end_lat, start_long, end_long,
                     start_prior, end_next, creation_type,
                     original_segment) %>%
              filter(rowid == x)) %>%
      
      mutate(est_start = floor(sqrt((((latitude - start_lat)*364000)^2) + (((longitude - start_long)*288000)^2))/50),
             est_finish = floor(sqrt((((latitude - end_lat)*364000)^2) + (((longitude - end_long)*288000)^2))/50)) %>%
      
      group_by(activity_id) %>%
      filter((est_start == min(est_start)) | (est_finish == min(est_finish))) %>%
      mutate(est_group = ifelse(est_start == min(est_start), "START", "END")) %>%
      ungroup() %>%
      
      # this calculates the difference between the actual distance and expected distance of the segment
      mutate(padding = ifelse(est_group == "START", (distance_gone/1000) - start_prior, (distance_gone/1000) - end_next))
    
    new_segments_list[[x]] <- clean_df
    
  }
  
  created_segments <- bind_rows(new_segments_list) %>%
    
    #group_by(activity_id) %>%
    #mutate(mid_padding = median(padding)) %>% 
    #filter((abs(padding-mid_padding)) < 50) %>%
    #ungroup() %>%
    
    # we either subtract mid_padding or zero
    
    #group_by(rowid, est_group, activity_id) %>% 
    #filter(rank(abs(padding-mid_padding), ties.method = "first")==1) %>%
    #ungroup()
  
  filter(abs(padding) <= 5) %>%
    
    group_by(rowid, est_group, activity_id) %>% 
    filter(rank(abs(padding-0), ties.method = "first")==1) %>%
    ungroup()
  
  #
  
  new_segments_list2 <- vector("list", length(new_segments_list))
  
  un_segs <- created_segments$rowid %>% unique()
  
  for(x in un_segs) {
    
    clean_df <- created_segments %>%
      filter(rowid == x)
    
    # sometimes the rider did not do the segment in their telemetry
    if(length(clean_df$est_group) > length(bring_in_telem$activity_id)) {
      
      power_of_new_segment <- clean_df %>%
        select(est_group, distance, activity_id) %>%
        spread(est_group, distance) %>%
        
        inner_join(df, by = c("activity_id")) %>%
        
        filter(distance >= START & distance <= END) %>%
        
        group_by(activity_id) %>%
        summarize(Power = mean(watts, na.rm = T),
                  ValidPower = sum(!is.na(watts)  & !is.nan(watts), na.rm = T),
                  ValidPoints = n()) %>%
        ungroup()
      
      distance_of_new_segment <- clean_df %>%
        select(est_group, distance, activity_id) %>%
        spread(est_group, distance) %>%
        mutate(segment_distance = END-START) %>%
        filter(!(is.na(END))) %>%
        filter(!(is.na(START))) %>%
        select(activity_id, segment_distance)
      
      position_of_new_segment <- clean_df %>%
        select(est_group, distance, activity_id) %>%
        spread(est_group, distance) %>%
        filter(!(is.na(END))) %>%
        filter(!(is.na(START))) %>%
        mutate(position_distance = (END+START)/2) %>%
        select(activity_id, position_distance)
      
      time_of_new_segment <- clean_df %>%
        select(est_group, time, activity_id) %>%
        spread(est_group, time) %>%
        mutate(segment_time = END-START) %>%
        filter(!(is.na(END))) %>%
        filter(!(is.na(START))) %>%
        select(activity_id, segment_time)
      
      alt_of_new_segment <- clean_df %>%
        select(est_group, altitude, activity_id) %>%
        spread(est_group, altitude) %>%
        mutate(segment_vertgain = END-START) %>%
        filter(!(is.na(END))) %>%
        filter(!(is.na(START))) %>%
        select(activity_id, segment_vertgain)
      
      new_segments_list2[[x]] <- link_with_correct_segments %>%
        select(rowid, race, stage, year, class, date,
               start_lat, end_lat, start_long, end_long,
               start_prior, end_next, creation_type,
               original_segment) %>%
        filter(rowid == x) %>%
        cbind(distance_of_new_segment) %>%
        inner_join(alt_of_new_segment, by = c("activity_id")) %>%
        inner_join(time_of_new_segment, by = c("activity_id")) %>%
        inner_join(position_of_new_segment, by = c("activity_id")) %>%
        inner_join(power_of_new_segment, by = c("activity_id")) %>%
        mutate(segment_speed_kmh = (segment_distance/segment_time)/1000*3600,
               segment_gradient = segment_vertgain / segment_distance,
               Power = ifelse(is.nan(Power), NA, Power))
      
    }
  }
  
  if(nrow(bind_rows(new_segments_list2)) == 0) {} else {
    
    all_new_segments <- bind_rows(new_segments_list2) %>%
      mutate(should_be_distance = (end_next - start_prior)*1000) %>%
      
      mutate(ratio = abs(1-(should_be_distance/segment_distance)))
    
    #
    
    dbWriteTable(con, "strava_new_segment_creation_climbs", all_new_segments, append = TRUE, row.names = F)
    
    rm(new_segments_list2)
    rm(all_new_segments)
    rm(created_segments)
    rm(new_segments_list)
    rm(clean_df)
    rm(df)
    rm(res_telem)
    
    gc()
    
  }
  
  print(paste0(R," - ", S, " - ", g))
  
  tictoc::toc()
  
}
