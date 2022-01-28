

library(tidyverse)
library(rvest)
library(RMySQL)

options(dplyr.summarise.inform = FALSE)
options(tidyverse.quiet = TRUE)

dbDisconnect(con)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

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
                        WHERE year > 2018") %>%
               
               mutate(date = as.Date(date)) %>%
               mutate(date = as.Date(date, origin = '1970-01-01')) %>%
               unique(), by = c("date", "pcs" = "rider")) %>% 
  
  # if two results exist for same day matching distance, it's probably a recon and TT which
  # means drop the lower watts
  
  # also, many riders include distance outside the TT as part of their strava activity
  # so maybe accept any riders +/- 10 km? or maybe we just can't get accurate TT data
  
  mutate(distance = distance * 1.609) %>% 
  filter((distance / length) > 0.50) %>%
  filter((distance / length) < 1.20) %>%
  
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
  filter(n() >= 15) %>%
  ungroup()

#

for_creating_segments_from_strava <- telemetry_available %>%
  select(-avg_alt, -missing_profile_data, -total_vert_gain, -sof_limit, -success, -leader_rating, -points_finish) %>%
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
  
  # big challenge is getting the actual length of the race correct??
  mutate(length = speed * (total_seconds/3600)) %>%
  
  group_by(stage, race, year, class, date, win_seconds) %>% 
  mutate(abs_time = abs(race_time - total_seconds),
         abs_dist = abs(distance - length),
         pct_time = abs_time/60,
         pct_dist = abs_dist,
         m = ((3*pct_time)+pct_dist)/4,
         hm = 2 / ((1/pct_time)+(1/pct_dist))) %>% 
  filter(rank(m, ties.method = "first") == 1) %>%
  ungroup() %>% 
  
  select(pcs, distance, race, stage, class, date, year, length, activity_id, stage_type) %>%
  mutate(creation_type = "TIME") %>%
  anti_join(dbGetQuery(con, "SELECT DISTINCT stage, race, year FROM strava_stage_distance_chunks")) %>%
  
  arrange(desc(class %in% c("1.UWT", "2.UWT", "WC", "Olympics")), desc(date)) %>%
  
  filter(!(stage == 1 & race == 'tour de romandie' & year == 2019)) %>%
  filter(!(stage == 4 & race == 'voo-tour de wallonie' & year == 2019 ))

#
#
#
#

for(c in 1:length(for_creating_segments_from_strava$activity_id)) {
  
  tictoc::tic()
  
  ACTIVITY_PRIME = for_creating_segments_from_strava$activity_id[[c]]
  R = for_creating_segments_from_strava$race[[c]]
  S = for_creating_segments_from_strava$stage[[c]]
  Y = for_creating_segments_from_strava$year[[c]]
  
  # bring in ideal strava race data
  
  data_lists <- read_rds(paste0("D:/Jake/Documents/STRAVA_JSON/strava-activity-id-", ACTIVITY_PRIME, ".rds"))
  
  # clean this up before writing to DB
  
  lat <- data_lists$latlng[,1]
  long <- data_lists$latlng[,2]
  
  route <- cbind(
    
    altitude = data_lists[["altitude"]],
    distance = data_lists[["distance"]],
    time = data_lists[["time"]],
    activity_id = ACTIVITY_PRIME
    
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
    
    arrange(distance) %>%
    filter(spd > 0)
  
  # extract finish and start data
  
  vfinish_lat <- route$lat[[length(route$distance)]]
  vfinish_long <- route$long[[length(route$distance)]]
  vfinish_dist <- route$distance[[length(route$distance)]]
  
  vstart_lat <- route$lat[[1]]
  vstart_long <- route$long[[1]]
  
  #
  
  bring_in_telem <- telemetry_available %>%
    filter(race == R & stage == S & year == Y)
  
  # bring in all telem data
  
  telem_list <- vector("list", length(bring_in_telem$activity_id))
  
  for(b in 1:length(bring_in_telem$activity_id)) {
    
    tictoc::tic()
    
    ACTIVITY <- bring_in_telem$activity_id[[b]]
    
    data_lists <- read_rds(paste0("D:/Jake/Documents/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
    
    # clean this up before writing to DB
    
    lat <- data_lists$latlng[,1]
    long <- data_lists$latlng[,2]
    
    if(length(lat) == 0) {
      
    } else {
      
      if("watts" %in% names(data_lists)) {
        
        df <- cbind(
          
          altitude = data_lists[["altitude"]],
          distance = data_lists[["distance"]],
          time = data_lists[["time"]],
          watts = data_lists[["watts"]],
          activity_id = ACTIVITY
          
        )
      } else {
        df <- cbind(
          
          altitude = data_lists[["altitude"]],
          distance = data_lists[["distance"]],
          time = data_lists[["time"]],
          watts = NA,
          activity_id = ACTIVITY
          
        )
      }
      
      
      telem_list[[b]] <- df %>%
        
        as_tibble() %>%
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
               gradient = alt_delta / dist_delta,
               coord_delta = 0.3048 * sqrt(((lat_delta*364000)^2)+((long_delta*288000)^2))) %>%
        rowid_to_column() %>%
        
        filter(!is.na(time_delta))
      
    }
    
  }
  
  #
  #
  #
  
  all_telem_df <- bind_rows(telem_list) %>%
    
    cbind(tibble(start_lat = vstart_lat,
                 start_long = vstart_long,
                 end_lat = vfinish_lat,
                 end_long = vfinish_long
    )) %>%
    
    mutate(est_start = floor(sqrt((((latitude - start_lat)*364000)^2) + (((longitude - start_long)*288000)^2))/50),
           est_finish = floor(sqrt((((latitude - end_lat)*364000)^2) + (((longitude - end_long)*288000)^2))/50)) %>%
    
    group_by(activity_id) %>%
    mutate(speed_window = (lag(speed)+lead(speed)+speed)/3,
           speed_window = ifelse(is.na(speed_window), speed, speed_window)) %>%
    mutate(est_finish = ifelse(speed_window > 2.75, est_finish, 999)) %>%
    mutate(est_group = ifelse(est_start == min(est_start), "START", 
                              ifelse(est_finish == min(est_finish), "END", "NONE"))) %>%
    ungroup() %>%
    
    # this calculates the difference between the actual distance and expected distance of the segment
    mutate(padding = ifelse(est_group == "START", (distance/1000) - 0, 
                            ifelse(est_group == "END", (distance - vfinish_dist)/1000, NA))) %>%
    
    group_by(activity_id, est_group) %>%
    mutate(estgroup_rowid = ifelse(is.na(padding), 0, min(abs(padding), na.rm = T)),
           estgroup_rowid = ifelse(abs(padding) == estgroup_rowid, rowid, NA)) %>%
    ungroup() %>%
    
    group_by(activity_id) %>%
    mutate(finish_rowid = ifelse(est_group == "END", estgroup_rowid, NA),
           start_rowid = ifelse(est_group == "START", estgroup_rowid, NA),
           finish_rowid = mean(finish_rowid, na.rm = T),
           start_rowid = mean(start_rowid, na.rm = T)) %>%
    ungroup() %>%
    
    select(-estgroup_rowid, -padding) %>%
    
    filter(rowid >= start_rowid & rowid <= finish_rowid) %>%
    
    mutate(start_distance = ifelse(start_rowid == rowid, distance, NA),
           finish_distance = ifelse(finish_rowid == rowid, distance, NA)) %>%
    
    group_by(activity_id) %>%
    mutate(start_distance = mean(start_distance, na.rm = T),
           finish_distance = mean(finish_distance, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(distance_left = finish_distance - distance,
           distance_gone = distance - start_distance) %>%
    
    inner_join(bring_in_telem %>% select(rider=pcs, activity_id, rnk, team), by = c("activity_id")) %>%
    
    left_join(
      
      dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
        
        mutate(rider = str_to_title(rider)) %>%
        filter(!is.na(weight)) %>%
        group_by(rider) %>%
        summarize(weight = median(weight)) %>%
        ungroup(), by = c("rider"))
 
  # save data
  
  furthest_distance_left <- all_telem_df %>%
    summarize(max(distance_left, na.rm = T)) %>%
    .[[1]]
  
  breakpts <- c(999999, seq(1000, furthest_distance_left, 1000), 750, 500, 250)
  
  for(w in breakpts) {
    
    all_telem_df %>% 
      
      filter(distance_left <= w & distance_left >= 0) %>%
      
      group_by(activity_id, rider, rnk) %>%
      mutate(start_rowid = min(rowid), 
             end_rowid = max(rowid),
             start_alt = ifelse(start_rowid == rowid, altitude, NA), 
             end_alt = ifelse(end_rowid == rowid, altitude, NA)) %>% 
      summarize(distance = max(distance)-min(distance), 
                time = max(time) - min(time), 
                vg = max(end_alt, na.rm = T) - min(start_alt, na.rm = T),
                avg_power = mean(watts/weight, na.rm = T),
                lat = median(latitude),
                long = median(longitude)) %>%
      ungroup() %>% 
      
      mutate(speed = distance / time, 
             gradient = vg/distance,
             lat_delta = abs(lat-median(lat)),
             long_delta = abs(long-median(long)),
             coord_delta = 0.3048 * sqrt(((lat_delta*364000)^2)+((long_delta*288000)^2))) %>%
      select(-lat_delta, -long_delta, -lat, -long, -gradient) %>%
      mutate(type = "distance_left",
             amount = w,
             stage = S,
             race = R,
             year = Y) -> check
    
    dbWriteTable(con, "strava_stage_distance_chunks", check, append = TRUE, row.names = FALSE)
    
  }
  
  #breakpts <- c(5000, 10000, 20000, 30000, 40000, 50000, 100000)
  
  #for(w in breakpts) {
    
    #all_telem_df %>% 
      
      #filter(distance_gone <= w & distance_gone >= 0) %>%
      
      #group_by(activity_id, rider, rnk) %>%
      #mutate(start_rowid = min(rowid), 
      #       end_rowid = max(rowid),
      #       start_alt = ifelse(start_rowid == rowid, altitude, NA), 
      #       end_alt = ifelse(end_rowid == rowid, altitude, NA)) %>% 
      #summarize(distance = max(distance)-min(distance), 
      #          time = max(time) - min(time), 
      #          vg = max(end_alt, na.rm = T) - min(start_alt, na.rm = T),
      #          avg_power = mean(watts/weight, na.rm = T),
      #          lat = median(latitude),
      #          long = median(longitude)) %>%
      #ungroup() %>% 
      
      #mutate(speed = distance / time, 
      #       gradient = vg/distance,
      #       lat_delta = abs(lat-median(lat)),
      #       long_delta = abs(long-median(long)),
      #       coord_delta = 0.3048 * sqrt(((lat_delta*364000)^2)+((long_delta*288000)^2))) %>%
      #select(-lat_delta, -long_delta, -lat, -long, -gradient) %>%
      #mutate(type = "distance_gone",
      #       amount = w,
      #       stage = S,
      #       race = R,
      #       year = Y) -> check
    
    #dbWriteTable(con, "strava_stage_distance_chunks", check, append = TRUE, row.names = FALSE)
    
  #}

  #
  
  print(paste0(R,"-",S,"-",Y,"-", c))
  
  tictoc::toc()
  
}

#

all_telem_df %>% 
  filter(distance_left <= 2500) %>%
  
  ggplot(aes(x = longitude, y = latitude, group = activity_id))+
  geom_path(alpha=0.33, size = 0.75)+
  geom_path(data = all_telem_df %>% filter(activity_id == 6088381248) %>% filter(distance_left <= 2500), 
            aes(x = longitude, y = latitude), color = "red", size = 1.5)+
  geom_point(data = all_telem_df %>% filter(distance_left == 0), shape = 21, fill = "white")

#
#
#
#
#
#

stage_distance_chunks <- dbReadTable(con, "strava_stage_distance_chunks") %>%
  
  inner_join(telemetry_available %>% select(activity_id, team, class, date, bunch_sprint, gain_1st, total_seconds,
                                            length, pred_climb_difficulty, one_day_race), by = c("activity_id"))

#

stage_distance_chunks_full <- dbGetQuery(con, "SELECT * FROM strava_stage_distance_chunks WHERE amount = 999999") %>%
  
  inner_join(telemetry_available %>% select(activity_id, team, class, date, bunch_sprint, gain_1st, total_seconds,
                                            length, pred_climb_difficulty, one_day_race), by = c("activity_id")) %>%

  filter(type == "distance_left" & amount == 999999 & ((coord_delta*0.3048)/amount) <= 0.033) %>%
  
  filter(((distance/1000)/length) > 0.9) %>%
  
  group_by(race, year, stage, class, date, length) %>% 
  summarize(avg_power = median(avg_power, na.rm = T),
            distance = median(distance, na.rm = T),
            time = median(time, na.rm = T)) %>% 
  ungroup() %>%
  mutate(kj_kg = avg_power * time / 1000) %>%
  
  filter(length > 35)


#

stage_distance_chunks_expanded <- stage_distance_chunks %>%
  
  filter(race %in% c(
                     "milano-sanremo",
                     "il lombardia", "ronde van vlaanderen - tour des flandres me",
                     "ronde van vlaanderen - tour des flandres")) %>%
  
  group_by(activity_id) %>%
  filter(mean(is.na(avg_power)) %in% c(0,1)) %>%
  ungroup() %>%
  
  filter(type == "distance_left" & ((coord_delta*0.3048)/amount) <= 0.033 & amount != 999999) %>%
  
  mutate(join_amount = ifelse(amount == 1000, 500, amount - 1000)) %>%
  
  select(-vg, -coord_delta, -speed) %>%
  
  inner_join(stage_distance_chunks %>%
               
               filter(race %in% c(
                 "milano-sanremo",
                 "il lombardia", "ronde van vlaanderen - tour des flandres me",
                 "ronde van vlaanderen - tour des flandres")) %>%
               
               group_by(activity_id) %>%
               filter(mean(is.na(avg_power)) %in% c(0,1)) %>%
               ungroup() %>%
               
               select(type, amount, activity_id, distance, time, avg_power, coord_delta) %>%
               filter(type == "distance_left" & ((coord_delta*0.3048)/amount) <= 0.033 & amount != 999999) %>%
               select(-coord_delta), by = c("activity_id", "type", "join_amount" = "amount")) %>%
  
  mutate(amountS = paste0(amount, "-", join_amount)) %>%
  
  select(-join_amount) %>%
  
  filter((is.na(avg_power.x) & is.na(avg_power.y)) | ((!is.na(avg_power.x) & !is.na(avg_power.y)))) %>%
  filter(distance.x != distance.y) %>%
  filter(time.x != time.y) %>%
  
  mutate(timeS = time.x - time.y,
         distanceS = distance.x - distance.y,
         avg_powerS = ((distance.x * avg_power.x)-(distance.y * avg_power.y))/distanceS) %>%
  
  select(-distance.x, -distance.y, -time.x, -time.y, -avg_power.x, -avg_power.y) %>%
  
  mutate(speedS = distanceS / timeS)

#
#
#

stage_distance_chunks_expanded %>% 
  filter(distanceS > 900 & speedS <= 30) %>% 
  
  group_by(amountS, stage, race, year) %>% 
  mutate(rel_power = avg_powerS - mean(avg_powerS, na.rm = T), 
         KM_power = mean(avg_powerS, na.rm = T)) %>%
  ungroup() %>% 
  
  group_by(rider, stage, race, year) %>%
  mutate(rider_rel = rel_power - mean(rel_power, na.rm = T), 
         rider_act_rel = avg_powerS - mean(avg_powerS, na.rm = T)) %>% 
  ungroup() -> specific_race

#

first_25km <- stage_distance_chunks_expanded %>% 
  filter(distanceS > 900 & speedS <= 30 & avg_powerS > 0) %>%
  
  group_by(amount, stage, race, year, class, pred_climb_difficulty, date) %>% 
  summarize(KM_power = median(avg_powerS, na.rm = T),
            riders = sum(!is.na(avg_powerS))) %>%
  ungroup() %>%
  
  group_by(stage, race, year, class, pred_climb_difficulty, date) %>%
  mutate(start = max(amount, na.rm = T)) %>% 
  filter(amount >= (start - 25000)) %>% 
  summarize(KM_power = mean(KM_power, na.rm = T),
            Nriders = mean(riders),
            MAXriders = max(riders)) %>%
  ungroup()

#

#

throughout <- stage_distance_chunks_expanded %>% 
  filter(distanceS > 900 & speedS <= 30 & avg_powerS > 0) %>%
  
  group_by(amount, stage, race, year, class, pred_climb_difficulty, date) %>% 
  summarize(KM_power = median(avg_powerS, na.rm = T),
            riders = sum(!is.na(avg_powerS))) %>%
  ungroup() %>%
  
  group_by(stage, race, year, class, pred_climb_difficulty, date) %>%
  mutate(start = max(amount, na.rm = T), 
         perc_thru = 1-(amount/start)) %>% 
  ungroup() %>% 
  
  group_by(stage, perc_thru, race, class, date) %>%
  summarize(KM_power = mean(KM_power, na.rm = T),
            Nriders = mean(riders),
            MAXriders = max(riders)) %>%
  ungroup() %>% 
  
  group_by(f = floor(perc_thru/0.01), class = case_when(class == "1.HC" ~ "1.Pro",
                                                        class == "2.HC" ~ "2.Pro",
                                                        class %in% c("CC", "WC", "Olympics") ~ "1.UWT",
                                                        TRUE ~ class)) %>%
  summarize(Power = mean(KM_power, na.rm = T), 
            races = n()) %>%
  ungroup()

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
#
#
#
#


# Peak Power Extractor ----------------------------------------------------

for(c in 1:length(for_creating_segments_from_strava$activity_id)) {
  
  tictoc::tic()
  
  ACTIVITY_PRIME = for_creating_segments_from_strava$activity_id[[c]]
  R = for_creating_segments_from_strava$race[[c]]
  S = for_creating_segments_from_strava$stage[[c]]
  Y = for_creating_segments_from_strava$year[[c]]
  
  # bring in ideal strava race data
  
  data_lists <- read_rds(paste0("D:/Jake/Documents/STRAVA_JSON/strava-activity-id-", ACTIVITY_PRIME, ".rds"))
  
  # clean this up before writing to DB
  
  lat <- data_lists$latlng[,1]
  long <- data_lists$latlng[,2]
  
  route <- cbind(
    
    altitude = data_lists[["altitude"]],
    distance = data_lists[["distance"]],
    time = data_lists[["time"]],
    activity_id = ACTIVITY_PRIME
    
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
    
    arrange(distance) %>%
    filter(spd > 0)
  
  # extract finish and start data
  
  vfinish_lat <- route$lat[[length(route$distance)]]
  vfinish_long <- route$long[[length(route$distance)]]
  vfinish_dist <- route$distance[[length(route$distance)]]
  
  vstart_lat <- route$lat[[1]]
  vstart_long <- route$long[[1]]
  
  #
  
  bring_in_telem <- telemetry_available %>%
    filter(race == R & stage == S & year == Y)
  
  # bring in all telem data
  
  telem_list <- vector("list", length(bring_in_telem$activity_id))
  
  for(b in 1:length(bring_in_telem$activity_id)) {
    
    tictoc::tic()
    
    ACTIVITY <- bring_in_telem$activity_id[[b]]
    
    data_lists <- read_rds(paste0("D:/Jake/Documents/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
    
    # clean this up before writing to DB
    
    lat <- data_lists$latlng[,1]
    long <- data_lists$latlng[,2]
    
    if(length(lat) == 0) {
      
    } else {
      
      if("watts" %in% names(data_lists)) {
        
        df <- cbind(
          
          altitude = data_lists[["altitude"]],
          distance = data_lists[["distance"]],
          time = data_lists[["time"]],
          watts = data_lists[["watts"]],
          activity_id = ACTIVITY
          
        )
        
        telem_list[[b]] <- df %>%
          
          as_tibble() %>%
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
                 gradient = alt_delta / dist_delta,
                 coord_delta = 0.3048 * sqrt(((lat_delta*364000)^2)+((long_delta*288000)^2))) %>%
          rowid_to_column() %>%
          
          filter(!is.na(time_delta))
        
      } else {
        # we don't care about people without watts
      }
      
    }
    
  }
  
  #
  #
  #
  
  all_telem_df <- bind_rows(telem_list) %>%
    
    cbind(tibble(start_lat = vstart_lat,
                 start_long = vstart_long,
                 end_lat = vfinish_lat,
                 end_long = vfinish_long
    )) %>%
    
    mutate(est_start = floor(sqrt((((latitude - start_lat)*364000)^2) + (((longitude - start_long)*288000)^2))/50),
           est_finish = floor(sqrt((((latitude - end_lat)*364000)^2) + (((longitude - end_long)*288000)^2))/50)) %>%
    
    group_by(activity_id) %>%
    mutate(speed_window = (lag(speed)+lead(speed)+speed)/3,
           speed_window = ifelse(is.na(speed_window), speed, speed_window)) %>%
    mutate(est_finish = ifelse(speed_window > 2.75, est_finish, 999)) %>%
    mutate(est_group = ifelse(est_start == min(est_start), "START", 
                              ifelse(est_finish == min(est_finish), "END", "NONE"))) %>%
    ungroup() %>%
    
    # this calculates the difference between the actual distance and expected distance of the segment
    mutate(padding = ifelse(est_group == "START", (distance/1000) - 0, 
                            ifelse(est_group == "END", (distance - vfinish_dist)/1000, NA))) %>%
    
    group_by(activity_id, est_group) %>%
    mutate(estgroup_rowid = ifelse(is.na(padding), 0, min(abs(padding), na.rm = T)),
           estgroup_rowid = ifelse(abs(padding) == estgroup_rowid, rowid, NA)) %>%
    ungroup() %>%
    
    group_by(activity_id) %>%
    mutate(finish_rowid = ifelse(est_group == "END", estgroup_rowid, NA),
           start_rowid = ifelse(est_group == "START", estgroup_rowid, NA),
           finish_rowid = mean(finish_rowid, na.rm = T),
           start_rowid = mean(start_rowid, na.rm = T),
           # basically I put this in so on circuit courses where the rider may have started Strava
           # right after real start they don't lose everything up to circuit
           start_rowid = ifelse(start_rowid > 2000, 1, start_rowid)) %>%
    ungroup() %>%
    
    select(-estgroup_rowid, -padding) %>%
    
    filter(rowid >= start_rowid & rowid <= finish_rowid) %>%
    
    mutate(start_distance = ifelse(start_rowid == rowid, distance, NA),
           finish_distance = ifelse(finish_rowid == rowid, distance, NA)) %>%
    
    group_by(activity_id) %>%
    mutate(start_distance = mean(start_distance, na.rm = T),
           invalid = ifelse(is.nan(start_distance), 1, 0),
           start_distance = ifelse(invalid == 1, 0, start_distance),
           finish_distance = mean(finish_distance, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(distance_left = finish_distance - distance,
           distance_gone = distance - start_distance) %>%
    
    inner_join(bring_in_telem %>% select(rider=pcs, activity_id, rnk, team, length), by = c("activity_id"))
  
  #
  
  rolling_speeds <- c(15, 30, 60, 120, 300, 600, 1200, 45, 90, 180, 450, 900, 2400)

  for(P in rolling_speeds) {
    
    rollingdf <- all_telem_df %>% 
      
      filter(!is.na(watts)) %>%
      
      group_by(activity_id) %>%
      mutate(time_correct = ((RcppRoll::roll_max(time, n = P, fill = 0, align = "right")-RcppRoll::roll_min(time,n = P, fill = 0, align = "right")) == P-1)) %>%
      mutate(Power = RcppRoll::roll_mean(watts, n = P, fill = NA, align = "right")) %>%
      filter(time_correct == TRUE) %>%
      ungroup() %>%
      
      mutate(fbin = floor((distance_gone / (length*1000)) / 0.2)*0.2,
             fbin = ifelse(fbin > 0.8, 0.8, fbin)) %>%
      
      group_by(activity_id, fbin) %>%
      filter(rank(desc(Power), ties.method = "first") == 1) %>%
      ungroup() %>%
      
      select(activity_id, latitude, longitude, distance_left, distance_gone, rider, Power, length, fbin) %>%
      mutate(rolling_speed = P)
    
    dbWriteTable(con, "strava_rolling_power_curves", rollingdf, append = TRUE, row.names = FALSE)
    
  }
  
  #
  
  print(paste0(R,"-",S,"-",Y,"-", c))
  
  tictoc::toc()
  
}

#
# 
#

# Bring in the Peak Effort Data -------------------------------------------

rider_date_power_speed_errors <- dbReadTable(con, "power_speed_errors_riderdate") %>%
  mutate(date = as.Date(date))

#

rollingdf <- dbReadTable(con, "strava_rolling_power_curves") %>%
  inner_join(telemetry_available %>%
               select(activity_id, stage, race, year, class, url, date, 
                      pred_climb_difficulty, rnk, bunch_sprint, tm_pos,
                      gain_gc, one_day_race, grand_tour, uphill_finish), by = c('activity_id')) %>%
  
  separate(url, c("j1", "url_race", "j2"), sep = "\\/") %>%
  select(-j1, -j2) %>%
  
  left_join(rider_date_power_speed_errors %>%
              select(-int), by = c("rider", "date")) %>%
  
  left_join(rider_date_power_speed_errors %>%
              filter(is.na(date)) %>%
              select(-int, -date) %>%
              rename(ifmissing = divide_by), by = c("rider")) %>%
  
  mutate(power_adjustment = ifelse(!is.na(divide_by), divide_by,
                                   ifelse(is.na(ifmissing), 1, ifmissing))) %>%
  
  select(-divide_by, -ifmissing) %>%
  
  mutate(adjPower = Power / power_adjustment)

#



#

# Relative Density Plots --------------------------------------------------

# Calculate P ggplot density and extract model

p <- rollingdf %>% 

  filter(rolling_speed %in% c(15,60,300,1200)) %>%
  
  group_by(activity_id, rolling_speed) %>%
  filter(max(Power) == Power) %>%
  mutate(distance_left = mean(distance_left, na.rm = T),
         distance_gone = mean(distance_gone, na.rm = T)) %>%
  ungroup() %>%
  
  select(-fbin) %>%
  
  unique() %>%
  
  mutate(distbin = (distance_gone) / (length*1000),
         distbin = ifelse(distbin > 0.995, 0.995, distbin)) %>%
  
  ggplot(aes(x = distbin, group = rolling_speed, color = as.factor(rolling_speed)))+
  geom_density()

#
     
density_model <- p %>% ggplot_build() %>% .$data %>% .[[1]] %>% 
  select(distbin = x, density = y, group, colour) %>%
  group_by(distbin = floor(distbin/0.005), group, colour) %>%
  summarize(density = mean(density, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(group, colour) %>%
  mutate(density = density/sum(density)) %>%
  ungroup()

# calculate pr specific density plot and extract model

pr <- rollingdf %>% 
  
  #filter(class == "1.UWT" & length > 220) %>%
  #filter(class == "2.1" & pred_climb_difficulty <= 4.5) %>%
  #filter(url_race == 'tour-de-france' & pred_climb_difficulty > 4.5) %>%
  
  filter(class == "NC") %>%
  
  filter(rolling_speed %in% c(15,60,300,1200)) %>%
  
  group_by(activity_id, rolling_speed) %>%
  filter(max(Power) == Power) %>%
  mutate(distance_left = mean(distance_left, na.rm = T),
         distance_gone = mean(distance_gone, na.rm = T)) %>%
  ungroup() %>%
  
  select(-fbin) %>%
  
  unique() %>%
  
  mutate(distbin = (distance_gone) / (length*1000),
         distbin = ifelse(distbin > 0.995, 0.995, distbin)) %>%
  
  ggplot(aes(x = distbin, group = rolling_speed, color = as.factor(rolling_speed)))+
  geom_density()

#

r_density_model <- pr %>% ggplot_build() %>% .$data %>% .[[1]] %>% 
  select(distbin = x, r_density = y, group, colour) %>%
  group_by(distbin = floor(distbin/0.005), group, colour) %>%
  summarize(r_density = mean(r_density, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(group, colour) %>%
  mutate(r_density = r_density/sum(r_density)) %>%
  ungroup()

# bin data and compare at 0.5% bins (0.75 to 1.25 KMs)

rel_density_model <- r_density_model %>%
  full_join(density_model, by = c("distbin", "group", "colour")) %>%
  
  mutate(r_density = ifelse(is.na(r_density), 0, r_density),
         density = ifelse(is.na(density), 0, density)) %>%
  
  mutate(diff = r_density - density)

# Plot difference as rolling bar chart

ggplot(rel_density_model %>%
         mutate(rolling_speed = case_when(group == 1 ~ 15,
                                          group == 2 ~ 60,
                                          group == 3 ~ 300,
                                          group == 4 ~ 1200,
                                          TRUE ~ 0)),
       aes(x = distbin*0.005, y = diff, fill = as.factor(rolling_speed), color = as.factor(rolling_speed)))+
  
  geom_hline(yintercept = 0)+
  geom_col(position = "dodge")+
  
  labs(x = "% thru race",
       y = "Where does peak power occur vs average race?",
       title ="Peak power output Nat'l Champs Road Races",
       subtitle = "Peak power comes early in race")+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~paste0(rolling_speed, " secs"), scales = "free_y")+
  scale_fill_manual(values = c("dark red", "orange", "gold", "blue"), guide = F)+
  scale_color_manual(values = c("dark red", "orange", "gold", "blue"), guide = F)+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(fill = "gray50"),
        strip.text = element_text(color = "white", face = 'bold', size = 14))

#
#
#
#
#

# Sandbox version of the above code ---------------------------------------

p <- rollingdf %>%
  
  filter(rolling_speed %in% c(15,60,300,1200)) %>%
  
  group_by(activity_id, rolling_speed) %>%
  filter(max(Power) == Power) %>%
  mutate(distance_left = mean(distance_left, na.rm = T),
         distance_gone = mean(distance_gone, na.rm = T)) %>%
  ungroup() %>%
  
  select(-fbin) %>%
  
  unique() %>%
  
  mutate(distbin = (distance_gone) / (length*1000),
         distbin = ifelse(distbin > 0.995, 0.995, distbin)) %>%
  
  ggplot(aes(x = distbin, group = rolling_speed, color = as.factor(rolling_speed)))+
  geom_density()

#

density_model <- p %>% ggplot_build() %>% .$data %>% .[[1]] %>% 
  select(distbin = x, density = y, group, colour) %>%
  group_by(distbin = floor(distbin/0.005), group, colour) %>%
  summarize(density = mean(density, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(group, colour) %>%
  mutate(density = density/sum(density)) %>%
  ungroup()

pr <- rollingdf %>% 
  
  #filter(class == "1.UWT" & length > 220) %>%
  #filter(class == "2.1" & pred_climb_difficulty <= 4.5) %>%
  #filter(url_race == 'tour-de-france' & pred_climb_difficulty > 4.5) %>%
  
  filter(tm_pos > 3) %>%
  
  filter(rolling_speed %in% c(15,60,300,1200)) %>%
  
  group_by(activity_id, rolling_speed) %>%
  filter(max(Power) == Power) %>%
  mutate(distance_left = mean(distance_left, na.rm = T),
         distance_gone = mean(distance_gone, na.rm = T)) %>%
  ungroup() %>%
  
  select(-fbin) %>%
  
  unique() %>%
  
  mutate(distbin = (distance_gone) / (length*1000),
         distbin = ifelse(distbin > 0.995, 0.995, distbin)) %>%
  
  ggplot(aes(x = distbin, group = rolling_speed, color = as.factor(rolling_speed)))+
  geom_density()

#

r_density_model <- pr %>% ggplot_build() %>% .$data %>% .[[1]] %>% 
  select(distbin = x, r_density = y, group, colour) %>%
  group_by(distbin = floor(distbin/0.005), group, colour) %>%
  summarize(r_density = mean(r_density, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(group, colour) %>%
  mutate(r_density = r_density/sum(r_density)) %>%
  ungroup()

#

rel_density_model <- r_density_model %>%
  full_join(density_model, by = c("distbin", "group", "colour")) %>%
  
  mutate(r_density = ifelse(is.na(r_density), 0, r_density),
         density = ifelse(is.na(density), 0, density)) %>%
  
  mutate(diff = r_density - density)

# Binning riders at 20% of course bins

rider_20perc_binned <- rel_density_model %>%
  mutate(rolling_speed = case_when(group == 1 ~ 15,
                                   group == 2 ~ 60,
                                   group == 3 ~ 300,
                                   group == 4 ~ 1200,
                                   TRUE ~ 0)) %>%
  
  group_by(f = floor(distbin/40)*20, rolling_speed) %>%
  summarize(rider_density = sum(r_density, na.rm = T),
            avg_density = sum(density, na.rm = T),
            difference = sum(diff, na.rm = T)) %>%
  ungroup()

#

ggplot(rel_density_model %>%
         mutate(rolling_speed = case_when(group == 1 ~ 15,
                                          group == 2 ~ 60,
                                          group == 3 ~ 300,
                                          group == 4 ~ 1200,
                                          TRUE ~ 0)),
       aes(x = distbin*0.005, y = diff, fill = as.factor(rolling_speed), color = as.factor(rolling_speed)))+
  
  geom_hline(yintercept = 0)+
  geom_col(position = "dodge")+
  
  labs(x = "% thru race",
       y = "Where does peak power occur vs competitors?",
       title ="Michael Valgren Peak Power Output in classics",
       subtitle = "")+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~paste0(rolling_speed, " secs"), scales = "free_y")+
  scale_fill_manual(values = c("dark red", "#FF7F27", "#FFC90E", "#3F48CC"), guide = F)+
  scale_color_manual(values = c("dark red", "#FF7F27", "#FFC90E", "#3F48CC"), guide = F)+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(fill = "gray50"),
        strip.text = element_text(color = "white", face = 'bold', size = 14))

#
#
#
#
#

chunk_binned <- rollingdf %>% 
  
  group_by(activity_id, rolling_speed, fbin) %>%
  filter(max(Power, na.rm = T) == Power) %>%
  mutate(distance_left = mean(distance_left),
         distance_gone = mean(distance_gone)) %>%
  ungroup() %>%
  
  group_by(activity_id) %>%
  mutate(RelPower = Power / mean(Power, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(fbin, rolling_speed) %>%
  summarize(RelPower = mean(RelPower, na.rm = T),
            Power = mean(Power / 70, na.rm = T)) %>%
  ungroup()

#

# Calculate Relative Peaks at Race Level ----------------------------------


race_level_power <- rollingdf %>% 
  
  filter(class == "NC") %>%
  
  group_by(activity_id, rolling_speed, fbin) %>%
  filter(max(Power, na.rm = T) == Power) %>%
  mutate(distance_left = mean(distance_left),
         distance_gone = mean(distance_gone)) %>%
  ungroup() %>%
  
  group_by(activity_id) %>%
  mutate(RelPower = Power / mean(Power, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(fbin, rolling_speed, stage, race, year, url_race, class, date, pred_climb_difficulty, length, bunch_sprint) %>%
  summarize(RelPower = mean(RelPower, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(fbin, rolling_speed) %>%
  mutate(x15 = quantile(RelPower, probs = 0.15, na.rm = T), 
         x85 = quantile(RelPower, probs = 0.85, na.rm = T),
         ZSCR = (RelPower - mean(RelPower, na.rm = T)) / sd(RelPower, na.rm = T)) %>%
  ungroup()

#

race_level_power %>% 
  
  filter(class == 'NC') %>%
  
  #filter(url_race %in% c('tour-de-france') & year == 2021 & bunch_sprint == 0) %>% 
  
  #filter(class %in% c("2.1", "2.HC", "2.Pro", "2.UWT", "2.2", "2.Ncup")) %>%
  
  mutate(InterestingDivideStat = ifelse(pred_climb_difficulty >= 4.5, "Med/High Mtns", "Flats/Hills")) %>%
  
  #mutate(InterestingDivideStat = ifelse(length > 225, "Long", "Shorter")) %>%
  
  ggplot(aes(x = rolling_speed, y = ZSCR, color = paste0(fbin*100,"%")))+
  
  geom_hline(yintercept = 0)+
  geom_smooth(se=F, size = 2, method = "lm")+
  scale_x_log10(breaks = c(15,30,60,120,300,600,1200,2400))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "gray85"),
        strip.background = element_rect(fill = "gray50"),
        strip.text = element_text(color = "white", face = 'bold', size = 14))+
  
  scale_color_viridis_d(option = 'plasma', name = '% thru bin')+
  
  facet_wrap(~InterestingDivideStat)+
  
  #facet_wrap(~stage)+
  
  labs(x = "Time Period",
       y = "Power required vs average race (SDs)",
       title = "Power output demanded at RVV vs MSR")

#

# Calculate Rider level relative peak efforts -----------------------------


where_is_rider_peak_power <- rollingdf %>% 
  
  group_by(stage, race, year, class, date, length) %>%
  mutate(implied_length = max(distance_left+distance_gone)) %>%
  ungroup() %>%
  
  group_by(activity_id, rolling_speed) %>%
  filter(max(Power) == Power) %>%
  mutate(distance_left = mean(distance_left, na.rm = T),
         distance_gone = mean(distance_gone, na.rm = T)) %>%
  ungroup() %>%
  
  select(-fbin) %>%
  
  unique() %>% 
  
  mutate(where = (distance_gone/(length*1000))) %>%
  
  group_by(activity_id) %>%
  filter(n_distinct(rolling_speed) == 13) %>%
  mutate(RelPower = Power / mean(Power, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(rider, rolling_speed) %>%
  summarize(MED = median(where, na.rm = T), 
            WT = mean(class %in% c("1.UWT", "2.UWT"), na.rm = T),
            Q1 = mean(where < 0.25, na.rm = T),
            Q2 = mean(where >= 0.25 & where < 0.5, na.rm = T),
            Q3 = mean(where >= 0.5 & where < 0.75, na.rm = T),
            Q4 = mean(where >= 0.75, na.rm = T),
            LAST10K = mean(distance_left <= 10000 & distance_left > 3000, na.rm = T),
            LAST3K = mean(distance_left <= 3000 & distance_left > 1000, na.rm = T),
            FINAL = mean(distance_left <= 1000, na.rm = T),
            RelPower = median(RelPower, na.rm = T),
            Power = median(Power, na.rm = T),
            races = n()) %>%
  ungroup() %>%
  
  group_by(rolling_speed) %>%
  mutate(RelPower25Plus = ifelse(races >= 25, RelPower, NA),
         x15 = quantile(RelPower25Plus, probs = 0.15, na.rm = T), 
         x85 = quantile(RelPower25Plus, probs = 0.85, na.rm = T)) %>% 
  ungroup() %>%
  select(-RelPower25Plus)

#

riders_power_curve <- where_is_rider_peak_power %>%
  filter(races >= 30) %>%
  select(rider, rolling_speed, RelPower) %>%
  spread(rolling_speed, RelPower) %>%
  janitor::clean_names() %>%
  
  mutate(x60vs15 = x60/x15,
         x20vs5 = x1200/x300) %>%
  
  mutate(x60vs15 = x60vs15/mean(x60vs15),
         x20vs5 = x20vs5/mean(x20vs5))

#

where_is_rider_peak_power %>%
  filter(races >= 25) %>%
  
  filter(rider %in% c("Kamna Lennard", "Woods Michael", "Ballerini Davide")) %>%
  
  ggplot()+
  geom_hline(yintercept = 1)+

  geom_ribbon(data = tibble(rolling_speed = c(15,30,45,60,90,120,180,300,450,600,900,1200,2400),
                            MN = c(1.58, 1.30, 1.18, 1.11, 1.04, 0.995, 0.931, 0.859, 0.803, 0.768, 0.721, 0.689, 0.615),
                            MX = c(1.73, 1.39, 1.24, 1.16, 1.07, 1.02, 0.964, 0.905, 0.858, 0.826, 0.779, 0.746, 0.67)),
              aes(x = rolling_speed, ymin = MN, ymax = MX),
              fill = "black", alpha = 0.2)+

  geom_path(aes(x = rolling_speed, y = RelPower, color = rider),
            size=1)+
  geom_point(aes(x = rolling_speed, y = RelPower, color = rider),
            size=2)+
  scale_x_log10(breaks = c(15,30,45,60,90,120,180,300,450,600,900,1200,2400))+
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.6,1.8,0.2))+
  labs(x = "Time Period",
       y = "Relative Power",
       title = "Relative power output by Time Period",
       subtitle = "70% of riders represented by gray outline")+
  theme(panel.grid.minor.x = element_blank())+
  scale_color_manual(values = c("dark red", "#3F48CC", "#FF7F27", "#37B36C", "#FFC90E"))

#
#
#
#
#
#

pca_df <- where_is_rider_peak_power %>%
  filter(races >= 20 & WT >= 0.5) %>%
  
  select(rider, rolling_speed, RelPower) %>%
  spread(rolling_speed, RelPower) %>%
  janitor::clean_names() %>%
  filter(!is.na(x15)) %>%
  filter(!is.na(x600))

#

PCA <- prcomp(pca_df[,2:14], scale = TRUE)

#

summary(PCA)

PCA$rotation

pca_transformed <- pca_df %>%
  cbind(PCA$x)

pca_transformed %>%
  ggplot(aes(x = PC1, y = PC2, label = rider))+
  geom_text()

#
#
#
#
#
#
#


# Calculate Power Curve ---------------------------------------------------

# Need to adjust all powers based on the time vs power model I developed in other script

GAM_PC <- rollingdf %>%
  
  group_by(rider) %>%
  filter(n_distinct(activity_id) >= 25) %>%
  ungroup() %>%
  
  group_by(activity_id, rolling_speed, fbin) %>%
  filter(max(adjPower) == adjPower) %>%
  mutate(distance_gone = mean(distance_gone, na.rm = T)) %>%
  ungroup() %>%
  
  left_join(
    dbGetQuery(con, "SELECT activity_id, stat_value as ath_weight
               FROM strava_activity_power
               WHERE stat_name = 'athlete_weight'") %>%
      filter(ath_weight > 45), by = c("activity_id")
  ) %>%

  mutate(distbin = (distance_gone) / (length*1000),
         distbin = ifelse(distbin > 0.995, 0.995, distbin)) %>%
  
  group_by(rolling_speed, rider) %>%
  mutate(pct_rk = percent_rank(adjPower)) %>%
  ungroup() %>%
  
  filter(pct_rk >= 0.95) %>%
  
  group_by(rolling_speed, rider) %>%
  summarize(maxPower = mean(adjPower, na.rm = T),
            rawPower = mean(Power, na.rm = T),
            ath_weight = mean(ath_weight, na.rm = T)) %>%
  ungroup() 

#

ggplot(GAM_PC %>%
         filter(rider %in% c("Colbrelli Sonny", "Groves Kaden", 
                             "Caruso Damiano", "Sinkeldam Ramon")), 
       aes(x = rolling_speed, y = rawPower/ath_weight, color = rider))+
  
  geom_path(size=1.5)+
  scale_x_log10(breaks = c(15,30,45,60,90,120,180,300,450,600,900,1200,2400))+
  scale_y_log10(breaks = c(5,6,7,8,10,12,16))+
  theme(panel.grid.minor = element_blank())

#
#
#
#


# Race Peak Power Curve ---------------------------------------------------

full_race_peak_PC <- rollingdf %>%
  
  group_by(activity_id, rolling_speed, fbin) %>%
  filter(max(Power) == Power) %>%
  ungroup() %>%
  
  mutate(rider = str_to_title(rider)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)) %>%
      filter(!is.na(weight)) %>%
      group_by(rider) %>%
      summarize(ath_weight = median(weight)) %>%
      ungroup(), by = c("rider")) %>%
  
  mutate(Power = Power/ath_weight) %>%
  filter(!is.na(ath_weight)) %>%
  
  filter(Power < 20) %>%
  
  group_by(rolling_speed, race, stage, year, class, date, length, pred_climb_difficulty) %>%
  mutate(pct_rk = percent_rank(Power)) %>%
  ungroup() %>%
  
  filter(pct_rk >= 0.95) %>%
  
  group_by(rolling_speed, race, stage, year, class, date, length) %>%
  summarize(maxPower = mean(Power, na.rm = T),
            sample = n()) %>%
  ungroup()  %>%
  
  group_by(rolling_speed) %>%
  mutate(ZSCR = (maxPower - mean(maxPower, na.rm = T)) / sd(maxPower)) %>%
  ungroup()

#
#
#
#

top_finishers_in_race_pc <- rollingdf %>%
  
  filter(rnk <= 10) %>%
  
  group_by(activity_id, rolling_speed, fbin) %>%
  filter(max(Power) == Power) %>%
  ungroup() %>%
  
  mutate(rider = str_to_title(rider)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)) %>%
      filter(!is.na(weight)) %>%
      group_by(rider) %>%
      summarize(ath_weight = median(weight)) %>%
      ungroup(), by = c("rider")) %>%
  
  mutate(Power = Power/ath_weight) %>%
  filter(!is.na(ath_weight)) %>%
  
  filter(Power < 20) %>%
  
  group_by(rolling_speed, race, stage, year, class, date, length, pred_climb_difficulty) %>%
  mutate(pct_rk = percent_rank(Power)) %>%
  ungroup() %>%
  
  filter(pct_rk >= 0.9) %>%
  
  group_by(rolling_speed, race, stage, year, class, date, length, pred_climb_difficulty) %>%
  summarize(maxPower = mean(Power, na.rm = T),
            sample = n()) %>%
  ungroup()  %>%
  
  group_by(rolling_speed) %>%
  mutate(ZSCR = (maxPower - mean(maxPower, na.rm = T)) / sd(maxPower)) %>%
  ungroup()
