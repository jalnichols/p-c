library(tidyverse)
library(DBI)

dbDisconnect(con)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      port = 5432,
                      host = 'localhost',
                      dbname = "cycling",
                      user = "postgres",
                      password = "braves")

#

all_stage_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE year > 2015 AND time_trial = 1") %>%
  
  mutate(date = as.Date(date))

#

all_race_activities <- dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Distance','AvgElapsed Time')") %>% 
  rename(DATE = date, VALUE = value, PCS = pcs, Stat = stat) %>%
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
                        WHERE year > 2015 AND time_trial = 1") %>%
               
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
  
  filter(time_trial == 1)

#

telemetry_available <- all_race_activities %>%
  
  inner_join(
    
    fs::dir_info('C:/Users/Jake Nichols/Documents/Old D Drive/STRAVA_JSON/') %>%
      select(path, birth_time) %>%
      filter(birth_time > as.Date('2022-01-01')) %>%
      mutate(activity_id = str_replace(path, 'C:/Users/Jake Nichols/Documents/Old D Drive/STRAVA_JSON/strava-activity-id-', ''),
             activity_id = str_replace(activity_id, ".rds", "")), by = c("activity_id")) %>%
  
  group_by(stage, race, year, class, date) %>%
  filter(n() >= 0) %>%
  ungroup()


for_creating_segments_from_strava <- telemetry_available %>%
  filter(rnk < 200) %>%
  mutate(race_time = as.numeric(lubridate::hms(avg_elapsed_time)),
         race_time = ifelse(is.na(race_time), as.numeric(lubridate::ms(avg_elapsed_time)), race_time)) %>%
  
  filter(race_time < 40000) %>%
  
  # not good matches
  filter(!activity_id %in% c("5028077839", "5063406337", "5125903648", "5485415918", "5026068681",
                             "5062666498", "5489444684", "5026221808", '2628306420', '2189109844',
                             '2628945164', '2624417915', '2628945164', '2722684158', '2770112067',
                             '2298569287', '2164552486', '2152995982', '2291747237', '2241381376',
                             '2768142025', '2686351242', '2428741219', '2339987833', '2575788478',
                             '2297631533', '2287342940', '4825833669', '2389816951', '5125549501', 
                             '2276954758')) %>%
  
  # no latlong
  filter(!activity_id %in% c('3959363352', '2449080492', '2504882299', '3125334148', '3125628754',
                            '2523926226', '2756555271', '2166857555', '4146300103', '4155751389',
                            '2429029764', '2713278229', '2111987912', '4110511812', '4245331954', 
                            '2235064421', '2676565644', '484980645', '613087610', '1223314923', 
                            '1023685655', '1231868696', '1642259618', '2379374531', '2579015868', 
                            '1623802283', '3882937712', '1000835949', '1016277906', '1795417686', 
                            '3881149473', '1760429870', '624973825', '686486696', '497252048', 
                            '4996034731', '5153085832', '5262823880', '5399779778', '4917637318',
                            '5261922002', '3959286671', '2167001861', '2430197470', '4145117285',
                            '2676716600', '3125264701', '1016517397', '686106093', '4996089905',
                            '2676045157', '2167125460', '3125309311', '686106076', '4995956892')) %>%
  
  # jumps in location
  filter(!activity_id %in% c("5658112675")) %>%

  # hertz not 1 second
  filter(!activity_id %in% c("2318969854", "4033241753", "5197024366", "3996881884", "2151659319", 
                             "2441761614", "2441881914", "2133574134", "3166204221", "2306421058", 
                             "2727955167", "4106654876", "2782052841", "4121562143", "4908379193", 
                             "3084639620", "3153982167", "2294258334", "5636523441", "2252632293", 
                             "2255063928", "2516848585", "2428606012", "4185892355", "2766460126", 
                             "2598914995", "2174416916", "2757040161", "3022528034", "2792708678", 
                             "5465011150", "3218342006", "2779809662", "3973733606", "2121907474",
                             "2701949096", "2704942109", "5904099350", "2130297851", "2694704894", 
                             "3062462837", "3063462828", "2366686993", "2279605925", "2393310836",
                             "2436242885", "2562202630", "3090053986", "2704910601", "2449688543", 
                             "2781225905", "2130359211", "2249437153", "4091358687", "3055396673", 
                             "3049236324", "5306839117", "2756865152", "2428657855", "3100165626", 
                             "2588105178", "2783183609", "5912353762", "5915208471", "2536051344")) %>%
  
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
  
  filter(!(stage == 1 & race == 'tour de romandie' & year == 2019)) %>%
  filter(!(stage == 4 & race == 'voo-tour de wallonie' & year == 2019))

#

RRR <- for_creating_segments_from_strava %>%
  mutate(date = as.Date(date),
         stage = as.character(stage)) %>%
  left_join(all_stage_data %>%
              filter(time_trial == 1) %>%
              select(stage, race, year, class, date, bunch_sprint) %>%
              unique(), by = c("race", "stage", "year", "date", "class")) %>%
  
  filter(bunch_sprint == 0) %>%
  
  anti_join(dbGetQuery(con, "SELECT DISTINCT stage, race, year, activity_id FROM time_trial_directions"))

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
  
  correct_stage_info <- RRR %>%
    
    filter(race == R &
             stage == S &
             year == Y) %>%
    select(activity_id) %>%
    unique()
  
  # bring in Power File
  
  ACTIVITY <- correct_stage_info$activity_id[[1]]
  
  data_lists <- read_rds(paste0("C:/Users/Jake Nichols/Documents/Old D Drive/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
  
  hertz = max(data_lists$time) / length(data_lists$time)
  
  # clean this up before writing to DB
  
  lat <- data_lists$latlng[,1]
  long <- data_lists$latlng[,2]
  
  if(is.null(long)) {
    print("missing")
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
      filter(spd > 2.75) %>%
      mutate(rk = rank(distance, ties.method = "first"))
    
    too_high_speed = route %>%
      filter((max(distance)-distance) <= 5000) %>%
      summarize(n = mean(spd >= 90, na.rm = T)) %>%
      .[[1]]
    
    route <- route %>%
      filter(rk %% 5 == 0)
    
    #
    
    direction_route <- route %>%
      
      mutate(prior_lat = lag(lat),
             prior_long = lag(long),
             next_lat = lead(lat),
             next_long = lead(long)) %>%
      mutate(x_delta = long - prior_long,
             y_delta = lat - prior_lat) %>%
      
      mutate(prior_dir = ifelse(lat > prior_lat, "N", "S"),
             next_dir = ifelse(next_lat > lat, "N", "S")) %>%
      
      mutate(angle = atan((next_long - long)/(next_lat - lat)) * 57.2958,
             prior_angle = atan((long - prior_long) / (lat - prior_lat)) * 57.2958,
             diff_angle = abs(angle-prior_angle),
             actual_angle_diff = ifelse(next_dir == prior_dir, diff_angle, 180-diff_angle))
    
    #
    
    tt_route <- direction_route %>% 
      mutate(stage = S, race = R, year = Y) %>%
      select(-x_delta, -y_delta, -prior_lat, -prior_long, -next_lat, -next_long, 
             -rk, -prior_dir, -next_dir, -angle, -prior_angle, -diff_angle,
             -altitude) %>%
      
      mutate(hertz = hertz,
             too_high_speed = too_high_speed)
    
    dbWriteTable(con, "time_trial_directions", tt_route, append = TRUE, row.names = FALSE)
    
    print(R)
    print(S)
    print(Y)
    
  }
}

#
#
#

tt_dirs <- dbGetQuery(con, "SELECT * FROM time_trial_directions") %>%
  group_by(activity_id, stage, race, year) %>%
  summarize(A = mean(actual_angle_diff, na.rm = T), 
            distance = max(distance, na.rm = T))


# determine direction

sf_route <- route %>%
  
  sf::st_as_sf(coords = c("long", "lat"))

ggplot()+
  geom_sf(data = sf_route, aes(color = distance))+
  scale_color_viridis_c(option = "A")

#

route <- route %>%
  mutate(rk = rank(distance, ties.method = "first")) %>%
  filter(rk %% 5 == 0)

#

direction_route <- route %>%
  
  mutate(prior_lat = lag(lat),
         prior_long = lag(long),
         next_lat = lead(lat),
         next_long = lead(long)) %>%
  
  mutate(delta_long = next_long - long,
         delta_lat = next_lat - lat) %>%
  
  mutate(prior_slope = (lat - prior_lat)/(long - prior_long),
         next_slope = (next_lat - lat)/(next_long - long),
         prior_dir = ifelse(lat > prior_lat, "N", "S"),
         next_dir = ifelse(next_lat > lat, "N", "S")) %>%
  
  mutate(angle = atan((next_long - long)/(next_lat - lat)) * 57.2958,
         prior_angle = atan((long - prior_long) / (lat - prior_lat)) * 57.2958,
         diff_angle = abs(angle-prior_angle),
         actual_angle_diff = ifelse(next_dir == prior_dir, diff_angle, 180-diff_angle))

#

direction_route %>%
  mutate(long = 0, lat = 0) %>% 
  mutate(next_long = delta_long, next_lat = delta_lat) %>% 
  
  ggplot(aes(x = long, y = lat, xend = next_long, yend = next_lat, color = angle))+
  geom_segment()

#

direction_route %>% 
  ggplot(aes(x = next_slope, fill = next_dir))+
  geom_histogram(binwidth = 0.1)+
  coord_cartesian(xlim = c(-10,10))

#

direction_route %>%
  mutate(change = next_slope - prior_slope,
         dir_change = ifelse(prior_dir == next_dir, 1, 0)) %>%
  ggplot(aes(x = change, fill = dir_change))+
  geom_histogram(binwidth = 0.05)+
  coord_cartesian(xlim = c(-1.5,1.5))

#

direction_route %>%
  
  ggplot(aes(x = long, xend = next_long, y = lat, yend = next_lat, color = actual_angle_diff))+
  geom_segment()+
  geom_point()+
  scale_color_viridis_c(option = "A")+
  
  coord_cartesian(xlim = c(-0.101,-0.088),
                  ylim = c(38.685,38.695))

#
#
#
#

direction_output_list <- vector("list", nrow(RRR))

for(g in 1:length(RRR$race)) {
  
  tictoc::tic()
  
  R=RRR$race[[g]]
  S=RRR$stage[[g]]
  Y=RRR$year[[g]]
  
  #
  
  correct_stage_info <- RRR %>%
    
    filter(race == R &
             stage == S &
             year == Y) %>%
    select(activity_id) %>%
    unique()
  
  # bring in Power File
  
  ACTIVITY <- correct_stage_info$activity_id[[1]]
  
  data_lists <- read_rds(paste0("D:/Jake/Documents/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
  
  hertz = max(data_lists$time) / length(data_lists$time)
  
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
    filter(spd > 2.75) %>%
    mutate(rk = rank(distance, ties.method = "first"))
  
  too_high_speed = route %>%
    filter((max(distance)-distance) <= 5000) %>%
    summarize(n = mean(spd >= 90, na.rm = T)) %>%
    .[[1]]
  
  route <- route %>%
    filter(rk %% 5 == 0)
  
  #
  
  direction_route <- route %>%
    
    mutate(prior_lat = lag(lat),
           prior_long = lag(long),
           next_lat = lead(lat),
           next_long = lead(long)) %>%
    mutate(x_delta = long - prior_long,
           y_delta = lat - prior_lat) %>%
    
    mutate(prior_dir = ifelse(lat > prior_lat, "N", "S"),
           next_dir = ifelse(next_lat > lat, "N", "S")) %>%
    
    mutate(angle = atan((next_long - long)/(next_lat - lat)) * 57.2958,
           prior_angle = atan((long - prior_long) / (lat - prior_lat)) * 57.2958,
           diff_angle = abs(angle-prior_angle),
           actual_angle_diff = ifelse(next_dir == prior_dir, diff_angle, 180-diff_angle))
  
  #
  
  last_5_km <- direction_route %>% 
    filter((max(distance) - distance) < 5000) %>%
    mutate(stage = S, race = R, year = Y) %>%
    select(-x_delta, -y_delta, -prior_lat, -prior_long, -next_lat, -next_long, 
           -rk, -prior_dir, -next_dir, -angle, -prior_angle, -diff_angle,
           -altitude) %>%
    
    mutate(hertz = hertz,
           too_high_speed = too_high_speed)
  
  dbWriteTable(con, "final_5km_directions", last_5_km, append = TRUE, row.names = FALSE)
  
  #
  
  #direction_output_list[[g]] <- direction_route
}

#
#

direction_route <- bind_rows(direction_output_list) %>%
  
  inner_join(segments_from_fr_data %>%
               select(activity_id, stage, race, year) %>%
               unique()) %>%
  
  group_by(race, stage) %>%
  summarize(mean(actual_angle_diff, na.rm = T))

#

ggplot(directions_sprints %>% filter(race == "tour de l'eurometropole" & stage == 1 & year == 2019), 
       aes(x = long, y = lat, color = actual_angle_diff >= 30))+
  
  geom_path(color = "black")+    geom_point()+
  labs(title = paste0(R, "-", S, "-", Y, "-final 5km"))+
  guides(color = FALSE)+
  geom_point(data = directions_sprints %>% filter(race == "tour de l'eurometropole" & stage == 1 & year == 2019) %>%
               filter(max(distance) == distance), size = 4, color = "red")+
  coord_fixed()



#
#
#
#

options(dplyr.summarise.inform = FALSE)

for(g in 178:length(RRR$race)) {
  
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
    
    rowid_to_column() %>%
    
    filter(end_next > start_prior)
  
  #
  #
  #
  #
  #
  
  df <- route %>%
  
    rename(latitude = lat,
           longitude = long) %>%
    
    mutate(rk = rank(distance, ties.method = "first")) %>%
    filter(rk %% 2 == 0) %>%
    
    mutate(time_delta = time - lag(time),
           dist_delta = distance - lag(distance),
           alt_delta = altitude - lag(altitude)) %>%
    
    mutate(speed = dist_delta/time_delta,
           gradient = alt_delta / dist_delta) %>%
    
    rowid_to_column() %>%
    
    mutate(prior_lat = lag(latitude),
           prior_long = lag(longitude),
           next_lat = lead(latitude),
           next_long = lead(longitude)) %>%
    
    mutate(x_delta = longitude - prior_long,
           y_delta = latitude - prior_lat) %>%
    
    mutate(prior_dir = ifelse(latitude >= prior_lat, "N", "S"),
           next_dir = ifelse(next_lat >= latitude, "N", "S")) %>%
    
    mutate(angle = atan((next_long - longitude)/(next_lat - latitude)) * 57.2958,
           prior_angle = atan((longitude - prior_long) / (latitude - prior_lat)) * 57.2958,
           diff_angle = abs(angle-prior_angle),
           actual_angle_diff = ifelse(next_dir == prior_dir, diff_angle, 180-diff_angle))

  df <- df %>%
    
    cbind(tibble(start_lat = vstart_lat,
                 start_long = vstart_long,
                 end_lat = vfinish_lat,
                 end_long = vfinish_long
    )) %>%
    
    mutate(distance_left = max(distance) - distance,
           distance_gone = distance - min(distance)) %>%
    
    select(-start_lat, -start_long,
           -end_lat, -end_long)
  
  if(nrow(df) > 0) {
    
    #
    # run thru each segment to match
    #
    
    new_segments_list <- vector("list", length(link_with_correct_segments$rowid))
    
    for(x in 1:length(link_with_correct_segments$rowid)) {
      
      ind_pts <- df %>%
        
        select(-rowid) %>%
        
        cbind(link_with_correct_segments %>%
                select(rowid, race, stage, year, class, date,
                       start_lat, end_lat, start_long, end_long,
                       start_prior, end_next, creation_type) %>%
                filter(rowid == x)) %>%
        
        filter((distance/1000) >= start_prior & (distance/1000) < end_next)
      
      clean_df <- ind_pts %>%
        group_by(rowid, start_prior, end_next, stage, race, year, class, date, start_lat, end_lat, start_long, end_long,
                 creation_type) %>%
        summarize(average_angle = mean(actual_angle_diff, na.rm = T),
                  extreme = mean(actual_angle_diff > 30, na.rm = T),
                  gradient = mean(gradient, na.rm = T),
                  x_delta = sum(x_delta, na.rm = T),
                  y_delta = sum(y_delta, na.rm = T),
                  points = n()) %>%
        ungroup()
      
      #if((clean_df$gradient > 0.03 | clean_df$gradient < -0.04) & (clean_df$end_next-clean_df$start_prior) > 1) {
        
        #dbWriteTable(con, 
        #             "direction_angle_points",
        #             ind_pts %>% select(activity_id, longitude, latitude, distance, actual_angle_diff, rowid),
        #             append = TRUE, row.names = FALSE)
        
      #}
      
      new_segments_list[[x]] <- clean_df
      
    }
    
    segments_with_direction_angle <- bind_rows(new_segments_list)
    
    dbWriteTable(con, "direction_angle_segments", segments_with_direction_angle, append = TRUE, row.names = FALSE)
    
    print(g)
    
  }
  
}

#
#
#
#

examine_dir_segs <- dbGetQuery(con, "SELECT * FROM direction_angle_segments")
