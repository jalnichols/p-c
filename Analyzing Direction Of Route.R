



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
    filter(spd > 2.75) %>%
    mutate(rk = rank(distance, ties.method = "first")) %>%
    filter(rk %% 5 == 0)
  
  #
  
  direction_route <- route %>%
    
    mutate(prior_lat = lag(lat),
           prior_long = lag(long),
           next_lat = lead(lat),
           next_long = lead(long)) %>%
    
    mutate(prior_dir = ifelse(lat > prior_lat, "N", "S"),
           next_dir = ifelse(next_lat > lat, "N", "S")) %>%
    
    mutate(angle = atan((next_long - long)/(next_lat - lat)) * 57.2958,
           prior_angle = atan((long - prior_long) / (lat - prior_lat)) * 57.2958,
           diff_angle = abs(angle-prior_angle),
           actual_angle_diff = ifelse(next_dir == prior_dir, diff_angle, 180-diff_angle))
  
  direction_output_list[[g]] <- direction_route
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
#
#
#

options(dplyr.summarise.inform = FALSE)

for(g in 1020:length(RRR$race)) {
  
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
                  points = n()) %>%
        ungroup()
      
      if((clean_df$gradient > 0.03 | clean_df$gradient < -0.04) & (clean_df$end_next-clean_df$start_prior) > 1) {
        
        dbWriteTable(con, 
                     "direction_angle_points",
                     ind_pts %>% select(activity_id, longitude, latitude, distance, actual_angle_diff, rowid),
                     append = TRUE, row.names = FALSE)
        
      }
      
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
