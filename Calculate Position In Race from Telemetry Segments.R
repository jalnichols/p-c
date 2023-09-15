
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
# Bring in Strava Activities
#

all_race_activities <- dbGetQuery(con, "SELECT activity_id, pcs, value, stat, date 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Distance')") %>% 
  
  rename(DATE = date, VALUE = value, STAT = stat, PCS = pcs) %>%
  
  # clean up the dates
  mutate(Y = str_sub(DATE, nchar(DATE)-3, nchar(DATE))) %>% 
  separate(DATE, into = c("weekday", "date", "drop"), sep = ",") %>% 
  mutate(date = paste0(str_trim(date),", ", Y)) %>% 
  select(-weekday, -drop, -Y) %>% 
  
  # clean up the stat values
  mutate(VALUE = str_replace(VALUE, "mi", ""), 
         VALUE = as.numeric(VALUE)) %>% 
  
  mutate(date = lubridate::mdy(date)) %>% 
  unique() %>% 
  mutate(Distance = VALUE) %>%
  select(-STAT) %>%
  janitor::clean_names() %>%
  
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
  filter((distance / length) > 0.80) %>%
  filter((distance / length) < 1.20) %>%
  
  filter(time_trial == 0)

#
# Link with telemetry data downloaded and filter to those races with 20+ files
#

telemetry_available <- all_race_activities %>%
  
  inner_join(
    
    fs::dir_info('C:/Users/Jake Nichols/Documents/Old D Drive/STRAVA_JSON/') %>%
      select(path, birth_time) %>%
      mutate(activity_id = str_replace(path, 'C:/Users/Jake Nichols/Documents/Old D Drive/STRAVA_JSON/strava-activity-id-', ''),
             activity_id = str_replace(activity_id, ".rds", "")), by = c("activity_id")) %>%
  
  group_by(stage, race, year, class, date) %>%
  filter(n() >= 15) %>%
  ungroup()

#
# Join with Segments calculated from those power files
#

all_new_segments_across_riders <- dbGetQuery(con, "SELECT * FROM strava_250m_segment_creation
                                             WHERE year = '2022'") %>% 
  filter(creation_type == "TIME") %>%
  select(-creation_type) %>%
  
  inner_join(telemetry_available %>% select(activity_id, rider=pcs, total_seconds, rnk) %>% unique()) %>% 
  
  filter(segment_distance > 0) %>%
  
  group_by(rowid, stage, race, year, date, class) %>%
  mutate(median_distance = median(segment_distance, na.rm = T),
         median_time = median(segment_time, na.rm = T),
         median_gradient = median(segment_gradient, na.rm = T),
         median_speed = mean(segment_speed_kmh, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(ratio = (median_distance/segment_distance),
         spd_ratio = segment_speed_kmh / ((median_distance/1000) / (median_time/3600))) %>%
  
  # in research, anything outside of 60% to 150% in spd ratio is probably either a crash, technical problem, or a data error
  
  mutate(bad_data = ifelse((median_distance < 1500 & (ratio > 1.2 | ratio < 0.8)), 1, 0)) %>%
  
  group_by(stage, race, year, class, rowid, date) %>%
  mutate(not_bad_data_speed = ifelse(bad_data == 1, NA, segment_speed_kmh),
         not_bad_data_speed = mean(not_bad_data_speed, na.rm = T),
         not_bad_data_dist = ifelse(bad_data == 1, NA, segment_distance),
         not_bad_data_dist = mean(not_bad_data_dist, na.rm = T)) %>%
  ungroup() %>%
  
  # what this does is it assigns median speed if your data is bad, but your speed otherwise
  # then it assigns everyone the median distance
  # and then time is calculated off of the median distance * whatever speed
  mutate(new_segment_speed_kmh = ifelse(bad_data == 1, not_bad_data_speed, segment_speed_kmh),
         new_segment_distance = ifelse(bad_data == 1, should_be_distance, should_be_distance),
         new_segment_time = new_segment_distance / ((new_segment_speed_kmh*1000/3600))) %>%

  #filter(segment_speed_kmh <= 120) %>%

  group_by(rowid, stage, race, year, date, class, should_be_distance) %>%
  mutate(rel_speed = segment_speed_kmh / mean(segment_speed_kmh)) %>%
  ungroup()

#
# Filter out races where position has been calculated
#

races_to_generate_position_data <- all_new_segments_across_riders %>%
  
  group_by(activity_id) %>%
  filter(max(bad_data) == 0) %>%
  ungroup() %>%
  
  anti_join(dbGetQuery(con, "SELECT DISTINCT stage, race, year, class, date 
                       FROM strava_position_in_race_250") %>%
              mutate(stage = as.character(stage))) %>%
  
  select(rider, race, stage, year, class, date) %>%
  unique() %>%
  
  group_by(race, stage, year, class, date) %>%
  filter(n() >= 15) %>%
  ungroup() %>%
  
  select(race, stage, year, class, date) %>%
  unique()

#
# Loop through missing races and output to table
#

for(i in 1:length(races_to_generate_position_data$race)) {
  
  R = races_to_generate_position_data$race[[i]]
  S = races_to_generate_position_data$stage[[i]]
  Y = races_to_generate_position_data$year[[i]]
  
  #
  
  segments_to_consider <- all_new_segments_across_riders %>%
    filter(race == R & year == Y & stage == S) %>%
    group_by(activity_id) %>%
    filter(max(bad_data) == 0) %>%
    ungroup()
  
  #
  
  summed_time <- rbind(
    
    segments_to_consider %>%
      
      filter(bad_data == 0) %>%
      select(rowid, stage, race, year, class, date, new_segment_distance = should_be_distance, 
             new_segment_speed_kmh = not_bad_data_speed, segment_gradient = median_gradient, segment_vertgain, 
             start_lat, end_lat, start_long, end_long, start_prior, end_next) %>%
      group_by(rowid) %>%
      mutate(segment_vertgain = median(segment_vertgain)) %>%
      unique() %>%
      
      inner_join(segments_to_consider %>% select(rider, stage, race, year, class, date, activity_id) %>% unique()) %>%
      
      anti_join(segments_to_consider, by = c("rowid", "rider")) %>%
      mutate(new_segment_time = new_segment_distance / ((new_segment_speed_kmh*1000/3600))) %>%
      as_tibble(),
    
    segments_to_consider %>%
      select(-rel_speed, -not_bad_data_speed, -not_bad_data_dist, -bad_data, -spd_ratio, -ratio,
             -median_gradient, -median_speed, -median_time, -median_distance, -should_be_distance,
             -position_distance, -segment_speed_kmh, -segment_time, -segment_distance,
             -segment_time, -ValidPoints, -ValidPower, -Power, -total_seconds)) %>%
    
    arrange(start_prior) %>%
    
    group_by(Segment = rowid, stage, race, year, class, date) %>%
    mutate(best_time = min(new_segment_time, na.rm = T),
           median_time = median(new_segment_time, na.rm = T)) %>%
    ungroup() %>%
    
    group_by(stage, race, year, class, rider, date) %>%
    mutate(behind_median = cumsum(new_segment_time - median_time)) %>%
    ungroup() %>%
    
    group_by(rider, stage, race, year, class, date) %>%
    mutate(Segment_Cnt = n()) %>%
    ungroup() %>%
    
    group_by(Segment, stage, race, year, class, date) %>%
    mutate(vs_median = behind_median - min(behind_median, na.rm = T)) %>%
    ungroup() %>%
    
    group_by(Segment, stage, race, year, class, date) %>%
    mutate(position_at_time = rank(behind_median, ties.method = "min")) %>%
    ungroup()
  
  dbWriteTable(con, "strava_position_in_race_250m", summed_time %>%
                 
                 select(Segment, Gradient = segment_gradient, Distance = new_segment_distance,
                        race, stage, year, class, date, low_end = start_prior, high_end = end_next,
                        rider, activity_id, time = new_segment_time, best_time, median_time, behind_median, vs_median,
                        bad_data), 
               row.names = F, append = TRUE)
  
  print(i)
  
}

#
#
#


#
# Loop through missing races and output to table
#

for(i in 1:length(races_to_generate_position_data$race)) {
  
  R = races_to_generate_position_data$race[[i]]
  S = races_to_generate_position_data$stage[[i]]
  Y = races_to_generate_position_data$year[[i]]
  
  #
  
  segments_to_consider <- all_new_segments_across_riders %>%
    filter(race == R & year == Y & stage == S) %>%
    filter(!(rider %in% c("Geschke Simon"))) %>%
    mutate(segs = max(rowid)) %>%
    group_by(activity_id) %>%
    filter(max(bad_data) == 0 & max(rowid) == segs & max(rowid) == n()) %>%
    ungroup()
  #
  
  summed_time <- rbind(
    
    segments_to_consider %>%
      
      filter(bad_data == 0) %>%
      select(rowid, stage, race, year, class, date, new_segment_distance = should_be_distance, 
             new_segment_speed_kmh = not_bad_data_speed, segment_gradient = median_gradient, segment_vertgain, 
             start_lat, end_lat, start_long, end_long, start_prior, end_next) %>%
      group_by(rowid) %>%
      mutate(segment_vertgain = median(segment_vertgain)) %>%
      unique() %>%
      
      inner_join(segments_to_consider %>% select(rider, stage, race, year, class, date, activity_id, total_seconds) %>% unique()) %>%
      
      anti_join(segments_to_consider, by = c("rowid", "rider")) %>%
      mutate(new_segment_time = new_segment_distance / ((new_segment_speed_kmh*1000/3600))) %>%
      as_tibble() %>%
      mutate(bad_data = 1,
             Power = as.numeric(NA),
             ValidPoints = 0,
             ValidPower = 0),
    
    segments_to_consider %>%
      select(-rel_speed, -not_bad_data_speed, -not_bad_data_dist, -bad_data, -spd_ratio, -ratio,
             -median_gradient, -median_speed, -median_time, -median_distance, -should_be_distance,
             -position_distance, -segment_speed_kmh, -segment_time, -segment_distance,
             -segment_time) %>%
      mutate(bad_data = 0)) %>%
    
    arrange(desc(start_prior)) %>%
    
    mutate(lastSegment = max(rowid)) %>%
    
    group_by(activity_id) %>%
    filter(lastSegment == max(rowid)) %>%
    ungroup() %>%
    
    group_by(Segment = rowid, stage, race, year, class, date) %>%
    mutate(best_time = min(new_segment_time, na.rm = T),
           median_time = median(new_segment_time, na.rm = T),
           median_finish = median(total_seconds, na.rm = T),
           best_finish = min(total_seconds, na.rm = T)) %>%
    ungroup() %>%
    
    group_by(stage, race, year, class, rider, date) %>%
    mutate(behind_median = cumsum(new_segment_time - median_time)) %>%
    ungroup() %>%
    
    group_by(rider, stage, race, year, class, date) %>%
    mutate(Segment_Cnt = n()) %>%
    ungroup() %>%
    
    mutate(vs_median_before = (total_seconds - median_finish) - behind_median) %>%
    
    group_by(Segment, stage, race, year, class, date) %>%
    mutate(position_at_time = rank(vs_median_before, ties.method = "min")) %>%
    ungroup()
  
  dbWriteTable(con, "strava_position_in_race", summed_time %>%
                 
                 select(Segment, Gradient = segment_gradient, Distance = new_segment_distance, 
                        race, stage, year, class, date, low_end = start_prior, high_end = end_next,
                        rider, activity_id, time = new_segment_time, best_time, median_time, behind_median, vs_median_before,
                        total_seconds, median_finish, position_at_time, bad_data), 
               row.names = F, append = TRUE)
  
  print(i)
  
}

#
#
#
#
#
#
#

breakaway_riders <- dbReadTable(con, "pcs_km_breakaway") %>%
  
  mutate(rider = str_to_title(rider)) %>%
  
  group_by(race, stage, year) %>%
  mutate(average = mean(km_before_peloton)) %>%
  ungroup() %>%
  
  mutate(breakaway_rider = ifelse(km_before_peloton > 40, 1, 0)) %>%
  
  mutate(url = str_replace(url, "/race", "race")) %>% 
  
  inner_join(all_race_activities %>%
               select(stage, url, rider = pcs, rnk, class, activity_id) %>%
               mutate(rider = str_replace(rider, "O C", "Oc"), 
                      rider = str_replace(rider, "O B", "Ob"), 
                      rider = str_replace(rider, "D H", "Dh")))

#

tdf13 <- all_new_segments_across_riders %>%
  
  filter(race == "giro d'italia" & year == 2022 & stage == 16) %>%
  
  left_join(breakaway_riders %>% select(activity_id, breakaway_rider), by = c("activity_id")) %>%
  
  mutate(breakaway_rider = ifelse(is.na(breakaway_rider), 0, breakaway_rider)) %>%
  mutate(rider = str_to_title(rider)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)) %>%
      filter(!is.na(weight)) %>%
      group_by(rider) %>%
      summarize(weight = median(weight)) %>%
      ungroup(), by = c("rider"))

#

tdf13 %>%
  filter(segment_distance > 200) %>%
  filter(end_next > 2.000 & end_next < 195.000) %>%
  filter(ValidPower == ValidPoints) %>%
  
  group_by(rowid, start_prior, end_next, breakaway_rider) %>%
  summarize(wattskg = mean(Power / weight, na.rm = T)) %>%
  ungroup() %>%

  ggplot(aes(x = start_prior, y = as.factor(breakaway_rider), fill = wattskg, color = wattskg))+
  geom_tile()+
  scale_fill_gradientn(colors = c("black", "gray30", "gray60", "gray90", "white",
                                  "yellow", "gold", "orange", "#FF5733", "red", "dark red", "#4A0D00"), name = "Watts/KG")+
  scale_color_gradientn(colors = c("black", "gray30", "gray60", "gray90", "white",
                                   "yellow", "gold", "orange", "#FF5733", "red", "dark red", "#4A0D00"), name = "Watts/KG")+
  labs(x = "KM",
       y = "Breakaway Rider?")

# 
 
tdf13 %>%
  filter(segment_distance > 200) %>%
  filter(end_next > 2.000 & end_next < 300.000) %>%
  filter(ValidPower == ValidPoints) %>%
  
  group_by(rowid, start_prior, end_next, category = ifelse(breakaway_rider == 1, "Break", ifelse(rnk <= 20, "Top 20", ifelse(rnk >= 80, "Broomwagon", "Peloton")))) %>%
  summarize(spd = mean(segment_speed_kmh - median_speed, na.rm = T)) %>%
  ungroup() %>%
  
  ggplot(aes(x = start_prior, y = category, fill = spd))+
  geom_tile()+
  geom_vline(xintercept = c(40,60,115,130,167,172,182,196))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "orange", name = "Rel Spd", midpoint = 0)+
  labs(x = "KM",
       y = "")
