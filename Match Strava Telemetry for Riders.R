

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
                  WHERE Stat IN ('Distance', 'AvgPower')") %>% 
  
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
  
  inner_join(dbGetQuery(con, "SELECT rider, date, stage, race, year, class, length, stage_type, missing_profile_data, rnk
                        FROM pcs_stage_data") %>%
               
               mutate(date = as.Date(date, origin = '1970-01-01')) %>%
               mutate(rider = str_to_title(rider)) %>%
               unique(), by = c("date", "pcs" = "rider")) %>% 
  
  # if two results exist for same day matching distance, it's probably a recon and TT which
  # means drop the lower watts
  
  # also, many riders include distance outside the TT as part of their strava activity
  # so maybe accept any riders +/- 10 km? or maybe we just can't get accurate TT data
  
  mutate(distance = distance * 1.609) %>% 
  #filter((distance / length) > 0.95) %>%
  #filter((distance / length) < 1.05) %>%
  
  inner_join(
    
    fs::dir_info('D:/Jake/Documents/STRAVA_JSON/') %>%
      select(path, birth_time) %>%
      mutate(activity_id = str_replace(path, 'D:/Jake/Documents/STRAVA_JSON/strava-activity-id-', ''),
             activity_id = str_replace(activity_id, ".rds", "")), by = c("activity_id")) %>%
  
  filter(birth_time > '2021-06-26 12:00:00')

#

all_race_activities <- all_race_activities %>%
  filter(!is.na(avg_power)) %>%
  anti_join(dbGetQuery(con, "SELECT DISTINCT activity_id FROM rider_telem_from_stava"))

#
#
#
#
#

for(r in 1:length(all_race_activities$activity_id)) {
  
  ACTIVITY <- all_race_activities$activity_id[[r]]
  
  data_lists <- read_rds(paste0("D:/Jake/Documents/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
  
  # clean this up before writing to DB
  
  lat <- data_lists$latlng[,1]
  long <- data_lists$latlng[,2]
  
  df <- cbind(
    
    altitude = data_lists[["altitude"]],
    distance = data_lists[["distance"]],
    time = data_lists[["time"]],
    watts = data_lists[["watts"]],
    activity_id = ACTIVITY
    
  )
  
  if(!"watts" %in% colnames(df)) {
    
  } else {
    
  df <- df %>%
    
    as_tibble() %>%
    mutate(altitude = as.numeric(altitude),
           distance = as.numeric(distance),
           watts = as.numeric(watts),
           time = as.numeric(time)) %>%
    mutate(distance = round(distance, 0),
           altitude = round(altitude, 0)) %>%
    
    cbind(latitude = lat,
          longitude = long)
  
  #
  
  Stage = all_race_activities$stage[[r]]
  Year = all_race_activities$year[[r]]
  
  #lengths <- paste0('https://racecenter.letour.fr/api/checkpointList-', Year, '-', Stage) %>% jsonlite::fromJSON() %>% rowid_to_column() %>% filter(length == 0) %>% filter(rowid == max(rowid))
  
  lengths <- paste0('https://racecenter.letour.fr/api/checkpoint-', Year, '-', Stage) %>%
    readLines() %>%
    rjson::fromJSON() %>%
    .[[1]] %>%
    unlist() %>%
    enframe() %>%
    mutate(dupe = name) %>%
    separate(dupe, c("rowid", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", 'v11'), sep = "\\.") %>%
    filter(v1 %in% c("length", "latitude", "longitude")) %>%
    select(value, rowid, v1) %>%
    spread(v1, value) %>%
    filter(length == 0) %>%
    filter(!is.na(latitude)) %>%
    filter(max(as.numeric(rowid)) == as.numeric(rowid))
  
  json <- paste0('https://racecenter.letour.fr/api/checkpoint-', Year, '-', Stage) %>%
    readLines() %>%
    rjson::fromJSON() %>%
    .[[1]]
  
  json <- json[lengths(json) > 1]
  
  json <- json[[length(json)]]
  
  finish_lat <- json$latitude
  finish_long <- json$longitude
  finish_dist <- json$length
  
  start_lat <- as.numeric(lengths$latitude)
  start_long <- as.numeric(lengths$longitude)
  
  #
  
  df %>%
    
    mutate(est_start = floor(round(abs(latitude - start_lat) + abs(longitude - start_long),4) / 0.0002),
           est_finish = floor(round(abs(latitude - finish_lat) + abs(longitude - finish_long),4) / 0.0002)) %>%
    
    mutate(est_finish = ifelse(is.na(watts) | watts <= 0, NA, est_finish)) %>%
    
    arrange(time) %>%
    
    mutate(spd = (distance - lag(distance)) / (time - lag(time))) %>%
    
    mutate(est_finish = ifelse(spd <= 0, NA, est_finish),
           est_finish = ifelse(est_finish <= 2, 0, est_finish)) %>%
    
    mutate(FINISH = ifelse(est_finish == min(est_finish, na.rm = T), 1, 0),
           START = ifelse(est_start == min(est_start, na.rm = T), 1, 0)) %>%
    
    mutate(start_distance = ifelse(START == 1, distance, NA),
           finish_distance = ifelse(FINISH == 1, distance, NA),
           start_distance = ifelse(start_distance == min(start_distance, na.rm = T), start_distance, NA),
           finish_distance = ifelse(finish_distance == max(finish_distance, na.rm = T), finish_distance, NA),
           start_distance = mean(start_distance, na.rm = T),
           finish_distance = mean(finish_distance, na.rm = T),
           
           dist_left = finish_distance - distance,
           distance = distance - start_distance) %>%
    
    select(-est_start, -est_finish, -start_distance, -finish_distance, -FINISH, -START, -spd) -> write_to_DB
  
  #
  
  dbWriteTable(con, "rider_telem_from_stava", write_to_DB, append = T, row.names = F)
  
  print(r)
  
  }

}

#
 
write_to_DB %>% 
  mutate(time = floor(time/10)*10) %>% 
  
  group_by(time) %>%
  summarize(longitude = median(longitude), 
            latitude = median(latitude), 
            distance = max(distance), 
            dist_left = median(dist_left),
            watts = mean(watts)) %>% 
  ungroup() %>% 
  
  filter(distance > 172000 & dist_left > 0) %>% 
  
  arrange(distance) %>%
  
  mutate(watts_color = lead(watts)) %>%
  
  ggplot(aes(x = distance/1000, y = watts/61, color = watts_color/61, label = round(dist_left/1000,1)))+
  geom_hline(yintercept = 0)+
  geom_line(size=2)+
  ggrepel::geom_text_repel(color = "black")+
  scale_color_gradientn(colors = c("gray", "white", "gold", "orange", "red", "dark red", "#641E16", "black", "black", "black"))+
  theme_bw()+labs(x = "", y = "", color = "Watts/kg")+
  
  theme(axis.text = element_text(size=15))

#
#
#
#
#
#
#
#

all_telem <- dbGetQuery(con, "SELECT * FROM rider_telem_from_stava")

#

telem <- all_telem %>%
  
  inner_join(all_race_activities %>% select(rider = pcs, stage, race, year, class, date, activity_id), by = c("activity_id")) %>%
  
  inner_join(dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
               mutate(rider = str_to_title(rider)))

#

telem %>% 
  filter(distance > 0 & dist_left > 0) %>%
  
  group_by(stage, race, year, rider) %>% 
  mutate(time = time - min(time, na.rm = T)) %>% 
  ungroup() %>% 
  
  group_by(min = floor(time/600), stage, race, year, class, date) %>%
  summarize(missing_watts = mean(is.na(watts)), 
            wattskg = mean(watts/weight, na.rm = T),
            distance = mean(distance, na.rm = T), 
            riders = n_distinct(rider),
            altitude = mean(altitude, na.rm = T)) %>% 
  ungroup() -> minute_by_minute

#

minute_by_minute %>% 
  group_by(min) %>% 
  filter(n() >= 5) %>% 
  ungroup() %>% 
  
  filter(riders >= 15) %>%
  
  ggplot(aes(x = min*10, y = wattskg, group = min))+
  geom_boxplot(outlier.colour = "transparent")+
  labs(x = "Minutes since start", y = "Watts per KG", 
       title = "Watts per KG in 10 minute buckets (2020-21 TDF)", 
       subtitle = "Stage 4 & 9 in 2021 shown as colored points")+
  
  geom_point(data = minute_by_minute %>% 
               filter(year == 2021 & stage %in% c(4,9) & riders >= 15) %>% 
               mutate(StageId = paste0("Stage ", stage, " 2021")),
             aes(fill = wattskg), color = "black", size = 3, shape = 21)+
  
  scale_x_continuous(breaks = seq(0,390,30))+
  scale_fill_gradientn(colors = c("blue", "light blue", "white", "yellow", "orange", "red", "dark red"))+
  facet_wrap(~StageId)

#


