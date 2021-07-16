

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
             activity_id = str_replace(activity_id, ".rds", "")), by = c("activity_id"))

#

all_race_activities <- all_race_activities %>%
  filter(year == 2021 & class == "2.UWT") %>%
  filter((distance / length) > 0.33) %>%
  filter(length > 50) %>%
  #filter(birth_time > '2021-06-25 12:00:00') %>%
  #filter(!is.na(avg_power)) %>%
  anti_join(dbGetQuery(con, "SELECT DISTINCT activity_id FROM telemetry_strava_fr_rider"))

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
  mutate(time = time - min(time, na.rm = T),
         time_left = max(time, na.rm = T) - time) %>% 
  ungroup() %>% 
  
  group_by(min = floor(time/600), stage, race, year, class, date) %>%
  summarize(missing_watts = mean(is.na(watts)), 
            wattskg = mean(watts/weight, na.rm = T),
            distance = mean(distance, na.rm = T), 
            riders = n_distinct(rider),
            altitude = mean(altitude, na.rm = T)) %>% 
  ungroup() -> minute_by_minute

#

telem %>% 
  filter(distance > 0 & dist_left > 0) %>%
  
  group_by(stage, race, year, rider) %>% 
  mutate(time = time - min(time, na.rm = T),
         time_left = max(time, na.rm = T) - time) %>% 
  ungroup() %>% 
  
  group_by(rider, min = floor(time_left/60), stage, race, year, class, date) %>%
  summarize(missing_watts = mean(is.na(watts)), 
            wattskg = mean(watts/weight, na.rm = T),
            distance = mean(distance, na.rm = T),
            altitude = mean(altitude, na.rm = T)) %>% 
  ungroup() -> minute_by_minute_riders

minute_by_minute_riders %>% filter(year==2021) %>% mutate(finish_type = ifelse(stage %in% c(1,2,7,8,9,11,12), "groups", "bunch_sprint")) %>% group_by(min, finish_type, rider) %>% summarize(wattskg = mean(wattskg, na.rm = T), stages = n()) %>% filter(min <= 60) %>% group_by(rider, finish_type) %>% mutate(rel_watts = wattskg - mean(wattskg, na.rm = T)) %>% ungroup() -> rider_wattskg_min

minute_by_minute_riders %>% filter(year==2020) %>% mutate(finish_type = ifelse(stage %in% c(1,3,5,7,10,11,14,19,21), "bunch_sprint", "groups")) %>% group_by(min, finish_type, rider) %>% summarize(wattskg = mean(wattskg, na.rm = T), stages = n()) %>% filter(min <= 60) %>% group_by(rider, finish_type) %>% mutate(rel_watts = wattskg - mean(wattskg, na.rm = T)) %>% ungroup() -> rider_wattskg_min


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
       subtitle = "Stage 11 & 12 in 2021 shown as colored points")+
  
  geom_point(data = minute_by_minute %>% 
               group_by(min) %>% 
               filter(n() >= 5) %>% 
               ungroup() %>% 
               filter(year == 2021 & stage %in% c(11,12) & riders >= 15) %>% 
               mutate(StageId = paste0("Stage ", stage, " 2021")),
             aes(fill = wattskg), color = "black", size = 3, shape = 21)+
  
  geom_blank(aes(fill = wattskg))+
  
  scale_x_continuous(breaks = seq(0,390,30))+
  scale_fill_gradientn(colors = c("navy", "blue", "light blue", "white", "yellow", "orange", "red", "dark red", "#641E16"))+
  facet_wrap(~StageId, nrow = 2)

#
#
#
#
#
#
#
#
#

all_stages <- dbReadTable(con, "fr_stage_info") %>%
  filter(year==2021) %>%
  unique() %>%
  
  mutate(race = case_when(race == "Paris-Nice" ~ "paris-nice",
                          race == "Tirreno-Adriatico" ~ "tirreno-adriatico",
                          race == "UAE Tour" ~ "uae tour",
                          race == "Tour de France" ~ "tour de france",
                          race == "Giro d'Italia" ~ "giro d'italia",
                          race == "Tour de Suisse" ~ "tour de suisse",
                          race == "Criterium du Dauphine" ~ "criterium du dauphine",
                          race == "Tour de Romandie" ~ "tour de romandie",
                          race == "Volta Ciclista a Catalunya" ~ "volta ciclista a catalunya",
                          race == "Itzulia Basque Country" ~ "itzulia basque country",
                          TRUE ~ race))

#
#
#

for(r in 1:length(all_race_activities$activity_id)) {
  
  ACTIVITY <- all_race_activities$activity_id[[r]]
  
  data_lists <- read_rds(paste0("D:/Jake/Documents/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
  
  # clean this up before writing to DB
  
  lat <- data_lists$latlng[,1]
  long <- data_lists$latlng[,2]
  
  if(length(lat) > 0) {
    
    df <- cbind(
      
      altitude = data_lists[["altitude"]],
      distance = data_lists[["distance"]],
      time = data_lists[["time"]],
      #watts = data_lists[["watts"]],
      activity_id = ACTIVITY
      
    ) %>% 
      as_tibble() %>%
      mutate(altitude = as.numeric(altitude),
             distance = as.numeric(distance),
             #watts = as.numeric(watts),
             time = as.numeric(time)) %>%
      mutate(distance = round(distance, 0),
             altitude = round(altitude, 0)) %>%
      
      cbind(latitude = lat,
            longitude = long)
    
    #
    
    Stage = all_race_activities$stage[[r]]
    Race = all_race_activities$race[[r]]
    Year = all_race_activities$year[[r]]
    
    #
    
    correct_stage_info <- all_stages %>%
      
      filter(race == Race &
               stage_name == Stage) %>%
      .[nrow(.),]
    
    # bring in Flamme Rouge data
    
    stage_FR <- read_rds(paste0("C:/Users/Jake/Documents/R Code/p-c/F-R-JSON/", 
                                str_replace(correct_stage_info$stage_url, 'https://www.la-flamme-rouge.eu/maps/loadtrack/', ''),
                                "-",
                                str_replace(correct_stage_info$race_url, "https://www.la-flamme-rouge.eu/maps/races/view/2021/", ""),
                                ".rds"))
    
    route <- stage_FR$altRoute
    
    # extract finish and start data
    
    finish_lat <- as.numeric(route[[length(route)]]$position$k)
    finish_long <- as.numeric(route[[length(route)]]$position$A)
    finish_dist <- as.numeric(route[[length(route)]]$distance)
    
    start_lat <- as.numeric(route[[1]]$position$k)
    start_long <- as.numeric(route[[1]]$position$A)
    
    #
    
    df %>%
      
      mutate(est_start = floor(round(abs(latitude - start_lat) + abs(longitude - start_long),4) / 0.0002),
             est_finish = floor(round(abs(latitude - finish_lat) + abs(longitude - finish_long),4) / 0.0002)) %>%
      
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
      
      mutate(best_start = min(est_start, na.rm = T),
             best_finish = min(est_finish, na.rm = T)) %>%
      
      select(-est_start, -est_finish, -start_distance, -finish_distance, -FINISH, -START, -spd) -> write_to_DB
    
    dbWriteTable(con, "telemetry_strava_fr_rider", 
                 write_to_DB %>%
                   mutate(activity_id = ACTIVITY) %>%
                   filter(best_start == 0 & best_finish == 0) %>%
                   select(-best_start, -best_finish), append = T, row.names = F)
    
    print(r)
    
  }
  
}

#
#
#

all_telem <- dbGetQuery(con, "SELECT altitude, distance, dist_left, time, activity_id
                        FROM telemetry_strava_fr_rider") %>%
  
  group_by(activity_id) %>% 
  mutate(time_race = ifelse(distance == 0, time, NA),
         time_race = mean(time_race, na.rm = T),
         time_race = time - time_race) %>%
  ungroup() %>%
  
  inner_join(all_race_activities %>% select(stage, race, year, activity_id, rider = pcs, rnk) %>% unique())

#

all_telem %>% 
  filter(distance >= 0 & dist_left >= 0) %>% 
  rename(kmToFinish = dist_left) %>% mutate(kmToFinish = kmToFinish/1000) %>%
  
  group_by(stage, race, year) %>%
  mutate(best_finisher = min(rnk, na.rm = T)) %>%
  ungroup() %>%
    
  mutate(primoz = ifelse(rnk == best_finisher, kmToFinish, NA)) %>% 
  
  group_by(stage, race, year, time_race) %>%
  mutate(primoz = mean(primoz, na.rm = T)) %>% 
  ungroup() %>%
  
  mutate(primoz = ifelse(primoz == "NaN", 0.001, primoz),
         gap = kmToFinish - primoz) %>%
  
  group_by(stage, race, year, rider) %>%
  mutate(zero_gap = ifelse(time_race == 0, gap, NA),
         finish_time = ifelse(kmToFinish == 0, time_race, NA),
         finish_time = mean(finish_time, na.rm = T),
         zero_gap = mean(zero_gap, na.rm = T)) %>%
  ungroup() %>%
  
  #mutate(adjkmToFinish = kmToFinish - zero_gap) %>%
  
  filter(primoz != 0.001) %>%
  
  mutate(to_roglic = ifelse(primoz == 0.001, 0, ifelse(abs(kmToFinish - primoz) < 0.33, 1, 
                                                       ifelse(kmToFinish < primoz, 1, 0))),
         #adj_to_roglic = ifelse(primoz == 0.001, 0, ifelse(abs(adjkmToFinish - primoz) < 0.33, 1, 
         #                                                  ifelse(adjkmToFinish < primoz, 1, 0)))
         ) %>% 
  
  filter(kmToFinish > 0 & time_race >= 0) %>% 
  
  mutate(valid = ifelse(to_roglic == 1, ifelse(primoz == 0.001, NA, kmToFinish), NA)) %>%
  
  group_by(stage, race, year, rider, rnk, finish_time) %>% 
  summarize(to_roglic = mean(to_roglic, na.rm = T),
            furthest = min(valid, na.rm = T),
            stamps = n()) %>%
  ungroup()  -> survival_w_roglic

#
#
#

gaps <- all_telem %>% 
  filter(distance >= 0 & dist_left >= 0) %>% 
  rename(kmToFinish = dist_left) %>% mutate(kmToFinish = kmToFinish/1000) %>%
  mutate(primoz = ifelse(rider == "O'connor Ben", kmToFinish, NA)) %>% 
  
  group_by(time_race) %>%
  mutate(primoz = mean(primoz, na.rm = T)) %>% 
  ungroup() %>%
  
  mutate(primoz = ifelse(primoz == "NaN", 0.001, primoz),
         gap = kmToFinish - primoz) %>%
  
  group_by(rider) %>%
  mutate(zero_gap = ifelse(time_race == 0, gap, NA),
         finish_time = ifelse(kmToFinish == 0, time_race, NA),
         finish_time = mean(finish_time, na.rm = T),
         zero_gap = mean(zero_gap, na.rm = T),
         MA_gap = mean(abs(gap), na.rm = T),
         med_gap = median(gap, na.rm = T)) %>%
  ungroup() %>%
  
  #mutate(adjkmToFinish = kmToFinish - zero_gap) %>%
  
  filter(primoz != 0.001) %>%
  
  mutate(to_roglic = ifelse(primoz == 0.001, 0, ifelse(abs(kmToFinish - primoz) < 0.33, 1, 
                                                       ifelse(kmToFinish < primoz, 1, 0))),
         #adj_to_roglic = ifelse(primoz == 0.001, 0, ifelse(abs(adjkmToFinish - primoz) < 0.33, 1, 
         #                                                  ifelse(adjkmToFinish < primoz, 1, 0)))
  ) %>% 
  
  filter(kmToFinish > 0 & time_race >= 0)
  