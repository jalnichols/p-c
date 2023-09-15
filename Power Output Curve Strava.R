
library(tidyverse)
library(rvest)
library(DBI)

dbDisconnect(con)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      port = 5432,
                      host = 'localhost',
                      dbname = "cycling",
                      user = "postgres",
                      password = "braves")


options(dplyr.summarise.inform = FALSE)
options(tidyverse.quiet = TRUE)

#

all_race_activities <- dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Distance','AvgElapsed Time', 'AvgPower')") %>% 
  mutate(pcs = ifelse(pcs %in% c("Oconnor Ben", "OConnor Ben"), "O'connor Ben",
                      ifelse(pcs %in% c("Obrien Kelland", "OBrien Kelland"), "O'brien Kelland", pcs))) %>%
  rename(PCS = pcs, DATE = date, VALUE = value, Stat = stat) %>%
  
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
                        WHERE year IN (2015, 2016, 2017, 2018, 2019, 2020,2021,2022,2023)") %>%
               
               mutate(date = as.Date(date)) %>%
               mutate(date = as.Date(date, origin = '1970-01-01')) %>%
               unique(), by = c("date", "pcs" = "rider")) %>% 
  
  # if two results exist for same day matching distance, it's probably a recon and TT which
  # means drop the lower watts
  
  # also, many riders include distance outside the TT as part of their strava activity
  # so maybe accept any riders +/- 10 km? or maybe we just can't get accurate TT data
  
  mutate(distance = distance * 1.609) %>% 
  filter((distance / length) > 0.50) %>%
  filter((distance / length) < 2)

#
#
#
#
#
#

telemetry_available <- all_race_activities %>%
  
  filter(class != "JR") %>%

  mutate(pcs = str_to_title(pcs)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)) %>%
      filter(!is.na(weight)) %>%
      group_by(rider) %>%
      summarize(weight = median(weight)) %>%
      ungroup(), by = c("pcs" = "rider"))

#

power_curves <- dbGetQuery(con, "SELECT * FROM strava_activity_power_curve
                           WHERE seconds IN (10,15,30,60,120,300,600,1200,2400,3600,7200)") %>%
  
  filter(seconds %in% c(10,15,30,60,120,300,600,1200,2400,3600,7200)) %>%
  
  inner_join(telemetry_available) %>%
  unique()

#
#
#
#

tdf <- power_curves %>%
  
  mutate(watts_per_kg = watts / weight) %>%
  
  group_by(year, rider = pcs, seconds) %>%
  mutate(vs_rider_season = watts_per_kg / max(watts_per_kg, na.rm = T),
         races = n()) %>%
  ungroup() %>%
  
  filter(races >= 20) %>%
  
  #filter(race == "tour de france") %>%
  
  select(-c(win_seconds, total_seconds, missing_profile_data, total_vert_gain, avg_alt, sof_limit, success, points_finish,
            leader_rating, pcs, gain_3rd, gain_5th, gain_10th, gain_40th, parcours_value))

#

tdf %>% 
  
  mutate(race_weight = 0.69 / log(rnk+1)) %>%
  
  group_by(url, year, stage, date, seconds) %>%
  mutate(top_finisher = rank(rnk, ties.method = "first") <= 5,
         race_weight = race_weight / sum(race_weight, na.rm = T),
         raw_top_finisher = ifelse(top_finisher == TRUE, watts_per_kg, NA),
         top_finisher = ifelse(top_finisher == TRUE, vs_rider_season, NA)) %>%
  summarize(weighted_adj_watts_per_kg = sum(race_weight * vs_rider_season, na.rm = T)/sum(race_weight, na.rm = T),
            weighted_raw_watts_per_kg = sum(race_weight * watts_per_kg, na.rm = T)/sum(race_weight, na.rm = T),
            adj_watts_per_kg = median(vs_rider_season, na.rm = T), 
            raw_watts_per_kg = median(watts_per_kg, na.rm = T),
            top10 = quantile(vs_rider_season, probs = 0.9, na.rm = T),
            raw_top10 = quantile(watts_per_kg, probs = 0.9, na.rm = T),
            top_finisher = median(top_finisher, na.rm = T),
            raw_top_finisher = median(raw_top_finisher, na.rm = T),
            sample = n()) %>%
  ungroup() %>% 
  
  filter(sample >= 10) %>%
  
  group_by(seconds, year) %>% 
  mutate(rel = adj_watts_per_kg / mean(adj_watts_per_kg),
         rel_wtd = weighted_raw_watts_per_kg / mean(weighted_raw_watts_per_kg, na.rm = T),
         rel90 = top10 / mean(top10),
         top_rnk = top_finisher / mean(top_finisher)) %>%
  ungroup() -> aggs


aggs %>% 
  filter(seconds > 10) %>%
  filter(str_detect(url, "france/2023")) %>%
  
  ggplot(aes(x = seconds, y = weighted_raw_watts_per_kg, color = as.character(stage), group = paste0(stage,year)))+
  geom_hline(yintercept = 1)+
  geom_line(linewidth=0.75, color = "gray")+
  geom_line(data = . %>% 
              group_by(seconds, year = '2023', stage = 0) %>% 
              summarize(weighted_raw_watts_per_kg = median(weighted_raw_watts_per_kg)) %>% 
              ungroup(), 
            color = "black", linewidth = 2)+
  geom_line(data = . %>% filter(stage == 1 & year == 2023), linewidth = 1)+
  geom_point(data = . %>% filter(stage == 1 & year == 2023), size=3)+
  scale_x_log10(breaks = c(15,30,60,120,300,600,1200,2400,3600,7200))+
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1))+
  #ggrepel::geom_label_repel(data = . %>% filter(seconds %in% c(15,7200)), aes(label = stage))+
  theme(legend.position = "bottom")+
  labs(x = "seconds", 
       y = "Watts per KG", 
       title = "2023 TDF Stage 12", color = "")+
  scale_color_manual(values = c("#FCFF50"), guide = "none")

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

seconds <- c(10,15,30,60,120,300,600,1200,2400,3600,7200)

dbSendQuery(con, "DELETE FROM race_peak_efforts_from_strava")

for(s in seconds) {
  
  peak_abs <- tdf %>%
    
    filter(!is.na(watts)) %>%
    
    mutate(spec_stage = paste0(url,"---",stage)) %>%
    
    filter(seconds == s) %>%
    
    group_by(spec_stage) %>%
    filter(n() >= 10) %>%
    ungroup() %>%
    
    mutate(year = as.character(year)) %>%
    
    lme4::lmer(watts ~ (1 | spec_stage) + log(rnk+1) + year, data = .)
  
  ranefs_abs <- lme4::ranef(peak_abs)[[1]] %>% rownames_to_column()
  
  fixefs_abs <- lme4::fixef(peak_abs)
  
  #####################
  
  per_race_efforts_abs <- ranefs_abs %>%
    rename(ranef = `(Intercept)`) %>%
    mutate(rolling_speed = s,
           intercept = fixefs_abs[[1]],
           log_rnk = fixefs_abs[[2]],
           winner_abs = ranef + (intercept + (log_rnk * (log(2))))) %>%
    
    separate(rowname, c("url", "stage"), sep = "---")
  
  #####################
  
  peak_wpk <- tdf %>%
    
    filter(!is.na(watts_per_kg)) %>%
    
    mutate(spec_stage = paste0(url,"---",stage)) %>%
    
    filter(seconds == s) %>%
    
    group_by(spec_stage) %>%
    filter(n() >= 10) %>%
    ungroup() %>%
    
    mutate(year = as.character(year)) %>%
    
    lme4::lmer(watts_per_kg ~ (1 | spec_stage) + log(rnk+1) + year, data = .)
  
  ranefs_wpk <- lme4::ranef(peak_wpk)[[1]] %>% rownames_to_column()
  
  fixefs_wpk <- lme4::fixef(peak_wpk)
  
  #####################
  
  per_race_efforts_wpk <- ranefs_wpk %>%
    rename(ranef = `(Intercept)`) %>%
    mutate(rolling_speed = s,
           intercept = fixefs_wpk[[1]],
           log_rnk = fixefs_wpk[[2]],
           winner_wpk = ranef + (intercept + (log_rnk * (log(2))))) %>%
    
    separate(rowname, c("url", "stage"), sep = "---")
  
  #####################
  
  peak_vsseason <- tdf %>%
    
    filter(!is.na(vs_rider_season)) %>%
    
    mutate(spec_stage = paste0(url,"---",stage)) %>%
    
    filter(seconds == s) %>%
    
    group_by(spec_stage) %>%
    filter(n() >= 10) %>%
    ungroup() %>%
    
    mutate(year = as.character(year)) %>%
    
    lme4::lmer(vs_rider_season ~ (1 | spec_stage) + log(rnk+1), data = .)
  
  ranefs_vsseason <- lme4::ranef(peak_vsseason)[[1]] %>% rownames_to_column()
  
  fixefs_vsseason <- lme4::fixef(peak_vsseason)
  
  #####################
  
  per_race_efforts_vsseason <- ranefs_vsseason %>%
    rename(ranef = `(Intercept)`) %>%
    mutate(rolling_speed = s,
           intercept = fixefs_vsseason[[1]],
           log_rnk = fixefs_vsseason[[2]],
           winner_vsseason = ranef + (intercept + (log_rnk * (log(2))))) %>%
    
    separate(rowname, c("url", "stage"), sep = "---")
  
  #####################
  
  per_race_efforts <- left_join(per_race_efforts_abs, per_race_efforts_wpk, by = c("url", "stage", "rolling_speed")) %>%
    rename(ranef_abs = ranef.x,
           ranef_wpk = ranef.y,
           intercept_abs = intercept.x,
           intercept_wpk = intercept.y,
           log_rnk_abs = log_rnk.x,
           log_rnk_wpk = log_rnk.y) %>%
    left_join(per_race_efforts_vsseason, by = c("url", "stage", "rolling_speed")) %>%
    rename(intercept_vsseason = intercept,
           log_rnk_vsseason = log_rnk,
           ranef_vsseason = ranef)
  
  #####################
  
  dbWriteTable(con, "race_peak_efforts_from_strava", per_race_efforts, append = TRUE, row.names = FALSE)
  
  print(s)
  
}

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


for_creating_segments_from_strava <- dbGetQuery(con, "SELECT * FROM ideal_activity_id_for_race") %>%
  inner_join(telemetry_available %>% select(year, race, class, stage, date, url) %>% unique()) %>%
  select(pcs, distance, race, stage, class, date, year, length, activity_id, stage_type) %>%
  mutate(creation_type = "TIME") %>%
  # anti_join(
  #   inner_join(telemetry_available,
  #              dbGetQuery(con, "SELECT DISTINCT activity_id FROM strava_rolling_power_curves"), by = c("activity_id")) %>%
  #     select(stage, race, year, class, date, length)) %>%
  
  arrange(desc(class %in% c("1.UWT", "2.UWT", "WC", "Olympics")), desc(date)) %>%
  
  filter(!(stage == 1 & race == 'tour de romandie' & year == 2019)) %>%
  filter(!(stage == 4 & race == 'voo-tour de wallonie' & year == 2019))

#

for(c in 1:nrow(for_creating_segments_from_strava)) {
  
  ACTIVITY_PRIME = for_creating_segments_from_strava$activity_id[[c]]
  R = for_creating_segments_from_strava$race[[c]]
  S = for_creating_segments_from_strava$stage[[c]]
  Y = for_creating_segments_from_strava$year[[c]]
  
  # bring in ideal strava race data
  
  data_lists <- read_rds(paste0("C:/Users/Jake Nichols/Documents/Old D Drive/STRAVA_JSON/strava-activity-id-", ACTIVITY_PRIME, ".rds"))
  
  if(length(data_lists$latlng) > 1) {
    
    lat <- data_lists$latlng[,1]
    long <- data_lists$latlng[,2]
    
    if(length(lat) == 0) {} else {
      
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
        
        data_lists <- read_rds(paste0("C:/Users/Jake Nichols/Documents/Old D Drive/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
        
        if(length(data_lists$latlng) > 1) {
          
          lat <- data_lists$latlng[,1]
          long <- data_lists$latlng[,2]
          
          if(length(lat) == 0) {} else {
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
            
            if("altitude" %in% names(data_lists)) {
              
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
        
        #filter(rowid >= start_rowid & rowid <= finish_rowid) %>%
        
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
  
    }
    
  }
  
}


where_peak <- power_curves %>% 
  filter(activity_id %in% all_telem_df$activity_id) %>% 
  
  select(activity_id, watts, seconds, start_time, end_time) %>% 
  
  gather(segment, time, start_time:end_time)  %>%
  
  inner_join(all_telem_df %>% 
               select(activity_id, altitude, distance, time, latitude, longitude, distance_left, 
                      distance_gone, rider, rnk, team, weight), by = c("time", "activity_id"))

#

where_peak %>% 
  filter(segment == "end_time") %>% 
  ggplot(aes(x = seconds/60, y = distance_gone/1000, alpha = 3*(1 / log(rnk))^2))+
  geom_point(size=3, color = "orange")+
  scale_x_log10(breaks = c(1,2,5,10,20,40,60,120))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = median(all_telem_df$finish_distance/1000))+
  labs(x = "efforts (min)", y = "distance raced (km)",
       alpha = "finish position",
       title = "Tour de France Stage 20 (max efforts)")

where_peak %>% 
  filter(segment == "end_time") %>% 
  ggplot(aes(x = seconds/60, y = distance_gone/1000, group = seconds/60))+
  geom_boxplot()+
  scale_x_log10(breaks = c(1,2,5,10,20,40,60,120))+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = median(all_telem_df$finish_distance/1000))+
  labs(x = "efforts (min)", y = "distance raced (km)",
       alpha = "finish position",
       title = "Tour de France Stage 20 (location max efforts)")+
  facet_wrap(~ifelse(rnk <= 20, "Top 20", "Everyone else"), ncol = 1)

where_peak %>% 
  filter(segment == "end_time") %>% 
  ggplot(aes(x = reorder(ifelse(seconds < 60, paste0(seconds,"s"), 
                                ifelse(seconds < 3500, paste0(seconds/60,"m"), paste0(seconds/3600, "hr"))),seconds), 
             y = distance_gone/1000,
             color = ifelse(rnk <= 20, "Top 20", "Everyone else")))+
  geom_boxplot()+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = median(all_telem_df$finish_distance/1000))+
  labs(x = "efforts (min)", y = "distance raced (km)",
       alpha = "finish position",
       title = "Tour de France Stage 20 (location max efforts)")+
  scale_color_manual(values = c("black", "orange"), name = "Top 20?")


where_peak %>% 
  filter(segment == "end_time") %>% 
  ggplot(aes(x = reorder(ifelse(seconds < 60, paste0(seconds,"s"), 
                                ifelse(seconds < 3500, paste0(seconds/60,"m"), paste0(seconds/3600, "hr"))),seconds), 
             y = distance_gone/1000,
             color = ifelse(rnk <= 20, "Top 20", "Everyone else")))+
  geom_point(size = 3)+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = median(all_telem_df$finish_distance/1000))+
  labs(x = "efforts (min)", y = "distance raced (km)",
       alpha = "finish position",
       title = "Tour de France Stage 20 (location max efforts)")+
  scale_color_manual(values = c("black", "orange"), name = "Top 20?")
