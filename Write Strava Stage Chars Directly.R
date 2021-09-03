

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
                  WHERE Stat IN ('Distance')") %>% 
  
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
  
  inner_join(dbGetQuery(con, "SELECT rider, date, stage, race, year, class, length, stage_type, missing_profile_data
                        FROM pcs_stage_data") %>%
              
               mutate(date = as.Date(date, origin = '1970-01-01')) %>%
               mutate(rider = str_to_title(rider)) %>%
               unique(), by = c("date", "pcs" = "rider")) %>% 
  
  # if two results exist for same day matching distance, it's probably a recon and TT which
  # means drop the lower watts
  
  # also, many riders include distance outside the TT as part of their strava activity
  # so maybe accept any riders +/- 10 km? or maybe we just can't get accurate TT data
  
  mutate(distance = distance * 1.609) %>% 
  filter((distance / length) > 0.95) %>%
  filter((distance / length) < 1.05) %>%
  
  inner_join(
    
    fs::dir_info('D:/Jake/Documents/STRAVA_JSON/') %>%
      select(path, birth_time) %>%
      mutate(activity_id = str_replace(path, 'D:/Jake/Documents/STRAVA_JSON/strava-activity-id-', ''),
             activity_id = str_replace(activity_id, ".rds", "")), by = c("activity_id")) %>%
  
  filter(birth_time > '2021-01-01 10:00:00') %>%
  
  group_by(stage, race, year, class) %>%
  filter(rank(abs((distance/length)-1), ties.method = 'random') <= 5) %>%
  ungroup()

# prep data

pcs_stage_data <- dbGetQuery(con, "SELECT date, rider, stage, race, year, class FROM pcs_stage_data WHERE time_trial = 0") %>%
  mutate(rider = str_to_title(rider)) %>%
  mutate(date = as.Date(date, origin = '1970-01-01')) %>%
  unique()

# gam mod

GAM_MOD <- read_rds('model-climb-difficulty.rds')

# alt mod

model_data <- tibble(elev = seq(0,14000,1000), 
                     aap = c(0.999, 0.992, 0.983, 0.972, 0.959, 0.944, 0.927, 0.907, 0.886, 0.863, 0.837, 0.809, 0.78, 0.748, 0.714)) %>% 
  mutate(elev = elev * 0.3048)

mgcv::gam(aap ~ s(elev, k = 5), data = model_data) -> gam_mod

# power required model

power_req_mod <- read_rds("Stored models/power-required-throughout-race-gam.rds")

# strava data

strava_act_data <- dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Distance')") %>% 
  
  # clean up the dates
  mutate(Y = str_sub(DATE, nchar(DATE)-3, nchar(DATE))) %>% 
  separate(DATE, into = c("weekday", "date", "drop"), sep = ",") %>% 
  mutate(date = paste0(str_trim(date),", ", Y)) %>% 
  select(-weekday, -drop, -Y, -VALUE, -Stat) %>% 
  
  mutate(date = lubridate::mdy(date)) %>% 
  unique() %>%
  janitor::clean_names()

# already written

st_chars <- dbGetQuery(con, "SELECT stage, race, year, class FROM strava_stage_characteristics") %>%
  unique()

st_climbs <- dbGetQuery(con, "SELECT stage, race, year, rider FROM climbs_from_strava_telemetry") %>%
  unique()

skip = 0

all_race_activities <- all_race_activities %>% anti_join(st_chars)

#
#
#
#
# DEFINE FUNCTION

extract_telemetry <- function(act_page) {
  
  data_lists <- read_rds(paste0("D:/Jake/Documents/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
  
  # clean this up before writing to DB
  
  df <- cbind(
    
    altitude = data_lists[["altitude"]],
    distance = data_lists[["distance"]],
    time = data_lists[["time"]],
    #watts = data_lists[["watts_calc"]],
    activity_id = ACTIVITY
    
  ) %>%
    
    #cbind(as_tibble(data_lists$latlng)) %>%
    
    #rename(latitude = V1,
    #       longitude = V2) %>%
    
    as_tibble() %>%
    mutate(altitude = as.numeric(altitude),
           distance = as.numeric(distance),
           #watts = as.numeric(watts),
           time = as.numeric(time)) %>%
    mutate(distance = round(distance, 0),
           altitude = round(altitude, 0))
  
  vertical_gain <- df %>% 
    mutate(vg = altitude - lag(altitude), 
           vg = ifelse(is.na(vg), 0, ifelse(vg < 0, 0, vg))) %>% 
    mutate(cum_vg = cumsum(vg)) %>% 
    
    group_by(distance = floor(distance / 500)/2, 
             activity_id) %>% 
    summarize(vertical_gain = mean(cum_vg, na.rm = T),
              altitude = max(altitude, na.rm = T)) %>%
    ungroup()
  
  #
  
  dbWriteTable(con, "strava_stats_by_kilometer", vertical_gain, append = TRUE, row.names = FALSE)
  
  # set up data for stage characteristics code
  
  all_routes <- df %>%
    
    inner_join(strava_act_data, by = c("activity_id")) %>%
    
    inner_join(pcs_stage_data, by = c("date", "pcs" = "rider")) %>% 
    
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
  
  # calc stage chars

  final_1km <- all_routes %>%

    group_by(stage, class, year, race, rider) %>%
    mutate(stage_end = max(length, na.rm = T)) %>%
    ungroup() %>%

    filter((stage_end - distances) < 1.1) %>%
    
    mutate(vert_gain = elevations - lag(elevations),
           vert_gain = ifelse(vert_gain > 0, vert_gain, 0)) %>%

    group_by(stage, class, year, race, rider) %>%
    summarize(final_1km_elev = mean(elevations, na.rm = T),
              final_1km_vertgain = sum(vert_gain, na.rm = T),
              max_gradient = max(grades, na.rm = T),
              med_gradient = median(grades, na.rm = T),
              avg_gradient = mean(grades, na.rm = T),
              x25th_gradient = quantile(grades, probs = 0.75, na.rm = T),
              n = n()) %>%
    ungroup() %>%

    mutate(final_1km_gradient = (med_gradient + avg_gradient + x25th_gradient) / 3) %>%

    select(stage, class, year, race, rider, final_1km_elev, final_1km_gradient, final_1km_vertgain)
  #

  final_5km <- all_routes %>%

    group_by(stage, class, year, race, rider) %>%
    mutate(stage_end = max(length, na.rm = T)) %>%
    ungroup() %>%

    filter((stage_end - distances) < 5.1) %>%
    
    mutate(vert_gain = elevations - lag(elevations),
           vert_gain = ifelse(vert_gain > 0, vert_gain, 0)) %>%
    
    group_by(stage, class, year, race, rider) %>%
    summarize(final_5km_elev = mean(elevations, na.rm = T),
              final_5km_vertgain = sum(vert_gain, na.rm = T),
              max_gradient = max(grades, na.rm = T),
              med_gradient = median(grades, na.rm = T),
              avg_gradient = mean(grades, na.rm = T),
              x25th_gradient = quantile(grades, probs = 0.75, na.rm = T),
              n = n()) %>%
    ungroup() %>%

    mutate(final_5km_gradient = (med_gradient + avg_gradient + x25th_gradient) / 3) %>%

    select(stage, class, year, race, rider, final_5km_elev, final_5km_gradient, final_5km_vertgain)

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

  percentage_climbing_start <- all_routes %>%

    arrange(stage, year, race, class, rider, points) %>%

    group_by(stage, class, year, race, rider) %>%
    mutate(stage_end = max(length, na.rm = T)) %>%
    ungroup() %>%

    mutate(first_30km = ifelse((distances) < 30.6, grades, NA)) %>%

    mutate(distance_chunks = distances - lag(distances),
           distance_chunks = ifelse(distances == 0, NA, distance_chunks)) %>%

    mutate(vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * first_30km, 0),
           total_vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * grades, 0)) %>%

    group_by(stage, class, year, race, rider) %>%
    summarize(first_30km_vert_gain = sum(vert_gain, na.rm = T),
              total_vert_gain = sum(total_vert_gain, na.rm = T)) %>%
    ungroup() %>%

    mutate(perc_gain_start = first_30km_vert_gain / total_vert_gain)

  # data below taken from a paper showing aerobic power at certain elevation levels (in feet) compared to sea-level
  # eg, at 14,000 feet it's about 71% of at sea-level

  weighted_alt <- cbind(all_routes, pred = all_routes %>%
                          rename(elev = elevations) %>%
                          mgcv::predict.gam(object = gam_mod)) %>%
    group_by(stage, race, year, class, rider) %>%
    summarize(weighted_altitude = mean(pred, na.rm = T)) %>%
    ungroup()

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

      weighted_alt, by = c("stage", "race", "year", "class", "rider")

    ) %>%
    inner_join(

      percentage_climbing_in_final_climb, by = c("stage", "race", "year", "class", "rider")

    ) %>%
    inner_join(

      percentage_climbing_start %>%
        select(-total_vert_gain), by = c("stage", "race", "year", "class", "rider")

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

  # write to DB

  dbWriteTable(con, "strava_stage_characteristics", stage_characteristics %>% select(-metric), append = T, row.names = F)
  
  #
  #
  # extract climbs
  #
  #
  
  if(skip == 0) {
  
  tdf14_2020 <- all_routes %>%
    
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
    mutate(everytenth = floor(every_km/0.25)/4) %>%
    
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
    filter(gradient > 0.025)
  
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
  
  if(length(different_groupings$grouping) == 0) {
    
  } else {
    
    #
    
    data_climbs <- cbind(different_groupings,
                         
                         model_category = mgcv::predict.gam(GAM_MOD,
                                                            different_groupings %>%
                                                              mutate(vam_poly = ((gradient^2)*length)) %>%
                                                              mutate(alt = end_elev - 1000))) %>%
      mutate(perc_thru = 1 - (end_km / stage_length))
    
    #
    
    final_data_climbs <- cbind(
      
      data_climbs,
      
      power_required = mgcv::predict.gam(power_req_mod, data_climbs)) %>%
      
      cbind(best = mgcv::predict.gam(power_req_mod, tibble(perc_thru = 1))) %>%
      
      mutate(rel_to_best = power_required / best) %>%
      
      select(-best, -power_required) %>%
      
      rename(power_required = rel_to_best) %>%
      
      mutate(power_model_category = power_required * model_category)
    
    #
    
    all_2021_koms <- final_data_climbs %>%
      select(-grouping, -stage_length, -start_km, -end_km, -start_elev) %>%
      rename(alt = end_elev) %>%
      mutate(alt = round(alt, 0),
             gain = round(gain, 0),
             gradient = round(gradient, 3),
             model_category = round(model_category, 1),
             perc_thru = round(perc_thru, 2),
             power_required = round(power_required, 2),
             power_model_category = round(power_model_category, 1))
    
    dbWriteTable(con, "climbs_from_strava_telemetry", all_2021_koms, append = TRUE, row.names = F)
    
  }
  
  }
  
}

#
# 
# 

tictoc::tic()

for(a in 1:length(all_race_activities$activity_id)) {
  
  ACTIVITY <- all_race_activities$activity_id[[a]]
  
  act_page <- paste0('https://www.strava.com/activities/', 
                     
                     ACTIVITY, 
                     
                     '/streams?stream_types%5B%5D=time',
                     #'&stream_types%5B%5D=watts_calc',
                     '&stream_types%5B%5D=altitude&stream_types%5B%5D=heartrate&stream_types%5B%5D=distance')
  
  safe_segments <- safely(.f = extract_telemetry, otherwise = print("error"))
  
  safe_segments(act_page)
  
  print(a)
  
}

tictoc::toc()

#
#
#
#
#
#
#
#