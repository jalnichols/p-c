
library(tidyverse)
library(rvest)
library(DBI)

options(tidyverse.quiet = TRUE)

dbDisconnect(con)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      port = 5432,
                      host = 'localhost',
                      dbname = "cycling",
                      user = "postgres",
                      password = "braves")
#
#
#

all_race_activities <- dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Distance')") %>% 
  mutate(pcs = ifelse(pcs %in% c("Oconnor Ben", "OConnor Ben"), "O'connor Ben",
                      ifelse(pcs %in% c("Obrien Kelland", "OBrien Kelland"), "O'brien Kelland", pcs))) %>%
  rename(DATE = date, VALUE = value, PCS = pcs, Stat = stat) %>%
  
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
  
  inner_join(dbGetQuery(con, "SELECT rider, date, stage, race, year, class, length, stage_type, missing_profile_data, url, time_trial
                        FROM pcs_stage_data") %>%
              
               mutate(date = as.Date(date, origin = '1970-01-01')) %>%
               mutate(rider = str_to_title(rider)) %>%
               unique(), by = c("date", "pcs" = "rider")) %>% 
  
  # if two results exist for same day matching distance, it's probably a recon and TT which
  # means drop the lower watts
  
  # also, many riders include distance outside the TT as part of their strava activity
  # so maybe accept any riders +/- 10 km? or maybe we just can't get accurate TT data
  
  mutate(distance = distance * 1.609) %>% 
  filter((distance / length) > 0.90) %>%
  filter((distance / length) < 1.10) %>%
  
  inner_join(
    
    fs::dir_info('C:/Users/Jake Nichols/Documents/Old D Drive/STRAVA_JSON/') %>%
      select(path, modification_time) %>%
      mutate(activity_id = str_replace(path, 'C:/Users/Jake Nichols/Documents/Old D Drive/STRAVA_JSON/strava-activity-id-', ''),
             activity_id = str_replace(activity_id, ".rds", "")), by = c("activity_id")) %>%
  
  group_by(stage, race, year, class, date, length) %>%
  filter(rank(abs((distance/length)-1), ties.method = 'random') <= 3) %>%
  ungroup() %>%
  
  arrange(desc(date)) %>%
  
  group_by(stage, race, year, class, date, length) %>%
  arrange(rank(abs((distance/length)-1), ties.method = 'random')) %>%
  ungroup() %>%
 
  filter(modification_time > (lubridate::today() - 7))


# prep data

pcs_stage_data <- dbGetQuery(con, "SELECT date, rider, stage, race, year, class, url
                             FROM pcs_stage_data WHERE time_trial = 0") %>%
  mutate(rider = str_to_title(rider)) %>%
  mutate(date = as.Date(date, origin = '1970-01-01')) %>%
  unique()

# gam mod

GAM_MOD <- read_rds('Stored models/model-climb-difficulty.rds')

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
  
  rename(DATE = date, VALUE = value, PCS = pcs, Stat = stat) %>%
  
  # clean up the dates
  mutate(Y = str_sub(DATE, nchar(DATE)-3, nchar(DATE))) %>% 
  separate(DATE, into = c("weekday", "date", "drop"), sep = ",") %>% 
  mutate(date = paste0(str_trim(date),", ", Y)) %>% 
  select(-weekday, -drop, -Y, -VALUE, -Stat) %>% 
  
  mutate(date = lubridate::mdy(date)) %>% 
  unique() %>%
  janitor::clean_names()

# from https://docs.google.com/spreadsheets/d/1-LdY9TWBxCFMLXM2HeFMuvTKcyOn0EzXLiYrlG_8kCo/edit#gid=0

weight_adv <- tibble(gradient = seq(-6,12,1),
                     difference = c(1.27, 1.16, 1.11, 1.08, 1.065, 1.054, 1.046, 1.041, 1.035, 
                                    1.032, 1.03, 1.028, 1.027, 1.026, 1.026, 1.025, 1.025, 1.025, 1.025))

drafting_bene <- tibble(gradient = seq(-4,12,1),
                        drafting_bene = c(1, 0.83, 0.61, 0.46, 0.34, 0.26, 0.17, 0.13, 0.095, 
                                          0.068, 0.052, 0.036, 0.027, 0.02, 0.015, 0.012, 0.008))

# already written

st_chars <- dbGetQuery(con, "SELECT stage, url FROM strava_stage_characteristics") %>% unique()
st_climbs <- dbGetQuery(con, "SELECT stage, url FROM climbs_from_strava_telemetry") %>% unique()

skip = 0

all_race_activities <- all_race_activities %>% 
  anti_join(st_chars, by = c("stage", "url")) %>%
  arrange(desc(date)) %>%
  filter(year == 2023)

#
#
#
#
# DEFINE FUNCTION

extract_telemetry <- function(ACTIVITY) {
  
  data_lists <- read_rds(paste0("C:/Users/Jake Nichols/Documents/Old D Drive/STRAVA_JSON/strava-activity-id-", ACTIVITY, ".rds"))
  
  # clean this up before writing to DB
  
  df <- cbind(
    
    altitude = data_lists[["altitude"]],
    distance = data_lists[["distance"]],
    time = data_lists[["time"]],
    #watts = data_lists[["watts_calc"]],
    activity_id = as.character(ACTIVITY)
    
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
    mutate(updown = altitude - lag(altitude), 
           updown = ifelse(is.na(updown), 0, updown), 
           vg = ifelse(is.na(updown), 0, ifelse(updown < 0, 0, updown))) %>% 
    mutate(cum_vg = cumsum(vg)) %>% 
    
    group_by(distance = floor(distance / 500)/2, 
             activity_id) %>% 
    summarize(vertical_gain = mean(cum_vg, na.rm = T),
              total_vg = sum(vg, na.rm = T),
              altitude = max(altitude, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(perc_thru = distance / (all_race_activities %>% filter(activity_id == ACTIVITY) %>% select(distance) %>% .[[1]])) %>%
    
    mutate(power_required = mgcv::predict.gam(power_req_mod, .),
           best = mgcv::predict.gam(power_req_mod, tibble(perc_thru = 1)),
           rel_to_best = power_required / best,
           weighted_vg = total_vg * rel_to_best) %>%
    
    select(activity_id, distance, total_vg, weighted_vg, perc_thru)
  
  #
  #
  #dbWriteTable(con, "strava_stats_by_kilometer", vertical_gain, append = TRUE, row.names = FALSE)
  
  # set up data for stage characteristics code
  
  all_routes <- df %>%
    
    inner_join(strava_act_data, by = c("activity_id")) %>%
    
    inner_join(pcs_stage_data, by = c("date", "pcs" = "rider")) %>% 
    
    rename(dist = distance,
           alt = altitude,
           rider = pcs) %>%
    
    mutate(dist = floor(dist / 50) * 0.05) %>%
    
    group_by(stage, race, year, class, rider, dist, url) %>%
    summarize(alt = median(alt, na.rm = T)) %>%
    ungroup() %>%
    
    arrange(year, race, stage, class, rider, dist) %>%
    
    group_by(year, race, stage, class, rider, url) %>%
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
  
  if(skip == 1) {
    
  } else {
  
  # calc stage chars

  final_1km <- all_routes %>%

    group_by(stage, class, year, race, rider, url) %>%
    mutate(stage_end = max(length, na.rm = T)) %>%
    ungroup() %>%

    filter((stage_end - distances) < 1.1) %>%
    
    mutate(vert_gain = elevations - lag(elevations),
           vert_gain = ifelse(vert_gain > 0, vert_gain, 0)) %>%

    group_by(stage, class, year, race, rider, url) %>%
    summarize(final_1km_elev = mean(elevations, na.rm = T),
              final_1km_vertgain = sum(vert_gain, na.rm = T),
              max_gradient = max(grades, na.rm = T),
              med_gradient = median(grades, na.rm = T),
              avg_gradient = mean(grades, na.rm = T),
              x25th_gradient = quantile(grades, probs = 0.75, na.rm = T),
              n = n()) %>%
    ungroup() %>%

    mutate(final_1km_gradient = (med_gradient + avg_gradient + x25th_gradient) / 3) %>%

    select(stage, class, year, race, rider, url, final_1km_elev, final_1km_gradient, final_1km_vertgain)
  #

  final_5km <- all_routes %>%

    group_by(stage, class, year, race, rider, url) %>%
    mutate(stage_end = max(length, na.rm = T)) %>%
    ungroup() %>%

    filter((stage_end - distances) < 5.1) %>%
    
    mutate(vert_gain = elevations - lag(elevations),
           vert_gain = ifelse(vert_gain > 0, vert_gain, 0)) %>%
    
    group_by(stage, class, year, race, rider, url) %>%
    summarize(final_5km_elev = mean(elevations, na.rm = T),
              final_5km_vertgain = sum(vert_gain, na.rm = T),
              max_gradient = max(grades, na.rm = T),
              med_gradient = median(grades, na.rm = T),
              avg_gradient = mean(grades, na.rm = T),
              x25th_gradient = quantile(grades, probs = 0.75, na.rm = T),
              n = n()) %>%
    ungroup() %>%

    mutate(final_5km_gradient = (med_gradient + avg_gradient + x25th_gradient) / 3) %>%

    select(stage, class, year, race, url, rider, final_5km_elev, final_5km_gradient, final_5km_vertgain)

  #

  percentage_climbing_in_final_climb <- all_routes %>%

    arrange(stage, year, race, class, rider, points) %>%

    group_by(stage, class, year, race, rider, url) %>%
    mutate(stage_end = max(length, na.rm = T)) %>%
    ungroup() %>%

    mutate(final_20km = ifelse((stage_end - distances) < 20.6, grades, NA)) %>%

    mutate(distance_chunks = distances - lag(distances),
           distance_chunks = ifelse(distances == 0, NA, distance_chunks)) %>%

    mutate(vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * final_20km, 0),
           total_vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * grades, 0)) %>%

    group_by(stage, class, year, race, rider, url) %>%
    summarize(final_20km_vert_gain = sum(vert_gain, na.rm = T),
              total_vert_gain = sum(total_vert_gain, na.rm = T)) %>%
    ungroup() %>%

    mutate(perc_gain_end = final_20km_vert_gain / total_vert_gain)

  #

  percentage_climbing_start <- all_routes %>%

    arrange(stage, year, race, class, rider, points) %>%

    group_by(stage, class, year, race, rider, url) %>%
    mutate(stage_end = max(length, na.rm = T)) %>%
    ungroup() %>%

    mutate(first_30km = ifelse((distances) < 30.6, grades, NA)) %>%

    mutate(distance_chunks = distances - lag(distances),
           distance_chunks = ifelse(distances == 0, NA, distance_chunks)) %>%

    mutate(vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * first_30km, 0),
           total_vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * grades, 0)) %>%

    group_by(stage, class, year, race, rider, url) %>%
    summarize(first_30km_vert_gain = sum(vert_gain, na.rm = T),
              total_vert_gain = sum(total_vert_gain, na.rm = T)) %>%
    ungroup() %>%

    mutate(perc_gain_start = first_30km_vert_gain / total_vert_gain)

  # data below taken from a paper showing aerobic power at certain elevation levels (in feet) compared to sea-level
  # eg, at 14,000 feet it's about 71% of at sea-level

  weighted_alt <- cbind(all_routes, pred = all_routes %>%
                          rename(elev = elevations) %>%
                          mgcv::predict.gam(object = gam_mod)) %>%
    group_by(stage, race, year, class, rider, url) %>%
    summarize(weighted_altitude = mean(pred, na.rm = T)) %>%
    ungroup()

  #

  lumpiness <- all_routes %>%

    arrange(stage, year, race, class, rider, points) %>%

    mutate(distance_chunks = distances - lag(distances),
           distance_chunks = ifelse(distances == 0, NA, distance_chunks)) %>%

    mutate(total_elev_change = ifelse(abs(grades) > 0.02, abs(elevations - lag(elevations)), 0),
           over_6_percent = ifelse(grades >= 0.06, 50, 0)) %>%
    
    mutate(updown = elevations - lag(elevations), 
           updown = ifelse(is.na(updown), 0, updown),
           cum_updown = cumsum(updown)) %>% 

    group_by(stage, class, year, race, rider, url) %>%
    summarize(total_elev_change = sum(total_elev_change, na.rm = T),
              stage_end = max(length, na.rm = T),
              cumul_updown = mean(cum_updown, na.rm = T),
              over_6_percent = sum(over_6_percent/1000, na.rm = T),
              time_at_1500m = mean(elevations > 1499.99, na.rm = T)) %>%
    ungroup() %>%

    mutate(perc_elev_change = total_elev_change / (stage_end * 1000))

  #

  stage_characteristics <- all_routes %>%

    filter(!(is.na(points))) %>%
    filter(points == 1) %>%
    select(-points, -distances, -elevations, -grades) %>%
    
    mutate(activity_id = as.character(ACTIVITY)) %>%

    inner_join(

      lumpiness %>%
        select(-stage_end), by = c("stage", "race", "year", "class", "rider", "url")

    ) %>%

    inner_join(

      weighted_alt, by = c("stage", "race", "year", "class", "rider", "url")

    ) %>%
    inner_join(

      percentage_climbing_in_final_climb, by = c("stage", "race", "year", "class", "rider", "url")

    ) %>%
    inner_join(

      percentage_climbing_start %>%
        select(-total_vert_gain), by = c("stage", "race", "year", "class", "rider", "url")

    ) %>%
    inner_join(

      final_1km, by = c("stage", "race", "year", "class", "rider", "url")

    ) %>%
    inner_join(

      final_5km, by = c("stage", "race", "year", "class", "rider", "url")

    ) %>%
    inner_join(
      
      vertical_gain %>%
        select(activity_id, weighted_vg) %>%
        group_by(activity_id) %>%
        summarize(weighted_vg = sum(weighted_vg, na.rm = T)) %>%
        ungroup(), by = c("activity_id")
      
    ) %>%
    
    cbind(all_routes %>%
            filter(!is.na(grades)) %>%
            mutate(grades = round(grades,2),
                   grades = ifelse(grades < -0.06, -0.06, ifelse(grades > 0.12, 0.12, grades))) %>%
            inner_join(weight_adv %>% mutate(grades = gradient/100), by = c("grades")) %>%
            cbind(weight_adv %>% filter(gradient == 0) %>% 
                    select(flat = difference)) %>% 
            mutate(rel = difference/flat) %>% 
            summarize(weight_adv = mean(rel, na.rm = T))) %>%
    
    cbind(all_routes %>%
            filter((distances / length) > 0.8) %>%
            filter(!is.na(grades)) %>%
            mutate(grades = round(grades,2),
                   grades = ifelse(grades < 0, 0, ifelse(grades > 0.12, 0.12, grades))) %>%
            inner_join(drafting_bene %>% mutate(grades = gradient/100), by = c("grades")) %>%
            summarize(drafting_bene = mean(drafting_bene, na.rm = T))) %>%
    
    filter(total_vert_gain >= 0) %>%

    gather(stat, value, -stage, -race, -year, -class, -rider, -url) %>%

    group_by(stat, stage, race, year, class, url) %>%
    summarize(value = median(value, na.rm = T)) %>%
    ungroup()

  # write to DB

  dbWriteTable(con, "strava_stage_characteristics", stage_characteristics, append = T, row.names = F)
  
  }
  
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
    
    group_by(every_km2, length, year, race, stage, rider, url) %>%
    summarize(
      
      left_km = min(left_km, na.rm = T),
      elevations = mean(elevations, na.rm = T)
      
    ) %>%
    ungroup() %>%
    
    arrange(rider, year, race, stage, every_km2) %>%
    
    mutate(every_km = floor(every_km2/0.25)/4) %>%
    #mutate(everytenth = floor(every_km2/0.1)/10) %>%
    
    mutate(every_km = ifelse(left_km < 0, everytenth, every_km),
           gradient = (elevations - lead(elevations)) / ((lead(left_km)-left_km)*1000)) %>%
    
    group_by(every_km, length, year, race, stage, rider, url) %>%
    summarize(min = min(elevations, na.rm = T),
              max = max(elevations, na.rm = T),
              gradient = mean(gradient, na.rm = T),
              elevations = max(elevations, na.rm = T),
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
    
    #mutate(every_km3 = floor(every_km)) %>%
    #mutate(everytenth = floor(every_km/0.25)/4) %>%
    
    #mutate(every_km3 = ifelse(every_km < 3, everytenth, every_km3)) %>%
    
    mutate(segment_distance = every_km - lead(every_km),
           segment_distance = ifelse(is.na(segment_distance), lag(segment_distance), segment_distance)) %>%
    
    group_by(every_km, length, year, race, stage, rider, url) %>%
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
    
    group_by(grouping, stage_length = length, race, stage, year, rider, url) %>%
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
  
  different_groupings <- tdf14_2020 %>%
    
    filter(change_gradient == "downhill") %>%
    
    group_by(grouping, stage_length = length, race, stage, year, rider, url) %>%
    summarize(start_km = max(every_km, na.rm = T),
              end_km = min(every_km, na.rm = T),
              start_elev = min(elevations, na.rm = T),
              end_elev = max(elevations, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(length = start_km - end_km,
           gain = start_elev - end_elev,
           gradient = gain / (length * 1000)) %>%
    
    filter(length > 0) %>%
    filter(gradient < -0.025)
  
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
                         
                         model_category = -1 * mgcv::predict.gam(GAM_MOD,
                                                            different_groupings %>%
                                                              mutate(vam_poly = ((gradient^2)*length)) %>%
                                                              mutate(alt = end_elev - 1000))) %>%
      mutate(perc_thru = 1 - (end_km / stage_length))
    
    #
    
    all_2021_koms <- data_climbs %>%
      select(-grouping, -stage_length, -start_km, -end_km, -start_elev) %>%
      rename(alt = end_elev) %>%
      mutate(alt = round(alt, 0),
             gain = round(gain, 0),
             gradient = round(gradient, 3),
             model_category = round(model_category, 1),
             perc_thru = round(perc_thru, 2),
             power_required = as.numeric(NA),
             power_model_category = as.numeric(NA))
    
    dbWriteTable(con, "downhills_from_strava_telemetry", all_2021_koms, append = TRUE, row.names = F)
    
  }
  
  }
  
}

#
# 
# 

tictoc::tic()

for(a in 1:length(all_race_activities$activity_id)) {
  
  ACTIVITY <- all_race_activities$activity_id[[a]]
  
  safe_segments <- safely(.f = extract_telemetry, otherwise = print("error"))
  
  safe_segments(ACTIVITY)
  
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