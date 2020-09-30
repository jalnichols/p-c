
library(tidyverse)
library(DBI)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#####
#####
##### Bring in data

all_stage_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE year > 2016") %>%
  
  mutate(date = as.Date(date))

#####
#####
##### calculate performance average based on windows of time / pred climb difficulty

pcd_windows <- tribble(
  
  ~floor_pcd, ~low_pcd, ~hi_pcd,
  0, 0, 2,
  1, 0, 2.5,
  2, 1, 3,
  3, 2, 4,
  4, 3, 5,
  5, 4, 6.5,
  6, 4.5, 7.5,
  7, 5.5, 8.5,
  8, 6, 10,
  9, 7, 11,
  10, 7.5, 12.5,
  11, 8.5, 13.5,
  12, 9, 15,
  13, 10, 16,
  14, 11, 18,
  15, 12, 20,
  16, 12.5, 22,
  17, 13, 24,
  18, 14, 35, 
  19, 15, 35, 
  20, 15, 35

)

#

stages_to_window <- all_stage_data %>%
  
  filter(!is.na(pred_climb_difficulty)) %>%
  
  select(stage, race, year, date, pred_climb_difficulty) %>%
  unique() %>%
  
  mutate(floor_pcd = floor(pred_climb_difficulty),
         floor_pcd = ifelse(floor_pcd < 0, 0,
                            ifelse(floor_pcd > 20, 20, floor_pcd))) %>%
  
  inner_join(pcd_windows, by = c("floor_pcd")) %>%
  
  filter(year >= 2019)

#

res_list <- vector("list", length(stages_to_window$date))

#

for(r in 1:length(stages_to_window$date)) {
  
  lo = stages_to_window$low_pcd[[r]]
  hi = stages_to_window$hi_pcd[[r]]
  da_te = stages_to_window$date[[r]]
  
  df <- all_stage_data %>%
    
    filter(pred_climb_difficulty >= lo & pred_climb_difficulty <= hi & date < da_te & date >= (da_te - 730)) %>%
    
    group_by(rider) %>%
    summarize(win_rate = mean(rnk == 1, na.rm = T),
              podium_rate = mean(rnk <= 3, na.rm = T),
              t10_rate = mean(rnk <= 10, na.rm = T),
              success_rate = mean(success == 1, na.rm = T),
              team_leader = mean(tm_pos == 1, na.rm = T),
              dnf_rate = mean(rnk == 200, na.rm = T),
              sof_rate = mean(sof, na.rm = T),
              races = n()) %>%
    ungroup() %>%
    
    mutate(DATE = da_te,
           race = stages_to_window$race[[r]],
           stage = stages_to_window$stage[[r]],
           year = stages_to_window$year[[r]],
           pcd = stages_to_window$pred_climb_difficulty[[r]])
  
  res_list[[r]] <- df
  
  if(r %% 10 == 0) {
    
    print(r)
    
  }
  
}

#

all_pre <- all_stage_data %>%
  
  inner_join(
    
    bind_rows(res_list), by = c("rider", "stage", "year", "race")
    
  )

#####
#####
##### Can we get signal out of where domestiques finish and how much time they lose?

time_lost_to_winner <- all_stage_data %>%
  
  filter(pred_climb_difficulty >= 8) %>%
  
  group_by(stage, race, year) %>%
  mutate(median_lost = median(gain_gc, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(rider) %>%
  summarize(avg_time_lost = mean(gain_gc / 60, na.rm = T),
            vs_median_lost = mean((gain_gc - median_lost) / 60, na.rm = T),
            avg_race_median = mean(median_lost / 60, na.rm = T),
            races = n()) %>%
  ungroup()




dates_list <- all_stage_data %>%
  
  filter(pred_climb_difficulty >= 8) %>%
  
  select(date) %>%
  
  unique() %>%
  
  filter(date > '2016-01-01')

#

res_list <- vector("list", length(dates_list$date))

#

for(i in 1:length(res_list)) {
  
  time_lost_to_winner <- all_stage_data %>%
    
    filter(pred_climb_difficulty >= 8 & date < dates_list$date[[i]] & date > (dates_list$date[[i]] - 1100)) %>%
    
    group_by(stage, race, year) %>%
    mutate(median_lost = median(gain_gc, na.rm = T)) %>%
    ungroup() %>%
    
    group_by(rider) %>%
    summarize(avg_time_lost = mean(gain_gc / 60, na.rm = T),
              vs_median_lost = mean((gain_gc - median_lost) / 60, na.rm = T),
              avg_race_median = mean(median_lost / 60, na.rm = T),
              races = n()) %>%
    ungroup() %>%
    
    mutate(date = dates_list$date[[i]])
  
  res_list[[i]] <- time_lost_to_winner
  
  print(i)
  
}

all_pre_mtn <- bind_rows(res_list) %>%
  
  inner_join(all_stage_data, by = c("rider", "date")) %>%
  
  mutate()

#
#
#

#

library(tidyverse)
library(RMySQL)
library(BradleyTerry2)

#

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#
#
#

All_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf
                       WHERE time_trial = 0 AND year >= 2017") %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  
  mutate(points_per_opp = ifelse(tm_pos == 1, points_finish, NA),
         sof_per_opp = ifelse(tm_pos == 1, sof, NA),
         pred_climb_diff_opp = ifelse(tm_pos == 1, pred_climb_difficulty, NA),
         pred_climb_diff_succ = ifelse(points_finish > 0, pred_climb_difficulty, NA),
         team_ldr = ifelse(tm_pos == 1, 1, 0)) %>%
  
  mutate(date = as.Date(date)) %>%
  
  select(-stage_name, -speed, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th,
         -time_trial, -gc_winner, -gc_pos, -parcours_value, -stage_type) %>%
  
  filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC")) |
           (class %in% c("2.1", "1.1") & Tour == "Europe Tour") | 
           (sof > 0.25 & class %in% c("2.1", "1.1"))) %>%
  unique()

#
#
#

sprint_h2h_data <- All_data %>%
  
  filter(bunch_sprint == 1) %>%
  filter(!is.na(rnk)) %>%
  filter(rnk < 21)

#

sprint_h2h <- sprint_h2h_data %>%
  
  rename(rnk1 = rnk,
         rider1 = rider) %>%
  
  inner_join(
    
    sprint_h2h_data %>%
      select(rider2 = rider,
             stage, race, year,
             rnk2 = rnk), by = c("stage", "race", "year")) %>%
  
  filter(!rider1 == rider2) %>%
  
  mutate(adv1 = ifelse(rnk1 < rnk2, 1, 0),
         adv2 = ifelse(rnk1 > rnk2, 1, 0)) %>%
  
  filter(year >= 2017) %>%
  
  group_by(rider1, rider2) %>%
  summarize(win1 = sum(adv1, na.rm = T),
            win2 = sum(adv2, na.rm = T)) %>%
  ungroup()

#

mod1 <- BradleyTerry2::BTm(cbind(win1, win2), rider1, rider2,
            formula = ~ rider, id = "rider",
            data = sprint_h2h)

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

uphill_TT <- all_stage_data %>%
  
  filter(time_trial == 1) %>%
  mutate(x5th = ifelse(rnk == 5, speed, NA)) %>%
  
  group_by(stage, race, year) %>%
  mutate(x5th = mean(x5th, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(rel_speed = speed / x5th)

#
#
#
#
#
#

strava_final_climbs <- dbReadTable(con, "strava_finalclimbsegments") %>%
  
  mutate(time_char = nchar(Time),
         seconds = str_sub(Time, time_char-1, time_char),
         minutes = ifelse(time_char>4, str_sub(Time, time_char-4, time_char-3), str_sub(Time, time_char-3, time_char-3)),
         hours = ifelse(time_char>6, str_sub(Time, time_char-6, time_char-6), 0),
         seconds = as.numeric(seconds),
         minutes = as.numeric(minutes),
         hours = as.numeric(hours),
         climb_seconds = (hours * 3600)+(minutes*60)+(seconds),
         kilometers = DistanceMiles * 1.60934,
         vertical_meters = (EndElevFeet - StartElevFeet)*0.3048,
         gradient = vertical_meters / (kilometers * 1000),
         power = as.numeric(str_replace(Power, "W", "")),
         km_hour = kilometers / (climb_seconds / 3600)) %>%
  
  group_by(SEGMENT, CLIMB) %>%
  mutate(act_Rank = rank(climb_seconds, ties.method = "first")) %>%
  ungroup() %>%
  
  left_join(
    
    read_csv("C:/Users/Jake/Documents/R Code/p-c/strava-final-climbs-integrated.csv"), by = c("SEGMENT", 'RACE', "CLIMB")
    
  ) %>%
  
  mutate(date = lubridate::mdy(Date))

#
# FIX DATES FOR THE RACES WITH REST DAYS
#

stage_data_final_climbs <- all_stage_data %>%
  
  mutate(date = ifelse(grand_tour == 1,
                       ifelse(race == "tour de france" & year == 2019,
                              ifelse(stage < 11, date + lubridate::days(0),
                                     ifelse(stage < 16, date + lubridate::days(1), date + lubridate::days(2))),
                              ifelse(race == "giro d'italia" & year %in% c(2017, 2018),
                                     ifelse(stage < 4, date,
                                            ifelse(stage < 10, date + 1,
                                                   ifelse(stage < 16, date + 2, date + 3))),
                                     ifelse(race == "la vuelta ciclista a espana" & year == '2019',
                                            ifelse(stage < 10, date, 
                                                   ifelse(stage < 17, date + 1, date + 2)),
                              ifelse(stage < 10, date + lubridate::days(0),
                                     ifelse(stage < 16, date + lubridate::days(1), date + lubridate::days(2)))))), 
                       date + lubridate::days(0)),
         date = as.Date(date, origin = '1970-01-01')) %>%
  
  inner_join(strava_final_climbs %>%
              select(SEGMENT, RACE, CLIMB,
                     race, year, stage, date,
                     kilometers, vertical_meters, gradient) %>%
               unique(), by = c("race", "year", "stage", "date")) %>%
  
  left_join(read_csv("C:/Users/Jake/Documents/R Code/p-c/riders-strava-profiles.csv") %>%
              select(-n), by = c("rider" = "PCS")) %>%
  
  left_join(strava_final_climbs %>%
              select(names, Name, race, stage, year, date,
                     power, km_hour, climb_seconds, act_Rank,
                     Rank, HR), by = c("names", "Name", "race", "year", "stage", "date")) %>%
  
  select(-stage_name, -parcours_value, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th, 
         -time_trial, -missing_profile_data, -uphill_finish, -points_finish, -leader_rating, -cobbles)

#
#
#
#

top_climbers_day <- stage_data_final_climbs %>%
  
  group_by(stage, race, year, CLIMB) %>%
  filter(min(climb_seconds, na.rm = T) == climb_seconds) %>%
  ungroup() %>%
  
  select(stage, race, year, CLIMB, climb_seconds, gradient, kilometers, pred_climb_difficulty,
         grand_tour, one_day_race, length, vertical_meters, km_hour) %>%
  unique()

#

time_climb_mod <- lm(km_hour ~ gradient + kilometers + pred_climb_difficulty, data = top_climbers_day)

summary(time_climb_mod)

preds <- cbind(pred_time = predict(time_climb_mod, top_climbers_day), 
               top_climbers_day) %>% 
  
  mutate(rel = km_hour / pred_time)
  
#
#
#
#
#

implied_climbing <- stage_data_final_climbs %>% 

  inner_join(dbGetQuery(con, "SELECT stage, race, year, rider, relevant FROM gc_relevance"),
             by = c("stage", "race", "year", "rider")) %>%
  
  filter(relevant == 1) %>%

  mutate(implied_winner = climb_seconds - gain_1st,
         implied_winner = ifelse(rnk < 26, implied_winner, NA)) %>% 
  
  group_by(stage, race, year, CLIMB) %>% 
  mutate(implied_winner = median(implied_winner, na.rm = T)) %>% 
  ungroup() %>% 
  
  mutate(implied_climb = implied_winner + gain_1st) %>% 
  select(-implied_winner) %>% 
  mutate(implied_km_hour = kilometers / (implied_climb / 3600)) %>% 
  
  group_by(race, stage, year, CLIMB) %>%
  mutate(daily_rank_climb = rank(-implied_km_hour, ties.method = "first")) %>%
  mutate(fifth = ifelse(daily_rank_climb == 5, implied_km_hour, NA),
         fifth = mean(fifth, na.rm = T)) %>%
  ungroup() %>% 
  
  mutate(rel_climbing = ifelse(is.na(climb_seconds), implied_km_hour / fifth, km_hour / fifth))

#
#
#
#
#
#
#

# link with stage level power data

stage_level_power <- dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Weighted Avg Power', 'Distance')") %>% 
  
  # I would like to bring in weight here so when I cut-off too low watts below it is watts/kg
  
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
  filter(!is.na(`Weighted Avg Power`)) %>% 
  janitor::clean_names() %>% 
  
  group_by(date) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  
  inner_join(all_stage_data %>%
               filter(year %in% c("2019", "2020")), by = c("date", "pcs" = "rider")) %>% 
  
  mutate(distance = distance * 1.609) %>% 
  filter((distance / length) > 0.9) %>%
  filter((distance / length) < 1.1) %>% 
  filter(weighted_avg_power > 175) %>% 
  
  group_by(rider = pcs) %>% 
  mutate(rel = weighted_avg_power / mean(weighted_avg_power, na.rm = T)) %>% 
  ungroup()

#
# stage level rels
#

stage_level_power %>% 
  group_by(stage, race, year, date) %>% 
  summarize(power = mean(weighted_avg_power, na.rm = T), 
            relative = mean(rel, na.rm = T), 
            n = n()) %>% ungroup() %>% 
  filter(n >= 5) %>% 
  
  arrange(-relative) -> stage_level_rel_power

#
# impact of rnk, DNF, and specific race on rel power
#

po_mod <- lme4::lmer(rel ~ log(rnk+1) + DNF + (1 | spec_race), 
                     
                     data = stage_level_power %>% 
                       mutate(DNF = ifelse(rnk == 200, 1, 0)) %>% 
                       mutate(spec_race = paste0(year,"-",stage,"-",race)))

#

summary(po_mod)

race_level_ranefs <- lme4::ranef(po_mod)[[1]] %>% rownames_to_column()