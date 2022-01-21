
library(tidyverse)
library(DBI)
library(RMySQL)

#

DBI::dbDisconnect(con)

con <- dbConnect(RMySQL::MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

stage_level_power <- dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Weighted Avg Power', 'Distance', 'AvgSpeed', 'AvgPower', 'AvgTemperature')") %>% 
  
  mutate(PCS = str_to_title(PCS)) %>%
  
  # I would like to bring in weight here so when I cut-off too low watts below it is watts/kg
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)) %>%
      filter(!is.na(weight)) %>%
      group_by(rider) %>%
      summarize(weight = median(weight)) %>%
      ungroup(), by = c("PCS" = "rider")) %>%
  
  # clean up the dates
  mutate(Y = str_sub(DATE, nchar(DATE)-3, nchar(DATE))) %>% 
  separate(DATE, into = c("weekday", "date", "drop"), sep = ",") %>% 
  mutate(date = paste0(str_trim(date),", ", Y)) %>% 
  select(-weekday, -drop, -Y) %>% 
  
  # clean up the stat values
  mutate(VALUE = str_replace(VALUE, "mi/h", ""), 
         VALUE = str_replace(VALUE, "mi", ""), 
         VALUE = str_replace(VALUE, "W", ""),
         VALUE = ifelse(Stat == "AvgTemperature",
                        str_sub(VALUE, 1, nchar(VALUE)-8), VALUE),
         VALUE = as.numeric(VALUE)) %>% 
  
  mutate(date = lubridate::mdy(date)) %>% 
  unique() %>% 
  spread(Stat, VALUE) %>% 
  
  janitor::clean_names() %>% 
  
  inner_join(dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE year > 2018") %>%
               
               mutate(date = as.Date(date)), by = c("date", "pcs" = "rider")) %>% 
  
  # if two results exist for same day matching distance, it's probably a recon and TT which
  # means drop the lower watts
  
  # also, many riders include distance outside the TT as part of their strava activity
  # so maybe accept any riders +/- 10 km? or maybe we just can't get accurate TT data
  
  mutate(distance = distance * 1.609) %>% 
  filter((distance / length) > 0.5) %>%
  filter((distance / length) < 1.2) %>%
  filter((time_trial == 1 & (distance / length) > 0.8) | time_trial == 0) %>%
  
  select(-win_seconds,
         -parcours_value, -stage_type,
         -leader_rating,
         -gain_40th, -gain_20th, -gain_10th) %>%
  
  mutate(avg_speed = ifelse(is.na(avg_speed), 
                            ifelse(is.na(speed), 0, speed), avg_speed)) %>%
  
  group_by(stage, race, year, rider = pcs, date, length) %>%
  filter(rank(-avg_speed, ties.method = "min") == 1) %>%
  ungroup() %>%
  
  unique() %>%
  
  group_by(stage, race, year, class, length, date) %>%
  mutate(avg_temperature = median(avg_temperature, na.rm = TRUE)) %>%
  ungroup()

#

distinct_positions_riders <- dbGetQuery(con, "SELECT DISTINCT rider, stage, race, year, class, date
                                   FROM strava_position_in_race") %>% 
  group_by(stage, race, year, class, date) %>%
  summarize(#covering = mean(covering_percentage), 
            riders = n_distinct(rider), 
            #segs = mean(total_segments)
            ) %>%
  ungroup() %>%
  
  filter(riders >= 20)

#

all_positions_riders <- dbGetQuery(con, "SELECT Segment, Gradient, Distance, race, stage, year, class, date, 
                          low_end, high_end, 
                          rider, time, best_time, median_time, behind_median, vs_median_before, position_at_time,
                          total_seconds, median_finish, bad_data
                          FROM strava_position_in_race") %>%
  
  inner_join(distinct_positions_riders, by = c("stage", "race", "year", "class", "date")) %>%
  
  mutate(date = as.Date(date),
         stage = as.character(stage)) %>%
  
  inner_join(stage_level_power %>%
               select(stage, race, year, class, length, date, pred_climb_difficulty, rider, rnk) %>%
               unique(), by = c("stage", "race", "year", "date", "class", "rider")) %>%
  
  #mutate(behind_median_start = behind_median - (time - median_time)) %>%
  
  group_by(stage, race, year, class, date, Segment, low_end) %>%
  mutate(best_placed_rk = min(rnk, na.rm = T)) %>%
  mutate(#best_placed = ifelse(rnk == best_placed_rk, behind_median, NA),
         best_placed_start = ifelse(rnk == best_placed_rk, vs_median_before, NA),
         #best_placed = mean(best_placed, na.rm = T),
         best_placed_start = mean(best_placed_start, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(#behind_best_placed = behind_median - best_placed,
         behind_best_placed_start = vs_median_before - best_placed_start)
  

#

all_positions_riders %>% 

  filter(behind_median < 4000) %>% 
  
  group_by(rider) %>% 
  filter(n_distinct(stage,race, year,class) >= 30) %>%
  ungroup() %>% 
  
  mutate(perc_thru = high_end/length,
         perc_thru = ifelse(perc_thru > 0.999, 0.999, perc_thru)) %>% 
  
  group_by(rider, stage, race, year, class, f = floor(perc_thru / 0.1)) %>% 
  summarize(behind = mean(behind_median_start/60, na.rm = T), 
            leaders = mean(behind_best_placed_start <= 15, na.rm = T),
            gap = mean(behind_median_start <= -60, na.rm = T)) %>% 
  ungroup() %>%
  
  group_by(rider, f) %>% 
  summarize(behind = mean(behind, na.rm = T), 
            leaders = mean(leaders, na.rm = T),
            gap = mean(gap, na.rm = T),
            races = n_distinct(stage, race, year, class)) %>% 
  ungroup() -> relative

#

relative %>%
  ggplot(aes(x = (f/10)+0.05, y = gap, group = rider))+
  geom_line(alpha=0.15, color = "dark red")+
  scale_x_continuous(breaks = seq(0,0.9,0.1), labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Percentage thru race", 
       y = "% of races with gap on median rider")

relative %>%
  ggplot(aes(x = (f/10)+0.05, y = leaders, group = rider))+
  geom_line(alpha=0.15, color = "dark red")+
  scale_x_continuous(breaks = seq(0,0.9,0.1), labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Percentage thru race", 
       y = "% of races with leaders still")

#

all_positions_riders %>% 
  
  filter(behind_median < 3000) %>% 
  
  group_by(rider) %>% 
  filter(n_distinct(stage,race, year,class) >= 20) %>%
  ungroup() %>% 
  
  mutate(perc_thru = high_end/length,
         perc_thru = ifelse(perc_thru > 0.999, 0.999, perc_thru)) %>% 
  
  inner_join(
    dbReadTable(con, "strava_new_segment_creation_interim") %>% 
      
      inner_join(telemetry_available %>% select(activity_id, rider=pcs) %>% unique()) %>% 
      
      group_by(rowid, stage, race, year, date, class) %>%
      mutate(median_distance = median(segment_distance, na.rm = T)) %>%
      ungroup() %>%
      
      mutate(ratio = abs(1-(median_distance/segment_distance))) %>%
      
      filter(segment_distance > 0) %>%
      filter(ratio <= 1.00) %>%
      
      filter(segment_speed_kmh <= 120) %>%
      
      group_by(rowid, stage, race, year, date, class, should_be_distance) %>%
      mutate(rel_speed = segment_speed_kmh / mean(segment_speed_kmh)) %>%
      ungroup() %>%
      mutate(date = as.Date(date)) %>%
      mutate(stage = as.character(stage)), by = c("Segment" = "rowid", "rider", "stage", "race", "year", "class", "date")) %>%
    
  group_by(rider, Gradient > 0.03, Gradient < -0.03, behind_best_placed <= 60) %>% 
  summarize(rel_speed = mean(rel_speed, na.rm = T),
            segments = n()) %>% 
  ungroup() -> relative_detailed

#
#
#
#
#

unique_races <- all_positions_riders %>%
  
  select(race, year, stage, class, date) %>%
  group_by(race, year, stage, class, date) %>%
  count() %>%
  ungroup()

#

summed_time <- all_positions_riders %>%
  
  filter(race == "tour de france" & year == 2021 & stage == 2) %>%
  
  arrange(desc(Segment)) %>%
  
  mutate(last = max(Segment)) %>%
  
  group_by(rider) %>%
  filter(max(Segment) == last) %>%
  mutate(bad_data_anywhere = cumsum(bad_data)) %>%
  mutate(bad_data_anywhere = ifelse(bad_data > 0, 1, 0)) %>%
  ungroup()

#

summed_time %>%
  filter(bad_data_anywhere == 0 & rnk < 200) %>%
  filter(Segment == 79) %>%
  
  ggplot(aes(x = -1*vs_median_before, y = behind_median/60, label = rider))+
  geom_point()+
  ggrepel::geom_text_repel(size=2)

#
#
#
#
#
#
#

summed_time %>% 
  
  filter(rnk < 200 & bad_data_anywhere == 0 & low_end > 150) %>%
  
  ggplot(aes(x = low_end, y = (vs_median_before / 60), color = rider))+
  geom_line()+
  geom_point()+
  theme(legend.text = element_text(size = 8))+

  labs(x = "KM thru race",
       y = "minutes behind",
       title = paste0(summed_time$year[[1]], " ", summed_time$race[[1]], " - stage ", summed_time$stage[[1]]))


#
#
#
#
#
#
#
#

# Win probability based on position before final climb

# 1. filter to stages where we have winner or someone who finished within 5 seconds of him (competitive for win)
# 2. before final climb
# 3. seconds behind leader

