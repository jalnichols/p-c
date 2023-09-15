
library(tidyverse)
library(DBI)
library(RMySQL)

dbDisconnect(con)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#####
#####
##### Bring in data

all_stage_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE year > 2013") %>%
  
  mutate(date = as.Date(date)) %>%
  
  left_join(
   
    dbGetQuery(con, "SELECT rider, bib, race, year FROM pcs_all_startlists"), by = c("rider", "year", "race") 
    
  ) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, km_before_peloton as km_break, race, year, stage FROM pcs_km_breakaway") %>%
      mutate(stage = as.numeric(stage)), by = c("rider", "year", "race", "stage") 
    
  ) %>%
  
  group_by(race, stage, year) %>%
  mutate(valid_data = ifelse(max(!is.na(km_break))==1, 1, 0)) %>%
  ungroup() %>%
  
  mutate(km_break = ifelse(is.na(km_break),
                           ifelse(valid_data == 1, 0, NA), km_break),
         perc_break = km_break / length) %>%
  
  select(-valid_data)
 
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
  
  filter(x5th > 35) %>%
  
  mutate(rel_speed = speed / x5th) %>%
  
  group_by(rider, Y = year >= 2019) %>%
  summarize(med_rnk = median(rnk, na.rm = T),
            peak = quantile(rel_speed, probs = 0.8, na.rm = T),
            avg = mean(rel_speed, na.rm = T),
            TTs = n()) %>%
  ungroup()
  
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
                  WHERE Stat IN ('Weighted Avg Power', 'Distance', 'AvgTemperature')") %>% 
  
  # I would like to bring in weight here so when I cut-off too low watts below it is watts/kg
  
  inner_join(
    
    dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)), by = c("PCS" = "rider")) %>%
  
  # clean up the dates
  mutate(Y = str_sub(DATE, nchar(DATE)-3, nchar(DATE))) %>% 
  separate(DATE, into = c("weekday", "date", "drop"), sep = ",") %>% 
  mutate(date = paste0(str_trim(date),", ", Y)) %>% 
  select(-weekday, -drop, -Y) %>% 
  
  # clean up the stat values
  mutate(VALUE = str_replace(VALUE, "mi", ""), 
         VALUE = str_replace(VALUE, "W", ""),
         VALUE = ifelse(Stat == "AvgTemperature",
                        str_sub(VALUE, 1, nchar(VALUE)-8), VALUE),
         VALUE = as.numeric(VALUE)) %>% 
  
  mutate(date = lubridate::mdy(date)) %>% 
  unique() %>% 
  spread(Stat, VALUE) %>% 
  #filter(!is.na(`Weighted Avg Power`)) %>% 
  janitor::clean_names() %>% 
  
  group_by(date) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  
  inner_join(all_stage_data %>%
               mutate(date = as.Date(date)), by = c("date", "pcs" = "rider")) %>% 
  
  # if two results exist for same day matching distance, it's probably a recon and TT which
  # means drop the lower watts
  
  # also, many riders include distance outside the TT as part of their strava activity
  # so maybe accept any riders +/- 10 km? or maybe we just can't get accurate TT data
  
  mutate(distance = distance * 1.609) %>% 
  filter((distance / length) > 0.8) %>%
  filter((distance / length) < 1.2) %>%
  
  group_by(stage, race, year) %>%
  mutate(temperature = mean(avg_temperature, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(final_group = ifelse(rnk == 200, 0,
                              ifelse(bunch_sprint == 1, ifelse(gain_1st <= 5, 1, 0), ifelse(rnk <= 20 | gain_20th == 0, 1, 0)))) %>%
  
  select(-avg_temperature, -n,
         -win_seconds, -total_seconds,
         -parcours_value, -stage_type,
         -gain_40th, -gain_20th, -gain_10th,
         -leader_rating) %>%
  
  # clear out anomalous data
  mutate(wattskg = weighted_avg_power / weight) %>% 
  
  # these average about 4.07 with 0.44 SD such that 99.8% of data should fall between 2.75 and 5.39
  #mutate(M = mean(wattskg, na.rm = T), 
  #       S = sd(wattskg, na.rm = T)) %>% 

  #filter(abs((wattskg - M) / S) < 3 | is.na(weight)) %>%
  
  #group_by(rider = pcs) %>% 
  #mutate(rel = weighted_avg_power / mean(weighted_avg_power, na.rm = T)) %>% 
  #ungroup() %>%
  
  mutate(rel_temp = abs(((temperature - 32) * 0.5556) - 11)) %>%
  
  #group_by(stage, race, year, rider, date) %>%
  #filter(rank(-weighted_avg_power, ties.method = "min") == 1) %>%
  #ungroup() %>%
  
  unique()

#
# breakaways
#

stage_level_power %>% 
  filter(!is.na(km_break)) %>%
  filter(time_trial == 0) %>%
  
  group_by(zero = perc_break == 0, stage, race, year) %>% 
  summarize(perc = mean(perc_break, na.rm = T), 
            power = mean(rel, na.rm = T), 
            n = n()) %>% 
  ungroup() %>% 
  
  group_by(stage, race, year) %>% 
  filter(min(zero) == 0) %>%
  ungroup() %>%
  
  group_by(zero) %>% 
  summarize(power = mean(power, na.rm = T), 
            sum(n), 
            n = n()) %>%
  ungroup()

#
# stage level rels
#

stage_level_power %>% 
  
  group_by(stage, race, year, date, time_trial, length, pred_climb_difficulty, class) %>% 
  summarize(power = mean(wattskg, na.rm = T), 
            relative = mean(rel, na.rm = T), 
            temp = mean(temperature, na.rm = T),
            n = n()) %>% ungroup() %>% 
  filter(n >= 5) %>% 
  
  arrange(-relative) -> stage_level_rel_power

#
#
#

stage_level_power %>%
  
  mutate(t25_power = ifelse(rnk <= 25, wattskg, NA),
         t25_time = ifelse(rnk <= 25, gain_1st, NA),
         not_power = ifelse(rnk > 25, wattskg, NA),
         not_time = ifelse(rnk > 25, gain_1st, NA)) %>%
  
  group_by(stage, race, year, date, time_trial, length, pred_climb_difficulty, class) %>% 
  summarize(power = mean(wattskg, na.rm = T), 
            top25_p = mean(t25_power, na.rm = T), 
            not_p = mean(not_power, na.rm = T),
            top25_t = mean(t25_time, na.rm = T),
            not_t = mean(not_time, na.rm = T),
            temp = mean(temperature, na.rm = T),
            n = n()) %>% 
  ungroup() %>%
  
  mutate(power_diff = (top25_p / not_p) - 1,
         time_diff = (top25_t - not_t) - 1) -> stage_power_averages

#
# impact of rnk, DNF, and specific race on rel power
#

extrap_alt <- lm(avg_alt ~ pred_climb_difficulty, data = stage_level_power)

po_mod <- lm(rel ~ log(rnk + 1) * pred_climb_difficulty + time_trial + length + one_day_race + I(rel_temp^2) + I(avg_alt^1.1), 
             
             data = cbind(stage_level_power,
                          pred_avg_alt = predict(extrap_alt, stage_level_power)) %>%
               mutate(avg_alt = ifelse(is.na(avg_alt), pred_avg_alt,
                                       ifelse(avg_alt < 0, 0, avg_alt))))

#

summary(po_mod)

preds <- cbind(pred = predict(po_mod, 
                              cbind(stage_level_power,
                                    pred_avg_alt = predict(extrap_alt, stage_level_power)) %>%
                                mutate(avg_alt = ifelse(is.na(avg_alt), pred_avg_alt,
                                                        ifelse(avg_alt < 0, 0, avg_alt)))),
               cbind(stage_level_power,
                     
                     pred_avg_alt = predict(extrap_alt, stage_level_power)) %>%
                 mutate(avg_alt = ifelse(is.na(avg_alt), pred_avg_alt,
                                         ifelse(avg_alt < 0, 0, avg_alt))))

#
# Impact of temperature on power output
#

ggplot(preds, aes(x = (temperature - 32) * 0.5556, y = (rel / pred)-1))+
  geom_hline(yintercept = 0)+
  geom_smooth(se=F, size=2, formula = y ~ x + I(x^2))+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Temperature in Celsius",
       y = "Relative power output vs Predicted",
       title = "Ideal temperature for power output is 11 C")+
  
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 15))

#
# Impact of average altitude
#

ggplot(preds, aes(x = (avg_alt), y = (rel / pred)-1))+
  geom_hline(yintercept = 0)+
  geom_point(size=1)+
  geom_smooth(se=F, size=2, method = "lm", formula = y ~ x)+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Average altitude in Meters",
       y = "Relative power output vs Predicted",
       title = "")+
  
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 15))+
  
  coord_cartesian(ylim = c(-0.05,0.05))

#

cbind(stage_level_power,
      
      pred_avg_alt = predict(extrap_alt, stage_level_power)) %>%
  mutate(avg_alt = ifelse(is.na(avg_alt), pred_avg_alt,
                          ifelse(avg_alt < 0, 0, avg_alt))) %>%
  
  select(-pred_avg_alt) %>%
  
  group_by(ALT = floor(avg_alt / 250)*250) %>%
  summarize(act = mean(rel, na.rm = T),
            n = n()) %>%
  ungroup()

#

stage_level_preds <- preds %>%
  
  group_by(stage, race, year, date, time_trial, pred_climb_difficulty) %>%
  summarize(exp = mean(pred, na.rm = T),
            act = mean(rel, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  
  mutate(diff = act - exp)

#
#
#

preds %>% 

  filter(time_trial == FALSE) %>%
  
  group_by(rider) %>%
  summarize(SD = sd(rel, na.rm = T), 
            SD_ADJ = sd((rel - pred), na.rm = T),
            Watts_KG = mean(weighted_avg_power / weight, na.rm = T),
            Median = median(weighted_avg_power / weight, na.rm = T),
            top20 = quantile((weighted_avg_power/weight), probs = 0.8, na.rm = T),
            adjusted = mean((rel - pred), na.rm = T),
            races = n()) %>%
  ungroup() -> rider_level_power

#
# join to clusters of rider types
#

clusters_power <- stage_level_power %>%
  
  filter(time_trial == 0) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  filter(!is.na(bunch_sprint)) %>%
  filter(!class %in% c("CC", 'NC', 'WC')) %>%
  
  inner_join(
    
    dbReadTable(con, "clusters_riders"), by = c("master_team", "pcs" = "rider", "year")
    
  )  %>%
  mutate(stage_type = ifelse(bunch_sprint == 1, "Bunch Sprint",
                             ifelse(pred_climb_difficulty <= 7.5, "Hilly", "Mountainous"))) %>%
  
  group_by(type, bunch_sprint, stage_type) %>%
  summarize(relwavgpower = mean(rel, na.rm=T),
            se = sd(rel, na.rm = T)/sqrt(n()),
            n = n()) %>%
  ungroup() %>%
  
  mutate(type = str_replace(type, " ", "\n"))

#
#
#

clusters_power$type <- factor(clusters_power$type, levels = c("Climber", "Mountain\nhelper", "Puncheur", "Domestique", "Sprinter", "Sprint\nTrain"))

ggplot(clusters_power, 
       
       aes(x = type, y = relwavgpower, ymin = relwavgpower-(1.96*se), ymax = relwavgpower+(1.96*se), color = stage_type))+
  
  geom_hline(yintercept = 1)+
  geom_errorbar(size=1)+
  geom_point(size = 6)+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Cluster type",
       y = "Relative weighted avg power",
       title = "Bunch sprints produce lower power outputs",
       color = "Stage type")+
  
  scale_color_manual(values = c("#404040", "orange", "dark red"))+
  theme(axis.text = element_text(size = 15),
        plot.title = element_text(size=18))

#

ggsave("clusters-by-stage-types.png", height = 6, width = 9)

#
# predictive modelling of power
#

train <- stage_level_power %>%
  
  group_by(rider) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  
  filter(year == 2019) %>%
  mutate(DNF = ifelse(rnk == 200, 1, 0)) %>% 
  mutate(spec_race = paste0(year,"-",stage,"-",race)) %>%
  
  mutate(pred_climb_difficulty = ifelse(is.na(pred_climb_difficulty),
                                        ifelse(time_trial == 1, 2, NA), pred_climb_difficulty)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  mutate(log_rnk = log(rnk + 1))

#

test <- stage_level_power %>%
  
  group_by(rider) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  
  filter(year == 2020) %>%
  mutate(DNF = ifelse(rnk == 200, 1, 0)) %>% 
  mutate(spec_race = paste0(year,"-",stage,"-",race)) %>%
  
  mutate(pred_climb_difficulty = ifelse(is.na(pred_climb_difficulty),
                                        ifelse(time_trial == 1, 2, NA), pred_climb_difficulty)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  mutate(log_rnk = log(rnk + 1))

#

po_mod <- lm(rel ~ log(rnk + 1) * pred_climb_difficulty + DNF + time_trial + length + one_day_race + rel_temp, 
             
             data = train %>% mutate(days = date - as.Date('2019-01-01')))

summary(po_mod)

#

preds <- cbind(pred = predict(po_mod, test %>% mutate(days = date - as.Date('2020-01-01')), allow.new.levels = TRUE),
               test %>% mutate(days = date - as.Date('2020-01-01'))) %>%
  
  group_by(stage, race, year, pred_climb_difficulty) %>%
  summarize(predicted = mean(pred, na.rm = T),
            actual = mean(rel, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  
  mutate(vs_predicted = actual / predicted)

test_mod <- lm(rel ~ pred, data = cbind(pred = predict(po_mod, test %>% mutate(days = date - as.Date('2020-01-01')), allow.new.levels = TRUE),
                                        test %>% mutate(days = date - as.Date('2020-01-01'))))

summary(test_mod)

#
#
#

library(xgboost)

# train

xgb.train <- xgb.DMatrix(
  data = as.matrix(train %>%

                     select(log_rnk,
                            pred_climb_difficulty,
                            DNF,
                            time_trial,
                            length,
                            one_day_race)),
  label = train$rel
)

# test

xgb.test <- xgb.DMatrix(
  
  data = as.matrix(test %>%

                     select(log_rnk,
                            pred_climb_difficulty,
                            DNF,
                            time_trial,
                            length,
                            one_day_race)),
  label = test$rel
  
)

# outline parameters

params <- list(
  
  booster = "gbtree",
  eta = 0.01,
  max_depth = 4,
  #gamma = 0.33,
  subsample = 1,
  colsample_bytree = 1,
  objective = "reg:squarederror"
  
)

# run xgboost model

gbm_model <- xgb.train(params = params,
                       data = xgb.train,
                       nrounds = 10000,
                       nthreads = 4,
                       early_stopping_rounds = 10,
                       watchlist = list(val1 = xgb.train,
                                        val2 = xgb.test),
                       verbose = 1)

# this outputs GBM predictions for all data

gbm_predict = cbind(
  
  test %>%
    
    select(rider,
           stage,
           race,
           year,
           log_rnk,
           pred_climb_difficulty,
           DNF,
           time_trial,
           length,
           one_day_race,
           rel),
  
  pred = predict(gbm_model, 
                 as.matrix(test %>% 

                             select(log_rnk,
                                    pred_climb_difficulty,
                                    DNF,
                                    time_trial,
                                    length,
                                    one_day_race)), reshape=T))

#

test_mod_xgb <- lm(rel ~ pred, data = gbm_predict)

summary(test_mod_xgb)

#

xgb.importance(model = gbm_model)

#
#
#
#

all_act_power <- dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Weighted Avg Power', 'Distance')") %>% 
  
  inner_join(
    
    dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)), by = c("PCS" = "rider")) %>%
  
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
  
  mutate(in_race = ifelse(activity_id %in% stage_level_power$activity_id, 1, 0)) %>%
  
  filter(weighted_avg_power > 0 & distance > 5)

#

pow_mod_all <- lme4::lmer(weighted_avg_power ~ (1 | pcs) + distance * in_race, data = all_act_power)

#

summary(pow_mod_all)

r <- lme4::ranef(pow_mod_all)[[1]] %>% rownames_to_column()

# a race is 315 - 0.35 * distance so a 150 km race is 261 watts and a 20 km TT is 308 watts
# a training session is 195 + 0.61 * distance

#
#
#
#
#
#
#
#

segment_data_races <- stage_level_power %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT * FROM strava_segment_data"), by = c("activity_id")
    
  ) %>%
  
  group_by(rider) %>% 
  mutate(rel_power = Power / mean(Power, na.rm = T)) %>% 
  ungroup() %>%
  
  select(-gain_3rd, -gain_5th, -gc_pos, -gain_gc, -missing_profile_data, -uphill_finish, 
         -sof_limit, -success, -points_finish, -cobbles, -M, -S, -VerticalGain, -VAM, -HR, -Tour,
         -gc_winner, -time_trial, -stage_name) %>%
  
  mutate(rowname = as.numeric(rowname)) %>% 
  
  mutate(time = Distance / Speed * 3600) %>%
  
  inner_join(preds %>%
               filter(!is.na(weight)) %>%
               filter(time_trial == 0) %>%
               
               group_by(race, stage, year) %>%
               filter(n() >= 5) %>%
               mutate(predicted = mean(rel / pred, na.rm = T)) %>%
               ungroup() %>%
               
               group_by(pcs) %>%
               summarize(watts_kg33 = quantile(wattskg / predicted, probs = 0.1, na.rm = T)) %>%
               ungroup() %>%
               
               mutate(peloton_avg = mean(watts_kg33, na.rm = T),
                      rel_peloton = watts_kg33 / peloton_avg), by = c("pcs")) %>%
  
  mutate(watts_adj = (Power/weight) / rel_peloton)

#
#
#

manually_marked_climbs <- segment_data_races %>%
  
  select(-segment_id) %>%
  
  inner_join(read_csv("strava_climbs_segments_manual.csv") %>%
               filter(Valid == 1) %>%
               mutate(AtKM = ifelse(AtKM > length, length, AtKM)) %>%
               select(-length), by = c("stage", 'race', "year", "Segment", "Type")) %>%
  
  #group_by(stage, race, year, pcs) %>%
  #filter(n()>1) %>% 
  #ungroup() %>%
  
  group_by(stage, race, year, Segment) %>%
  mutate(watts_adj = watts_adj / mean(watts_adj, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(perc_thru = (AtKM / length)-0.01) %>%

  inner_join(
    
    dbReadTable(con, "clusters_riders") %>%
      select(rider, type, Date) %>%
      mutate(Date = as.Date(Date)) %>%
      unique(), by = c("pcs" = "rider", "date" = "Date"))

#
# FULL MODEL FOR FINAL CLIMB WATTS PER KG
#

# grand tours increase power output by 0.1
# one day races increase by 0.1
# stage 20 will have 0.1 less power than stage 10 and 0.15 less than stage 5
# races with lots of prior climbing (10 pcd) will have 0.4 less watts per KG than stages with no prior climbing
# Sprinter, Domestique, and Sprint train types have 0.25 less than Climbers
# Puncheurs/Mtn helpers have 0.12 less than Climbers
# Gradient is the main determinant of climb difficulty with climbs of 9% requiring 0.3 more watts/kG than 5% climbs
# Distance matters on the periphery with climbs of 17 KM raced at 0.15 watts/KG less than climbs of 7 KM
# 10 degrees warmer in celsius is worth around 0.1 fewer watts/KG
# length of race matters with 225 KM races resulting in 0.15-0.20 watts/KG less than 150 KM races

# which all adds up to riders producing fewer watts on hot days with lots of climbing later in stage races
# ideal conditions for producing watts are one day races or early in grand tours after easy rides with comfortable temperatures

# finish position is highly correlated with watts produced with riders finishing 10th producing 1.6 more watts/KG than those finishing 120th
# the best rider in a race on a team will produce about 0.25 watts/KG more than the worst - controlling for finish position - and about 1.55 watts/KG
# less overall

summary(
  
  stats::lm(
    
    wkg ~ 
        log(tm_pos+1) + 
        log(rnk+1) + 
        type + 
        stage +
        length +
        temperature + 
        grand_tour +
        one_day_race + 
        prior_pcd + 
        as.factor(year) + 
        Gradient * Distance, 
    
    data = cbind(
      
      manually_marked_climbs %>%
        group_by(stage, race, year) %>%
        filter(perc_thru > 0.94 | rank(-AtKM, ties.method = "min") == 1) %>%
        ungroup() %>%
        mutate(wkg = (Power/weight)) %>% 
        select(-Valid, -Type, -Time) %>% 
        filter(wkg > 2.5) %>% 
        unique(), 
      
      clpcd = predict(read_rds('model-climb-difficulty.rds'), 
                      manually_marked_climbs %>% 
                        group_by(stage, race, year) %>%
                        filter(perc_thru > 0.94 | rank(-AtKM, ties.method = "min") == 1) %>%
                        ungroup() %>%
                        mutate(wkg = (Power/weight)) %>%
                        select(-Valid, -Type, -Time) %>%
                        filter(wkg > 2.5) %>%
                        unique() %>% 
                        mutate(alt = 0, 
                               vam_poly = (Distance * (Gradient^2))))) %>%
      mutate(prior_pcd = pred_climb_difficulty - clpcd) %>%
      mutate(prior_pcd = ifelse(stage == 4 & race == "tour de suisse" & year == 2017, 6,
                                ifelse(stage == 7 & race == "tour de suisse" & year == 2017, 2,
                                       ifelse(stage == 2 & race == "tour of the alps" & year == 2018, 8,
                                              ifelse(prior_pcd < 0, 0, prior_pcd)))))))


#

manually_marked_climbs %>%
  
  group_by(stage, race, year, pcs) %>%
  filter(n()>1) %>% 
  ungroup() %>%

  group_by(rnk = ifelse(rnk <= 20, "top20",
                        ifelse(rnk <= 50, "top50",
                               ifelse(rnk <= 100, "top100", "remaining"))), 
           f = ifelse(perc_thru <= 0.2, 0.2, floor(perc_thru / 0.1) / 10)) %>% 
  summarize(M = mean(watts_adj, na.rm = T), 
            n = n()) %>%
  ungroup() -> progress_thru_stage

# model with percent thru stage, final group presence, finish position

summary(manually_marked_climbs %>%
          
          group_by(stage, race, year, pcs) %>%
          filter(n()>1) %>% 
          ungroup() %>%
          
          lm(watts_adj ~ log(rnk+1) * perc_thru + perc_break*perc_thru, data = .))

#

progress_thru_stage$rnk <- factor(progress_thru_stage$rnk, levels = c("top20", "top50", "top100", "remaining"))

avg_power <- segment_data_races %>%
  
  inner_join(read_csv("strava_climbs_segments_manual.csv") %>%
               filter(Valid == 1) %>%
               mutate(AtKM = ifelse(AtKM > length, length, AtKM)) %>%
               select(-length), by = c("stage", 'race', "year", "Segment", "Type")) %>%
  
  group_by(stage, race, year, pcs) %>%
  filter(n()>1) %>% 
  ungroup() %>%
  
  summarize(M = mean(watts_adj, na.rm = T)) %>%
  .[[1]]

ggplot(progress_thru_stage, aes(x = f+0.1, y = M * avg_power, color = as.factor(rnk)))+
  geom_hline(yintercept = avg_power, size = 1, linetype = "dashed")+
  geom_smooth(se=F, size = 2)+
  scale_color_manual(values = c("dark red", 'red', "orange", "blue"),
                     name = "Finish position")+
  labs(x = "Percentage thru stage", 
       y = "Watts per KG average", 
       title = "Top finishers produce more power in last 3rd of race")+
  scale_x_continuous(labels = scales::percent)+
  facet_wrap(~summit_finish, ncol = 2)

#

manually_marked_climbs %>% 
  
  group_by(stage, race, year, pcs) %>%
  filter(n()>1) %>% 
  ungroup() %>%
  
  group_by(stage, race, year, Segment, perc_thru, Gradient, Distance) %>% 
  summarize(x85th_percentile = quantile(watts_adj, probs = 0.85, na.rm = T), 
            x50th_percentile = median(watts_adj, na.rm = T), 
            x15th_percentile = quantile(watts_adj, probs = 0.15, na.rm = T)) %>% 
  ungroup() %>%
  
  gather(stat, value, x85th_percentile:x15th_percentile) -> bands_of_perf

# this model evaluates watts per kg based on climb characteristics and percentage thru stage vs finish position

mod_power_chars <- lm(Watts_kg_adj ~ Gradient * Distance + perc_thru * log(rnk + 1), 
                      
                      data = manually_marked_climbs %>%
                        group_by(stage, race, year, pcs) %>%
                        filter(n()>1) %>% 
                        ungroup() %>%
                        mutate(Watts_kg_adj = (Power/weight) / rel_peloton))

summary(mod_power_chars)

#

bands_of_perf %>% 
  mutate(stat = str_sub(stat, 2, nchar(stat))) %>%
  ggplot(aes(x = perc_thru, y = value-1, color = stat))+
  geom_point(size=1, alpha = 0.25)+
  geom_smooth(se=F, size=2)+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent, breaks = seq(-0.3,0.3,0.1))+
  labs(x = "Percentage thru race", 
       y = "Power output versus peloton", 
       title = 'Power outputs differ most in last 3rd of race')+
  
  scale_color_manual(values = c("blue", "orange", "red"), name = "Power output\npercentile")

#
#
#

manually_marked_climbs %>% 
  
  group_by(stage, race, year, pcs) %>%
  filter(n()>1) %>% 
  ungroup() %>%

  group_by(stage, race, year, Segment, perc_thru, Gradient, Distance, type) %>% 
  summarize(Average_Power = mean(watts_adj, na.rm = T)) %>% 
  ungroup() -> bands_of_clust

bands_of_clust %>% 
  ggplot(aes(x = perc_thru, y = avg_power * (Average_Power), color = type))+
  geom_point(size=1, alpha = 0.25)+
  geom_smooth(se=F, size=2)+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  coord_cartesian(ylim = c(3.5,6.5))+
  labs(x = "Percentage thru race", 
       y = "Average power output", 
       title = 'Power outputs differ based on rider type',
       color = "Rider type")+
  
  
  scale_color_manual(values = c("#F02108", "gray40", "#F0A608",
                                "#37B36C", "#16BEF2", "#162CF2"), name = "Rider Type")

#

avg_power <- segment_data_races %>%
  
  inner_join(read_csv("strava_climbs_segments_manual.csv") %>%
               filter(Valid == 1) %>%
               mutate(AtKM = ifelse(AtKM > length, length, AtKM)) %>%
               select(-length), by = c("stage", 'race', "year", "Segment", "Type")) %>%
  
  group_by(stage, race, year, pcs) %>%
  filter(n()>1) %>% 
  ungroup() %>%
  summarize(M = mean(watts_adj, na.rm = T)) %>%
  .[[1]]

manually_marked_climbs %>% 
  
  group_by(stage, race, year, pcs) %>%
  filter(n()>1) %>% 
  ungroup() %>%
  
  mutate(fin_pos = ifelse(rnk <= 10, "top_10",
                          ifelse(rnk <= 25, "top_25",
                                 ifelse(rnk <= 50, "top_50",
                                        ifelse(rnk <= 100, "top_100", "remaining"))))) %>%
  
  group_by(stage, race, year, Segment, perc_thru, Gradient, Distance, fin_pos) %>% 
  summarize(Average_Power = mean(watts_adj, na.rm = T)) %>% 
  ungroup() -> bands_of_rnk

bands_of_rnk$fin_pos <- factor(bands_of_rnk$fin_pos, levels = c("top_10", "top_25", "top_50", "top_100", "remaining"))

bands_of_rnk %>% 
  ggplot(aes(x = perc_thru, y = avg_power * (Average_Power), color = fin_pos))+
  geom_point(size=1, alpha = 0.25)+
  geom_smooth(se=F, size=2)+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  coord_cartesian(ylim = c(3.5,7))+
  labs(x = "Percentage thru race", 
       y = "Average power output", 
       title = 'Power outputs differ most in last 3rd of race')+
  
  scale_color_manual(values = c("dark red", "red", "orange", "blue", "black"), name = "Finish position")

#

gam_mod <- mgcv::gam(AP ~ s(perc_thru, k = 10),
      
      data = bands_of_rnk %>%
        filter(fin_pos == "top_10") %>%
        mutate(AP = (Average_Power * avg_power) - 4.2))

write_rds(gam_mod, "Stored models/power-required-throughout-race-gam.rds")

gam_preds <- cbind(pred = predict(gam_mod, tibble(perc_thru = seq(0,1,0.01))),
                   tibble(perc_thru = seq(0,1,0.01)))

gam_preds %>%
  
  mutate(best = max(pred, na.rm = T),
         rel_to_best = pred / best) %>%
  
  ggplot(aes(x = perc_thru, y = rel_to_best))+
  geom_point(size=2)+
  geom_line()+
  
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  
  labs(x = "Percentage through race",
       y = "Power required relative to final climb",
       title = "Power required on climbs throughout race",
       subtitle = "Relative to that required on final climb")+
  
  theme(axis.text = element_text(size = 15))

#

segment_data_races %>% group_by(rider, weight) %>% count(sort=TRUE) -> segs_riders

#

max_effort <- segment_data_races %>%
  
  group_by(rider) %>%
  
  filter(n() > 1500) %>%
  
  arrange(-time) %>%
  
  mutate(keep = max(time, na.rm = T)) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = ifelse(Power > lag(Power), Power, NA)) %>%
  filter(!is.na(best) | keep == time) %>%
  
  mutate(best = Power) %>%
  
  ungroup() %>%
  
  filter(keep != time) %>%
  
  mutate(wattskg = best/weight) %>%
  
  filter(time >= 15 & time <= 3700)

#
#
#

ggplot(max_effort %>% filter(time < 3700 & time > 14), aes(x = time, y = best / weight))+
  geom_smooth(se = F, size = 2, color = "black")+
  
  geom_line(data = max_effort %>%
              filter(rider %in% c("Naesen Oliver", "Declercq Tim", "Colbrelli Sonny", "Bol Cees")), aes(x = time, y = best / weight, color = rider), size = 2)+
  
  scale_x_log10(breaks = c(15, 30, 60, 120, 300, 600, 1200, 1800, 2700))+
  scale_y_continuous(breaks = seq(3.5,12.5,0.5))+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")+
  
  labs(x = "Time of Effort",
       y = "Power output",
       title = "Best Efforts (vs Avg Pro in black)", color = "")

#

loess_fit <- loess(wattskg ~ time, data = max_effort, span = 0.2)

#

power_vs_avg <- cbind(exp = predict(loess_fit, max_effort),
               max_effort) %>%
  
  mutate(rel_to_exp = wattskg / exp)

#
#
#

segment_intensity <- segment_data_races %>%
  
  group_by(stage, race, year, rider, Type) %>%
  mutate(race_position = rowname / max(rowname, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(Segment, Gradient, VerticalGain, Distance, stage, race, year, date) %>%
  summarize(Wattskg = median(Power/weight, na.rm = T),
         riders = n_distinct(rider),
         race_position = mean(race_position, na.rm = T)) %>%
  ungroup() %>%
  
  filter(riders >= 10 & Gradient > 0.03 & Distance >= 0.5 & Distance <= 25)

#
#
#

seg_inten_fit <- mgcv::gam(Wattskg ~ s(race_position, k = 5) + Gradient * Distance, data = segment_intensity)

possible <- expand_grid(Gradient = seq(0.03,0.12,0.01), Distance = seq(0.5, 25, 0.5), race_position = seq(0,1,0.1))

model_fit <- cbind(pred = predict(seg_inten_fit, possible),
                   possible)

#
#
#

key_segments <- segment_data_races %>%
  
  group_by(Segment, Distance, VerticalGain, Gradient, rider, stage, race, year) %>%
  filter(min(time) == time) %>%
  ungroup() %>%
  
  select(-rowname, -Tour, -gc_winner, -time_trial, -pcs, -activity_id, -
         -distance, -VAM, -HR, -Type, -Time, -rel_power) %>%

  inner_join(read_csv('key-segments-strava.csv'), by = c("stage", "race", "year", "Segment")) %>%
  
  unique() %>%
  
  mutate(wattskg = Power / weight) %>%
  
  filter(wattskg > 2)

#
#
#

key_segs_averages <- key_segments %>%

  inner_join(rider_level_power %>%
               filter(races >= 10) %>%
               select(rider, avgwattskg = Watts_KG), by = c("rider")) %>%
  
  mutate(vs_avg = wattskg / avgwattskg * 4.07) %>%
  
  mutate(top25 = ifelse(rnk <= 25, wattskg, NA),
         top25_rel = ifelse(rnk <= 25, vs_avg, NA)) %>%
  
  group_by(Climb, Segment, Distance, VerticalGain, Gradient, stage, race, year) %>%
  summarize(x80 = quantile(wattskg, probs = 0.8),
            x80_rel = quantile(vs_avg, probs = 0.8),
            median = median(wattskg, na.rm = T),
            median_rel = median(vs_avg, na.rm = T),
            top25 = mean(top25, na.rm = T),
            top25_rel = mean(top25_rel, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(est = 6.47 - (0.1 * Distance),
         vs_est = top25_rel - est)

#
# use climbing records.com data to make baseline from elite GC riders
#

climbing_records <- readxl::read_excel("climbing-records-dot-com.xlsx") %>%
  
  janitor::clean_names()

#

peak_mod <- lm(watts_per_kg ~ Gradient * Distance, 
               data = climbing_records %>%
                 group_by(iteration, climb) %>%
                 summarize(watts_per_kg = mean(watts_per_kg, na.rm = T),
                           gradient = mean(gradient/100, na.rm = T),
                           km = mean(km, na.rm = T)) %>%
                 ungroup() %>%
                 rename(Distance = km,
                        Gradient = gradient))

summary(peak_mod)

#

estimates <- cbind(key_segments,
                   est = predict(peak_mod, key_segments)) %>%
  
  mutate(vs_model = wattskg / est) %>%
  
  group_by(rider) %>%
  mutate(relative = vs_model / mean(vs_model, na.rm = T)) %>%
  ungroup()

#

seg_inten_fit <- lm(relative ~ pred_climb_difficulty + log(rnk+1) + length + one_day_race + grand_tour, data = estimates)

summary(seg_inten_fit)

segment_level_intensity_preds <- cbind(
  
  est_inten = predict(seg_inten_fit, estimates),
  
  estimates
  
)

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

who_finishes_near <- all_stage_data %>%
  
  filter(class %in% c('1.UWT', '1.HC', '1.1', 'WC', 'CC',
                      '2.UWT', '2.HC', '2.1', '1.Pro', '2.Pro')) %>%
  
  filter(bunch_sprint == 0 | rnk <= 20) %>%
  filter(!rnk == 200)

#

tdf2019 <- who_finishes_near %>%
  
  select(rider1 = rider, stage, race, year, rnk1 = rnk, bunch_sprint,
         team1 = team, secs1 = gain_1st) %>%
  
  inner_join(
    
    who_finishes_near %>%
      select(rider2 = rider, stage, race, year, rnk2 = rnk,
             team2 = team, secs2 = gain_1st), by = c("stage", "race", "year")
    
  ) %>%
  
  # eliminate same team as it may match riders pacing each other in mtns or clumped in bunch
  filter(team1 != team2) %>%
  filter(rider1 != rider2) %>%
  
  group_by(rider1, rider2) %>%
  mutate(together = n()) %>%
  ungroup() %>%
  
  # filter riders within 5 places in bunch sprint
  # OR riders within a minute in non-bunch sprint
  filter((bunch_sprint == 1 & abs(rnk2 - rnk1) <= 5) |
           (bunch_sprint == 0 & abs(secs1 - secs2) <= 60)) %>%

  group_by(rider1, rider2) %>%
  summarize(together = mean(together, na.rm = T),
            matched = n()) %>%
  ungroup() %>%
  
  mutate(matched_perc = matched / together) %>%
  
  group_by(rider1) %>%
  filter(together >= 8 & (max(together, na.rm = T) / together) <= 2.5) %>%
  ungroup() %>%
  
  group_by(rider1) %>%
  mutate(pct_rk = percent_rank(matched_perc)) %>%
  ungroup()

#
#
#

# top 5 finishes 

top_finishes <- all_stage_data %>%
  
  filter(class %in% c('1.UWT', '1.HC', '1.1', 'WC', 'CC',
                      '2.UWT', '2.HC', '2.1', '1.Pro', '2.Pro')) %>%
  
  group_by(rider) %>% 
  mutate(races = n(),
         pct_rk = rank(-rnk, ties.method = 'first') / races) %>%
  filter(pct_rk > 0.899 | rank(rnk, ties.method = "first") <= 3) %>% 
  summarize(races = mean(races, na.rm = T),
            avg_top = mean(rnk, na.rm = T),
            avg_log = mean(log(rnk+1), na.rm = T),
            n=n()) %>% 
  ungroup() %>%
  
  mutate(avg_log = exp(avg_log)-1)

#
#
#

tictoc::tic()

curves <- dbGetQuery(con, "SELECT SA.PCS, CP.seconds, CP.watts, SA.activity_id
FROM strava_activities SA 
JOIN strava_activity_power_curve CP
ON SA.activity_id = CP.activity_id
WHERE CP.seconds IN (10, 30, 60, 120, 300, 600, 1200, 2400, 3600)")

tictoc::toc()

#

best <- curves %>%
  
  #filter(!PCS %in% c("Madouas Valentin", "Gougeard Alexis", "Jauregui Quentin")) %>%
  
  group_by(PCS) %>% 
  filter(n_distinct(activity_id) >= 10) %>% 
  ungroup() %>% 
  
  inner_join(preds %>%
               filter(!is.na(weight)) %>%
               filter(time_trial == 0) %>%
               
               group_by(race, stage, year) %>%
               filter(n() >= 5) %>%
               mutate(predicted = mean(rel / pred, na.rm = T)) %>%
               ungroup() %>%
               
               group_by(pcs) %>%
               summarize(watts_kg33 = quantile(wattskg / predicted, probs = 0.1, na.rm = T)) %>%
               ungroup() %>%
               
               mutate(peloton_avg = mean(watts_kg33, na.rm = T),
                      rel_peloton = watts_kg33 / peloton_avg), by = c("PCS" = "pcs")) %>%
  
  mutate(watts_adj = watts / rel_peloton) %>%
  
  group_by(PCS, seconds) %>%
  mutate(best80 = quantile(watts_adj, probs = 0.8, na.rm = T),
         best90 = quantile(watts_adj, probs = 0.9, na.rm = T),
         median = median(watts_adj, na.rm = T),
         mean = mean(watts_adj, na.rm = T),
         races = n()) %>%
  ungroup() %>%

  inner_join(stage_level_power %>%
               filter(time_trial == 0) %>%
               select(-M, -S, -rider,
                      -temperature, -km_break,
                      -bib, -limit, -gc_winner,
                      -time_trial, -missing_profile_data,
                      -stage_name), by = c("PCS" = "pcs", "activity_id")) %>%

  mutate(vs_80 = watts_adj / best80,
         vs_90 = watts_adj / best90,
         vs_med = watts_adj / median,
         vs_mean = watts_adj / mean) %>%
  
  inner_join(
    
    dbReadTable(con, "clusters_riders") %>%
      select(rider, year, master_team, type) %>%
      unique(), by = c("master_team", "PCS" = "rider", "year"))

#
#
#

po_curves_riders <- best %>% 
  group_by(PCS, seconds) %>% 
  summarize(best80 = quantile(watts_adj / weight, probs = 0.8, na.rm = T),
            best90 = quantile(watts_adj / weight, probs = 0.9, na.rm = T),
            median = median(watts_adj / weight, na.rm = T),
            mean = mean(watts_adj / weight, na.rm = T),
            median_raw = median(watts/weight, na.rm = T),
            rel_peloton = mean(rel_peloton, na.rm = T),
            races = n()) %>%
  ungroup()

#

po_curves_riders %>%
  filter(PCS %in% c("Declercq Tim", "De Gendt Thomas", "Kamna Lennard", "Coquard Bryan")) %>%
  
  ggplot(aes(x = seconds, y = best80, color = PCS))+
  
  geom_line(size=2)+
  
  scale_x_log10(breaks = c(10,30,60,120,300,600,1200,2400))+
  
  labs(x = "Time duration (secs)",
       y = "80th percentile of races",
       title = "Top efforts by time duration")+
  
  scale_color_manual(values = c("gold", "#404040", "blue", "orange"), name = "Rider")

#
#
#

po_curves_clusters <- best %>% 
  group_by(type, seconds) %>% 
  summarize(best80 = quantile(watts_adj / weight, probs = 0.8, na.rm = T),
            best90 = quantile(watts_adj / weight, probs = 0.9, na.rm = T),
            best80_raw = quantile(watts / weight, probs = 0.8, na.rm = T),
            median = median(watts_adj / weight, na.rm = T),
            median_raw = median(watts/weight, na.rm = T),
            mean = mean(watts_adj / weight, na.rm = T),
            races = n()) %>%
  ungroup()

po_curves_clusters %>% 
  group_by(seconds) %>% 
  mutate(peloton = sum(best80_raw * races, na.rm = T) / sum(races, na.rm = T)) %>% 
  ungroup() %>% 
  
  filter(seconds < 3600) %>%
  
  ggplot(aes(x = seconds, y = (best80_raw / peloton)-1, color = type))+
  
  geom_hline(yintercept = 0)+
  geom_smooth(size=2, se = F)+
  scale_x_log10(breaks = c(10,30,60, 120, 300, 600, 1200, 2400, 3600))+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "seconds of effort", 
       y = 'watts per KG vs average rider', 
       title = "Power outputs vs rider type clusters",
       subtitle = "80th percentile efforts")+
  theme(panel.grid.minor.x = element_blank())+
  
  scale_color_manual(values = c("#F02108", "gray40", "#F0A608",
                                "#37B36C", "#16BEF2", "#162CF2"), name = "Rider Type")

#
#
#

stage_level_power_curve <- best %>%
  
  group_by(stage, race, year, date, pred_climb_difficulty, seconds) %>%
  summarize(vs_80 = mean(vs_80, na.rm = T),
            vs_90 = mean(vs_90, na.rm = T),
            vs_median = mean(vs_med, na.rm = T),
            vs_mean = mean(vs_mean, na.rm = T),
            rel_wt_avg_pow = mean(rel, na.rm = T),
            watts_adj = mean(watts_adj, na.rm = T),
            n = n()) %>%
  ungroup()

# correlations between overall weighted avg power and each N seconds power

best %>% 
  
  group_by(seconds) %>%
  summarize(cor(x = vs_mean, y = rel))

stage_level_power_curve %>% 
  filter(n > 9) %>% 
  
  group_by(seconds) %>% 
  summarize(cor(x = watts_adj, y = rel_wt_avg_pow))

#

po_curve_mod <- lm(vs_mean ~ log(rnk + 1) * pred_climb_difficulty + time_trial + length + one_day_race + bunch_sprint, 
             
             data = best %>%
               filter(seconds == 600))

#

summary(po_curve_mod)

#

ggplot(best %>% 
         filter(PCS %in% c("Laas Martin", "Naesen Oliver", "Declercq Tim", "Haig Jack")) %>%
         
         select(PCS, seconds, weight, best80, best90, median, mean) %>%
         unique() %>%
         
         mutate(wattskg = best90 / weight), 
       
       aes(x = seconds, y = wattskg, color = PCS))+
  
  geom_line(size = 1)+
  scale_x_log10(breaks = c(10,30,60,120,300,600,1200,1800,3600))+
  scale_y_continuous(breaks = seq(4,16,1))+
  
  theme(panel.grid.minor = element_blank())

#

strava_matches_power <- tdf2019 %>%
  
  arrange(-pct_rk) %>% 
  
  group_by(rider1) %>% 
  filter(rank(-pct_rk, ties.method= 'min') <= 50) %>% 
  ungroup() %>% 
  
  filter(pct_rk > 0.8) %>%
  
  inner_join(
    
    top_finishes %>%
      filter(n >= 3) %>%
      select(rider, avg_top1 = avg_top), by = c("rider1" = "rider")
    
  ) %>%
  
  inner_join(
    
    top_finishes %>%
      filter(n >= 3) %>%
      select(rider, avg_top2 = avg_top), by = c("rider2" = "rider")
    
  ) %>%
  
  # find abs diff between logs of avg ranks
  # 1 SD is 0.9 abs log diff
  mutate(DiffScore = abs(log(avg_top2)-log(avg_top1))) %>%
  
  filter(DiffScore <= 1) %>%
  
  inner_join(best, by = c("rider2" = "PCS")) %>%
  
  group_by(rider1, seconds) %>%
  summarize(wattskg = mean(wattskg, na.rm = T),
            rel = mean(rel, na.rm = T),
            avg_rank = mean(avg_top1, na.rm = T),
            matches = n()) %>%
  ungroup()

#
#
#
#
#

leadouts <- curves %>%
  
  inner_join(stage_level_power %>%
               select(stage, race, year, rider, pred_climb_difficulty, bunch_sprint,
                      activity_id), by = c("PCS" = "rider", "activity_id")) %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)), by = c("PCS" = "rider")) %>% 
  
  mutate(wattskg = watts / weight) %>%
  
  filter(PCS %in% c("Sinkeldam Ramon", "Kluge Roger",
                    "Ackermann Pascal", "Dupont Timothy",
                    "Haig Jack", "Kuss Sepp",
                    "Declercq Tim", "De Gendt Thomas"))

#

leadouts %>% 
  
  filter(seconds == 30 & !is.na(bunch_sprint)) %>% 
  
  ggplot(aes(x = as.factor(bunch_sprint), y = wattskg))+
  geom_boxplot()+
  facet_wrap(~PCS, nrow = 3)+
  coord_flip()

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
#
#

strava_telemetry <- dbGetQuery(con, "SELECT * FROM strava_telemetry") %>%
  
  inner_join(
    
    stage_level_power %>%
      select(activity_id, stage, race, year, rider, 
             length, pred_climb_difficulty, rnk, 
             weighted_avg_power, distance, weight, date), by = c("activity_id")
    
  ) %>%
  
  filter(stage == 7 & year == 2020 & race == "tour de france")

#
#
#
#
#
#
#

all_segment_data <- stage_level_power %>%
  select(stage, race, year, date, class, one_day_race, grand_tour, time_trial, length, activity_id) %>%
  inner_join(
    
    dbGetQuery(con, "SELECT * FROM strava_segment_data"), by = c("activity_id")
    
  ) %>%
  
  group_by(Segment, stage, race, year) %>%
  filter(n_distinct(activity_id) >= 4) %>%
  ungroup() %>%
  
  select(stage, race, year, date, class, one_day_race, grand_tour, time_trial, length,
         Type, Gradient, VerticalGain, Distance, Segment) %>%
  unique()

#

just_climbs <- all_segment_data %>%
  
  filter(Gradient > 0.035 & Distance >= 0.5) %>%
  
  anti_join(read_csv("strava_climbs_segments_manual.csv") %>%
              select(stage, race, year) %>%
              unique(), by = c("stage", "race", "year")) %>%
  
  group_by(stage, race, year) %>%
  filter(Distance > (max(Distance, na.rm = T) * 0.2) | Distance > 2) %>%
  ungroup() %>%
  
  select(stage, race, year, length, Type, Gradient, Distance, Segment)

#

just_descents <- all_segment_data %>%
  
  filter(Gradient <= -0.035 & Distance >= 2) %>%
  
  #anti_join(read_csv("strava_climbs_segments_manual.csv") %>%
  #            select(stage, race, year) %>%
  #            unique(), by = c("stage", "race", "year")) %>%
  
  group_by(stage, race, year) %>%
  filter(Distance > (max(Distance, na.rm = T) * 0.2) | Distance > 3) %>%
  ungroup() %>%
  
  select(stage, race, year, length, Type, Gradient, Distance, Segment)

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
#
#
#


# Strava vertical gain ----------------------------------------------------


stage_level_power <- dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Elevation', 'Distance')") %>% 
  
  # I would like to bring in weight here so when I cut-off too low watts below it is watts/kg
  
  inner_join(
    
    dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)), by = c("PCS" = "rider")) %>%
  
  # clean up the dates
  mutate(Y = str_sub(DATE, nchar(DATE)-3, nchar(DATE))) %>% 
  separate(DATE, into = c("weekday", "date", "drop"), sep = ",") %>% 
  mutate(date = paste0(str_trim(date),", ", Y)) %>% 
  select(-weekday, -drop, -Y) %>% 
  
  # clean up the stat values
  mutate(VALUE = str_replace(VALUE, "mi", ""), 
         VALUE = str_replace(VALUE, "ft", ""),
         VALUE = str_replace(VALUE, ",", ""),
         VALUE = as.numeric(VALUE)) %>% 
  
  mutate(date = lubridate::mdy(date)) %>% 
  unique() %>% 
  spread(Stat, VALUE) %>% 
  filter(!is.na(`Elevation`)) %>% 
  janitor::clean_names() %>% 
  
  group_by(date) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  
  inner_join(all_stage_data %>%
               filter(year %in% c("2014", "2019", "2020")) %>%
               mutate(date = ifelse(race == "tour de romandie" & year == 2019,
                                    as.Date(date + 1), as.Date(date)),
                      date = ifelse(race == "giro d'italia" & year == 2019,
                                    ifelse(stage < 10, date, ifelse(stage < 16, date + 1, date + 2)), date),
                      date = ifelse(race == "giro d'italia" & year == 2014,
                                    ifelse(stage < 4, date, ifelse(stage < 10, date + 1, ifelse(stage < 16, date + 2, date + 3))), date),
                      date = ifelse(race == "giro d'italia" & year == 2020,
                                    ifelse(stage < 10, date, ifelse(stage < 16, date + 1, date + 2)), date),
                      date = ifelse(race == "tour de france" & year == 2019,
                                    ifelse(stage < 11, date, ifelse(stage < 16, date + 1, date + 2)), date),
                      date = ifelse(race == "la vuelta ciclista a espana" & year == 2019,
                                    ifelse(stage < 10, date, ifelse(stage < 17, date + 1, date + 2)), date),
                      date = ifelse(race == "la vuelta ciclista a espana" & year == 2020,
                                    ifelse(stage < 7, date, ifelse(stage < 13, date + 1, date + 2)), date),
                      date = ifelse(race == "volta a portugal em bicicleta edicao especial" & year == 2020,
                                    date + 1, date),
                      date = ifelse(race == "volta a portugal santander" & year == 2020,
                                    date + 1, date),
                      date = ifelse(race == "skoda-tour de luxembourg" & year == 2019,
                                    date + 1, date),
                      date = ifelse(race == "sibiu cycling tour" & year == 2019,
                                    date + 1, date),
                      date = ifelse(race == "sibiu cycling tour" & year == 2020,
                                    date + 1, date),
                      date = ifelse(race == "tour de hongrie" & year == 2019,
                                    date + 1, date),
                      date = ifelse(race == "boucles de la mayenne" & year == 2019,
                                    date + 1, date),
                      date = ifelse(race == "zlm tour" & year == 2019,
                                    date + 1, date),
                      date = ifelse(race == "int. osterreich-rundfahrt-tour of austria" & year == 2019,
                                    date + 1, date),
                      date = ifelse(race == "the larry h.miller tour of utah" & year == 2019,
                                    date + 1, date),
                      date = ifelse(race == "vuelta a san juan internacional" & year == 2019,
                                    ifelse(stage < 5, date, date + 1), date),
                      date = ifelse(race == "vuelta a san juan internacional" & year == 2020,
                                    ifelse(stage < 5, date, date + 1), date),
                      date = ifelse(race == "tour de france" & year == 2020,
                                    ifelse(stage < 10, date, ifelse(stage < 16, date + 1, date + 2)), date)) %>%
               mutate(date = as.Date(date, origin = '1970-01-01')) %>%
               unique(), by = c("date", "pcs" = "rider")) %>% 
  
  # if two results exist for same day matching distance, it's probably a recon and TT which
  # means drop the lower watts
  
  # also, many riders include distance outside the TT as part of their strava activity
  # so maybe accept any riders +/- 10 km? or maybe we just can't get accurate TT data
  mutate(elevation = elevation * 0.3048) %>%
  mutate(distance = distance * 1.609) %>% 
  filter((distance / length) > 0.8) %>%
  filter((distance / length) < 1.2) %>%
  mutate(tvg = elevation / distance) %>% select(-n,
                                                -win_seconds, -total_seconds,
                                                -parcours_value, -stage_type,
                                                -gain_40th, -gain_20th, -gain_10th,
                                                -leader_rating)

#
#
#

stage_level_power %>% 
  filter(rnk != 200 & time_trial == 0) %>% 
  
  group_by(stage, race, year, class, date, pred_climb_difficulty) %>% 
  summarize(tvg = median(tvg, na.rm = T),
            riders= n(), 
            distance = median(distance, na.rm = T), 
            elevation = median(elevation)) %>% 
  ungroup() %>% 
  unique() -> TVG

#
#
#
#
#
#
#
#
#


# Time Trial Segments -----------------------------------------------------

itt_segments <- dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Weighted Avg Power', 'Distance', 'AvgTemperature')") %>% 
  
  # I would like to bring in weight here so when I cut-off too low watts below it is watts/kg
  
  inner_join(
    
    dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)), by = c("PCS" = "rider")) %>%
  
  # clean up the dates
  mutate(Y = str_sub(DATE, nchar(DATE)-3, nchar(DATE))) %>% 
  separate(DATE, into = c("weekday", "date", "drop"), sep = ",") %>% 
  mutate(date = paste0(str_trim(date),", ", Y)) %>% 
  select(-weekday, -drop, -Y) %>% 
  
  # clean up the stat values
  mutate(VALUE = str_replace(VALUE, "mi", ""), 
         VALUE = str_replace(VALUE, "W", ""),
         VALUE = ifelse(Stat == "AvgTemperature",
                        str_sub(VALUE, 1, nchar(VALUE)-8), VALUE),
         VALUE = as.numeric(VALUE)) %>% 
  
  mutate(date = lubridate::mdy(date)) %>% 
  unique() %>% 
  spread(Stat, VALUE) %>% 
  filter(!is.na(`Weighted Avg Power`)) %>% 
  janitor::clean_names() %>% 
  
  inner_join(all_stage_data %>%
               filter(time_trial == 1) %>%
               mutate(date = as.Date(date)), by = c("date", "pcs" = "rider")) %>% 
  
  mutate(distance = distance * 1.609) %>% 
  
  select(-win_seconds, -total_seconds,
         -parcours_value, -stage_type,
         -gain_40th, -gain_20th, -gain_10th,
         -leader_rating) %>%
  
  # clear out anomalous data
  mutate(wattskg = weighted_avg_power / weight) %>% 
  
  mutate(M = mean(wattskg, na.rm = T), 
         S = sd(wattskg, na.rm = T)) %>% 
  
  filter(abs((wattskg - M) / S) < 3 | is.na(weight)) %>%
  rename(rider = pcs) %>%
  group_by(stage, race, year, rider, date) %>%
  filter(rank(-weighted_avg_power, ties.method = "min") == 1) %>%
  ungroup() %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT * FROM strava_segment_data"), by = c("activity_id")
    
  ) %>%
  filter(time_trial == 1) %>%
  group_by(rider) %>% 
  mutate(rel_power = Power / mean(Power, na.rm = T)) %>% 
  ungroup() %>%
  
  select(-gain_1st, -gain_3rd, -gain_5th, -gc_pos, -gain_gc, -missing_profile_data, -uphill_finish, 
         -sof_limit, -success, -points_finish, -cobbles, -M, -S, -VerticalGain, -VAM, -HR, -Tour,
         -gc_winner, -time_trial, -stage_name, -Time, -Type, -rowname, -perc_break, -km_break, -bunch_sprint) %>%
  
  mutate(time = Distance / Speed * 3600) %>%
  
  unique() %>% 
  mutate(SegmentWattsKG = Power / weight)

#
#


