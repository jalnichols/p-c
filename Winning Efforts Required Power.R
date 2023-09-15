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
                  WHERE Stat IN ('Distance','AvgElapsed Time')") %>% 
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
                        WHERE year IN (2023)") %>%
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
  
  inner_join(
    
    fs::dir_info('C:/Users/Jake Nichols/Documents/Old D Drive/STRAVA_JSON/') %>%
      select(path, birth_time) %>%
      mutate(activity_id = str_replace(path, 'C:/Users/Jake Nichols/Documents/Old D Drive/STRAVA_JSON/strava-activity-id-', ''),
             activity_id = str_replace(activity_id, ".rds", "")), by = c("activity_id")) %>%
  
  filter(class != "JR") %>%
  
  group_by(stage, race, year, class, date) %>%
  filter(n() >= 15) %>%
  ungroup() %>%
  
  mutate(pcs = str_to_title(pcs)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)) %>%
      filter(!is.na(weight)) %>%
      group_by(rider) %>%
      summarize(weight = median(weight)) %>%
      ungroup(), by = c("pcs" = "rider"))

#
#
# 
#
#


# Supplemental Data -------------------------------------------------------

breakaway_riders <- dbReadTable(con, "pcs_km_breakaway_1") %>%
  
  mutate(rider = str_to_title(rider),
         url = str_replace(url, "/race", "race")) %>%
  
  left_join(dbGetQuery(con, "SELECT DISTINCT stage, url, length, date 
                       FROM stage_data_perf WHERE date >= '2016-01-01'"), by = c("stage", "url")) %>%
  
  group_by(url, race, stage, year, length) %>%
  mutate(average = mean(km_before_peloton),
         longest = max(km_before_peloton),
         perc_of = km_before_peloton / longest,
         perc_stage = km_before_peloton / length) %>%
  ungroup() %>%
  
  mutate(breakaway_rider = ifelse(perc_stage > 0.25 & (perc_of) > 0.33, 1, 0)) %>%
  select(-race, -average, -km_in_first_group, -km_before_peloton,
         -date, -length, -perc_of, -perc_stage, -longest) %>%
  anti_join(read_delim("breakaway-riders-2019.csv",
                       col_types = "cccc") %>%
              rename(url = `...4`) %>%
              fill(url, .direction = "down") %>%
              fill(stage, .direction = "down") %>%
              select(url, stage) %>%
              unique()) %>%
  rbind(read_delim("breakaway-riders-2019.csv",
                   col_types = "cccc") %>%
          rename(url = `...4`) %>%
          fill(url, .direction = "down") %>%
          
          mutate(year = ifelse(is.na(race), "", str_sub(race, nchar(race)-3, nchar(race))),
                 year = ifelse(year == "", str_sub(url, nchar(url)-3, nchar(url)), year)) %>%
          
          fill(race, .direction = "down") %>%
          fill(year, .direction = "down") %>%
          fill(stage, .direction = "down") %>%
          
          left_join(dbGetQuery(con, "SELECT rider, COUNT(*) as N FROM pcs_stage_data WHERE year >= 2016
                       GROUP BY rider")) %>%
          select(-race, -n) %>%
          mutate(breakaway_rider = 1)) %>%
  inner_join(dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE date >= '2016-01-01'") %>%
               
               mutate(date = as.Date(date)) %>%
               
               select(-speed, -gain_3rd, -gain_5th, -gain_10th, -gain_40th,
                      -time_trial, -gc_winner, -gc_pos, -parcours_value, -stage_type,
                      -avg_alt, -missing_profile_data) %>%
               
               select(-tm_pos) %>%
               select(stage, url, rider, rnk, class, race) %>%
               mutate(rider = str_replace(rider, "O C", "Oc"), 
                      rider = str_replace(rider, "O B", "Ob"), 
                      rider = str_replace(rider, "D H", "Dh"))) %>%
  unique()

#

winner_type <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE date >= '2016-01-01' AND rnk = 1") %>%
  
  mutate(date = as.Date(date)) %>%
  
  select(-speed, -gain_3rd, -gain_5th, -gain_10th, -gain_40th,
         -time_trial, -gc_winner, -gc_pos, -parcours_value, -stage_type,
         -avg_alt, -missing_profile_data, -tm_pos) %>%
  
  filter(year >= 2016) %>%
  
  inner_join(breakaway_riders %>%
               select(url, stage) %>%
               unique()) %>%
  
  select(rider, stage, race, year, class, date, length, url, pred_climb_difficulty, uphill_finish, total_vert_gain,
         sof, bunch_sprint, tour, one_day_race, grand_tour, gain_gc) %>%
  
  group_by(race, year, url, class) %>%
  mutate(perc_thru = rank(stage, ties.method = "first") / max(rank(stage, ties.method = "first"), na.rm = T)) %>%
  ungroup() %>%
  
  mutate(stage_join = as.character(stage)) %>%
  inner_join(dbGetQuery(con, "SELECT * FROM predictions_stage_bunchsprint") %>%
               select(-bunch_sprint) %>%
               select(stage, url, predicted_bs) %>%
               unique(), by = c("stage_join" = "stage", "url")) %>%
  select(-stage_join) %>%
  
  unique() %>%
  
  left_join(
    
    breakaway_riders %>%
      filter(breakaway_rider == 1 & rnk == 1) %>%
      select(rider, url, stage, breakaway_rider) %>%
      unique(), by = c("rider", "url", "stage")
    
  ) %>%
  
  mutate(breakaway_rider = ifelse(is.na(breakaway_rider), 0, breakaway_rider)) %>%
  
  mutate(winner_type = ifelse(breakaway_rider == 0, 0, 1))

#
#
#
#
#

# Bring in Power Efforts and Calculate ------------------------------------

all_efforts <- dbGetQuery(con, "SELECT * FROM strava_rolling_power_curves") %>%
  inner_join(telemetry_available %>%
               select(activity_id, stage, race, year, class, url, date, 
                      pred_climb_difficulty, rnk, bunch_sprint,
                      time_trial, team_time_trial, weight), by = c('activity_id')) %>%
  
  separate(url, c("j1", "url_race", "j2"), sep = "\\/", remove = FALSE) %>%
  select(-j1, -j2) %>%
  
  #group_by(rider, url_race, stage, rolling_speed) %>%
  #filter(n() == 5 & min(fbin)==0 & max(fbin)==0.8) %>%
  #ungroup() %>%
  
  group_by(rider, year, rolling_speed) %>%
  mutate(vs_rider_season = power / mean(power, na.rm = T)) %>%
  ungroup()

#
#
#

top_finishers <- all_efforts %>% 
  
  inner_join(
    all_efforts %>%
      select(rider, activity_id, url, stage, rnk) %>%
      unique() %>%
      filter(rnk <= 20) %>% # 20
      group_by(url, stage) %>% 
      filter(rank(rnk, ties.method = "min") <= 5) %>% # 5
      ungroup(), by = c("rider", "activity_id", "url", "stage", "rnk")) %>%
  
  group_by(rolling_speed, activity_id, stage, url) %>%
  filter(vs_rider_season == max(vs_rider_season, na.rm = T)) %>%
  ungroup() %>%
  
  left_join(
    breakaway_riders %>%
      select(rider, url, stage, breakaway_rider), by = c("url", "stage", "rider")) %>%
  
  left_join(
    winner_type %>%
      select(url, stage, winner_type), by = c("url", "stage")) %>%
  mutate(breakaway_rider = ifelse(is.na(breakaway_rider),
                                  ifelse(is.na(winner_type), NA, 0), breakaway_rider)) %>%
  
  group_by(rolling_speed, rider, stage, year, url, url_race, class, date, bunch_sprint, pred_climb_difficulty, 
           time_trial, team_time_trial, rnk, breakaway_rider, winner_type) %>% 
  summarize(relative = mean(power / weight, na.rm = T),
            absolute = mean(power, na.rm = T),
            vs_rider_season = mean(vs_rider_season, na.rm = T),
            distance_left = mean(distance_left, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(rolling_speed) %>%
  mutate(relative_vs_race = relative - mean(relative, na.rm = T),
         absolute_vs_race = absolute - mean(absolute, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(vs_rider_season_vs_race = vs_rider_season - mean(vs_rider_season, na.rm = T)) %>%
  
  filter(!is.na(relative)) %>%
  
  group_by(rolling_speed) %>%
  mutate(xlo = quantile(relative, probs = 0.005),
         xhi = quantile(relative, probs = 0.995)) %>%
  filter(relative > xlo & relative < xhi) %>%
  ungroup() %>%
  
  select(-xhi, -xlo)

#

rel_power_by_rnk <- top_finishers %>%
  group_by(rolling_speed) %>%
  do(broom::tidy(lm(relative ~ log(rnk), data = .))) %>%
  ungroup()

abs_power_by_rnk <- top_finishers %>%
  group_by(rolling_speed) %>%
  do(broom::tidy(lm(absolute ~ log(rnk), data = .))) %>%
  ungroup()

vrs_power_by_rnk <- top_finishers %>%
  group_by(rolling_speed) %>%
  do(broom::tidy(lm(vs_rider_season ~ log(rnk), data = .))) %>%
  ungroup()

#

top_finishers <- top_finishers %>%
  inner_join(abs_power_by_rnk %>%
               select(rolling_speed, term, estimate) %>%
               spread(term, estimate) %>%
               janitor::clean_names(), by = c("rolling_speed")) %>%
  mutate(adjusted_absolute = absolute - ((log(rnk)*log_rnk))) %>%
  select(-log_rnk, -intercept) %>%
  inner_join(rel_power_by_rnk %>%
               select(rolling_speed, term, estimate) %>%
               spread(term, estimate) %>%
               janitor::clean_names(), by = c("rolling_speed")) %>%
  mutate(adjusted_relative = relative - ((log(rnk)*log_rnk))) %>%
  select(-log_rnk, -intercept) %>%
  inner_join(vrs_power_by_rnk %>%
               select(rolling_speed, term, estimate) %>%
               spread(term, estimate) %>%
               janitor::clean_names(), by = c("rolling_speed")) %>%
  mutate(adjusted_vsriderseason = vs_rider_season - (intercept + (log(rnk)*log_rnk))) %>%
  select(-log_rnk, -intercept)

#

agg_finishers <- top_finishers %>%
 
  group_by(rolling_speed, stage, year, url, url_race, class, date, bunch_sprint, pred_climb_difficulty, 
           time_trial, team_time_trial, winner_type) %>%
  summarize(relative = mean(adjusted_relative, na.rm = T),
            absolute = mean(adjusted_absolute, na.rm = T),
            vs_rider_season = mean(adjusted_vsriderseason, na.rm = T),
            rnks = mean(rnk, na.rm = T),
            riders = n()) %>%
  ungroup() %>%
  
  rowwise() %>%
  
  mutate(g_year = ifelse(lubridate::month(lubridate::today()) >= 5, year, 
                         ifelse(year == lubridate::year(lubridate::today()), year-1, year))) %>%
  
  ungroup() %>%
  
  group_by(rolling_speed, g_year) %>%
  mutate(relative = ((relative * riders) + (2 * mean(relative, na.rm = T))) / (riders+2),
         absolute = ((absolute * riders) + (2 * mean(absolute, na.rm = T))) / (riders+2),
         vs_rider_season = ((vs_rider_season * riders) + (2 * mean(vs_rider_season, na.rm = T))) / (riders+2)) %>%
  ungroup() %>%
  
  mutate(compoundscore = relative * absolute) %>%
  
  group_by(rolling_speed, g_year) %>%
  mutate(relative_vsavg = relative - mean(relative, na.rm = T),
         absolute_vsavg = absolute - mean(absolute, na.rm = T),
         compoundscore_vsavg = compoundscore - mean(compoundscore, na.rm = T),
         vs_rider_season_vsavg = vs_rider_season - mean(vs_rider_season, na.rm = T)) %>%
  ungroup()

#

agg_finishers %>%
  
  group_by(rolling_speed, g_year) %>%
  mutate(pctrk_rel = percent_rank(relative_vsavg)-0.5,
         pctrk_abs = percent_rank(absolute_vsavg)-0.5,
         pctrk_vs = percent_rank(vs_rider_season_vsavg)-0.5,
         pctrk_compsc = percent_rank(compoundscore_vsavg)-0.5,
         sdrel = (relative_vsavg) / sd(relative),
         sdabs = (absolute_vsavg) / sd(absolute),
         sdvs = (vs_rider_season_vsavg) / sd(vs_rider_season_vsavg),
         sdcompsc = compoundscore_vsavg / sd(compoundscore_vsavg),
         rnks = rnks - mean(rnks, na.rm = T)) %>%
  ungroup() %>%
  
  select(rolling_speed, stage, url, year, class, date, 
         percentile_rel = pctrk_rel, percentile_abs = pctrk_abs, percentile_vs = pctrk_vs, pctrk_compsc,
         sdrel, sdabs, sdvs, sdcompsc,
         relative, absolute, vs_rider_season_vsavg) %>%
  filter(rolling_speed %in% c(15, 120, 300, 600, 1200, 2400)) %>%
  unique() %>%
  
  arrange(desc(date), url) %>%
  
  anti_join(dbGetQuery(con, "SELECT url, stage FROM win_efforts_by_race")) -> efforts_to_write

print(efforts_to_write)
  
dbWriteTable(con, "win_efforts_by_race", efforts_to_write, row.names = FALSE, append = TRUE)

#

etw %>% 
  filter(class %in% c("2.UWT")) %>% 
  
  ggplot(
    aes(x = reorder(as.character(ifelse(rolling_speed < 60, paste0(rolling_speed,"s"), paste0(rolling_speed/60, "m"))), rolling_speed), y = sdabs, group = rolling_speed))+
  geom_boxplot(alpha = 0.3)+
  geom_hline(yintercept = 0)+
  geom_point(data = etw %>% filter(url == "race/world-championship/2023"), color = "black", size = 3)+
  geom_point(data = etw %>% filter(url == "race/ronde-van-vlaanderen/2023"), color = "gold", size = 3)+
  geom_point(data = etw %>% filter(url == "race/milano-sanremo/2023"), color = "red", size = 3)+
  labs(x = "effort length",
       y = "standard deviation of power", 
       title = "2023 ME WC/Flanders/Sanremo vs World Tour races")

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

filtered_efforts = all_efforts %>%

  group_by(rolling_speed, activity_id, stage, url, fatigue_bin = ifelse(distance_gone < 50000, "fresher", "fatigued")) %>%
  filter(vs_rider_season == max(vs_rider_season, na.rm = T)) %>%
  ungroup() %>%

  left_join(
    breakaway_riders %>%
      select(rider, url, stage, breakaway_rider), by = c("url", "stage", "rider")) %>%

  left_join(
    winner_type %>%
      select(url, stage, winner_type), by = c("url", "stage")) %>%
  mutate(breakaway_rider = ifelse(is.na(breakaway_rider),
                                  ifelse(is.na(winner_type), NA, 0), breakaway_rider)) %>%

  group_by(rolling_speed, rider, stage, year, url, url_race, class, date, bunch_sprint, pred_climb_difficulty,
           time_trial, team_time_trial, rnk, breakaway_rider, winner_type, fatigue_bin) %>%
  summarize(relative = mean(power / weight, na.rm = T),
            absolute = mean(power, na.rm = T),
            vs_rider_season = mean(vs_rider_season, na.rm = T),
            distance_gone = mean(distance_gone, na.rm = T)) %>%
  ungroup() %>%

  group_by(rolling_speed) %>%
  mutate(relative_vs_race = relative - mean(relative, na.rm = T),
         absolute_vs_race = absolute - mean(absolute, na.rm = T)) %>%
  ungroup() %>%

  mutate(vs_rider_season_vs_race = vs_rider_season - mean(vs_rider_season, na.rm = T)) %>%

  group_by(rolling_speed) %>%
  mutate(xlo = quantile(relative, probs = 0.005),
         xhi = quantile(relative, probs = 0.995)) %>%
  filter(relative > xlo & relative < xhi) %>%
  ungroup() %>%

  select(-xhi, -xlo)

#
# This looks at rider season
#

peak_power_curve_efforts <- filtered_efforts %>%

  filter(!rolling_speed %in% c(45,90,180,450,900)) %>%
  
  group_by(rider, year, rolling_speed, fatigue_bin) %>%
  mutate(total_efforts = n()) %>%
  filter(rank(desc(relative), ties.method = "first") <= 3) %>%
  ungroup() %>%

  filter(rolling_speed >= 60) %>%

  group_by(rider, year, rolling_speed) %>%
  summarize(absolute = mean(absolute, na.rm = T),
            relative = mean(relative, na.rm = T),
            vs_rider_season = mean(vs_rider_season, na.rm = T)) %>%
  ungroup()

peak_power_curve_efforts %>%
  group_by(rider, year) %>%
  do(broom::tidy(lm(relative ~ log(rolling_speed), data = .))) %>%
  ungroup() -> many_curves

many_curves %>%
  group_by(year, term) %>%
  summarize(estimate = median(estimate, na.rm = T)) %>%
  ungroup()

#

peak_power_curve_efforts %>%
  filter(year != 2023) %>%
  filter(total_efforts >= 20) %>%

  group_by(rider, year, fatigue_bin) %>%
  do(broom::tidy(lm(absolute ~ log10(rolling_speed), data = .))) %>%
  ungroup() -> logged_power_models

#

average_effort <- filtered_efforts %>%
  
  filter(!rolling_speed %in% c(45,90,180,450,900)) %>%
  
  mutate(group = ifelse(class %in% c("1.UWT", "2.UWT"), "World Tour", "Other")) %>%

  group_by(rider, year, rolling_speed) %>%
  mutate(total_efforts = n()) %>%
  filter(rank(desc(relative), ties.method = "first") <= 3) %>%
  ungroup() %>%

  filter(rolling_speed >= 60) %>%

  group_by(rolling_speed, year, group) %>%
  summarize(relative = mean(relative, na.rm = T),
            absolute = mean(absolute, na.rm = T),
            riders = n_distinct(rider)) %>%
  ungroup()

#

winning_effort <- filtered_efforts %>%
  
  filter(!rolling_speed %in% c(45,90,180,450,900)) %>%
  
  filter(rnk == 1 & class %in% c("2.1", "1.1", "1.Pro", "1.HC", "2.HC", "2.Pro", "2.UWT", "1.UWT") &
           bunch_sprint == 0 & time_trial == 0 & team_time_trial == 0) %>%

  filter(rolling_speed >= 60) %>%

  group_by(rolling_speed) %>%
  summarize(relative = quantile(relative, na.rm = T, probs = 0.9),
            absolute = mean(absolute, na.rm = T, probs = 0.9),
            riders = n_distinct(rider)) %>%
  ungroup()

#

peak_power_curve_efforts %>%
  filter(rider == "Groves Kaden") %>%
  ggplot(aes(x = rolling_speed, y = absolute, color = as.factor(year)))+
  geom_smooth(se=F)+
  geom_point(size=2)+
  geom_smooth(data = average_effort %>%
              filter(year == 2022 & group == "World Tour"), aes(x = rolling_speed, y = relative*76),
            color = "black", linetype = 'dashed', se=F, span = 1)+

  geom_smooth(data = winning_effort, aes(x = rolling_speed, y = relative*76),
            color = "black", size = 1, se=F, span = 1)+

  scale_x_log10(breaks = c(60,120,300,600,1200,2400,3600))+
  labs(x = "effort length",
       y = "watts per kg",
       title = "Power Curve",
       color = 'season',
       subtitle = "dashed line = average WT rider\nsolid line = winners")

#

agg_finishers %>% 
  
  group_by(rolling_speed, url_race, time_trial, team_time_trial) %>%
  summarize(relative = mean(relative, na.rm = T),
            absolute = mean(absolute, na.rm = T),
            vs_rider_season = mean(vs_rider_season, na.rm = T),
            relative_vsavg = mean(relative_vsavg, na.rm = T),
            absolute_vsavg = mean(absolute_vsavg, na.rm = T),
            riders = n()) %>%
  ungroup() %>%
  
  filter(url_race %in% c("ronde-van-vlaanderen", "milano-sanremo", "liege-bastogne-liege")) %>% 
  ggplot(aes(x = rolling_speed, y = relative_vsavg, fill = absolute_vsavg))+
  geom_hline(yintercept=0)+
  geom_col(position = "dodge")+
  theme(legend.position = "bottom")+
  scale_x_log10(breaks = c(10,15,30,45,60,90,120,180,300,450,600,900,1200,2400,3600))+
  facet_wrap(~paste0(url_race))+
  scale_fill_viridis_c()


#

agg_finishers %>%
  filter(class == '2.UWT' & winner_type == 'Breakaway' & url_race %in% c("giro-d-italia","vuelta-a-espana","tour-de-france") & year == 2022) %>% 
  ggplot(aes(x = rolling_speed, y = absolute_vsavg, fill = absolute))+
  geom_hline(yintercept=0)+
  geom_col(position = "dodge")+
  theme(legend.position = "bottom")+
  scale_x_log10(breaks = c(10,15,30,45,60,90,120,180,300,450,600,900,1200,2400,3600))+
  facet_wrap(~paste0(url,"-",stage))+
  scale_fill_viridis_c()

#

agg_finishers %>% 
  filter(class %in% c("1.1", "2.1", "1.HC", "2.HC", "1.Pro", "2.Pro", "WC", "Olympics", "1.UWT", "2.UWT")) %>%
  filter(!is.na(bunch_sprint) & !(time_trial == 1 & bunch_sprint == 1)) %>%
  mutate(race_type = ifelse(time_trial == 1, "ITT",
                            ifelse(team_time_trial == 1, "TTT",
                                   ifelse(bunch_sprint == 1, "BS",
                                          #ifelse(winner_type == "Breakaway", "Break",
                                          ifelse(pred_climb_difficulty >= 6, "Climbing", "Other")))),
         race_type = ifelse(race_type == "Other",
                            ifelse(url_race %in% c("gent-wevelgem", 
                                                   "kuurne-brussel-kuurne",
                                                   "omloop-het-nieuwsblad", "paris-tours", "ronde-van-vlaanderen",
                                                   "e3-harelbeke", "dwars-door-vlaanderen", "paris-roubaix", 
                                                   "bretagne-classic"), "Classics", "Other"), race_type)) %>%
  group_by(rolling_speed, time_trial, race_type) %>%
  summarize(absolute = sum(absolute * riders, na.rm = T) / sum(riders), 
            relative = sum(relative * riders, na.rm = T) / sum(riders),
            vs_rider_season = sum(vs_rider_season * riders, na.rm = T) / sum(riders),
            n=n()) %>%
  ungroup() -> Averages_across_race_type

Averages_across_race_type %>%
  #filter(time_trial == 0) %>%
  ggplot(aes(x = rolling_speed, y = absolute, color = race_type))+
  geom_hline(yintercept = 0)+
  geom_line()+
  geom_point(size=4)+
  scale_x_log10(breaks = c(10,15,30,45,60,90,120,180,300,450,600,900,1200,2400,3600))+
  theme(panel.grid.minor = element_blank())+
  scale_color_manual(values = c("gray", "#37B36C", "blue", "orange", "dark red", "purple", "black"), name = "Race type")

#

ggplot(top_finishers %>% 
         filter(class %in% c("1.1", "2.1", "1.HC", "2.HC", "1.Pro", "2.Pro", "WC", "Olympics", "1.UWT", "2.UWT")) %>% 
         group_by(rolling_speed, time_trial, breakaway_rider) %>%
         summarize(vs_rider_season = mean(adjusted_vsriderseason, na.rm = T),
                   n=n()) %>%
         ungroup() %>% 
         filter(!is.na(breakaway_rider)), 
       aes(x = rolling_speed, y = vs_rider_season, color = as.factor(breakaway_rider)))+
  geom_hline(yintercept = 0)+
  geom_line()+
  geom_point(size=4)+
  scale_x_log10(breaks = c(10,15,30,45,60,90,120,180,300,450,600,900,1200,2400,3600))+
  theme(panel.grid.minor = element_blank())+
  scale_color_manual(values = c("black", "red"), name = "in break?")+
  labs(title = "Top Finishers Only")
