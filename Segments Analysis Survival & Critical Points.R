
library(tidyverse)
library(DBI)

dbDisconnect(con)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      port = 5432,
                      host = 'localhost',
                      dbname = "cycling",
                      user = "postgres",
                      password = "braves")

#####
#####
##### Bring in data

all_stage_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE year > 2015") %>%
  
  mutate(date = as.Date(date),
         stage = as.character(stage)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, bib, race, year FROM pcs_all_startlists") %>%
      unique(), by = c("rider", "year", "race"))

#

breakaway_riders <- dbReadTable(con, "pcs_km_breakaway_1") %>%
  
  mutate(rider = str_to_title(rider)) %>%
  
  group_by(race, stage, year) %>%
  mutate(average = mean(km_before_peloton)) %>%
  ungroup() %>%
  
  mutate(breakaway_rider = ifelse(km_before_peloton > 40, 1, 0)) %>%
  
  mutate(url = str_replace(url, "/race", "race")) %>% 
  
  select(-race, -average, -km_in_first_group, -km_before_peloton) %>%
  
  rbind(read_delim("breakaway-riders-2019.csv",
                   col_types = "cccc") %>%
          rename(url = `...4`) %>%
          fill(url, .direction = "down") %>%
          
          mutate(year = ifelse(is.na(race), "", str_sub(race, nchar(race)-3, nchar(race))),
                 year = ifelse(year == "", str_sub(url, nchar(url)-3, nchar(url)), year)) %>%
          
          fill(race, .direction = "down") %>%
          fill(year, .direction = "down") %>%
          fill(stage, .direction = "down") %>%
          
          left_join(dbGetQuery(con, "SELECT rider, COUNT(*) as N FROM pcs_stage_data WHERE year >= 2018
                       GROUP BY rider")) %>%
          select(-race, -n) %>%
          mutate(breakaway_rider = 1)) %>%
  
  inner_join(dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE date > '2016-01-01'") %>%
               
               filter(time_trial == 0 & team_time_trial == 0) %>%
               filter(!is.na(bunch_sprint)) %>%
               filter(!is.na(pred_climb_difficulty)) %>%
               
               mutate(team_ldr = ifelse(tm_pos == 1, 1, 0)) %>%
               
               mutate(date = as.Date(date)) %>%
               
               select(-speed, -gain_3rd, -gain_5th, -gain_10th, -gain_40th,
                      -time_trial, -gc_winner, -gc_pos, -parcours_value, -stage_type,
                      -avg_alt, -missing_profile_data) %>%
               select(stage, url, rider, rnk, class, race) %>%
               mutate(rider = str_replace(rider, "O C", "Oc"), 
                      rider = str_replace(rider, "O B", "Ob"), 
                      rider = str_replace(rider, "D H", "Dh"))) %>%
  unique()

#
#
#

stage_level_power <- dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Weighted Avg Power', 'Distance', 'AvgSpeed', 'AvgPower', 'AvgTemperature',
                                'AvgHeart Rate', 'MaxHeartRate')") %>% 
  mutate(pcs = ifelse(pcs %in% c("Oconnor Ben", "OConnor Ben"), "O'connor Ben",
                      ifelse(pcs %in% c("Obrien Kelland", "OBrien Kelland"), "O'brien Kelland", pcs))) %>%
  rename(PCS = pcs, VALUE = value, Stat = stat, DATE = date) %>%
  
  # I would like to bring in weight here so when I cut-off too low watts below it is watts/kg
  
  mutate(PCS = str_to_title(PCS)) %>%
  
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
         VALUE = str_replace(VALUE, "bpm", ""),
         VALUE = str_replace(VALUE, "℉", ""),
         VALUE = ifelse(Stat == "AvgTemperature",
                        ifelse(nchar(VALUE) > 4, str_sub(VALUE, 1, nchar(VALUE)-8), VALUE), VALUE),
         VALUE = as.numeric(VALUE)) %>% 
  
  mutate(date = lubridate::mdy(date)) %>% 
  unique() %>% 
  spread(Stat, VALUE) %>% 
 
  janitor::clean_names() %>% 

  inner_join(all_stage_data %>%
               mutate(date = as.Date(date)), by = c("date", "pcs" = "rider")) %>% 
  
  # if two results exist for same day matching distance, it's probably a recon and TT which
  # means drop the lower watts
  
  # also, many riders include distance outside the TT as part of their strava activity
  # so maybe accept any riders +/- 10 km? or maybe we just can't get accurate TT data
  
  mutate(distance = distance * 1.609) %>% 
  filter((distance / length) > 0.5) %>%
  filter((distance / length) < 2) %>%
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
  ungroup() %>%
  
  left_join(breakaway_riders %>% select(-race, -year), by = c("url", "stage", "rider", "class", "rnk")) %>%
  
  mutate(diff = abs(1 - (distance / length))) %>%
  
  group_by(url, stage, length, date) %>%
  mutate(closest = rank(diff, ties.method = "first")) %>% 
  ungroup() %>%
  
  group_by(pcs) %>% 
  mutate(rel_HR = avg_heart_rate / mean(avg_heart_rate, na.rm = T)) %>%
  ungroup()

#

dbWriteTable(con, "race_level_temperature",
             stage_level_power %>%
               group_by(stage, race, year, class, date, length, url) %>%
               summarize(temperature = mean(avg_temperature, na.rm = T)) %>%
               ungroup(),
             overwrite = TRUE,
             row.names = FALSE)

#
#
#
#

relative_power <- stage_level_power %>%
  
  select(-c(bib, success, sof_limit, points_finish, cobbles)) %>%
  
  group_by(rider, year) %>%
  filter(!is.na(weighted_avg_power)) %>%
  filter(n() >= 25) %>%
  mutate(weighted_avg_powerTT = ifelse(time_trial == 0 & team_time_trial == 0, weighted_avg_power, NA),
         weighted_avg_powerTT = max(weighted_avg_powerTT, na.rm = T),
         relative_power = weighted_avg_power / weighted_avg_powerTT) %>%
  ungroup()

relative_power %>%
  mutate(scale_to_300_max = relative_power * 300 / weight) %>%
  filter(race == "tour de france" & time_trial == 0 & stage != 21) %>%
  
  group_by(rider, year) %>%
  summarize(KJ_kg = mean(scale_to_300_max * total_seconds / 1000, na.rm = T), 
            stages = n()) -> xyz

#

by_race <- relative_power %>%
  group_by(url, date, race, stage, year, class, time_trial, team_time_trial) %>%
  summarize(median = median(relative_power, na.rm = T),
            mean = mean(relative_power, na.rm = T),
            weighted = mean(weighted_avg_power, na.rm = T),
            kilojoules = mean(avg_power * total_seconds, na.rm = T) / 1000,
            above90 = mean(relative_power > 0.9, na.rm = T),
            above95 = mean(relative_power > 0.95, na.rm = T),
            max = mean(relative_power == 1, na.rm = T),
            riders = n()) %>%
  ungroup()

by_rider <- relative_power %>%
  filter(str_detect(url, "tour-de-france")) %>% 
  filter(time_trial == 0 & team_time_trial == 0) %>%
  group_by(rider, url, year) %>%
  summarize(median = median(relative_power, na.rm = T),
            mean = mean(relative_power, na.rm = T),
            weighted = mean(weighted_avg_power/weight, na.rm = T),
            kilojoules = mean(avg_power * total_seconds, na.rm = T) / 1000 / mean(weight, na.rm = T),
            above90 = mean(relative_power > 0.9, na.rm = T),
            above95 = mean(relative_power > 0.95, na.rm = T),
            max = mean(relative_power == 1, na.rm = T),
            riders = n()) %>%
  ungroup()

#

library(gt)

diff <- by_race %>%
  filter(str_detect(url, "tour-de-france")) %>% 
  filter(stage %in% c(1,2,3,4,5,6,7,8,9,10) & time_trial == 0 & team_time_trial == 0) %>% 
  group_by(url) %>%
  filter(rank(as.numeric(stage), ties.method = "first") <= 9) %>%
  ungroup() %>%
  
  group_by(year) %>%
  summarize(median = mean(median),
            weighted = mean(weighted),
            kilojoules = sum(kilojoules),
            stage = paste0(min(as.numeric(stage)),"-",max(as.numeric(stage)))) %>%
  ungroup() %>%
  
  select(Year = year, Stages = stage, `% of Max Effort` = median, `Normalized Power` = weighted, `Kilojoules` = kilojoules) %>%
  
  gt() %>%
  tab_header(
    title = 'Tour de France First Nine Road Stages Difficulty',
    subtitle = 'in Strava era (2016-23)'
  ) %>%
  fmt_percent(
    columns = c(`% of Max Effort`),
    decimals = 0
  ) %>%
  fmt_integer(
    columns = c(`Normalized Power`, `Kilojoules`)
  )

gtsave(data = diff, 'tdf-stage-9-difficulty.png', expand = 20)

#
#
#

power_by_finpos <- lm(rel ~ logrnk + pred_climb_difficulty + avg_temperature + perc_break,
                      data = stage_level_power %>%
                        filter(time_trial == 0) %>%
                        filter(!is.na(weighted_avg_power)) %>%
                        filter(!is.na(perc_break)) %>%
                        filter(!is.na(weight)) %>%
                        
                        filter((weighted_avg_power/weight) > 2) %>%
                        filter((weighted_avg_power/weight) < 6) %>%
                        
                        group_by(pcs) %>%
                        filter(n() >= 20) %>%
                        mutate(rel = weighted_avg_power / median(weighted_avg_power, na.rm = T)) %>%
                        ungroup() %>%
                        
                        mutate(logrnk = log(rnk)))

summary(power_by_finpos)

#

residuals <- cbind(stage_level_power %>%
                     filter(time_trial == 0) %>%
                     filter(!is.na(perc_break)) %>%
                     filter(!is.na(weighted_avg_power)) %>%
                     filter(!is.na(weight)) %>%
                     
                     filter((weighted_avg_power/weight) > 2) %>%
                     filter((weighted_avg_power/weight) < 6) %>%
                     
                     group_by(pcs) %>%
                     filter(n() >= 20) %>%
                     mutate(rel = weighted_avg_power / median(weighted_avg_power, na.rm = T)) %>%
                     ungroup() %>%
                     
                     mutate(logrnk = log(rnk)),
                   RESID = power_by_finpos$residuals)

#
#
#

segment_data_races <- stage_level_power %>%
  
  filter(url %in% c(
    paste0("race/paris-roubaix/", seq(2017,2023,1)),
    paste0("race/ronde-van-vlaanderen/", seq(2017,2023,1)),
    paste0("race/liege-bastogne-liege/", seq(2017,2023,1)),
    paste0("race/strade-bianche/", seq(2017,2023,1))
    )) %>%

  inner_join(
    
    dbGetQuery(con, "SELECT rowname, Segment,
               Distance, 
               Gradient, 
               Speed,
               Power, 
               HR,
               Type,
               activity_id
               FROM strava_segment_data") %>%
      rename(Distance = distance,
             Gradient = gradient,
             Power = power,
             HR = hr,
             Speed = speed,
             Type = type,
             Segment = segment), by = c("activity_id")) %>%
  
  select(-gain_3rd, -gain_5th, -gc_pos, -gain_gc, -missing_profile_data, 
         -sof_limit, -success, -points_finish, -cobbles,
         -gc_winner, -bib,
         -pcs, -master_team, -avg_alt,
         -total_vert_gain, -speed) %>%
  
  mutate(rowname = as.numeric(rowname)) %>% 
  
  mutate(time = Distance / Speed * 3600)

#

segment_data_races %>% 
  filter(url == "race/trofeo-alcudia/2022" & Segment == "ColldesaBatalla(fromCaimari)") %>% 
  mutate(wattskg = Power/weight) %>% 
  
  group_by(rnk <= 46, rnk == 200) %>%
  summarize(median(wattskg, na.rm = T), 
            median(Power, na.rm = T),
            n = n()) %>%
  ungroup()

#

segments_position <- dbGetQuery(con, "SELECT * FROM strava_segments_position") %>%
  
  group_by(Segment = segment, Gradient = gradient, Distance = distance, activity_id) %>%
  mutate(OrderInRace = rank(perc_thru, ties.method = "first")) %>%
  ungroup() %>%
  
  inner_join(stage_level_power %>%
               select(activity_id, url, stage, race, year, class, date, length), by = c("activity_id")) %>%
  
  select(Segment, OrderInRace, url, stage, race, year, class, date, length, Distance, Gradient, perc_thru, km_thru,
         activity_distance)
  
#

segments_to_be_analyzed <- segment_data_races %>%
  
  group_by(Segment, rider, race, stage, year, date, length, class) %>% 
  mutate(OrderInRace = rank(rowname, ties.method = "first")) %>%
  ungroup() %>% 
  
  mutate(Distance = round(Distance,2),
         Gradient = round(Gradient,3)) %>%
  
  inner_join(segments_position %>%
               
               mutate(Distance = round(Distance,2),
                      Gradient = round(Gradient,3)) %>%
               
               group_by(Segment, OrderInRace, Distance, Gradient, stage, race, year, class, length, date) %>%
               summarize(perc_thru = mean(perc_thru, na.rm = T),
                         activity_distance = mean(activity_distance, na.rm = T),
                         km_thru = mean(km_thru, na.rm = T)) %>%
               ungroup(), by = c("Segment", "OrderInRace", "stage", "race", "year", "class", "date",
                                 "length", "Distance", "Gradient"))

#

segments_to_be_analyzed %>%
  filter(url == "race/strade-bianche/2023") %>% 
  filter(rnk <= 5 & rnk >= 3) %>% 
  
  group_by(Segment, OrderInRace, km_thru, Distance, Gradient) %>% 
  mutate(rel_time = mean(time, na.rm = T) - time, comps = n()) %>% 
  ungroup() %>% 
  
  mutate(secs_per_KM = rel_time / Distance) %>% 
  select(-c(HR, Power, activity_distance, perc_thru, Type)) -> RelSegments

RelSegments %>%
  filter(rnk == 3) %>% 
  
  ggplot()+
  geom_rect(aes(xmin = km_thru, xmax = km_thru - Distance, ymin = Gradient-0.01, ymax = Gradient+0.01,
                fill = rel_time/Distance))+
  
  labs(x = "km thru", y = 'Gradient', title = 'Pidcock at Strade')+
  
  scale_fill_gradientn(colors = c("dark red", "red", "white", "#43E75F", "#00A51C", "#007313"),
                        name = "time vs contenders")

#
#
#

SB_history <- segments_to_be_analyzed %>%
  filter(race == "strade bianche") %>%
  filter(rnk <= 25) %>%
  
  group_by(Segment, OrderInRace, km_thru, Distance, Gradient) %>% 
  mutate(rel_time = mean(time, na.rm = T) - time, comps = n()) %>% 
  ungroup() %>% 
  
  mutate(secs_per_KM = rel_time / Distance) %>% 
  select(-c(HR, Power, activity_distance, perc_thru, Type))
  
SB_history %>% 
  filter(Segment %in% c("StradeBianche/L'Eroica-MonteSanteMarie", "StradeBianche-CollePinzuto", "tolfecompleto", 
                        "StradeBianche:Fontebranda-SanCaterina", "SLITASANMARTINOINGRANIA", "StradeBianche-Monteaperti",
                        "StradeBianche/L'Eroica-PieveaSalti", "EroicaMontalcino96km1°ristoro->2°ristoro",
                        "CastelnuovotoDelPiero")) -> top_sectors

top_sectors %>%

  group_by(url, Segment) %>%
  summarize(km_thru = mean(km_thru, na.rm = T), 
            time = mean(time, na.rm = T), 
            files = n()) %>%
  ungroup() -> SB_top

#


#

where_are_peak_efforts_segments <- segments_position %>% 
  filter(url == "race/trofeo-alcudia/2022") %>% 
  group_by(url, stage, Segment, Distance, Gradient) %>% 
  mutate(unique_acts = n_distinct(activity_distance), 
         km_thru = median(km_thru, na.rm = T)) %>% 
  ungroup() %>% 
  
  expand_grid(dbGetQuery(con, "SELECT * FROM peak_power_locations WHERE rolling_speed IN (30,120,300,600,1200,2400)") %>% 
                filter(url_race == "trofeo-alcudia") %>% 
                select(rolling_speed, finish_group, q20, q80, distance_gone, start_distance, avgrnk, n) %>%
                mutate(start_distance = start_distance/1000)) %>% 
  filter(distance_gone <= km_thru + Distance & distance_gone >= km_thru)

#

SB_history <- segments_to_be_analyzed %>%
  filter(str_detect(race, "ronde van vlaanderen")) %>%
  filter(rnk <= 25) %>%
  
  group_by(Segment, OrderInRace, km_thru, Distance, Gradient) %>% 
  mutate(rel_time = mean(time, na.rm = T) - time, comps = n()) %>% 
  ungroup() %>% 
  
  mutate(secs_per_KM = rel_time / Distance) %>% 
  select(-c(HR, Power, activity_distance, perc_thru, Type))

SB_history %>% 
  filter(Segment %in% c("OudeKwaremont+Paterberg", "Kruisberg-Hotondberg", 
                        "Taaienberg", "SteenbeekdriesWall*-flandreslove.com", "koppenbergfullclimb",
                        "AanloopKortekeer", "KanariebergFlandersHill", "KOMWolvenberg2014",
                        "finishrvv240km")) -> top_sectors

top_sectors %>%
  
  group_by(url, Segment, OrderInRace) %>%
  summarize(km_thru = mean(km_thru, na.rm = T), 
            time = mean(time, na.rm = T), 
            files = n()) %>%
  ungroup() -> SB_top

#

# PARIS ROUBAIX

segments_to_be_analyzed %>%  mutate(PAVE = ifelse(str_detect(str_to_lower(Segment), "pav") & !str_detect(Segment, "entresecteur"), 1, 0)) %>% filter(Distance < 10 & Distance > 0.4) %>% group_by(Distance, Gradient, OrderInRace, Segment, stage, race, year, date) %>% mutate(perc_thru = mean(perc_thru), rel_time = time / median(time)) %>% ungroup() %>% filter(rel_time > 0.5 & rel_time < 2) %>% mutate(year = as.character(year)) %>% mgcv::gam(Speed ~ Gradient + s(perc_thru, k = 6) + PAVE + year + log(rnk), data = .) -> GAM_MOD

segments_to_be_analyzed %>% mutate(PAVE = ifelse(str_detect(str_to_lower(Segment), "pav") & !str_detect(Segment, "entresecteur"), 1, 0)) %>% filter(Distance < 10 & Distance > 0.4) %>% group_by(Distance, Gradient, OrderInRace, Segment, stage, race, year, date) %>% mutate(perc_thru = mean(perc_thru), rel_time = time / median(time)) %>% ungroup() %>% filter(rel_time > 0.5 & rel_time < 2) %>% mutate(year = as.character(year)) %>% mutate(pred = mgcv::predict.gam(GAM_MOD, .)) %>% group_by(Segment) %>% summarize(perc_thru = mean(perc_thru), pred = median(pred), actual = median(Speed)) %>% ungroup() %>% mutate(rel = actual / pred) -> SEGMENTS

#

All_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE date > '2016-06-30'") %>%
  
  filter(time_trial == 0 & team_time_trial == 0) %>%
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  
  mutate(points_per_opp = ifelse(tm_pos == 1, points_finish, NA),
         sof_per_opp = ifelse(tm_pos == 1, sof, NA),
         pred_climb_diff_opp = ifelse(tm_pos == 1, pred_climb_difficulty, NA),
         pred_climb_diff_succ = ifelse(points_finish > 0, pred_climb_difficulty, NA),
         team_ldr = ifelse(tm_pos == 1, 1, 0)) %>%
  
  mutate(date = as.Date(date)) %>%
  
  select(-speed, -gain_3rd, -gain_5th, -gain_10th, -gain_40th,
         -time_trial, -gc_winner, -gc_pos, -parcours_value, -stage_type,
         -avg_alt, -missing_profile_data) %>%
  
  filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC", "Olympics")) |
           (class %in% c("2.1", "1.1") & tour == "Europe Tour") |
           race %in% c("tour colombia 2.1", "colombia 2.1", "vuelta a san juan internacional", "saudi tour")) %>%
  
  left_join(read_csv("cobbles.csv") %>% mutate(stage = as.character(stage))) %>% 
  
  mutate(cobbles = ifelse(is.na(cobbles), 0, cobbles)) %>%
  
  mutate(final_group = ifelse(gain_1st <= 2, 1, 0),
         sprinted = ifelse(gain_1st == 0 & rnk <= 20, 1, 0),
         top10 = ifelse(rnk <= 10, 1, 0)) %>%
  
  select(-gain_20th)

#

breakaway_riders <- dbReadTable(con, "pcs_km_breakaway") %>%
  
  mutate(rider = str_to_title(rider)) %>%
  
  group_by(race, stage, year) %>%
  mutate(average = mean(km_before_peloton)) %>%
  ungroup() %>%
  
  mutate(breakaway_rider = ifelse(km_before_peloton > 40, 1, 0)) %>%
  
  mutate(url = str_replace(url, "/race", "race")) %>% 
  
  inner_join(All_data %>%
               select(stage, url, rider, rnk, class) %>%
               mutate(rider = str_replace(rider, "O C", "Oc"), 
                      rider = str_replace(rider, "O B", "Ob"), 
                      rider = str_replace(rider, "D H", "Dh")))

#

final_climbs <- segments_to_be_analyzed %>%
  
  filter(Gradient > 0.03 & Distance > 2 & year >= 2020) %>%
  
  mutate(endKM = km_thru + Distance,
         VM = Distance * Gradient * 1000) %>%
  
  filter((endKM / activity_distance) > 0.99) %>%
  
  group_by(Segment, stage, race, year, class, date, length) %>%
  mutate(n = n()) %>%
  filter(n >= 20) %>%
  ungroup() %>%
  
  mutate(rider = str_replace(rider, "O C", "Oc"), 
         rider = str_replace(rider, "O B", "Ob"), 
         rider = str_replace(rider, "D H", "Dh")) %>%
  
  mutate(race = ifelse(race == "giro d italia", "giro ditalia", race)) %>%
  
  group_by(stage, race, year, class, date, length) %>%
  mutate(maxVM = max(VM, na.rm = T),
         maxDist = max(Distance, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(percVM = VM / maxVM,
         percDist = Distance / maxDist,
         otherVM = maxVM - VM,
         otherDist = maxDist - Distance,
         otherGradient= otherVM / otherDist) 

#

final_climbs %>% 
  group_by(stage,race,year,class,date,length,Segment, Distance, Gradient, VM, OrderInRace, 
           perc_thru, maxDist, maxVM, percDist, percVM, otherGradient, otherVM) %>% 
  count() %>% 
  ungroup() %>% 
  
  group_by(stage,race,year,class,date,length) %>% 
  mutate(otherGradient = ifelse(is.nan(otherGradient), Gradient, otherGradient)) %>% 
  mutate(minDist = min(Distance),
         minVM = min(VM)) %>%
  mutate(rkD = ifelse(Distance == maxDist, 1, 
                      ifelse(Distance == minDist, 0, 
                             ((Distance-minDist)/(maxDist-minDist)))),
         rkVM = ifelse(VM == maxVM, 1, 
                       ifelse(Distance == minVM, 0, 
                              ((VM-minVM)/(maxVM-minVM)))),
         rkOth = percent_rank(-1 * otherGradient),
         rkRatio = percent_rank(percVM/percDist),
         rkRatio = (rkRatio+rkOth)/2) %>%
  mutate(n = n()) %>%
  rowwise() %>%
  mutate(Overall = (rkRatio+rkD+rkVM)/3) %>%
  ungroup() %>%
  group_by(stage,race,year,class,date,length) %>%
  filter(max(Overall, na.rm = T)==Overall | n == 1) %>%
  ungroup()  -> best_segment_to_use

#

final_climb_model <- final_climbs %>%

  inner_join(best_segment_to_use %>%
               select(Segment, Distance, Gradient, OrderInRace, stage, race, year, class, date, length)) %>%
  
  left_join(breakaway_riders %>%
              select(rider, race, year, stage, class, breakaway_rider)) %>%
  
  mutate(breakaway_rider = ifelse(is.na(breakaway_rider), 0, breakaway_rider)) %>%
  
  group_by(stage, race, year, class, date, length) %>% 
  filter(max(breakaway_rider) > 0) %>%
  ungroup() %>% 
  
  lm(Speed ~ Distance * Gradient + log(rnk) + avg_temperature + pred_climb_difficulty + breakaway_rider,
     data = .)
  
summary(final_climb_model)

#

dvb_PR <- segments_to_be_analyzed %>% 
  filter(year==2022) %>%
  
  filter(rnk %in% c(1,2,4,5,11)) %>% 
  
  select(rider, Segment, Distance, Gradient, time, perc_thru, km_thru) %>% 
  spread(rider, time) %>%
  gather(rider, time, `Devriendt Tom`:`Van Aert Wout`) %>%
  
  mutate(vs_remco = round(time - `Van Baarle Dylan`,0)) %>%
  
  mutate(end_km = km_thru + Distance)

#

evenepoel <- segments_to_be_analyzed %>% 
  filter(year==2022 & !Segment == "RueduRemicham") %>%
  
  filter(rnk %in% c(1,3,8,10,16)) %>% 
  
  select(rider, Segment, Distance, Gradient, time, perc_thru, km_thru) %>% 
  spread(rider, time) %>%
  
  mutate(vs_remco = round(((`Powless Neilson` + `Van Aert Wout` + `Woods Michael`)/3) - `Evenepoel Remco`,0), 
         armirail = round(`Armirail Bruno` - `Evenepoel Remco`, 0)) %>%
  
  mutate(end_km = km_thru + Distance)


pogacar <- segments_to_be_analyzed %>% 
  filter(year==2021 & !Segment == "RueduRemicham") %>%
  
  filter(rnk %in% c(1,3,5,7,10)) %>% 
  
  select(rider, Segment, Distance, Gradient, time, perc_thru, km_thru) %>% 
  spread(rider, time) %>%
  
  mutate(vs_remco = round(((`Benoot Tiesj` + `Gaudu David` + `Woods Michael`)/3) - `Pogacar Tadej`,0), 
         armirail = round(`Mohoric Matej` - `Pogacar Tadej`, 0)) %>%
  
  mutate(end_km = km_thru + Distance)

#

romandie4 <- segments_to_be_analyzed %>% 
  filter(year==2020 & stage == 2) %>%
  
  filter(rnk %in% c(5,16,114)) %>% 
  
  select(rider, Segment, Distance, Gradient, time, perc_thru, km_thru) %>% 
  spread(rider, time) %>%
  
  mutate(end_km = km_thru + Distance)

#

segment_data_races %>%
  filter(time_trial == 0 & pred_climb_difficulty > 2) %>%
  anti_join(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
              select(stage, race, year, class) %>%
              unique(), by = c("stage", "race", 'year', "class")) %>%

  group_by(race, stage, year, date, class, pred_climb_difficulty, length, uphill_finish) %>% summarize(n = n_distinct(rider)) %>% ungroup() %>% 
  arrange(desc(n)) -> r

#

abc <- segment_data_races %>%
  filter(time_trial == 0 & pred_climb_difficulty > 2) %>%
  anti_join(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
              select(stage, race, year, class) %>%
              unique(), by = c("stage", "race", 'year', "class")) %>%
  
  #filter(year == 2021) %>%
  filter(rnk != 200) %>%
  select(activity_id, rider, rnk, stage, race, year, class, rowname, Segment, Distance, Gradient, Type, distance, length)

#

single_kms <- segment_data_races %>%
  
  select(-Type, -rowname, -tm_pos,
         -sof, -bunch_sprint, -uphill_finish,
         -total_seconds, -gain_1st, -grand_tour,
         -one_day_race, -distance, -activity_id) %>%
  
  filter(Distance > 0.9 & Distance < 1.6 & Gradient > 0) %>%
  filter(time_trial == 0) %>%
  inner_join(read_csv("candidates_1km.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
               select(stage, race, year, class) %>% 
               unique(), by = c("race", "stage", 'year', "class")) %>%
  inner_join(read_csv("candidates_1km.csv", locale = readr::locale(encoding = 'ISO-8859-1')), 
             by = c("Segment", "race", "stage", 'year', "class", "Gradient", "Distance")) %>%
  
  mutate(wattskg = Power / weight) %>%
  
  mutate(seg_number = abs(parse_number(str_replace_all(Segment, "9,5", "9.5"))))

#

single_kms %>%
  
  filter(str_detect(Segment, "Masdesvignes") & race == "tour de la provence" & year == 2020) %>%
  
  group_by(rider, stage, race, year, class, rnk) %>% 
  do(broom::tidy(lm(Speed ~ seg_number, data = .))) %>%
  ungroup() %>% 
  
  select(-std.error, -statistic, -p.value) %>% 
  spread(term, estimate) -> chalet_reynard

single_kms %>% 
  filter(str_detect(Segment, "Masdesvignes") & race == "tour de la provence" & year == 2020) %>% 
  
  group_by(Segment, stage, race, year, class) %>% 
  mutate(rel_power = Speed / mean(Speed, na.rm = T)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = seg_number, y = rel_power, group = rider, label = rider, color = (rider %in% c('Bernal Egan',"Mader Gino"))))+
  
  geom_line()+
  
  geom_text(data = . %>% 
              group_by(rider) %>% filter(rank(rider, ties.method = "random") == 1) %>% ungroup())+
  guides(color = F)+
  scale_color_manual(values = c("gray30", "red"))
  
#
#
#

strava_before_segments <- dbReadTable(con, "strava_stats_by_kilometer") %>%
  
  filter(altitude < 4000 & vertical_gain < 6500) %>%
  
  inner_join(stage_level_power %>%
               select(activity_id, stage, race, year, class, date, length) %>%
               unique(), by = c("activity_id")) %>%
  
  group_by(stage, race, year, class, date, length, distance) %>%
  summarize(altitude = median(altitude, na.rm = T),
            vertical_gain = median(vertical_gain, na.rm = T)) %>%
  ungroup()
  
#
#
#

segment_impact <- segment_data_races %>%
  
  inner_join(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
               mutate(stage = as.character(stage)) %>%
               select(stage, race, year, class) %>%
               unique(), by = c("stage", "race", 'year', "class")) %>% 
  
  mutate(wattskg = Power/weight) %>%
  
  select(Segment, race, stage, year, class, rider, time, Distance, Gradient,
         rowname, rnk, length, total_seconds, wattskg) %>% 
  unique() %>% 
  
  group_by(Segment, rider, race, stage, year) %>% 
  mutate(ordered = rank(rowname, ties.method = "first")) %>%
  ungroup() %>% 
  
  select(Segment, race, stage, year, class, rider, time, Distance, Gradient,
         OrderInRace = ordered, length, rnk, wattskg) %>%
  unique() %>% 
  
  mutate(t25_time = ifelse(rnk <= 25, time, NA),
         t25_watts = ifelse(rnk <= 25, wattskg, NA)) %>%
  
  group_by(Segment, race, stage, year, class, Distance, Gradient, OrderInRace, length) %>%
  
  summarize(SD = sd(time, na.rm = T), 
            x90 = quantile(time, probs = 0.9, na.rm = T),
            x10 = quantile(time, probs = 0.1, na.rm = T),
            med = median(time, na.rm = T),
            power = mean(wattskg, na.rm = T),
            peak_power = median(t25_watts, na.rm = T),
            with_power = sum(!is.na(wattskg)),
            t25_power = sum(!is.na(t25_watts)),
            riders = n()) %>% 
  
  ungroup() %>%
  
  inner_join(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
               mutate(stage = as.character(stage)) %>%
               rename(Segment = StravaSegment), by = c("Segment", "OrderInRace", "race", "stage", 'year', "class")) %>%

  mutate(perc_thru = AtKM / length) %>%
  
  filter(riders >= 10) %>% 
  
  mutate(perc_90_10 = (x90-x10) / med,
         fastest = (med / x10) - 1)

#
#
#

segment_impact %>% lm(peak_power ~ class + perc_thru + Distance:Gradient + log(Distance):Gradient + Gradient + Distance, data = .) -> MOD

segment_impact %>% mutate(pred = predict(MOD, .), rel = peak_power - pred) -> PRED



#
#
#

downhill <- segment_data_races %>%
  
  inner_join(read_csv("DownhillSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
               mutate(stage = as.character(stage)) %>%
               select(stage, race, year, class) %>%
               unique(), by = c("stage", "race", 'year', "class")) %>% 
  
  select(Segment, race, stage, year, class, rider, time, Distance, Gradient,
         rowname, rnk, length, total_seconds) %>% 
  unique() %>% 
  
  group_by(Segment, rider, race, stage, year) %>% 
  mutate(ordered = rank(rowname, ties.method = "first")) %>%
  ungroup() %>% 
  
  select(Segment, race, stage, year, class, rider, time, Distance, Gradient,
         OrderInRace = ordered, length, rnk) %>%
  unique() %>% 
  
  inner_join(read_csv("DownhillSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
               mutate(stage = as.character(stage)) %>%
               rename(Segment = StravaSegment), by = c("Segment", "OrderInRace", "race", "stage", 'year', "class")) %>%
  
  mutate(perc_thru = AtKM / length) %>%
  
  mutate(kmh_speed = (Distance / (time / 3600))) %>%
  
  group_by(Segment, OrderInRace, stage, race, year, class) %>%
  mutate(rel_time = median(time, na.rm = T) / time) %>%
  ungroup()

dh <- downhill %>% group_by(rider) %>% summarize(x80 = quantile(rel_time, na.rm = T, probs = 0.8), median = median(rel_time, na.rm = T), succ_weight = sum((log(rnk+1) * rel_time), na.rm = T) / sum(log(rnk+1), na.rm = T), descents = n(), KMs = sum(Distance, na.rm = T), finpos = mean(log(rnk), na.rm = T)) %>% ungroup()


#
#
#

segment_impact %>% 
  
  group_by(stage, race, year, class) %>%
  filter(riders > max(riders, na.rm = T) * 0.8) %>% 
  ungroup() %>%
  
  filter(riders >= 10) %>%
  mutate(perc_90_10 = (x90-x10) / med) %>%
  mutate(Gradient = ifelse(Gradient <= -0.06, -0.06, 
                           ifelse(Gradient >= 0.15, 0.15, Gradient))) %>% 
  
  group_by(f = floor(Gradient/0.01)/100) %>% 
  summarize(Distance = mean(Distance, na.rm = T), 
            perc_90_10 = mean(perc_90_10, na.rm = T), 
            n=n()) %>%
  ungroup() %>% 
  
  ggplot(aes(x = f, y = perc_90_10, size = n))+
  geom_vline(xintercept = 0)+
  geom_smooth(size = 2, se = F, color = "black", span = 1)+
  geom_point(shape = 21, color = "black", fill = "orange", stroke = 1)+
  labs(x = "Gradient of Segment", 
       y = "Spread of rider times", 
       size = "N of\nsegments",
       title = "Higher gradients lead to more separation",
       subtitle = "Gradients over 6% average a difference of 20% or more\nbetween 90th percentile and 10th percentile of riders's times")+
  scale_x_continuous(label = scales::percent)+
  scale_y_continuous(label = scales::percent)+
  theme(plot.title = element_text(size = 18, face = "bold"))
  

#
#
#
#
#
#

spec_race <- segment_data_races %>%
  
  #filter(year >= 2020) %>%
  
  select(Segment, race, stage, year, class, date, rider, time, Distance, Gradient,
         rowname, rnk, length, total_seconds, avg_temperature,
         Power, weight, pred_climb_difficulty, gain_1st) %>% 
  
  inner_join(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
               select(stage, race, year, class, Segment = StravaSegment) %>% 
               mutate(stage = as.character(stage)) %>%
               unique(), by = c("stage", "race", 'year', "class", "Segment")) %>% 
  
  unique() %>%

  group_by(Segment, rider, race, stage, year) %>% 
  mutate(ordered = rank(rowname, ties.method = "first")) %>%
  ungroup() %>% 
  
  select(Segment, race, stage, year, class, date, rider, time, Distance, Gradient,
         OrderInRace = ordered, rnk, length, total_seconds, avg_temperature,
         Power, weight, pred_climb_difficulty, gain_1st) %>%
  
  group_by(race, stage, year, class, Segment, OrderInRace) %>%
  mutate(best_rank = min(rnk, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(t25_time = ifelse(rnk <= (best_rank + 10), time, NA),
         #t25_stage = ifelse(rnk <= (best_rank + 10), total_seconds, NA)
         ) %>%
  
  inner_join(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
               mutate(stage = as.character(stage)) %>%
               rename(Segment = StravaSegment), by = c("Segment", "OrderInRace", "race", "stage", 'year', "class")) %>%
  
  group_by(Segment, race, stage, year, class, Distance, Gradient, OrderInRace) %>%
  
  mutate(#SD = sd(time, na.rm = T), 
            #x90 = quantile(time, probs = 0.9, na.rm = T),
            #x10 = quantile(time, probs = 0.1, na.rm = T),
            #med = median(time, na.rm = T),
            t25 = median(t25_time, na.rm = T),
            #stage_time = median(t25_stage, na.rm = T),
            riders = n()) %>% 
  
  ungroup() %>%

  arrange(AtKM) %>% 
  
  mutate(perc_thru = AtKM / length,
         #stage_time_lost = total_seconds - stage_time
         ) %>%
  
  mutate(rel25 = time - t25) %>%
  
  group_by(rider, race, year, stage, class) %>%
  mutate(cumul_t25 = cumsum(rel25)) %>% 
  ungroup() %>%
  
  arrange(desc(AtKM)) %>% 
  
  group_by(rider, race, year, stage, class) %>%
  mutate(segments_time_lost = sum(rel25, na.rm = T)) %>%
  mutate(subsequent_time_lost = cumsum(rel25)) %>% 
  mutate(subsequent_time_lost = lag(subsequent_time_lost)) %>%
  mutate(subsequent_time_lost = ifelse(is.na(subsequent_time_lost), 0, subsequent_time_lost)) %>%
  ungroup() %>%
  
  mutate(time_lost_to_point = segments_time_lost - subsequent_time_lost - rel25,
         time_lost_after_point = segments_time_lost - subsequent_time_lost,
         alive = ifelse((time_lost_to_point + rel25) > 45, 0, 1)) %>%
  
  mutate(rd_AtKM = plyr::round_any(AtKM-Distance, 0.5)) %>%
  
  left_join(strava_before_segments %>%
              select(-altitude), by = c("stage", "race", "year", "class", "date", "length", "rd_AtKM" = "distance")) %>%
  
  mutate(rd_AtKM = plyr::round_any(AtKM, 0.5)) %>%
  
  left_join(strava_before_segments %>%
              select(-vertical_gain), by = c("stage", "race", "year", "class", "date", "length", "rd_AtKM" = "distance"))

#
#
#

segment_impact_alive <- spec_race %>%
  filter(alive == 1) %>% 
  
  group_by(Segment, race, stage, year, class, perc_thru, Distance, Gradient, OrderInRace, length) %>%
  
  summarize(power = mean(Power/weight, na.rm = T),
            with_power = sum(!is.na(Power)),
            riders = n()) %>% 
  
  ungroup()

#
#
#
#
#

library(lme4)

timing_based_wattskg <- spec_race %>%    
  
  mutate(wattskg = (Power / weight),
         speed = (Distance/(time/3600))) %>%
  
  inner_join(spec_race %>%
               mutate(wattskg = (Power / weight),
                      speed = (Distance/(time/3600))) %>%
               
               filter(speed > 5) %>%
               filter(wattskg < 10) %>%
               
               group_by(stage, race, year, Segment, OrderInRace) %>%
               summarize(wattskg_median = median(wattskg, na.rm = T),
                         speed_median = median(speed, na.rm = T)) %>%
               ungroup(), by = c("stage", "race", "year", "Segment", "OrderInRace")) %>% 
  
  mutate(ProjectedWattsKG = (speed / speed_median) * wattskg_median) %>% 
  
  filter(((Distance * 1000) * Gradient) > 250) %>% 
  
  select(rider, year, race, Segment, OrderInRace, stage, ProjectedWattsKG, perc_thru) %>% 
  spread(rider, ProjectedWattsKG) %>% 
  gather(rider, ProjectedWattsKG, -c("race","Segment", "OrderInRace", "stage", "perc_thru", "year")) %>%
  
  group_by(Segment, OrderInRace, stage, race, year) %>% 
  mutate(avg = mean(ProjectedWattsKG, na.rm = T)) %>% 
  ungroup() %>% 
  filter(!is.na(ProjectedWattsKG)) %>% 
  
  mutate(rel = ProjectedWattsKG / (avg)) %>% 
  arrange(desc(rel))

data_for_relmodel <- spec_race %>%

  mutate(wattskg = (Power / weight),
         speed = (Distance/(time/3600))) %>%
  
  filter(speed > 5) %>%
  filter(wattskg < 10 | is.na(wattskg)) %>%
  
  group_by(stage, race, year, Segment, OrderInRace) %>%
  mutate(wattskg_rel = wattskg / median(wattskg, na.rm = T),
         median_wattskg = median(wattskg, na.rm = T),
         speed_rel = speed / median(speed, na.rm = T)) %>%
  ungroup()

rel_model <- lme4::lmer(wattskg_rel ~ speed_rel + 0 + (1 | rider) + (1 | rider:char_date), 
                        
                        data = data_for_relmodel %>%
                          mutate(char_date = as.character(date)))

lme4::ranef(rel_model)[[1]] %>%
  rownames_to_column() %>% 
  separate(rowname, c("rowname", "date"), sep = ":") %>%
  mutate(date = as.Date(date)) %>%
  rename(rider = rowname, int = `(Intercept)`) -> riderdate_power_speed_errors

lme4::ranef(rel_model)[[2]] %>%
  rownames_to_column() -> rider_power_speed_errors

rider_power_speed_errors <- riderdate_power_speed_errors %>%
  
  inner_join(rider_power_speed_errors, by = c("rider" = "rowname")) %>%
  mutate(int = int + `(Intercept)`) %>%
  select(rider, int, date) %>%
  rbind(
    
    rider_power_speed_errors %>%
      rename(rider = rowname, int = `(Intercept)`) %>%
      mutate(date = as.Date(NA))) %>%
  
  mutate(divide_by = 1+int)

#

dbWriteTable(con, "power_speed_errors_riderdate", rider_power_speed_errors, row.names = F, overwrite = TRUE)

#

spec_race_adj <- data_for_relmodel %>%
  
  left_join(lme4::ranef(rel_model)[[1]] %>%
              rownames_to_column() %>% 
              separate(rowname, c("rowname", "date"), sep = ":") %>%
              mutate(date = as.Date(date)) %>%
              rename(rider = rowname, int = `(Intercept)`), by = c("rider", "date")) %>%
  
  mutate(int = ifelse(is.na(int), 0, int)) %>%
  mutate(wattskg_perf = ifelse(is.na(wattskg_rel), speed_rel, wattskg_rel - int),
         wattskg_modeled = median_wattskg * wattskg_rel)

#

SPD = 21

spec_race_adj %>%
  
  filter(stage == 8 & race == "tour de france" & year == 2021) %>%
  #filter(!rider == "Garcia Cortina Ivan") %>%
  filter(perc_thru > 0.85) %>%
  
  ggplot(aes(x = speed, y = median_wattskg * wattskg_perf, label = rider))+
  geom_point()+
  geom_text()+
  stat_smooth(se=F, method = "lm", fullrange = TRUE)+
  facet_wrap(~Segment, scales = 'free')+
  geom_segment(aes(x = SPD, xend = SPD, y = 4.5, yend = 6.25))+
  labs(x = "speed in kph", y = "estimated watts/kg", title = "St6 2019 TDF")

#

final_segments_speed <- data_for_relmodel %>%
  
  group_by(rider, stage, race, year) %>%
  filter(max(perc_thru) == perc_thru) %>%
  ungroup() %>%
  
  group_by(stage, race, year, class, Segment, OrderInRace) %>%
  mutate(q85 = quantile(speed, probs = 0.85, na.rm = T),
         q50 = quantile(speed, probs = 0.50, na.rm = T),
         q15 = quantile(speed, probs = 0.15, na.rm = T),
         n_riders = n()) %>%
  ungroup() %>%
  
  mutate(ref_level = (q85+q50+q15)/3,
         rel_to_ref = speed / ref_level)
  

#
#
#
#
#
#

placing_mod <- lm(adj_wattskg ~ log(rnk) + perc_thru:log(rnk) + class,
                  data = spec_race_adj %>%
                    mutate(adj_wattskg = median_wattskg * wattskg_perf))

placing_mod <- mgcv::gam(adj_wattskg ~ s(rnk, k = 20, bs = "cr") + perc_thru:log(rnk) + class,
                         data = spec_race_adj %>%
                           mutate(adj_wattskg = median_wattskg * wattskg_perf)
                         )

summary(placing_mod)

#

cbind(pred = predict(placing_mod, expand_grid(rnk = seq(1,150,1), 
                                                        perc_thru = seq(0,1,0.2),
                                                        class = c("2.UWT", "2.Pro", "2.1", "2.2", "2.Ncup"))),
      expand_grid(rnk = seq(1,150,1), 
                  perc_thru = seq(0,1,0.2),
                  class = c("2.UWT", "2.Pro", "2.1", "2.2", "2.Ncup"))) %>%
  
  ggplot(aes(x = rnk, y = pred, color = perc_thru, group = perc_thru))+
  geom_hline(yintercept = c(4.5,5))+
  geom_line()+
  scale_x_reverse()+
  facet_wrap(~class)+
  scale_color_viridis_c(breaks = seq(0,1,0.2), labels = scales::percent, name = "% Thru\nRace")

#

data_for_mod <- spec_race %>% 
  
  mutate(wattskg = (Power / weight),
         speed = (Distance/(time/3600))) %>%
  
  filter(speed > 5) %>%
  
  group_by(rider, year, race, class, stage) %>%
  mutate(last = max(perc_thru)) %>%
  mutate(last = ifelse(perc_thru == last, alive, NA)) %>%
  mutate(alive = ifelse(max(last, na.rm = T) == 1, 1, alive)) %>%
  ungroup() %>%
  
  arrange(year, race, class, stage, rider, perc_thru) %>%
  
  group_by(rider, stage, race, year, class) %>%
  mutate(alive_before = lag(alive)) %>%
  ungroup() %>%
  
  mutate(alive_before = ifelse(is.na(alive_before), 1, alive_before)) %>%
  select(-last) %>%
  mutate(SpecSeg = paste0(Segment,race,stage,year))

#

lme4::lmer(speed ~ Gradient * Distance + perc_thru + alive_before + (1 | class) + (1|rider) + avg_temperature, 
           
           data = data_for_mod) -> spd_model

summary(spd_model)

#segment_ranef <- ranef(spd_model)[[1]] %>% rownames_to_column()
rider_ranef <- lme4::ranef(spd_model)[[2]] %>% rownames_to_column()
class_ranef <- lme4::ranef(spd_model)[[1]] %>% rownames_to_column()

#

rbind(
  cbind(predicted_speed = predict(spd_model, 
                                  data_for_mod %>%
                                    select(Gradient, Distance, Segment, race, stage, year, class, SpecSeg, OrderInRace, perc_thru,
                                           avg_temperature) %>%
                                    mutate(alive_before = 1,
                                           rider = "Generic Rider") %>%
                                    unique(), allow.new.levels = TRUE),
        data_for_mod %>%
          select(Gradient, Distance, Segment, race, stage, year, class, SpecSeg, OrderInRace, perc_thru,
                 avg_temperature) %>%
          mutate(alive_before = 1,
                 rider = "Generic Rider") %>%
          unique()),
  cbind(predicted_speed = predict(spd_model, 
                                  data_for_mod %>%
                                    select(Gradient, Distance, Segment, race, stage, year, class, SpecSeg, OrderInRace, perc_thru,
                                           avg_temperature) %>%
                                    mutate(alive_before = 0,
                                           rider = "Generic Rider") %>%
                                    unique(), allow.new.levels = TRUE),
        data_for_mod %>%
          select(Gradient, Distance, Segment, race, stage, year, class, SpecSeg, OrderInRace, perc_thru,
                 avg_temperature) %>%
          mutate(alive_before = 0,
                 rider = "Generic Rider") %>%
          unique())) -> predicted_speed_segments

#
#
#

ind_segment_predictions <- data_for_mod %>%
  
  inner_join(predicted_speed_segments %>%
               select(stage, race, year, class, Segment, OrderInRace, perc_thru, alive_before, predicted_speed),
             by = c("stage", "race", "year", "class", "Segment", "OrderInRace", "perc_thru", "alive_before")) %>%
  
  mutate(SpeedRelToPredict = speed / predicted_speed)

#

ind_segment_predictions %>% 
  filter(perc_thru >= 0.9) %>%
  group_by(rider) %>% 
  summarize(exp = mean(predicted_speed, na.rm = T), 
            act = mean(speed, na.rm = T),
            rel = mean(SpeedRelToPredict, na.rm = T),
            segments = n()) %>% ungroup() -> rider_relative_speed

#

spec_race %>%
  group_by(Segment, OrderInRace, perc_thru, race, stage, year) %>%
  do(broom::tidy(lm(log(rnk+1) ~ time_lost_after_point, data = .))) %>%
  ungroup() -> segment_impact_finpos_models

#

spec_race %>% 
  
  arrange(rider, year, class, race, stage, perc_thru) %>%
  
  group_by(stage, race, year, rider, class) %>% 
  mutate(alive_start = lag(alive)) %>% 
  ungroup() %>%
  
  group_by(race, stage, year, class, Segment, OrderInRace, perc_thru) %>% 
  summarize(still_in_group = mean(alive, na.rm = T), 
            in_group_start = mean(alive_start, na.rm = T),
            riders = n()) %>% 
  ungroup() %>%
  
  arrange(perc_thru) -> survival_probability

# bring in rider level model data

rider_level_pcd_impact <- dbGetQuery(con, "SELECT rider, Date, pcd_impact, random_intercept + (0.5 * bunchsprint_impact) as ability_intercept
                                     FROM lme4_rider_logranks
                                     WHERE test_or_prod = 'prod'") %>%
  mutate(Date = as.Date(Date))

# rider random intercepts and slopes (perc_thru) for survival

spec_race %>%
  
  filter(date < '2021-07-01') %>%
  
  mutate(vam_poly = (Gradient^2)*(Distance*1000),
         perc_thru = ifelse(perc_thru >= 1, 1, perc_thru),
         inv_perc_thru = sqrt(1/perc_thru)) %>% 
  
  arrange(perc_thru) %>%
  
  group_by(stage, race, year, rider) %>%
  mutate(tot_vam_poly = cumsum(vam_poly)-vam_poly) %>%
  ungroup() %>%
  
  inner_join(rider_level_pcd_impact, by = c("rider", "date" = "Date")) -> surv_mod_data

#

glm(alive ~ pcd_impact + ability_intercept + perc_thru + vam_poly + tot_vam_poly + pcd_impact:inv_perc_thru:vam_poly,
    
    data = surv_mod_data,
    family = "binomial") -> surv_mod

#

cbind(surv_mod_data,
      
      coef_surv = predict(surv_mod,
                          surv_mod_data)) %>%
  mutate(pred_surv = exp(coef_surv)/(1+exp(coef_surv))) -> survival_predictions

#

segment_impact %>% 
  
  select(stage, race, year, Segment, OrderInRace, Distance, Gradient, perc_thru) %>% 
  unique() %>%
  mutate(vam_poly = (Gradient^2)*(Distance*1000),
         perc_thru = ifelse(perc_thru >= 1, 1, perc_thru),
         inv_perc_thru = sqrt(1/perc_thru)) %>%  
  
  arrange(perc_thru) %>%
  
  group_by(stage, race, year) %>%
  mutate(tot_vam_poly = cumsum(vam_poly)-vam_poly) %>%
  ungroup() %>%
  
  inner_join(all_stage_data %>%
               filter(date >= '2020-07-01') %>%
               select(rider, team, Date = date, stage, race, year), by = c("stage", "race", "year")) %>% 
  
  inner_join(rider_level_pcd_impact, by = c("rider", "Date")) -> any_stage

cbind(any_stage,
      
      coef_surv = predict(surv_mod,
                          any_stage)) %>%
  mutate(pred_surv = exp(coef_surv)/(1+exp(coef_surv))) %>%
  select(-inv_perc_thru, -coef_surv) -> survival_predictions_any





lme4::glmer(alive ~ (1 + perc_thru | rider) + (0 + vam_poly | rider) + vam_poly * perc_thru, 
            data = ., 
            family = "binomial") -> surv_mod

summary(surv_mod)

#

lme4::ranef(surv_mod)[[1]] %>% rownames_to_column() -> riders_ranefs

#

cbind(
  expand_grid(spec_race %>% 
                group_by(rider) %>% 
                summarize(n = n_distinct(race,stage,year,class)) %>% 
                ungroup() %>% 
                filter(n > max(n, na.rm = T)/4), 
              vam_poly = c(16,34,65),
              perc_thru = c(0.5,0.9)),
  pred = predict(surv_mod, 
                 expand_grid(spec_race %>% 
                               group_by(rider) %>% 
                               summarize(n = n_distinct(race,stage,year,class)) %>% 
                               ungroup() %>% 
                               filter(n > max(n, na.rm = T)/4), 
                             vam_poly = c(16,34,65),
                             perc_thru = c(0.5,0.9)))) %>% 
  mutate(prob = exp(pred)/(1+exp(pred))) %>% 
  select(rider, perc_thru, vam_poly, prob) %>%
  spread(perc_thru, prob) %>% 
  janitor::clean_names() %>%
  mutate(diff = x0_9 - x0_5) -> rider_spec_survival

#

rider_empirical_survival <- spec_race %>% 
  
  mutate(perc_thru_bins = ifelse(perc_thru < 0.5, 'first half',
                                 ifelse(perc_thru < 0.75, "3rd quarter",
                                        ifelse(perc_thru < 0.9, "heating up",
                                               ifelse(perc_thru > 0.8999, "final stretch", NA))))) %>%
  
  group_by(rider, perc_thru_bins) %>% 
  summarize(still_in_group = mean(alive, na.rm = T), 
            perc_thru = mean(perc_thru, na.rm = T),
            segments = n()) %>% 
  ungroup()

#

segment_data_races %>%
  
  inner_join(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
               select(stage, race, year, class) %>%
               unique(), by = c("stage", "race", 'year', "class")) %>% 
  
  mutate(wattskg = Power/weight) %>%
  
  select(Segment, race, stage, year, class, rider, time, Distance, Gradient,
         rowname, rnk, perc_break, length, total_seconds, wattskg) %>% 
  unique() %>% 
  
  group_by(Segment, rider, race, stage, year) %>% 
  mutate(ordered = rank(rowname, ties.method = "first")) %>%
  ungroup() %>% 
  
  select(Segment, race, stage, year, class, rider, time, Distance, Gradient,
         OrderInRace = ordered, length, rnk, wattskg) %>%
  unique() %>% 
  
  mutate(t25_time = ifelse(rnk <= 25, time, NA)) %>%
  
  group_by(Segment, race, stage, year, class, Distance, Gradient, OrderInRace, length) %>%
  
  summarize(SD = sd(time, na.rm = T), 
         x90 = quantile(time, probs = 0.9, na.rm = T),
         x10 = quantile(time, probs = 0.1, na.rm = T),
         med = median(time, na.rm = T),
         t25 = median(t25_time, na.rm = T),
         power = mean(wattskg, na.rm = T),
         riders = n()) %>% 
  
  ungroup() %>%
  
  inner_join(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
               rename(Segment = StravaSegment), by = c("Segment", "OrderInRace", "race", "stage", 'year', "class")) %>%
  
  arrange(AtKM) %>% 
  
  mutate(perc_thru = AtKM / length) %>%

  filter(riders >= 10) %>% 
  
  mutate(perc_90_10 = (x90-x10) / med) -> segs

#
#
#

modeled_power <- segment_data_races %>%
  
  inner_join(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = "latin1")) %>%
               select(stage, race, year, class) %>%
               unique(), by = c("stage", "race", 'year', "class")) %>% 
  
  mutate(wattskg = Power/weight) %>%
  
  select(Segment, race, stage, year, class, rider, time, Distance, Gradient,
         rowname, rnk, length, total_seconds, wattskg) %>% 
  unique() %>% 
  
  group_by(Segment, rider, race, stage, year) %>% 
  mutate(ordered = rank(rowname, ties.method = "first")) %>%
  ungroup() %>% 
  
  select(Segment, race, stage, year, class, rider, time, Distance, Gradient,
         OrderInRace = ordered, length, rnk, wattskg) %>%
  unique() %>% 
  
  inner_join(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = "latin1")) %>%
               rename(Segment = StravaSegment), by = c("Segment", "OrderInRace", "race", "stage", 'year', "class")) %>%
  
  arrange(AtKM) %>% 
  
  mutate(perc_thru = AtKM / length) %>%
  
  filter(wattskg < (quantile(wattskg, na.rm = T, probs = 0.995)) & 
           wattskg > (quantile(wattskg, na.rm = T, probs = 0.005))) %>%
  
  mutate(time_if_power = ifelse(is.na(wattskg), NA, time)) %>%
  
  group_by(Segment, race, stage, year, class, OrderInRace) %>% 
  filter(is.na(wattskg) | (time < (2.5 * mean(time, na.rm = T)))) %>%
  mutate(rel_time = mean(time_if_power, na.rm = T) / time_if_power,
         rel_power = wattskg / mean(wattskg, na.rm = T)) %>%
  ungroup()

#

lme4::lmer(rel_power ~ rel_time + (1 | rider), data = modeled_power) -> lmer_power_vs_time_riders

lme4::lmer(wattskg ~ (1 | rider), data = modeled_power) -> lmer_power_riders

lme4::fixef(lmer_power_riders)[[1]] -> avg_power

lme4::ranef(lmer_power_riders)[[1]] %>%
  rownames_to_column() %>% 
  
  inner_join(modeled_power %>% 
               group_by(rider) %>% 
               summarize(power_races = n_distinct(race, stage, year, class), power_rnks = mean(log(rnk), na.rm = T)+1) %>%
               ungroup(), by = c("rowname" = "rider")) %>% 
  
  inner_join(all_stage_data %>% 
               filter(year >= 2018) %>%
               group_by(rider) %>% 
               summarize(races = n(), all_rnks = mean(log(rnk), na.rm = T)+1) %>%
               ungroup(), by = c("rowname" = "rider")) %>% 
  
  janitor::clean_names() %>% 
  
  mutate(wattskg = intercept + avg_power) %>% 
  
  inner_join(lme4::ranef(lmer_power_vs_time_riders)[[1]] %>%
               rownames_to_column(), by = c("rowname")) %>% 
  janitor::clean_names() %>% 
  
  mutate(wattskg = wattskg / (1+intercept_2)) %>%
  
  mutate(perc_w_power = power_races / races,
         ratio_power = power_rnks / all_rnks) -> rider_power_ability

#

modeled_power %>%
  group_by(rider) %>%
  filter(percent_rank(wattskg) > 0.8) %>%
  summarize(wattskg = mean(wattskg, na.rm = T)) %>%
  ungroup() %>%
  
  inner_join(modeled_power %>% 
               group_by(rider) %>% 
               summarize(power_races = n_distinct(race, stage, year, class), power_rnks = mean(log(rnk), na.rm = T)+1) %>%
               ungroup(), by = c("rider" = "rider")) %>% 
  
  inner_join(all_stage_data %>% 
               filter(year >= 2018) %>%
               group_by(rider) %>% 
               summarize(races = n(), all_rnks = mean(log(rnk), na.rm = T)+1) %>%
               ungroup(), by = c("rider" = "rider")) %>% 
  
  janitor::clean_names() %>% 
  
  inner_join(lme4::ranef(lmer_power_vs_time_riders)[[1]] %>%
               rownames_to_column(), by = c("rider" = "rowname")) %>%
  
  janitor::clean_names() %>% 
  
  mutate(wattskg = wattskg / (1+intercept)) %>%
  
  mutate(perc_w_power = power_races / races,
         ratio_power = power_rnks / all_rnks) -> rider_peak_ability

#

ggplot(rider_power_ability %>% filter(power_races >= 25), aes(x = wattskg))+
  geom_histogram(color = "white")+
  geom_vline(xintercept = 4.9, size = 2, color = "gold")+
  labs(x = "Modeled watts/kg ability on generic climb", 
       y = "", 
       title = "Distribution of power ability")

#

ggplot(rider_peak_ability %>% filter(power_races >= 25),
       aes(x = wattskg, y = exp(power_rnks-1), label = rider))+
  
  geom_text()+
  
  labs(x = "Modeled watts/kg ability on generic climb",
       y = "Average finish position")+
  
  scale_y_continuous(breaks = seq(0,150,10))+
  
  theme(axis.text = element_text(size=15))

#
#
# Final Climbs
#
#

final_climbs_data <- cbind(
  
  spec_race %>%
    group_by(stage, race, year) %>%
    filter(perc_thru > 0.97
           # | rank(-AtKM, ties.method = "min") == 1
           ) %>%
    ungroup() %>%
    mutate(wkg = (Power/weight)) %>% 
    filter(wkg > 2.5) %>% 
    unique(), 
  
  clpcd = mgcv::predict.gam(read_rds('model-climb-difficulty.rds'), 
                  spec_race %>% 
                    group_by(stage, race, year) %>%
                    filter(perc_thru > 0.97
                           # | rank(-AtKM, ties.method = "min") == 1
                    ) %>%
                    ungroup() %>%
                    mutate(wkg = (Power/weight)) %>%
                    filter(wkg > 2.5) %>%
                    unique() %>% 
                    mutate(alt = 0, 
                           vam_poly = (Distance * (Gradient^2))))) %>%
  mutate(prior_pcd = pred_climb_difficulty - clpcd,
         prior_pcd = ifelse(prior_pcd <= 0, 0, prior_pcd))

#

final_climbs_data %>%
  filter((Distance * 1000 * Gradient) > 100) %>%
  mutate(kmh = Distance / (time/3600), 
         final_group = ifelse(gain_1st == 0, 1, 0),
         SpecSeg = paste0(Segment,race,year,stage)) %>% 
  filter(!is.na(gain_1st)) %>%
  
  lme4::lmer(kmh ~ log(rnk) + Gradient * Distance + alive + (1 | SpecSeg), data = .) -> fc_model

lme4::ranef(fc_model)[[1]] %>% rownames_to_column -> ranefs

preds <- cbind(pred = predict(fc_model,
                              final_climbs_data %>% 
                                filter((Distance * 1000 * Gradient) > 100) %>% 
                                filter(!is.na(gain_1st)) %>%
                                mutate(kmh = Distance / (time/3600), 
                                       final_group = ifelse(gain_1st == 0, 1, 0),
                                       SpecSeg = paste0(Segment,race,year,stage))), 
               final_climbs_data %>%
                 filter((Distance * 1000 * Gradient) > 100) %>% 
                 filter(!is.na(gain_1st)) %>%
                 mutate(kmh = Distance / (time/3600), 
                        final_group = ifelse(gain_1st == 0, 1, 0),
                        SpecSeg = paste0(Segment,race,year,stage)))

ggplot(preds, aes(x = rnk, y = kmh - pred))+geom_smooth(se=F)+geom_smooth(se=F, method="lm", color = "red")

mean(((preds$pred)-(preds$kmh))^2)

preds %>% group_by(f = floor(rnk/10)*10) %>% summarize(RMSE = sqrt(mean(((pred)-(kmh))^2)))

#
#
#

spec_race %>% 
  mutate(t25 = ifelse(rnk <= 25, 1, 0)) %>% 
  
  glm(t25 ~ perc_thru:time_lost_to_point + time_lost_to_point, data = ., family = "binomial")
  
#

segment_data_races %>% 
  filter(stage == 15 & year == 2019 & race == "tour de france" &
           (str_detect(Segment, "èmekilomètre") & str_detect(Segment, "Pratd'Albis")) | 
           (Segment == "Pratd'Albis-1erkilomètre")) %>% 
  
  group_by(Segment) %>% 
  mutate(rel_speed = Speed / mean(Speed, na.rm = T), n = n()) %>%
  ungroup() -> prat_d_albis

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

race_segments <- segment_data_races %>%
  
  filter(between(date, as.Date("2020-06-30"), as.Date("2020-11-20"))) %>%
  
  select(Segment, race, stage, year, class, date, rider, time, Distance, Gradient,
         rowname, rnk, perc_break, length, total_seconds,
         avg_power, act_distance = distance,
         Power, weight, pred_climb_difficulty, gain_1st) %>% 
  unique() %>% 
  
  inner_join(    
    rbind(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')),
          read_csv("DownhillSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1'))) %>%
      select(stage, race, year, class) %>% 
      unique(), by = c("stage", "race", 'year', "class")) %>% 

  group_by(Segment, rider, race, stage, year) %>% 
  mutate(ordered = rank(rowname, ties.method = "first")) %>%
  ungroup() %>% 
  
  select(Segment, race, stage, year, class, date, rider, time, Distance, Gradient,
         OrderInRace = ordered, rnk, length, total_seconds,
         avg_power, act_distance,
         Power, weight, pred_climb_difficulty, gain_1st) %>%
  unique() %>% 
  
  inner_join(    
    rbind(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')),
          read_csv("DownhillSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1'))) %>%
      rename(Segment = StravaSegment), by = c("Segment", "OrderInRace", "race", "stage", 'year', "class")) %>%
  
  mutate(perc_thru = AtKM / length)

#

library(lme4)

data_for_relmodel <- race_segments %>%
  
  mutate(wattskg = (Power / weight),
         speed = (Distance/(time/3600))) %>%
  
  filter(speed > 5) %>%
  filter(wattskg < 10) %>%
  
  group_by(stage, race, year, Segment, OrderInRace) %>%
  mutate(wattskg_rel = wattskg / median(wattskg, na.rm = T),
         speed_rel = speed / median(speed, na.rm = T)) %>%
  ungroup()

rel_model <- lme4::lmer(wattskg_rel ~ speed_rel + 0 + (1 | rider), data = data_for_relmodel)

ranef(rel_model)[[1]] %>% rownames_to_column() %>% rename(rider = rowname, int = `(Intercept)`) -> rider_power_speed_errors

#

kamna <- race_segments %>%
  
  inner_join(rider_power_speed_errors, by = c("rider")) %>%
  
  mutate(Power = Power / (1+int),
         avg_power = avg_power / (1+int)) %>%
  
  filter(!is.na(avg_power)) %>%
  filter(!is.na(Power)) %>%
  filter(time < 7200) %>%
  #filter(race == "tour de france" & year == 2020) %>%
  
  arrange(perc_thru) %>%
  
  mutate(startKM = AtKM - Distance,
         KJ = Power * time / 1000 / weight) %>%
  
  group_by(stage, race, year, class, rider) %>%
  mutate(SegDist = sum(Distance, na.rm = T),
         SegTime = sum(time, na.rm = T),
         SegKJ = sum(KJ, na.rm = T),
         ToPointKM = cumsum(Distance) - Distance,
         ToPointKJ = cumsum(KJ) - KJ) %>%
  ungroup() %>%
  
  mutate(act_seconds = (total_seconds + ((act_distance - length)/24*3600)),
         TotalKJ = avg_power * act_seconds / 1000 / weight,
         leftoverDist = act_distance - SegDist,
         leftoverTime = act_seconds - SegTime,
         leftoverKJ = TotalKJ - SegKJ) %>%
  
  mutate(residualKJ_per_KM = leftoverKJ / leftoverDist,
         NonSegKJ = residualKJ_per_KM * (startKM - ToPointKM),
         CombinedKJ = ToPointKJ + NonSegKJ) %>%
  
  group_by(Segment, OrderInRace, stage, race, year, class) %>% 
  filter(n() >= 10) %>%
  mutate(rel_KJ = CombinedKJ / mean(CombinedKJ, na.rm = T)) %>% 
  ungroup() %>%
  
  inner_join(spec_race %>% select(Segment, stage, race, year, OrderInRace, class, rider, alive))

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

stage_level_power %>% 
  filter(time_trial == 1 & race == "tour de france" & distance > 30 & year == 2021) %>% 
  
  mutate(wattskg = weighted_avg_power / weight,
         avgwattskg = avg_power/weight) %>% 
  
  select(rnk, rider = pcs, wattskg, avgwattskg, stage, year) %>% 
  arrange(rnk) %>%
  
  ggplot(aes(x = log(rnk), y = wattskg))+
  
  geom_point()+
  
  scale_x_reverse()+
  
  labs(x = "LN(stage finish position)", 
       y = "Weighted Avg Power/KG")
  
  geom_point(data = tibble(rider = "Van Der Poel Mathieu", wattskg = 6.03, rnk = 5), 
             fill = "gold", size=3, shape = 21)
  
  geom_line(data = cbind(wattskg = predict(tt_wattskg_mod, tibble(logrnk = seq(0,5.5,0.25))), 
                         tibble(logrnk = seq(0,5.5,0.25), rider = "X")), 
            
            aes(x = logrnk, y = wattskg), 
            
            color = "red", size=2)
  
  ggrepel::geom_text_repel(data = stage_level_power %>% 
                             filter(time_trial == 1 & race == "tour de france" & distance < 28.5) %>% 
                             mutate(wattskg = weighted_avg_power / weight), 
                           aes(x = log(rnk), y = wattskg, label = pcs))+
  
  geom_text(data = tibble(rider = "Van Der Poel Mathieu", wattskg = 5.93, rnk = 5), 
            aes(label = rider))+
  
  geom_segment(data = tibble(wattskgTOP = 6.85, wattskgBOT = 6.45, logrnk = 0), 
               aes(x = logrnk, xend = logrnk, y = wattskgTOP, yend = wattskgBOT), 
               size = 2, color = "black")

