

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

#
#
#

stage_level_power <- dbGetQuery(con, "SELECT activity_id, PCS, VALUE, Stat, DATE 
                  FROM strava_activity_data 
                  WHERE Stat IN ('Weighted Avg Power', 'Distance', 'AvgSpeed')") %>% 
  
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

  inner_join(all_stage_data %>%
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
  
  group_by(stage, race, year, rider = pcs, date) %>%
  filter(rank(-avg_speed, ties.method = "min") == 1) %>%
  ungroup() %>%
  
  unique()

#
#
#

segment_data_races <- stage_level_power %>%
  
  filter(year %in% c(2019, 2020, 2021)) %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT rowname, Segment,
               Distance, Gradient, Speed,
               Power, Type,
               activity_id
               FROM strava_segment_data WHERE Gradient < -0.029"), by = c("activity_id")
    
  ) %>%
  
  group_by(rider = pcs) %>% 
  mutate(rel_power = Power / mean(Power, na.rm = T)) %>% 
  ungroup() %>%
  
  select(-gain_3rd, -gain_5th, -gc_pos, -gain_gc, -missing_profile_data, 
         -uphill_finish, -sof_limit, -success, -points_finish, -cobbles, -Tour,
         -gc_winner, -time_trial, -stage_name) %>%
  
  mutate(rowname = as.numeric(rowname)) %>% 
  
  mutate(time = Distance / Speed * 3600)

#

segment_data_races %>%
  
  anti_join(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
              select(stage, race, year, class) %>%
              unique(), by = c("stage", "race", 'year', "class")) %>%

  group_by(race, stage, year, date, class, pred_climb_difficulty, length) %>% summarize(n = n_distinct(rider)) %>% ungroup() %>% 
  arrange(desc(n)) -> r

#

abc <- segment_data_races %>%
  
  anti_join(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
              select(stage, race, year, class) %>%
              unique(), by = c("stage", "race", 'year', "class")) %>%
  
  #filter(year == 2021) %>%
  filter(rnk != 200) %>%
  select(activity_id, pcs, rnk, stage, race, year, class, rowname, Segment, Distance, Gradient, Type, distance, length)

#
#
#

segment_impact <- segment_data_races %>%
  
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
  
  mutate(t25_time = ifelse(rnk <= 25, time, NA),
         t25_watts = ifelse(rnk <= 25, wattskg, NA)) %>%
  
  group_by(Segment, race, stage, year, class, Distance, Gradient, OrderInRace, length) %>%
  
  summarize(SD = sd(time, na.rm = T), 
            x90 = quantile(time, probs = 0.9, na.rm = T),
            x10 = quantile(time, probs = 0.1, na.rm = T),
            med = median(time, na.rm = T),
            power = mean(wattskg, na.rm = T),
            peak_power = mean(t25_watts, na.rm = T),
            with_power = sum(!is.na(wattskg)),
            t25_power = sum(!is.na(t25_watts)),
            riders = n()) %>% 
  
  ungroup() %>%
  
  inner_join(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
               rename(Segment = StravaSegment), by = c("Segment", "OrderInRace", "race", "stage", 'year', "class")) %>%

  mutate(perc_thru = AtKM / length) %>%
  
  filter(riders >= 10) %>% 
  
  mutate(perc_90_10 = (x90-x10) / med,
         fastest = (med / x10) - 1)

#
#
#

downhill <- segment_data_races %>%
  
  inner_join(read_csv("DownhillSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
               select(stage, race, year, class) %>%
               unique(), by = c("stage", "race", 'year', "class")) %>% 
  
  select(Segment, race, stage, year, class, rider, time, Distance, Gradient,
         rowname, rnk, perc_break, length, total_seconds) %>% 
  unique() %>% 
  
  group_by(Segment, rider, race, stage, year) %>% 
  mutate(ordered = rank(rowname, ties.method = "first")) %>%
  ungroup() %>% 
  
  select(Segment, race, stage, year, class, rider, time, Distance, Gradient,
         OrderInRace = ordered, length, rnk) %>%
  unique() %>% 
  
  inner_join(read_csv("DownhillSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
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
  
  inner_join(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
               select(stage, race, year, class) %>% 
               unique(), by = c("stage", "race", 'year', "class")) %>% 
  
  select(Segment, race, stage, year, class, rider, time, Distance, Gradient,
         rowname, rnk, perc_break, length, total_seconds,
         Power, weight, pred_climb_difficulty, gain_1st) %>% 
  unique() %>% 
  
  group_by(Segment, rider, race, stage, year) %>% 
  mutate(ordered = rank(rowname, ties.method = "first")) %>%
  ungroup() %>% 
  
  select(Segment, race, stage, year, class, rider, time, Distance, Gradient,
         OrderInRace = ordered, rnk, length, total_seconds,
         Power, weight, pred_climb_difficulty, gain_1st) %>%
  unique() %>% 
  
  group_by(race, stage, year, class) %>%
  mutate(best_rank = min(rnk, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(t25_time = ifelse(rnk <= (best_rank + 10), time, NA),
         t25_stage = ifelse(rnk <= (best_rank + 10), total_seconds, NA)) %>%
  
  inner_join(read_csv("ExpandedStravaSegments.csv", locale = readr::locale(encoding = 'ISO-8859-1')) %>%
               rename(Segment = StravaSegment), by = c("Segment", "OrderInRace", "race", "stage", 'year', "class")) %>%
  
  group_by(Segment, race, stage, year, class, Distance, Gradient, OrderInRace) %>%
  
  mutate(SD = sd(time, na.rm = T), 
            x90 = quantile(time, probs = 0.9, na.rm = T),
            x10 = quantile(time, probs = 0.1, na.rm = T),
            med = median(time, na.rm = T),
            t25 = median(t25_time, na.rm = T),
            stage_time = median(t25_stage, na.rm = T),
            riders = n()) %>% 
  
  ungroup() %>%

  arrange(AtKM) %>% 
  
  mutate(perc_thru = AtKM / length,
         stage_time_lost = total_seconds - stage_time) %>%
  
  mutate(rel25 = time - t25) %>%
  
  group_by(rider, race, year, stage) %>%
  mutate(cumul_t25 = cumsum(rel25)) %>% 
  ungroup() %>%
  
  arrange(desc(AtKM)) %>% 
  
  group_by(rider, race, year, stage) %>%
  mutate(segments_time_lost = sum(rel25, na.rm = T)) %>%
  mutate(subsequent_time_lost = cumsum(rel25)) %>% 
  mutate(subsequent_time_lost = lag(subsequent_time_lost)) %>%
  mutate(subsequent_time_lost = ifelse(is.na(subsequent_time_lost), 0, subsequent_time_lost)) %>%
  ungroup() %>%
  
  mutate(time_lost_to_point = segments_time_lost - subsequent_time_lost - rel25,
         time_lost_after_point = segments_time_lost - subsequent_time_lost,
         alive = ifelse((time_lost_to_point + rel25) > 45, 0, 1))

#

spec_race %>% 
  arrange(rider, year, class, race, stage, perc_thru) %>%
  
  group_by(stage, race, year, rider, class) %>% 
  mutate(alive_start = lag(alive)) %>% 
  filter(perc_thru == max(perc_thru, na.rm = T)) %>%
  ungroup() %>% 
  
  filter(perc_thru >= 0.9) %>% 
  
  group_by(rider) %>% 
  summarize(alive = mean(alive, na.rm = T), 
            alive_start = mean(alive_start, na.rm = T),
            time_on_climb = sum(time, na.rm = T) / sum(t25, na.rm = T), 
            power = mean(Power/weight, na.rm = T),
            n = n()) %>% 
  ungroup() -> final_segments

#

library(lme4)

data_for_relmodel <- spec_race %>%

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

spec_race_adj <- data_for_relmodel %>%
  
  left_join(ranef(rel_model)[[1]] %>% rownames_to_column() %>% rename(rider = rowname, int = `(Intercept)`), by = c("rider")) %>%
  
  mutate(int = ifelse(is.na(int), 0, int)) %>%
  mutate(wattskg_perf = ifelse(is.na(wattskg_rel), speed_rel, wattskg_rel - int))

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

lme4::lmer(speed ~ Gradient * Distance + perc_thru + alive_before + (1 | SpecSeg) + (1 | rider), 
           
           data = data_for_mod) -> spd_model

summary(spd_model)

segment_ranef <- ranef(spd_model)[[1]] %>% rownames_to_column()
rider_ranef <- ranef(spd_model)[[2]] %>% rownames_to_column()
class_ranef <- ranef(spd_model)[[3]] %>% rownames_to_column()

#

rbind(
  cbind(predicted_speed = predict(spd_model, 
                                  data_for_mod %>%
                                    select(Gradient, Distance, Segment, race, stage, year, class, SpecSeg, OrderInRace, perc_thru) %>%
                                    mutate(alive_before = 1,
                                           rider = "Generic Rider") %>%
                                    unique(), allow.new.levels = TRUE),
        data_for_mod %>%
          select(Gradient, Distance, Segment, race, stage, year, class, SpecSeg, OrderInRace, perc_thru) %>%
          mutate(alive_before = 1,
                 rider = "Generic Rider") %>%
          unique()),
  cbind(predicted_speed = predict(spd_model, 
                                  data_for_mod %>%
                                    select(Gradient, Distance, Segment, race, stage, year, class, SpecSeg, OrderInRace, perc_thru) %>%
                                    mutate(alive_before = 0,
                                           rider = "Generic Rider") %>%
                                    unique(), allow.new.levels = TRUE),
        data_for_mod %>%
          select(Gradient, Distance, Segment, race, stage, year, class, SpecSeg, OrderInRace, perc_thru) %>%
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

# rider random intercepts and slopes (perc_thru) for survival

spec_race %>% 
  
  mutate(vam_poly = (Gradient^2)*(Distance*1000)) %>%
  
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
         rowname, rnk, perc_break, length, total_seconds, wattskg) %>% 
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