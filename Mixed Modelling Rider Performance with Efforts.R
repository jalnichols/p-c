library(tidyverse)
library(DBI)

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

# pull in dates

All_dates <- dbGetQuery(con, "SELECT DISTINCT date, year FROM stage_data_perf WHERE year > 2016") %>%
  filter(year > 2016 & year <= 2023) %>%
  filter(date >= '2018-01-01') %>%
  select(date) %>%
  unique() %>%
  filter(!is.na(date)) %>%
  
  mutate(date = as.Date(date, origin = '1970-01-01')) %>%
  arrange(desc(date)) %>%
  
  rbind(tibble(date = seq(as.Date('2018-01-01'), as.Date('2023-12-31'), 1)) %>%
          filter(lubridate::month(date) %in% seq(2,10,1))) %>%
  
  unique() %>%
  
  anti_join(dbGetQuery(con, "SELECT DISTINCT date FROM lme4_rider_logranks_new") %>%
              mutate(date = as.Date(date, origin = '1970-01-01')-1), by = c("date")) %>%
  
  filter(date <= lubridate::today()) %>%
  filter(date <= dbGetQuery(con, "SELECT MAX(date) as max_date FROM stage_data_perf")[1,1]) %>%
  arrange(desc(date))

#

All_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE year > 2016") %>%
  mutate(rider = case_when(rider == "O Connor Ben" ~ "O'connor Ben",
                           rider == "O Brien Kelland" ~ "O'brien Kelland",
                           rider == "Ghirmay Hailu Biniam" ~ "Girmay Biniam",
                           rider == "Girmay Hailu Biniam" ~ "Girmay Biniam",
                           rider == "O Donnell Bailey" ~ "O'donnell Bailey",
                           rider == "D Heygere Gil" ~ "D'heygere Gil",
                           rider == "Skjelmose Jensen Mattias" ~ "Skjelmose Mattias",
                           rider == "Wright Alfred" ~ "Wright Fred",
                           TRUE ~ rider)) %>%
  mutate(rider = str_trim(rider),
         rider = str_to_title(rider)) %>%
  filter(!str_detect(url, "-itt")) %>%
  filter(time_trial == 0 & team_time_trial == 0) %>%
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  mutate(uphill_finish = ifelse(is.na(uphill_finish), mean(uphill_finish, na.rm = T), uphill_finish)) %>%
  
  mutate(team_ldr = ifelse(tm_pos == 1, 1, 0),
         team_ldrRESTR = ifelse(tm_pos == 1 & rnk <= 10, 1, 0)) %>%
  
  mutate(date = as.Date(date)) %>%
  
  select(-speed, -gain_3rd, -gain_5th, -gain_10th, -gain_40th,
         -time_trial, -gc_winner, -gc_pos, -parcours_value, -stage_type,
         -avg_alt, -missing_profile_data) %>%
  
  unique() %>% 
  
  mutate(final_group = ifelse(bunch_sprint == 1, ifelse(gain_1st <= 5, 1, 0), ifelse(rnk <= 20 | gain_20th == 0, 1, 0))) %>%
  
  select(-gain_1st, -gain_20th) %>%
  
  mutate(points_finish = (1 / (rnk + 1)) * (sof_limit / 5),
         points_finish = ifelse(rnk <= (sof_limit * 5), points_finish, 0)) %>%
  
  mutate(one_day_race = ifelse(bunch_sprint == 1, 0, one_day_race),
         one_day_race = ifelse(pred_climb_difficulty <= 3, one_day_race,
                               ifelse(pred_climb_difficulty >= 8, 0, (8 - pred_climb_difficulty) / 5 * one_day_race)),
         hilly = ifelse(bunch_sprint == 1, 0,
                        ifelse(pred_climb_difficulty >= 8 | uphill_finish == 1, 0, 1))) %>%
  
  inner_join(
    dbReadTable(con, "win_efforts_by_race") %>%
      filter(rolling_speed == 300) %>%
      select(stage, url, year, class, date, relative, absolute, sdrel, sdabs, percentile_abs, percentile_rel) %>%
      rename(rel_300 = relative, abs_300 = absolute, perc_rel_300 = percentile_rel, perc_abs_300 = percentile_abs,
             sd_rel_300 = sdrel, sd_abs_300 = sdabs), by = c("stage", "url", "year", "class", "date")) %>%
  
  inner_join(
    dbReadTable(con, "win_efforts_by_race") %>%
      filter(rolling_speed == 15) %>%
      select(stage, url, year, class, date, relative, absolute, sdrel, sdabs, percentile_abs, percentile_rel) %>%
      rename(rel_15 = relative, abs_15 = absolute, perc_rel_15 = percentile_rel, perc_abs_15 = percentile_abs,
             sd_rel_15 = sdrel, sd_abs_15 = sdabs), by = c("stage", "url", "year", "class", "date")) %>%
  
  inner_join(
    dbReadTable(con, "win_efforts_by_race") %>%
      filter(rolling_speed == 600) %>%
      select(stage, url, year, class, date, relative, absolute, sdrel, sdabs) %>%
      rename(rel_600 = relative, abs_600 = absolute,
             sd_rel_600 = sdrel, sd_abs_600 = sdabs), by = c("stage", "url", "year", "class", "date")) %>%
  
  inner_join(
    dbReadTable(con, "win_efforts_by_race") %>%
      filter(rolling_speed == 120) %>%
      select(stage, url, year, class, date, relative, absolute, sdrel, sdabs) %>%
      rename(rel_120 = relative, abs_120 = absolute,
             sd_rel_120 = sdrel, sd_abs_120 = sdabs), by = c("stage", "url", "year", "class", "date")) %>%
  
  inner_join(
    dbReadTable(con, "win_efforts_by_race") %>%
      filter(rolling_speed == 2400) %>%
      select(stage, url, year, class, date, relative, absolute, sdrel, sdabs, percentile_abs, percentile_rel) %>%
      rename(rel_2400 = relative, abs_2400 = absolute, perc_rel_2400 = percentile_rel, perc_abs_2400 = percentile_abs,
             sd_rel_2400 = sdrel, sd_abs_2400 = sdabs), by = c("stage", "url", "year", "class", "date")) %>%
  
  inner_join(
    dbReadTable(con, "win_efforts_by_race") %>%
      filter(rolling_speed == 1200) %>%
      select(stage, url, year, class, date, relative, absolute, sdrel, sdabs) %>%
      rename(rel_1200 = relative, abs_1200 = absolute,
             sd_rel_1200 = sdrel, sd_abs_1200 = sdabs), by = c("stage", "url", "year", "class", "date")) %>%
  
  mutate(pcs = str_to_title(rider)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)) %>%
      filter(!is.na(weight)) %>%
      group_by(rider) %>%
      summarize(weight = median(weight)) %>%
      ungroup(), by = c("pcs" = "rider")) %>%
  
  mutate(weight = ifelse(is.na(weight), median(weight, na.rm = T), weight)) %>%
  
  group_by(stage, race, year, class, length, date) %>%
  mutate(corr_weight = cor(x = weight, y = rnk, use = "everything", method = "pearson")) %>%
  ungroup() %>%
  
  mutate(abs_or_rel_5min = sd_abs_300 - sd_rel_300,
         abs_or_rel_5min = (abs_or_rel_5min - mean(abs_or_rel_5min, na.rm = T)) / sd(abs_or_rel_5min, na.rm = T)) %>%
  
  mutate(rel_120 = rel_120 / 8.2,
         rel_300 = rel_300 / 7.1,
         rel_600 = rel_600 / 6.45,
         rel_1200 = rel_1200 / 5.95,
         rel_2400 = rel_2400 / 5.5) %>%
  
  mutate(weighted = ((2 * ((rel_120^10)))+(5 * ((rel_300^10)))+(10 * ((rel_600^10)))+(20 * ((rel_1200^10)))+(40 * ((rel_2400^10)))) / 
           ((rel_120^10)+(rel_300^10)+(rel_600^10)+(rel_1200^10)+(rel_2400^10))) %>%
  
  rowwise() %>%
  
  mutate(largest_column = max(rel_120, rel_300, rel_600, rel_1200, rel_2400)) %>%
  ungroup() %>%
  mutate(main_effort = case_when(largest_column == rel_120 ~ 2,
                                 largest_column == rel_300 ~ 5,
                                 largest_column == rel_600 ~ 10,
                                 largest_column == rel_1200 ~ 20,
                                 largest_column == rel_2400 ~ 40,
                                 TRUE ~ 0),
         main_effort = log(main_effort),
         main_effort = ifelse(bunch_sprint == 1, 0, main_effort),
         main_effort = main_effort - mean(main_effort, na.rm = T),
         weighted = (weighted - mean(weighted, na.rm = T)) / sd(weighted, na.rm = T),
         main_effort = ifelse(bunch_sprint == 1, 0, main_effort))

#

All_data %>%
  filter(year >= 2021) %>%
  
  group_by(rider) %>%
  arrange(rnk, desc(sof_logrk)) %>%
  filter(rank(log(rnk)+sof_logrk, ties.method = "first") <= 15) %>%
  summarize(rel025 = mean(sd_rel_15, na.rm = T), 
            rel2 = mean(sd_rel_120, na.rm = T),
            rel5 = mean(sd_rel_300, na.rm = T),
            rel10 = mean(sd_rel_600, na.rm = T),
            rel20 = mean(sd_rel_1200, na.rm = T),
            rel40 = mean(sd_rel_2400, na.rm = T),
            abs025 = mean(sd_abs_15, na.rm = T),
            abs2 = mean(sd_abs_120, na.rm = T),
            abs5 = mean(sd_abs_300, na.rm = T),
            abs10 = mean(sd_abs_600, na.rm = T),
            abs20 = mean(sd_abs_1200, na.rm = T),
            abs40 = mean(sd_abs_2400, na.rm = T),
            BS = mean(bunch_sprint, na.rm = T),
            rnk = mean(log(rnk), na.rm = T),
            races = n()) %>%
  ungroup() %>%
  
  mutate(overall = ((1-BS)*((rel2+rel5+rel10+rel20+rel40+abs2+abs5+abs10+abs20+abs40)/10)) + 
           (BS * (abs025+rel025)/2)) %>%
  
  mutate(aboverrl_40 = abs40 - rel40,
         aboverrl_20 = abs20 - rel20,
         aboverrl_10 = abs10 - rel10,
         aboverrl_5 = abs5 - rel5,
         aboverrl_2 = abs2 - rel2,
         aboverrl_025 = abs025 - rel025) -> implied_power

#

dbWriteTable(con, "model_implied_power", implied_power, overwrite = TRUE, row.names = FALSE)

#

implied_power %>%
  gather(stat, value, c(rel025:abs40, aboverrl_40:aboverrl_025)) %>% 
  mutate(stat = str_replace(stat, "rel", "relative_"), 
         stat = str_replace(stat, "abs", "absolute_")) %>%
  separate(stat, c("power", "duration"), sep = "_") %>% 
  inner_join(riders_in_race %>% filter(team == "Jumbo-Visma (WT)")) %>%
  filter(duration != "025") %>%
  mutate(duration = ifelse(duration == "025", 0.25, duration),
         power = ifelse(power == "aboverrl", "absolute over relative", power)) %>%
  
  filter(power %in% c("absolute", "relative")) %>%
  
  ggplot(aes(x = as.numeric(duration), y = value, color = rider))+
  geom_hline(yintercept = 0)+
  geom_point(size=3)+
  geom_line(size=1)+
  facet_wrap(~power, ncol = 1)+
  scale_x_log10(breaks = c(2,5,10,20,40))+
  scale_y_continuous()+labs(x = "duration (mins)", 
                            y = "SDs vs average",
                            title = "Estimated power curves")

# #
# 
# implied_power %>% 
#   filter(rider %in% c("Roglic Primoz", "Pogacar Tadej", "Bernal Egan", "Philipsen Jasper",
#                       "Van Baarle Dylan", "Evenepoel Remco", "Van Der Poel Mathieu")) %>%
#   gather(stat, value, rel2:abs40) %>% 
#   mutate(effort = str_replace(stat, "rel", ""), 
#          effort = str_replace(effort, "abs", ""),
#          effort = as.numeric(effort),
#          type = str_sub(stat, 1, 3)) %>%
#   ggplot(aes(x = effort, y = value, color = rider))+
#   geom_hline(yintercept = 0)+
#   geom_line()+
#   geom_point(size=3, shape = 15)+
#   scale_x_log10(breaks = c(2,5,10,20,40))+
#   facet_wrap(~type, ncol = 1)

#

# I can generate all of these lme4 predictions
# and then compare them with what Stan out-puts
# that gives me an idea of how much Stan is regressing the small samples data
# to the mean, so I can just regress the lme4 predictions based on small samples
# because one iteration of lme4 takes 45 seconds and can be run on AWS
# while Stan takes 7500 seconds and is trickier to run on AWS

days_behind = c(#1100, 
  730)#, 365)#, 180, 90)

for(b in 1:length(All_dates$date)) {
  
  # go back 4 months further in 2020 post-lockdown
  
  if(All_dates$date[[b]] > '2020-04-01' & All_dates$date[[b]] < '2022-08-01') {
    howfar = 120
  } else {
    howfar = 0
  }
  
  for(N in days_behind) {
    
    # one day before predicting date and two years back
    maxD <- as.Date(All_dates$date[[b]])
    minD <- maxD - N - howfar
    
    # set-up different time length data-sets
    
    dx <- All_data %>% filter(between(date, minD, maxD)==TRUE) %>%
      
      mutate(rnk1 = ifelse(rnk >= 200, NA, rnk)) %>%
      
      group_by(stage, race, year, class, date) %>%
      mutate(rnk = ifelse(rnk==200, max(rnk1, na.rm = T)+1, rnk)) %>%
      ungroup() %>%
      
      mutate(adjusted_logrk = log(rnk) - sof_logrk) %>%
      
      group_by(rider) %>% 
      filter(n()>9) %>% 
      ungroup() %>%
      
      mutate(win = ifelse(rnk == 1, 1, 0)) %>%
      
      mutate(weights = 1 / (25 + as.numeric(maxD - date)),
             lt_weights = 1 / (750 + as.numeric(maxD - date)))
    
    if(nrow(dx) > 0) {
      
      #
      # including uphill finish
      #
      
      # mod_logrk2_efforts <- lme4::lmer(adjusted_logrk ~ (1 + bunch_sprint | rider) +
      #                                    (0 + perc_rel_300 | rider) +
      #                                    (0 + perc_rel_2400 | rider) +
      #                                    (0 + perc_abs_300 | rider),
      #                                  data = dx,
      #                                  #weights = dx$lt_weights,
      #                                  control = lme4::lmerControl(optimizer = "nloptwrap"))
      
      mod_logrk2_efforts <- lme4::lmer(adjusted_logrk ~ (1 + bunch_sprint | rider) +
                                         (0 + sd_rel_300 | rider) +
                                         (0 + sd_rel_2400 | rider) +
                                         (0 + sd_abs_300 | rider),
                                       data = dx,
                                       #weights = dx$lt_weights,
                                       control = lme4::lmerControl(optimizer = "nloptwrap"))

      # mod_logrk2_efforts <- lme4::lmer(adjusted_logrk ~ (1 + bunch_sprint | rider) +
      #                                    (0 + longer | rider) +
      #                                    (0 + abs_or_rel_5min | rider),
      #                                  data = dx %>%
      #                                    mutate(longer = ((sd_abs_600+sd_abs_1200+sd_abs_2400))/3,
      #                                           longer = (longer - mean(longer, na.rm = T)) / sd(longer, na.rm = T)),
      #                                  #weights = dx$lt_weights,
      #                                  control = lme4::lmerControl(optimizer = "nloptwrap"))
      
      summary(mod_logrk2_efforts)
      
      # random_effects <- lme4::ranef(mod_logrk2_efforts)[[1]] %>%
      #   rownames_to_column() %>%
      #   rename(abs_vs_rel_5min = abs_or_rel_5min,
      #          abs_vs_rel_long = longer,
      #          bunchsprint_impact = bunch_sprint,
      #          random_intercept = `(Intercept)`,
      #          rider = rowname) %>%
      #   #what date are we predicting
      #   mutate(date = as.Date(maxD)+1)  %>%
      #   
      #   mutate(model_desc = 'efforts',
      #          notes = 'abs_vs_rel',
      #          sample_days = N) %>%
      #   
      #   gather(metric, coefficient, abs_vs_rel_5min:random_intercept)
      # 
      #dbWriteTable(con, "lme4_rider_logranks_gather", random_effects, append = TRUE, row.names = FALSE)
      
      # random_effects <- lme4::ranef(mod_logrk2_efforts)[[1]] %>%
      #   rownames_to_column() %>%
      #   rename(impact_abs_5 = perc_abs_300,
      #          impact_rel_5 = perc_rel_300,
      #          impact_rel_40 = perc_rel_2400,
      #          bunchsprint_impact = bunch_sprint,
      #          random_intercept = `(Intercept)`,
      #          rider = rowname) %>%
      #   #what date are we predicting
      #   mutate(date = as.Date(maxD)+1)  %>%
      # 
      #   mutate(model_desc = 'efforts',
      #          notes = 'unweighted w/o long abs',
      #          sample_days = N)
      
      random_effects <- lme4::ranef(mod_logrk2_efforts)[[1]] %>%
        rownames_to_column() %>%
        rename(impact_abs_5 = sd_abs_300,
               impact_rel_5 = sd_rel_300,
               impact_rel_40 = sd_rel_2400,
               bunchsprint_impact = bunch_sprint,
               random_intercept = `(Intercept)`,
               rider = rowname) %>%
        #what date are we predicting
        mutate(date = as.Date(maxD)+1)  %>%
        
        mutate(model_desc = 'efforts',
               notes = 'unweighted w/o long abs SD',
               sample_days = N)
      
      #
      
      dbWriteTable(con, "lme4_rider_logranks_new", random_effects, append = TRUE, row.names = FALSE)
      
    }
    
    rm(dx)
    print(b)
    print(random_effects %>% arrange(impact_abs_5) %>% select(-c(model_desc, notes, sample_days)) %>% .[1,])
    
  }
  
}
