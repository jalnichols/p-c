
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

All_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE date > '2014-07-04'") %>%
  mutate(rider = str_replace(rider, "'", " "), 
         rider = str_trim(rider),
         rider = str_to_title(rider)) %>%
  mutate(rider = case_when(rider == "Ghirmay Hailu Biniam" ~ "Girmay Biniam",
                           rider == "Girmay Hailu Biniam" ~ "Girmay Biniam",
                           TRUE ~ rider)) %>%
  filter(team_time_trial == 0) %>%
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty) | time_trial == 1) %>%
  
  mutate(points_per_opp = ifelse(tm_pos == 1, points_finish, NA),
         sof_per_opp = ifelse(tm_pos == 1, sof, NA),
         pred_climb_diff_opp = ifelse(tm_pos == 1, pred_climb_difficulty, NA),
         pred_climb_diff_succ = ifelse(points_finish > 0, pred_climb_difficulty, NA),
         team_ldr = ifelse(tm_pos == 1, 1, 0)) %>%
  
  mutate(date = as.Date(date)) %>%
  
  select(-speed, -gain_3rd, -gain_5th, -gain_10th, -gain_40th,
         -gc_winner, -gc_pos, -parcours_value, -stage_type,
         -avg_alt, -missing_profile_data) %>%
  
  #filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC", "Olympics")) |
  #         (class %in% c("2.1", "1.1") & tour == "Europe Tour") | 
  #         (sof > 0.2 & class %in% c("2.2", "1.2", "2.2U", "1.2U", "2.Ncup", "1.Ncup", "JR")) |
  #        (sof > 0.1 & !class %in% c("2.2", "1.2", "2.2U", "1.2U", "2.Ncup", "1.Ncup", "JR"))) %>%
  
  left_join(read_delim("cobbles.csv") %>% mutate(stage = as.character(stage))) %>% 
  
  mutate(cobbles = ifelse(is.na(cobbles), 0, cobbles)) %>%
  
  mutate(final_group = ifelse(bunch_sprint == 1, ifelse(gain_1st <= 5, 1, 0), ifelse(rnk <= 20 | gain_20th == 0, 1, 0))) %>%
  
  select(-gain_1st, -gain_20th)

#
#
#

predicting_all <- All_data %>%
  
  filter(date > '2014-07-04') %>%
  
  filter(time_trial == 0) %>%
  select(-points_per_opp, -sof_per_opp, -pred_climb_diff_opp, -final_group,
         -team_time_trial, -time_trial, -pred_climb_diff_succ, -win_seconds,
         -total_seconds, -gain_gc, -cobbles, -sof_limit, -points_finish,
         -leader_rating) %>%
  mutate(stage_join = as.character(stage)) %>%
  inner_join(dbGetQuery(con, "SELECT * FROM predictions_xgboost_stage_bunchsprint") %>%
               select(-bunch_sprint) %>%
               select(stage, url, predicted_bs = model_pred) %>%
               unique(), by = c("stage_join" = "stage", "url")) %>%
  select(-stage_join) %>%
  
  unique() %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT DISTINCT date FROM lme4_rider_logranks_sq WHERE test_or_prod = 'BS_not_ODR'") %>%
      mutate(date = as.Date(date)), by = c("date")
    
  ) %>%
  
  unique() %>%
  
  mutate(pred_climb_difficulty = ifelse(pred_climb_difficulty < 0, 0, pred_climb_difficulty ^ 2),
         one_day_race = ifelse(one_day_race == 1, 1 - predicted_bs, 0)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT * FROM lme4_rider_logranks_sq WHERE test_or_prod IN ('BS_not_ODR')") %>%
      filter(date >= as.Date('2014-07-04'))  %>%
      filter(!is.na(one_day_race)) %>%
      mutate(sample_days = ifelse(test_or_prod == "wtd_BS_not_ODR", paste0("wtd_", sample_days), 
                                  ifelse(test_or_prod == "wtd_BS_not_ODR_5", paste0("wtd5_", sample_days), sample_days))) %>%
      select(-test_or_prod) %>%
      unique() %>%
      mutate(rider = str_to_title(rider)) %>%
      mutate(date = as.Date(date)) %>%
      
      mutate(level_data = ifelse(is.na(sqpcd_impact), "just_rider",
                                 ifelse(is.na(bunchsprint_impact), "pcd_added",
                                        ifelse(is.na(one_day_race), "bs_added", "odr_added")))) %>%
      
      rename(pcd_impact = sqpcd_impact) %>%
      
      # the standard deviations of random intercept and pcd impact both vary widely (increase as you move from 2015 to 2020)
      # we adjust here
      group_by(date, level_data, sample_days) %>%
      mutate(pcd_impact_new = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact, na.rm = T),
             random_intercept_new = (random_intercept - mean(random_intercept, na.rm = T)) / sd(random_intercept, na.rm = T)) %>%
      # this transforms them back to input into the regression equation
      mutate(pcd_impact = pcd_impact_new * sd(pcd_impact, na.rm = T),
             random_intercept = random_intercept_new * sd(random_intercept, na.rm = T)) %>%
      ungroup() %>%
      
      select(-pcd_impact_new, -random_intercept_new) %>%
      
      rename(pcd_logrk_impact = pcd_impact,
             bs_logrk_impact = bunchsprint_impact,
             rand_logrk_impact = random_intercept,
             odr_logrk_impact = one_day_race) %>%
      
      mutate(pcd_logrk_impact = ifelse(is.na(pcd_logrk_impact), 0, pcd_logrk_impact),
             bs_logrk_impact = ifelse(is.na(bs_logrk_impact), 0, bs_logrk_impact),
             odr_logrk_impact = ifelse(is.na(odr_logrk_impact), 0, odr_logrk_impact)
      ), by = c("rider", "date")
    
  ) %>%
  
  #expand_grid(INTERCEPT = seq(3.5,5.5,0.1)) %>%
  
  mutate(INTERCEPT = 4.4) %>%
  
  mutate(pred_rank = exp(INTERCEPT + (1.15 * sof) + 
                           ((rand_logrk_impact) + 
                              (predicted_bs * bs_logrk_impact) + 
                              (one_day_race * odr_logrk_impact) + 
                              (pcd_logrk_impact * pred_climb_difficulty)))) %>%
  
  select(-rand_logrk_impact, -pcd_logrk_impact, -bs_logrk_impact, 
         -odr_logrk_impact) %>%
  
  filter(sample_days == 730) %>%
  
  rowid_to_column() %>%
  
  group_by(sample_days, rider, race, stage, year, class, date, length, url) %>%
  filter(max(rowid) == rowid) %>%
  ungroup() %>%
  
  mutate(sample_days = paste0("pred_rank_", sample_days)) %>%
  
  spread(sample_days, pred_rank) %>%
  
  left_join(
    
    dbReadTable(con, "lme4_rider_teamleader")  %>%
      filter(test_or_prod == "prod") %>%
      select(-test_or_prod) %>%
      unique() %>%
      mutate(rider = str_to_title(rider)) %>%
      mutate(date = as.Date(date)-1) %>%
      
      mutate(level_data = ifelse(is.na(pcd_impact), "just_rider",
                                 ifelse(is.na(bunchsprint_impact), "pcd_added",
                                        ifelse(is.na(one_day_race), "bs_added", "odr_added")))) %>%
      
      # the standard deviations of random intercept and pcd impact both vary widely (increase as you move from 2015 to 2020)
      # we adjust here
      group_by(date, level_data) %>%
      mutate(pcd_impact_new = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact, na.rm = T),
             random_intercept_new = (random_intercept - mean(random_intercept, na.rm = T)) / sd(random_intercept, na.rm = T)) %>%
      ungroup() %>%
      
      # this transforms them back to input into the regression equation
      mutate(pcd_impact = pcd_impact_new * sd(pcd_impact, na.rm = T),
             random_intercept = random_intercept_new * sd(random_intercept, na.rm = T)) %>%
      
      select(-pcd_impact_new, -random_intercept_new) %>%
      
      rename(pcd_tmldr_impact = pcd_impact,
             bs_tmldr_impact = bunchsprint_impact,
             odr_tmldr_impact = one_day_race) %>%
      
      mutate(pcd_tmldr_impact = ifelse(is.na(pcd_tmldr_impact), 0, pcd_tmldr_impact),
             bs_tmldr_impact = ifelse(is.na(bs_tmldr_impact), 0, bs_tmldr_impact),
             odr_tmldr_impact = ifelse(is.na(odr_tmldr_impact), 0, odr_tmldr_impact)
      ) %>%
      
      mutate(level_data = "odr_added") %>%
      
      filter(date >= as.Date('2014-01-01')) %>%
      
      group_by(rider, date, level_data) %>%
      summarize(pcd_tmldr_impact = mean(pcd_tmldr_impact),
                bs_tmldr_impact = mean(bs_tmldr_impact),
                odr_tmldr_impact = mean(odr_tmldr_impact),
                random_intercept = mean(random_intercept)) %>%
      ungroup(), by = c("rider", "date", 'level_data')
    
  ) %>%
  
  # calculate team leader and success predictions using random effects
  mutate(glmer_pred = -3.5 + (random_intercept + 
                                (sqrt(pred_climb_difficulty) * pcd_tmldr_impact) + 
                                (one_day_race * odr_tmldr_impact) + 
                                (predicted_bs * bs_tmldr_impact)),
         glmer_pred = exp(glmer_pred) / (1+exp(glmer_pred))) %>%
  
  select(-random_intercept, -pcd_tmldr_impact, -odr_tmldr_impact, -bs_tmldr_impact) %>%
  unique() %>%
  
  mutate(rider_match = str_to_title(rider)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, date as dob FROM rider_attributes") %>%
      filter(!is.na(dob)) %>%
      mutate(rider = str_to_title(rider)) %>%
      unique() %>%
      group_by(rider) %>%
      filter(max(dob) == dob) %>%
      ungroup(), by = c("rider_match" = "rider")) %>%
  
  mutate(age = as.numeric(as.Date(date)-as.Date(dob))/365.25) %>%
  
  group_by(stage, race, year, level_data) %>%
  mutate(rel_age = age - mean(age, na.rm = T)) %>%
  ungroup() %>%
  
  select(-age, -dob) %>%
  mutate(rel_age = ifelse(is.na(rel_age), 0, rel_age)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
      filter(!is.na(weight)) %>%
      mutate(rider = str_to_title(rider)) %>%
      unique() %>%
      group_by(rider) %>%
      summarize(weight = median(weight, na.rm = T)) %>%
      ungroup(), by = c("rider_match" = "rider")) %>%
  
  group_by(stage, race, year, level_data) %>%
  mutate(rel_weight = weight - mean(weight, na.rm = T)) %>%
  ungroup() %>%
  
  select(-rider_match) %>%
  mutate(rel_weight = ifelse(is.na(rel_weight), 0, rel_weight)) %>%
  
  # give everyone with missing data the median data point
  group_by(stage, race, year, class, level_data) %>%
  mutate(glmer_pred = ifelse(is.na(glmer_pred), median(glmer_pred, na.rm = T), glmer_pred)) %>%
  ungroup() %>%
  
  # rank each stat within race
  group_by(stage, race, year, class, level_data) %>%
  mutate(rk_teamldr = rank(-glmer_pred, ties.method = "min"),
         #rk_rank90 = rank(pred_rank_90, ties.method = "min"),
         #rk_rank180 = rank(pred_rank_180, ties.method = "min"),
         #rk_rank365 = rank(pred_rank_365, ties.method = "min"),
         rk_rank730 = rank(pred_rank_730, ties.method = "min")#,
         #rk_rank1100 = rank(pred_rank_1100, ties.method = "min")
  ) %>%
  ungroup() %>%
  
  group_by(stage, race, year, team, level_data) %>% 
  # shrink estimates to account for other riders in team
  mutate(shrunk_teamldr = glmer_pred/sum(glmer_pred, na.rm = T)) %>% 
  # rank within team
  mutate(teamldr_within_team = rank(rk_teamldr, ties.method = "min")) %>% 
  ungroup() %>%
  
  mutate(class_small = case_when(class %in% c("2.1", "1.1", "CC") ~ ".1", 
                                 class %in% c("2.2", "1.2") ~ ".2", 
                                 class %in% c("WC", "2.UWT", "1.UWT") ~ "WT", 
                                 class %in% c("1.Pro", "1.HC", "2.Pro", "2.HC") ~ ".HC", 
                                 TRUE ~ class)) %>%
  
  left_join(dbGetQuery(con, "SELECT rider, race, year, team, bib 
                       FROM pcs_all_startlists") %>%
              mutate(rider = str_to_title(rider)) %>%
              group_by(race, year, team) %>%
              mutate(bibrk = rank(bib, ties.method = "min")) %>%
              ungroup() %>%
              mutate(bib_tmldr = ifelse(bibrk == 1, 1, 0)), by = c("rider", "race", "year", "team")) %>%
  
  mutate(pred_rank_730 = ifelse(pred_rank_730 >= 150, 150, pred_rank_730)) %>%
  
  group_by(race, stage, year, team) %>%
  mutate(tm_rel_logrk = (mean(log(pred_rank_730), na.rm = T) - log(pred_rank_730)) / (n() - 1),
         vsbest_tm_logrk = log(pred_rank_730) - min(log(pred_rank_730), na.rm = T)) %>%
  ungroup()

#
#
#

# Write Predictions -------------------------------------------------------

predictions_for_races <- predicting_all %>%
  select(rnk, rider, team, length, stage, race, year, url, class, date,
         grand_tour, one_day_race, pred_climb_difficulty, bunch_sprint,
         sof_logrk, predicted_bs, pred_rank = pred_rank_730, ordinal_pred_rank = rk_rank730)

#
#
#

unique_dates <- predicting_all %>%
  select(date) %>%
  unique() %>%
  filter(date > '2018-01-01') %>%
  arrange(desc(date)) %>%
  
  anti_join(dbGetQuery(con, "SELECT DISTINCT date FROM last_two_years_position_in_team") %>%
              mutate(date = as.Date(date)), by = c("date")) %>%
  
  rbind(tibble(date = lubridate::today())) %>%
  arrange(desc(date))

#

for(i in 2:length(unique_dates$date)) {
  
  MAX = unique_dates$date[[i]] - 1
  MIN = MAX - 730
  
  vs_exp <- predicting_all %>%
    filter(between(date, MIN, MAX)) %>%

    group_by(rider) %>%
    summarize(exp_rank = mean(log(pred_rank_730), na.rm = T),
              act_rank = mean(log(rnk), na.rm = T),
              team_leader = (mean(team_ldr, na.rm = T) - mean(glmer_pred, na.rm = T)),
              within_tm = mean(tm_rel_logrk, na.rm = T),
              vsbest_tn = mean(vsbest_tm_logrk, na.rm = T),
              races = n()) %>%
    ungroup() %>%
    
    mutate(date = unique_dates$date[[i]])
  
  dbWriteTable(con, "last_two_years_position_in_team", vs_exp, append = TRUE, row.names = F)
  
  print(i)
  
}


#
#
#

unique_dates <- predicting_all %>%
  select(date) %>%
  unique() %>%
  filter(date > '2015-01-01') %>%
  arrange(desc(date)) %>%
  
  anti_join(dbGetQuery(con, "SELECT DISTINCT date FROM performance_last10races_vsmodel") %>%
              mutate(date = as.Date(date)), by = c("date")) %>%
  
  rbind(tibble(date = lubridate::today())) %>%
  arrange(desc(date))

#

for(i in 2:length(unique_dates$date)) {
  
  MAX = unique_dates$date[[i]] - 1
  MIN = MAX - 366
  
  vs_exp <- predicting_all %>%
    filter(between(date, MIN, MAX)) %>%
    
    mutate(weighting = 1 / pred_rank_730) %>%
    
    group_by(rider) %>%
    filter(date >= (MAX - 29)) %>%
    summarize(exp_rank = sum(log(pred_rank_730) * weighting, na.rm = T) / sum(weighting, na.rm = T),
              act_rank = sum(log(rnk) * weighting, na.rm = T) / sum(weighting, na.rm = T),
              error_rank = sum((log(rnk) - log(pred_rank_730)) * weighting, na.rm = T) / sum(weighting, na.rm = T),
              team_leader = (sum(team_ldr * weighting, na.rm = T) - sum(glmer_pred * weighting, na.rm = T)) / sum(weighting, na.rm = T),
              sof = mean(sof, na.rm = T),
              races = n()) %>%
    ungroup() %>%
    
    mutate(ratio_rk = act_rank / exp_rank,
           exp_rank = exp(exp_rank),
           act_rank = exp(act_rank)) %>%
    
    mutate(date = unique_dates$date[[i]])
  
  dbWriteTable(con, "performance_last10races_vsmodel", vs_exp, append = TRUE, row.names = F)
  
  print(i)
  
}

#
#
#

race_errors <- predicting_all %>%
  
  group_by(race, stage, year, date, class, url) %>%
  mutate(pred_rank_730 = pred_rank_730 / mean(pred_rank_730) * mean(rnk)) %>%
  ungroup() %>%
  
  mutate(pred_for_top_3 = ifelse(rnk <= 3, pred_rank_730, NA),
         pred_for_winner = ifelse(rnk == 1, pred_rank_730, NA),
         rnk_for_winner = ifelse(rnk == 1, log(rk_rank730), NA),
         rnk_for_top_3 = ifelse(rnk <= 3, log(rk_rank730), NA)) %>%
  
  group_by(race, stage, year, date, class, url, pred_climb_difficulty, 
           bunch_sprint, grand_tour, one_day_race, predicted_bs, uphill_finish) %>% 
  filter(!is.na(pred_rank_730)) %>% 
  summarize(error = mean(abs(log(rnk)-log(pred_rank_730)), na.rm = T), 
            correlation = cor(x = log(rnk), y = log(pred_rank_730), use = 'complete.obs'),
            top_3 = mean(log(pred_for_top_3), na.rm = T),
            winner = mean(log(pred_for_winner), na.rm = T),
            top_3_rk = mean(rnk_for_top_3, na.rm = T),
            winner_rk = mean(rnk_for_winner, na.rm = T),
            riders = n()) %>% 
  ungroup() %>%

  mutate(perc_rk_corr = percent_rank(correlation),
         perc_rk_err = 1-percent_rank(error),
         perc_rk_winner = 1-percent_rank(winner),
         perc_rk_winner_rk = 1-percent_rank(winner_rk),
         perc_rk_top3_rk = 1-percent_rank(top_3_rk)) %>%
  
  separate(url, c("j1", "url_race", "j2"), sep = "\\/") %>%
  select(-j1, -j2)

#

dbWriteTable(con, "race_errors_normal_logrank_model", race_errors, overwrite = TRUE, row.names = FALSE)

#
#
#

# Impact of temperature

predicting_all %>%
  inner_join(dbGetQuery(con, "SELECT * FROM race_level_temperature")) %>% 
  mutate(temperature = (temperature - 32) * 5/9) %>%
  mutate(rnk = log(rnk), 
         predrnk = log(pred_rank_730), 
         rnkdiff = rnk - predrnk, 
         temp = temperature - 21) %>% 
  
  filter(class_small %in% c(".1", "WT", ".HC")) %>% 
  
  group_by(rider) %>% 
  mutate(temp = temp - mean(temp, na.rm = T)) %>% 
  filter(n() >= 50) %>% 
  do(broom::tidy(lm(rnkdiff ~ temp, data = ., weights = 1 / exp(predrnk)))) %>%
  ungroup() -> rider_temps

#

filtered_for_best_riders <- rider_temps %>%
  
  inner_join(predicting_all %>%
               filter(class_small %in% c(".1", "WT", "HC")) %>% 
               group_by(rider) %>%
               filter(n() >= 50) %>%
               summarize(perf = mean(log(rnk)-sof_logrk, na.rm = T),
                         pred = mean(log(pred_rank_730), na.rm = T),
                         races = n()) %>%
               ungroup())

#

filtered_for_best_riders %>% 
  filter(term == "temp" & perf <= 2.6 & !rider %in% c("Lambrecht Bjorg", "Aru Fabio", "Pinto Edgar", "Colbrelli Sonny",
                                                      "Contador Alberto", "Mader Gino")) %>%
  
  ggplot(aes(x = term, 
             y = exp(2.5 + (estimate*11)) - exp(2.5 + (estimate*-16)),
             label = rider, 
             group = term))+
  
  geom_boxplot(outlier.color = "transparent")+
  geom_jitter(width = 0.33)+
  coord_flip()+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())+
  geom_text(check_overlap = TRUE, angle = 90)+
  labs(y = "WARMER = BETTER                                                                                               COLDER = BETTER", 
       title = "Impact of temperature on performance", 
       subtitle = "x axis shown as finishing position difference between hot and cool races")
       
#

predicting_all %>%
  inner_join(dbGetQuery(con, "SELECT * FROM race_level_temperature")) %>% 
  mutate(temperature = (temperature - 32) * 5/9) %>%
  mutate(rnk = log(rnk), 
         predrnk = log(pred_rank_730), 
         rnkdiff = rnk - predrnk, 
         temp = temperature - 21) %>% 
  
  filter(class_small %in% c(".1", "WT", ".HC")) %>%
  
  filter(rider %in% c("Van Der Poel Mathieu", "Kuss Sepp", "Pogacar Tadej")) %>%
  
  ggplot(aes(x = temp, y = rnkdiff, color = rider))+
  
  geom_point(alpha=0.25)+
  geom_smooth(method = "lm")+
  labs(x = "temperature vs 21 degrees C",
       y = "Logged rank difference vs expected",
       title = "MVDP, Pog underperform in heat\nKuss overperforms")+
  scale_color_manual(values = c("#FFCE0C", "red", "#015AA4"), name = "")+
  theme(legend.position = "bottom", plot.title = element_text(size = 18, face = "bold"))+
  annotate("text",
           x = 0,
           y = -4.5,
           label = "Better Performance than Expected")+
  annotate("text",
           x = 0,
           y = 4,
           label = "Worse Performance than Expected")+
  annotate("text",
           x = 25,
           y = 0,
           label = "Hotter than average",
           angle = 90)+
  annotate("text",
           x = -20,
           y = 0,
           label = "Colder than average",
           angle = 90)

#
#
#

# VARIANCE

variance_results <- predicting_all %>% 
  filter(class_small %in% c(".1", "WT", "HC")) %>% 
  filter(date >= '2020-06-01') %>% 
  mutate(rnk = log(rnk), 
         predrnk = log(pred_rank_730), 
         rnkdiff = rnk - predrnk,
         wt_rnkdiff = rnkdiff * (1 / predrnk)) %>% 
  group_by(rider) %>% 
  filter(n() >= 50) %>%
  summarize(IQR = quantile(rnkdiff, probs = 0.75) - quantile(rnkdiff, probs = 0.25),
            x10_90 = quantile(rnkdiff, probs = 0.90) - quantile(rnkdiff, probs = 0.10), 
            stdev = sd(rnkdiff, na.rm = T),
            wt_stdev = sd(wt_rnkdiff, na.rm = T),
            median = exp(median(rnk-sof_logrk, na.rm = T)),
            races = n()) %>%
  ungroup()

#
#
#
#
#
#
#

# Errors based on course features
# eg, downhill finish

unique_dates <- predicting_all %>%
  select(date) %>%
  unique() %>%
  filter(date > '2017-04-01') %>%
  arrange(desc(date)) %>%
  
  #anti_join(dbGetQuery(con, "SELECT DISTINCT date FROM performance_last10races_vsmodel") %>%
  #            mutate(date = as.Date(date)), by = c("date")) %>%
  
  rbind(tibble(date = lubridate::today())) %>%
  arrange(desc(date))

#

stage_dhs <- dbGetQuery(con, sprintf("SELECT *
               FROM downhills_from_new_fr_telemetry")) %>%

  group_by(id) %>%
  filter(perc_thru >= 0.67) %>%
  summarize(desc_length = sum(length, na.rm = T)) %>%
  ungroup() %>%
  
  inner_join(dbGetQuery(con, "SELECT DISTINCT * FROM new_fr_stages") %>%
               
               mutate(start_date = ifelse(race_url == "https://www.la-flamme-rouge.eu/maps/races/view/2022/3", as.Date("2022-08-19"), as.Date(start_date)),
                      start_date = as.Date(start_date, origin = "1970-01-01"),
                      start_date = ifelse(start_date < as.Date('1900-01-01'), as.Date(paste0(lubridate::year(date), "-", str_sub(start_date, 6, 10))), start_date),
                      start_date = as.Date(start_date, origin = "1970-01-01")) %>%
               
               arrange(date) %>%
               mutate(id = str_replace(url, "https://www.la-flamme-rouge.eu/maps/loadtrack/", "")) %>%
               select(id, stage, race, date, class, length), by = c("id")) %>%
  
  mutate(final_chunk_descending = desc_length / (0.25 * length)) %>%
  
  mutate(pcs_race = str_replace_all(str_to_lower(race)," "," "),
         year = lubridate::year(date)) %>%
  
  left_join(predicting_all %>%
              select(race, url, year, class, date, length) %>%
              unique(), by = c("pcs_race" = "race", "class", "year", "date"))

#

for(i in 2:length(unique_dates$date)) {
  
  MAX = unique_dates$date[[i]] - 1
  MIN = MAX - 731
  
  vs_exp <- predicting_all %>%
    filter(between(date, MIN, MAX)) %>%
    
    mutate(weighting = 1 / pred_rank_730) %>%
    
    left_join(stage_dhs %>%
                select(url, year, desc_length, stage), by = c("stage", "url", "year")) %>% 
    
    mutate(pred_error = log(pred_rank_730) - log(rnk)) %>%
    
    filter(!is.na(desc_length)) %>%
    group_by(rider) %>%
    summarize(races = n(), 
              corr_desc = cor(x = pred_error, y = desc_length, use = "complete.obs"),
              corr_pcd = cor(x = pred_error, y = pred_climb_difficulty, use = "complete.obs")) %>%
    
    mutate(date = unique_dates$date[[i]]) %>%
    
    mutate(corr_desc - corr_pcd)
  
}

#
#
#
#
#


# Efforts Predictions -----------------------------------------------------

predicting_all_efforts <- All_data %>%
  
  filter(date > '2017-01-01') %>%
  
  filter(time_trial == 0) %>%
  select(-points_per_opp, -sof_per_opp, -pred_climb_diff_opp, -final_group,
         -team_time_trial, -time_trial, -pred_climb_diff_succ, -win_seconds,
         -total_seconds, -gain_gc, -cobbles, -sof_limit, -points_finish,
         -leader_rating) %>%
  mutate(stage_join = as.character(stage)) %>%
  inner_join(dbGetQuery(con, "SELECT * FROM predictions_xgboost_stage_bunchsprint") %>%
               select(-bunch_sprint) %>%
               select(stage, url, predicted_bs = model_pred) %>%
               unique(), by = c("stage_join" = "stage", "url")) %>%
  select(-stage_join) %>%
  
  unique() %>%
  
  inner_join(
    rbind(
      dbGetQuery(con, "SELECT url, stage, date, rolling_speed, sdrel, sdabs
               FROM win_efforts_by_race WHERE rolling_speed IN (300, 1200, 2400, 120)") %>%
        select(url, stage, date, rolling_speed, value = sdrel) %>%
        mutate(rolling_speed = paste0("rel", rolling_speed/60, "min_pred_effort")),
      dbGetQuery(con, "SELECT url, stage, date, rolling_speed, sdrel, sdabs
               FROM win_efforts_by_race WHERE rolling_speed IN (300, 1200, 2400, 120)") %>%
        select(url, stage, date, rolling_speed, value = sdabs) %>%
        mutate(rolling_speed = paste0("abs", rolling_speed/60, "min_pred_effort"))) %>%
      
      group_by(stage, date, rolling_speed, url) %>%
      summarize(value = median(value, na.rm = T)) %>%
      ungroup() %>%
      
      spread(rolling_speed, value), by = c("stage", "url", "date")) %>%
      
  inner_join(
    
    dbGetQuery(con, "SELECT DISTINCT date FROM lme4_rider_logranks_new") %>%
      mutate(date = as.Date(date)), by = c("date")) %>%
  
  unique() %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT * FROM lme4_rider_logranks_new") %>%
      filter(date >= as.Date('2017-01-01'))  %>%
      unique() %>%
      mutate(rider = str_to_title(rider)) %>%
      mutate(date = as.Date(date)) %>%

      rename(rel5_impact = impact_rel_5,
             bs_logrk_impact = bunchsprint_impact,
             rand_logrk_impact = random_intercept,
             rel40_impact = impact_rel_40,
             abs5_impact = impact_abs_5), by = c("rider", "date")) %>%
  
  group_by(stage, url, year, class, date) %>%
  mutate(coef = rand_logrk_impact + (abs5_impact * abs5min_pred_effort) + 
           (bs_logrk_impact * bunch_sprint) + (rel5_impact * rel5min_pred_effort) +
           (rel40_impact * rel40min_pred_effort),
         pred_eff_rank = coef + mean(log(seq(1,n(),1)))) %>%
  ungroup() %>%
  
  mutate(rider_match = str_to_title(rider)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, date as dob FROM rider_attributes") %>%
      filter(!is.na(dob)) %>%
      mutate(rider = str_to_title(rider)) %>%
      unique() %>%
      group_by(rider) %>%
      filter(max(dob) == dob) %>%
      ungroup(), by = c("rider_match" = "rider")) %>%
  
  mutate(age = as.numeric(as.Date(date)-as.Date(dob))/365.25) %>%
  
  group_by(stage, race, year) %>%
  mutate(rel_age = age - mean(age, na.rm = T)) %>%
  ungroup() %>%
  
  select(-age, -rider_match, -dob) %>%
  mutate(rel_age = ifelse(is.na(rel_age), 0, rel_age)) %>%
  
  mutate(class_small = case_when(class %in% c("2.1", "1.1", "CC") ~ ".1", 
                                 class %in% c("2.2", "1.2") ~ ".2", 
                                 class %in% c("WC", "2.UWT", "1.UWT") ~ "WT", 
                                 class %in% c("1.Pro", "1.HC", "2.Pro", "2.HC") ~ ".HC", 
                                 TRUE ~ class))

#
#
#

race_errors_efforts <- predicting_all_efforts %>%
  
  mutate(pred_for_top_3 = ifelse(rnk <= 3, pred_eff_rank, NA),
         pred_for_winner = ifelse(rnk == 1, pred_eff_rank, NA)) %>%
  
  group_by(race, stage, year, date, class, url, pred_climb_difficulty, 
           bunch_sprint, grand_tour, one_day_race, predicted_bs, uphill_finish) %>% 
  filter(!is.na(pred_eff_rank)) %>% 
  summarize(error = mean(abs(log(rnk)-log(pred_eff_rank)), na.rm = T), 
            correlation = cor(x = log(rnk), y = log(pred_eff_rank), use = 'complete.obs'),
            top_3 = mean(log(pred_for_top_3), na.rm = T),
            winner = mean(log(pred_for_winner), na.rm = T),
            riders = n()) %>% 
  ungroup() %>%
  
  mutate(perc_rk_corr = percent_rank(correlation),
         perc_rk_err = 1-percent_rank(error),
         perc_rk_winner = 1-percent_rank(winner)) %>%
  
  separate(url, c("j1", "url_race", "j2"), sep = "\\/") %>%
  select(-j1, -j2)
  
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


# Multiclass predictions of team finish position --------------------------

# Multiclass predictions of 1st/2nd/3rd/other on a team

predicting_all_team_model <- predicting_all %>%
  
  mutate(team_finish = case_when(tm_pos == 1 ~ "Leader",
                                 tm_pos == 2 ~ "Second",
                                 tm_pos == 3 ~ "Third",
                                 TRUE ~ "Other"),
         tm_pos = ifelse(tm_pos > 3, 3, tm_pos-1)) %>%
  
  mutate(pred_rank = pred_rank_730,
         log_pred_rank = log(pred_rank),
         bib_tmldr = ifelse(is.na(bib_tmldr), 0, bib_tmldr))

#

library(xgboost)

xgb.train <- xgb.DMatrix(
  data = as.matrix(predicting_all_team_model %>%
                     filter(year < 2022 & one_day_race != 0) %>%
                     select(pred_rank, log_pred_rank, glmer_pred, teamldr_within_team,
                            bibrk)),
  label = predicting_all_team_model %>%
    filter(year < 2022 & one_day_race != 0) %>%
    select(tm_pos) %>%
    .[[1]]
)

xgb.test <- xgb.DMatrix(
  data = as.matrix(predicting_all_team_model %>%
                     filter(year >= 2022 & one_day_race != 0) %>%
                     select(pred_rank, log_pred_rank, glmer_pred, teamldr_within_team,
                            bibrk)),
  label = predicting_all_team_model %>%
    filter(year >= 2022 & one_day_race != 0) %>%
    select(tm_pos) %>%
    .[[1]]
)

#

params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 5,
  gamma = 0,
  subsample = 1,
  colsample_bytree = 1,
  num_class = n_distinct(predicting_all_team_model$tm_pos),
  tree_method = "hist",
  eval_metric = "mlogloss",
  objective = "multi:softprob"
)

# run xgboost model

gbm_model <- xgb.train(params = params,
                       data = xgb.train,
                       nrounds = 2000,
                       early_stopping_rounds = 500,
                       watchlist = list(val1 = xgb.train,
                                        val2 = xgb.test),
                       verbose = 1)

#

test_pred <- predict(gbm_model, newdata = xgb.test)
test_prediction <- matrix(test_pred, nrow = n_distinct(predicting_all_team_model$tm_pos),
                          ncol=length(test_pred)/n_distinct(predicting_all_team_model$tm_pos)) %>%
  t() %>%
  data.frame() %>%
  cbind(predicting_all_team_model %>% filter(year >= 2022 & one_day_race != 0) %>% 
          select(rnk, team, rider, pred_rank, race, stage, year, class, bib, bib_tmldr, glmer_pred)) %>%
  rename(Leader = X1, Second = X2, Third = X3, Rest = X4)

WIN_GAM_MOD <- mgcv::gam(Win ~ s(pred_rank, k = 15), data = test_prediction)

PODIUM_GAM_MOD <- mgcv::gam(Podium ~ s(pred_rank, k = 15), data = test_prediction)

TOP10_GAM_MOD <- mgcv::gam(Top10 ~ s(pred_rank, k = 15), data = test_prediction)

smoothed_preds <- test_prediction %>%
  mutate(smoothed_WIN = mgcv::predict.gam(WIN_GAM_MOD, .),
         smoothed_PODIUM = mgcv::predict.gam(PODIUM_GAM_MOD, .),
         smoothed_Top10 = mgcv::predict.gam(TOP10_GAM_MOD, .))

#

smoothed_preds %>% 
  group_by(race, stage, year, class) %>% 
  mutate(smoothed_WIN = smoothed_WIN / sum(smoothed_WIN), 
         smoothed_PODIUM = smoothed_PODIUM / sum(smoothed_PODIUM) * 2, 
         smoothed_Top10 = smoothed_Top10 / sum(smoothed_Top10) * 7, 
         riders = n(), 
         mean(log(pred_rank))) -> smoothed_preds_addl

#
#
#

# Specific race modelling -------------------------------------------------

MSR <- predicting_all %>% 
  separate(url, c("j1", "url_race", "j2"), sep = "/") %>%
  
  filter(url_race == "milano-sanremo") %>%
  
  mutate(pred_rank_730 = ifelse(pred_rank_730 >= 150, 150, pred_rank_730))
