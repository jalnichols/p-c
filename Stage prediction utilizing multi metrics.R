
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

All_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE date > '2016-01-01'") %>%
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
  
  filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC", "Olympics")) |
           (class %in% c("2.1", "1.1") & tour == "Europe Tour") | 
           (sof > 0.2 & class %in% c("2.2", "1.2", "2.2U", "1.2U", "2.Ncup", "1.Ncup", "JR")) |
           (sof > 0.1 & !class %in% c("2.2", "1.2", "2.2U", "1.2U", "2.Ncup", "1.Ncup", "JR"))) %>%
  
  left_join(read_delim("cobbles.csv") %>% mutate(stage = as.character(stage))) %>% 
  
  mutate(cobbles = ifelse(is.na(cobbles), 0, cobbles)) %>%
  
  mutate(final_group = ifelse(bunch_sprint == 1, ifelse(gain_1st <= 5, 1, 0), ifelse(rnk <= 20 | gain_20th == 0, 1, 0))) %>%
  
  select(-gain_1st, -gain_20th)

#
#

predicting_TT <- All_data %>%
  filter(time_trial == 1) %>%
  select(-points_per_opp, -sof_per_opp, -pred_climb_diff_opp, -final_group) %>%
  
  unique() %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT DISTINCT date FROM lme4_rider_timetrial") %>%
      mutate(date = as.Date(date)), by = c("date")
    
  ) %>%
  
  unique() %>%
  
  left_join(
    
    dbReadTable(con, "lme4_rider_timetrial")  %>%
      unique() %>%
      mutate(rider = str_to_title(rider)) %>%
      mutate(date = as.Date(date)) %>%
      
      rename(pcd_logrk_impact = tvg_impact,
             rand_logrk_impact = random_intercept) %>%
      
      mutate(rand_logrk_impact = rand_logrk_impact + modelintercept,
             pcd_logrk_impact = ifelse(is.na(pcd_logrk_impact), 0, pcd_logrk_impact)) %>%
      select(-modelintercept, -test_or_prod) %>%
      filter(date >= as.Date('2014-01-01')), by = c("rider", "date")
    
  ) %>%
  
  mutate(pred_rank = exp(rand_logrk_impact + (pcd_logrk_impact * 10))) %>%
  
  select(-rand_logrk_impact) %>%

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
  
  # give everyone with missing data the median data point
  group_by(stage, race, year, class) %>%
  mutate(pred_rank = ifelse(is.na(pred_rank), median(pred_rank, na.rm = T), pred_rank)) %>%
  ungroup() %>%
  
  # rank each stat within race
  group_by(stage, race, year, class) %>%
  mutate(rk_rank = rank(pred_rank, ties.method = "min")) %>%
  ungroup() %>%
  
  mutate(class_small = case_when(class %in% c("2.1", "1.1", "CC") ~ ".1", 
                                 class %in% c("2.2", "1.2") ~ ".2", 
                                 class %in% c("WC", "2.UWT", "1.UWT") ~ "WT", 
                                 class %in% c("1.Pro", "1.HC", "2.Pro", "2.HC") ~ ".HC", 
                                 TRUE ~ class))
#
#
#
#
#

predicting_all <- All_data %>%
  
  filter(race == "tour de france" & stage == 9) %>%
  
  filter(time_trial == 0) %>%
  select(-points_per_opp, -sof_per_opp, -pred_climb_diff_opp, -final_group) %>%
  mutate(stage_join = as.character(stage)) %>%
  inner_join(dbGetQuery(con, "SELECT * FROM predictions_stage_bunchsprint") %>%
               select(-bunch_sprint) %>%
               select(stage, url, predicted_bs) %>%
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
    
    dbReadTable(con, "lme4_rider_logranks_sq")  %>%
      filter(!is.na(one_day_race)) %>%
      filter(test_or_prod == "BS_not_ODR") %>%
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
      group_by(date, level_data) %>%
      mutate(pcd_impact_new = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact, na.rm = T),
             random_intercept_new = (random_intercept - mean(random_intercept, na.rm = T)) / sd(random_intercept, na.rm = T)) %>%
      ungroup() %>%
      
      # this transforms them back to input into the regression equation
      mutate(pcd_impact = pcd_impact_new * sd(pcd_impact, na.rm = T),
             random_intercept = random_intercept_new * sd(random_intercept, na.rm = T)) %>%
      
      select(-pcd_impact_new, -random_intercept_new) %>%
      
      rename(pcd_logrk_impact = pcd_impact,
             bs_logrk_impact = bunchsprint_impact,
             rand_logrk_impact = random_intercept,
             odr_logrk_impact = one_day_race) %>%
      
      mutate(pcd_logrk_impact = ifelse(is.na(pcd_logrk_impact), 0, pcd_logrk_impact),
             bs_logrk_impact = ifelse(is.na(bs_logrk_impact), 0, bs_logrk_impact),
             odr_logrk_impact = ifelse(is.na(odr_logrk_impact), 0, odr_logrk_impact)
             ) %>%
      
      filter(date >= as.Date('2014-01-01')), by = c("rider", "date")
    
  ) %>%
  
  # 0.97 is SD and 3.9 is Mean -- intercept is -0.6 and sof is 1.15 with an avg of 0.36 so -0.2 is left-over
  mutate(pred_rank = exp(-0.2 + ((
    ((rand_logrk_impact + 
      (predicted_bs * bs_logrk_impact) + 
        (one_day_race * odr_logrk_impact) + 
      (pcd_logrk_impact * pred_climb_difficulty))*-1) / 0.97)+3.9))) %>%
  
  select(-rand_logrk_impact, #-pcd_logrk_impact, -bs_logrk_impact, 
         -odr_logrk_impact) %>%
  
  left_join(dbGetQuery(con, "SELECT * FROM lme4_rider_logranks_sq WHERE date > '2022-01-01'")  %>%
              filter(!is.na(one_day_race)) %>%
              filter(test_or_prod == "BS_not_ODR") %>%
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
              group_by(date, level_data) %>%
              mutate(pcd_impact_new = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact, na.rm = T),
                     random_intercept_new = (random_intercept - mean(random_intercept, na.rm = T)) / sd(random_intercept, na.rm = T)) %>%
              ungroup() %>%
              
              # this transforms them back to input into the regression equation
              mutate(pcd_impact = pcd_impact_new * sd(pcd_impact, na.rm = T),
                     random_intercept = random_intercept_new * sd(random_intercept, na.rm = T)) %>%
              
              select(-pcd_impact_new, -random_intercept_new) %>%
              
              rename(pcd_logrk_impact = pcd_impact,
                     bs_logrk_impact = bunchsprint_impact,
                     rand_logrk_impact = random_intercept,
                     odr_logrk_impact = one_day_race) %>%
              
              mutate(pcd_logrk_impact = ifelse(is.na(pcd_logrk_impact), 0, pcd_logrk_impact),
                     bs_logrk_impact = ifelse(is.na(bs_logrk_impact), 0, bs_logrk_impact),
                     odr_logrk_impact = ifelse(is.na(odr_logrk_impact), 0, odr_logrk_impact)
              ) %>%
              filter(between(date, All_data$date[[1]] - 60, All_data$date[[1]]-1)) %>%
              group_by(rider) %>% 
              filter(n() >= 7) %>% 
              ungroup() %>% 
              group_by(rider) %>% 
              filter(date == min(date) | date == max(date)) %>% 
              ungroup() %>%
              group_by(rider) %>%
              mutate(segment = ifelse(date == min(date), "Start", "End")) %>%
              ungroup() %>%
              select(-date, -level_data), by = c("rider")) %>%
  
  mutate(pred_rank.y = exp(-0.2 + ((
    ((rand_logrk_impact + 
        (predicted_bs * bs_logrk_impact.y) + 
        (one_day_race * odr_logrk_impact) + 
        (pcd_logrk_impact.y * pred_climb_difficulty))*-1) / 0.97)+3.9))) %>%
  
  select(-c(rand_logrk_impact, pcd_logrk_impact.y, bs_logrk_impact.y, odr_logrk_impact)) %>% 
  rename(pcd_logrk_impact = pcd_logrk_impact.x, bs_logrk_impact = bs_logrk_impact.x) %>% 
  
  spread(segment, pred_rank.y) %>%
  
  select(-`<NA>`) %>%
  mutate(slope = (End - Start)/60)
  

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
      
      filter(date >= as.Date('2014-01-01')), by = c("rider", "date", 'level_data')
    
  ) %>%
  
  # calculate team leader and success predictions using random effects
  mutate(glmer_pred = -3.5 + (random_intercept + 
                                 (sqrt(pred_climb_difficulty) * pcd_tmldr_impact) + 
                                 (one_day_race * odr_tmldr_impact) + 
                                 (predicted_bs * bs_tmldr_impact)),
         glmer_pred = exp(glmer_pred) / (1+exp(glmer_pred))) %>%
  
  select(-random_intercept, -pcd_tmldr_impact, -odr_tmldr_impact, -bs_tmldr_impact) %>%
  
  # add in cobbles performance
  #left_join(
    
  #  dbReadTable(con, "performance_rider_cobbles")  %>%
      
  #    #mutate(rider = str_to_title(rider)) %>%
      
  #    rename(cobbles_intercept = cobbles), by = c("rider")) %>%

  unique() %>%
  
  #group_by(stage, race, year, team, level_data) %>%
  #mutate(No1_Team = ifelse(rank(-glmer_pred, ties.method = "min")==1, 1, 0),
  #       No1_Team_pwo = ifelse(rank(-pred_pointswhenopp, ties.method = "min")==1, 1, 0)
  #       ) %>%
  #ungroup() %>%
  
  #select(-cobbles_intercept) %>%
  
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
  
  select(-age, -rider_match, -dob) %>%
  mutate(rel_age = ifelse(is.na(rel_age), 0, rel_age)) %>%
  
  #inner_join(
  #  
  #  dbGetQuery(con, "SELECT race, year, rider, bib FROM pcs_all_startlists") %>%
  #    mutate(bib_leader = ifelse(bib %% 10 == 1, 1, 0)) %>%
  #    unique(), by = c("rider", "race", "year")
  #  
  #) %>%
  
  # give everyone with missing data the median data point
  group_by(stage, race, year, class, level_data) %>%
  mutate(glmer_pred = ifelse(is.na(glmer_pred), median(glmer_pred, na.rm = T), glmer_pred),
         #win_pred = ifelse(is.na(win_pred), median(win_pred, na.rm = T), win_pred),
         #pred_points = ifelse(is.na(pred_points), median(pred_points, na.rm = T), pred_points),
         #pred_timelost = ifelse(is.na(pred_timelost), median(pred_timelost, na.rm = T), pred_timelost),
         #pred_pointswhenopp = ifelse(is.na(pred_pointswhenopp), median(pred_pointswhenopp, na.rm = T), pred_pointswhenopp),
         pred_rank = ifelse(is.na(pred_rank), median(pred_rank, na.rm = T), pred_rank)) %>%
  ungroup() %>%
  
  # rank each stat within race
  group_by(stage, race, year, class, level_data) %>%
  mutate(rk_teamldr = rank(-glmer_pred, ties.method = "min"),
         #rk_points = rank(-pred_points, ties.method = "min"),
         #rk_wins = rank(desc(win_pred), ties.method = "min"),
         #rk_timelost = rank(pred_timelost, ties.method = "min"),
         #rk_pointswhenopp = rank(-pred_pointswhenopp, ties.method = "min"),
         rk_rank = rank(pred_rank, ties.method = "min")) %>%
  ungroup() %>%
  
  group_by(stage, race, year, team, level_data) %>% 
  # shrink estimates to account for other riders in team
  mutate(shrunk_teamldr = glmer_pred/sum(glmer_pred, na.rm = T)) %>% 
  # rank within team
  mutate(teamldr_within_team = rank(rk_teamldr, ties.method = "min")) %>% 
  ungroup() %>%
  
  #filter(!is.na(level_data)) %>%
  #filter(level_data == "bs_added") %>%
  
  mutate(class_small = case_when(class %in% c("2.1", "1.1", "CC") ~ ".1", 
                           class %in% c("2.2", "1.2") ~ ".2", 
                           class %in% c("WC", "2.UWT", "1.UWT") ~ "WT", 
                           class %in% c("1.Pro", "1.HC", "2.Pro", "2.HC") ~ ".HC", 
                           TRUE ~ class))

#
# adjust pred_rank for SOF
#

pred_rank_model <- lm(log(rnk) ~ log(pred_rank) + class + sof,
                      data = predicting_all)

#

write_rds(pred_rank_model, "Stored models/predicting_rank_adjusting_for_Zscores.rds")

#

predicting_all_supp <- cbind(
  
  predicting_all %>% rename(old_pred_rank = pred_rank),
  
  pred_rank = predict(read_rds("Stored models/predicting_rank_adjusting_for_Zscores.rds"), predicting_all)) %>%
  
  mutate(pred_rank = exp(pred_rank)) %>%
  unique() %>%
  
  mutate(pred_rank = ifelse(is.na(pred_rank), median(pred_rank, na.rm = T), pred_rank)) %>%
  
  group_by(stage, race, year, class, date) %>%
  mutate(xrnk = ifelse(rnk == 200, NA, rnk),
         missrnk = sum(is.na(xrnk)),
         xrnk = max(xrnk, na.rm = T),
         rnk = ifelse(rnk == 200, xrnk + (missrnk/2), rnk)) %>%
  mutate(pred_rank = pred_rank / mean(pred_rank, na.rm = T) * mean(rnk, na.rm = T)) %>%
  ungroup() %>% 
  select(-c(success, points_finish, leader_rating, team_time_trial, pred_climb_diff_succ, 
            level_data, old_pred_rank, xrnk, missrnk, sof_limit))

#


# Write Features About Predictions ----------------------------------------

dbWriteTable(con, 
             "race_predictions", 
             predicting_all_supp %>%
               filter(year >= 2019) %>%
               select(rider, race, stage, year, class, date,
                      rnk, pred_climb_difficulty, sof, bunch_sprint,
                      predicted_bs, pred_rank),
             overwrite = TRUE,
             row.names = FALSE)


#
#
#
#
#

# Basic Win Model ---------------------------------------------------------

library(xgboost)

fullset <- predicting_all_supp %>%
  filter(class_small %in% c("WT", ".1", ".HC")) %>% 
  mutate(win = ifelse(rnk == 1, 1, 0)) %>% 
  
  group_by(stage, race, year, class, date) %>% 
  filter(n() >= 75) %>% 
  mutate(rel_logrank = mean(log(pred_rank)) - log(pred_rank),
         pred_rank = ifelse(pred_rank < 1.01, 1.01, pred_rank),
         mult_logrank = mean(log(pred_rank)) / log(pred_rank),
         ordered = log(rank(pred_rank, ties.method = "min")),
         pct_rk = percent_rank(pred_rank)) %>%
  ungroup()

#

xgb.train <- xgb.DMatrix(
  data = as.matrix(fullset %>%
                     filter(year != 2022) %>%
                     select(rel_logrank, mult_logrank, ordered, pct_rk, pred_rank, predicted_bs, pred_climb_difficulty)),
  label = fullset %>%
    filter(year != 2022) %>%
    select(win) %>%
    .[[1]]
)

# test

xgb.test <- xgb.DMatrix(
  data = as.matrix(fullset %>%
                     filter(year == 2022) %>%
                     select(rel_logrank, mult_logrank, ordered, pct_rk, pred_rank, predicted_bs, pred_climb_difficulty)),
  label = fullset %>%
    filter(year == 2022) %>%
    select(win) %>%
    .[[1]]
)

# outline parameters

params <- list(
  
  booster = "gbtree",
  eta = 0.3,
  max_depth = 5,
  gamma = 0,
  subsample = 1,
  colsample_bytree = 1,
  tree_method = "hist",
  objective = "binary:logistic"
  
)

# run xgboost model

gbm_model <- xgb.train(params = params,
                       data = xgb.train,
                       nrounds = 10000,
                       monotone_constraints = c(1,1,-1,-1,-1,0,0),
                       early_stopping_rounds = 5000,
                       watchlist = list(val1 = xgb.train,
                                        val2 = xgb.test),
                       verbose = 0)

#
#
# xgb Importance

xgb.importance(model = gbm_model)

gbm_model$best_score

#

gbm_BS_predictions = cbind(
  
  model_pred = predict(gbm_model, 
                       as.matrix(fullset %>%
                                   filter(year == 2022) %>%
                                   select(rel_logrank, mult_logrank, ordered, pct_rk,
                                          pred_rank, predicted_bs, pred_climb_difficulty), reshape=T)),
  
  fullset %>%
    filter(year == 2022)) %>%
  
  mutate(Win_Pred = exp(model_pred) / (1 + exp(model_pred)))
  
#
#
# Race Pred Errors
#
#

race_errors <- predicting_all_supp %>%
  
  group_by(race, stage, year, date, class, class_small, url) %>%
  mutate(pred_rank = pred_rank / mean(pred_rank) * mean(rnk)) %>%
  ungroup() %>%
  
  mutate(pred_for_top_3 = ifelse(rnk <= 3, pred_rank, NA),
         pred_for_winner = ifelse(rnk == 1, pred_rank, NA),
         gain_gc_winner = ifelse(rnk == 1, gain_gc, NA)) %>%
  
  group_by(race, stage, year, date, class, class_small, url, pred_climb_difficulty, 
           bunch_sprint, grand_tour, one_day_race, predicted_bs, uphill_finish) %>% 
  filter(!is.na(pred_rank)) %>% 
  summarize(error = mean(abs(log(rnk)-log(pred_rank)), na.rm = T), 
            correlation = cor(x = log(rnk), y = log(pred_rank), use = 'complete.obs'),
            top_3 = mean(log(pred_for_top_3), na.rm = T),
            winner = mean(log(pred_for_winner), na.rm = T),
            winner_gain_gc = mean(gain_gc_winner, na.rm = T),
            riders = n()) %>% 
  ungroup() %>%
  
  mutate(perc_rk_corr = percent_rank(correlation),
         perc_rk_err = 1-percent_rank(error),
         perc_rk_winner = 1-percent_rank(winner)) %>%
  
  separate(url, c("j1", "url_race", "j2"), sep = "\\/") %>%
  select(-j1, -j2)

#

dbWriteTable(con, "race_prediction_errors", race_errors, overwrite = TRUE, row.names = FALSE)

#
#
# Recent Performance
#
#

# iterate through a last ten races model for every date

unique_dates <- predicting_all_supp %>%
  select(date) %>%
  unique() %>%
  filter(date > '2016-02-28') %>%
  arrange(desc(date)) %>%
  
  anti_join(dbGetQuery(con, "SELECT DISTINCT date FROM performance_last10races_vsmodel") %>%
              mutate(date = as.Date(date)-1), by = c("date")) %>%
  
  rbind(tibble(date = lubridate::today())) %>%
  arrange(desc(date))

#

for(i in 2:length(unique_dates$date)) {
  
  MAX = unique_dates$date[[i]]
  MIN = MAX - 31
  
  vs_exp <- predicting_all_supp %>%
    filter(between(date, MIN, MAX)) %>%
    
    mutate(weighting = 1 / pred_rank) %>%
    
    group_by(rider) %>%
    filter(date >= (MAX - 30)) %>%
    summarize(exp_rank = sum(log(pred_rank) * weighting, na.rm = T) / sum(weighting, na.rm = T),
              act_rank = sum(log(rnk) * weighting, na.rm = T) / sum(weighting, na.rm = T),
              error_rank = sum((log(rnk) - log(pred_rank)) * weighting, na.rm = T) / sum(weighting, na.rm = T),
              sof = mean(sof, na.rm = T),
              races = n()) %>%
    ungroup() %>%
    
    mutate(ratio_rk = act_rank / exp_rank,
           exp_rank = exp(exp_rank),
           act_rank = exp(act_rank)) %>%
    
    mutate(date = unique_dates$date[[i]]+1)
  
  dbWriteTable(con, "performance_last10races_vsmodel", vs_exp, append = TRUE, row.names = F)
  
  print(i)
  
}

#
#
# Team Fit
#
#

MT <- c("Wanty Gobert", "Sunweb", "Trek", "Sky", "UAE Team", "Movistar", "EF Education First",
        "FDJ", "Mitchelton Scott", "Jumbo Visma", "Israel Startup Nation", "Lotto Soudal", "Cofidis", 
        "Astana", "Alpecin Fenix", "Arkea Samsic", "Bahrain McLaren", "BORA", "AG2R", "Quick Step")

racing_days <- All_data %>%
  filter(year == 2022 & master_team %in% MT) %>%
  select(rider, master_team, date) %>%
  unique()

racing_races <- All_data %>%
  filter(year == 2022 & master_team %in% MT) %>%
  select(rider, url) %>%
  unique()

unique_races <- All_data %>%
  filter(year == 2022) %>%
  group_by(url, race, year, class) %>%
  summarize(date = min(date)) %>%
  ungroup()

racing_days <- racing_days %>%
  inner_join(unique_races) %>%
  select(rider, master_team, date) %>%
  unique()

unique_team_races <- All_data %>%
  filter(year == 2022 & master_team %in% MT) %>%
  select(master_team, race, year, class, url) %>%
  unique() %>%
  inner_join(unique_races)

#

for(x in 1:nrow(unique_races)) {
  
  race_info <- unique_races[x,]
  MIN_DATE = race_info$date - 21
  MAX_DATE = race_info$date + 21
  
  teams_in_race <- unique_team_races %>% inner_join(race_info)
  
  riders_in_range <- racing_days %>%
    filter(between(date, MIN_DATE, MAX_DATE) & master_team %in% teams_in_race$master_team) %>%
    select(rider, master_team) %>%
    unique()
  
  #
  
  Race_Days <- All_data %>%
    select(-date) %>%
    inner_join(race_info %>% select(url, date)) %>%
    filter(time_trial == 0) %>%
    select(stage, race, year, class, date, url, pred_climb_difficulty, length,
           total_vert_gain, cobbles, uphill_finish, sof, one_day_race) %>%
    unique() %>%
    mutate(stage_join = as.character(stage)) %>%
    inner_join(dbGetQuery(con, "SELECT * FROM predictions_stage_bunchsprint") %>%
                 select(-bunch_sprint) %>%
                 select(stage, url, predicted_bs) %>%
                 unique(), by = c("stage_join" = "stage", "url")) %>%
    select(-stage_join) %>%
    
    unique() %>%
    
    inner_join(
      
      dbGetQuery(con, "SELECT DISTINCT date FROM lme4_rider_logranks_sq WHERE test_or_prod = 'prod'") %>%
        mutate(date = as.Date(date)-1), by = c("date")
      
    ) %>%
    
    unique() %>%
    
    mutate(pred_climb_difficulty = ifelse(pred_climb_difficulty < 0, 0, pred_climb_difficulty ^ 2))
  
  #
  
  All_Riders_Predicted <- expand_grid(Race_Days, riders_in_range) %>%
    
    left_join(
      
      dbGetQuery(con, sprintf("SELECT * FROM lme4_rider_logranks_sq WHERE date > '%s' AND date < '%s'", MIN_DATE, MAX_DATE))  %>%
        filter(!is.na(one_day_race)) %>%
        filter(test_or_prod == "prod") %>%
        select(-test_or_prod) %>%
        unique() %>%
        mutate(rider = str_to_title(rider)) %>%
        mutate(date = as.Date(date)-1) %>%
        
        mutate(level_data = ifelse(is.na(sqpcd_impact), "just_rider",
                                   ifelse(is.na(bunchsprint_impact), "pcd_added",
                                          ifelse(is.na(one_day_race), "bs_added", "odr_added")))) %>%
        
        rename(pcd_impact = sqpcd_impact) %>%
        
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
        
        rename(pcd_logrk_impact = pcd_impact,
               bs_logrk_impact = bunchsprint_impact,
               rand_logrk_impact = random_intercept,
               odr_logrk_impact = one_day_race) %>%
        
        mutate(pcd_logrk_impact = ifelse(is.na(pcd_logrk_impact), 0, pcd_logrk_impact),
               bs_logrk_impact = ifelse(is.na(bs_logrk_impact), 0, bs_logrk_impact),
               odr_logrk_impact = ifelse(is.na(odr_logrk_impact), 0, odr_logrk_impact)
        ) %>%
        
        filter(date >= as.Date('2014-01-01')), by = c("rider", "date")
      
    ) %>%
    
    # 0.97 is SD and 3.9 is Mean -- intercept is -0.6 and sof is 1.15 with an avg of 0.36 so -0.2 is left-over
    mutate(pred_rank = exp(-0.2 + ((
      ((rand_logrk_impact + 
          (predicted_bs * bs_logrk_impact) + 
          (one_day_race * odr_logrk_impact) + 
          (pcd_logrk_impact * pred_climb_difficulty))*-1) / 0.97)+3.9))) %>%
    
    select(-rand_logrk_impact,
           -odr_logrk_impact) %>%
    
    mutate(pred_rank = predict(read_rds("Stored models/predicting_rank_adjusting_for_Zscores.rds"), .)) %>%
    mutate(pred_rank = exp(pred_rank)) %>%
    unique() %>%
    
    mutate(pred_rank = ifelse(is.na(pred_rank), median(pred_rank, na.rm = T), pred_rank))
  
  #
  
  OUTPUT <- All_Riders_Predicted %>% 
    group_by(master_team, rider, race, url) %>%
    summarize(mean_pred_rank = exp(mean(log(pred_rank))), 
              pcd_impact = mean(pcd_logrk_impact), 
              bs_impact = mean(bs_logrk_impact)) %>% 
    ungroup() %>% 
    
    left_join(racing_races %>%
                mutate(raced = 1), by = c("rider", "url"))
  
}

#



predicting_all <- All_data %>%
  filter(time_trial == 0) %>%
  select(-points_per_opp, -sof_per_opp, -pred_climb_diff_opp, -final_group) %>%
  mutate(stage_join = as.character(stage)) %>%
  inner_join(dbGetQuery(con, "SELECT * FROM predictions_stage_bunchsprint") %>%
               select(-bunch_sprint) %>%
               select(stage, url, predicted_bs) %>%
               unique(), by = c("stage_join" = "stage", "url")) %>%
  select(-stage_join) %>%
  
  unique() %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT DISTINCT date FROM lme4_rider_logranks_sq WHERE test_or_prod = 'prod'") %>%
      mutate(date = as.Date(date)-1), by = c("date")
    
  ) %>%
  
  unique() %>%
  
  mutate(pred_climb_difficulty = ifelse(pred_climb_difficulty < 0, 0, pred_climb_difficulty ^ 2)) %>%
  
  left_join(
    
    dbReadTable(con, "lme4_rider_logranks_sq")  %>%
      filter(!is.na(one_day_race)) %>%
      filter(test_or_prod == "prod") %>%
      select(-test_or_prod) %>%
      unique() %>%
      mutate(rider = str_to_title(rider)) %>%
      mutate(date = as.Date(date)-1) %>%
      
      mutate(level_data = ifelse(is.na(sqpcd_impact), "just_rider",
                                 ifelse(is.na(bunchsprint_impact), "pcd_added",
                                        ifelse(is.na(one_day_race), "bs_added", "odr_added")))) %>%
      
      rename(pcd_impact = sqpcd_impact) %>%
      
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
      
      rename(pcd_logrk_impact = pcd_impact,
             bs_logrk_impact = bunchsprint_impact,
             rand_logrk_impact = random_intercept,
             odr_logrk_impact = one_day_race) %>%
      
      mutate(pcd_logrk_impact = ifelse(is.na(pcd_logrk_impact), 0, pcd_logrk_impact),
             bs_logrk_impact = ifelse(is.na(bs_logrk_impact), 0, bs_logrk_impact),
             odr_logrk_impact = ifelse(is.na(odr_logrk_impact), 0, odr_logrk_impact)
      ) %>%
      
      filter(date >= as.Date('2014-01-01')), by = c("rider", "date")
    
  ) %>%
  
  # 0.97 is SD and 3.9 is Mean -- intercept is -0.6 and sof is 1.15 with an avg of 0.36 so -0.2 is left-over
  mutate(pred_rank = exp(-0.2 + ((
    ((rand_logrk_impact + 
        (predicted_bs * bs_logrk_impact) + 
        (one_day_race * odr_logrk_impact) + 
        (pcd_logrk_impact * pred_climb_difficulty))*-1) / 0.97)+3.9))) %>%
  
  select(-rand_logrk_impact,
         -odr_logrk_impact) %>%
  
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
      
      filter(date >= as.Date('2014-01-01')), by = c("rider", "date", 'level_data')
    
  ) %>%
  
  # calculate team leader and success predictions using random effects
  mutate(glmer_pred = -3.5 + (random_intercept + 
                                (sqrt(pred_climb_difficulty) * pcd_tmldr_impact) + 
                                (one_day_race * odr_tmldr_impact) + 
                                (predicted_bs * bs_tmldr_impact)),
         glmer_pred = exp(glmer_pred) / (1+exp(glmer_pred))) %>%
  
  select(-random_intercept, -pcd_tmldr_impact, -odr_tmldr_impact, -bs_tmldr_impact) %>%

  unique() %>%

  # give everyone with missing data the median data point
  group_by(stage, race, year, class, level_data) %>%
  mutate(glmer_pred = ifelse(is.na(glmer_pred), median(glmer_pred, na.rm = T), glmer_pred),
         pred_rank = ifelse(is.na(pred_rank), median(pred_rank, na.rm = T), pred_rank)) %>%
  ungroup() %>%
  
  # rank each stat within race
  group_by(stage, race, year, class, level_data) %>%
  mutate(rk_teamldr = rank(-glmer_pred, ties.method = "min"),
         rk_rank = rank(pred_rank, ties.method = "min")) %>%
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
                                 TRUE ~ class))


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

# Sandbox Section ---------------------------------------------------------

predicting_all_supp %>%
  inner_join(dbReadTable(con, "race_level_temperature")) %>%
  group_by(rider) %>% 
  filter(n() >= 60) %>% 
  ungroup() %>% 
  mutate(avg_temperature = avg_temperature - median(avg_temperature, na.rm = T), 
         rnk = log(rnk), 
         pred_rank = log(pred_rank)) %>%  
  lme4::lmer(rnk ~ pred_rank + (1 + avg_temperature | rider), data = .) -> temper_lmer

#
# cobbles
#

pred_cobbles <- lme4::lmer(logrnk ~ logpredrnk + (1 + cobbles | rider),
                           data = predicting_all_supp %>% 
                             mutate(logrnk = log(rnk),
                                    logpredrnk = log(pred_rank)) %>%
                             group_by(rider) %>%
                             filter(sum(cobbles)>0) %>%
                             ungroup())

#

summary(pred_cobbles)

random_intercepts_cobbles <- lme4::ranef(pred_cobbles)[[1]] %>% rownames_to_column()

cobbles_predrnk <- predicting_all_supp %>% 
  filter(cobbles == 1 & year == 2021) %>%
  mutate(logrnk = log(rnk),
         logpredrnk = log(pred_rank)) %>%
  group_by(rider) %>%
  summarize(logpredrnk = mean(logpredrnk),
            races = n()) %>%
  ungroup()

#
# Recent performances
#

predicting_all_supp %>%
  
  filter(date > '2022-07-30' & date < '2022-09-27') %>%
  
  unique() %>%
  
  select(rider, pred_rank, rnk, sof, race, stage, year, class, date, length, url, time_trial) %>%
  
  #rbind(predicting_TT %>%  filter(date > '2022-06-30' & date < '2022-12-01') %>%
  #        select(rider, pred_rank, rnk, sof, race, stage, year, class, date, length, url, time_trial)) %>%
  
  group_by(rider) %>%
  summarize(#exp_points = sum(pred_points, na.rm = T),
            #act_points = sum(points_finish, na.rm = T),
            exp_rank = mean(log(pred_rank), na.rm = T),
            act_rank = mean(log(rnk), na.rm = T),
            error_rank = mean((log(rnk) - log(pred_rank)), na.rm = T),
            sof_level = mean(sof, na.rm = T),
            distinct_races = n_distinct(race),
            races = n()) %>%
  ungroup() %>%
  
  mutate(ratio_rk = act_rank / exp_rank,
         exp_rank = exp(exp_rank),
         act_rank = exp(act_rank)) -> recent_performances

#

# predict quality of winners

# difficulty of final 20 - 30 km might be a good indication of breakaway likelihood

race_errors %>% lm(winner ~ pred_climb_difficulty + predicted_bs + class + grand_tour + one_day_race, data = .) -> pred_w_qual

cbind(race_errors, pred = predict(pred_w_qual, race_errors)) -> qual_predictions

#

train_set_brk <- read_csv("breakaway-training-set.csv") %>%
  mutate(stage = as.character(stage)) %>%
  inner_join(race_errors, by = c("stage", "race", 'year'))

train_set_brk %>% glm(breakaway ~ winner + winner_gain_gc, data = ., family = "binomial") -> breakaway_preds

summary(breakaway_preds)

cbind(race_errors, pred = predict(breakaway_preds,race_errors)) %>% 
  mutate(pred = exp(pred)/(1+exp(pred))) %>% 
  anti_join(train_set_brk, by = c("stage", "race", "year")) -> preds_break

preds_break %>%
  select(stage, race, year, breakaway = pred) %>%
  mutate(breakaway = ifelse(breakaway >= 0.5, 1, 0)) %>%
  rbind(read_csv("breakaway-training-set.csv") %>%
          mutate(stage = as.character(stage))) %>%
  
  inner_join(race_errors, by = c("stage", "race", 'year')) %>%
  
  filter(!is.na(winner)) %>%
  filter(one_day_race == 0) -> breakaway_model_data

breakaway_model_data %>%
  group_by(grand_tour) %>% 
  summarize(breakaway = mean(breakaway, na.rm = T), 
            stages = n()) %>% 
  ungroup()

breakaway_model_data %>%  

  glm(breakaway ~ pred_climb_difficulty * predicted_bs + grand_tour + uphill_finish, data = ., family = "binomial") -> breakaway_glm

write_rds(breakaway_glm, "Stored models/breakaway_glm.rds")

#
# iterate through a last ten races model for every date
#

unique_dates <- predicting_all_supp %>%
  select(date) %>%
  unique() %>%
  filter(date > '2019-02-28') %>%
  arrange(desc(date)) %>%
  
  anti_join(dbGetQuery(con, "SELECT DISTINCT date FROM performance_last10races_vsmodel") %>%
              mutate(date = as.Date(date)), by = c("date")) %>%
  
  rbind(tibble(date = lubridate::today())) %>%
  arrange(desc(date))

#

for(i in 2:length(unique_dates$date)) {
  
  MAX = unique_dates$date[[i]] - 1
  MIN = MAX - 366

  vs_exp <- predicting_all_supp %>%
    filter(between(date, MIN, MAX)) %>%
    
    mutate(weighting = 1 / pred_rank) %>%
    
    group_by(rider) %>%
    filter(date >= (MAX - 29)) %>%
    summarize(exp_rank = sum(log(pred_rank) * weighting, na.rm = T) / sum(weighting, na.rm = T),
              act_rank = sum(log(rnk) * weighting, na.rm = T) / sum(weighting, na.rm = T),
              error_rank = sum((log(rnk) - log(pred_rank)) * weighting, na.rm = T) / sum(weighting, na.rm = T),
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
# link with recent performance
#

predicting_all_with_recent <- predicting_all_supp %>%
  
  select(-pred_climb_diff_succ,
         -level_data, -leader_rating, -points_finish, -success) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, date, (act_leader - exp_leader) as rel_leader, 
    exp_leader, act_leader, exp_rank, act_rank, error_rank, races, days_of_comps,
    bunch_sprint
               FROM performance_last10races_vsmodel") %>%
      mutate(date = as.Date(date)), by = c('rider', 'date', 'bunch_sprint')
    
  ) %>%
  
  mutate(error_rank = ifelse(is.na(error_rank), 0, error_rank),
         rel_leader = ifelse(is.na(rel_leader), 0, rel_leader),
         races = ifelse(is.na(races), 0, races),
         days_of_comps = ifelse(is.na(days_of_comps), 0, days_of_comps)) %>%
  
  # center error rank and adjust for <10 races
  mutate(error_rank = error_rank / (1 / races) * 0.1) %>%
  
  group_by(stage, race, year, class) %>%
  mutate(error_rank = error_rank - mean(error_rank, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(pred_break_win = predict(read_rds("Stored models/breakaway_glm.rds"), .)) %>%
  
  mutate(pred_break_win = exp(pred_break_win)/(1+exp(pred_break_win)))
  
#
# Predicting team leader
#

predicting_all_with_recent %>% 

  # filter non-teams
  filter(!class %in% c("WC", 'CC', 'NC'))  %>%
  filter(!team == "") %>% 
  
  # aggregate
  group_by(teamldr_within_team) %>% 
  summarize(actual = mean(tm_pos == 1, na.rm = T),
            shrunk = mean(shrunk_teamldr, na.rm = T), 
            n =n())

#
#
#

pick <- sample(predicting_all$date %>% unique(), 650)

predicting_all %>%
  # filter non-teams
  filter(!class %in% c("WC", 'CC', 'NC'))  %>%
  filter(!team == "") %>% 
  #filter(date %in% pick) %>%
  filter(!is.na(shrunk_teamldr)) -> tmldr_train_data

justrider <- glm(team_ldr ~ shrunk_teamldr + pred_rank + pred_points + rk_teamldr,
                 data = tmldr_train_data %>% filter(level_data == "just_rider"), 
                 family = "binomial")

pcdadded <- glm(team_ldr ~ shrunk_teamldr + pred_rank + pred_points + rk_teamldr,
                 data = tmldr_train_data %>% filter(level_data == "pcd_added"), 
                 family = "binomial")

BSadded <- glm(team_ldr ~ shrunk_teamldr + pred_rank + pred_points + rk_teamldr,
                data = tmldr_train_data %>% filter(level_data == "bs_added"), 
                family = "binomial")

ODRadded <- glm(team_ldr ~ shrunk_teamldr + pred_rank + pred_points + rk_teamldr,
                data = tmldr_train_data %>% filter(level_data == "odr_added"), 
                family = "binomial")

#

pred_tmldr = rbind(
  
  cbind(
    
    pred = predict(BSadded, 
                   predicting_all %>% 
                     unique() %>%
                     filter(level_data == "bs_added") %>%
                     filter(!date %in% pick)),
    
    predicting_all %>% 
      unique() %>%
      filter(level_data == "bs_added") %>%
      filter(!date %in% pick))) %>%
  
  mutate(pred = exp(pred)/(1+exp(pred)),
         leader = tm_pos==1)

# brier score for accuracy
brierScore <- pred_tmldr %>%
  
  group_by(level_data) %>%
  summarize(BrierScore = mean((pred-leader)^2, na.rm = T),
            n=n()) %>%
  ungroup()

# all preds for utility

ALL_pred_tmldr <- cbind(
    
    pred = predict(BSadded, 
                   predicting_all %>% 
                     unique() %>%
                     filter(level_data == "bs_added")),
    
    predicting_all %>% 
      unique() %>%
      filter(level_data == "bs_added")) %>%
  
  mutate(pred = exp(pred)/(1+exp(pred)),
         leader = tm_pos==1)

#
#
#


# predict test set out of sample
pred <- gbm_predict_TMLDR %>% 
  
  mutate(pred = exp(pred)/(1+exp(pred)),
         leader = tm_pos==1) %>% 
  
  group_by(team, race, year, stage) %>% 
  mutate(pred_leader = ifelse(pred == max(pred, na.rm = T), 1, 0), 
         compared = pred / max(pred, na.rm = T),
         rk_ldr = rank(-pred, ties.method = "first"), 
         n=n()) %>% 
  ungroup()

# accuracy matrix

pred %>% 
  group_by(team_ldr, rk_ldr) %>% 
  count() %>%
  ungroup() %>%
  group_by(rk_ldr) %>% 
  mutate(perc = n / sum(n)) %>%
  ungroup() %>%
  filter(team_ldr == 1)

pred %>% 
  group_by(team_ldr, pred_leader) %>% 
  count() %>%
  ungroup() -> predictedldr_vs_leader

pred %>%
  
  group_by(f = floor(pred / 0.05) * 0.05, team_ldr) %>%
  count() %>%
  ungroup() -> pred_str_vs_leader

# brier score for accuracy
brierScore <- mean((pred$pred-pred$leader)^2, na.rm = T)

print(brierScore)

# ROC / AUC
roc_obj <- pROC::roc(pred$leader, pred$pred)
print(pROC::auc(roc_obj))

pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
  labs(title = "Predict team leader", subtitle = pROC::auc(roc_obj))

#
#
#

library(xgboost)

xgb.train <- xgb.DMatrix(
  
  data = as.matrix(predicting_all %>%
                     filter(year < 2020) %>%
                     select(rk_teamldr, rk_points, rk_rank, rk_success, rk_succwhenopp)),
  
  label = predicting_all %>%
    filter(year < 2020) %>%
    mutate(tmldr = tm_pos == 1) %>%
    select(tmldr) %>%
    .[[1]]
  
)

# test

xgb.test <- xgb.DMatrix(
  
  data = as.matrix(predicting_all %>%
                     filter(year >= 2020) %>%
                     unique() %>%
                     select(rk_teamldr, rk_points, rk_rank, rk_success, rk_succwhenopp)),
  
  label = predicting_all %>%
    filter(year >= 2020) %>%
    unique() %>%
    mutate(tmldr = tm_pos == 1) %>%
    select(tmldr) %>%
    .[[1]]
  
)

# outline parameters

params <- list(
  
  booster = "gbtree",
  eta = 0.3,
  max_depth = 3,
  gamma = 0,
  subsample = 1,
  colsample_bytree = 1,
  tree_method = "hist",
  objective = "binary:logistic"
  
)

# run xgboost model

gbm_model <- xgb.train(params = params,
                       data = xgb.train,
                       nrounds = 10000,
                       nthreads = 4,
                       early_stopping_rounds = 1000,
                       watchlist = list(val1 = xgb.train,
                                        val2 = xgb.test),
                       verbose = 0)

#
#
# xgb Importance

xgb.importance(model = gbm_model)

gbm_model$best_score

#

gbm_predict_TMLDR = cbind(
  
  pred = predict(gbm_model, 
                 as.matrix(predicting_all %>%
                             filter(year >= 2020) %>%
                             filter(!class %in% c("CC", 'NC', 'WC')) %>% 
                             unique() %>%
                             select(rk_teamldr, rk_points, rk_rank, rk_success, rk_succwhenopp), reshape=T)),
  
  predicting_all %>%
    filter(year >= 2020) %>%
    filter(!class %in% c("CC", 'NC', 'WC')) %>% 
    unique() %>%
    select(tm_pos, rk_teamldr, rk_points, rk_rank, rk_success, rk_succwhenopp))

# predict test set out of sample
pred <- gbm_predict_TMLDR %>% 
  
  mutate(leader = tm_pos==1)

# brier score for accuracy
brierScore <- mean((pred$pred-pred$leader)^2)

print(brierScore)

# ROC / AUC
roc_obj <- pROC::roc(pred$leader, pred$pred)
print(pROC::auc(roc_obj))

pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
  labs(title = "Predict team leader", subtitle = pROC::auc(roc_obj))

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

glm(win ~ 
      log_pred_rank + 
      rel_age +                       # age vs race average (lower is younger)
      rel_leader +                    # team ldr recent vs expected
      rel_tm_RANK:predicted_bs +      # gives extra to clear team leaders in bunch_sprints
      
      pred_break_win:log_pred_rank +  # gives extra to lower rated riders in breakaways
      error_rank,                     # recent performances errors (higher is worse recent performance)
    
    family = "binomial", 
    data = predicting_all_with_recent %>%
      mutate(log_pred_rank = log(pred_rank)) %>%
      mutate(win = ifelse(rnk == 1, 1, 0)) %>%
      
      group_by(team, race, stage, year, class, length, date) %>%
      mutate(rel_tm_RANK = ((sum(log_pred_rank)-log_pred_rank) / (n()-1)) - log_pred_rank) %>%
      ungroup() %>%
      
      filter(year < 2021) %>%
      filter(!class %in% c("NC", "CC", "WC"))
) -> win_mod

summary(win_mod)

write_rds(win_mod, "Stored models/final_win_prob_model.rds")

gbm_predict_WIN = cbind(
  
  pred = predict(win_mod, 
                 predicting_all_with_recent %>%
                   unique() %>%
                   mutate(log_pred_rank = log(pred_rank)) %>%
                   
                   group_by(team, race, stage, year, class, length, date) %>%
                   mutate(rel_tm_RANK = ((sum(log_pred_rank)-log_pred_rank) / (n()-1)) - log_pred_rank) %>%
                   ungroup() %>%
                   
                   filter(year >= 2021)),
  
  predicting_all_with_recent %>%
    unique() %>%
    mutate(log_pred_rank = log(pred_rank)) %>%
    
    group_by(team, race, stage, year, class, length, date) %>%
    mutate(rel_tm_RANK = ((sum(log_pred_rank)-log_pred_rank) / (n()-1)) - log_pred_rank) %>%
    ungroup() %>%
    
    filter(year >= 2021))

# predict test set out of sample
pred <- gbm_predict_WIN %>% 
  
  mutate(pred = exp(pred)/(1+exp(pred)),
         win = rnk==1) %>%
  
  group_by(stage, race, year, class) %>%
  mutate(pred = pred / sum(pred, na.rm = T)) %>%
  ungroup()

# brier score for accuracy
brierScore <- mean((pred$pred-pred$win)^2, na.rm = T)

print(brierScore)

# ROC / AUC
roc_obj <- pROC::roc(pred$win, pred$pred)
print(pROC::auc(roc_obj))

# by percentage chance

pred %>% group_by(fl = floor(pred / 0.03)*0.03) %>% summarize(RMSE = sqrt(mean((win-pred)^2)), SAMPLE = n())

pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
  labs(title = "Predict winner", subtitle = pROC::auc(roc_obj))

#
#
#
#

library(xgboost)

xgb.train <- xgb.DMatrix(
  
  data = as.matrix(predicting_all_with_recent %>% 
                     mutate(win = ifelse(rnk == 1, 1, 0),
                            ppwo = pred_pointswhenopp * -1) %>%
                     
                     filter(year < 2020) %>%
                     filter(!class %in% c("NC", "CC", "WC")) %>%
                     select(log_rk_pwo,
                              pred_pointswhenopp,
                              rel_age,
                              rel_leader,
                              log_rk_pwo,
                              pred_break_win,
                              ppwo,
                              error_rank)),
  
  label = predicting_all %>%
    filter(year < 2020) %>%
    filter(!class %in% c("NC", "CC", "WC")) %>%
    mutate(win = rnk == 1) %>%
    select(win) %>%
    .[[1]]
  
)

# test

xgb.test <- xgb.DMatrix(
  
  data = as.matrix(predicting_all_with_recent %>% 
                     mutate(win = ifelse(rnk == 1, 1, 0),
                            ppwo = pred_pointswhenopp * -1) %>%
                     
                     filter(year >= 2020) %>%
                     filter(!class %in% c("NC", "CC", "WC")) %>%
                     select(log_rk_pwo,
                            pred_pointswhenopp,
                            rel_age,
                            rel_leader,
                            log_rk_pwo,
                            pred_break_win,
                            ppwo,
                            error_rank)),
  
  label = predicting_all %>%
    filter(year >= 2020) %>%
    filter(!class %in% c("NC", "CC", "WC")) %>%
    mutate(win = rnk == 1) %>%
    select(win) %>%
    .[[1]]
  
)

# outline parameters

params <- list(
  
  booster = "gbtree",
  eta = 0.1,
  max_depth = 10,
  subsample = 1,
  colsample_bytree = 1,
  tree_method = "hist",
  objective = "binary:logistic"
  
)

# run xgboost model

gbm_model <- xgb.train(params = params,
                       data = xgb.train,
                       nrounds = 10000,
                       nthreads = 4,
                       early_stopping_rounds = 500,
                       watchlist = list(val1 = xgb.train,
                                        val2 = xgb.test),
                       verbose = 1)

#
#
# xgb Importance

xgb.importance(model = gbm_model)

gbm_model$best_score

#

gbm_predict_WIN = cbind(
  
  pred = predict(gbm_model, 
                 as.matrix(predicting_all_with_recent %>% 
                             mutate(win = ifelse(rnk == 1, 1, 0),
                                    ppwo = pred_pointswhenopp * -1) %>%
                             
                             filter(year >= 2020) %>%
                             filter(!class %in% c("NC", "CC", "WC")) %>%
                             select(log_rk_pwo,
                                    pred_pointswhenopp,
                                    rel_age,
                                    rel_leader,
                                    log_rk_pwo,
                                    pred_break_win,
                                    ppwo,
                                    error_rank), reshape=T)),
  
  predicting_all_with_recent %>% 
    mutate(win = ifelse(rnk == 1, 1, 0),
           ppwo = pred_pointswhenopp * -1) %>%
    
    filter(year >= 2020) %>%
    filter(!class %in% c("NC", "CC", "WC")))

# predict test set out of sample
pred <- gbm_predict_WIN %>% 
  
  mutate(win = rnk==1)

# brier score for accuracy
brierScore <- mean((pred$pred-pred$win)^2)

print(brierScore)

# ROC / AUC
roc_obj <- pROC::roc(pred$win, pred$pred)
print(pROC::auc(roc_obj))

pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
  labs(title = "Predict team leader", subtitle = pROC::auc(roc_obj))



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


indiv_race_level_weighted_stats <- predicting_all_supp %>%

  group_by(race, year, class, stage, date, length) %>%
  mutate(log_actual_rnk = 1 / (log(rnk)+0.5),
         log_actual_rnk = log_actual_rnk - min(log_actual_rnk, na.rm = T),
         lar_top10 = ifelse(rnk <= 10, log_actual_rnk, 0)) %>%
  ungroup() %>%
  
  group_by(race, year, class, stage, date, length, pred_climb_difficulty, predicted_bs) %>%
  summarize(races = n_distinct(race, year),
            pcd_impact = sum(pcd_logrk_impact * log_actual_rnk, na.rm = T) / sum(log_actual_rnk, na.rm = T),
            bs_impact = sum(bs_logrk_impact * log_actual_rnk, na.rm = T) / sum(log_actual_rnk, na.rm = T),
            pcd_impact_t10 = sum(pcd_logrk_impact * lar_top10, na.rm = T) / sum(lar_top10, na.rm = T),
            bs_impact_t10 = sum(bs_logrk_impact * lar_top10, na.rm = T) / sum(lar_top10, na.rm = T),
            pcd_impact_field = mean(pcd_logrk_impact, na.rm = T),
            bs_impact_field = mean(bs_logrk_impact, na.rm = T),
            sof = mean(sof, na.rm = T)) %>%
  ungroup()

#
#
#


race_level_weighted_stats <- predicting_all_supp %>%
  
  filter(one_day_race == 1 | str_sub(class,1,1) == "1") %>%
  
  separate(url, c("j1", "url_race", "j2"), sep = "\\/") %>%

  group_by(race, year, class) %>%
  mutate(log_actual_rnk = 1 / (log(rnk)+0.5),
         log_actual_rnk = log_actual_rnk - min(log_actual_rnk, na.rm = T),
         lar_top10 = ifelse(rnk <= 10, log_actual_rnk, 0)) %>%
  ungroup() %>%
  
  mutate(url_race = case_when(url_race == "oxyclean-classic-brugge-de-panne" ~ "driedaagse-vd-panne",
                              TRUE ~ url_race)) %>%
  
  group_by(url_race) %>%
  summarize(races = n_distinct(race, year),
            predicted_BS = mean(predicted_bs, na.rm = T),
            PCD = mean(pred_climb_difficulty, na.rm = T),
            pcd_impact = sum(pcd_logrk_impact * log_actual_rnk, na.rm = T) / sum(log_actual_rnk, na.rm = T),
            bs_impact = sum(bs_logrk_impact * log_actual_rnk, na.rm = T) / sum(log_actual_rnk, na.rm = T),
            pcd_impact_t10 = sum(pcd_logrk_impact * lar_top10, na.rm = T) / sum(lar_top10, na.rm = T),
            bs_impact_t10 = sum(bs_logrk_impact * lar_top10, na.rm = T) / sum(lar_top10, na.rm = T),
            pcd_impact_field = mean(pcd_logrk_impact, na.rm = T),
            bs_impact_field = mean(bs_logrk_impact, na.rm = T),
            sof = mean(sof, na.rm = T)) %>%
  ungroup() %>%
  
  inner_join(predicting_all_supp %>%
               
               filter(one_day_race == 1 | str_sub(class,1,1) == "1") %>%
               
               separate(url, c("j1", "url_race", "j2"), sep = "\\/") %>%
               
               mutate(url_race = case_when(url_race == "oxyclean-classic-brugge-de-panne" ~ "driedaagse-vd-panne",
                                           TRUE ~ url_race)) %>%
               
               group_by(url_race) %>%
               filter(rank(url_race, ties.method = "first") == 1) %>%
               ungroup() %>%

               select(url_race, race, class), by = c("url_race"))

#

ggplot(race_level_weighted_stats %>% 
         filter(races >= 3), 
       aes(x = bs_impact, y = pcd_impact, label = url_race))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_text()+
  scale_x_reverse()+
  scale_y_reverse()

#

predicting_all_supp %>%
  
  filter(one_day_race == 1 | str_sub(class,1,1) == "1") %>%
  
  separate(url, c("j1", "url_race", "j2"), sep = "\\/") %>%
  
  filter(url_race %in% c("gp-d-ouverture", "16th-challenge-volta-a-mallorca",
                         "trofeo-andratx-mirador-d-es-colomer", "deia-trophy",
                         "clasica-de-almeria")) %>%
  
  mutate(lar_top10 = ifelse(rnk <= 5, 1, NA)) %>%
  
  mutate(url_race = case_when(url_race == "oxyclean-classic-brugge-de-panne" ~ "driedaagse-vd-panne",
                              TRUE ~ url_race)) %>%

  
  filter(!is.na(lar_top10)) %>%
  
  mutate(RaceName = case_when(url_race == "gp-d-ouverture" ~ "GP Marseille",
                              url_race == "deia-trophy" ~ "Trofeo Serra Tramuntana",
                              url_race == "trofeo-andratx-mirador-d-es-colomer" ~ "Trofeo Pollenca",
                              url_race == "16th-challenge-volta-a-mallorca" ~ "Trofeo Palma",
                              url_race == "clasica-de-almeria" ~ "Clasica Almeria",
                              TRUE ~ "x")) %>%
  
  ggplot(aes(x = bs_logrk_impact*-1, y = pcd_logrk_impact*-1))+
  
  stat_density_2d(
    geom = "raster",
    aes(fill = after_stat(ndensity)),
    contour = FALSE
  )+
  geom_hline(yintercept = 0, color = "black")+
  geom_vline(xintercept = 0, color = "black")+
  facet_wrap(~RaceName)+
  scale_fill_gradientn(colours = c("transparent", "#F9C968", "#FC9045", "#FF7F27", "#F74B00"),
                       guide = FALSE)+
  
  labs(x = "Bunch Sprint Ability",
       y = "Climbing Ability",
       title = "What Skills Top 5 Finishers Possess")+
  
  theme(panel.background = element_rect(fill = "gray90"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray60"))

#

latest_modeled_riders <- dbGetQuery(con, "SELECT * FROM lme4_rider_logranks WHERE Date = '2021-10-24'")

latest_modeled_riders %>% 
  mutate(BS = exp(random_intercept + 4 + (1*pcd_impact)+bunchsprint_impact), 
         MT = exp(random_intercept + 4 + (15 * pcd_impact)), 
         CLA = exp(random_intercept + 4 + (4.5 * pcd_impact))) %>% 
  rowwise() %>% 
  mutate(best = min(MT, BS, CLA)) %>% 
  ungroup() %>% 
  filter(best <= 15) %>% 
  
  inner_join(predicting_all_supp %>%
               filter(year == 2021) %>% 
               group_by(rider) %>%
               summarize(sof = mean(sof, na.rm = T),
                         races =n()) %>%
               ungroup() %>% 
               filter(sof > 0.3 & races > 19), by = c("rider")) %>%
  
  mutate(pcd_impact = pcd_impact / sd(pcd_impact), 
         bunchsprint_impact = bunchsprint_impact / sd(bunchsprint_impact)) %>% 
  
  ggplot(aes(x = bunchsprint_impact*-1,
             y = pcd_impact*-1, 
             label = rider, 
             color = (rider %in% c("Hayter Ethan", "Pidcock Thomas"))))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept=0)+
  geom_text()+
  guides(color = F)+
  scale_color_manual(values = c("gray50", "black"))+
  labs(x = "Bunch Sprint Impact", y = "Climbing Impact", title = 'Skill distributions of top riders')

#

who_matches <- race_level_weighted_stats %>%
  
  filter(races >= 4) %>%
  
  mutate(M = 1) %>%
  rename(race_pcd = pcd_impact,
         race_bs = bs_impact) %>%
  
  inner_join(latest_modeled_riders %>% mutate(M = 1), by = c("M")) %>%
  
  mutate(pcd_gap = (pcd_impact - race_pcd),
         bs_gap = (bunchsprint_impact - race_bs),
         pcd_gap = abs(pcd_gap) / sd(pcd_gap, na.rm = T),
         bs_gap = abs(bs_gap) / sd(bs_gap, na.rm = T),
         combined = (pcd_gap + bs_gap) / 2 * exp(random_intercept+4.3)) %>%
  
  group_by(url_race) %>%
  mutate(rk = rank(combined, ties.method = "min")) %>%
  ungroup()

#
#
#
#
#
#
#
#
#

predict_WIN_full = cbind(
  
  pred = predict(win_mod, 
                 predicting_all_with_recent %>%
                   unique() %>%
                   mutate(log_pred_rank = log(pred_rank)) %>%
                   
                   group_by(team, race, stage, year, class, length, date) %>%
                   mutate(rel_tm_RANK = ((sum(log_pred_rank)-log_pred_rank) / (n()-1)) - log_pred_rank) %>%
                   ungroup() %>%
                   
                   filter(year >= 2017)),
  
  predicting_all_with_recent %>%
    unique() %>%
    mutate(log_pred_rank = log(pred_rank)) %>%
    
    group_by(team, race, stage, year, class, length, date) %>%
    mutate(rel_tm_RANK = ((sum(log_pred_rank)-log_pred_rank) / (n()-1)) - log_pred_rank) %>%
    ungroup() %>%
    
    filter(year >= 2017)) %>%
  
  mutate(pred = exp(pred)/(1+exp(pred)),
         win = rnk==1) %>%
  
  group_by(stage, race, year, class, date, length) %>%
  mutate(pred = pred / sum(pred, na.rm = T)) %>%
  ungroup()

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
#
#
#
#
#
#

time_gap_model <- predicting_all_supp %>%
  
  filter(pred_climb_difficulty >= 7.5) %>%
  
  mutate(logtimegap = log10(total_seconds - win_seconds + 1),
         logtimegap = ifelse(rnk == 200, 6.91, logtimegap),
         log_rank = log(pred_rank)) %>%
  
  lm(logtimegap ~ log_rank * pred_climb_difficulty, data = .)

#

summary(time_gap_model)

#

predictions <- expand_grid(log_rank = seq(0,6,0.5),
                           pred_climb_difficulty = seq(7.5,22.5,2.5)) %>%
  
  mutate(predicted_time_gap = 10 ^ predict(time_gap_model, .),
         predicted_time_gap = round(predicted_time_gap),
         predicted_time_gap = ifelse(predicted_time_gap <= 3600, predicted_time_gap, 3600))

#

actual_predictions <- predicting_all_supp %>%
  
  filter(pred_climb_difficulty >= 7.5) %>%
  
  mutate(time_gap = total_seconds - win_seconds,
         log_rank = log(pred_rank)) %>%
  
  mutate(predicted_time_gap = 10 ^ predict(time_gap_model, .),
         predicted_time_gap = round(predicted_time_gap),
         predicted_time_gap = ifelse(predicted_time_gap <= 3600, predicted_time_gap, 3600),
         
         rel_time_gap = predicted_time_gap - time_gap)

#

examples <- actual_predictions %>%
  
  filter(year == 2021 & ((stage == 8 & race == "tour de france") |
                           (stage == 9 & race == "giro d'italia") |
                           (race == "il lombardia"))) %>%
  mutate(RK = exp(log_rank),
         RK = case_when(RK <= 5 ~ 5,
                        RK <= 10 ~ 10,
                        RK <= 20 ~ 20,
                        RK <= 40 ~ 40,
                        RK <= 60 ~ 60,
                        RK <= 100 ~ 100,
                        RK <= 150 ~ 150,
                        TRUE ~ 200))

actual_predictions %>%
  
  mutate(time_gap = ifelse(time_gap <= 3600, time_gap, 3600),
         log_timegap = log(time_gap+1)) %>%
  
  mutate(RK = exp(log_rank),
         RK = case_when(RK <= 5 ~ 5,
                        RK <= 10 ~ 10,
                        RK <= 20 ~ 20,
                        RK <= 40 ~ 40,
                        RK <= 60 ~ 60,
                        RK <= 100 ~ 100,
                        RK <= 150 ~ 150,
                        TRUE ~ 200)) %>% 
  
  mutate(time_gap_bin = case_when(time_gap == 0 ~ 0,
                                  time_gap <= 5 ~ 5,
                                  time_gap <= 20 ~ 20,
                                  time_gap <= 60 ~ 60,
                                  time_gap <= 300 ~ 300,
                                  time_gap <= 600 ~ 600,
                                  time_gap <= 1200 ~ 1200,
                                  TRUE ~ 2400)) %>% 
  
  group_by(RK) %>%
  filter(n() > 100) %>%
  mutate(total = n()) %>%
  ungroup() %>% 
  
  group_by(RK, time_gap_bin) %>%
  summarize(results = n(),
            total = max(total)) %>%
  ungroup() %>%
  
  mutate(perc = results/total) %>%
  
  ggplot(aes(x = as.factor(time_gap_bin), y = perc))+
  
  geom_col()+
  facet_wrap(~RK, ncol = 2, scales = "free_y")+
  
  labs(title = "Distribution of time lost by projected RK in race",
       x = "time gap to winner (seconds)",
       y = "Percentage of races")+
  
  scale_y_continuous(labels = scales::percent)

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
#
#
#
#

predicting_all_supp <- predicting_all_supp %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, date, error_rank, races
               FROM performance_last10races_vsmodel") %>%
      mutate(date = as.Date(date)), by = c('rider', 'date')
    
  ) %>%
  
  mutate(error_rank = ifelse(is.na(error_rank), 0, error_rank),
         races = ifelse(is.na(races), 0, races)) %>%
  
  # center error rank and adjust for <10 races
  mutate(error_rank = error_rank / (1 / races) * 0.1) %>%
  
  group_by(stage, race, year, class, date, length) %>%
  mutate(error_rank = error_rank - mean(error_rank, na.rm = T)) %>%
  ungroup() %>%
  
  rename(recent_races = races)

# Bring in GC stage by stage win probs
# set to 0 for ODRs

gc_probabilities <- dbGetQuery(con, "SELECT rider, stage, url, seconds_behind_contenders, total_seconds_back, model_winprob, virtual_gc
                               FROM stage_by_stage_gc_winprob")

#

predicting_all_supp <- predicting_all_supp %>%
  
  left_join(gc_probabilities, by = c("rider", "url", "stage")) %>%
  
  mutate(model_winprob = ifelse(one_day_race == 1, 0.01, model_winprob),
         total_seconds_back = ifelse(one_day_race == 1, 0, total_seconds_back)) %>%
  
  filter(!is.na(model_winprob))

# Build Win Model ---------------------------------------------------------

library(xgboost)

xgb.train <- xgb.DMatrix(
  
  data = as.matrix(predicting_all_supp %>% 
                     mutate(win = ifelse(rnk == 1, 1, 0)) %>%
                     
                     mutate(pred_break = predict(read_rds("Stored models/breakaway-prediction-model.rds"), 
                                                 as.matrix(predicting_all_supp %>%
                                                             
                                                             group_by(race, year, url) %>%
                                                             mutate(perc_thru = as.numeric(stage)/max(as.numeric(stage))) %>%
                                                             ungroup() %>%
                                                             
                                                             select(predicted_bs,
                                                                    grand_tour,
                                                                    pred_climb_difficulty,
                                                                    length,
                                                                    uphill_finish,
                                                                    perc_thru,
                                                                    total_vert_gain)))) %>%
                     
                     filter(year < 2022) %>%
                     filter(!class %in% c("NC", "CC", "WC")) %>%
                     mutate(pred_break = ifelse(one_day_race == 1, 0.01, pred_break)) %>%
                     select(pred_rank,
                            model_winprob,
                            rel_age,
                            error_rank,
                            recent_races,
                            shrunk_teamldr,
                            total_seconds_back,
                            predicted_bs,
                            pred_climb_difficulty,
                            sof,
                            pred_break,
                            one_day_race,
                            grand_tour,
                            uphill_finish)),
  
  label = predicting_all_supp %>%
    filter(year < 2022) %>%
    filter(!class %in% c("NC", "CC", "WC")) %>%
    mutate(win = rnk == 1) %>%
    select(win) %>%
    .[[1]]
  
)

# test

xgb.test <- xgb.DMatrix(
  
  data = as.matrix(predicting_all_supp %>% 
                     mutate(win = ifelse(rnk == 1, 1, 0)) %>%
                     
                     mutate(pred_break = predict(read_rds("Stored models/breakaway-prediction-model.rds"), 
                                                 as.matrix(predicting_all_supp %>%
                                                             
                                                             group_by(race, year, url) %>%
                                                             mutate(perc_thru = as.numeric(stage)/max(as.numeric(stage))) %>%
                                                             ungroup() %>%
                                                             
                                                             select(predicted_bs,
                                                                    grand_tour,
                                                                    pred_climb_difficulty,
                                                                    length,
                                                                    uphill_finish,
                                                                    perc_thru,
                                                                    total_vert_gain)))) %>%
                     
                     filter(year >= 2022) %>%
                     filter(!class %in% c("NC", "CC", "WC")) %>%
                     mutate(pred_break = ifelse(one_day_race == 1, 0.01, pred_break)) %>%
                     select(pred_rank,
                            model_winprob,
                            rel_age,
                            error_rank,
                            recent_races,
                            shrunk_teamldr,
                            total_seconds_back,
                            predicted_bs,
                            pred_climb_difficulty,
                            sof,
                            pred_break,
                            one_day_race,
                            grand_tour,
                            uphill_finish)),
  
  label = predicting_all_supp %>%
    filter(year >= 2022) %>%
    filter(!class %in% c("NC", "CC", "WC")) %>%
    mutate(win = rnk == 1) %>%
    select(win) %>%
    .[[1]]
  
)

# outline parameters

params <- list(
  
  booster = "gbtree",
  eta = 0.3,
  max_depth = 8,
  gamma = 0,
  subsample = 1,
  colsample_bytree = 1,
  tree_method = "hist",
  objective = "binary:logistic"
  
)

# run xgboost model

gbm_model <- xgb.train(params = params,
                       data = xgb.train,
                       nrounds = 6000,
                       monotone_constraints = c(-1, 0, -1, -1, 0, 1, 0, 0, 0, 0, 0, 0, 0),
                       early_stopping_rounds = 2000,
                       watchlist = list(val1 = xgb.train,
                                        val2 = xgb.test),
                       verbose = 0)

#
#
# xgb Importance

xgb.importance(model = gbm_model)

gbm_model$best_score

#

#write_rds(gbm_model, "Stored models/very-basic-logrank-xgboost-win.rds")
write_rds(gbm_model, "Stored models/more-advanced-logrank-xgboost-win.rds")

#

gbm_predict_WIN = cbind(
  
  pred = predict(gbm_model, 
                 as.matrix(predicting_all_supp %>% 
                             mutate(win = ifelse(rnk == 1, 1, 0)) %>%
                             
                             mutate(pred_break = predict(read_rds("Stored models/breakaway-prediction-model.rds"), 
                                                         as.matrix(predicting_all_supp %>%
                                                                     
                                                                     group_by(race, year, url) %>%
                                                                     mutate(perc_thru = as.numeric(stage)/max(as.numeric(stage))) %>%
                                                                     ungroup() %>%
                                                                     
                                                                     select(predicted_bs,
                                                                            grand_tour,
                                                                            pred_climb_difficulty,
                                                                            length,
                                                                            uphill_finish,
                                                                            perc_thru,
                                                                            total_vert_gain)))) %>%
                             
                             filter(year >= 2022) %>%
                             filter(!class %in% c("NC", "CC", "WC")) %>%
                             mutate(pred_break = ifelse(one_day_race == 1, 0.01, pred_break)) %>%
                             select(pred_rank,
                                    model_winprob,
                                    rel_age,
                                    error_rank,
                                    recent_races,
                                    shrunk_teamldr,
                                    total_seconds_back,
                                    predicted_bs,
                                    pred_climb_difficulty,
                                    sof,
                                    pred_break,
                                    one_day_race,
                                    grand_tour,
                                    uphill_finish))
  ),
  
  predicting_all_supp %>% 
    mutate(win = ifelse(rnk == 1, 1, 0)) %>%
    
    mutate(pred_break = predict(read_rds("Stored models/breakaway-prediction-model.rds"), 
                                as.matrix(predicting_all_supp %>%
                                            
                                            group_by(race, year, url) %>%
                                            mutate(perc_thru = as.numeric(stage)/max(as.numeric(stage))) %>%
                                            ungroup() %>%
                                            
                                            select(predicted_bs,
                                                   grand_tour,
                                                   pred_climb_difficulty,
                                                   length,
                                                   uphill_finish,
                                                   perc_thru,
                                                   total_vert_gain)))) %>%
    
    filter(year >= 2022) %>%
    filter(!class %in% c("NC", "CC", "WC")) %>%
    mutate(pred_break = ifelse(one_day_race == 1, 0.01, pred_break)) %>%
    select(-c(team, win_seconds, total_seconds, url, grand_tour, tm_pos, gain_gc, total_vert_gain,
             master_team, tour, cobbles, sof_limit, success, points_finish, leader_rating, 
             pred_climb_diff_succ, team_ldr, level_data, rk_teamldr, rk_rank, class_small, old_pred_rank, xrnk, missrnk))) %>%
  
  group_by(stage, race, year, class, date, length) %>%
  mutate(pred = pred/sum(pred)) %>%
  ungroup()

#
#
#
#

# ONE DAY RACE

xgb.train_ODR <- xgb.DMatrix(
  
  data = as.matrix(predicting_all_supp %>% 
                     mutate(win = ifelse(rnk == 1, 1, 0)) %>%
                     
                     mutate(pred_break = predict(read_rds("Stored models/breakaway-prediction-model.rds"), 
                                                 as.matrix(predicting_all_supp %>%
                                                             
                                                             group_by(race, year, url) %>%
                                                             mutate(perc_thru = as.numeric(stage)/max(as.numeric(stage))) %>%
                                                             ungroup() %>%
                                                             
                                                             select(predicted_bs,
                                                                    grand_tour,
                                                                    pred_climb_difficulty,
                                                                    length,
                                                                    uphill_finish,
                                                                    perc_thru,
                                                                    total_vert_gain)))) %>%
                     
                     filter(year < 2022 & one_day_race == 1) %>%
                     filter(!class %in% c("NC")) %>%
                     select(pred_rank,
                            predicted_bs,
                            pred_climb_difficulty,
                            sof,
                            pred_break,
                            one_day_race,
                            grand_tour,
                            uphill_finish)),
  
  label = predicting_all %>%
    filter(year < 2022 & one_day_race == 1) %>%
    filter(!class %in% c("NC")) %>%
    mutate(win = rnk == 1) %>%
    select(win) %>%
    .[[1]]
  
)

# test

xgb.test_ODR <- xgb.DMatrix(
  
  data = as.matrix(predicting_all_supp %>% 
                     mutate(win = ifelse(rnk == 1, 1, 0)) %>%
                     
                     mutate(pred_break = predict(read_rds("Stored models/breakaway-prediction-model.rds"), 
                                                 as.matrix(predicting_all_supp %>%
                                                             
                                                             group_by(race, year, url) %>%
                                                             mutate(perc_thru = as.numeric(stage)/max(as.numeric(stage))) %>%
                                                             ungroup() %>%
                                                             
                                                             select(predicted_bs,
                                                                    grand_tour,
                                                                    pred_climb_difficulty,
                                                                    length,
                                                                    uphill_finish,
                                                                    perc_thru,
                                                                    total_vert_gain)))) %>%
                     
                     filter(year >= 2022 & one_day_race == 1) %>%
                     filter(!class %in% c("NC")) %>%
                     select(pred_rank,
                            predicted_bs,
                            pred_climb_difficulty,
                            sof,
                            pred_break,
                            one_day_race,
                            grand_tour,
                            uphill_finish)),
  
  label = predicting_all %>%
    filter(year >= 2022 & one_day_race == 1) %>%
    filter(!class %in% c("NC")) %>%
    mutate(win = rnk == 1) %>%
    select(win) %>%
    .[[1]]
  
)

# outline parameters

params_ODR <- list(
  
  booster = "gbtree",
  eta = 0.3,
  max_depth = 5,
  gamma = 0,
  subsample = 1,
  colsample_bytree = 1,
  tree_method = "hist",
  objective = "binary:logistic"
  
)

# run xgboost model

gbm_model_ODR <- xgb.train(params = params_ODR,
                       data = xgb.train_ODR,
                       nrounds = 6000,
                       monotone_constraints = c(-1, 0, 0, 0, 0, 0, 0, 0),
                       early_stopping_rounds = 2000,
                       watchlist = list(val1 = xgb.train_ODR,
                                        val2 = xgb.test_ODR),
                       verbose = 0)

#
#
# xgb Importance

xgb.importance(model = gbm_model_ODR)

gbm_model_ODR$best_score

#

#write_rds(gbm_model, "Stored models/very-basic-logrank-xgboost-win.rds")

#

gbm_predict_WIN_ODR = cbind(
  
  pred = predict(gbm_model_ODR, 
                 as.matrix(predicting_all_supp %>% 
                             mutate(win = ifelse(rnk == 1, 1, 0)) %>%
                             
                             mutate(pred_break = predict(read_rds("Stored models/breakaway-prediction-model.rds"), 
                                                         as.matrix(predicting_all_supp %>%
                                                                     
                                                                     group_by(race, year, url) %>%
                                                                     mutate(perc_thru = as.numeric(stage)/max(as.numeric(stage))) %>%
                                                                     ungroup() %>%
                                                                     
                                                                     select(predicted_bs,
                                                                            grand_tour,
                                                                            pred_climb_difficulty,
                                                                            length,
                                                                            uphill_finish,
                                                                            perc_thru,
                                                                            total_vert_gain)))) %>%
                             
                             filter(year >= 2022 & one_day_race == 1) %>%
                             filter(!class %in% c("NC")) %>%
                             select(pred_rank,
                                    predicted_bs,
                                    pred_climb_difficulty,
                                    sof,
                                    pred_break,
                                    one_day_race,
                                    grand_tour,
                                    uphill_finish))
  ),
  
  predicting_all_supp %>% 
    mutate(win = ifelse(rnk == 1, 1, 0)) %>%
    
    mutate(pred_break = predict(read_rds("Stored models/breakaway-prediction-model.rds"), 
                                as.matrix(predicting_all_supp %>%
                                            
                                            group_by(race, year, url) %>%
                                            mutate(perc_thru = as.numeric(stage)/max(as.numeric(stage))) %>%
                                            ungroup() %>%
                                            
                                            select(predicted_bs,
                                                   grand_tour,
                                                   pred_climb_difficulty,
                                                   length,
                                                   uphill_finish,
                                                   perc_thru,
                                                   total_vert_gain)))) %>%
    
    filter(year >= 2022 & one_day_race == 1) %>%
    filter(!class %in% c("NC"))) %>%
  
  group_by(stage, race, year, class, date, length) %>%
  mutate(pred = pred/sum(pred)) %>%
  ungroup()


# NOT OBVIOUS BS

xgb.train_notBS <- xgb.DMatrix(
  
  data = as.matrix(predicting_all_supp %>% 
                     mutate(win = ifelse(rnk == 1, 1, 0)) %>%
                     
                     mutate(pred_break = predict(read_rds("Stored models/breakaway-prediction-model.rds"), 
                                                 as.matrix(predicting_all_supp %>%
                                                             
                                                             group_by(race, year, url) %>%
                                                             mutate(perc_thru = as.numeric(stage)/max(as.numeric(stage))) %>%
                                                             ungroup() %>%
                                                             
                                                             select(predicted_bs,
                                                                    grand_tour,
                                                                    pred_climb_difficulty,
                                                                    length,
                                                                    uphill_finish,
                                                                    perc_thru,
                                                                    total_vert_gain)))) %>%
                     
                     filter(year < 2022 & one_day_race == 0 & predicted_bs < 0.7) %>%
                     filter(!class %in% c("NC")) %>%
                     select(pred_rank,
                            predicted_bs,
                            pred_climb_difficulty,
                            sof,
                            pred_break,
                            one_day_race,
                            grand_tour,
                            uphill_finish)),
  
  label = predicting_all %>%
    filter(year < 2022 & one_day_race == 0 & predicted_bs < 0.7) %>%
    filter(!class %in% c("NC")) %>%
    mutate(win = rnk == 1) %>%
    select(win) %>%
    .[[1]]
  
)

# test

xgb.test_notBS <- xgb.DMatrix(
  
  data = as.matrix(predicting_all_supp %>% 
                     mutate(win = ifelse(rnk == 1, 1, 0)) %>%
                     
                     mutate(pred_break = predict(read_rds("Stored models/breakaway-prediction-model.rds"), 
                                                 as.matrix(predicting_all_supp %>%
                                                             
                                                             group_by(race, year, url) %>%
                                                             mutate(perc_thru = as.numeric(stage)/max(as.numeric(stage))) %>%
                                                             ungroup() %>%
                                                             
                                                             select(predicted_bs,
                                                                    grand_tour,
                                                                    pred_climb_difficulty,
                                                                    length,
                                                                    uphill_finish,
                                                                    perc_thru,
                                                                    total_vert_gain)))) %>%
                     
                     filter(year >= 2022 & one_day_race == 0 & predicted_bs < 0.7) %>%
                     filter(!class %in% c("NC")) %>%
                     select(pred_rank,
                            predicted_bs,
                            pred_climb_difficulty,
                            sof,
                            pred_break,
                            one_day_race,
                            grand_tour,
                            uphill_finish)),
  
  label = predicting_all %>%
    filter(year >= 2022 & one_day_race == 0 & predicted_bs < 0.7) %>%
    filter(!class %in% c("NC")) %>%
    mutate(win = rnk == 1) %>%
    select(win) %>%
    .[[1]]
  
)

# outline parameters

params_notBS <- list(
  
  booster = "gbtree",
  eta = 0.3,
  max_depth = 5,
  gamma = 0,
  subsample = 1,
  colsample_bytree = 1,
  tree_method = "hist",
  objective = "binary:logistic"
  
)

# run xgboost model

gbm_model_notBS <- xgb.train(params = params_notBS,
                           data = xgb.train_notBS,
                           nrounds = 6000,
                           monotone_constraints = c(-1, 0, 0, 0, 0, 0, 0, 0),
                           early_stopping_rounds = 2000,
                           watchlist = list(val1 = xgb.train_notBS,
                                            val2 = xgb.test_notBS),
                           verbose = 0)

#
#
# xgb Importance

xgb.importance(model = gbm_model_notBS)

gbm_model_notBS$best_score

#

#write_rds(gbm_model, "Stored models/very-basic-logrank-xgboost-win.rds")

#

gbm_predict_WIN_notBS = cbind(
  
  pred = predict(gbm_model_notBS, 
                 as.matrix(predicting_all_supp %>% 
                             mutate(win = ifelse(rnk == 1, 1, 0)) %>%
                             
                             mutate(pred_break = predict(read_rds("Stored models/breakaway-prediction-model.rds"), 
                                                         as.matrix(predicting_all_supp %>%
                                                                     
                                                                     group_by(race, year, url) %>%
                                                                     mutate(perc_thru = as.numeric(stage)/max(as.numeric(stage))) %>%
                                                                     ungroup() %>%
                                                                     
                                                                     select(predicted_bs,
                                                                            grand_tour,
                                                                            pred_climb_difficulty,
                                                                            length,
                                                                            uphill_finish,
                                                                            perc_thru,
                                                                            total_vert_gain)))) %>%
                             
                             filter(year >= 2022 & one_day_race == 0 & predicted_bs < 0.7) %>%
                             filter(!class %in% c("NC")) %>%
                             select(pred_rank,
                                    predicted_bs,
                                    pred_climb_difficulty,
                                    sof,
                                    pred_break,
                                    one_day_race,
                                    grand_tour,
                                    uphill_finish))
  ),
  
  predicting_all_supp %>% 
    mutate(win = ifelse(rnk == 1, 1, 0)) %>%
    
    mutate(pred_break = predict(read_rds("Stored models/breakaway-prediction-model.rds"), 
                                as.matrix(predicting_all_supp %>%
                                            
                                            group_by(race, year, url) %>%
                                            mutate(perc_thru = as.numeric(stage)/max(as.numeric(stage))) %>%
                                            ungroup() %>%
                                            
                                            select(predicted_bs,
                                                   grand_tour,
                                                   pred_climb_difficulty,
                                                   length,
                                                   uphill_finish,
                                                   perc_thru,
                                                   total_vert_gain)))) %>%
    
    filter(year >= 2022 & one_day_race == 0 & predicted_bs < 0.7) %>%
    filter(!class %in% c("NC"))) %>%
  
  group_by(stage, race, year, class, date, length) %>%
  mutate(pred = pred/sum(pred)) %>%
  ungroup()

#
#
#
#
#
#
#
#
#

uphill_finishes <- predicting_all_supp %>%
  
  filter(url %in% paste0(c("race/vuelta-a-espana/", "race/giro-d-italia/", "race/tour-de-france/"), year)) %>%
  filter(uphill_finish == 1)

uphill_finishes %>%
  filter(rnk == 1) %>%
  ggplot(aes(x = pred_climb_difficulty))+
  geom_histogram()+
  geom_vline(xintercept = 5.4)+
  labs(x = "Climbing difficulty",
       y = "Number of stages",
       title = "Grand Tour uphill finish stages")

similar_to_tomorrow <- uphill_finishes %>%
  
  filter(race == "tour de france" & 
           ((year == 2016 & stage == 2) |
              (year == 2017 & stage == 3) |
              (year == 2018 & stage %in% c(5,6)) |
              (year == 2019 & stage == 3) |
              (year == 2021 & stage %in% c(1,2))))

similar_to_tomorrow %>% 
  group_by(race, stage, year) %>%
  mutate(pctrk_pcd = percent_rank(pcd_logrk_impact), 
         pctrk_bs = percent_rank(bs_logrk_impact), 
         sd_pcd = (pcd_logrk_impact - mean(pcd_logrk_impact)) / sd(pcd_logrk_impact), 
         sd_bs = (bs_logrk_impact - mean(bs_logrk_impact)) / sd(bs_logrk_impact)) %>% 
  ungroup() %>% 
  
  group_by(rnk == 1, rnk <= 5, rnk <= 10, rnk <= 25, rnk <= 50) %>%
  summarize(pcd_logrk_impact = mean(sd_pcd), 
            bs_logrk_impact = mean(sd_bs), 
            pred = exp(mean(log(pred_rank))), 
            actual = exp(mean(log(rnk))), 
            riders = n()) %>%
  ungroup() %>%
  cbind(tibble(grouping = c("Rest of peloton", "finish 26-50", "finish 11-25", "finish 6-10", "finish 2-5", "wins"))) %>% 
  
  select(grouping, pcd_logrk_impact, bs_logrk_impact, pred)