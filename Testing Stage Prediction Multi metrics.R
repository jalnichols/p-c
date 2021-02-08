
library(tidyverse)
library(RMySQL)

#

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

All_data <- dbReadTable(con, "stage_data_perf") %>%
  
  filter(time_trial == 0) %>%
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  
  mutate(points_per_opp = ifelse(tm_pos == 1, points_finish, NA),
         sof_per_opp = ifelse(tm_pos == 1, sof, NA),
         pred_climb_diff_opp = ifelse(tm_pos == 1, pred_climb_difficulty, NA),
         pred_climb_diff_succ = ifelse(points_finish > 0, pred_climb_difficulty, NA),
         team_ldr = ifelse(tm_pos == 1, 1, 0)) %>%
  
  mutate(date = as.Date(date)) %>%
  
  select(-stage_name, -speed, -gain_3rd, -gain_5th, -gain_10th, -gain_40th,
         -time_trial, -gc_winner, -gc_pos, -parcours_value, -stage_type,
         -avg_alt, -missing_profile_data) %>%
  
  filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC", "Olympics")) |
           (class %in% c("2.1", "1.1") & Tour == "Europe Tour") | 
           (sof > 0.2 & class %in% c("2.2", "1.2", "2.2U", "1.2U", "2.Ncup", "1.Ncup", "JR")) |
           (sof > 0.1 & !class %in% c("2.2", "1.2", "2.2U", "1.2U", "2.Ncup", "1.Ncup", "JR"))) %>%
  unique() %>% 
  
  left_join(read_csv("cobbles.csv")) %>% 
  mutate(cobbles = ifelse(is.na(cobbles), 0, cobbles)) %>%
  
  mutate(final_group = ifelse(bunch_sprint == 1, ifelse(gain_1st <= 5, 1, 0), ifelse(rnk <= 20 | gain_20th == 0, 1, 0))) %>%
  
  select(-gain_1st, -gain_20th)

#
#
#
#
#

predicting_all <- All_data %>%
  
  select(-points_per_opp, -sof_per_opp, -pred_climb_diff_opp, -final_group) %>%
  
  inner_join(dbGetQuery(con, "SELECT * FROM predictions_stage_bunchsprint") %>%
               select(-bunch_sprint), by = c("stage", "race", "year")) %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT DISTINCT date FROM lme4_rider_logranks WHERE test_or_prod = 'test'") %>%
      mutate(date = as.Date(date)), by = c("date")
    
  ) %>%
  
  left_join(
    
    dbReadTable(con, "lme4_rider_logranks")  %>%
      
      filter(test_or_prod == "test") %>%
      
      #mutate(rider = str_to_title(rider)) %>%
      mutate(date = as.Date(Date)) %>%
      select(-Date) %>%
      
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
      
      rename(pcd_logrk_impact = pcd_impact,
             bs_logrk_impact = bunchsprint_impact,
             rand_logrk_impact = random_intercept,
             odr_logrk_impact = one_day_race) %>%
      
      mutate(pcd_logrk_impact = ifelse(is.na(pcd_logrk_impact), 0, pcd_logrk_impact),
             bs_logrk_impact = ifelse(is.na(bs_logrk_impact), 0, bs_logrk_impact),
             odr_logrk_impact = ifelse(is.na(odr_logrk_impact), 0, odr_logrk_impact)
      ) %>%
      
      filter(date >= as.Date('2017-01-01')), by = c("rider", "date")
    
  ) %>%
  
  # 0.97 is SD and 3.9 is Mean -- intercept is -0.6 and sof is 1.15 with an avg of 0.36 so -0.2 is left-over
  mutate(pred_rank = exp(-0.2 + ((
    ((rand_logrk_impact + 
        (predicted_bs * bs_logrk_impact) + 
        (one_day_race * odr_logrk_impact) + 
        (pcd_logrk_impact * pred_climb_difficulty))*-1) / 0.97)+3.9))) %>%
  
  select(-rand_logrk_impact, -pcd_logrk_impact, -bs_logrk_impact) %>%
  
  left_join(
    
    dbReadTable(con, "lme4_rider_points")  %>%
      filter(test_or_prod == "test") %>%
      #mutate(rider = str_to_title(rider)) %>%
      mutate(date = as.Date(Date)) %>%
      select(-Date) %>%
      
      mutate(level_data = ifelse(is.na(pcd_impact), "just_rider",
                                 ifelse(is.na(bunchsprint_impact), "pcd_added",
                                        ifelse(is.na(one_day_race), "bs_added", "odr_added")))) %>%
      
      # the standard deviations of random intercept and pcd impact both vary widely (increase as you move from 2015 to 2020)
      # we adjust here
      group_by(date, level_data) %>%
      mutate(pcd_impact_new = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact),
             random_intercept_new = (random_intercept - mean(random_intercept, na.rm = T)) / sd(random_intercept, na.rm = T)) %>%
      ungroup() %>%
      
      # this transforms them back to input into the regression equation
      mutate(pcd_impact = pcd_impact_new * sd(pcd_impact),
             random_intercept = random_intercept_new * sd(random_intercept)) %>%
      
      select(-pcd_impact_new, -random_intercept_new) %>%
      
      rename(pcd_points_impact = pcd_impact,
             bs_points_impact = bunchsprint_impact,
             rand_points_impact = random_intercept,
             odr_points_impact = one_day_race) %>%
      
      mutate(pcd_points_impact = ifelse(is.na(pcd_points_impact), 0, pcd_points_impact),
             bs_points_impact = ifelse(is.na(bs_points_impact), 0, bs_points_impact),
             odr_points_impact = ifelse(is.na(odr_points_impact), 0, odr_points_impact)
      ) %>%
      
      filter(date >= as.Date('2017-01-01')), by = c("rider", "date", 'level_data')
    
  ) %>%
  
  mutate(pred_points = -0.013 + (rand_points_impact + 
                                   (predicted_bs * bs_points_impact) + 
                                   (one_day_race * odr_points_impact) + 
                                   (pcd_points_impact * pred_climb_difficulty))) %>%
  
  select(-rand_points_impact, -pcd_points_impact, -bs_points_impact) %>%
  
  left_join(
    
    dbReadTable(con, "lme4_rider_teamleader")  %>%
      filter(test_or_prod == "test") %>%
      #mutate(rider = str_to_title(rider)) %>%
      mutate(date = as.Date(Date)) %>%
      select(-Date) %>%
      
      mutate(level_data = ifelse(is.na(pcd_impact), "just_rider",
                                 ifelse(is.na(bunchsprint_impact), "pcd_added",
                                        ifelse(is.na(one_day_race), "bs_added", "odr_added")))) %>%
      
      # the standard deviations of random intercept and pcd impact both vary widely (increase as you move from 2015 to 2020)
      # we adjust here
      group_by(date, level_data) %>%
      mutate(pcd_impact_new = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact),
             random_intercept_new = (random_intercept - mean(random_intercept, na.rm = T)) / sd(random_intercept, na.rm = T)) %>%
      ungroup() %>%
      
      # this transforms them back to input into the regression equation
      mutate(pcd_impact = pcd_impact_new * sd(pcd_impact),
             random_intercept = random_intercept_new * sd(random_intercept)) %>%
      
      select(-pcd_impact_new, -random_intercept_new) %>%
      
      rename(pcd_tmldr_impact = pcd_impact,
             bs_tmldr_impact = bunchsprint_impact,
             odr_tmldr_impact = one_day_race) %>%
      
      mutate(pcd_tmldr_impact = ifelse(is.na(pcd_tmldr_impact), 0, pcd_tmldr_impact),
             bs_tmldr_impact = ifelse(is.na(bs_tmldr_impact), 0, bs_tmldr_impact),
             odr_tmldr_impact = ifelse(is.na(odr_tmldr_impact), 0, odr_tmldr_impact)
      ) %>%
      
      filter(date >= as.Date('2017-01-01')), by = c("rider", "date", 'level_data')
    
  ) %>%
  
  left_join(
    
    dbReadTable(con, "lme4_rider_succwhenopp")  %>%
      filter(test_or_prod == "test") %>%
      #mutate(rider = str_to_title(rider)) %>%
      mutate(date = as.Date(Date)) %>%
      select(-Date) %>%
      
      mutate(level_data = ifelse(is.na(pcd_impact), "just_rider",
                                 ifelse(is.na(bunchsprint_impact), "pcd_added",
                                        ifelse(is.na(one_day_race), "bs_added", "odr_added")))) %>%
      
      # the standard deviations of random intercept and pcd impact both vary widely (increase as you move from 2015 to 2020)
      # we adjust here
      group_by(date, level_data) %>%
      mutate(pcd_impact_new = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact),
             random_intercept_new = (random_intercept - mean(random_intercept, na.rm = T)) / sd(random_intercept, na.rm = T)) %>%
      ungroup() %>%
      
      # this transforms them back to input into the regression equation
      mutate(pcd_impact = pcd_impact_new * sd(pcd_impact),
             random_intercept = random_intercept_new * sd(random_intercept)) %>%
      
      select(-pcd_impact_new, -random_intercept_new) %>%
      
      rename(pcd_succwhenopp_impact = pcd_impact,
             bs_succwhenopp_impact = bunchsprint_impact,
             succwhenopp_intercept = random_intercept,
             odr_succwhenopp_impact = one_day_race) %>%
      
      mutate(pcd_succwhenopp_impact = ifelse(is.na(pcd_succwhenopp_impact), 0, pcd_succwhenopp_impact),
             bs_succwhenopp_impact = ifelse(is.na(bs_succwhenopp_impact), 0, bs_succwhenopp_impact),
             odr_succwhenopp_impact = ifelse(is.na(odr_succwhenopp_impact), 0, odr_succwhenopp_impact)
      ) %>%
      
      filter(date >= as.Date('2017-01-01')), by = c("rider", "date", 'level_data')
    
  ) %>%
  
  left_join(
    
    dbReadTable(con, "lme4_rider_success") %>%
      filter(test_or_prod == "test") %>%
      mutate(date = as.Date(Date)) %>%
      select(-Date) %>%
      
      mutate(level_data = ifelse(is.na(pcd_impact), "just_rider",
                                 ifelse(is.na(bunchsprint_impact), "pcd_added",
                                        ifelse(is.na(one_day_race), "bs_added", "odr_added")))) %>%
      
      filter(date >= as.Date('2017-01-01'))  %>%
      
      #mutate(rider = str_to_title(rider)) %>%
      
      # the standard deviations of random intercept and pcd impact both vary widely (increase as you move from 2015 to 2020)
      # we adjust here
      group_by(date, level_data) %>%
      mutate(pcd_impact_new = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact),
             random_intercept_new = (random_intercept - mean(random_intercept, na.rm = T)) / sd(random_intercept, na.rm = T)) %>%
      ungroup() %>%
      
      # this transforms them back to input into the regression equation
      mutate(pcd_impact = pcd_impact_new * sd(pcd_impact),
             random_intercept = random_intercept_new * sd(random_intercept)) %>%
      
      select(-pcd_impact_new, -random_intercept_new) %>%
      
      rename(success_intercept = random_intercept,
             pcd_success_impact = pcd_impact,
             bs_success_impact = bunchsprint_impact,
             odr_success_impact = one_day_race) %>%
      
      mutate(pcd_success_impact = ifelse(is.na(pcd_success_impact), 0, pcd_success_impact),
             bs_success_impact = ifelse(is.na(bs_success_impact), 0, bs_success_impact),
             odr_success_impact = ifelse(is.na(odr_success_impact), 0, odr_success_impact)), by = c("rider", "date", 'level_data')) %>%
  
  # add in cobbles performance
  left_join(
    
    dbReadTable(con, "performance_rider_cobbles")  %>%
      
      #mutate(rider = str_to_title(rider)) %>%
      
      rename(cobbles_intercept = cobbles), by = c("rider")) %>%
  
  # calculate team leader and success predictions using random effects
  mutate(glmer_pred = -2.11 + (random_intercept + 
                                 (pred_climb_difficulty * pcd_tmldr_impact) + 
                                 (one_day_race * odr_tmldr_impact) + 
                                 (predicted_bs * bs_tmldr_impact)),
         glmer_pred = exp(glmer_pred) / (1+exp(glmer_pred))) %>%
  
  mutate(succ_pred = -4.5 +
           ((cobbles_intercept * cobbles) + 
              success_intercept + 
              (pred_climb_difficulty * pcd_success_impact) + 
              (one_day_race * odr_success_impact) +
              (predicted_bs * bs_success_impact)),
         succ_pred = exp(succ_pred) / (1+exp(succ_pred))) %>%
  
  mutate(succwhenopp_pred = -2.13 +
           ((cobbles_intercept * cobbles) + 
              succwhenopp_intercept + 
              (one_day_race * odr_succwhenopp_impact) +
              (pred_climb_difficulty * pcd_succwhenopp_impact) + 
              (predicted_bs * bs_succwhenopp_impact)),
         succwhenopp_pred = exp(succwhenopp_pred) / (1+exp(succwhenopp_pred))) %>%
  
  group_by(stage, race, year, team, level_data) %>%
  mutate(No1_Team = ifelse(rank(-glmer_pred, ties.method = "min")==1, 1, 0),
         No1_Team_succ = ifelse(rank(-succ_pred, ties.method = "min")==1, 1, 0)) %>%
  ungroup() %>%
  
  select(-pcd_succwhenopp_impact, -bs_succwhenopp_impact, -succwhenopp_intercept, 
         -cobbles_intercept, -bs_success_impact, -success_intercept, -pcd_success_impact,
         -random_intercept, -pcd_tmldr_impact, -bs_tmldr_impact,
         -odr_points_impact, -odr_tmldr_impact, -odr_success_impact,
         -odr_logrk_impact, -odr_succwhenopp_impact) %>%
  
  mutate(rider_match = str_to_title(rider)) %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT rider, date as dob FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)), by = c("rider_match" = "rider")) %>%
  
  mutate(age = as.numeric(as.Date(date)-as.Date(dob))/365.25) %>%
  
  group_by(stage, race, year, level_data) %>%
  mutate(rel_age = age - mean(age, na.rm = T)) %>%
  ungroup() %>%
  
  select(-age, -rider_match, -dob) %>%
  mutate(rel_age = ifelse(is.na(rel_age), 0, rel_age)) %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT race, year, rider, bib FROM pcs_all_startlists") %>%
      mutate(bib_leader = ifelse(bib %% 10 == 1, 1, 0)) %>%
      unique(), by = c("rider", "race", "year")
    
  ) %>%
  
  # give everyone with missing data the median data point
  group_by(stage, race, year, class, level_data) %>%
  mutate(glmer_pred = ifelse(is.na(glmer_pred), median(glmer_pred, na.rm = T), glmer_pred),
         succ_pred = ifelse(is.na(succ_pred), median(succ_pred, na.rm = T), succ_pred),
         succwhenopp_pred = ifelse(is.na(succwhenopp_pred), median(succwhenopp_pred, na.rm = T), succwhenopp_pred),
         pred_points = ifelse(is.na(pred_points), median(pred_points, na.rm = T), pred_points),
         pred_rank = ifelse(is.na(pred_rank), median(pred_rank, na.rm = T), pred_rank)) %>%
  ungroup() %>%
  
  # rank each stat within race
  group_by(stage, race, year, class, level_data) %>%
  mutate(rk_teamldr = rank(-glmer_pred, ties.method = "min"),
         rk_points = rank(-pred_points, ties.method = "min"),
         rk_rank = rank(pred_rank, ties.method = "min"),
         rk_success = rank(-succ_pred, ties.method = "min"),
         rk_succwhenopp = rank(-succwhenopp_pred, ties.method = "min")) %>%
  ungroup() %>%
  
  group_by(stage, race, year, team, level_data) %>% 
  # shrink estimates to account for other riders in team
  mutate(shrunk_teamldr = glmer_pred/sum(glmer_pred, na.rm = T)) %>% 
  # rank within team
  mutate(teamldr_within_team = rank(rk_teamldr, ties.method = "min")) %>% 
  ungroup()

#
# Predicting team leader
#

predicting_all %>% 
  
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

pick <- sample(predicting_all$date %>% unique(), 40)

predicting_all %>%
  # filter non-teams
  filter(!class %in% c("WC", 'CC', 'NC'))  %>%
  filter(!team == "") %>% 
  filter(date %in% pick) %>%
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
    
    pred = predict(justrider, 
                   predicting_all %>% 
                     unique() %>%
                     filter(level_data == "just_rider") %>%
                     filter(!date %in% pick)),
    
    predicting_all %>% 
      unique() %>%
      filter(level_data == "just_rider") %>%
      filter(!date %in% pick)),
  
  cbind(
    
    pred = predict(pcdadded, 
                   predicting_all %>% 
                     unique() %>%
                     filter(level_data == "pcd_added") %>%
                     filter(!date %in% pick)),
    
    predicting_all %>% 
      unique() %>%
      filter(level_data == "pcd_added") %>%
      filter(!date %in% pick)),
  
  cbind(
    
    pred = predict(BSadded, 
                   predicting_all %>% 
                     unique() %>%
                     filter(level_data == "bs_added") %>%
                     filter(!date %in% pick)),
    
    predicting_all %>% 
      unique() %>%
      filter(level_data == "bs_added") %>%
      filter(!date %in% pick)),
  
  cbind(
    
    pred = predict(ODRadded, 
                   predicting_all %>% 
                     unique() %>%
                     filter(level_data == "odr_added") %>%
                     filter(!date %in% pick)),
    
    predicting_all %>% 
      unique() %>%
      filter(level_data == "odr_added") %>%
      filter(!date %in% pick))) %>% 
  
  mutate(pred = exp(pred)/(1+exp(pred)),
         leader = tm_pos==1)

# brier score for accuracy
brierScore <- pred_tmldr %>%
  
  group_by(level_data) %>%
  summarize(BrierScore = mean((pred-leader)^2, na.rm = T),
            n=n()) %>%
  ungroup()

#
#
#

glm(team_ldr ~ shrunk_teamldr + pred_rank + pred_points + rk_teamldr, 
    
    family = "binomial", 
    data = predicting_all %>% 
      filter(year < 2019) %>%
      filter(!class %in% c("NC", "CC", "WC"))) -> glm_mod

summary(glm_mod)

gbm_predict_TMLDR = cbind(
  
  pred = predict(glm_mod, 
                 predicting_all %>% 
                   unique() %>%
                   filter(year >= 2019)),
  
  predicting_all %>%
    unique() %>%
    filter(year >= 2019))

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

glm(win ~ rk_teamldr + rk_points + rk_rank + rk_success + rk_succwhenopp + shrunk_teamldr + 
      pred_points + pred_rank, 
    
    family = "binomial", 
    data = predicting_all %>% 
      mutate(win = ifelse(rnk == 1, 1, 0)) %>%
      filter(year < 2020) %>%
      filter(!class %in% c("NC", "CC", "WC"))
) -> win_mod

summary(win_mod)

gbm_predict_WIN = cbind(
  
  pred = predict(win_mod, 
                 predicting_all %>% 
                   unique() %>%
                   filter(year >= 2020)),
  
  predicting_all %>%
    unique() %>%
    filter(year >= 2020))

# predict test set out of sample
pred <- gbm_predict_WIN %>% 
  
  mutate(pred = exp(pred)/(1+exp(pred)),
         win = rnk==1)

# brier score for accuracy
brierScore <- mean((pred$pred-pred$win)^2, na.rm = T)

print(brierScore)

# ROC / AUC
roc_obj <- pROC::roc(pred$win, pred$pred)
print(pROC::auc(roc_obj))

pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
  labs(title = "Predict winner", subtitle = pROC::auc(roc_obj))
