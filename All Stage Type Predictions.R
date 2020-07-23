#

library(tidyverse)
library(RMySQL)

#

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#
#
#

# going to start very easy with rankings of bunch_sprint performance

All_dates <- dbReadTable(con, "stage_data_perf") %>%
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  filter(year > 2014) %>%
  select(date) %>%
  unique()

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
  
  select(-stage_name, -speed, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th, -gain_1st,
         -time_trial, -gc_winner, -gc_pos, -parcours_value, -stage_type) %>%
  
  filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC")) |
           (class %in% c("2.1", "1.1") & Tour == "Europe Tour") | 
           (sof > 0.25 & class %in% c("2.1", "1.1"))) %>%
  unique()

# per date glmer models using pcd interaction and rider random effects

All_dates <- expand_grid(month = c("01","02","03","04","05","06","07","08","09","10"), year = c(2015, 2016, 2017, 2018, 2019, 2020), day = 1) %>%
  
  mutate(date = paste0(year,"-",month,"-0", day)) %>%
  select(date) %>%
  unique()

model_list <- vector("list", length(All_dates$date))
summary_list <- vector("list", length(All_dates$date))

# I can generate all of these lme4 predictions
# and then compare them with what Stan out-puts
# that gives me an idea of how much Stan is regressing the small samples data
# to the mean, so I can just regress the lme4 predictions based on small samples
# because one iteration of lme4 takes 45 seconds and can be run on AWS
# while Stan takes 7500 seconds and is trickier to run on AWS

for(b in 1:length(All_dates$date)) {
  
  # one day before predicting date and two years back
  maxD <- as.Date(All_dates$date[[b]]) - 1
  minD <- maxD - 730
  
  dx <- All_data %>% filter(between(date, minD, maxD)==TRUE)
  
  # run a lme4 model for rider success and impact of pcd on success
  
  tictoc::tic()
  
  # mod_succ <- lme4::glmer(success ~ (1 + pred_climb_difficulty | rider),
  #                      data = dx,
  #                      family = binomial("logit"),
  #                      nAGQ=0,
  #                      control=lme4::glmerControl(optimizer = "nloptwrap"))
  
  tictoc::toc()
  
  # random_effects <- lme4::ranef(mod_succ)[[1]] %>%
  #  rownames_to_column() %>%
  #  rename(rider = rowname,
  #         random_intercept = `(Intercept)`,
  #         pcd_impact = pred_climb_difficulty) %>%
  # #what date are we predicting
  # mutate(Date = as.Date(maxD + 1))
  
  ################################################
  
  #dbWriteTable(con, "lme4_rider_success", random_effects, append = TRUE, row.names = FALSE)
  
  ################################################
  
  tictoc::tic()
  
  # mod4 <- lme4::glmer(team_ldr ~ (1 + pred_climb_difficulty | rider),
  
                      # data = dx,
  
                      # family = binomial("logit"),
                      # nAGQ=0,
                      # control=lme4::glmerControl(optimizer = "nloptwrap"))
  tictoc::toc()
  
  # summary
  
  # model_list[[b]] <- mod4
  # summary_list[[b]] <- summary(mod4)
  
  # random_effects <- lme4::ranef(mod4)[[1]] %>%
  #  rownames_to_column() %>%
  #  rename(rider = rowname,
  #         random_intercept = `(Intercept)`,
  #         pcd_impact = pred_climb_difficulty) %>%
  
  # what date are we predicting
   # mutate(Date = as.Date(maxD + 1))
  
  ################################################
  
  #dbWriteTable(con, "lme4_rider_teamleader", random_effects, append = TRUE, row.names = FALSE)
  
  ################################################
  
  tictoc::tic()
  
  # All_riders <- dx %>%
    
    # mutate(pcd_success = ifelse(success == 1, pred_climb_difficulty, NA),
    #        pcd_tmldr = ifelse(tm_pos == 1, pred_climb_difficulty, NA)) %>%
  
    # group_by(rider, bunch_sprint) %>%
    # summarize(
      
      # team_leader = mean(tm_pos == 1, na.rm = T),
      # domestique = mean(tm_pos >= 4, na.rm = T),
      # in_pack = mean(gain_gc <= 5, na.rm = T),
      
      # pcd_corr_tmldr = cor(tm_pos==1, pred_climb_difficulty, method = "pearson"),
      # pcd_corr_pts = cor(points_finish, pred_climb_difficulty, method = "pearson"),
      # pcd_corr_succ = cor(success, pred_climb_difficulty, method = "pearson"),
      
      # first_race = min(date, na.rm = T),
      # latest_race = max(date, na.rm = T),
      
      # points_per_opp = mean(points_per_opp, na.rm = T),
      # points_per_race = mean(points_finish, na.rm = T),
      
      # sof_leader = mean(sof_per_opp, na.rm = T),
      # sof_overall = mean(sof, na.rm = T),
      
      # pcd_leader = mean(pred_climb_diff_opp, na.rm = T),
      # pcd_success = mean(pred_climb_diff_succ, na.rm = T),
      # pcd_overall = mean(pred_climb_difficulty, na.rm = T),
  
      # high_success = quantile(pcd_success, probs = 0.9, na.rm = T),
      # low_success = quantile(pcd_success, probs = 0.1, na.rm = T),
      
    #   successes = sum(points_finish > 0, na.rm = T),
    #   opportunities = sum(tm_pos == 1, na.rm = T),
    #   races = n()) %>%
    # ungroup() %>%
    
    # what date are we predicting
    #mutate(Date = as.Date(maxD + 1)) %>%
    
    #gather(stat, value, team_leader:pcd_overall) %>%
    
    #mutate(value = ifelse(is.na(value), 0, value)) %>%
    
    #spread(stat, value)
  
  tictoc::toc()
  
  ############################################################
  
  #dbWriteTable(con, "performance_rider_allpcd", All_riders, append = TRUE, row.names = FALSE)
  
  ############################################################
  
  print(maxD)
  
}

##
##
##
##
##
##
##
##
##

##################################################

performance_table <- dbReadTable(con, "performance_rider_allpcd") %>%
  
  mutate(date = as.Date(Date)) %>%
  select(-Date) %>%
  
  filter(date <= as.Date('2020-04-01') | date >= as.Date('2020-08-01'))

##################################################

lme4_success_table <- dbReadTable(con, "lme4_rider_success") %>%
  
  mutate(date = as.Date(Date)) %>%
  select(-Date) %>%
  
  filter(date <= as.Date('2020-04-01') | date >= as.Date('2020-08-01'))

##################################################

predicting_all <- All_data %>%
  
  filter(class %in% c("1.UWT", "2.UWT", "WC", "CC") | 
           (class %in% c("1.HC", "2.HC", "1.Pro", "2.Pro") & Tour == 'Europe Tour') |
           (sof > 0.2)) %>%
  
  select(-points_per_opp, -sof_per_opp, -pred_climb_diff_opp, -bunch_sprint) %>%
  
  inner_join(
    
    rbind(
      dbGetQuery(con, "SELECT * FROM predictions_stage_bunchsprint") %>%
        mutate(bunch_sprint = 1),
      dbGetQuery(con, "SELECT * FROM predictions_stage_bunchsprint") %>%
        mutate(bunch_sprint = 0)), by = c("stage", "race", "year")
    
  ) %>%
  
  mutate(date = as.Date(paste0(lubridate::year(date), "-", lubridate::month(date), "-", "01"))) %>%
  
  inner_join(
    
    performance_table, by = c("rider", "date", "bunch_sprint")) %>%

  #
  # need to regress points_per_opp as some riders have won on their only opportunity
  # filtered to tm_pos == 1 and regressed a number of combinations of N and regress amt between 0.01 and 0.024 (raw average and weighted avg of ppo)
  # N = 4 and 0.01 was the best R2 of about 0.109
  #
  
  mutate(points_per_opp = (0.04 + (opportunities * points_per_opp)) / (4 + opportunities)) %>%
  
  # also adjust pcd_success to consider riders who don't have successes or even opportunities
  
  mutate(pcd_success = ((pcd_overall * 1) + (pcd_success * successes * 2) + (pcd_leader * opportunities)) / 
           (opportunities + 1 + (successes * 2))) %>%
  
  group_by(stage, race, year) %>%
  mutate(field_points_per_race = mean(points_per_race, na.rm = T),
         field_team_leader = mean(team_leader, na.rm = T),
         field_domestique = mean(domestique, na.rm = T),
         field_sof_overall = mean(sof_overall, na.rm = T),
         rel_in_pack = in_pack - mean(in_pack, na.rm = T),
         rank_ppo = rank(-points_per_opp, ties.method = "min")) %>%
  ungroup() %>%
  
  mutate(top_5 = ifelse(rank_ppo <=5, points_per_opp, NA)) %>%
  
  group_by(stage, race, year) %>%
  mutate(strength_elite = mean(top_5, na.rm = T)) %>%
  ungroup() %>%
  
  select(-top_5) %>%
  
  mutate(rel_elite = points_per_opp - strength_elite) %>%
  
  group_by(stage, race, year, team) %>%
  mutate(team_team_leader = mean(team_leader, na.rm = T),
         team_domestique = mean(domestique, na.rm = T),
         team_ppo_rank = rank(-points_per_opp, ties.method = "min"),
         team_sof = mean(sof_overall, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(win = rnk == 1,
         rel_ppo = points_per_opp - field_points_per_race,
         rel_team_tmldr = team_leader - team_team_leader,
         rel_team_domestique = domestique - team_domestique)  %>%
  
  mutate(pcd_corr_both = (0.5*pcd_corr_pts)+(0.25*pcd_corr_succ)+(0.25*pcd_corr_tmldr)) %>%
  
  mutate(pcd_abs_diff = abs(pred_climb_difficulty - pcd_success),
         pcd_corr = (pred_climb_difficulty - mean(pred_climb_difficulty, na.rm = T)) * pcd_corr_both) %>%
  
  group_by(stage, race, year) %>%
  mutate(rel_pcd_abs_diff = mean(pcd_abs_diff) - pcd_abs_diff,
         rel_pcd_corr = pcd_corr - mean(pcd_corr, na.rm = T)) %>%
  ungroup() %>%
  
  # calculate sof vs rest of field
  group_by(stage, race, year) %>%
  mutate(rel_sof_race = sof_overall - mean(sof_overall, na.rm = T)) %>%
  ungroup() %>%
  
  # calculate race outcome in terms of rider pcd_success
  mutate(results_pcd_success = ifelse(points_finish > 0, pcd_success, NA),
         results_points_finish = ifelse(points_finish > 0, points_finish, NA)) %>%
  
  group_by(stage, race, year) %>%
  mutate(results_pcd_success = mean(results_pcd_success, na.rm = T),
         results_points_finish = mean(results_points_finish, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(sof_vs_team = sof_overall - team_sof) %>%
  
  # calculate interaction variables
  mutate(elite_pcd_corr = rel_elite * rel_pcd_corr,
         pcd_succ_vs_race = pred_climb_difficulty - pcd_leader) %>%
  
  inner_join(
    
    dbReadTable(con, "lme4_rider_teamleader") %>%
      mutate(date = as.Date(Date)) %>%
      select(-Date) %>%
      
      filter(date <= as.Date('2020-04-01') | date >= as.Date('2020-08-01')) %>%
      group_by(date) %>%
      mutate(pcd_impact = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact)) %>%
      ungroup(), by = c("rider", "date")
    
  ) %>%
  
  inner_join(
    
    lme4_success_table %>%
      group_by(date) %>%
      mutate(pcd_impact = (pcd_impact - mean(pcd_impact, na.rm = T))/sd(pcd_impact)) %>%
      ungroup() %>%
      rename(success_intercept = random_intercept,
             pcd_success_impact = pcd_impact), by = c("rider", "date")) %>%
  
  # calculate team leader and success predictions using random effects
  mutate(glmer_pred = (random_intercept + (pred_climb_difficulty * pcd_impact)),
         glmer_pred = exp(glmer_pred) / (1+exp(glmer_pred))) %>%
  
  mutate(succ_pred = (success_intercept + (pred_climb_difficulty * pcd_success_impact)),
         succ_pred = exp(succ_pred) / (1+exp(succ_pred))) %>%
  
  #
  mutate(mod_pcd_corr_both = (0.67*pcd_success_impact)+(0.33*pcd_impact)) %>%
  
  mutate(mod_pcd_corr = (pred_climb_difficulty - mean(pred_climb_difficulty, na.rm = T)) * mod_pcd_corr_both) %>%
  
  group_by(stage, race, year) %>%
  mutate(mod_rel_pcd_corr = mod_pcd_corr - mean(mod_pcd_corr, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(mod_elite_pcd_corr = rel_elite * mod_rel_pcd_corr) %>%
  
  group_by(stage, race, year, team) %>%
  mutate(rel_glmer_pred = glmer_pred - mean(glmer_pred, na.rm = T),
         rel_succ_pred = succ_pred - mean(succ_pred, na.rm = T)) %>%
  mutate(No1_Team = ifelse(rank(-glmer_pred, ties.method = "min")==1, 1, 0),
         No1_Team_succ = ifelse(rank(-succ_pred, ties.method = "min")==1, 1, 0)) %>%
  ungroup() %>%
  
  group_by(stage, race, year, team) %>%
  mutate(teammates_rel_sof = (sum(rel_sof_race, na.rm = T) - rel_sof_race) / (n() - 1)) %>%
  ungroup()

#
# how does pcd_impact map onto fit for pred_climb_difficulty?
#

predicting_all %>% 
  
  group_by(rider) %>%
  filter(mean(success, na.rm = T) >= 0.05) %>% 
  ungroup() %>% 
  
  mutate(pred_climb_difficulty = ifelse(pred_climb_difficulty > 18, 18, pred_climb_difficulty)) %>%
  
  mutate(w = ifelse(rnk==1,pred_climb_difficulty,NA), 
         s = ifelse(success==1,pred_climb_difficulty, NA), 
         l = ifelse(tm_pos==1,pred_climb_difficulty, NA)) %>%
  
  group_by(f = ifelse(mod_pcd_corr_both < -6, -13, 
                      ifelse(mod_pcd_corr_both > 2.5, 5, 
                             floor(mod_pcd_corr_both / 0.5)))) %>%
  summarize(win = median(w, na.rm = T),
            ldr = median(l, na.rm = T),
            succ = median(s, na.rm = T), 
            x85 = quantile(w, probs = 0.85, na.rm = T),
            x15 = quantile(w, probs = 0.15, na.rm = T),
            nw=sum(rnk==1, na.rm = T),
            ns=sum(success==1, na.rm = T)) %>%
  ungroup() %>% 
  
  mutate(Range = x85-x15) %>%
  select(-x15, -x85) %>%
  
  gather(stat, value, -f) %>% 
  mutate(f = f/2) %>%
  filter(!stat %in% c("nw", 'ns')) -> link_pcdimpact_w_pcdperf

# plot to check
ggplot(link_pcdimpact_w_pcdperf, aes(x = f, y = value))+
  geom_smooth(se=F)+
  labs(x = "standard deviation of pcd_impact",
       y = "ideal pred_climb_difficulty fit",
       title = "")+
  facet_wrap(~stat)

# set basic gam smoother to find non-linear impact of f on value (success/leader/wins) above
gam_link_pcd <- mgcv::gam(value ~ s(f, k = 5),
                          data = link_pcdimpact_w_pcdperf %>% filter(!stat == 'Range'))

# predict based on -12 to +5 (reasonable ranges)
pred_link_pcd <- cbind(
  
  pcd_optimal = predict(gam_link_pcd, tibble(f = seq(-8,4,1))),
  
  tibble(f = seq(-8,4,1))
  
)

# plot to check (0 pcd_impacts will optimally fit at 6 pcd)
ggplot(pred_link_pcd, aes(x = f, y = pcd_optimal))+geom_point()

# set basic gam smoother to find non-linear impact of f on SD of pcd_success above
gam_link_pcd_SD <- mgcv::gam(value ~ s(f, k=5),
                          data = link_pcdimpact_w_pcdperf %>% filter(stat == 'Range'))

# predict based on -8 to +4 (reasonable ranges) for SD
pred_link_pcd_SD <- cbind(
  
  pcd_SD = predict(gam_link_pcd_SD, tibble(f = seq(-8,4,1))),
  
  tibble(f = seq(-8,4,1))
  
)

# now use to create pcd_impact variable

predicting_all <- predicting_all %>%
  
  cbind(pcd_optimal = predict(gam_link_pcd, predicting_all %>%
                                select(f = mod_pcd_corr_both))) %>%
  
  #cbind(pcd_Range = predict(lm_link_pcd_SD, predicting_all %>%
  #                            select(f = mod_pcd_corr_both))) %>%
  
  mutate(#pcd_Range = ifelse(pcd_Range < 2, 2, pcd_Range),
         pcd_optimal_race = abs(pcd_optimal - pred_climb_difficulty)
         #/ pcd_Range
         ) %>%
  
  group_by(stage, race, year) %>%
  mutate(pcd_optimal_race = mean(pcd_optimal_race, na.rm = T) - pcd_optimal_race) %>%
  ungroup()

##################################################

#
#
#
#
# team leader

tmldr_model_All <- glm(tmldr ~ 
                         rel_elite:pcd_optimal_race +
                         No1_Team + 
                         rel_glmer_pred +
                         No1_Team_succ +
                         pcd_optimal_race +
                         rel_succ_pred,
                       
                       data = predicting_all %>%
                         mutate(tmldr = tm_pos == 1) %>%
                         filter(bunch_sprint == 1),
                       family = "binomial",
                       
                       weights = predicting_all %>%
                         filter(bunch_sprint == 1) %>%
                         mutate(predicted_bs = 1 - predicted_bs) %>%
                         select(predicted_bs) %>% .[[1]])

summary(tmldr_model_All)

# XGBoost -----------------------------------------------------------------

#
#
#
#
#
# train

library(xgboost)

#
# First train the model on pre-2019, test on 2019-20 using bunch_sprint == 1 and weighting by predicted_bs
# this slightly improves overall model
#

xgb.train_BS1 <- xgb.DMatrix(
  
  data = as.matrix(predicting_all %>%
                     filter(year < 2019 & bunch_sprint == 1) %>%
                     select(rel_team_tmldr, No1_Team, rel_glmer_pred, rel_elite, No1_Team_succ, rel_succ_pred,
                            mod_rel_pcd_corr, mod_elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
                            pcd_optimal_race)),
  
  label = predicting_all %>%
    filter(year < 2019 & bunch_sprint == 1) %>%
    mutate(tmldr = tm_pos == 1) %>%
    select(tmldr) %>%
    .[[1]],
  
  weight = predicting_all %>%
    filter(year < 2019 & bunch_sprint == 1) %>%
    select(predicted_bs) %>%
    .[[1]]
  
)

# test

xgb.test_BS1 <- xgb.DMatrix(
  
  data = as.matrix(predicting_all %>%
                     filter(year >= 2019 & bunch_sprint == 1) %>%
                     select(rel_team_tmldr, No1_Team, rel_glmer_pred, rel_elite, No1_Team_succ, rel_succ_pred,
                            mod_rel_pcd_corr, mod_elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
                            pcd_optimal_race)),
  
  label = predicting_all %>%
    filter(year >= 2019 & bunch_sprint == 1) %>%
    mutate(tmldr = tm_pos == 1) %>%
    select(tmldr) %>%
    .[[1]],
  
  weight = predicting_all %>%
    filter(year >= 2019 & bunch_sprint == 1) %>%
    select(predicted_bs) %>%
    .[[1]]
  
)

# outline parameters

params <- list(
  
  booster = "gbtree",
  eta = 0.3,
  max_depth = 4,
  gamma = 0,
  subsample = 1,
  colsample_bytree = 1,
  tree_method = "hist",
  objective = "binary:logistic"
  
)

# run xgboost model

gbm_model_BS1 <- xgb.train(params = params,
                       data = xgb.train_BS1,
                       nrounds = 10000,
                       nthreads = 4,
                       early_stopping_rounds = 1000,
                       watchlist = list(val1 = xgb.train_BS1,
                                        val2 = xgb.test_BS1),
                       verbose = 1)

#
#
# xgb Importance

xgb.importance(model = gbm_model_BS1)

gbm_model_BS1$best_score

#
#
# this outputs GBM predictions for all data

gbm_predict_TMLDR_BS1 = cbind(
  
  
  pred = predict(gbm_model_BS1, 
                 as.matrix(predicting_all %>%
                             filter(year >= 2019 & bunch_sprint == 1) %>%
                             select(rel_team_tmldr, No1_Team, rel_glmer_pred, rel_elite, No1_Team_succ, rel_succ_pred,
                                    mod_rel_pcd_corr, mod_elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
                                    pcd_optimal_race), reshape=T)),
  
  predicting_all %>%
    filter(year >= 2019 & bunch_sprint == 1) %>%
    select(stage, race, year, rider, team, pred_climb_difficulty, rel_team_tmldr, No1_Team, 
           rel_glmer_pred, rel_elite, No1_Team_succ, rel_succ_pred,
           mod_rel_pcd_corr, elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
           tm_pos, win, predicted_bs, one_day_race, pcd_optimal_race))

#
# Next run same train/test split on bunch_sprint == 0, weighting the same
#
#

xgb.train_BS0 <- xgb.DMatrix(
  
  data = as.matrix(predicting_all %>%
                     filter(year < 2019 & bunch_sprint == 0) %>%
                     select(rel_team_tmldr, No1_Team, rel_glmer_pred, rel_elite, No1_Team_succ, rel_succ_pred,
                            mod_rel_pcd_corr, mod_elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
                            pcd_optimal_race)),
  
  label = predicting_all %>%
    filter(year < 2019 & bunch_sprint == 0) %>%
    mutate(tmldr = tm_pos == 1) %>%
    select(tmldr) %>%
    .[[1]],
  
  weight = predicting_all %>%
    filter(year < 2019 & bunch_sprint == 0) %>%
    mutate(predicted_bs = 1-predicted_bs) %>%
    select(predicted_bs) %>%
    .[[1]]
  
)

#
# test

xgb.test_BS0 <- xgb.DMatrix(
  
  data = as.matrix(predicting_all %>%
                     filter(year >= 2019 & bunch_sprint == 0) %>%
                     select(rel_team_tmldr, No1_Team, rel_glmer_pred, rel_elite, No1_Team_succ, rel_succ_pred,
                            mod_rel_pcd_corr, mod_elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
                            pcd_optimal_race)),
  
  label = predicting_all %>%
    filter(year >= 2019 & bunch_sprint == 0) %>%
    mutate(tmldr = tm_pos == 1) %>%
    select(tmldr) %>%
    .[[1]],
  
  weight = predicting_all %>%
    filter(year >= 2019 & bunch_sprint == 0) %>%
    mutate(predicted_bs = 1-predicted_bs) %>%
    select(predicted_bs) %>%
    .[[1]])

#
# outline parameters

params <- list(
  
  booster = "gbtree",
  eta = 0.3,
  max_depth = 4,
  gamma = 0,
  subsample = 1,
  colsample_bytree = 1,
  tree_method = "hist",
  objective = "binary:logistic"
  
)

# run xgboost model

gbm_model_BS0 <- xgb.train(params = params,
                       data = xgb.train_BS0,
                       nrounds = 10000,
                       nthreads = 4,
                       early_stopping_rounds = 1000,
                       watchlist = list(val1 = xgb.train_BS0,
                                        val2 = xgb.test_BS0),
                       verbose = 1)

# xgb Importance

xgb.importance(model = gbm_model_BS0)

gbm_model_BS0$best_score

#
#
# this outputs GBM predictions for all data

gbm_predict_TMLDR_BS0 = cbind(
  
  
  pred = predict(gbm_model_BS0, 
                 as.matrix(predicting_all %>%
                             filter(year >= 2019 & bunch_sprint == 0) %>%
                             select(rel_team_tmldr, No1_Team, rel_glmer_pred, rel_elite, No1_Team_succ, rel_succ_pred,
                                    mod_rel_pcd_corr, mod_elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
                                    pcd_optimal_race), reshape=T)),
  
  predicting_all %>%
    filter(year >= 2019 & bunch_sprint == 0) %>%
    select(stage, race, year, rider, team, pred_climb_difficulty, rel_team_tmldr, No1_Team, 
           rel_glmer_pred, rel_elite, No1_Team_succ, rel_succ_pred,
           mod_rel_pcd_corr, elite_pcd_corr, pcd_succ_vs_race, sof_vs_team, 
           tm_pos, win, predicted_bs, one_day_race, pcd_optimal_race))


#
# Team Leader predictions weighting using predicted_bs
#

gbm_predict_TMLDR <- rbind(
  
  gbm_predict_TMLDR_BS0 %>%
    mutate(predicted_bs = 1 - predicted_bs) %>%
    mutate(BSTYPE = 0),
  
  gbm_predict_TMLDR_BS1 %>%
    mutate(BSTYPE = 1)
  
) %>%
  
  group_by(stage, race, year, team, BSTYPE) %>%
  mutate(pred2 = pred / sum(pred, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(pred = predicted_bs * pred,
         pred2 = predicted_bs * pred2) %>%
  unique() %>%
  
  group_by(stage, race, year, rider, team, win, tm_pos, pred_climb_difficulty, one_day_race) %>%
  summarize(pred_tmldr = sum(pred2, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(stage, race, year, team) %>%
  filter(n()>1) %>%
  ungroup()

#

ggplot(gbm_predict_TMLDR, 
       aes(x = pred_tmldr, y = as.numeric(tm_pos==1)))+
  
  geom_smooth(se=F)+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1))+
  facet_wrap(~one_day_race)


#
#
#
#
#

# Apply Team Ldr to Win Prob Model ----------------------------------------

# I generate predictions for team leader using both models on the full dataset

predicting_win <- predicting_all %>%
  
  inner_join(
    
    rbind(
      
      cbind(

        pred = predict(gbm_model_BS0, 
                       as.matrix(predicting_all %>%
                                   filter(bunch_sprint == 0) %>%
                                   select(rel_team_tmldr, No1_Team, rel_glmer_pred, rel_elite, No1_Team_succ, rel_succ_pred,
                                          mod_rel_pcd_corr, mod_elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
                                          pcd_optimal_race), reshape=T)),
        
        predicting_all %>%
          filter(bunch_sprint == 0) %>%
          select(stage, race, year, rider, team, pred_climb_difficulty, rel_team_tmldr, No1_Team, 
                 rel_glmer_pred, rel_elite, mod_rel_pcd_corr, mod_elite_pcd_corr,
                 pcd_succ_vs_race, sof_vs_team, rel_succ_pred, No1_Team_succ, 
                 tm_pos, win, predicted_bs, one_day_race, pcd_optimal_race)) %>%
        mutate(predicted_bs = 1 - predicted_bs) %>%
        mutate(BSTYPE = 0),
      
      cbind(
        
        
        pred = predict(gbm_model_BS1, 
                       as.matrix(predicting_all %>%
                                   filter(bunch_sprint == 1) %>%
                                   select(rel_team_tmldr, No1_Team, rel_glmer_pred, rel_elite, No1_Team_succ, rel_succ_pred,
                                          mod_rel_pcd_corr, mod_elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
                                          pcd_optimal_race), reshape=T)),
        
        predicting_all %>%
          filter(bunch_sprint == 1) %>%
          select(stage, race, year, rider, team, pred_climb_difficulty, rel_team_tmldr, No1_Team, 
                 rel_glmer_pred, rel_elite, mod_rel_pcd_corr, mod_elite_pcd_corr,
                 pcd_succ_vs_race, sof_vs_team, rel_succ_pred, No1_Team_succ, 
                 tm_pos, win, predicted_bs, one_day_race, pcd_optimal_race)) %>%
        mutate(BSTYPE = 1)
      
    ) %>%
      
      group_by(stage, race, year, team, BSTYPE) %>%
      mutate(pred2 = pred / sum(pred, na.rm = T)) %>%
      ungroup() %>%
      rename(modeled_tmldr = pred2) %>%
      select(stage, race, year, team, rider, BSTYPE, modeled_tmldr), 
    by = c("stage", "race", "year", "team", "rider", "bunch_sprint" = "BSTYPE")
    
  ) %>%
  unique()

#
# moving onto win probability
#

#
#
# regression for wins
#
#

train_BS1_W <- predicting_win %>%
  filter(year < 2019 & bunch_sprint == 1)

test_BS1_W <- predicting_win %>%
  filter(year >= 2019 & bunch_sprint == 1)

####
####
####
####

cols <- c("modeled_tmldr", "rank_ppo", "rel_elite", "rel_in_pack", "rel_pcd_abs_diff",
          "mod_rel_pcd_corr", "rel_succ_pred", "pcd_succ_vs_race", "rel_sof_race",
          "pcd_optimal_race", "teammates_rel_sof")

pre_proc_val <- caret::preProcess(train_BS1_W[,cols], method = c("center", "scale"))

train_BS1_W[,cols] = predict(pre_proc_val, train_BS1_W[,cols])
test_BS1_W[,cols] = predict(pre_proc_val, test_BS1_W[,cols])

#
#
# run logistic regression on each variable

for(v in 1:length(cols)) {
  
  print(cols[[v]])
  
  # run single variable logistic model
  mod <- glm(win ~ .,
             data = train_BS1_W[, c(cols[[v]], "win")],
             weights = train_BS1_W$predicted_bs,
             family = "binomial")
  
  # summary
  summary(mod)
  
  # predict test set out of sample
  pred <- cbind(coef = predict(mod, test_BS1_W),
                test_BS1_W) %>%
    mutate(pred = exp(coef)/(1+exp(coef)))
  
  # brier score for accuracy
  brierScore <- mean((pred$pred-pred$win)^2)
  
  print(brierScore)
  
  # ROC / AUC
  roc_obj <- pROC::roc(pred$win, pred$pred)
  print(pROC::auc(roc_obj))
  
  ggsave(
    filename = paste0("Images/bs1", cols[[v]], ".png"),
    plot = pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
      labs(title = cols[[v]], subtitle = pROC::auc(roc_obj))
  )
  
}

#
#
# run full logistic regression model

# run single variable logistic model
mod <- glm(win ~ .,
           data = train_BS1_W[, c(cols, "win")],
           weights = train_BS1_W$predicted_bs,
           family = "binomial")

# summary
summary(mod)

# predict test set out of sample
pred <- cbind(coef = predict(mod, test_BS1_W),
              test_BS1_W) %>%
  mutate(pred = exp(coef)/(1+exp(coef)))

# ROC / AUC
roc_obj <- pROC::roc(pred$win, pred$pred)
print(pROC::auc(roc_obj))

ggsave(
  filename = paste0("Images/bs1-all.png"),
  plot = pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
    labs(title = "all variables", subtitle = pROC::auc(roc_obj))
)

#
#
# set up LASSO model
#
#

cols <- c("modeled_tmldr", "rank_ppo", "rel_elite", "rel_in_pack", "rel_pcd_abs_diff",
          "mod_rel_pcd_corr", "rel_succ_pred", "pcd_succ_vs_race", "rel_sof_race", "win",
          "pcd_optimal_race", "teammates_rel_sof")

TRdummies <- caret::dummyVars(win ~ ., data = train_BS1_W[,cols])

TSdummies <- caret::dummyVars(win ~ ., data = test_BS1_W[,cols])

train_dummies = predict(TRdummies, newdata = train_BS1_W[,cols])

test_dummies = predict(TSdummies, newdata = test_BS1_W[,cols])

# set up matrices

x = as.matrix(train_dummies)
y_train = train_BS1_W$win

x_test = as.matrix(test_dummies)
y_test = test_BS1_W$win

# run to find optimal lambda

lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- glmnet::cv.glmnet(x, 
                               y_train,
                               alpha = 1, 
                               lambda = lambdas, 
                               weights = train_BS1_W$predicted_bs,
                               family = "binomial", 
                               standardize = TRUE, 
                               nfolds = 5)

# Best 
lambda_best <- lasso_reg$lambda.min 

# run the lasso model

lasso_model_BS1 <- glmnet::glmnet(x, 
                              y_train, 
                              alpha = 1, 
                              lambda = lambda_best, 
                              weights = train_BS1_W$predicted_bs,
                              standardize = TRUE, 
                              family = "binomial")

#

lasso_model_BS1$beta

#

predictions_train_BS1 <- cbind(coef = predict(lasso_model_BS1, s = lambda_best, newx = x),
                           train_BS1_W) %>%
  rename(pred = `1`) %>%
  mutate(pred = exp(pred) / (1+exp(pred)))

predictions_test_BS1 <- cbind(coef = predict(lasso_model_BS1, s = lambda_best, newx = x_test),
                          test_BS1_W) %>%
  rename(pred = `1`) %>%
  mutate(pred = exp(pred) / (1+exp(pred)))

# ROC / AUC
roc_obj <- pROC::roc(predictions_test_BS1$win, predictions_test_BS1$pred)
print(pROC::auc(roc_obj))

ggsave(
  filename = paste0("Images/bs1-all-lasso.png"),
  plot = pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
    labs(title = "all variables", subtitle = pROC::auc(roc_obj))
)

#
# bunch_sprint == 0
#

train_BS0_W <- predicting_win %>%
  filter(year < 2019 & bunch_sprint == 0)

test_BS0_W <- predicting_win %>%
  filter(year >= 2019 & bunch_sprint == 0)

####
####
####
####

cols <- c("modeled_tmldr", "rank_ppo", "rel_elite", "rel_in_pack", "rel_pcd_abs_diff",
          "mod_rel_pcd_corr", "rel_succ_pred", "pcd_succ_vs_race", "rel_sof_race",
          "mod_elite_pcd_corr", "pcd_optimal_race", "teammates_rel_sof")

pre_proc_val <- caret::preProcess(train_BS0_W[,cols], method = c("center", "scale"))

train_BS0_W[,cols] = predict(pre_proc_val, train_BS0_W[,cols])
test_BS0_W[,cols] = predict(pre_proc_val, test_BS0_W[,cols])

#
#
# run logistic regression on each variable

for(v in 1:length(cols)) {
  
  print(cols[[v]])
  
  # run single variable logistic model
  mod <- glm(win ~ .,
             data = train_BS0_W[, c(cols[[v]], "win")],
             weights = 1 - train_BS0_W$predicted_bs,
             family = "binomial")
  
  # summary
  summary(mod)
  
  # predict test set out of sample
  pred <- cbind(coef = predict(mod, test_BS1_W),
                test_BS1_W) %>%
    mutate(pred = exp(coef)/(1+exp(coef)))
  
  # brier score for accuracy
  brierScore <- mean((pred$pred-pred$win)^2)
  
  print(brierScore)
  
  # ROC / AUC
  roc_obj <- pROC::roc(pred$win, pred$pred)
  print(pROC::auc(roc_obj))
  
  ggsave(
    filename = paste0("Images/bs0", cols[[v]], ".png"),
    plot = pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
      labs(title = cols[[v]], subtitle = pROC::auc(roc_obj))
  )
  
}

#
#
# run full logistic regression model

mod <- glm(win ~ .,
           data = train_BS0_W[, c(cols, "win")],
           weights = 1 - train_BS0_W$predicted_bs,
           family = "binomial")

# summary
summary(mod)

# predict test set out of sample
pred <- cbind(coef = predict(mod, test_BS1_W),
              test_BS1_W) %>%
  mutate(pred = exp(coef)/(1+exp(coef)))

# ROC / AUC
roc_obj <- pROC::roc(pred$win, pred$pred)
print(pROC::auc(roc_obj))

ggsave(
  filename = paste0("Images/bs0-all.png"),
  plot = pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
    labs(title = cols[[v]], subtitle = pROC::auc(roc_obj))
)

#
#
# set up LASSO model
#
#

cols <- c("modeled_tmldr", "rank_ppo", "rel_elite", "rel_in_pack", "rel_pcd_abs_diff",
          "mod_rel_pcd_corr", 
          "rel_succ_pred", "pcd_succ_vs_race", "rel_sof_race", "win", "pcd_optimal_race",
          "teammates_rel_sof")

TRdummies <- caret::dummyVars(win ~ ., data = train_BS0_W[,cols])

TSdummies <- caret::dummyVars(win ~ ., data = test_BS0_W[,cols])

train_dummies = predict(TRdummies, newdata = train_BS0_W[,cols])

test_dummies = predict(TSdummies, newdata = test_BS0_W[,cols])

# set up matrices

x = as.matrix(train_dummies)
y_train = train_BS0_W$win

x_test = as.matrix(test_dummies)
y_test = test_BS0_W$win

# I STILL NEED TO ADD WEIGHTS TO THIS PART

# run to find optimal lambda

lambdas <- 10^seq(2, -3, by = -.1)

lambdas <- c(lambdas, seq(0.01, 0.1, 0.01)) %>% unique()

# Setting alpha = 1 implements lasso regression
lasso_reg <- glmnet::cv.glmnet(x, 
                               y_train,
                               alpha = 1, 
                               lambda = lambdas, 
                               weights = train_BS0_W$predicted_bs,
                               family = "binomial", 
                               standardize = TRUE, 
                               nfolds = 5)

# Best 
lambda_best <- lasso_reg$lambda.min 

# run the lasso model

lasso_model_BS0 <- glmnet::glmnet(x, 
                              y_train, 
                              alpha = 1, 
                              lambda = lambda_best, 
                              weights = train_BS0_W$predicted_bs,
                              standardize = TRUE, 
                              family = "binomial")

#

lasso_model_BS0$beta

#

predictions_train_BS0 <- cbind(coef = predict(lasso_model_BS0, s = lambda_best, newx = x),
                           train_BS0_W) %>%
  rename(pred = `1`) %>%
  mutate(pred = exp(pred) / (1+exp(pred)))

predictions_test_BS0 <- cbind(coef = predict(lasso_model_BS0, s = lambda_best, newx = x_test),
                          test_BS0_W) %>%
  rename(pred = `1`) %>%
  mutate(pred = exp(pred) / (1+exp(pred)))

# ROC / AUC
roc_obj <- pROC::roc(predictions_test_BS0$win, predictions_test_BS0$pred)
print(pROC::auc(roc_obj))

ggsave(
  filename = paste0("Images/bs0-all-lasso.png"),
  plot = pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
    labs(title = "all variables", subtitle = pROC::auc(roc_obj))
)

#
#
# naive adjustments below (three team leaders)
#
#

predictions_test_BS0_ADJ <- predictions_test_BS0 %>%
  
  # 1. reduce win probabilities for riders ranked >3 on their team to 0
  # and re-distribute within the team
  group_by(team, race, stage, year) %>%
  mutate(win_prob = ifelse(rank(-pred, ties.method = "min")<=3, pred, 0)) %>%
  mutate(win_prob = win_prob / sum(win_prob, na.rm = T) * sum(pred, na.rm = T)) %>%
  ungroup() %>%
  
  # 2. then adjust so that win_prob / sum(win_prob) and everything = 1
  group_by(race, stage, year) %>%
  mutate(win_prob = win_prob / sum(win_prob, na.rm = T)) %>%
  ungroup() %>%
  
  # 3. select relevant data and remove the rest
  select(rider, team, win_prob, stage, race, year, pred_climb_difficulty, predicted_bs, rnk,
         modeled_tmldr, pcd_optimal_race, rank_ppo, rel_elite, rel_sof_race)

#
#
# naive adjustments below for BS==1 (only two team leaders)
#
#

predictions_test_BS1_ADJ <- predictions_test_BS1 %>%
  
  # 1. reduce win probabilities for riders ranked >2 on their team to 0
  # and re-distribute within the team
  group_by(team, race, stage, year) %>%
  mutate(win_prob = ifelse(rank(-pred, ties.method = "min")<=2, pred, 0)) %>%
  mutate(win_prob = win_prob / sum(win_prob, na.rm = T) * sum(pred, na.rm = T)) %>%
  ungroup() %>%
  
  # 2. then adjust so that win_prob / sum(win_prob) and everything = 1
  group_by(race, stage, year) %>%
  mutate(win_prob = win_prob / sum(win_prob, na.rm = T)) %>%
  ungroup() %>%
  
  # 3. select relevant data and remove the rest
  select(rider, team, win_prob, stage, race, year, pred_climb_difficulty, predicted_bs, rnk,
         modeled_tmldr, pcd_optimal_race, rank_ppo, rel_elite, rel_sof_race)


#
#
# below I adjust BS==0 and BS==1 so that everything adds up to 1
#
#

# BS==0

preds_races_All_0 <- predictions_test_BS0 %>%
  select(stage, race, year) %>%
  unique()

adjust_win_probs_All_0 <- vector("list", length(preds_races_All_0$race))

for(x in 1:length(preds_races_All_0$race)) {
  
  R = preds_races_All_0$race[[x]]
  S = preds_races_All_0$stage[[x]]
  Y = preds_races_All_0$year[[x]]
  
  d <- predictions_test_BS0 %>%
    filter(stage == S & race == R & year == Y) %>%
    mutate(odds = ((1-pred)/pred)+1)
  
  imp <- implied::implied_probabilities(d$odds, method = "or")
  
  res <- imp$probabilities
  
  res <- res %>% as.data.frame() %>% gather(rownum, new_win_prob)
  
  out <- cbind(d, res) %>%
    select(-rownum)
  
  adjust_win_probs_All_0[[x]] <- out
  
}

#
#
#

adjusted_preds_All <- bind_rows(adjust_win_probs_All_0) %>%
  select(
    
    rider,
    team,
    race,
    stage, 
    year, 

    win_prob = new_win_prob)

#
# write to table
#

#dbWriteTable(con, "predictions_basic_climbing", adjusted_preds_C, row.names = F, append = T)
