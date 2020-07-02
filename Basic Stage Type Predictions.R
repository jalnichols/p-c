
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

BS_dates <- stage_data_perf %>%
  filter(!is.na(bunch_sprint)) %>%
  mutate(length = length - 200) %>%
  
  select(-data) %>%
  select(-new_st, -missing_profile_data, -position_highest, -last_climb, -act_climb_difficulty, 
         -raw_climb_difficulty, -number_cat_climbs, -concentration, -cat_climb_length, 
         -final_1km_gradient, -total_vert_gain, -final_20km_vert_gain, -perc_elev_change,
         -gain_back_5, -back_5_seconds, -limit, -success_time, -solo, -rel_success, -url, 
         -summit_finish, -gc_seconds, -rel_speed, -top_variance, -variance) %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  filter(year > 2014) %>%
  select(date) %>%
  unique()

BS_data <- stage_data_perf %>%
  filter(!is.na(bunch_sprint)) %>%
  
  select(-data) %>%
  select(-new_st, -missing_profile_data, -position_highest, -last_climb, -act_climb_difficulty, 
         -raw_climb_difficulty, -number_cat_climbs, -concentration, -cat_climb_length, 
         -final_1km_gradient, -total_vert_gain, -final_20km_vert_gain, -perc_elev_change,
         -gain_back_5, -back_5_seconds, -limit, -success_time, -solo, -rel_success, -url, 
         -summit_finish, -gc_seconds, -rel_speed, -top_variance, -variance) %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  
  mutate(points_per_opp = ifelse(tm_pos == 1, points_finish, NA),
         sof_per_opp = ifelse(tm_pos == 1, sof, NA),
         pred_climb_diff_opp = ifelse(tm_pos == 1, pred_climb_difficulty, NA)) %>%
  
  mutate(date = as.Date(date)) %>%
  
  filter(bunch_sprint == 1) %>%
  
  select(-stage_name, -speed, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th, -gain_1st,
         -time_trial, -gc_winner, -gain_gc, -gc_pos, -NEW, -parcours_value, -stage_type) %>%
  
  filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC")) |
           (class %in% c("2.1", "1.1") & Tour == "Europe Tour") | 
           (sof > 0.25 & class %in% c("2.1", "1.1")))

#

for(b in 1:length(BS_dates$date)) {
  
  # one day before predicting date and two years back
  maxD <- as.Date(BS_dates$date[[b]]) - 1
  minD <- maxD - 730
  
  BS_riders <- BS_data %>%
    filter(between(date, minD, maxD)==TRUE) %>%
    
    group_by(rider) %>%
    summarize(
      
      team_leader = mean(tm_pos == 1, na.rm = T),
      domestique = mean(tm_pos >= 4, na.rm = T),
      
      points_per_opp = mean(points_per_opp, na.rm = T),
      points_per_race = mean(points_finish, na.rm = T),
      
      sof_leader = mean(sof_per_opp, na.rm = T),
      sof_overall = mean(sof, na.rm = T),
      
      pcd_leader = mean(pred_climb_diff_opp, na.rm = T),
      pcd_overall = mean(pred_climb_difficulty, na.rm = T),
      
      opportunities = sum(tm_pos == 1, na.rm = T),
      races = n()) %>%
    ungroup() %>%
    
    # what date are we predicting
    mutate(Date = as.Date(maxD + 1)) %>%
    
    gather(stat, value, team_leader:pcd_overall) %>%
    
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    
    spread(stat, value)
  
  ################################################
  
  dbWriteTable(con, "performance_rider_bunchsprint", BS_riders, append = TRUE, row.names = FALSE)
  
  ################################################
  
  BS_teams <- BS_data %>%
    filter(between(date, minD, maxD)==TRUE) %>%
    
    filter(tm_pos == 1) %>%
    
    group_by(master_team) %>%
    summarize(
      
      team_points_per_race = mean(points_finish, na.rm = T),
      
      team_sof_overall = mean(sof, na.rm = T),
      
      team_pcd_overall = mean(pred_climb_difficulty, na.rm = T),

      team_races = n()) %>%
    ungroup() %>%
    
    # what date are we predicting
    mutate(Date = as.Date(maxD + 1)) %>%
    
    mutate(team_points_per_race = ifelse(is.na(team_points_per_race), 0, team_points_per_race))
  
  ################################################
  
  dbWriteTable(con, "performance_team_bunchsprint", BS_teams, append = TRUE, row.names = FALSE)
  
  ################################################
  
  print(maxD)
  
}

##################################################

bunch_sprints_performance_table <- dbReadTable(con, "performance_rider_bunchsprint") %>%
  
  mutate(date = as.Date(Date)) %>%
  select(-Date)

##################################################

predicting_bunch_sprints <- BS_data %>%
  
  filter(class %in% c("1.UWT", "2.UWT", "WC", "CC") | (class %in% c("1.HC", "2.HC", "1.Pro", "2.Pro") & Tour == 'Europe Tour')) %>%
  
  select(-points_per_opp, -sof_per_opp, -pred_climb_diff_opp) %>%
  
  left_join(
    
    bunch_sprints_performance_table, by = c("rider", "date")) %>%
  
  #
  # change so NAs are replaced by zeroes
  #
  
  mutate(points_per_opp = (0.12 + (opportunities * points_per_opp)) / (12 + opportunities)) %>%
  
  group_by(stage, race, year) %>%
  mutate(field_points_per_race = mean(points_per_race, na.rm = T),
         field_team_leader = mean(team_leader, na.rm = T),
         field_domestique = mean(domestique, na.rm = T),
         field_sof_overall = mean(sof_overall, na.rm = T),
         
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
         team_ppo_rank = rank(-points_per_opp, ties.method = "min")) %>%
  ungroup() %>%
  
  mutate(win = rnk == 1,
         rel_ppo = points_per_opp - field_points_per_race,
         rel_team_tmldr = team_leader - team_team_leader,
         rel_team_domestique = domestique - team_domestique)

##################################################

# win probability

# three pillars of win prob for bunch sprints
# 1. ranking well vs others in race
# 2. a lot of success in opportunities
# 3. more of team leader than other teammates

win_model <- glm(win ~ rel_team_tmldr + rank_ppo + rel_elite,
                 
                 data = predicting_bunch_sprints,
                 
                 family = "binomial")

summary(win_model)

#

preds <- cbind(
  
  win_prob = predict(win_model, predicting_bunch_sprints),
  
  predicting_bunch_sprints
  
) %>%
  
  mutate(win_prob = exp(win_prob) / (1+exp(win_prob))) %>%
  
  group_by(stage, race, year) %>%
  mutate(total_win_prob = sum(win_prob, na.rm = T)) %>%
  ungroup()

#

preds_races <- preds %>%
  select(stage, race, year) %>%
  unique()

adjust_win_probs <- vector("list", length(preds_races$race))

for(x in 1:length(preds_races$race)) {
  
  R = preds_races$race[[x]]
  S = preds_races$stage[[x]]
  Y = preds_races$year[[x]]
  
  d <- preds %>%
    filter(stage == S & race == R & year == Y) %>%
    mutate(odds = ((1-win_prob)/win_prob)+1)
  
  imp <- implied::implied_probabilities(d$odds, method = "or")
  
  res <- imp$probabilities
  
  res <- res %>% as.data.frame() %>% gather(rownum, new_win_prob)
  
  out <- cbind(d, res) %>%
    select(-rownum)
  
  adjust_win_probs[[x]] <- out
  
}

#
#
#

adjusted_preds <- bind_rows(adjust_win_probs) %>%
  select(
    
    rider,
    team,
    race,
    stage, 
    year, 
    
    rel_team_tmldr,
    rel_elite,
    rank_ppo,
    
    win_prob = new_win_prob)

#
# write to table
#

dbWriteTable(con, "predictions_basic_bunchsprint", adjusted_preds, row.names = F, append = T)

#
#
# Probability of being team leader
#
#

tmldr_model <- glm(tmldr ~ rel_team_tmldr + rel_elite + rel_team_domestique + team_ppo_rank,
                 
                 data = predicting_bunch_sprints %>%
                   mutate(tmldr = tm_pos == 1),
                 
                 family = "binomial")

summary(tmldr_model)

#

preds_tmldr <- cbind(
  
  tmldr_prob = predict(tmldr_model, predicting_bunch_sprints),
  
  predicting_bunch_sprints
  
) %>%
  
  mutate(tmldr_prob = exp(tmldr_prob) / (1+exp(tmldr_prob))) %>%
  
  group_by(stage, race, year, team) %>%
  mutate(total_tmldr = sum(tmldr_prob, na.rm = T)) %>%
  ungroup()

#
#
#
#
#
#
#

# Other Races -------------------------------------------------------------


#
#
#

# going to start very easy with rankings of bunch_sprint performance

BS_dates <- stage_data_perf %>%
  filter(!is.na(bunch_sprint)) %>%
  
  select(-data) %>%
  select(-new_st, -missing_profile_data, -position_highest, -last_climb, -act_climb_difficulty, 
         -raw_climb_difficulty, -number_cat_climbs, -concentration, -cat_climb_length, 
         -final_1km_gradient, -total_vert_gain, -final_20km_vert_gain, -perc_elev_change,
         -gain_back_5, -back_5_seconds, -limit, -success_time, -solo, -rel_success, -url, 
         -summit_finish, -gc_seconds, -rel_speed, -top_variance, -variance) %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  filter(year > 2014) %>%
  select(date) %>%
  unique()

#

OTH_data <- stage_data_perf %>%
  filter(!is.na(bunch_sprint)) %>%
  
  select(-data) %>%
  select(-new_st, -missing_profile_data, -position_highest, -last_climb, -act_climb_difficulty, 
         -raw_climb_difficulty, -number_cat_climbs, -concentration, -cat_climb_length, 
         -final_1km_gradient, -total_vert_gain, -final_20km_vert_gain, -perc_elev_change,
         -gain_back_5, -back_5_seconds, -limit, -success_time, -solo, -rel_success, -url, 
         -summit_finish, -gc_seconds, -rel_speed, -top_variance, -variance) %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  
  mutate(points_per_opp = ifelse(tm_pos == 1, points_finish, NA),
         sof_per_opp = ifelse(tm_pos == 1, sof, NA),
         pred_climb_diff_opp = ifelse(tm_pos == 1, pred_climb_difficulty, NA)) %>%
  
  mutate(date = as.Date(date)) %>%
  
  filter(bunch_sprint == 0 & pred_climb_difficulty <= 8.4) %>%
  
  select(-stage_name, -speed, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th,
         -time_trial, -gc_winner, -gain_gc, -gc_pos, -NEW, -parcours_value, -stage_type) %>%
  
  filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC")) |
           (class %in% c("2.1", "1.1") & Tour == "Europe Tour") | 
           (sof > 0.25 & class %in% c("2.1", "1.1")))

#

for(b in 1:length(BS_dates$date)) {
  
  # one day before predicting date and two years back
  maxD <- as.Date(BS_dates$date[[b]]) - 1
  minD <- maxD - 730
  
  OTH_riders <- OTH_data %>%
    filter(between(date, minD, maxD)==TRUE) %>%
    
    group_by(rider) %>%
    summarize(
      
      team_leader = mean(tm_pos == 1, na.rm = T),
      domestique = mean(tm_pos >= 4, na.rm = T),
      in_pack = mean(gain_1st <= 5, na.rm = T),
      
      points_per_opp = mean(points_per_opp, na.rm = T),
      points_per_race = mean(points_finish, na.rm = T),
      
      sof_leader = mean(sof_per_opp, na.rm = T),
      sof_overall = mean(sof, na.rm = T),
      
      pcd_leader = mean(pred_climb_diff_opp, na.rm = T),
      pcd_overall = mean(pred_climb_difficulty, na.rm = T),
      
      opportunities = sum(tm_pos == 1, na.rm = T),
      races = n()) %>%
    ungroup() %>%
    
    # what date are we predicting
    mutate(Date = as.Date(maxD + 1)) %>%
    
    gather(stat, value, team_leader:pcd_overall) %>%
    
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    
    spread(stat, value)
  
  ################################################
  
  dbWriteTable(con, "performance_rider_hills", OTH_riders, append = TRUE, row.names = FALSE)
  
  ################################################
  
  OTH_teams <- OTH_data %>%
    filter(between(date, minD, maxD)==TRUE) %>%
    
    filter(tm_pos == 1) %>%
    
    group_by(master_team) %>%
    summarize(
      
      team_points_per_race = mean(points_finish, na.rm = T),
      in_pack = mean(gain_1st <= 5, na.rm = T),
      team_sof_overall = mean(sof, na.rm = T),
      
      team_pcd_overall = mean(pred_climb_difficulty, na.rm = T),
      
      team_races = n()) %>%
    ungroup() %>%
    
    # what date are we predicting
    mutate(Date = as.Date(maxD + 1)) %>%
    
    mutate(team_points_per_race = ifelse(is.na(team_points_per_race), 0, team_points_per_race))
  
  ################################################
  
  dbWriteTable(con, "performance_team_hills", OTH_teams, append = TRUE, row.names = FALSE)
  
  ################################################
  
  print(maxD)
  
}

##################################################

hills_performance_table <- dbReadTable(con, "performance_rider_hills") %>%
  
  mutate(date = as.Date(Date)) %>%
  select(-Date)

##################################################

predicting_hills <- OTH_data %>%
  
  filter(class %in% c("1.UWT", "2.UWT", "WC", "CC") | (class %in% c("1.HC", "2.HC", "1.Pro", "2.Pro") & Tour == 'Europe Tour')) %>%
  
  select(-points_per_opp, -sof_per_opp, -pred_climb_diff_opp) %>%
  
  inner_join(
    
    hills_performance_table, by = c("rider", "date")) %>%
  
  #
  # change so NAs are replaced by zeroes
  #
  
  mutate(points_per_opp = (0.12 + (opportunities * points_per_opp)) / (12 + opportunities)) %>%
  
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
         team_ppo_rank = rank(-points_per_opp, ties.method = "min")) %>%
  ungroup() %>%
  
  mutate(win = rnk == 1,
         rel_ppo = points_per_opp - field_points_per_race,
         rel_team_tmldr = team_leader - team_team_leader,
         rel_team_domestique = domestique - team_domestique)

##################################################

# win probability


win_model_H <- glm(win ~ rel_team_tmldr + rank_ppo + rel_elite + rel_in_pack,
                 
                 data = predicting_hills,
                 
                 family = "binomial")

summary(win_model_H)

#

preds_H <- cbind(
  
  win_prob = predict(win_model_H, predicting_hills),
  
  predicting_hills
  
) %>%
  
  mutate(win_prob = exp(win_prob) / (1+exp(win_prob))) %>%
  
  group_by(stage, race, year) %>%
  mutate(total_win_prob = sum(win_prob, na.rm = T)) %>%
  ungroup()

#

preds_races_H <- preds_H %>%
  select(stage, race, year) %>%
  unique()

adjust_win_probs_H <- vector("list", length(preds_races_H$race))

for(x in 1:length(preds_races_H$race)) {
  
  R = preds_races_H$race[[x]]
  S = preds_races_H$stage[[x]]
  Y = preds_races_H$year[[x]]
  
  d <- preds_H %>%
    filter(stage == S & race == R & year == Y) %>%
    mutate(odds = ((1-win_prob)/win_prob)+1)
  
  imp <- implied::implied_probabilities(d$odds, method = "or")
  
  res <- imp$probabilities
  
  res <- res %>% as.data.frame() %>% gather(rownum, new_win_prob)
  
  out <- cbind(d, res) %>%
    select(-rownum)
  
  adjust_win_probs_H[[x]] <- out
  
}

#
#
#

adjusted_preds_H <- bind_rows(adjust_win_probs_H) %>%
  select(
    
    rider,
    team,
    race,
    stage, 
    year, 
    
    rel_team_tmldr,
    rel_elite,
    rank_ppo,
    in_pack,
    
    win_prob = new_win_prob)

#
# write to table
#

dbWriteTable(con, "predictions_basic_hills", adjusted_preds_H, row.names = F, append = T)

#
#
# Probability of being team leader
#
#

tmldr_model_H <- glm(tmldr ~ rel_team_tmldr + rel_elite + rel_team_domestique + team_ppo_rank,
                   
                   data = predicting_hills %>%
                     mutate(tmldr = tm_pos == 1),
                   
                   family = "binomial")

summary(tmldr_model_H)

#

preds_tmldr_H <- cbind(
  
  tmldr_prob = predict(tmldr_model_H, predicting_hills),
  
  predicting_hills
  
) %>%
  
  mutate(tmldr_prob = exp(tmldr_prob) / (1+exp(tmldr_prob))) %>%
  
  group_by(stage, race, year, team) %>%
  mutate(total_tmldr = sum(tmldr_prob, na.rm = T)) %>%
  ungroup()

#
#
#
#
#
#
#
#


# Climbing Races -------------------------------------------------------------

#
#
#

BS_dates <- stage_data_perf %>%
  filter(!is.na(bunch_sprint)) %>%
  
  select(-data) %>%
  select(-new_st, -missing_profile_data, -position_highest, -last_climb, -act_climb_difficulty, 
         -raw_climb_difficulty, -number_cat_climbs, -concentration, -cat_climb_length, 
         -final_1km_gradient, -total_vert_gain, -final_20km_vert_gain, -perc_elev_change,
         -gain_back_5, -back_5_seconds, -limit, -success_time, -solo, -rel_success, -url, 
         -summit_finish, -gc_seconds, -rel_speed, -top_variance, -variance) %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  filter(year > 2014) %>%
  select(date) %>%
  unique()

#

CLM_data <- stage_data_perf %>%
  filter(!is.na(bunch_sprint)) %>%
  
  select(-data) %>%
  select(-new_st, -missing_profile_data, -position_highest, -last_climb, -act_climb_difficulty, 
         -raw_climb_difficulty, -number_cat_climbs, -concentration, -cat_climb_length, 
         -final_1km_gradient, -total_vert_gain, -final_20km_vert_gain, -perc_elev_change,
         -gain_back_5, -back_5_seconds, -limit, -success_time, -solo, -rel_success, -url, 
         -summit_finish, -gc_seconds, -rel_speed, -top_variance, -variance) %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  
  mutate(points_per_opp = ifelse(tm_pos == 1, points_finish, NA),
         sof_per_opp = ifelse(tm_pos == 1, sof, NA),
         pred_climb_diff_opp = ifelse(tm_pos == 1, pred_climb_difficulty, NA)) %>%
  
  mutate(date = as.Date(date)) %>%
  
  filter(bunch_sprint == 0 & pred_climb_difficulty > 8.4) %>%
  
  select(-stage_name, -speed, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th,
         -time_trial, -gc_winner, -gc_pos, -NEW, -parcours_value, -stage_type) %>%
  
  filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC")) |
           (class %in% c("2.1", "1.1") & Tour == "Europe Tour") | 
           (sof > 0.25 & class %in% c("2.1", "1.1")))

#

for(b in 1:length(BS_dates$date)) {
  
  # one day before predicting date and two years back
  maxD <- as.Date(BS_dates$date[[b]]) - 1
  minD <- maxD - 730
  
  CLM_riders <- CLM_data %>%
    filter(between(date, minD, maxD)==TRUE) %>%
    
    group_by(rider) %>%
    summarize(
      
      team_leader = mean(tm_pos == 1, na.rm = T),
      domestique = mean(tm_pos >= 4, na.rm = T),
      in_pack = mean(gain_gc <= 5, na.rm = T),
      
      points_per_opp = mean(points_per_opp, na.rm = T),
      points_per_race = mean(points_finish, na.rm = T),
      
      sof_leader = mean(sof_per_opp, na.rm = T),
      sof_overall = mean(sof, na.rm = T),
      
      pcd_leader = mean(pred_climb_diff_opp, na.rm = T),
      pcd_overall = mean(pred_climb_difficulty, na.rm = T),
      
      opportunities = sum(tm_pos == 1, na.rm = T),
      races = n()) %>%
    ungroup() %>%
    
    # what date are we predicting
    mutate(Date = as.Date(maxD + 1)) %>%
    
    gather(stat, value, team_leader:pcd_overall) %>%
    
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    
    spread(stat, value)
  
  ################################################
  
  dbWriteTable(con, "performance_rider_climbing", CLM_riders, append = TRUE, row.names = FALSE)
  
  ################################################
  
  CLM_teams <- CLM_data %>%
    filter(between(date, minD, maxD)==TRUE) %>%
    
    filter(tm_pos == 1) %>%
    
    group_by(master_team) %>%
    summarize(
      
      team_points_per_race = mean(points_finish, na.rm = T),
      in_pack = mean(gain_1st <= 5, na.rm = T),
      team_sof_overall = mean(sof, na.rm = T),
      
      team_pcd_overall = mean(pred_climb_difficulty, na.rm = T),
      
      team_races = n()) %>%
    ungroup() %>%
    
    # what date are we predicting
    mutate(Date = as.Date(maxD + 1)) %>%
    
    mutate(team_points_per_race = ifelse(is.na(team_points_per_race), 0, team_points_per_race))
  
  ################################################
  
  dbWriteTable(con, "performance_team_climbing", CLM_teams, append = TRUE, row.names = FALSE)
  
  ################################################
  
  print(maxD)
  
}

##################################################

climbing_performance_table <- dbReadTable(con, "performance_rider_climbing") %>%
  
  mutate(date = as.Date(Date)) %>%
  select(-Date)

##################################################

predicting_climbing <- CLM_data %>%
  
  filter(class %in% c("1.UWT", "2.UWT", "WC", "CC") | (class %in% c("1.HC", "2.HC", "1.Pro", "2.Pro") & Tour == 'Europe Tour')) %>%
  
  select(-points_per_opp, -sof_per_opp, -pred_climb_diff_opp) %>%
  
  inner_join(
    
    climbing_performance_table, by = c("rider", "date")) %>%
  
  #
  # change so NAs are replaced by zeroes
  #
  
  mutate(points_per_opp = (0.12 + (opportunities * points_per_opp)) / (12 + opportunities)) %>%
  
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
         team_ppo_rank = rank(-points_per_opp, ties.method = "min")) %>%
  ungroup() %>%
  
  mutate(win = rnk == 1,
         rel_ppo = points_per_opp - field_points_per_race,
         rel_team_tmldr = team_leader - team_team_leader,
         rel_team_domestique = domestique - team_domestique)

##################################################

# win probability

# three pillars of win prob for bunch sprints
# 1. ranking well vs others in race
# 2. a lot of success in opportunities
# 3. more of team leader than other teammates

win_model_C <- glm(win ~ rel_team_tmldr + rank_ppo + rel_elite + rel_in_pack,
                   
                   data = predicting_climbing,
                   
                   family = "binomial")

summary(win_model_C)

#

preds_C <- cbind(
  
  win_prob = predict(win_model_C, predicting_climbing),
  
  predicting_climbing
  
) %>%
  
  mutate(win_prob = exp(win_prob) / (1+exp(win_prob))) %>%
  
  group_by(stage, race, year) %>%
  mutate(total_win_prob = sum(win_prob, na.rm = T)) %>%
  ungroup()

#

preds_races_C <- preds_C %>%
  select(stage, race, year) %>%
  unique()

adjust_win_probs_C <- vector("list", length(preds_races_C$race))

for(x in 1:length(preds_races_C$race)) {
  
  R = preds_races_C$race[[x]]
  S = preds_races_C$stage[[x]]
  Y = preds_races_C$year[[x]]
  
  d <- preds_C %>%
    filter(stage == S & race == R & year == Y) %>%
    mutate(odds = ((1-win_prob)/win_prob)+1)
  
  imp <- implied::implied_probabilities(d$odds, method = "or")
  
  res <- imp$probabilities
  
  res <- res %>% as.data.frame() %>% gather(rownum, new_win_prob)
  
  out <- cbind(d, res) %>%
    select(-rownum)
  
  adjust_win_probs_C[[x]] <- out
  
}

#
#
#

adjusted_preds_C <- bind_rows(adjust_win_probs_C) %>%
  select(
    
    rider,
    team,
    race,
    stage, 
    year, 
    
    rel_team_tmldr,
    rel_elite,
    rank_ppo,
    in_pack,
    
    win_prob = new_win_prob)

#
# write to table
#

dbWriteTable(con, "predictions_basic_climbing", adjusted_preds_C, row.names = F, append = T)


#
#
# Probability of being team leader
#
#

tmldr_model_C <- glm(tmldr ~ rel_team_tmldr + rel_elite + rel_team_domestique + team_ppo_rank,
                     
                     data = predicting_climbing %>%
                       mutate(tmldr = tm_pos == 1),
                     
                     family = "binomial")

summary(tmldr_model_C)

#

preds_tmldr_C <- cbind(
  
  tmldr_prob = predict(tmldr_model_C, predicting_climbing),
  
  predicting_climbing
  
) %>%
  
  mutate(tmldr_prob = exp(tmldr_prob) / (1+exp(tmldr_prob))) %>%
  
  group_by(stage, race, year, team) %>%
  mutate(total_tmldr = sum(tmldr_prob, na.rm = T)) %>%
  ungroup()
