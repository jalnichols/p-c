
library(tidyverse)
library(rvest)
library(DBI)

#

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#
# read in the field
#

field <- 'https://www.procyclingstats.com/race/la-route-d-occitanie/2020/gc/startlist/alphabetical-with-filters' %>%
  read_html() %>%
  html_nodes('table') %>%
  html_table() %>%
  .[[1]] %>%
  janitor::clean_names() %>%
  select(rider = ridername,
         team = team) %>%
  
  mutate(rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT"),
         rider = str_to_title(tolower(rider))) %>%
  
  group_by(team) %>%
  filter(rank(team, ties.method = "first")<= 8) %>%
  ungroup()

#
# enter the conditions
#

conditions <- field %>%
  
  mutate(length = 164,
         pred_climb_difficulty = 20,
         summit_finish = 1,
         one_day_race = 0,
         grand_tour = 0,
         finalGT = 0,
         cobbles = 0,
         perc_thru = 0.5
         ) %>%
  
  mutate(length = length - 200,
         sq_pcd = pred_climb_difficulty ^ 2)

#
# calculate bunch sprint probability
#

conditions_w_bs <- cbind(
  
  conditions,
  
  bs_pred = predict(read_rds("Stored models/bunchsprint-glm-mod.rds"), conditions)) %>%
  
  mutate(predicted_bs = exp(bs_pred)/(1+exp(bs_pred))) %>%
  select(rider, team, pred_climb_difficulty, predicted_bs)

# predicted bunch sprint
conditions_w_bs %>%
  select(predicted_bs) %>%
  .[[1]] %>%
  .[[1]]

#
# add all the performance stats
#

adding_stats <- rbind(
  
  conditions_w_bs %>%
    mutate(bunch_sprint = 0) %>%
    mutate(date = as.Date('2020-04-01')),
  
  conditions_w_bs %>%
    mutate(bunch_sprint = 1) %>%
    mutate(date = as.Date('2020-04-01'))) %>%
  
  inner_join(
    
    performance_table %>%
      select(-first_race, -latest_race) %>%
      mutate(rider = str_to_title(tolower(rider))), by = c("rider", "date", "bunch_sprint")) %>%
  
  # add some regression to the numbers here
  mutate(points_per_opp = (0.05 + (opportunities * points_per_opp)) / (5 + opportunities)) %>%
  
  # this process combines the bs == 1 and bs == 0 stats
  # weighting the combination by predicted_bs and # of races in that category
  gather(stat, value, -rider, -team, -pred_climb_difficulty,
         -bunch_sprint, -predicted_bs, -date, -races) %>%
  
  mutate(multiplier = ifelse(bunch_sprint == 1, predicted_bs, 1-predicted_bs),
         
         value = multiplier * value) %>%
  
  select(-bunch_sprint) %>%
  
  group_by(rider, team, pred_climb_difficulty,
           predicted_bs, date,
           stat) %>%
  summarize(new_value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  
  spread(stat, new_value) %>%
  
  mutate(pcd_success = ((pcd_overall * 1) + (pcd_success * successes * 2) + (pcd_leader * opportunities)) / 
           (opportunities + 1 + (successes * 2))) %>%
  
  mutate(field_points_per_race = mean(points_per_race, na.rm = T),
         field_team_leader = mean(team_leader, na.rm = T),
         field_domestique = mean(domestique, na.rm = T),
         field_sof_overall = mean(sof_overall, na.rm = T),
         rel_in_pack = in_pack - mean(in_pack, na.rm = T),
         rank_ppo = rank(-points_per_opp, ties.method = "min")) %>%

  mutate(top_5 = ifelse(rank_ppo <=5, points_per_opp, NA)) %>%
  
  mutate(strength_elite = mean(top_5, na.rm = T)) %>%
  
  select(-top_5) %>%
  
  mutate(rel_elite = points_per_opp - strength_elite) %>%
  
  group_by(team) %>%
  mutate(team_team_leader = mean(team_leader, na.rm = T),
         team_domestique = mean(domestique, na.rm = T),
         team_ppo_rank = rank(-points_per_opp, ties.method = "min"),
         team_sof = mean(sof_overall, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(
         rel_ppo = points_per_opp - field_points_per_race,
         rel_team_tmldr = team_leader - team_team_leader,
         rel_team_domestique = domestique - team_domestique)  %>%
  
  mutate(pcd_corr_both = (0.5*pcd_corr_pts)+(0.25*pcd_corr_succ)+(0.25*pcd_corr_tmldr)) %>%
  
  mutate(pcd_abs_diff = abs(pred_climb_difficulty - pcd_success),
         pcd_corr = (pred_climb_difficulty - mean(pred_climb_difficulty, na.rm = T)) * pcd_corr_both) %>%
  
  mutate(rel_pcd_abs_diff = mean(pcd_abs_diff) - pcd_abs_diff,
         rel_pcd_corr = pcd_corr - mean(pcd_corr, na.rm = T)) %>%

  mutate(rel_sof_race = sof_overall - mean(sof_overall, na.rm = T)) %>%
  
  mutate(sof_vs_team = sof_overall - team_sof) %>%
  
  # calculate interaction variables
  mutate(elite_pcd_corr = rel_elite * rel_pcd_corr,
         pcd_succ_vs_race = pred_climb_difficulty - pcd_leader) %>%
  
  inner_join(
    
    dbReadTable(con, "lme4_rider_teamleader")  %>%
      
      mutate(rider = str_to_title(rider)) %>%
      mutate(date = as.Date(Date)) %>%
      select(-Date) %>%
      
      # the standard deviations of random intercept and pcd impact both vary widely (increase as you move from 2015 to 2020)
      # we adjust here
      group_by(date) %>%
      mutate(pcd_impact_new = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact),
             random_intercept_new = (random_intercept - mean(random_intercept, na.rm = T)) / sd(random_intercept, na.rm = T)) %>%
      ungroup() %>%
      
      # this transforms them back to input into the regression equation
      mutate(pcd_impact = pcd_impact_new * sd(pcd_impact),
             random_intercept = random_intercept_new * sd(random_intercept)) %>%
      
      select(-pcd_impact_new, -random_intercept_new) %>%
      
      rename(pcd_tmldr_impact = pcd_impact,
             bs_tmldr_impact = bunchsprint_impact) %>%
      
      filter(date <= as.Date('2020-04-01') | date >= as.Date('2020-08-01')), by = c("rider", "date")
    
  ) %>%
  
  inner_join(
    
    lme4_success_table %>%
      
      mutate(rider = str_to_title(rider)) %>%
      
      # the standard deviations of random intercept and pcd impact both vary widely (increase as you move from 2015 to 2020)
      # we adjust here
      group_by(date) %>%
      mutate(pcd_impact_new = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact),
             random_intercept_new = (random_intercept - mean(random_intercept, na.rm = T)) / sd(random_intercept, na.rm = T)) %>%
      ungroup() %>%
      
      # this transforms them back to input into the regression equation
      mutate(pcd_impact = pcd_impact_new * sd(pcd_impact),
             random_intercept = random_intercept_new * sd(random_intercept)) %>%
      
      select(-pcd_impact_new, -random_intercept_new) %>%
      
      rename(success_intercept = random_intercept,
             pcd_success_impact = pcd_impact,
             bs_success_impact = bunchsprint_impact), by = c("rider", "date")) %>%
  
  # calculate team leader and success predictions using random effects
  mutate(glmer_pred = (random_intercept + (pred_climb_difficulty * pcd_tmldr_impact) + (predicted_bs * bs_tmldr_impact)),
         glmer_pred = exp(glmer_pred) / (1+exp(glmer_pred))) %>%
  
  mutate(succ_pred = (success_intercept + (pred_climb_difficulty * pcd_success_impact) + (predicted_bs * bs_success_impact)),
         succ_pred = exp(succ_pred) / (1+exp(succ_pred))) %>%

  mutate(mod_pcd_corr_both = (0.67*pcd_success_impact)+(0.33*pcd_tmldr_impact),
         mod_bs_imp_both = (0.5*bs_tmldr_impact)+(0.5*bs_success_impact)) %>%
  
  mutate(mod_pcd_corr = (pred_climb_difficulty - mean(pred_climb_difficulty, na.rm = T)) * mod_pcd_corr_both) %>%

  mutate(mod_rel_pcd_corr = mod_pcd_corr - mean(mod_pcd_corr, na.rm = T)) %>%

  mutate(mod_elite_pcd_corr = rel_elite * mod_rel_pcd_corr) %>%
  
  group_by(team) %>%
  mutate(rel_glmer_pred = glmer_pred - mean(glmer_pred, na.rm = T),
         rel_succ_pred = succ_pred - mean(succ_pred, na.rm = T)) %>%
  mutate(No1_Team = ifelse(rank(-glmer_pred, ties.method = "min")==1, 1, 0),
         No1_Team_succ = ifelse(rank(-succ_pred, ties.method = "min")==1, 1, 0)) %>%
  ungroup() %>%
  
  group_by(team) %>%
  mutate(teammates_rel_sof = (sum(rel_sof_race, na.rm = T) - rel_sof_race) / (n() - 1)) %>%
  ungroup() %>%
  
  mutate(rider_match = str_to_title(rider)) %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT rider, date as dob FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)), by = c("rider_match" = "rider")) %>%
  
  mutate(age = as.numeric(as.Date(date)-as.Date(dob))/365.25) %>%
  
  mutate(rel_age = age - mean(age, na.rm = T)) %>%
  
  select(-age, -rider_match, -dob) %>%
  mutate(rel_age = ifelse(is.na(rel_age), 0, rel_age))

# who are the top riders
adding_stats %>%
  arrange(rank_ppo) %>%
  select(rider, team, rel_elite)

#
# adjust to create pcd_optimal stat
#

adding_pcd_optimal <- cbind(
  
  adding_stats,
  
  pcd_optimal = predict(read_rds("Stored models/gam_pcd_linked.rds"), 
                        adding_stats %>% select(f = mod_pcd_corr_both))) %>%
  
  mutate(pcd_optimal_race = abs(pcd_optimal - pred_climb_difficulty)) %>%

  mutate(pcd_optimal_race = mean(pcd_optimal_race, na.rm = T) - pcd_optimal_race)

# most optimal
adding_pcd_optimal %>%
  arrange(-pcd_optimal_race) %>%
  select(rider, team, pcd_optimal_race) %>%
  head()

#
#
#

predicting_teamleader <- cbind(
  
  adding_pcd_optimal,
  
  modeled_tmldr = predict(read_rds("Stored models/predict_teamleader_xgb.rds"),
                          as.matrix(adding_pcd_optimal %>%
                                      select(rel_team_tmldr, No1_Team, rel_glmer_pred, rel_elite, No1_Team_succ, rel_succ_pred,
                                             mod_rel_pcd_corr, mod_elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
                                             pcd_optimal_race) %>%
                                      mutate(int_pcdo_elite = pcd_optimal_race * rel_elite), reshape=T))) %>%

  # I set the lowest prediction on the team to 0% chance of being leader
  # and scale everyone down by that amount
  # this has affect of increasing margin held by the most likely riders to more
  # match the outcomes if that team has someone competitive in the mix
  group_by(team) %>%
  mutate(modeled_tmldr = modeled_tmldr - min(modeled_tmldr, na.rm = T)) %>%
  mutate(modeled_tmldr = modeled_tmldr / sum(modeled_tmldr, na.rm = T)) %>%
  ungroup()

# most likely leaders
predicting_teamleader %>%
  arrange(-modeled_tmldr) %>%
  select(rider, team, modeled_tmldr)

#
# predict winner
#

predicting_winner <- cbind(
  
  coef = predict(read_rds("Stored models/basic-winprob-glm.rds"),
                 predicting_teamleader),
  
  predicting_teamleader
  
) %>%
  
  mutate(pred = exp(coef)/(1+exp(coef))) %>%
  
  # 1. reduce win probabilities for riders ranked >3 on their team to 0
  # and re-distribute within the team
  group_by(team) %>%
  mutate(win_prob = ifelse((rank(-pred, ties.method = "min")<=3) | pred > 0.01, pred, 0)) %>%
  mutate(win_prob = win_prob / sum(win_prob, na.rm = T) * sum(pred, na.rm = T)) %>%
  ungroup() %>%
  
  # 2. then adjust so that win_prob / sum(win_prob) and everything = 1
  mutate(win_prob = win_prob / sum(win_prob, na.rm = T)) %>%
  
  # 3. select relevant data and remove the rest
  select(rider, team, win_prob, pred_climb_difficulty, predicted_bs,
         modeled_tmldr, pcd_optimal_race, rank_ppo, rel_elite, rel_sof_race,
         rel_age, rel_succ_pred)

# most likely winners
predicting_winner %>%
  arrange(-win_prob) %>%
  select(rider, team, win_prob) %>%
  mutate(ml_odds = round((1-win_prob)/(win_prob), 1)+1)

#
# write win probabilities
#

dbWriteTable(con, 
             
             "predictions_modeled_winprob",
             
             predicting_winner %>%
               mutate(race = "Route d'Occitanie",
                      year = 2020,
                      stage = as.numeric(3)) %>%
               mutate(updated = lubridate::now()),

             append = TRUE,
             row.names = FALSE,
             
)