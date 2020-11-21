#

library(tidyverse)
library(RMySQL)

# define function to clean extraneous fat from GLM

strip_glm = function(cm) {
  cm$y = c()
  cm$model = c()
  
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()  
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  
  
  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  attr(cm$terms,".Environment") = c()
  attr(cm$formula,".Environment") = c()
  
  cm
}

#

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#
#
#

# pull in dates

All_dates <- dbReadTable(con, "stage_data_perf") %>%
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC")) |
           (class %in% c("2.1", "1.1") & Tour == "Europe Tour") | 
           (sof > 0.25 & class %in% c("2.1", "1.1"))) %>%
  filter(year > 2016 & year <= 2020) %>%
  select(date) %>%
  unique() %>%
  filter(!is.na(date))

#write_csv(All_dates, "C:/Users/Jake/Documents/all-stage-type-preds-dates-AWS.csv")

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
         -avg_alt, -missing_profile_data, ) %>%
  
  filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC")) |
           (class %in% c("2.1", "1.1") & Tour == "Europe Tour") | 
           (sof > 0.25 & class %in% c("2.1", "1.1"))) %>%
  unique() %>% 
  
  left_join(read_csv("cobbles.csv")) %>% 
  mutate(cobbles = ifelse(is.na(cobbles), 0, cobbles)) %>%
  
  mutate(final_group = ifelse(bunch_sprint == 1, ifelse(gain_1st <= 5, 1, 0), ifelse(rnk <= 20 | gain_20th == 0, 1, 0))) %>%
  
  select(-gain_1st, -gain_20th)

#write_csv(All_data, "C:/Users/Jake/Documents/all-stage-type-preds-all-data-AWS.csv")

# per date glmer models using pcd interaction and rider random effects

#All_dates <- expand_grid(month = c("01","02","03","04","05","06","07","08","09","10"), 
#                         year = c(2017, 2018, 2019, 2020), day = 1) %>%
#  
#  mutate(date = paste0(year,"-",month,"-0", day)) %>%
#  select(date) %>%
#  unique()

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
  
  #dx <- All_data %>% filter(between(date, minD, maxD)==TRUE) %>% group_by(rider) %>% filter(n()>5) %>% ungroup()
  
  #dy <- All_data %>% filter(between(date, minD - 366, maxD)==TRUE) %>% group_by(rider) %>% filter(n()>5) %>% ungroup()
  
  dz <- All_data %>% filter(between(date, minD + 364, maxD)==TRUE) %>% group_by(rider) %>% filter(n()>5) %>% ungroup()
  
  # run a lme4 model for rider success and impact of pcd on success
  
  tictoc::tic()
  
  #mod_succ <- lme4::glmer(success ~ (1 + pred_climb_difficulty | rider) + (0 + bunch_sprint | rider) + sof,
  #                     data = dx,
  #                     family = binomial("logit"),
  #                     nAGQ=0,
  #                     control=lme4::glmerControl(optimizer = "nloptwrap"))
  
  tictoc::toc()
  
  #random_effects <- lme4::ranef(mod_succ)[[1]] %>%
  # rownames_to_column() %>%
  # rename(rider = rowname,
  #        random_intercept = `(Intercept)`,
  #        pcd_impact = pred_climb_difficulty,
  #        bunchsprint_impact = bunch_sprint) %>%
  #what date are we predicting
  #mutate(Date = as.Date(maxD + 1))
  
  ################################################
  
  #dbWriteTable(con, "lme4_rider_success", random_effects, append = TRUE, row.names = FALSE)
  
  #rm(mod_succ)
  #rm(random_effects)
  
  ################################################
  
  tictoc::tic()
  
  #mod4 <- lme4::glmer(team_ldr ~ (1 + pred_climb_difficulty | rider) + (0 + bunch_sprint | rider),
  #                    data = dx,
  #                    family = binomial("logit"),
  #                    nAGQ=0,
  #                    control=lme4::glmerControl(optimizer = "nloptwrap"))
  
  tictoc::toc()
  
  # summary
  
  #random_effects <- lme4::ranef(mod4)[[1]] %>%
  #  rownames_to_column() %>%
  #  rename(rider = rowname,
  #         random_intercept = `(Intercept)`,
  #         pcd_impact = pred_climb_difficulty,
  #         bunchsprint_impact = bunch_sprint) %>%
    #what date are we predicting
  #  mutate(Date = as.Date(maxD + 1))
  
  ################################################
  
  #dbWriteTable(con, "lme4_rider_teamleader", random_effects, append = TRUE, row.names = FALSE)
  
  #rm(mod4)
  #rm(random_effects)
  
  ################################################
  
  # run a lme4 model for rider success and impact of pcd on success
  # SOLELY for races where they finished as team's leader
  
  tictoc::tic()
  
  #mod_succwhenopp <- lme4::glmer(success ~ (1 + pred_climb_difficulty | rider) + (0 + bunch_sprint | rider) + sof,
  #                        data = dy %>% filter(tm_pos == 1),
  #                        family = binomial("logit"),
  #                        nAGQ=0,
  #                        control=lme4::glmerControl(optimizer = "nloptwrap"))
  
  tictoc::toc()
  
  #random_effects <- lme4::ranef(mod_succwhenopp)[[1]] %>%
  #  rownames_to_column() %>%
  #  rename(rider = rowname,
  #         random_intercept = `(Intercept)`,
  #         pcd_impact = pred_climb_difficulty,
  #         bunchsprint_impact = bunch_sprint) %>%
    #what date are we predicting
  #  mutate(Date = as.Date(maxD + 1))
  
  ################################################
  
  #dbWriteTable(con, "lme4_rider_succwhenopp", random_effects, append = TRUE, row.names = FALSE)
  
  #rm(mod_succwhenopp)
  #rm(random_effects)
  
  ################################################
  
  tictoc::tic()
  
   # All_riders <- dx %>%
   #   
   #   group_by(rider, bunch_sprint) %>%
   #   summarize(
   #    
   #     team_leader = mean(tm_pos == 1, na.rm = T),
   #     domestique = mean(tm_pos >= 4, na.rm = T),
   #     in_pack = mean(gain_gc <= 5, na.rm = T),
   #    
   #     pcd_corr_tmldr = cor(tm_pos==1, pred_climb_difficulty, method = "pearson"),
   #     pcd_corr_pts = cor(points_finish, pred_climb_difficulty, method = "pearson"),
   #     pcd_corr_succ = cor(success, pred_climb_difficulty, method = "pearson"),
   #    
   #     points_per_opp = mean(points_per_opp, na.rm = T),
   #     points_per_race = mean(points_finish, na.rm = T),
   #    
   #     final_group = mean(final_group, na.rm = T),
   #     
   #     sof_leader = mean(sof_per_opp, na.rm = T),
   #     sof_overall = mean(sof, na.rm = T),
   #    
   #     pcd_leader = mean(pred_climb_diff_opp, na.rm = T),
   #     pcd_success = mean(pred_climb_diff_succ, na.rm = T),
   #     pcd_overall = mean(pred_climb_difficulty, na.rm = T),
   # 
   #     successes = sum(points_finish > 0, na.rm = T),
   #     opportunities = sum(tm_pos == 1, na.rm = T),
   #     races = n()) %>%
   #  ungroup() %>%
   #  
   #  # what date are we predicting
   #  mutate(Date = as.Date(maxD + 1)) %>%
   #  
   #  gather(stat, value, team_leader:pcd_overall) %>%
   #  
   #  mutate(value = ifelse(is.na(value), 0, value)) %>%
   #  
   #  spread(stat, value)
   # 
  tictoc::toc()
  
  ############################################################
  
  #dbWriteTable(con, "performance_rider_allpcd", All_riders, append = TRUE, row.names = FALSE)
  
  ############################################################
  
  weighted_pcd <- dz %>%
    filter(!is.na(pred_climb_difficulty)) %>%
    mutate(weight = 1 / (rnk ^ 1)) %>%
    
    mutate(tm_pcd = ifelse(tm_pos == 1, pred_climb_difficulty, NA),
           tm_sof = ifelse(tm_pos == 1, sof, NA)) %>%
    
    group_by(master_team, year) %>%
    mutate(tm_sof = mean(tm_sof, na.rm = T),
           tm_pcd = mean(tm_pcd, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(points_finish = (1 / (rnk + 1)) * (limit / 5),
           points_finish = ifelse(rnk <= limit * 5, points_finish, 0),
           leader = ifelse(rnk <= 20, tm_pos == 1, 0)) %>%
    
    mutate(pointsBS = ifelse(bunch_sprint == 1, points_finish, 0),
           pointsNOBS = ifelse(bunch_sprint == 0, points_finish, 0)) %>%
    
    group_by(rider) %>%
    summarize(weighted_pcd = sum(pred_climb_difficulty * weight, na.rm = T) / sum(weight, na.rm = T), 
              races = n(), 
              pointsBS = sum(pointsBS, na.rm = T),
              pointsNOBS = sum(pointsNOBS, na.rm = T),
              team_pcd = sum(tm_pcd * weight, na.rm = T) / sum(weight, na.rm = T),
              points = mean(points_finish, na.rm = T),
              in_final_group = mean(final_group, na.rm = T),
              one_day_races = sum(one_day_race == 1, na.rm = T),
              stage_races = sum(one_day_race == 0 & stage == 1, na.rm = T),
              leader = mean(leader, na.rm = T),
              rider_sof = mean(sof, na.rm = T),
              team_sof = mean(tm_sof, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(ODR_tilt = one_day_races / (one_day_races + stage_races),
           BS_tilt = pointsBS / (pointsBS + pointsNOBS),
           BS_tilt = ifelse(is.na(BS_tilt), median(BS_tilt, na.rm = T), BS_tilt),
           rel_sof = rider_sof - team_sof,
           rel_pcd = weighted_pcd - team_pcd) %>%
    
    select(rider, rel_sof, weighted_pcd, BS_tilt, points, leader, in_final_group, races) %>%
    
    mutate(points = log10(points+0.01)) %>%
    
    mutate(Date = as.Date(maxD + 1))
  
  ############################################################
  
  dbWriteTable(con, "performance_rider_clustering", weighted_pcd, append = TRUE, row.names = FALSE)
  
  ############################################################
  
  #rm(All_riders)
  #rm(dx)
  #rm(dy)
  rm(dz)
  
  print(maxD)
  
}

#
# model the overall cobbles data
#

tictoc::tic()

cobbles_lme4_model <- lme4::glmer(success ~ 
                                    (1 + pred_climb_difficulty | rider) +
                                    (0 + bunch_sprint | rider) + 
                                    (0 + cobbles | rider),
                        data = All_data,
                        family = binomial("logit"),
                        nAGQ=0,
                        control=lme4::glmerControl(optimizer = "nloptwrap"))

tictoc::toc()

# generate random effects

random_effects_cobbles <- lme4::ranef(cobbles_lme4_model)[[1]] %>%
  rownames_to_column() %>%
  rename(rider = rowname,
         random_intercept = `(Intercept)`,
         pcd_impact = pred_climb_difficulty,
         bunchsprint_impact = bunch_sprint)

# write cobbles impacts

dbWriteTable(con, 
             "performance_rider_cobbles", 
             random_effects_cobbles %>% select(rider, cobbles) %>% mutate(updated = lubridate::today()),
             overwrite = TRUE,
             row.names = FALSE)

# write model

write_rds(cobbles_lme4_model, "Stored models/cobbles_added_to_normal_success_lme4_2013-2020.rds")

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
  
  filter(date >= as.Date('2017-01-01'))

##################################################

lme4_success_table <- dbReadTable(con, "lme4_rider_success") %>%
  
  mutate(date = as.Date(Date)) %>%
  select(-Date) %>%
  
  filter(date >= as.Date('2017-01-01'))

##################################################

predicting_all <- All_data %>%
  
  filter(class %in% c("1.UWT", "2.UWT", "WC", "CC") | 
           (class %in% c("1.HC", "2.HC", "1.Pro", "2.Pro") & Tour == 'Europe Tour') |
           (sof > 0.2)) %>%
  
  select(-points_per_opp, -sof_per_opp, -pred_climb_diff_opp, -bunch_sprint, -final_group) %>%
  
  inner_join(
    
    rbind(
      dbGetQuery(con, "SELECT * FROM predictions_stage_bunchsprint") %>%
        mutate(bunch_sprint = 1),
      dbGetQuery(con, "SELECT * FROM predictions_stage_bunchsprint") %>%
        mutate(bunch_sprint = 0)), by = c("stage", "race", "year")
    
  ) %>%
  
  inner_join(
    
    performance_table, by = c("rider", "date", "bunch_sprint")) %>%
  
  # add some regression to the numbers here
  mutate(points_per_opp = (0.05 + (opportunities * points_per_opp)) / (5 + opportunities)) %>%
  
  select(-win_seconds, -total_seconds, -gain_gc,
         -limit, -leader_rating,
         -pred_climb_diff_succ) %>%
  
  # this process combines the bs == 1 and bs == 0 stats
  # weighting the combination by predicted_bs and # of races in that category
  gather(stat, value, -rnk, -rider, -team, -master_team, -length,
         -stage, -race, -year, -class, -grand_tour, -one_day_race,
         -tm_pos, -Tour, -uphill_finish, -pred_climb_difficulty,
         -bunch_sprint, -predicted_bs, -points_finish, -success, -cobbles,
         -sof, -team_ldr, -date, -races) %>%
  
  mutate(multiplier = ifelse(bunch_sprint == 1, predicted_bs, 1-predicted_bs),
         
         value = multiplier * value) %>%
  
  select(-bunch_sprint) %>%
  
  group_by(rnk, rider, team, master_team, length,
           stage, race, year, class, grand_tour, one_day_race,
           tm_pos, Tour, uphill_finish, pred_climb_difficulty,
           predicted_bs, points_finish, success, date,
           sof, team_ldr, cobbles,
           stat) %>%
  summarize(new_value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  
  spread(stat, new_value) %>%

  #
  # need to regress points_per_opp as some riders have won on their only opportunity
  # filtered to tm_pos == 1 and regressed a number of combinations of N and regress amt between 0.01 and 0.024 (raw average and weighted avg of ppo)
  # N = 4 and 0.01 was the best R2 of about 0.109
  #
  
  #mutate(points_per_opp = (0.04 + (opportunities * points_per_opp)) / (4 + opportunities)) %>%
  
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
      
      filter(date >= as.Date('2017-01-01')), by = c("rider", "date")
    
  ) %>%
  
  inner_join(
    
    dbReadTable(con, "lme4_rider_succwhenopp")  %>%
      
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
      
      rename(pcd_succwhenopp_impact = pcd_impact,
             bs_succwhenopp_impact = bunchsprint_impact,
             succwhenopp_intercept = random_intercept) %>%
      
      filter(date >= as.Date('2017-01-01')), by = c("rider", "date")
    
  ) %>%
  
  inner_join(
    
    lme4_success_table  %>%
      
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
  
  # add in cobbles performance
  inner_join(
    
    dbReadTable(con, "performance_rider_cobbles")  %>%
      
      mutate(rider = str_to_title(rider)) %>%
      
      rename(cobbles_intercept = cobbles), by = c("rider")) %>%
  
  # calculate team leader and success predictions using random effects
  mutate(glmer_pred = -2 + (random_intercept + (pred_climb_difficulty * pcd_tmldr_impact) + (predicted_bs * bs_tmldr_impact)),
         glmer_pred = exp(glmer_pred) / (1+exp(glmer_pred))) %>%
  
  mutate(succ_pred = -5.2 +
           ((cobbles_intercept * cobbles) + 
              success_intercept + 
              (pred_climb_difficulty * pcd_success_impact) + 
              (predicted_bs * bs_success_impact)),
         succ_pred = exp(succ_pred) / (1+exp(succ_pred))) %>%
  
  mutate(succwhenopp_pred = -2.7 +
           ((cobbles_intercept * cobbles) + 
              succwhenopp_intercept + 
              (pred_climb_difficulty * pcd_succwhenopp_impact) + 
              (predicted_bs * bs_succwhenopp_impact)),
         succwhenopp_pred = exp(succwhenopp_pred) / (1+exp(succwhenopp_pred))) %>%
  
  mutate(mod_pcd_corr_both = (0.67*pcd_success_impact)+(0.33*pcd_tmldr_impact),
         mod_bs_imp_both = (0.5*bs_tmldr_impact)+(0.5*bs_success_impact)) %>%
  
  mutate(mod_pcd_corr = (pred_climb_difficulty - mean(pred_climb_difficulty, na.rm = T)) * mod_pcd_corr_both) %>%
  
  group_by(stage, race, year) %>%
  mutate(mod_rel_pcd_corr = mod_pcd_corr - mean(mod_pcd_corr, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(mod_elite_pcd_corr = rel_elite * mod_rel_pcd_corr) %>%
  
  group_by(stage, race, year, team) %>%
  mutate(rel_glmer_pred = glmer_pred - mean(glmer_pred, na.rm = T),
         rel_succ_pred = succ_pred - mean(succ_pred, na.rm = T),
         rel_succwhenopp_pred = succwhenopp_pred - mean(succwhenopp_pred, na.rm = T)) %>%
  
  mutate(No1_Team = ifelse(rank(-glmer_pred, ties.method = "min")==1, 1, 0),
         No1_Team_succ = ifelse(rank(-succ_pred, ties.method = "min")==1, 1, 0)) %>%
  ungroup() %>%
  
  group_by(stage, race, year, team) %>%
  mutate(teammates_rel_sof = (sum(rel_sof_race, na.rm = T) - rel_sof_race) / (n() - 1)) %>%
  ungroup() %>%
  
  mutate(rider_match = str_to_title(rider)) %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT rider, date as dob FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)), by = c("rider_match" = "rider")) %>%
  
  mutate(age = as.numeric(as.Date(date)-as.Date(dob))/365.25) %>%
  
  group_by(stage, race, year) %>%
  mutate(rel_age = age - mean(age, na.rm = T)) %>%
  ungroup() %>%
  
  select(-age, -rider_match, -dob) %>%
  mutate(rel_age = ifelse(is.na(rel_age), 0, rel_age)) %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT race, year, rider, bib FROM pcs_all_startlists") %>%
      mutate(bib_leader = ifelse(bib %% 10 == 1, 1, 0)), by = c("rider", "race", "year")
    
  )

#
# average of pcd_impact, random_intercept, bs_impact, cobbles_impact for top 5 on each race
#

race_top5_averages <- predicting_all %>%
  
  filter(rnk <= 5) %>%
  
  group_by(stage, race, year, pred_climb_difficulty, predicted_bs, cobbles) %>%
  summarize(pcd = mean(pcd_success_impact, na.rm = T),
            suc = mean(rel_succ_pred, na.rm = T),
            bs = mean(bs_success_impact, na.rm = T),
            cob = mean(cobbles_intercept, na.rm = T)) %>%
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
  
  group_by(f = ifelse(mod_pcd_corr_both < -0.2, -0.24, 
                      ifelse(mod_pcd_corr_both > 0.15, 0.18, 
                             floor(mod_pcd_corr_both / 0.03)))) %>%
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

write_rds(gam_link_pcd, "Stored models/gam_pcd_linked.rds")

# predict based on -12 to +5 (reasonable ranges)
pred_link_pcd <- cbind(
  
  pcd_optimal = predict(gam_link_pcd, tibble(f = seq(-8,4,0.5))),
  
  tibble(f = seq(-8,4,0.5))
  
)

# plot to check (0 pcd_impacts will optimally fit at 7 pcd)
ggplot(pred_link_pcd, aes(x = f, y = pcd_optimal))+geom_point()

# now use to create pcd_impact variable

predicting_all <- predicting_all %>%
  
  cbind(pcd_optimal = predict(gam_link_pcd, predicting_all %>%
                                select(f = mod_pcd_corr_both))) %>%

  mutate(pcd_optimal_race = abs(pcd_optimal - pred_climb_difficulty)
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

# Logistic Model

glm(team_ldr ~ rel_team_tmldr + No1_Team + rel_glmer_pred + No1_Team_succ + rel_succ_pred + 
      mod_rel_pcd_corr + mod_elite_pcd_corr + pcd_succ_vs_race + sof_vs_team + 
      pcd_optimal_race + rel_succwhenopp_pred + rel_elite + bib_leader, 
    
    family = "binomial", 
    data = predicting_all %>% 
      filter(year < 2020) %>%
      filter(!class %in% c("NC", "CC", "WC"))) -> glm_mod

summary(glm_mod)

gbm_predict_TMLDR = cbind(
  
  pred = predict(glm_mod, 
                 predicting_all %>% 
                   unique() %>%
                   filter(year >= 2020)),
  
  predicting_all %>%
    unique() %>%
    filter(year >= 2020))

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
  ungroup() -> predicted_rank_vs_leader

pred %>% 
  group_by(team_ldr, pred_leader) %>% 
  count() %>%
  ungroup() -> predictedldr_vs_leader

pred %>%
  
  group_by(f = floor(pred / 0.05) * 0.05, team_ldr) %>%
  count() %>%
  ungroup() -> pred_str_vs_leader

# brier score for accuracy
brierScore <- mean((pred$pred-pred$leader)^2)

print(brierScore)

# ROC / AUC
roc_obj <- pROC::roc(pred$leader, pred$pred)
print(pROC::auc(roc_obj))

pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
  labs(title = "Predict team leader", subtitle = pROC::auc(roc_obj))

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

xgb.train <- xgb.DMatrix(
  
  data = as.matrix(predicting_all %>%
                     filter(year < 2020) %>%
                     select(rel_team_tmldr, No1_Team, rel_glmer_pred, No1_Team_succ, rel_succ_pred,
                            mod_rel_pcd_corr, mod_elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
                            pcd_optimal_race, rel_succwhenopp_pred, rel_elite, bib_leader) %>%
                     mutate(int_pcdo_succ = pcd_optimal_race * rel_succwhenopp_pred)),
  
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
                     select(rel_team_tmldr, No1_Team, rel_glmer_pred, No1_Team_succ, rel_succ_pred,
                            mod_rel_pcd_corr, mod_elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
                            pcd_optimal_race, rel_succwhenopp_pred, rel_elite, bib_leader) %>%
                     mutate(int_pcdo_succ = pcd_optimal_race * rel_succwhenopp_pred)),
  
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
  max_depth = 4,
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
# write model
#

write_rds(gbm_model, "Stored models/predict_teamleader_xgb-match-every.rds")

#
#
# this outputs GBM predictions for all data

gbm_predict_TMLDR = cbind(
  
  pred = predict(gbm_model, 
                 as.matrix(predicting_all %>%
                             filter(year >= 2020) %>%
                             filter(!class %in% c("CC", 'NC', 'WC')) %>% 
                             unique() %>%
                             select(rel_team_tmldr, No1_Team, rel_glmer_pred, No1_Team_succ, rel_succ_pred,
                                    mod_rel_pcd_corr, mod_elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
                                    pcd_optimal_race, rel_succwhenopp_pred, rel_elite, bib_leader) %>%
                             mutate(int_pcdo_succ = pcd_optimal_race * rel_succwhenopp_pred), reshape=T)),
  
  predicting_all %>%
    filter(year >= 2020) %>%
    filter(!class %in% c("CC", 'NC', 'WC')) %>% 
    unique() %>%
    select(team, team_ldr, race, year, stage, rel_team_tmldr, No1_Team, rel_glmer_pred, No1_Team_succ, rel_succ_pred,
           mod_rel_pcd_corr, mod_elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
           pcd_optimal_race, rel_succwhenopp_pred, tm_pos, rel_elite, bib_leader) %>%
    mutate(int_pcdo_succ = pcd_optimal_race * rel_succwhenopp_pred))



# brier score for accuracy
brierScore <- mean((pred$pred-pred$leader)^2)

print(brierScore)

# ROC / AUC
roc_obj <- pROC::roc(pred$leader, pred$pred)
print(pROC::auc(roc_obj))

pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
  labs(title = "Predict team leader", subtitle = pROC::auc(roc_obj))

#

gbm_predict_TMLDR_all = cbind(
  
  
  pred = predict(gbm_model, 
                 as.matrix(predicting_all %>%
                             select(rel_team_tmldr, No1_Team, rel_glmer_pred, No1_Team_succ, rel_succ_pred,
                                    mod_rel_pcd_corr, mod_elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
                                    pcd_optimal_race, rel_succwhenopp_pred, rel_elite) %>%
                             mutate(int_pcdo_succ = pcd_optimal_race * rel_succwhenopp_pred), reshape=T)),
  
  predicting_all %>%
    select(stage, race, year, team, rider, rel_team_tmldr, No1_Team, rel_glmer_pred, No1_Team_succ, rel_succ_pred,
           mod_rel_pcd_corr, mod_elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
           pcd_optimal_race, rel_succwhenopp_pred, rel_elite) %>%
    mutate(int_pcdo_succ = pcd_optimal_race * rel_succwhenopp_pred))

#
#
#
#
#

# Apply Team Ldr to Win Prob Model ----------------------------------------

# I generate predictions for team leader using both models on the full dataset

predicting_win <- predicting_all %>%
  
  inner_join(
    gbm_predict_TMLDR_all %>%
      group_by(stage, race, year, team) %>%
      mutate(pred2 = pred / sum(pred, na.rm = T)) %>%
      ungroup() %>%
      rename(modeled_tmldr = pred2) %>%
      select(stage, race, year, team, rider, modeled_tmldr), 
    by = c("stage", "race", "year", "team", "rider")
    
  ) %>%
  unique() %>%
  
  mutate(tot_pred_succ = (modeled_tmldr * rel_succwhenopp_pred)) %>%
  
  group_by(stage, race, year) %>%
  mutate(tot_pred_succ = tot_pred_succ - mean(tot_pred_succ, na.rm = T)) %>%
  ungroup()

#
# moving onto win probability
#

#
#
# regression for wins
#
#

train_W <- predicting_win %>%
  filter(year < 2020) %>% 
  filter(!is.na(teammates_rel_sof)) %>%
  mutate(win = as.numeric(win))

test_W <- predicting_win %>%
  filter(year >= 2020) %>% 
  filter(!is.na(teammates_rel_sof)) %>%
  mutate(win = as.numeric(win))

####
####
####
####

regular_train <- train_W

cols <- c("modeled_tmldr", 
          "rank_ppo", "rel_elite", 
          "tot_pred_succ",
          "rel_in_pack",
          "rel_succ_pred", 
          "rel_sof_race", 
          "pcd_optimal_race", 
          "teammates_rel_sof", 
          "rel_age")

#pre_proc_val <- caret::preProcess(train_W[,cols], method = c("center", "scale"))

#pre_proc_val <- caret::preProcess(train_W[,cols])

#train_W[,cols] = predict(pre_proc_val, train_W[,cols])
#test_W[,cols] = predict(pre_proc_val, test_W[,cols])

#
#
# run logistic regression on each variable

for(v in 1:length(cols)) {
  
  print(cols[[v]])
  
  # run single variable logistic model
  mod <- glm(win ~ .,
             data = train_W[, c(cols[[v]], "win")],
             weights = train_W$predicted_bs,
             family = "binomial")
  
  # summary
  summary(mod)
  
  # predict test set out of sample
  pred <- cbind(coef = predict(mod, test_W),
                test_W) %>%
    mutate(pred = exp(coef)/(1+exp(coef)))
  
  # brier score for accuracy
  brierScore <- mean((pred$pred-pred$win)^2)
  
  print(brierScore)
  
  # ROC / AUC
  roc_obj <- pROC::roc(pred$win, pred$pred)
  print(pROC::auc(roc_obj))
  
  ggsave(
    filename = paste0("Images/each-var-nov-20-", cols[[v]], ".png"),
    plot = pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
      labs(title = cols[[v]], subtitle = pROC::auc(roc_obj))
  )
  
}

#
#
# run full logistic regression model

# run single variable logistic model
mod <- glm(win ~ .,
           data = train_W[, c(cols, "win")],
           family = "binomial")

# summary
summary(mod)

# run without scaling/centering
reg_mod <- glm(win ~ .,
           data = regular_train[, c(cols, "win")],
           family = "binomial")

# summary
summary(reg_mod)

# write model // this writes an un-standardized/normalized version

write_rds(strip_glm(reg_mod), "Stored models/basic-winprob-glm-match-every-rds")

# predict test set out of sample
pred <- cbind(coef = predict(mod, test_W),
              test_W) %>%
  mutate(pred = exp(coef)/(1+exp(coef)))

# ROC / AUC
roc_obj <- pROC::roc(pred$win, pred$pred)
print(pROC::auc(roc_obj))

ggsave(
  filename = paste0("Images/all-nov20.png"),
  plot = pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
    labs(title = "all variables", subtitle = pROC::auc(roc_obj))
)

#
#
# set up LASSO model
#
#

cols <- c("modeled_tmldr", 
          "rank_ppo", "rel_elite", 
          "tot_pred_succ",
          "rel_in_pack",
          "rel_succ_pred", 
          "rel_sof_race", 
          "pcd_optimal_race", 
          "teammates_rel_sof", 
          "rel_age", 
          "win")

TRdummies <- caret::dummyVars(win ~ ., data = train_W[,cols])

TSdummies <- caret::dummyVars(win ~ ., data = test_W[,cols])

train_dummies = predict(TRdummies, newdata = train_W[,cols])

test_dummies = predict(TSdummies, newdata = test_W[,cols])

# set up matrices

x = as.matrix(train_dummies)
y_train = train_W$win

x_test = as.matrix(test_dummies)
y_test = test_W$win

# run to find optimal lambda

#lambdas <- seq(0.0001, 1.0001, 0.005)

lambdas <- 10^seq(1.25, -5, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- glmnet::cv.glmnet(x, 
                               y_train,
                               alpha = 1, 
                               lambda = lambdas,
                               family = "binomial",
                               nfolds = 5)

# Best 
lambda_best <- lasso_reg$lambda.min 

# run the lasso model

tictoc::tic()

lasso_model_all <- glmnet::glmnet(x, 
                      y_train,
                      alpha = 1, 
                      lambda = lambda_best, 
                      standardize = TRUE,
                      family = "binomial")

tictoc::toc()

#

lasso_model_all$beta

#

predictions_train_all <- cbind(coef = predict(lasso_model_all, s = lambda_best, newx = x),
                           train_W) %>%
  rename(pred = `1`) %>%
  mutate(pred = exp(pred) / (1+exp(pred)))

predictions_test_all <- cbind(coef = predict(lasso_model_all, s = lambda_best, newx = x_test),
                          test_W) %>%
  rename(pred = `1`) %>%
  mutate(pred = exp(pred) / (1+exp(pred)))

# ROC / AUC
roc_obj <- pROC::roc(predictions_test_all$win, predictions_test_all$pred)
print(pROC::auc(roc_obj))

ggsave(
  filename = paste0("Images/all-lasso-nov20.png"),
  plot = pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1))+
    labs(title = "all variables", subtitle = pROC::auc(roc_obj))
)

#
#
# naive adjustments below (three team leaders)
#
#

predictions_test_ADJ <- rbind(predictions_test_all, predictions_train_all) %>%
  
  # 1. reduce win probabilities for riders ranked >3 on their team to 0
  # and re-distribute within the team
  group_by(team, race, stage, year) %>%
  mutate(win_prob = ifelse(rank(-pred, ties.method = "min")<=5, pred, 0)) %>%
  mutate(win_prob = win_prob / sum(win_prob, na.rm = T) * sum(pred, na.rm = T)) %>%
  ungroup() %>%
  
  # 2. then adjust so that win_prob / sum(win_prob) and everything = 1
  group_by(race, stage, year) %>%
  mutate(win_prob = win_prob / sum(win_prob, na.rm = T)) %>%
  ungroup() %>%
  
  # 3. select relevant data and remove the rest
  select(rider, team, win_prob, stage, race, year, pred_climb_difficulty, predicted_bs, rnk,
         modeled_tmldr, pcd_optimal_race, rank_ppo, rel_elite, rel_sof_race, 
         rel_succ_pred) %>%
  
  inner_join(
    
    All_data %>%
      select(stage, race, year, cobbles, rnk, rider) %>%
      mutate(win = rnk == 1) %>%
      unique(), by = c("stage", "race", "year", "rider")
    
  )

#

cobbles_model <- lme4::glmer(win ~ win_prob + (0 + cobbles | rider),
                             data = predictions_test_ADJ %>%
                               mutate(resid = win - win_prob), family = "binomial")

# 
# # XG Boost for Win Probability --------------------------------------------
# 
# #
# #
# #
# #
# #
# # train
# 
# library(xgboost)
# 
# #
# # First train the model on pre-2019, test on 2019-20 using bunch_sprint == 1 and weighting by predicted_bs
# # this slightly improves overall model
# #
# 
# xgb.trainW <- xgb.DMatrix(
#   
#   data = as.matrix(predicting_win %>%
#                      filter(year < 2019) %>%
#                      select(modeled_tmldr,
#                             rank_ppo, 
#                             rel_elite, 
#                             rel_in_pack, 
#                             rel_succ_pred, 
#                             rel_sof_race, 
#                             pcd_optimal_race, 
#                             teammates_rel_sof, 
#                             rel_age)),
#   
#   label = predicting_win %>%
#     filter(year < 2019) %>%
#     select(win) %>%
#     .[[1]]
#   
# )
# 
# # test
# 
# xgb.testW <- xgb.DMatrix(
#   
#   data = as.matrix(predicting_win %>%
#                      filter(year >= 2019) %>%
#                      select(modeled_tmldr,
#                             rank_ppo, 
#                             rel_elite, 
#                             rel_in_pack, 
#                             rel_succ_pred, 
#                             rel_sof_race, 
#                             pcd_optimal_race, 
#                             teammates_rel_sof, 
#                             rel_age)),
#   
#   label = predicting_win %>%
#     filter(year >= 2019) %>%
#     select(win) %>%
#     .[[1]]
#   
# )
# 
# # outline parameters
# 
# paramsW <- list(
#   
#   booster = "gbtree",
#   eta = 0.3,
#   max_depth = 4,
#   gamma = 0,
#   subsample = 1,
#   colsample_bytree = 1,
#   tree_method = "hist",
#   objective = "binary:logistic"
#   
# )
# 
# # run xgboost model
# 
# gbm_modelW <- xgb.train(params = paramsW,
#                        data = xgb.trainW,
#                        nrounds = 10000,
#                        nthreads = 4,
#                        early_stopping_rounds = 1000,
#                        watchlist = list(val1 = xgb.trainW,
#                                         val2 = xgb.testW),
#                        verbose = 0)
# 
# #
# #
# # xgb Importance
# 
# xgb.importance(model = gbm_modelW)
# 
# gbm_modelW$best_score
# 
# #
# # write model
# #
# 
# write_rds(gbm_modelW, "Stored models/predict_win_xgb.rds")
# 
# #
# #
# # this outputs GBM predictions for all data
# 
# gbm_predict_WIN = cbind(
#   
#   
#   pred = predict(gbm_modelW, 
#                  as.matrix(predicting_win %>%
#                              filter(year >= 2019) %>%
#                              select(modeled_tmldr,
#                                     rank_ppo, 
#                                     rel_elite, 
#                                     rel_in_pack, 
#                                     rel_succ_pred, 
#                                     rel_sof_race, 
#                                     pcd_optimal_race, 
#                                     teammates_rel_sof, 
#                                     rel_age), reshape=T)),
#   
#   predicting_win %>%
#     filter(year >= 2019) %>%
#     select(stage, race, year, rider, team, pred_climb_difficulty, rel_team_tmldr, No1_Team, 
#            rel_glmer_pred, rel_elite, No1_Team_succ, rel_succ_pred,
#            mod_rel_pcd_corr, elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
#            tm_pos, win, predicted_bs, one_day_race, pcd_optimal_race,
#            modeled_tmldr,
#            rank_ppo, 
#            rel_elite, 
#            rel_in_pack, 
#            rel_succ_pred, 
#            rel_sof_race, 
#            pcd_optimal_race, 
#            teammates_rel_sof, 
#            rel_age))
# 
# #
# #
# #
# 
# # ROC / AUC
# roc_obj <- pROC::roc(gbm_predict_WIN$win, gbm_predict_WIN$pred)
# print(pROC::auc(roc_obj))
# 
# #
# 
# gbm_predict_WIN_all = cbind(
#   
#   
#   pred = predict(gbm_modelW, 
#                  as.matrix(predicting_win %>%
#                              select(modeled_tmldr,
#                                     rank_ppo, 
#                                     rel_elite, 
#                                     rel_in_pack, 
#                                     rel_succ_pred, 
#                                     rel_sof_race, 
#                                     pcd_optimal_race, 
#                                     teammates_rel_sof, 
#                                     rel_age), reshape=T)),
#   
#   predicting_win %>%
#     select(stage, race, year, rider, team, pred_climb_difficulty, rel_team_tmldr, No1_Team, 
#            rel_glmer_pred, rel_elite, No1_Team_succ, rel_succ_pred,
#            mod_rel_pcd_corr, elite_pcd_corr, pcd_succ_vs_race, sof_vs_team,
#            tm_pos, win, predicted_bs, one_day_race, pcd_optimal_race,
#            modeled_tmldr,
#            rank_ppo, 
#            rel_elite, 
#            rel_in_pack, 
#            rel_succ_pred, 
#            rel_sof_race, 
#            pcd_optimal_race, 
#            teammates_rel_sof, 
#            rel_age))


#
#
# below I adjust BS==0 and BS==1 so that everything adds up to 1
#
#

# BS==0

preds_races_All <- predictions_test_all %>%
  select(stage, race, year) %>%
  unique()

adjust_win_probs_All <- vector("list", length(preds_races_All$race))

for(x in 1:length(preds_races_All$race)) {
  
  R = preds_races_All$race[[x]]
  S = preds_races_All$stage[[x]]
  Y = preds_races_All$year[[x]]
  
  d <- predictions_test_all %>%
    filter(stage == S & race == R & year == Y) %>%
    mutate(odds = ((1-pred)/pred)+1)
  
  imp <- implied::implied_probabilities(d$odds, method = "or")
  
  res <- imp$probabilities
  
  res <- res %>% as.data.frame() %>% gather(rownum, new_win_prob)
  
  out <- cbind(d, res) %>%
    select(-rownum)
  
  adjust_win_probs_All[[x]] <- out
  
}

#
#
#

adjusted_preds_All <- bind_rows(adjust_win_probs_All) %>%
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

# Assign riders to kmeans model clusters

kmeans_model <- read_rds('Stored models/rider-clusters-kmeans-model.rds')

centers <- kmeans_model$centers %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(CL = rowname) %>%
  gather(stat, center, -CL)

#

prc <- dbReadTable(con, "performance_rider_clustering")

dist <- prc %>%
  select(-in_final_group, -rel_sof) %>%
  
  # filter out riders without enough races (max will normally be around 75 so 15+ races needed)
  group_by(Date) %>%
  filter(races > (max(races)/5)) %>%
  ungroup() %>%
  
  gather(stat, value, -rider, -Date, -races) %>%
  
  # scale each stat
  group_by(stat) %>%
  mutate(value = (value - mean(value, na.rm = T)) / sd(value, na.rm = T)) %>%
  ungroup() %>%
  
  # join with centers
  inner_join(centers, by = c("stat")) %>%
  
  # take sum squared euclidean distance
  group_by(rider, Date, races, CL) %>%
  summarize(SED = sum((value - center)^2, na.rm = T)) %>%
  ungroup() %>%
  
  # filter out smallest distance
  group_by(rider, Date, races) %>%
  filter(SED == min(SED, na.rm = T)) %>%
  ungroup() %>%
  
  select(-SED) %>%
  
  # match with correct cluster labels
  inner_join(dbReadTable(con, "kmeans_rider_clusters") %>%
               mutate(CL = as.character(CL)), by = c("CL"))

#
#
#
#
#

#dbWriteTable(con, "clusters_riders", dist, append = TRUE, row.names = F)

#
#
#
#
#

tdf_2020 <- stage_data_perf %>%
  filter(race == "tour de france" & stage == 1 & year == 2020) %>%
  
  inner_join(dist, by = c("rider", "date" = "Date"))
