
library(tidyverse)
library(RMySQL)

#

dbDisconnect(con)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

All_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE date > '2016-06-30'") %>%
  
  filter(time_trial == 0) %>%
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
           (class %in% c("2.1", "1.1") & Tour == "Europe Tour") | 
           (sof > 0.2 & class %in% c("2.2", "1.2", "2.2U", "1.2U", "2.Ncup", "1.Ncup", "JR")) |
           (sof > 0.1 & !class %in% c("2.2", "1.2", "2.2U", "1.2U", "2.Ncup", "1.Ncup", "JR")) |
           (year == 2021 & class != "NC")) %>%
  
  left_join(read_csv("cobbles.csv") %>% mutate(stage = as.character(stage))) %>% 
  
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
  mutate(stage_join = as.character(stage)) %>%
  inner_join(dbGetQuery(con, "SELECT * FROM predictions_stage_bunchsprint") %>%
               select(-bunch_sprint) %>%
               unique(), by = c("stage_join" = "stage", "race", "year")) %>%
  select(-stage_join) %>%
  
  unique() %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT DISTINCT date FROM lme4_rider_logranks WHERE test_or_prod = 'prod'") %>%
      mutate(date = as.Date(date)), by = c("date")
    
  ) %>%
  
  unique() %>%
  
  left_join(
    
    dbReadTable(con, "lme4_rider_logranks")  %>%
      filter(test_or_prod == "prod") %>%
      select(-test_or_prod) %>%
      unique() %>%
      mutate(rider = str_to_title(rider)) %>%
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
      
      filter(date >= as.Date('2016-07-01')), by = c("rider", "date")
    
  ) %>%
  
  # 0.97 is SD and 3.9 is Mean -- intercept is -0.6 and sof is 1.15 with an avg of 0.36 so -0.2 is left-over
  mutate(pred_rank = exp(-0.2 + ((
    ((rand_logrk_impact + 
      (predicted_bs * bs_logrk_impact) + 
        (one_day_race * odr_logrk_impact) + 
      (pcd_logrk_impact * pred_climb_difficulty))*-1) / 0.97)+3.9))) %>%
  
  # mutate(pred_sprint_rank = exp(-0.2 + ((
  #   ((rand_logrk_impact + 
  #       (0.9 * bs_logrk_impact) + 
  #       (one_day_race * odr_logrk_impact) + 
  #       (pcd_logrk_impact * 1.5))*-1) / 0.97)+3.9))) %>%
  # 
  # mutate(pred_classics_rank = exp(-0.2 + ((
  #   ((rand_logrk_impact + 
  #       (0.25 * bs_logrk_impact) + 
  #       (one_day_race * odr_logrk_impact) + 
  #       (pcd_logrk_impact * 3.5))*-1) / 0.97)+3.9))) %>%
  # 
  # mutate(pred_ardennes_rank = exp(-0.2 + ((
  #   ((rand_logrk_impact + 
  #       (0.05 * bs_logrk_impact) + 
  #       (one_day_race * odr_logrk_impact) + 
  #       (pcd_logrk_impact * 7))*-1) / 0.97)+3.9))) %>%
  # 
  # mutate(pred_mountains_rank = exp(-0.2 + ((
  #   ((rand_logrk_impact + 
  #       (0.01 * bs_logrk_impact) + 
  #       (one_day_race * odr_logrk_impact) + 
  #       (pcd_logrk_impact * 15))*-1) / 0.97)+3.9))) %>%
  
  select(-rand_logrk_impact, #-pcd_logrk_impact, -bs_logrk_impact, 
         -odr_logrk_impact) %>%
  
  # left_join(
  #   
  #   dbReadTable(con, "lme4_rider_points")  %>%
  #     filter(test_or_prod == "prod") %>%
  #     select(-test_or_prod) %>%
  #     unique() %>%
  #     mutate(rider = str_to_title(rider)) %>%
  #     mutate(date = as.Date(Date)) %>%
  #     select(-Date) %>%
  #     
  #     mutate(level_data = ifelse(is.na(pcd_impact), "just_rider",
  #                                ifelse(is.na(bunchsprint_impact), "pcd_added",
  #                                       ifelse(is.na(one_day_race), "bs_added", "odr_added")))) %>%
  #     
  #     # the standard deviations of random intercept and pcd impact both vary widely (increase as you move from 2015 to 2020)
  #     # we adjust here
  #     group_by(date, level_data) %>%
  #     mutate(pcd_impact_new = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact),
  #            random_intercept_new = (random_intercept - mean(random_intercept, na.rm = T)) / sd(random_intercept, na.rm = T)) %>%
  #     ungroup() %>%
  #     
  #     # this transforms them back to input into the regression equation
  #     mutate(pcd_impact = pcd_impact_new * sd(pcd_impact),
  #            random_intercept = random_intercept_new * sd(random_intercept)) %>%
  #     
  #     select(-pcd_impact_new, -random_intercept_new) %>%
  #     
  #     rename(pcd_points_impact = pcd_impact,
  #            bs_points_impact = bunchsprint_impact,
  #            rand_points_impact = random_intercept,
  #            odr_points_impact = one_day_race) %>%
  #     
  #     mutate(pcd_points_impact = ifelse(is.na(pcd_points_impact), 0, pcd_points_impact),
  #            bs_points_impact = ifelse(is.na(bs_points_impact), 0, bs_points_impact),
  #            odr_points_impact = ifelse(is.na(odr_points_impact), 0, odr_points_impact)
  #     ) %>%
  #     
  #     filter(date >= as.Date('2016-07-01')), by = c("rider", "date", 'level_data')
  #   
  # ) %>%
  # 
  # mutate(pred_points = -0.013 + (rand_points_impact + 
  #       (predicted_bs * bs_points_impact) + 
  #         (one_day_race * odr_points_impact) + 
  #       (pcd_points_impact * pred_climb_difficulty))) %>%
  # 
  # select(-rand_points_impact, -pcd_points_impact, -bs_points_impact, -odr_points_impact) %>%
  # 
  # left_join(
  #   
  #   dbReadTable(con, "lme4_rider_pointswhenopp")  %>%
  #     filter(test_or_prod == "prod") %>%
  #     select(-test_or_prod) %>%
  #     unique() %>%
  #     mutate(rider = str_to_title(rider)) %>%
  #     mutate(date = as.Date(Date)) %>%
  #     select(-Date) %>%
  #     
  #     mutate(level_data = ifelse(is.na(pcd_impact), "just_rider",
  #                                ifelse(is.na(bunchsprint_impact), "pcd_added",
  #                                       ifelse(is.na(one_day_race), "bs_added", "odr_added")))) %>%
  #     
  #     # the standard deviations of random intercept and pcd impact both vary widely (increase as you move from 2015 to 2020)
  #     # we adjust here
  #     group_by(date, level_data) %>%
  #     mutate(pcd_impact_new = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact),
  #            random_intercept_new = (random_intercept - mean(random_intercept, na.rm = T)) / sd(random_intercept, na.rm = T)) %>%
  #     ungroup() %>%
  #     
  #     # this transforms them back to input into the regression equation
  #     mutate(pcd_impact = pcd_impact_new * sd(pcd_impact),
  #            random_intercept = random_intercept_new * sd(random_intercept)) %>%
  #     
  #     select(-pcd_impact_new, -random_intercept_new) %>%
  #     
  #     rename(pcd_points_impact = pcd_impact,
  #            bs_points_impact = bunchsprint_impact,
  #            rand_points_impact = random_intercept,
  #            odr_points_impact = one_day_race) %>%
  #     
  #     mutate(pcd_points_impact = ifelse(is.na(pcd_points_impact), 0, pcd_points_impact),
  #            bs_points_impact = ifelse(is.na(bs_points_impact), 0, bs_points_impact),
  #            odr_points_impact = ifelse(is.na(odr_points_impact), 0, odr_points_impact)
  #     ) %>%
  #     
  #     filter(date >= as.Date('2016-07-01')), by = c("rider", "date", 'level_data')
  #   
  # ) %>%
  # 
  # mutate(pred_pointswhenopp = -0.013 + (rand_points_impact + 
  #                                  (predicted_bs * bs_points_impact) + 
  #                                  (one_day_race * odr_points_impact) + 
  #                                  (pcd_points_impact * pred_climb_difficulty))) %>%
  # 
  # select(-rand_points_impact, -pcd_points_impact, -bs_points_impact, -odr_points_impact) %>%
  
  # left_join(
  #   
  #   dbReadTable(con, "lme4_rider_timelost")  %>%
  #     filter(test_or_prod == "prod") %>%
  #     select(-test_or_prod) %>%
  #     unique() %>%
  #     mutate(rider = str_to_title(rider)) %>%
  #     mutate(date = as.Date(Date)) %>%
  #     select(-Date) %>%
  #     
  #     mutate(level_data = ifelse(is.na(pcd_impact), "just_rider",
  #                                ifelse(is.na(bunchsprint_impact), "pcd_added",
  #                                       ifelse(is.na(one_day_race), "bs_added", "odr_added")))) %>%
  #     
  #     # the standard deviations of random intercept and pcd impact both vary widely (increase as you move from 2015 to 2020)
  #     # we adjust here
  #     group_by(date, level_data) %>%
  #     mutate(pcd_impact_new = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact),
  #            random_intercept_new = (random_intercept - mean(random_intercept, na.rm = T)) / sd(random_intercept, na.rm = T)) %>%
  #     ungroup() %>%
  #     
  #     # this transforms them back to input into the regression equation
  #     mutate(pcd_impact = pcd_impact_new * sd(pcd_impact),
  #            random_intercept = random_intercept_new * sd(random_intercept)) %>%
  #     
  #     select(-pcd_impact_new, -random_intercept_new) %>%
  #     
  #     rename(pcd_time_impact = pcd_impact,
  #            bs_time_impact = bunchsprint_impact,
  #            rand_time_impact = random_intercept,
  #            odr_time_impact = one_day_race) %>%
  #     
  #     mutate(pcd_time_impact = ifelse(is.na(pcd_time_impact), 0, pcd_time_impact),
  #            bs_time_impact = ifelse(is.na(bs_time_impact), 0, bs_time_impact),
  #            odr_time_impact = ifelse(is.na(odr_time_impact), 0, odr_time_impact)
  #     ) %>%
  #     
  #     filter(date >= as.Date('2016-07-01')), by = c("rider", "date", 'level_data')
  #   
  # ) %>%
  # 
  # mutate(pred_timelost = 500 + (rand_time_impact + 
  #                                         (predicted_bs * bs_time_impact) + 
  #                                         (one_day_race * odr_time_impact) + 
  #                                         (pcd_time_impact * pred_climb_difficulty))) %>%
  # 
  # select(-rand_time_impact, -odr_time_impact, -bs_time_impact, -pcd_time_impact) %>%

# left_join(
#   
#   dbReadTable(con, "lme4_rider_wins")  %>%
#     filter(test_or_prod == "prod") %>%
#     select(-test_or_prod) %>%
#     unique() %>%
#     mutate(rider = str_to_title(rider)) %>%
#     mutate(date = as.Date(Date)) %>%
#     select(-Date) %>%
#     
#     mutate(level_data = ifelse(is.na(pcd_impact), "just_rider",
#                                ifelse(is.na(bunchsprint_impact), "pcd_added",
#                                       ifelse(is.na(one_day_race), "bs_added", "odr_added")))) %>%
#     
#     # the standard deviations of random intercept and pcd impact both vary widely (increase as you move from 2015 to 2020)
#     # we adjust here
#     group_by(date, level_data) %>%
#     mutate(pcd_impact_new = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact),
#            random_intercept_new = (random_intercept - mean(random_intercept, na.rm = T)) / sd(random_intercept, na.rm = T)) %>%
#     ungroup() %>%
#     
#     # this transforms them back to input into the regression equation
#     mutate(pcd_impact = pcd_impact_new * sd(pcd_impact),
#            random_intercept = random_intercept_new * sd(random_intercept)) %>%
#     
#     select(-pcd_impact_new, -random_intercept_new) %>%
#     
#     rename(pcd_w_impact = pcd_impact,
#            bs_w_impact = bunchsprint_impact,
#            odr_w_impact = one_day_race) %>%
#     
#     mutate(pcd_w_impact = ifelse(is.na(pcd_w_impact), 0, pcd_w_impact),
#            bs_w_impact = ifelse(is.na(bs_w_impact), 0, bs_w_impact),
#            odr_w_impact = ifelse(is.na(odr_w_impact), 0, odr_w_impact)
#     ) %>%
#     
#     filter(date >= as.Date('2016-07-01')), by = c("rider", "date", 'level_data')
#   
# ) %>%
#   
#   # calculate team leader and success predictions using random effects
#   mutate(win_pred = -6.5 + (random_intercept + 
#                               (pred_climb_difficulty * pcd_w_impact) + 
#                               (one_day_race * odr_w_impact) + 
#                               (predicted_bs * bs_w_impact)),
#          win_pred = exp(win_pred) / (1+exp(win_pred))) %>%
#   
#   select(-random_intercept, -pcd_w_impact, -odr_w_impact, -bs_w_impact) %>%
  
  left_join(
    
    dbReadTable(con, "lme4_rider_teamleader")  %>%
      filter(test_or_prod == "prod") %>%
      select(-test_or_prod) %>%
      unique() %>%
      mutate(rider = str_to_title(rider)) %>%
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
      
      filter(date >= as.Date('2016-07-01')), by = c("rider", "date", 'level_data')
    
  ) %>%
  
  # calculate team leader and success predictions using random effects
  mutate(glmer_pred = -3.5 + (random_intercept + 
                                 (pred_climb_difficulty * pcd_tmldr_impact) + 
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
  
  filter(!is.na(level_data)) %>%
  filter(level_data == "bs_added") %>%
  
  mutate(class_small = case_when(class %in% c("2.1", "1.1", "CC") ~ ".1", 
                           class %in% c("2.2", "1.2") ~ ".2", 
                           class %in% c("WC", "2.UWT", "1.UWT") ~ "WT", 
                           class %in% c("1.Pro", "1.HC", "2.Pro", "2.HC") ~ ".HC", 
                           TRUE ~ class)) %>%
  
  # predicted_bs underestimates true bunch sprints
  mutate(predicted_bs  = predicted_bs / 0.755 * 0.82)

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
  
  pred_rank = predict(pred_rank_model, predicting_all)) %>%
  
  mutate(pred_rank = exp(pred_rank)) %>%
  unique()

#

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
  
  filter(date > '2022-01-01' & date < '2022-12-01') %>%
  
  unique() %>%
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
  
  mutate(exp_rank = exp(exp_rank),
         act_rank = exp(act_rank)) %>%
  
  mutate(#ratio = (act_points - exp_points)/races,
         ratio_rk = act_rank / exp_rank) -> recent_performances

#

predicting_all_supp %>%
  
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
  separate(url, c("j1", "url_race", "j2"), sep = "\\/") %>%
  select(-j1, -j2) -> race_errors

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
  filter(date > '2016-09-01') %>%
  arrange(desc(date)) %>%
  
  anti_join(dbGetQuery(con, "SELECT DISTINCT date FROM performance_last10races_vsmodel") %>%
              mutate(date = as.Date(date)), by = c("date"))

#

for(i in 1:length(unique_dates$date)) {
  
  MAX = unique_dates$date[[i]] - 1
  MIN = MAX - 366

  vs_exp1 <- predicting_all_supp %>%
    filter(between(date, MIN, MAX)) %>%
    
    group_by(rider) %>%
    filter(rank(desc(date), ties.method = "first") <= 10 | date >= (MAX - 25)) %>%
    summarize(#exp_points = mean(pred_points, na.rm = T),
              #act_points = mean(points_finish, na.rm = T),
              exp_rank = mean(log(pred_rank), na.rm = T),
              act_rank = mean(log(rnk), na.rm = T),
              error_rank = mean((log(rnk) - log(pred_rank)), na.rm = T),
              days_of_comps = round(mean(as.numeric(MAX - date), na.rm = T),0),
              races = n()) %>%
    ungroup() %>%
    
    mutate(exp_rank = exp(exp_rank),
           act_rank = exp(act_rank)) %>%
    
    mutate(date = unique_dates$date[[i]])
  
  vs_exp2 <- predicting_all_supp %>%
    filter(!class %in% c("NC", "WC", 'CC')) %>%
    filter(between(date, MIN, MAX)) %>%
    
    group_by(rider) %>%
    filter(rank(desc(date), ties.method = "first") <= 5 | date >= (MAX - 14)) %>%
    summarize(exp_leader = mean(shrunk_teamldr, na.rm = T),
              act_leader = mean(team_ldr, na.rm = T)) %>%
    ungroup()
  
  vs_exp <- vs_exp1 %>%
    
    left_join(vs_exp2, by = c("rider"))
  
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
    exp_leader, act_leader, exp_rank, act_rank, error_rank, races, days_of_comps
               FROM performance_last10races_vsmodel") %>%
      mutate(date = as.Date(date)), by = c('rider', 'date')
    
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
  
  cbind(pred_break_win = predict(breakaway_glm, predicting_all_supp)) %>%
  
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
