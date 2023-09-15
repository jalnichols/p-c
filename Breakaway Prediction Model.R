
library(tidyverse)
library(DBI)

dbDisconnect(con)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      port = 5432,
                      host = 'localhost',
                      dbname = "cycling",
                      user = "postgres",
                      password = "braves")


options(dplyr.summarise.inform = FALSE)
options(tidyverse.quiet = TRUE)

#

All_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE date > '2017-01-01'") %>%
  
  mutate(rider = case_when(rider == "O Connor Ben" ~ "O'connor Ben",
                           rider == "O Brien Kelland" ~ "O'brien Kelland",
                           rider == "Ghirmay Hailu Biniam" ~ "Girmay Biniam",
                           rider == "Girmay Hailu Biniam" ~ "Girmay Biniam",
                           rider == "O Donnell Bailey" ~ "O'donnell Bailey",
                           rider == "D Heygere Gil" ~ "D'heygere Gil",
                           rider == "Skjelmose Jensen Mattias" ~ "Skjelmose Mattias",
                           rider == "Wright Alfred" ~ "Wright Fred",
                           TRUE ~ rider)) %>%
  
  filter(time_trial == 0 & team_time_trial == 0) %>%
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
           (class %in% c("2.1", "1.1") & tour == "Europe Tour") |
           race %in% c("tour colombia 2.1", "colombia 2.1", "vuelta a san juan internacional", "saudi tour")) %>%
  
  left_join(read_delim("cobbles.csv") %>% mutate(stage = as.character(stage))) %>% 
  
  mutate(cobbles = ifelse(is.na(cobbles), 0, cobbles)) %>%
  
  mutate(final_group = ifelse(gain_1st <= 2, 1, 0),
         sprinted = ifelse(gain_1st == 0 & rnk <= 20, 1, 0),
         top10 = ifelse(rnk <= 10, 1, 0)) %>%
  
  select(-gain_20th)

#

breakaway_manual <- read_delim("breakaway-riders-2019.csv",
                               col_types = "cccc") %>%
  rename(url = `...4`) %>%
  fill(url, .direction = "down") %>%
  
  mutate(year = ifelse(is.na(race), "", str_sub(race, nchar(race)-3, nchar(race))),
         year = ifelse(year == "", str_sub(url, nchar(url)-3, nchar(url)), year)) %>%
  
  fill(race, .direction = "down") %>%
  fill(year, .direction = "down") %>%
  fill(stage, .direction = "down") %>%
  
  left_join(dbGetQuery(con, "SELECT rider, COUNT(*) as N FROM pcs_stage_data WHERE year >= 2017
                       GROUP BY rider"))

#

breakaway_riders <- dbReadTable(con, "pcs_km_breakaway_1") %>%
  
  mutate(rider = str_to_title(rider)) %>%
  
  group_by(race, stage, year) %>%
  mutate(average = mean(km_before_peloton)) %>%
  ungroup() %>%
  
  mutate(breakaway_rider = ifelse(km_before_peloton > 40, 1, 0)) %>%
  
  mutate(url = str_replace(url, "/race", "race")) %>% 
  
  select(-race, -average, -km_in_first_group, -km_before_peloton) %>%
  
  rbind(breakaway_manual %>%
          select(-race, -n) %>%
          mutate(breakaway_rider = 1)) %>%
  
  inner_join(All_data %>%
               select(stage, url, rider, rnk, class, race, date) %>%
               mutate(rider = str_replace(rider, "O C", "Oc"), 
                      rider = str_replace(rider, "O B", "Ob"), 
                      rider = str_replace(rider, "D H", "Dh"))) %>%
  
  unique()

#

winner_type <- All_data %>%
  
  filter(rnk == 1 & year >= 2017) %>%
  
  inner_join(breakaway_riders %>%
               select(url, stage) %>%
               unique()) %>%
  
  select(rider, stage, race, year, class, date, length, url, pred_climb_difficulty, uphill_finish, total_vert_gain,
         sof, bunch_sprint, tour, one_day_race, grand_tour, gain_gc) %>%
  
  mutate(stage_join = as.character(stage)) %>%
  inner_join(dbGetQuery(con, "SELECT * FROM predictions_xgboost_stage_bunchsprint") %>%
               select(-bunch_sprint) %>%
               select(stage, url, predicted_bs = model_pred) %>%
               unique(), by = c("stage_join" = "stage", "url")) %>%
  select(-stage_join) %>%
  
  unique() %>%
  
  left_join(
    
    breakaway_riders %>%
      filter(breakaway_rider == 1 & rnk == 1) %>%
      select(rider, url, stage, breakaway_rider) %>%
      unique(), by = c("rider", "url", "stage")
    
  ) %>%
  
  mutate(breakaway_rider = ifelse(is.na(breakaway_rider), 0, breakaway_rider)) %>%
  
  mutate(winner_type = ifelse(breakaway_rider == 0, "Peloton", "Breakaway")) %>%
  
  inner_join(
    dbReadTable(con, "race_prediction_errors") %>%
      mutate(url = paste0("race/", url_race, "/", year)) %>%
      select(stage, url, error, correlation, winner,
             perc_rk_corr, perc_rk_err, class) %>%
      group_by(class) %>%
      mutate(winner_rel = winner - mean(winner, na.rm = T),
             perc_rk_corr = percent_rank(perc_rk_corr),
             perc_rk_err = percent_rank(perc_rk_err)) %>%
      ungroup() %>%
      select(-class), by = c("stage", "url")
  ) %>%
  
  mutate(same_time_gc = gain_gc == 0) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, stage, url, date, seconds_behind_contenders, model_winprob, virtual_gc
               FROM stage_by_stage_gc_winprob") %>%
      mutate(url = ifelse(url == "race/benelux-tour/2020", "race/binckbank-tour/2020", url)) %>%
      mutate(rider = ifelse(rider == "O'connor Ben", "O Connor Ben", rider)) %>%
      mutate(stage = ifelse(url %in% c('race/tour-cycliste-international-la-provence/2022',
                                       'race/tour-de-romandie/2021',
                                       'race/tour-de-romandie/2022'), as.numeric(stage) -1, stage)) %>%
      mutate(date = as.Date(date)) %>%
      mutate(virtual_gc = virtual_gc <= 5,
             model_winprob = ifelse(model_winprob < 0.005, 1, 0)), by = c("rider", "url", "date", "stage"))

#
#
#

model_breakaway_winner <- glm(breakaway_rider ~ 
                                bunch_sprint +
                                winner_rel +
                                gain_gc +
                                same_time_gc +
                                grand_tour +
                                model_winprob +
                                virtual_gc
                                ,
                              data = winner_type,
                              family = "binomial")

summary(model_breakaway_winner)

in_sample_predictions <- winner_type %>%
  
  mutate(coef = predict(model_breakaway_winner, .),
         post_break_win = exp(coef)/(1+exp(coef)))

out_sample_predictions <- All_data %>%
  
  filter(rnk == 1 & year >= 2019 & one_day_race == 0) %>%
  
  anti_join(breakaway_riders %>%
               select(url) %>%
               unique()) %>%
  
  select(rider, stage, race, year, class, date, length, url, pred_climb_difficulty, uphill_finish, total_vert_gain,
         sof, bunch_sprint, tour, one_day_race, grand_tour, gain_gc) %>%
  
  mutate(stage_join = as.character(stage)) %>%
  inner_join(dbGetQuery(con, "SELECT * FROM predictions_stage_bunchsprint") %>%
               select(-bunch_sprint) %>%
               unique(), by = c("stage_join" = "stage", "race", "year")) %>%
  select(-stage_join) %>%
  
  unique() %>%
  
  # predicted_bs underestimates true bunch sprints
  mutate(predicted_bs  = predicted_bs / 0.755 * 0.82) %>%
  inner_join(
    dbReadTable(con, "race_prediction_errors") %>%
      mutate(url = paste0("race/", url_race, "/", year)) %>%
      select(stage, url, error, correlation, winner,
             perc_rk_corr, perc_rk_err, class) %>%
      
      group_by(class) %>%
      mutate(winner_rel = winner - mean(winner, na.rm = T),
             perc_rk_corr = percent_rank(perc_rk_corr),
             perc_rk_err = percent_rank(perc_rk_err)) %>%
      ungroup() %>%
      select(-class), by = c("stage", "url")
  ) %>%
  
  mutate(same_time_gc = gain_gc == 0) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, stage, url, date, seconds_behind_contenders, model_winprob, virtual_gc
               FROM stage_by_stage_gc_winprob") %>%
      mutate(url = ifelse(url == "race/benelux-tour/2020", "race/binckbank-tour/2020", url)) %>%
      mutate(rider = ifelse(rider == "O'connor Ben", "O Connor Ben", rider)) %>%
      mutate(stage = ifelse(url %in% c('race/tour-cycliste-international-la-provence/2022',
                                       'race/tour-de-romandie/2021',
                                       'race/tour-de-romandie/2022'), as.numeric(stage) -1, stage)) %>%
      mutate(date = as.Date(date)) %>%
      mutate(virtual_gc = virtual_gc <= 5,
             model_winprob = ifelse(model_winprob < 0.005, 1, 0)), by = c("rider", "url", "date", "stage")) %>%
  
  unique() %>%
  
  mutate(coef = predict(model_breakaway_winner, .),
         post_break_win = exp(coef)/(1+exp(coef)))

#
#
#
#
#

# XG BOOSTED BREAKWAY -----------------------------------------------------

winner_type <- All_data %>%
  
  filter(rnk == 1 & year >= 2017) %>%
  
  inner_join(breakaway_riders %>%
               select(url, stage) %>%
               unique()) %>%
  
  select(rider, stage, race, year, class, date, length, url, pred_climb_difficulty, uphill_finish, total_vert_gain,
         sof, bunch_sprint, tour, one_day_race, grand_tour, gain_gc) %>%
  
  group_by(race, year, url, class) %>%
  mutate(perc_thru = as.numeric(stage) / max(as.numeric(stage), na.rm = T)) %>%
  ungroup() %>%
  
  mutate(stage_join = as.character(stage)) %>%
  inner_join(dbGetQuery(con, "SELECT * FROM predictions_xgboost_stage_bunchsprint") %>%
               select(-bunch_sprint) %>%
               select(stage, url, predicted_bs = model_pred) %>%
               unique(), by = c("stage_join" = "stage", "url")) %>%
  select(-stage_join) %>%
  
  unique() %>%
  
  left_join(
    
    breakaway_riders %>%
      filter(breakaway_rider == 1 & rnk == 1) %>%
      select(rider, url, stage, breakaway_rider) %>%
      unique(), by = c("rider", "url", "stage")
    
  ) %>%
  
  mutate(breakaway_rider = ifelse(is.na(breakaway_rider), 0, breakaway_rider)) %>%
  
  mutate(winner_type = ifelse(breakaway_rider == 0, 0, 1))

#

library(xgboost)

xgb.train <- xgb.DMatrix(
  
  data = as.matrix(winner_type %>%
                     filter(year < 2022) %>%
                     select(predicted_bs, grand_tour, pred_climb_difficulty,
                            length, uphill_finish, perc_thru, total_vert_gain,
                            one_day_race) %>%
                     filter(!is.na(total_vert_gain))),
  
  label = winner_type %>%
    filter(year < 2022) %>%
    filter(!is.na(total_vert_gain)) %>%
    select(winner_type) %>%
    .[[1]]
  
)

# test

xgb.test <- xgb.DMatrix(
  
  data = as.matrix(winner_type %>%
                     filter(year >= 2022) %>%
                     select(predicted_bs, grand_tour, pred_climb_difficulty,
                            length, uphill_finish, perc_thru, total_vert_gain,
                            one_day_race) %>%
                     filter(!is.na(total_vert_gain))),
  
  label = winner_type %>%
    filter(year >= 2022) %>%
    filter(!is.na(total_vert_gain)) %>%
    select(winner_type) %>%
    .[[1]]
  
)

# outline parameters

params <- list(
  
  booster = "gbtree",
  eta = 0.01,
  max_depth = 6,
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
                       monotone_constraints = c(-1, 1, 1, 0, 0, 0, 1, 0),
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

gbm_breakaway_predictions = cbind(
  
  model_pred = predict(gbm_model, 
                       as.matrix(winner_type %>%
                                   filter(year >= 2022) %>%
                                   select(predicted_bs, grand_tour, pred_climb_difficulty,
                                          length, uphill_finish, perc_thru, total_vert_gain,
                                          one_day_race) %>%
                                   filter(!is.na(total_vert_gain)), reshape=T)),
  
  winner_type %>%
    filter(year >= 2022) %>%
    filter(!is.na(total_vert_gain)))

write_rds(gbm_model, "Stored models/breakaway-prediction-model.rds")

#

all_gbm_breakaway_predictions = cbind(
  
  model_pred = predict(gbm_model, 
                       as.matrix(winner_type %>%
                                   filter(year >= 2000) %>%
                                   select(predicted_bs, grand_tour, pred_climb_difficulty,
                                          length, uphill_finish, perc_thru, total_vert_gain) %>%
                                   filter(!is.na(total_vert_gain)), reshape=T)),
  
  winner_type %>%
    filter(year >= 2000) %>%
    filter(!is.na(total_vert_gain)))

#
#
#
#
#
#
#
#
#


# Breakaway Probability ---------------------------------------------------


breakaway_all <- dbReadTable(con, "pcs_km_breakaway_1") %>%
  
  mutate(rider = str_to_title(rider)) %>%
  
  group_by(race, stage, year) %>%
  mutate(average = mean(km_before_peloton)) %>%
  ungroup() %>%
  
  mutate(breakaway_rider = ifelse(km_before_peloton > 40, 1, 0)) %>%
  
  mutate(url = str_replace(url, "/race", "race")) %>% 
  
  select(-race, -average, -km_in_first_group, -km_before_peloton) %>%
  
  rbind(breakaway_manual %>%
          select(-race, -n) %>%
          mutate(breakaway_rider = 1)) %>%
  
  unique() %>%
  
  select(-year) %>%
  
  full_join(All_data %>%
               select(stage, url, rider, rnk, class) %>%
               mutate(rider = str_replace(rider, "O C", "Oc"), 
                      rider = str_replace(rider, "O B", "Ob"), 
                      rider = str_replace(rider, "D H", "Dh"))) %>%
  
  group_by(stage, url) %>%
  filter(sum(!is.na(breakaway_rider)) > 0) %>% 
  ungroup() %>% 
  
  mutate(breakaway_rider = ifelse(is.na(breakaway_rider), 0, breakaway_rider)) %>% 
  filter(class %in% c("2.Pro", "2.UWT", "2.1",
                      "1.Pro", "1.UWT", "1.1",
                      "WC", "Olympics", "CC")) %>%
  
  inner_join(
    All_data %>%
      select(stage, date, length, url, pred_climb_difficulty, uphill_finish, total_vert_gain,
             sof, bunch_sprint, tour, grand_tour) %>%
      unique(), by = c("stage", "url")
  )

#
#
#

model_breaka <- lme4::glmer(breakaway_rider ~ (1 + pred_climb_difficulty | rider) + (0 + bunch_sprint | rider) +
                              (0 + sof | rider),
                            
                            data = breakaway_all %>%
                              filter(date > '2021-01-01') %>%
                              group_by(rider) %>%
                              filter(n() >= 30) %>%
                              ungroup(),
                            family = "binomial")

#

summary(model_breaka)

random_effects <- lme4::ranef(model_breaka)[[1]] %>% 
  rownames_to_column() %>%
  rename(rider = rowname, 
         random_intercept = `(Intercept)`,
         pcd_impact = pred_climb_difficulty,
         bunchsprint_impact = bunch_sprint) %>%
  mutate(date = lubridate::today(),
         model_intercept = lme4::fixef(model_breaka))

#
#
#

gt_dates <- All_data %>%
  filter(date > '2018-12-31') %>%
  select(date) %>%
  unique() %>%
  
  anti_join(dbGetQuery(con, "SELECT DISTINCT date FROM lme4_rider_breakaway")) %>%
  
  rbind(tibble(date = as.Date(lubridate::today())))

#
#
#

for(D in 2:nrow(gt_dates)) {
  
  tictoc::tic()
  
  model_breaka <- lme4::glmer(breakaway_rider ~ (1 + pred_climb_difficulty | rider) + (0 + bunch_sprint | rider),
                              
                              data = breakaway_all %>%
                                filter(date < gt_dates$date[[D]] & date >= (gt_dates$date[[D]] - 1100)) %>%
                                group_by(rider) %>%
                                filter(n() >= 20) %>%
                                ungroup(),
                              family = "binomial")
  
  summary(model_breaka)
  
  random_effects <- lme4::ranef(model_breaka)[[1]] %>% 
    rownames_to_column() %>%
    rename(rider = rowname, 
           random_intercept = `(Intercept)`,
           pcd_impact = pred_climb_difficulty,
           bunchsprint_impact = bunch_sprint) %>%
    mutate(date = gt_dates$date[[D]],
           model_intercept = lme4::fixef(model_breaka))
  
  dbWriteTable(con, "lme4_rider_breakaway", random_effects, append = TRUE, row.names = FALSE)
 
  print(D)
  tictoc::toc()
   
}

#

winner_type %>%
  select(url, stage, winner_type) %>% 
  inner_join(All_data, by = c("url", "stage")) %>% 
  left_join(breakaway_riders %>% 
              select(url, stage, rider, breakaway_rider), by = c("url", "stage", "rider")) %>% 
  
  group_by(rider) %>%
  summarize(races =n(), 
            breaks = sum(breakaway_rider == 1, na.rm = T), 
            break_won = sum(winner_type == "Breakaway" & breakaway_rider == 1, na.rm = T),
            they_won = sum(rnk == 1 & breakaway_rider == 1, na.rm = T), 
            in_break = sum(breakaway_rider, na.rm = T)) %>%
  ungroup() %>% 
  
  mutate(in_winning_break = break_won / breaks, 
         in_break_at_all = in_break / races,
         win_perc = they_won / in_break) -> break_success_metrics