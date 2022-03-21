
library(tidyverse)
library(lubridate)
library(rvest)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#
#
#

# Bring in PCS GC Results -------------------------------------------------


pcs_gc_results <- dbReadTable(con, "pcs_gc_results") %>%
  
  filter(year >= 2014) %>%
  
  mutate(pnt = ifelse(is.na(pnt), 0, pnt)) %>%  
  mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team)),
         rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT"),
         rider = str_to_title(rider),
         race = str_to_lower(race)) %>%
  mutate(date = ifelse(url == "race/tour-de-langkawi/2020", "2020-02-08",
                       ifelse(url == "race/sibiu-cycling-tour/2019", '2019-08-01', date)),
         date = as.Date(date)) %>%
  mutate(pnt = ifelse(rnk > 25, 0, pnt)) %>%
  
  mutate(present = 1) %>%
  
  right_join(dbGetQuery(con, "SELECT DISTINCT url, year, rider
                        FROM stage_data_perf
                        WHERE year > 2013 AND class IN ('2.1', '2.HC', '2.UWT', '2.2U', '2.2', '2.Ncup', '2.Pro')") %>%
               mutate(url = ifelse(str_detect(url, "/binckbank-tour/"), str_replace(url, "/binckbank-tour/", "/benelux-tour/"), url)) %>%
               mutate(url = ifelse(str_detect(url, "/tour-du-haut-var/"), str_replace(url, "/tour-du-haut-var/", "/tour-des-alpes-maritimes-et-du-var/"), url)) %>%
               mutate(url = ifelse(str_detect(url, "/driedaagse-vd-panne/"), str_replace(url, "/driedaagse-vd-panne/", "/oxyclean-classic-brugge-de-panne/"), url)) %>%
               mutate(url = ifelse(str_detect(url, "/uae-tour/2017"), str_replace(url, "/uae-tour/2017", "/abu-dhabi-tour/2017"), url)) %>%
               mutate(url = ifelse(str_detect(url, "/uae-tour/2018"), str_replace(url, "/uae-tour/2018", "/abu-dhabi-tour/2018"), url)) %>%
               mutate(url = ifelse(str_detect(url, "/uae-tour/2016"), str_replace(url, "/uae-tour/2016", "/abu-dhabi-tour/2016"), url)) %>%
               mutate(url = ifelse(str_detect(url, "/uae-tour/2015"), str_replace(url, "/uae-tour/2015", "/abu-dhabi-tour/2015"), url)) %>%
               mutate(data = 1)) %>%
  
  mutate(pnt = ifelse(is.na(pnt), 0, pnt)) %>%
  
  # adjust for U23
  mutate(pnt = ifelse(str_sub(url, 1, nchar(url)-6) %in% c("race/giro-ciclistico-d-italia", "race/tour-de-l-avenir"), pnt*4, pnt)) %>%
  
  group_by(url) %>%
  mutate(date = max(date, na.rm = T)) %>%
  ungroup() %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT DISTINCT stage, url, year, pred_climb_difficulty
                        FROM stage_data_perf
                        WHERE year > 2013 AND time_trial = 0 AND 
               class IN ('2.1', '2.HC', '2.UWT', '2.2U', '2.2', '2.Ncup', '2.Pro')") %>%
      group_by(url, year) %>%
      summarize(avg_pcd = mean(pred_climb_difficulty, na.rm = T),
                total_stages = n()) %>%
      ungroup() %>%
      mutate(url = ifelse(str_detect(url, "/binckbank-tour/"), str_replace(url, "/binckbank-tour/", "/benelux-tour/"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/tour-du-haut-var/"), str_replace(url, "/tour-du-haut-var/", "/tour-des-alpes-maritimes-et-du-var/"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/driedaagse-vd-panne/"), str_replace(url, "/driedaagse-vd-panne/", "/oxyclean-classic-brugge-de-panne/"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2017"), str_replace(url, "/uae-tour/2017", "/abu-dhabi-tour/2017"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2018"), str_replace(url, "/uae-tour/2018", "/abu-dhabi-tour/2018"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2016"), str_replace(url, "/uae-tour/2016", "/abu-dhabi-tour/2016"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2015"), str_replace(url, "/uae-tour/2015", "/abu-dhabi-tour/2015"), url))
    
  ) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT DISTINCT stage, url, year, length
                        FROM stage_data_perf
                        WHERE year > 2013 AND time_trial = 1 AND 
               class IN ('2.1', '2.HC', '2.UWT', '2.2U', '2.2', '2.Ncup', '2.Pro')") %>%
      group_by(url, year) %>%
      summarize(time_trial_kms = sum(length, na.rm = T),
                total_TT = n()) %>%
      ungroup() %>%
      mutate(url = ifelse(str_detect(url, "/binckbank-tour/"), str_replace(url, "/binckbank-tour/", "/benelux-tour/"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/tour-du-haut-var/"), str_replace(url, "/tour-du-haut-var/", "/tour-des-alpes-maritimes-et-du-var/"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/driedaagse-vd-panne/"), str_replace(url, "/driedaagse-vd-panne/", "/oxyclean-classic-brugge-de-panne/"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2017"), str_replace(url, "/uae-tour/2017", "/abu-dhabi-tour/2017"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2018"), str_replace(url, "/uae-tour/2018", "/abu-dhabi-tour/2018"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2016"), str_replace(url, "/uae-tour/2016", "/abu-dhabi-tour/2016"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2015"), str_replace(url, "/uae-tour/2015", "/abu-dhabi-tour/2015"), url))
    
  ) %>%
  
  mutate(time_trial_kms = ifelse(is.na(time_trial_kms), 0, time_trial_kms),
         total_TT = ifelse(is.na(total_TT), 0, total_TT)) %>%
  
  group_by(url, year) %>%
  mutate(max_pnt = max(pnt, na.rm = T)) %>%
  ungroup()

#
#
#

# Calculate GC ranking model impact ---------------------------------------


predictiveness_gc_rankings <- pcs_gc_results %>%
  
  filter(year >= 2016) %>%
  
  select(rider, url, race, year, date, rnk, avg_pcd, time_trial_kms, total_stages) %>%
  unique() %>%
  
  inner_join(dbGetQuery(con, "SELECT DISTINCT rider, url, team FROM stage_data_perf"), by = c("rider", "url")) %>%
  
  left_join(dbGetQuery(con, "SELECT D, rider, top7_wtd, top3_wtd, perc_max, races 
                       FROM rider_gc_rankings") %>%
              mutate(D = as.Date(D)), by = c("date" = "D", "rider")) %>%
  
  mutate(top7_wtd = ifelse(is.na(top7_wtd), 0, top7_wtd)) %>%
  mutate(top3_wtd = ifelse(is.na(top3_wtd), 0, top3_wtd)) %>%
  
  group_by(url, year) %>%
  mutate(behind_best = max(top7_wtd, na.rm = T) - top7_wtd,
         top_wtd = (top3_wtd + top7_wtd)/2,
         gc_rnk = rank(desc(top_wtd), ties.method = "min")) %>%
  ungroup() %>%
  
  group_by(url, team, year) %>%
  mutate(tm_rnk = rank(desc(top_wtd), ties.method = "min")) %>%
  ungroup()

#

gc_mod_preds <- predictiveness_gc_rankings %>%
  filter(avg_pcd >= 3) %>%
  mutate(rnk = ifelse(is.na(rnk), 200, rnk)) %>% 
  filter(rnk <= 200 & rnk > 0) %>% 
  mutate(win = ifelse(rnk==1,1,0)) %>% 
  
  glm(win ~ top_wtd + 
        log(gc_rnk) + 
        tm_rnk + 
        top_wtd:avg_pcd +
        log(gc_rnk):avg_pcd, data = ., family = "binomial")

#

applied_predictions <- predictiveness_gc_rankings %>%
  
  mutate(rnk = ifelse(is.na(rnk), 200, rnk)) %>% 
  filter(rnk <= 200 & rnk > 0) %>% 
  mutate(win = ifelse(rnk==1,1,0)) %>%
  
  mutate(pred = predict(gc_mod_preds, .),
         pred = exp(pred)/(1+exp(pred))) %>%
  
  group_by(url, year) %>%
  mutate(pred = pred / sum(pred)) %>%
  ungroup() %>%
  
  mutate(pred = ifelse(pred < 0.005, 0, pred)) %>%
  
  group_by(url, year) %>%
  mutate(pred = pred / sum(pred)) %>%
  ungroup()

#
#
#

# Bring in Stage by Stage GC data -----------------------------------------


stage_by_stage_GC <- dbReadTable(con, "pcs_stage_by_stage_gc") %>%

  mutate(rider = str_to_title(tolower(rider)),
         gc_time = str_replace(gc_time, "//+", ""),
         gc_time = str_replace(gc_time, ",,", ""),
         old_gc_time = gc_time,
         gcn = nchar(gc_time)) %>% 
  
  mutate(duplic = ifelse(str_sub(gc_time, 1, floor(gcn/2)) == str_sub(gc_time, ceiling(gcn/2)+1, gcn), TRUE, FALSE),
         gc_time = ifelse(duplic == TRUE, str_sub(gc_time, 1, floor(gcn/2)), gc_time)) %>%
  
  separate(gc_time, into = c("hours","minutes", "seconds"), sep = ":") %>%
  mutate(h = is.na(seconds),
         seconds = ifelse(h == TRUE, minutes, seconds),
         minutes = ifelse(h == TRUE, hours, minutes),
         hours = ifelse(h == TRUE, 0, hours),
         seconds = as.numeric(seconds),
         minutes = as.numeric(minutes),
         hours = as.numeric(hours),
         total_seconds_back = (seconds + (minutes*60) + (hours*3600)),
         total_seconds_back = ifelse(gc_rnk == "1", 0, total_seconds_back)) %>%
  
  filter(!gc_rnk == "") %>%
  
  mutate(gc_rnk = as.numeric(gc_rnk)) %>%
  
  filter(!is.na(gc_rnk)) %>%
  
  group_by(race, year) %>%
  mutate(final = ifelse(max(stage, na.rm = T)==stage, gc_rnk, NA),
         stages_left = max(stage, na.rm = T)-stage) %>%
  ungroup() %>%
  
  group_by(rider, race, year) %>%
  mutate(final = mean(final, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(race, year) %>%
  mutate(final = ifelse(is.na(final) | final == "NaN", max(final, na.rm = T)+1, final)) %>%
  ungroup() %>%
  
  mutate(race = tolower(race),
         stage = as.character(stage)) %>%  
  
  left_join(
    
    dbGetQuery(con, "SELECT DISTINCT stage, race, year, date, class, time_trial, team_time_trial, url, pred_climb_difficulty, bunch_sprint
               FROM stage_data_perf") %>%
      
      group_by(race, year) %>%
      mutate(min = min(as.numeric(str_replace(stage, "b", "")))) %>%
      ungroup() %>%
      
      mutate(stage = ifelse(min == 0, as.character(as.numeric(stage) + 1), stage)) %>%
      
      left_join(dbGetQuery(con, "SELECT * FROM predictions_stage_bunchsprint") %>%
                   select(-bunch_sprint) %>%
                   unique(), by = c("stage" = "stage", "race", "year")) %>%
      
      mutate(predicted_bs = ifelse(is.na(predicted_bs), 0, predicted_bs)) %>%
      
      mutate(GC_stage = ifelse(time_trial == 1 | team_time_trial == 1, 1, 1-predicted_bs),
             GC_stage = ifelse(is.na(GC_stage), 0, GC_stage)) %>%
      select(date, stage, race, year, GC_stage) %>%
      unique() %>%
      arrange(date, year, race, stage) %>%
      select(-date) %>%
      group_by(year, race) %>%
      mutate(GC_done = cumsum(GC_stage),
             GC_all = sum(GC_stage),
             GC_done = lag(GC_done)) %>%
      ungroup() %>%
      mutate(GC_done = ifelse(is.na(GC_done), 0, GC_done),
             percent_through = (GC_done / GC_all),
             percent_through = ifelse(GC_all == 0, 1, percent_through)), by = c("stage", "race", "year")
    
  ) %>%
  
  arrange(year, race, rider, stage) %>%
  
  select(-GC_done, -GC_stage) %>%
  
  arrange(rider, race, year, as.numeric(stage)) %>%
  
  group_by(rider, race, year) %>%
  mutate(total_seconds_back = lag(total_seconds_back),
         total_seconds_back = ifelse(stage == 1, 0, total_seconds_back)) %>%
  ungroup()

#
#
#

# Calculate GC Win Probs during race --------------------------------------

who_has_GC_chances <- stage_by_stage_GC %>%
  
  select(-duplic, -gcn, -old_gc_time, -hours, -minutes, -seconds, -h, -date) %>%
  
  inner_join(applied_predictions %>%
               
               filter(avg_pcd >= 3) %>%
               
               arrange(url, !is.na(race)) %>%
               fill(race, .direction = "down") %>%
               select(-date, -team) %>%
               rename(pre_gc_rnk = gc_rnk) %>%
               filter(!is.na(pred)), by = c("race", "year", "rider")) %>%
  
  left_join(dbGetQuery(con, "SELECT DISTINCT stage, race, year, date, class, time_trial, url
               FROM stage_data_perf") %>%
              
              group_by(race, year) %>%
              mutate(min = min(as.numeric(str_replace(stage, "b", "")))) %>%
              ungroup() %>%
              
              mutate(stage = ifelse(min == 0, as.character(as.numeric(stage) + 1), stage)), by = c("stage", "race", "year", "url")) %>%
  
  group_by(stage, race, year, url) %>%
  mutate(contender_time = ifelse(pred > 0, total_seconds_back, NA),
         contender_time = min(contender_time, na.rm = T),
         seconds_behind_contenders = total_seconds_back - contender_time) %>%
  ungroup() %>%
  
  mutate(GC_rem = (GC_all*(1-percent_through))) %>%
  unique()

# no one has won GC since 2014 if they're more than 150 seconds per GC stage remaining behind
# best placed contender
# only Carapaz is 2019 Giro has won being more than 300 seconds behind *at any point in race*

library(xgboost)

#

xgb.train <- xgb.DMatrix(
  
  data = as.matrix(who_has_GC_chances %>%
                     filter(year < 2022) %>%
                     select(seconds_behind_contenders, percent_through, GC_rem, pred, pre_gc_rnk)),
  
  label = who_has_GC_chances %>%
    filter(year < 2022) %>%
    select(win) %>%
    .[[1]]
  
)

# test

xgb.test <- xgb.DMatrix(
  
  data = as.matrix(who_has_GC_chances %>%
                     filter(year >= 2022) %>%
                     select(seconds_behind_contenders, percent_through, GC_rem, pred, pre_gc_rnk)),
  
  label = who_has_GC_chances %>%
    filter(year >= 2022) %>%
    select(win) %>%
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
                       verbose = 1)

#
#
# xgb Importance

xgb.importance(model = gbm_model)

gbm_model$best_score

#

gbm_gcwins_predictions = cbind(
  
  model_pred = predict(gbm_model, 
                 as.matrix(who_has_GC_chances %>%
                             select(seconds_behind_contenders, percent_through, GC_rem, pred, pre_gc_rnk), reshape=T)),
  
  who_has_GC_chances) %>%
  
  select(rider, stage, race, year, url, class, date, total_seconds_back, seconds_behind_contenders, pre_winprob = pred, 
         percent_through, final_gc_rnk = rnk, model_winprob = model_pred) %>%
  
  group_by(stage, race, year) %>%
  mutate(model_winprob = model_winprob / sum(model_winprob, na.rm = T)) %>%
  ungroup()

#
#
#

dbSendQuery(con, "DELETE FROM stage_by_stage_gc_winprob")

dbWriteTable(con, "stage_by_stage_gc_winprob", gbm_gcwins_predictions, append = TRUE, row.names = FALSE)