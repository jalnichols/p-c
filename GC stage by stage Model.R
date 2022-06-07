
library(tidyverse)
library(lubridate)
library(rvest)
library(DBI)

dbDisconnect(con)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      port = 5432,
                      host = 'localhost',
                      dbname = "cycling",
                      user = "postgres",
                      password = "braves")

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
  
  inner_join(dbGetQuery(con, "SELECT DISTINCT rider, url, team FROM stage_data_perf") %>%
               mutate(url = ifelse(str_detect(url, "/binckbank-tour/"), str_replace(url, "/binckbank-tour/", "/benelux-tour/"), url)) %>%
               mutate(url = ifelse(str_detect(url, "/tour-du-haut-var/"), str_replace(url, "/tour-du-haut-var/", "/tour-des-alpes-maritimes-et-du-var/"), url)) %>%
               mutate(url = ifelse(str_detect(url, "/driedaagse-vd-panne/"), str_replace(url, "/driedaagse-vd-panne/", "/oxyclean-classic-brugge-de-panne/"), url)) %>%
               mutate(url = ifelse(str_detect(url, "/uae-tour/2017"), str_replace(url, "/uae-tour/2017", "/abu-dhabi-tour/2017"), url)) %>%
               mutate(url = ifelse(str_detect(url, "/uae-tour/2018"), str_replace(url, "/uae-tour/2018", "/abu-dhabi-tour/2018"), url)) %>%
               mutate(url = ifelse(str_detect(url, "/uae-tour/2016"), str_replace(url, "/uae-tour/2016", "/abu-dhabi-tour/2016"), url)) %>%
               mutate(url = ifelse(str_detect(url, "/uae-tour/2015"), str_replace(url, "/uae-tour/2015", "/abu-dhabi-tour/2015"), url)), by = c("rider", "url")) %>%
  
  # NEED: make sure GC historical model has run for all dates
  
  left_join(dbGetQuery(con, "SELECT date, rider, top7_wtd, top3_wtd, perc_max, races 
                       FROM rider_gc_rankings") %>%
              mutate(date = as.Date(date)), by = c("date", "rider")) %>%
  
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

#write_rds(gc_mod_preds, "Stored models/very-basic-gc-preds-model.rds")

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

gc_stages_rating <- dbGetQuery(con, "SELECT DISTINCT stage, race, year, date, class, time_trial, 
    team_time_trial, url, pred_climb_difficulty, bunch_sprint
               FROM stage_data_perf") %>%
  
  mutate(url = ifelse(str_detect(url, "/binckbank-tour/"), str_replace(url, "/binckbank-tour/", "/benelux-tour/"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/tour-du-haut-var/"), str_replace(url, "/tour-du-haut-var/", "/tour-des-alpes-maritimes-et-du-var/"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/driedaagse-vd-panne/"), str_replace(url, "/driedaagse-vd-panne/", "/oxyclean-classic-brugge-de-panne/"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/uae-tour/2017"), str_replace(url, "/uae-tour/2017", "/abu-dhabi-tour/2017"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/uae-tour/2018"), str_replace(url, "/uae-tour/2018", "/abu-dhabi-tour/2018"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/uae-tour/2016"), str_replace(url, "/uae-tour/2016", "/abu-dhabi-tour/2016"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/uae-tour/2015"), str_replace(url, "/uae-tour/2015", "/abu-dhabi-tour/2015"), url)) %>%
  
  group_by(race, year, url) %>%
  mutate(min = min(as.numeric(str_replace(stage, "b", "")))) %>%
  ungroup() %>%
  
  mutate(stage = ifelse(min == 0, as.character(as.numeric(stage) + 1), stage)) %>%
  
  left_join(dbGetQuery(con, "SELECT url, predicted_bs, stage FROM predictions_stage_bunchsprint") %>%
              mutate(url = ifelse(str_detect(url, "/binckbank-tour/"), str_replace(url, "/binckbank-tour/", "/benelux-tour/"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/tour-du-haut-var/"), str_replace(url, "/tour-du-haut-var/", "/tour-des-alpes-maritimes-et-du-var/"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/driedaagse-vd-panne/"), str_replace(url, "/driedaagse-vd-panne/", "/oxyclean-classic-brugge-de-panne/"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2017"), str_replace(url, "/uae-tour/2017", "/abu-dhabi-tour/2017"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2018"), str_replace(url, "/uae-tour/2018", "/abu-dhabi-tour/2018"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2016"), str_replace(url, "/uae-tour/2016", "/abu-dhabi-tour/2016"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2015"), str_replace(url, "/uae-tour/2015", "/abu-dhabi-tour/2015"), url)) %>%
              unique(), by = c("stage" = "stage", "url")) %>%
  
  mutate(predicted_bs = ifelse(is.na(predicted_bs), 0, predicted_bs)) %>%
  
  mutate(GC_stage = ifelse(time_trial == 1 | team_time_trial == 1, 1, 1-predicted_bs),
         GC_stage = ifelse(is.na(GC_stage), 0, GC_stage)) %>%
  select(date, stage, race, year, GC_stage, url) %>%
  unique() %>%
  arrange(date, year, race, stage) %>%
  select(-date) %>%
  group_by(year, race, url) %>%
  mutate(GC_done = cumsum(GC_stage),
         GC_all = sum(GC_stage),
         GC_done = lag(GC_done)) %>%
  ungroup() %>%
  mutate(GC_done = ifelse(is.na(GC_done), 0, GC_done),
         percent_through = (GC_done / GC_all),
         percent_through = ifelse(GC_all == 0, 1, percent_through))

#
#
#

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
  
  group_by(race, year, url) %>%
  mutate(final = ifelse(max(stage, na.rm = T)==stage, gc_rnk, NA),
         stages_left = max(stage, na.rm = T)-stage) %>%
  ungroup() %>%
  
  group_by(rider, race, year, url) %>%
  mutate(final = mean(final, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(race, year, url) %>%
  mutate(final = ifelse(is.na(final) | final == "NaN", max(final, na.rm = T)+1, final)) %>%
  ungroup() %>%
  
  mutate(race = tolower(race),
         stage = as.character(stage)) %>%  
  
  mutate(url = ifelse(str_detect(url, "/binckbank-tour/"), str_replace(url, "/binckbank-tour/", "/benelux-tour/"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/tour-du-haut-var/"), str_replace(url, "/tour-du-haut-var/", "/tour-des-alpes-maritimes-et-du-var/"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/driedaagse-vd-panne/"), str_replace(url, "/driedaagse-vd-panne/", "/oxyclean-classic-brugge-de-panne/"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/uae-tour/2017"), str_replace(url, "/uae-tour/2017", "/abu-dhabi-tour/2017"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/uae-tour/2018"), str_replace(url, "/uae-tour/2018", "/abu-dhabi-tour/2018"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/uae-tour/2016"), str_replace(url, "/uae-tour/2016", "/abu-dhabi-tour/2016"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/uae-tour/2015"), str_replace(url, "/uae-tour/2015", "/abu-dhabi-tour/2015"), url)) %>%
  
  select(-race) %>%
  
  left_join(
    
    gc_stages_rating, by = c("stage", "url", "year")
    
  ) %>%
  
  arrange(year, race, rider, stage) %>%
  
  select(-GC_done, -GC_stage) %>%
  
  arrange(rider, race, year, as.numeric(stage)) %>%
  
  group_by(rider, race, year, url) %>%
  mutate(total_seconds_back = lag(total_seconds_back),
         total_seconds_back = ifelse(stage == 1, 0, total_seconds_back)) %>%
  ungroup()

#
#
#

# Calculate GC Win Probs during race --------------------------------------

who_has_GC_chances <- stage_by_stage_GC %>%
  
  select(-duplic, -gcn, -old_gc_time, -hours, -minutes, -seconds, -h, -date, -race, -year, -class) %>%
  
  inner_join(applied_predictions %>%
               
               #filter(avg_pcd >= 3) %>%
               
               arrange(url, !is.na(race)) %>%
               fill(race, .direction = "down") %>%
               select(-date, -team) %>%
               rename(pre_gc_rnk = gc_rnk) %>%
               filter(!is.na(pred)), by = c("rider", "url")) %>%
  
  left_join(dbGetQuery(con, "SELECT DISTINCT stage, date, class, time_trial, url
               FROM stage_data_perf") %>%
              
              mutate(url = ifelse(str_detect(url, "/binckbank-tour/"), str_replace(url, "/binckbank-tour/", "/benelux-tour/"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/tour-du-haut-var/"), str_replace(url, "/tour-du-haut-var/", "/tour-des-alpes-maritimes-et-du-var/"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/driedaagse-vd-panne/"), str_replace(url, "/driedaagse-vd-panne/", "/oxyclean-classic-brugge-de-panne/"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2017"), str_replace(url, "/uae-tour/2017", "/abu-dhabi-tour/2017"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2018"), str_replace(url, "/uae-tour/2018", "/abu-dhabi-tour/2018"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2016"), str_replace(url, "/uae-tour/2016", "/abu-dhabi-tour/2016"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2015"), str_replace(url, "/uae-tour/2015", "/abu-dhabi-tour/2015"), url)) %>%
              
              group_by(url) %>%
              mutate(min = min(as.numeric(str_replace(stage, "b", "")))) %>%
              ungroup() %>%
              
              mutate(stage = ifelse(min == 0, as.character(as.numeric(stage) + 1), stage)), by = c("stage", "url")) %>%
  
  group_by(stage, race, year, url) %>%
  mutate(contender_time = ifelse(pred > 0, total_seconds_back, NA),
         contender_time = min(contender_time, na.rm = T),
         seconds_behind_contenders = total_seconds_back - contender_time) %>%
  ungroup() %>%
  
  mutate(GC_rem = (GC_all*(1-percent_through))) %>%
  unique() %>%
  
  left_join(dbGetQuery(con, "SELECT DISTINCT stage, date, url, rider, rnk
               FROM stage_data_perf
               WHERE one_day_race = 0") %>%
              
              mutate(url = ifelse(str_detect(url, "/binckbank-tour/"), str_replace(url, "/binckbank-tour/", "/benelux-tour/"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/tour-du-haut-var/"), str_replace(url, "/tour-du-haut-var/", "/tour-des-alpes-maritimes-et-du-var/"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/driedaagse-vd-panne/"), str_replace(url, "/driedaagse-vd-panne/", "/oxyclean-classic-brugge-de-panne/"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2017"), str_replace(url, "/uae-tour/2017", "/abu-dhabi-tour/2017"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2018"), str_replace(url, "/uae-tour/2018", "/abu-dhabi-tour/2018"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2016"), str_replace(url, "/uae-tour/2016", "/abu-dhabi-tour/2016"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2015"), str_replace(url, "/uae-tour/2015", "/abu-dhabi-tour/2015"), url)) %>%
              
              group_by(url) %>%
              mutate(min = min(as.numeric(str_replace(stage, "b", "")))) %>%
              ungroup() %>%
              
              mutate(stage = ifelse(min == 0, as.character(as.numeric(stage) + 1), stage)) %>%
              arrange(date) %>%
              
              inner_join(gc_stages_rating %>%
                           select(stage, url, GC_stage), by = c("stage", "url")) %>%
              
              group_by(rider, url) %>%
              mutate(log_rnk = cumsum(log(rnk) * GC_stage)/cumsum(GC_stage)) %>%
              ungroup() %>%
              mutate(stage = as.character(as.numeric(stage) + 1)) %>%
              
              select(stage, url, rider, log_rnk), by = c("stage", "url", "rider")) %>%
  
  mutate(log_rnk = ifelse(is.na(log_rnk), 4, log_rnk))

# no one has won GC since 2014 if they're more than 150 seconds per GC stage remaining behind
# best placed contender
# only Carapaz is 2019 Giro has won being more than 300 seconds behind *at any point in race*

library(xgboost)

#

distinct_races <- who_has_GC_chances %>%
  filter(!is.na(GC_rem)) %>%
  filter(!is.na(seconds_behind_contenders)) %>%
  filter(!seconds_behind_contenders == -Inf) %>%
  select(race, year, url, class) %>%
  unique()

for(u in 1:nrow(distinct_races)) {
  
  xgb.train <- xgb.DMatrix(
    
    data = as.matrix(who_has_GC_chances %>%
                       #anti_join(distinct_races[u,]) %>%
                       filter(year < 2022) %>%
                       filter(!is.na(GC_rem)) %>%
                       filter(!is.na(seconds_behind_contenders)) %>%
                       filter(!seconds_behind_contenders == -Inf) %>%
                       select(seconds_behind_contenders, percent_through, GC_rem, pred, pre_gc_rnk, log_rnk)),
    
    label = who_has_GC_chances %>%
      #anti_join(distinct_races[u,]) %>%
      filter(year < 2022) %>%
      filter(!is.na(GC_rem)) %>%
      filter(!is.na(seconds_behind_contenders)) %>%
      filter(!seconds_behind_contenders == -Inf) %>%
      select(win) %>%
      .[[1]]
    
  )
  
  # test
  
  xgb.test <- xgb.DMatrix(
    
    data = as.matrix(who_has_GC_chances %>%
                       #anti_join(distinct_races[u,]) %>%
                       filter(year >= 2022) %>%
                       filter(!is.na(GC_rem)) %>%
                       filter(!is.na(seconds_behind_contenders)) %>%
                       filter(!seconds_behind_contenders == -Inf) %>%
                       select(seconds_behind_contenders, percent_through, GC_rem, pred, pre_gc_rnk, log_rnk)),
    
    label = who_has_GC_chances %>%
      #anti_join(distinct_races[u,]) %>%
      filter(year >= 2022) %>%
      filter(!is.na(GC_rem)) %>%
      filter(!is.na(seconds_behind_contenders)) %>%
      filter(!seconds_behind_contenders == -Inf) %>%
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
                         nrounds = 1500,
                         monotone_constraints = c(-1,0,0,1,-1,-1),
                         early_stopping_rounds = 300,
                         watchlist = list(val1 = xgb.train,
                                          val2 = xgb.test),
                         verbose = 0)
  
  #
  #
  # xgb Importance
  
  xgb.importance(model = gbm_model)
  
  gbm_model$best_score
  
  #
  
  gbm_gcwins_predictions = cbind(
    
    model_pred = predict(gbm_model, 
                         as.matrix(who_has_GC_chances %>%
                                     #inner_join(distinct_races[u,]) %>%
                                     filter(!is.na(GC_rem)) %>%
                                     filter(!is.na(seconds_behind_contenders)) %>%
                                     filter(!seconds_behind_contenders == -Inf) %>%
                                     select(seconds_behind_contenders, percent_through, GC_rem, pred, pre_gc_rnk,
                                            log_rnk), reshape=T)),
    
    who_has_GC_chances %>%
      #inner_join(distinct_races[u,]) %>%
      filter(!is.na(GC_rem)) %>%
      filter(!is.na(seconds_behind_contenders)) %>%
      filter(!seconds_behind_contenders == -Inf)) %>%
    
    select(rider, stage, race, year, url, class, date, total_seconds_back, seconds_behind_contenders, pre_winprob = pred, 
           percent_through, final_gc_rnk = rnk, model_winprob = model_pred, log_rnk) %>%
    
    group_by(stage, race, year) %>%
    mutate(model_winprob = model_winprob / sum(model_winprob, na.rm = T)) %>%
    
    arrange(desc(model_winprob), seconds_behind_contenders) %>%
    
    mutate(virtual_gc = rank(url, ties.method = 'first')) %>%
    ungroup()
  
  #
  
  dbWriteTable(con, "stage_by_stage_gc_winprob", gbm_gcwins_predictions, overwrite = TRUE, row.names = FALSE)
  
  print(u)
  
}
  
#

#write_rds(gbm_model, "Stored models/gc_s_b_s_model.rds")

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


# Ongoing GC Win Prob -----------------------------------------------------

latest_rider_gc_rankings <- dbGetQuery(con, "SELECT r.date, rider, top7_wtd, top3_wtd, perc_max, races
             FROM rider_gc_rankings r
             JOIN (
             
              SELECT max(date) as date
              FROM rider_gc_rankings
             
             ) x ON r.date = x.date") %>%
  mutate(date = as.Date(date)) %>%
  mutate(top7_wtd = ifelse(is.na(top7_wtd), 0, top7_wtd)) %>%
  mutate(top3_wtd = ifelse(is.na(top3_wtd), 0, top3_wtd),
         top_wtd = (top3_wtd + top7_wtd)/2)

#
#
#

latest_stage_by_stage_gc <- dbGetQuery(con, "SELECT * FROM pcs_stage_by_stage_gc WHERE year > 2021") %>%

  mutate(race = tolower(race),
         stage = as.character(stage)) %>%
  
  select(-race, -year) %>%
  
  inner_join(
    dbGetQuery(con, paste0("SELECT DISTINCT url, stage, race, year 
                           FROM pcs_stage_data WHERE date > '", lubridate::today() - 25, "'")) %>%
      inner_join(
        dbGetQuery(con, paste0("SELECT url FROM pcs_all_races
                           WHERE Winner = '' AND Date > '", lubridate::today() - 25, "'")), 
        by = c("url")), 
    by = c("stage", "url")) %>%
    
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
  
  group_by(url) %>%
  filter(max(stage) == stage) %>%
  ungroup()

#
#
#

stage_characteristics <- dbGetQuery(con, "SELECT * 
                                    FROM pcs_stage_characteristics
                                    WHERE year > 2021") %>%
  
  mutate(time_trial_kms = ifelse(time_trial == 1, length, 0),
         uphill_finish = ifelse(stage_type %in% c("icon profile p3", "icon profile p5"), 1, 0)) %>%
  
  mutate(sq_pcd = ifelse(pred_climb_difficulty <= 0, 0, pred_climb_difficulty ^ 2),
         level = ifelse(class %in% c("UWT", "WT", "1.UWT", "2.UWT"), "WT",
                        ifelse(class %in% c("Olympics", "WC", 'CC', "NC"), "Championships",
                               ifelse(class %in% c("2.2U", "2.Ncup", "1.2U", "1.Ncup"), "U23", 
                                      ifelse(class == "JR", "JR", "Regular")))),
         cobbles = 0,
         actual_length = length,
         length = length - 200) %>%
  
  group_by(race, year, url) %>%
  mutate(finalGT = ifelse(stage == max(stage) & grand_tour == 1, 1, 0),
         stage_no = rank(stage, ties.method = "first"),
         perc_thru = stage_no / max(stage_no)) %>%
  ungroup() %>%
  
  mutate(predicted_bs = predict(read_rds("Stored models/bunchsprint-glm-mod.rds"), .),
         predicted_bs = ifelse(time_trial == 1, NA,
                               ifelse(str_detect(stage_name, "TTT"), NA, predicted_bs)),
         predicted_bs = exp(predicted_bs)/(1+exp(predicted_bs))) %>%
  
  mutate(GC_stage = ifelse(time_trial == 1 | str_detect(stage_name, "TTT"), 1, 1-predicted_bs),
         GC_stage = ifelse(is.na(GC_stage), 0, GC_stage)) %>%

  arrange(date, year, race, stage) %>%
  
  group_by(url) %>%
  mutate(GC_done = cumsum(GC_stage),
         GC_all = sum(GC_stage)) %>%
  ungroup() %>%
  
  mutate(GC_done = ifelse(is.na(GC_done), 0, GC_done),
         percent_through = (GC_done / GC_all))

#

race_characteristics <- stage_characteristics %>%
  
  anti_join(latest_stage_by_stage_gc %>%
              select(url, stage) %>%
              unique()) %>%
  
  group_by(race = str_to_lower(race), year, url) %>%
  summarize(time_trial_kms = sum(time_trial_kms),
            total_TT = sum(time_trial == 1),
            avg_pcd = mean(pred_climb_difficulty, na.rm = T),
            total_stages = n()) %>%
  ungroup()

#
#
#

who_is_still_in_race <- latest_stage_by_stage_gc %>%
  
  left_join(latest_rider_gc_rankings %>%
              select(rider, top_wtd), by = c("rider")) %>%
  
  mutate(top_wtd = ifelse(is.na(top_wtd), 0, top_wtd)) %>%
  
  group_by(race, url, year) %>%
  mutate(gc_rnk = rank(desc(top_wtd), ties.method = "min")) %>%
  ungroup() %>%
  
  group_by(race, url, team, year) %>%
  mutate(tm_rnk = rank(desc(top_wtd), ties.method = "min")) %>%
  ungroup() %>%
  
  inner_join(race_characteristics, by = c("race", "year", "url")) %>%
  
  mutate(pred = predict(read_rds("Stored models/very-basic-gc-preds-model.rds"), .),
         pred = exp(pred)/(1+exp(pred))) %>%
  
  group_by(race, url, year) %>%
  mutate(pred = pred / sum(pred)) %>%
  ungroup() %>%
  
  mutate(pred = ifelse(pred < 0.005, 0, pred)) %>%
  
  group_by(race, url, year) %>%
  mutate(pred = pred / sum(pred)) %>%
  ungroup() %>%
  
  group_by(race, year, url) %>%
  mutate(contender_time = ifelse(pred > 0, total_seconds_back, NA),
         contender_time = min(contender_time, na.rm = T),
         seconds_behind_contenders = total_seconds_back - contender_time) %>%
  ungroup() %>%
  
  select(-hours, -minutes, -seconds, -old_gc_time, -gcn, -duplic, -h)

#

final_df_to_predict_from <- who_is_still_in_race %>%
  
  rename(pre_gc_rnk = gc_rnk) %>%
  
  inner_join(stage_characteristics %>%
               mutate(race = str_to_lower(race)) %>%
               mutate(GC_rem = GC_all - GC_done) %>%
               select(race, year, url, stage, class, 
                      percent_through, GC_rem), by = c("race", "year", "url", "stage")) 

#

gbm_live_predictions <- cbind(
  
  model_pred = predict(read_rds("Stored models/gc_s_b_s_model.rds"), 
                       as.matrix(final_df_to_predict_from %>%
                                   select(seconds_behind_contenders, percent_through, GC_rem, pred, pre_gc_rnk), reshape=T)),
  
  final_df_to_predict_from) %>%
  
  select(rider, stage, race, year, url, class, date, total_seconds_back, seconds_behind_contenders, pre_winprob = pred, 
         percent_through, model_winprob = model_pred) %>%
  
  group_by(stage, race, year) %>%
  mutate(model_winprob = model_winprob / sum(model_winprob, na.rm = T)) %>%
  ungroup()


# Future GC Win Prob -----------------------------------------------------

library(rvest)

latest_rider_gc_rankings <- dbGetQuery(con, "SELECT r.D, rider, top7_wtd, top3_wtd, perc_max, races
             FROM rider_gc_rankings r
             JOIN (
             
              SELECT max(D) as D
              FROM rider_gc_rankings
             
             ) x ON r.D = x.D") %>%
  mutate(D = as.Date(D)) %>%
  mutate(top7_wtd = ifelse(is.na(top7_wtd), 0, top7_wtd)) %>%
  mutate(top3_wtd = ifelse(is.na(top3_wtd), 0, top3_wtd),
         top_wtd = (top3_wtd + top7_wtd)/2)

#
#
#

future_gc_races <- dbGetQuery(con, paste0("SELECT url, Class, Race as race, year FROM pcs_all_races
                           WHERE Winner = '' AND Date > '", lubridate::today(), "'")) %>%
  filter(str_sub(Class, 1, 1) == "2")

#

startlist_df <- vector("list", nrow(future_gc_races))

for(s in 1:nrow(future_gc_races)) {
  
  URL <- paste0('https://www.procyclingstats.com/', future_gc_races$url[[s]], '/startlist')
  
  page <- URL %>%
    read_html() 
  
  #
  
  startlist <- page %>%
    html_nodes('ul.startlist_v3') %>%
    html_nodes('a')
  
  riders <- startlist %>%
    html_nodes('span') %>%
    html_text() %>%
    enframe(name = NULL) %>%
    mutate(value = iconv(value, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
    mutate(value = str_to_title(tolower(value)))
  
  teams <- cbind(
    startlist %>%
      html_text() %>%
      enframe(name = NULL),
    startlist %>%
      html_attr(name = "href") %>%
      enframe(name = NULL) %>%
      rename(url = value)) %>%
    filter(value != "") %>%
    mutate(team_2nd = ifelse(str_detect(url, "team"), value, NA)) %>%
    fill(team_2nd, .direction = "down") %>%
    mutate(value = iconv(value, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
    mutate(value = str_to_title(tolower(value))) %>%
    select(-url) %>%
    rename(team = team_2nd)
  
  riders <- cbind(riders %>% rename(rider = value)) %>%
    inner_join(teams, by = c("rider" = "value"))
  
  if(nrow(riders) > 93) {
  
  startlist_df[[s]] <- riders %>%
    mutate(url = future_gc_races$url[[s]])
  
  }
  
}

#

pre_event_startlist <- bind_rows(startlist_df) %>%
  inner_join(future_gc_races, by = c("url"))

#
#
#

stage_characteristics <- dbGetQuery(con, "SELECT * 
                                    FROM pcs_stage_characteristics
                                    WHERE year > 2021") %>%
  
  mutate(time_trial_kms = ifelse(time_trial == 1, length, 0),
         uphill_finish = ifelse(stage_type %in% c("icon profile p3", "icon profile p5"), 1, 0)) %>%
  
  mutate(sq_pcd = ifelse(pred_climb_difficulty <= 0, 0, pred_climb_difficulty ^ 2),
         level = ifelse(class %in% c("UWT", "WT", "1.UWT", "2.UWT"), "WT",
                        ifelse(class %in% c("Olympics", "WC", 'CC', "NC"), "Championships",
                               ifelse(class %in% c("2.2U", "2.Ncup", "1.2U", "1.Ncup"), "U23", 
                                      ifelse(class == "JR", "JR", "Regular")))),
         cobbles = 0,
         actual_length = length,
         length = length - 200) %>%
  
  group_by(race, year, url) %>%
  mutate(finalGT = ifelse(stage == max(stage) & grand_tour == 1, 1, 0),
         stage_no = rank(stage, ties.method = "first"),
         perc_thru = stage_no / max(stage_no)) %>%
  ungroup() %>%
  
  mutate(predicted_bs = predict(read_rds("Stored models/bunchsprint-glm-mod.rds"), .),
         predicted_bs = ifelse(time_trial == 1, NA,
                               ifelse(str_detect(stage_name, "TTT"), NA, predicted_bs)),
         predicted_bs = exp(predicted_bs)/(1+exp(predicted_bs))) %>%
  
  mutate(GC_stage = ifelse(time_trial == 1 | str_detect(stage_name, "TTT"), 1, 1-predicted_bs),
         GC_stage = ifelse(is.na(GC_stage), 0, GC_stage)) %>%
  
  arrange(date, year, race, stage) %>%
  
  group_by(year, race) %>%
  mutate(GC_done = cumsum(GC_stage),
         GC_all = sum(GC_stage)) %>%
  ungroup() %>%
  
  mutate(GC_done = ifelse(is.na(GC_done), 0, GC_done),
         percent_through = (GC_done / GC_all))

#

race_characteristics <- stage_characteristics %>%
  
  group_by(race = str_to_lower(race), year, url) %>%
  summarize(time_trial_kms = sum(time_trial_kms),
            total_TT = sum(time_trial == 1),
            avg_pcd = mean(pred_climb_difficulty, na.rm = T),
            total_stages = n()) %>%
  ungroup()

#
#
#

who_is_still_in_race <- pre_event_startlist %>%
  
  mutate(race = str_to_lower(race)) %>%
  
  mutate(total_seconds_back = 0) %>%
  
  left_join(latest_rider_gc_rankings %>%
              select(rider, top_wtd), by = c("rider")) %>%
  
  mutate(top_wtd = ifelse(is.na(top_wtd), 0, top_wtd)) %>%
  
  group_by(race, url, year) %>%
  mutate(gc_rnk = rank(desc(top_wtd), ties.method = "min")) %>%
  ungroup() %>%
  
  group_by(race, url, team, year) %>%
  mutate(tm_rnk = rank(desc(top_wtd), ties.method = "min")) %>%
  ungroup() %>%
  
  inner_join(race_characteristics, by = c("race", "year", "url")) %>%
  
  mutate(pred = predict(read_rds("Stored models/very-basic-gc-preds-model.rds"), .),
         pred = exp(pred)/(1+exp(pred))) %>%
  
  group_by(race, url, year) %>%
  mutate(pred = pred / sum(pred)) %>%
  ungroup() %>%
  
  mutate(pred = ifelse(pred < 0.005, 0, pred)) %>%
  
  group_by(race, url, year) %>%
  mutate(pred = pred / sum(pred)) %>%
  ungroup() %>%
  
  group_by(race, year, url) %>%
  mutate(contender_time = ifelse(pred > 0, total_seconds_back, NA),
         contender_time = min(contender_time, na.rm = T),
         seconds_behind_contenders = total_seconds_back - contender_time) %>%
  ungroup()

#

final_df_to_predict_from <- who_is_still_in_race %>%
  
  rename(pre_gc_rnk = gc_rnk) %>%
  
  inner_join(stage_characteristics %>%
               
               group_by(race, year, url) %>%
               filter(as.numeric(stage) == min(as.numeric(stage))) %>%
               ungroup() %>%
               
               mutate(race = str_to_lower(race)) %>%
               mutate(GC_rem = GC_all,
                      percent_through = 0,
                      stage = 0) %>%
               select(race, year, url, stage, class, 
                      percent_through, GC_rem) %>%
               unique(), by = c("race", "year", "url")) 

#

gbm_future_predictions <- cbind(
  
  model_pred = predict(read_rds("Stored models/gc_s_b_s_model.rds"), 
                       as.matrix(final_df_to_predict_from %>%
                                   select(seconds_behind_contenders, percent_through, GC_rem, pred, pre_gc_rnk), reshape=T)),
  
  final_df_to_predict_from) %>%
  
  select(rider, stage, race, year, url, class, total_seconds_back, seconds_behind_contenders, pre_winprob = pred, 
         percent_through, model_winprob = model_pred, pre_gc_rnk) %>%
  
  group_by(stage, race, year) %>%
  mutate(model_winprob = model_winprob / sum(model_winprob, na.rm = T)) %>%
  ungroup()
