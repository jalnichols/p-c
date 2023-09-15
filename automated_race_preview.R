library(tidyverse)
library(rvest)
library(DBI)

options(tidyverse.quiet = TRUE)

dbDisconnect(con)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      port = 5432,
                      host = 'localhost',
                      dbname = "cycling",
                      user = "postgres",
                      password = "braves")

# Link with PCS -----------------------------------------------------------

all_stages_monthly <- dbGetQuery(con, "SELECT DISTINCT * FROM new_fr_stages") %>%
  
  mutate(start_date = ifelse(race_url == "https://www.la-flamme-rouge.eu/maps/races/view/2022/3", as.Date("2022-08-19"), as.Date(start_date)),
         start_date = as.Date(start_date, origin = "1970-01-01"),
         start_date = ifelse(start_date < as.Date('1900-01-01'), as.Date(paste0(lubridate::year(date), "-", str_sub(start_date, 6, 10))), start_date),
         start_date = as.Date(start_date, origin = "1970-01-01")) %>%
  
  arrange(date)

#

todays_races <- all_stages_monthly %>%
  
  filter(date == lubridate::today()+6)

write_csv(x = todays_races,
          file = paste0("FR-routes/fr-schedule-", lubridate::today()+6, ".csv"))

interesting_races_today <- read_csv(paste0("FR-routes/fr-schedule-", lubridate::today()+6, ".csv")) %>%
  mutate(id = str_replace(url, "https://www.la-flamme-rouge.eu/maps/loadtrack/", "")) %>%
  
  mutate(race = str_trim(str_replace(race, "\\(([^\\)]+)\\)", "")),
         stage = as.character(stage))

PCS_races_today <- dbGetQuery(con, sprintf("SELECT * FROM pcs_stage_characteristics WHERE year = '%s'", lubridate::year(lubridate::today()))) %>%
  
  filter(lubridate::today()+6 == date)

races_which_match <- interesting_races_today %>%
  select(-c(stages, start_date, end_date, race_url, class)) %>%
  mutate(match_race = str_to_lower(race)) %>%
  inner_join(PCS_races_today %>%
               mutate(race = case_when(race == "Trofeo Andratx - Mirador D'es Colomer (Pollenca)" ~ "Trofeo Andratx - Mirador des Colomer",
                                       race == "World Championships ME - Road Race" ~ "UCI Road World Championships",
                                       TRUE ~ race)) %>%
               select(pcs_race = race, pcs_url = url, pcs_stage = stage) %>%
               mutate(match_race = str_to_lower(pcs_race)), by = c("match_race" = "match_race")) %>%
  inner_join(PCS_races_today %>%
               mutate(race = case_when(race == "Trofeo Andratx - Mirador D'es Colomer (Pollenca)" ~ "Trofeo Andratx - Mirador des Colomer",
                                       race == "World Championships ME - Road Race" ~ "UCI Road World Championships",
                                       TRUE ~ race)) %>%
               select(pcs_race = race, pcs_url = url, pcs_stage = stage,
                      stage_type, parcours_value, time_trial, grand_tour,
                      one_day_race, pred_climb_difficulty, class))

#
#

# Select Just One Race ----------------------------------------------------

race_features <- races_which_match %>%
  inner_join(dbGetQuery(con, sprintf("SELECT * FROM new_fr_stage_characteristics WHERE activity_id IN (%s)",
                                     toString(paste0("'", races_which_match$id, "'")))) %>%
               unique() %>%
               spread(stat, value) %>%
               select(-length), by = c("id" = "activity_id")) %>%
  
  .[1,]


# Bring in Startlist ------------------------------------------------------

race_url <- race_features$pcs_url[[1]]
choose_stage = race_features$pcs_stage[[1]]

URL <- paste0('https://www.procyclingstats.com/', str_replace(race_url,"/gc",""), '/startlist')

#

page <- URL %>%
  read_html() 

startlist <- page %>%
  html_nodes('ul.startlist_v4') %>%
  html_nodes('a')

riders <- startlist %>%
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
  filter(!str_detect(url, "staff")) %>%
  mutate(team_2nd = ifelse(str_detect(url, "team"), value, NA)) %>%
  fill(team_2nd, .direction = "down") %>%
  mutate(value = iconv(value, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
  mutate(value = str_to_title(tolower(value))) %>%
  select(-url) %>%
  rename(team = team_2nd)

riders <- cbind(riders %>% rename(rider = value)) %>%
  filter(!str_detect(rider, "(Wt)")) %>%
  filter(!str_detect(rider, "(Prt)")) %>%
  inner_join(teams, by = c("rider" = "value")) %>%
  filter(!str_detect(rider, "Statistics")) %>%
  filter(!(str_to_lower(rider) == str_to_lower(team))) %>%
  group_by(team) %>%
  filter(rank(team, ties.method = "first") <= 9) %>%
  ungroup()

#

# Predict Bunch Sprint ---------------------------------------------------


# BS <- predict(read_rds("Stored models/bunchsprint-glm-mod.rds"), 
#               tibble(pred_climb_difficulty = race_features$pred_climb_difficulty,
#                      grand_tour = race_features$grand_tour,
#                      length = race_features$length - 200,
#                      cobbles = 0,
#                      one_day_race = race_features$one_day_race,
#                      perc_thru = 0.5,
#                      sq_pcd = race_features$pred_climb_difficulty ^ 2
#                      
#               ) %>%
#                 mutate(level = ifelse(grand_tour == 1 | race_features$class %in% c("1.UWT", "2.UWT"), "WT", "Regular")) %>%
#                 mutate(finalGT = ifelse(race_features$pcs_stage == 21 & grand_tour == 1, 1, 0),
#                        uphill_finish = ifelse(race_features$stage_type %in% c("icon profile p3", "icon profile p5"),1,0)))

BS <- predict(read_rds("Stored models/bunchsprint-xgboost-model.rds"),
              tibble(perc_thru = ifelse(race_features$grand_tour == 1, as.numeric(race_features$stage)/21, 0.5),
                     one_day_race = race_features$one_day_race,
                     sq_pcd = race_features$pred_climb_difficulty ^ 2,
                     pred_climb_difficulty = race_features$pred_climb_difficulty,
                     final_20km_vert_gain = race_features$final_20km_vert_gain,
                     avg_alt = race_features$avg_alt,
                     final_1km_gradient = race_features$final_1km_gradient,
                     length = race_features$length - 200,
                     uphill_finish = ifelse(race_features$stage_type %in% c("icon profile p3", "icon profile p5"),1,0),
                     finalGT = ifelse(race_features$pcs_stage == 21 & race_features$grand_tour == 1, 1, 0),
                     Championships = ifelse(race_features$class %in% c("WC", "NC", "CC"), 1, 0),
                     Regular = ifelse(race_features$class %in% c("1.1", "2.1", "1.Pro", "2.Pro", "1.2", "2.2"), 1, 0),
                     U23 = ifelse(race_features$class %in% c("1.Ncup", "2.Ncup", "1.2U", "2.2U"),1,0),
                     WT = ifelse(race_features$class %in% c("1.UWT", "2.UWT"), 1, 0),
                     cobbles = 0) %>%
                as.matrix())

glmBS <- predict(read_rds("Stored models/bunchsprint-glm-mod.rds"),
              tibble(perc_thru = ifelse(race_features$grand_tour == 1, as.numeric(race_features$stage)/21, 0.5),
                     one_day_race = race_features$one_day_race,
                     sq_pcd = race_features$pred_climb_difficulty ^ 2,
                     pred_climb_difficulty = race_features$pred_climb_difficulty,
                     final_20km_vert_gain = race_features$final_20km_vert_gain,
                     avg_alt = race_features$avg_alt,
                     final_1km_gradient = race_features$final_1km_gradient,
                     length = race_features$length - 200,
                     grand_tour = race_features$grand_tour,
                     level = ifelse(race_features$class %in% c("UWT", "WT", "1.UWT", "2.UWT"), "WT",
                                    ifelse(race_features$class %in% c("Olympics", "WC", 'CC', "NC"), "Championships",
                                           ifelse(race_features$class %in% c("2.2U", "2.Ncup", "1.2U", "1.Ncup"), "U23", 
                                                  ifelse(race_features$class == "JR", "JR", "Regular")))),
                     uphill_finish = ifelse(race_features$stage_type %in% c("icon profile p3", "icon profile p5"),1,0),
                     finalGT = ifelse(race_features$pcs_stage == 21 & race_features$grand_tour == 1, 1, 0),
                     Championships = ifelse(race_features$class %in% c("WC", "NC", "CC"), 1, 0),
                     Regular = ifelse(race_features$class %in% c("1.1", "2.1", "1.Pro", "2.Pro", "1.2", "2.2"), 1, 0),
                     U23 = ifelse(race_features$class %in% c("1.Ncup", "2.Ncup", "1.2U", "2.2U"),1,0),
                     WT = ifelse(race_features$class %in% c("1.UWT", "2.UWT"), 1, 0),
                     cobbles = 0))

glmBS = exp(glmBS)/(1+exp(glmBS))

# Predict Breakaway -------------------------------------------------------

prob_Break <- predict(read_rds("Stored models/breakaway-prediction-model.rds"), 
                      as.matrix(tibble(predicted_bs = (BS+glmBS)/2,
                                       grand_tour = race_features$grand_tour,
                                       pred_climb_difficulty = race_features$pred_climb_difficulty,
                                       length = race_features$length,
                                       uphill_finish = ifelse(race_features$stage_type %in% c("icon profile p3", "icon profile p5"),1,0),
                                       perc_thru = ifelse(race_features$grand_tour == 1, as.numeric(race_features$stage)/21,
                                                          ifelse(race_features$one_day_race == 1, 0, 0.5)),
                                       total_vert_gain = race_features$total_vert_gain,
                                       one_day_race = race_features$one_day_race)))

# Predict Efforts Required ------------------------------------------------

# I need a good sof_logrk estimate

vars <- c("climb_1", "BS_pred", "weighted_vg", "length", "climb_2", "climb_3",
          "sof_logrk", "one_day_race", "uphill_finish", "grand_tour",
          "total_vert_gain", "over_6_percent", "weighted_altitude",
          "drafting_bene", "weight_adv", "abs_climb_1", "final_1km_gradient", 
          "final_5km_vertgain", "final_20km_vert_gain", "first_30km_vert_gain",
          "U23", "WT")

#

stage_climbs <- dbGetQuery(con, sprintf("SELECT id, perc_thru, model_category, power_model_category, length as climb_length, gradient
               FROM climbs_from_new_fr_telemetry
               WHERE id IN (%s)", toString(paste0("'", race_features$id, "'")))) %>%
  group_by(id) %>%
  mutate(GiniClimbs = DescTools::Gini(power_model_category),
         CountClimbs = n(),
         SigClimbs = sum(power_model_category >= 1)) %>%
  ungroup()

#

stage_climbs <- stage_climbs %>%
  
  rbind(tibble(id = c(race_features$id,race_features$id,race_features$id),
               perc_thru = 0,
               model_category = 0.1,
               power_model_category = 0.1,
               climb_length = 0.1,
               gradient = 0.01,
               GiniClimbs = 0,
               CountClimbs = 0,
               SigClimbs = 0)) %>%
  
  group_by(id) %>%
  mutate(rk = rank(desc(power_model_category), ties.method = "first")) %>%
  filter(rk <= 3) %>%
  mutate(rk = paste0("climb_", rk)) %>%
  ungroup() %>%
  
  select(id, GiniClimbs, CountClimbs, SigClimbs, rk, power_model_category) %>%
  
  spread(rk, power_model_category, fill = 0) %>%
  
  inner_join(
    stage_climbs %>%
      rbind(tibble(id = c(race_features$id,race_features$id,race_features$id),
                   perc_thru = 0,
                   model_category = 0.1,
                   power_model_category = 0.1,
                   climb_length = 0.1,
                   gradient = 0.01,
                   GiniClimbs = 0,
                   CountClimbs = 0,
                   SigClimbs = 0)) %>%
      group_by(id) %>%
      filter(max(model_category) == model_category) %>%
      ungroup() %>%
      select(id, abs_climb_1 = model_category) %>%
      unique())
#

predictor_df_for_efforts <- race_features %>%
  
  left_join(stage_climbs, by = c("id")) %>%
  
  mutate(climb_1 = ifelse(is.na(climb_1), 0, climb_1),
         climb_2 = ifelse(is.na(climb_2), 0, climb_2),
         climb_3 = ifelse(is.na(climb_3), 0, climb_3),
         abs_climb_1 = ifelse(is.na(abs_climb_1), 0, abs_climb_1)) %>%
  
  mutate(level = ifelse(class %in% c("UWT", "WT", "1.UWT", "2.UWT", 'WC'), "WT",
                        ifelse(class %in% c("Olympics", 'CC', "NC"), "Championships",
                               ifelse(class %in% c("2.2U", "2.Ncup", "1.2U", "1.Ncup"), "U23",
                                      ifelse(class %in% c("NAT"), "Nat'l Races",
                                             ifelse(class == "JR", "JR", "Regular")))))) %>%
  
  mutate(sof_logrk = ifelse(level %in% c("WT"), 1.0,
                            ifelse(level == "U23", -0.25,
                                   ifelse(level == "Regular", 0.25, -0.35)))) %>%
  
  mutate(BS_pred = (BS+glmBS)/2,
         U23 = ifelse(level == "U23", 1, 0),
         WT = ifelse(level == "WT", 1, 0),
         uphill_finish = ifelse(race_features$stage_type %in% c("icon profile p3", "icon profile p5"),1,0)) %>%
  
  select(vars) %>%
  
  as.matrix()

#

rel5min_mod <- read_rds("Stored models/5min_rel-prediction-model.rds")
abs5min_mod <- read_rds("Stored models/5min_abs-prediction-model.rds")
rel40min_mod <- read_rds("Stored models/40min_rel-prediction-model.rds")

#

predicted_efforts <- tibble(rel5min_pred_effort = predict(rel5min_mod, predictor_df_for_efforts),
                            abs5min_pred_effort = predict(abs5min_mod, predictor_df_for_efforts),
                            rel40min_pred_effort = predict(rel40min_mod, predictor_df_for_efforts))

#

race_features <- race_features %>%
  cbind(predicted_efforts)

#

most_recent_models1 <- dbGetQuery(con, "SELECT rider, random_intercept, bunchsprint_impact, impact_rel_5, impact_rel_40, impact_abs_5, notes
             FROM lme4_rider_logranks_new r
             WHERE date IN (
              SELECT max(CAST(date AS DATE)) as date
              FROM lme4_rider_logranks_new r
              WHERE notes = 'unweighted w/o long abs SD'
             )") %>%
  mutate(Type = 'Efforts') %>%
  filter(notes == 'unweighted w/o long abs SD')


# Predict LogRanks --------------------------------------------------------

most_recent_models <- dbGetQuery(con, "SELECT rider, test_or_prod, random_intercept, sqpcd_impact, r.Date, bunchsprint_impact, one_day_race as one_day_race_impact
             FROM lme4_rider_logranks_sq r
             JOIN (
               SELECT max(Date) as Date
               FROM lme4_rider_logranks_sq r
               WHERE test_or_prod = 'BS_not_ODR'
             ) x ON r.Date = x.Date") %>%
  mutate(Type = case_when(test_or_prod == 'BS_not_ODR' ~ "LogRanks",
                          test_or_prod == 'ltweights_BS_not_ODR' ~ "lt_LogRanks",
                          test_or_prod == 'weights_BS_not_ODR' ~ "st_LogRanks",
                          test_or_prod == 'BS_not_ODR_100_racedays' ~ "last100_LogRanks",
                          TRUE ~ NA)) %>%
  
  filter(Type == "last100_LogRanks")

#

models <- full_join(
  
  riders %>%
    
    left_join(
      
      most_recent_models, by = c("rider")
      
    ) %>%
    
    mutate(syn = 1) %>%
    inner_join(race_features %>%
                 mutate(syn = 1), by = c("syn")) %>%
    
    mutate(sqpcd = ifelse(pred_climb_difficulty <= 0, 0, pred_climb_difficulty ^ 2)) %>%
    mutate(coef = random_intercept + (sqpcd_impact * sqpcd) + (bunchsprint_impact * BS) + (one_day_race_impact * (one_day_race-BS)),
           BS_coef = random_intercept + (sqpcd_impact * sqpcd) + (bunchsprint_impact * 1) + (one_day_race_impact * (0)),
           notBS_coef = random_intercept + (sqpcd_impact * sqpcd) + (bunchsprint_impact * 0) + (one_day_race_impact * one_day_race)) %>%
    
    mutate(xRank = exp(coef + mean(log(seq(1,nrow(riders),1)))),
           xRank_BS = exp(BS_coef + mean(log(seq(1,nrow(riders),1)))),
           xRank_notBS = exp(notBS_coef + mean(log(seq(1,nrow(riders),1))))) %>%
    
    group_by(Type, stage) %>%
    mutate(xRank = xRank / mean(xRank, na.rm = T) * exp(mean(log(seq(1,nrow(riders),1)))),
           xRank_BS = xRank_BS / mean(xRank_BS, na.rm = T) * exp(mean(log(seq(1,nrow(riders),1)))),
           xRank_notBS = xRank_notBS / mean(xRank_notBS, na.rm = T) * exp(mean(log(seq(1,nrow(riders),1))))) %>%
    ungroup() %>%
    
    select(rider, stage, team, Type, LogRanks = xRank, BSValue = xRank_BS, notBSValue = xRank_notBS),
  
  riders %>%
    
    left_join(
      
      most_recent_models1, by = c("rider")
      
    ) %>%
    
    mutate(syn = 1) %>%
    inner_join(race_features %>%
                 mutate(syn = 1), by = c("syn")) %>%
    
    mutate(coef = random_intercept + (impact_abs_5 * abs5min_pred_effort) + 
             (bunchsprint_impact * BS) + (impact_rel_5 * rel5min_pred_effort) +
             (impact_rel_40 * rel40min_pred_effort),
           BS_coef = random_intercept + (impact_abs_5 * abs5min_pred_effort) + 
             (bunchsprint_impact * 1) + (impact_rel_5 * rel5min_pred_effort) +
             (impact_rel_40 * rel40min_pred_effort),
           notBS_coef = random_intercept + (impact_abs_5 * abs5min_pred_effort) + 
             (bunchsprint_impact * 0) + (impact_rel_5 * rel5min_pred_effort) +
             (impact_rel_40 * rel40min_pred_effort)) %>%
    
    mutate(xRank = ifelse(Type == "Efforts", exp(coef + mean(log(seq(1,nrow(riders),1)))), as.numeric(NA)),
           xRank_BS = ifelse(Type == "Efforts", exp(BS_coef + mean(log(seq(1,nrow(riders),1)))), as.numeric(NA)),
           xRank_notBS = ifelse(Type == "Efforts", exp(notBS_coef + mean(log(seq(1,nrow(riders),1)))), as.numeric(NA))) %>%
    
    group_by(Type, stage) %>%
    mutate(xRank = xRank / mean(xRank, na.rm = T) * exp(mean(log(seq(1,nrow(riders),1)))),
           xRank_BS = xRank_BS / mean(xRank_BS, na.rm = T) * exp(mean(log(seq(1,nrow(riders),1)))),
           xRank_notBS = xRank_notBS / mean(xRank_notBS, na.rm = T) * exp(mean(log(seq(1,nrow(riders),1))))) %>%
    ungroup()  %>%
    
    select(rider, stage, team, Efforts = xRank, BSEfforts = xRank_BS, notBSEfforts = xRank_notBS)) %>%
  
  mutate(Combined = (LogRanks + Efforts)/2,
         CombinedBS = (BSValue + BSEfforts)/2,
         CombinednotBS = (notBSValue + notBSEfforts)/2) %>%
  
  inner_join(
    dbGetQuery(con, "SELECT rider, within_tm
             FROM last_two_years_position_in_team r
             JOIN (
             
             SELECT max(Date) as Date
             FROM last_two_years_position_in_team
             
             ) x ON r.Date = x.Date") %>%
      unique(), by = c("rider"), multiple = 'all') %>%
  
  group_by(team, Type) %>%
  mutate(within_tm_today = log(Combined) - ((sum(log(Combined), na.rm = T) - log(Combined)) / (n() - 1))) %>%
  ungroup() %>%
  
  mutate(vsnormal_within_tm = within_tm_today - within_tm) %>%
  
  filter(!is.na(Type))

# Predict Win Prob --------------------------------------------------------

predicted_win <- cbind(
  
  pred = predict(read_rds("Stored models/very-basic-logrank-xgboost-win.rds"), 
                 as.matrix(models %>% 
                             rename(pred_rank = Combined) %>%
                             mutate(predicted_bs = BS,
                                    pred_climb_difficulty = race_features$pred_climb_difficulty,
                                    sof = 0.5,
                                    one_day_race = race_features$one_day_race,
                                    grand_tour = race_features$grand_tour,
                                    uphill_finish = ifelse(race_features$stage_type %in% c("icon profile p3", "icon profile p5"),1,0)) %>%
                             
                             mutate(pred_break = prob_Break) %>%
                             
                             select(pred_rank,
                                    predicted_bs,
                                    pred_climb_difficulty,
                                    sof,
                                    pred_break,
                                    one_day_race,
                                    grand_tour,
                                    uphill_finish))
  ),
  
  cbind(models,
        tibble(pred_break = prob_Break))) %>%
  
  group_by(team) %>%
  mutate(rk = rank(desc(pred), ties.method = "first"),
         riders = n()) %>%
  ungroup() %>%
  
  mutate(pred = ifelse(rk > floor(riders/2), pred^(rk-(floor(riders/2)-1)), pred)) %>%
  
  mutate(decimal_odds = 1/pred)

#

res4 <- implied::implied_probabilities(predicted_win$decimal_odds, method = 'power')

#

predicted_win <- predicted_win %>%
  
  cbind(marginless_prob = res4$probabilities[1,]) %>%

  group_by(Type) %>%
  mutate(implied_ML = round((1 - marginless_prob)/(marginless_prob)*100,-2)) %>%
  ungroup()

#

predicted_win_BS <- cbind(
  
  pred = predict(read_rds("Stored models/very-basic-logrank-xgboost-win.rds"), 
                 as.matrix(models %>% 
                             rename(pred_rank = CombinedBS) %>%
                             mutate(predicted_bs = 1,
                                    pred_climb_difficulty = race_features$pred_climb_difficulty,
                                    sof = 0.5,
                                    one_day_race = race_features$one_day_race,
                                    grand_tour = race_features$grand_tour,
                                    uphill_finish = ifelse(race_features$stage_type %in% c("icon profile p3", "icon profile p5"),1,0)) %>%
                             
                             mutate(pred_break = prob_Break) %>%
                             
                             select(pred_rank,
                                    predicted_bs,
                                    pred_climb_difficulty,
                                    sof,
                                    pred_break,
                                    one_day_race,
                                    grand_tour,
                                    uphill_finish))
  ),
  
  cbind(models,
        tibble(pred_break = prob_Break))) %>%
  
  mutate(pred = pred/sum(pred, na.rm = T),
         implied_ML = round((1 - pred)/(pred)*100,-2))

predicted_win_NOT_BS <- cbind(
  
  pred = predict(read_rds("Stored models/very-basic-logrank-xgboost-win.rds"), 
                 as.matrix(models %>% 
                             rename(pred_rank = CombinednotBS) %>%
                             mutate(predicted_bs = 0,
                                    pred_climb_difficulty = race_features$pred_climb_difficulty,
                                    sof = 0.5,
                                    one_day_race = race_features$one_day_race,
                                    grand_tour = race_features$grand_tour,
                                    uphill_finish = ifelse(race_features$stage_type %in% c("icon profile p3", "icon profile p5"),1,0)) %>%
                             
                             mutate(pred_break = prob_Break) %>%
                             
                             select(pred_rank,
                                    predicted_bs,
                                    pred_climb_difficulty,
                                    sof,
                                    pred_break,
                                    one_day_race,
                                    grand_tour,
                                    uphill_finish))
  ),
  
  cbind(models,
        tibble(pred_break = prob_Break))) %>%
  
  mutate(pred = pred/sum(pred, na.rm = T),
         implied_ML = round((1 - pred)/(pred)*100,-2))

#
#
#

# Bring in Winning Efforts from Past Races --------------------------------

winning_efforts_prior_races <- dbGetQuery(con, sprintf("SELECT * FROM win_efforts_by_race WHERE url LIKE (%s)",
                                                       paste0("'%", str_sub(race_features$pcs_url, 1, nchar(race_features$pcs_url)-4), "%'"))) %>%
  filter(year >= 2018)

#

ggplot(winning_efforts_prior_races %>%
         select(rolling_speed, stage, url, class, date, sdrel, sdabs, relative, absolute) %>%
         gather(power_type, value, sdrel:absolute) %>%
         mutate(power_type = case_when(power_type %in% c("absolute", "relative") ~ power_type,
                                       power_type == "sdabs" ~ "Z-score Absolute",
                                       power_type == "sdrel" ~ "Z-score Relative",
                                       TRUE ~ power_type)),
       aes(x = rolling_speed/60, y = value))+
  geom_hline(data = tibble(power_type = c("absolute", "relative", "Z-score Absolute", "Z-score Relative"),
                           hline = c(NA, NA, 0, 0)),
             aes(yintercept = hline))+
  geom_point(size=3)+
  geom_smooth(se=F, color = "red")+
  
  geom_blank(data = tibble(power_type = c("Z-score Absolute", "Z-score Relative",
                                   "Z-score Absolute", "Z-score Relative"),
                    rolling_speed = 300,
                    value = c(-2.1,-2.1,2.1,2.1)),
             aes(x = rolling_speed/60, y = value))+
  
  scale_x_log10(breaks = winning_efforts_prior_races$rolling_speed / 60 %>% unique())+
  facet_wrap(~power_type, scales = "free_y")+
  labs(title = paste0("Required to win efforts ", str_to_title(str_replace(str_replace(str_replace(winning_efforts_prior_races$url, "race/", ""), "/", " "), "-", " "))),
       x = "Length of effort",
       y = "Power output",
       subtitle = "based on top finishers power outputs")

ggsave(paste0("reqd-efforts-", race_features$race, ".png"), 
       height = 7, width = 7)

#
#
# Where were peak efforts -------------------------------------------------

dbGetQuery(con, sprintf("SELECT * FROM peak_power_locations WHERE url LIKE (%s) AND rolling_speed IN (30,120,300,600,1200,2400)",
                        paste0("'%", str_sub(race_features$pcs_url, 1, nchar(race_features$pcs_url)-4), "%'"))) %>% 
  
  group_by(rolling_speed, finish_group) %>%
  summarize(q20 = mean(q20),
            q80 = mean(q80),
            distance_gone = mean(distance_gone),
            avgrnk = mean(avgrnk)) %>%
  ungroup() %>%
  
  ggplot(aes(x = rolling_speed/60, y = distance_gone))+
  facet_wrap(~finish_group)+
  geom_point(size=3)+
  geom_errorbar(aes(ymin = q20, ymax = q80))+
  #geom_blank(aes(x = 5, y = length))+
  scale_x_log10(breaks = c(0.5,2,5,10,20,40))+
  facet_wrap(~reorder(finish_group, avgrnk))+
  labs(x = "Effort length (mins)",
       y = "Distance thru race (KMs)",
       title = paste0("Where were peak efforts at ", str_to_title(str_replace(str_replace(str_replace(winning_efforts_prior_races$url, "race/", ""), "/", " "), "-", " ")), "?"))

ggsave(paste0("peak-effort-location-", race_features$race, ".png"), 
       height = 7, width = 7)

#
#
# Generate Race Characteristics Table -------------------------------------

sd_rel_5 = 0.376
sd_abs_5 = 21.9
m_rel_5 = 5.93
m_abs_5 = 408
sd_rel_40 = 0.399
sd_abs_40 = 23.7
m_rel_40 = 4.33
m_abs_40 = 297

library(gt)

if(race_features$one_day_race == 1) {} else {
  race_features$race <- paste0("Stage ", race_features$stage, " ", race_features$race)
}

# this is broken if less than 3 climbs

features_tab <- race_features %>%
  
  mutate(pred_break_win = paste0(round(prob_Break*100,0),"%"),
         pred_BS_win = paste0(round(BS*100,0),"%")) %>%
  
  cbind(stage_climbs %>%
          mutate(uphill_finish = ifelse(race_features$stage_type %in% c("icon profile p3", "icon profile p5"),1,0)) %>%
          select(climb_1, climb_2, climb_3, abs_climb_1, SigClimbs, uphill_finish)) %>%
  
  mutate(pred_climb_difficulty = case_when(abs_climb_1 >= 5 & pred_climb_difficulty <= 2.5 ~ "hilly",
                                           pred_climb_difficulty <= 2.5 ~ "flat",
                                           pred_climb_difficulty <= 7.5 ~ "hilly",
                                           pred_climb_difficulty <= 12 ~ "medium mountains",
                                           TRUE ~ "high mountains"),
         climb_1 = case_when(abs_climb_1 <= 1 ~ "Cat 4",
                             abs_climb_1 <= 2.5 ~ "Cat 3",
                             abs_climb_1 <= 5 ~ "Cat 2",
                             abs_climb_1 <= 13 ~ "Cat 1",
                             abs_climb_1 < 0 ~ "None",
                             TRUE ~ "HC"),
         climb_2 = case_when(climb_2 <= 1 ~ "Cat 4",
                             climb_2 <= 2.5 ~ "Cat 3",
                             climb_2 <= 5 ~ "Cat 2",
                             climb_2 <= 13 ~ "Cat 1",
                             climb_2 < 0 ~ "None",
                             TRUE ~ "HC"),
         climb_3 = case_when(climb_3 <= 1 ~ "Cat 4",
                             climb_3 <= 2.5 ~ "Cat 3",
                             climb_3 <= 5 ~ "Cat 2",
                             climb_3 <= 13 ~ "Cat 1",
                             climb_3 < 0 ~ "None",
                             TRUE ~ "HC")) %>%
  
  select(length, BreakawayWin = pred_break_win, BunchSprintWin = pred_BS_win, pred_climb_difficulty, uphill_finish, total_vert_gain, 
         rel5min_pred_effort, abs5min_pred_effort, rel40min_pred_effort,
         climb_1, climb_2, climb_3, SignificantClimbs = SigClimbs) %>%
  
  mutate(rel5min_pred_effort = round(m_rel_5 + (rel5min_pred_effort * sd_rel_5),1),
         abs5min_pred_effort = round(m_abs_5 + (abs5min_pred_effort * sd_abs_5),-1),
         rel40min_pred_effort = round(m_rel_40 + (rel40min_pred_effort * sd_rel_40),1),
         total_vert_gain = round(total_vert_gain, -2),
         length = round(length,1),
         uphill_finish = ifelse(uphill_finish == 1, "true", "false")) %>%
  
  rename(hardest_climb = climb_1, `2nd hardest climb` = climb_2, `3rd hardest climb` = climb_3,
         climbing_difficulty = pred_climb_difficulty, `5 min Relative Effort` = rel5min_pred_effort,
         `5 min Absolute Effort` = abs5min_pred_effort, `40 min Relative Effort` = rel40min_pred_effort) %>%
  
  gather(metric, value) %>%
  
  mutate(value = case_when(metric == "length" ~ paste0(value, "km"),
                           metric == "total_vert_gain" ~ paste0(value,"m"),
                           metric %in% c("5 min Relative Effort", "40 min Relative Effort") ~ paste0(value, " w/kg"),
                           metric == "5 min Absolute Effort" ~ paste0(value, " watts"),
                           TRUE ~ value)) %>%
  
  gt() %>%
  tab_header(
    title = paste0(race_features$race, " - ", race_features$class),
    subtitle = paste0("from ", race_features$depart, " to ", race_features$arrive)
  ) %>%
  tab_options(column_labels.hidden = TRUE)

gtsave(data = features_tab, paste0("race-features-", race_features$race, ".png"), expand = 20)

#
#
#

# Generate Most likely winners table -------------------------------------

favorites_tab <- predicted_win %>%
  
  select(rider, team, WinProbability = marginless_prob, PredictedRank = Combined, ImpliedML = implied_ML) %>%
  
  arrange(desc(WinProbability), PredictedRank) %>%
  
  .[1:20,] %>%
  
  mutate(WinProbability = paste0(round(WinProbability*100,1),"%"),
         PredictedRank = round(PredictedRank,1),
         ImpliedML = paste0("+", ImpliedML)) %>%
  
  gt() %>%
  tab_header(
    title = paste0("Favorites for ", race_features$race, " - ", race_features$class),
    subtitle = paste0(round(race_features$length,1), " km from ", race_features$depart, " to ", race_features$arrive)
  )

gtsave(data = favorites_tab, paste0("race-favorites-", race_features$race,".png"), expand = 20)

#
#
#
#
#

# How Predictable was this race -------------------------------------------

race_errors <- dbGetQuery(con, sprintf("SELECT * FROM race_errors_normal_logrank_model WHERE url_race LIKE (%s)",
                                       paste0("'%", str_sub(race_features$pcs_url, 6, nchar(race_features$pcs_url)-5), "%'")))


# Race Intensity ----------------------------------------------------------

Agg_Intensity <- dbGetQuery(con, "SELECT * FROM intensity_of_efforts") %>% 
  filter(rnk <= 25) %>% 
  
  group_by(stage, race, year, effort) %>%
  summarize(intensity = mean(intensity, na.rm = T), 
            norm_power = mean(norm_power / weight, na.rm = T), 
            total_distance = median(total_distance, na.rm = T),
            samples = n()) %>% 
  ungroup() %>%
  
  filter((total_distance > 75000 & effort == "full race") |
           (total_distance > 18750 & effort != "full race"))


# Rider History ----------------------------------------------------------

rider_history <- dbGetQuery(con, sprintf("SELECT * FROM pcs_stage_data WHERE url LIKE (%s)",
                                       paste0("'%", str_sub(race_features$pcs_url, 6, nchar(race_features$pcs_url)-5), "%'"))) %>%
  filter(str_sub(url, 1, nchar(url)-4) == str_replace(race_features$pcs_url[[1]], as.character(lubridate::year(lubridate::today())), "")) %>%
  filter(rider %in% riders$rider) %>%
  select(rnk, rider, url, team, year) %>%
  
  group_by(url, year) %>%
  mutate(dnf = ifelse(is.na(rnk), 1, 0),
         rnk = ifelse(dnf == 1, max(rnk, na.rm = T)+1, rnk)) %>%
  ungroup() %>%
  
  group_by(rider) %>%
  summarize(best = min(rnk, na.rm = T),
            dnf = sum(dnf, na.rm = T),
            median = exp(median(log(rnk), na.rm = T)),
            win = sum(rnk == 1, na.rm = T),
            podium = sum(rnk <= 3, na.rm = T),
            top10 = sum(rnk <= 10, na.rm = T),
            first_year = min(year, na.rm = T),
            last_year = max(year, na.rm = T),
            races = n()) %>%
  ungroup()