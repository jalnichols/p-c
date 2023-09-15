
library(tidyverse)
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

race_url <- "race/tour-down-under/2023/gc"
choose_stage = 3

URL <- paste0('https://www.procyclingstats.com/', str_replace(race_url,"/gc",""), '/startlist')

#

stage_chars <- dbGetQuery(con, sprintf("SELECT * FROM pcs_stage_characteristics WHERE url = '%s'", str_replace(race_url,"/gc",""))) %>%
  unique() %>%
  arrange(stage) %>%
  mutate(last_stage = max(as.numeric(stage))) %>%
  filter(stage_number == choose_stage)

KM <- stage_chars$distance[[1]]
ST <- stage_chars$stage_type[[1]]
STAGE = as.numeric(stage_chars$stage_number[[1]])
PV = stage_chars$pv[[1]]
GT = stage_chars$grand_tour[[1]]
level = stage_chars$class[[1]]
PCD <- stage_chars$pred_climb_difficulty[[1]]
ODR <- stage_chars$one_day_race[[1]]
TVG = 1800
SOF = 0.3
LAST_STAGE = as.numeric(stage_chars$last_stage)

BS <- predict(read_rds("Stored models/bunchsprint-glm-mod.rds"), 
              tibble(pred_climb_difficulty = PCD,
                     grand_tour = GT,
                     length = KM - 200,
                     cobbles = 1,
                     one_day_race = ODR,
                     perc_thru = STAGE / LAST_STAGE,
                     sq_pcd = PCD ^ 2
                     
              ) %>%
                mutate(level = ifelse(grand_tour == 1 | level %in% c("1.UWT", "2.UWT"), "WT", "Regular")) %>%
                mutate(finalGT = ifelse(STAGE == 21 & GT == 1, 1, 0),
                       uphill_finish = ifelse(ST %in% c("icon profile p3", "icon profile p5"),1,0)))

BS <- exp(BS)/(1+exp(BS))

#

extract_startlist <- function(URL) {
  
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
    inner_join(teams, by = c("rider" = "value")) %>%
    mutate(rider = ifelse(rider == "Girmay Biniam", "Girmay Hailu Biniam", rider)) %>%
    left_join(
      
      dbGetQuery(con, sprintf("SELECT * FROM pcs_stage_by_stage_gc
               WHERE url = '%s'", str_replace(race_url, "/gc", ""))) %>%
        filter(stage == max(stage)) %>%
        unique() %>%
        mutate(rider = str_to_title(rider)) %>%
        select(-team), by = c("rider"))
}

#

riders <- extract_startlist(URL)

most_recent_models <- rbind(
  
  dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact
             FROM lme4_rider_logranks_sq r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_logranks_sq r
             WHERE test_or_prod = 'prod'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'") %>% mutate(Type = 'LogRanks') #,
  # 
  # dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact
  #            FROM lme4_rider_teamleader r
  #            JOIN (
  #            
  #            SELECT max(Date) as Date
  #            FROM lme4_rider_teamleader r
  #            WHERE test_or_prod = 'prod'
  #            
  #            ) x ON r.Date = x.Date
  #            WHERE test_or_prod = 'prod'") %>% mutate(Type = 'Leader'),
  # 
  # dbGetQuery(con, "SELECT r.*
  #            FROM lme4_rider_pcsgamepicks r
  #            JOIN (
  #            
  #            SELECT max(Date) as Date
  #            FROM lme4_rider_pcsgamepicks
  #            
  #            ) x ON r.Date = x.Date") %>% 
    # janitor::clean_names() %>%
    # rename(rider = rowname,
    #        random_intercept = intercept,
    #        pcd_impact = pred_climb_difficulty,
    #        bunchsprint_impact = bunch_sprint,
    #        Date = date) %>% 
    # mutate(Type = 'GamePicks')
  )

#
#
#

models <- riders %>%
  
  left_join(
    
    most_recent_models, by = c("rider")
    
  ) %>%
  mutate(sqpcd = ifelse(PCD <= 0, 0, PCD ^2)) %>%
  mutate(coef = ifelse(Type == "GamePicks", random_intercept + (sqpcd_impact * PCD) + (bunchsprint_impact * BS) -0.03,
                       ifelse(Type == "PointsWhenOpp", random_intercept + (sqpcd_impact * PCD) + (bunchsprint_impact * BS) + 0.012,
                              ifelse(Type == "LogRanks",  random_intercept + (sqpcd_impact * sqpcd) + (bunchsprint_impact * BS),
                                     ifelse(Type == "TimeLost", random_intercept + (sqpcd_impact * PCD) + (bunchsprint_impact * BS),
                                            ifelse(Type == "Leader", exp(random_intercept + (sqpcd_impact * PCD) + (bunchsprint_impact * BS) - 2),
                                                   ifelse(Type == "Wins", exp(random_intercept + (sqpcd_impact * PCD) + (bunchsprint_impact * BS) - 6.5), 
                                                          ifelse(Type == "WhenOpp", exp(random_intercept + (sqpcd_impact * PCD) + (bunchsprint_impact * BS) - 2), NA))))))),
         coef = ifelse(Type %in% c("Success", "Leader", "WhenOpp"), coef / (1+coef), coef)) %>%
  
  mutate(coef = ifelse(is.na(coef),
                       ifelse(Type %in% c("GamePicks", "PointsWhenOpp"), 0,
                              ifelse(Type %in% c("LogRanks", "Wins"), min(coef, na.rm = T),
                                     ifelse(Type == "Leader", 0.02,
                                            ifelse(Type %in% c("Success","WhenOpp"), 0.01,
                                                   ifelse(Type == "TimeLost", max(coef, na.rm = T), NA))))), coef)) %>%
  group_by(team, Type) %>%
  mutate(coef = ifelse(Type == "Leader", ifelse(coef <= 0.1, 0, coef), coef)) %>%
  ungroup() %>%
  
  mutate(adj = ifelse(Type == "Wins", coef / sum(coef), coef)) %>%
  
  group_by(team, Type) %>%
  mutate(adj = ifelse(Type %in% c("Leader"), coef / sum(coef), coef)) %>%
  ungroup() %>%
  
  mutate(xRank = ifelse(Type == "LogRanks", exp(coef + mean(log(seq(1,nrow(riders),1)))), as.numeric(NA))) %>%
  
  group_by(Type) %>%
  mutate(xRank = xRank / mean(xRank, na.rm = T) * exp(mean(log(seq(1,nrow(riders),1))))) %>%
  ungroup() %>%
  
  mutate(Value = ifelse(Type == "LogRanks", xRank, adj)) %>%
  
  select(rider, team, Type, Value, gc_rnk, gc_time) %>%
  filter(!is.na(Type)) %>%
  spread(Type, Value)

#
#
#

predicted_win <- cbind(
  
  pred = predict(read_rds("Stored models/very-basic-logrank-xgboost-win.rds"), 
                 as.matrix(models %>% 
                             rename(pred_rank = LogRanks) %>%
                             mutate(predicted_bs = BS,
                                    pred_climb_difficulty = PCD,
                                    sof = SOF,
                                    one_day_race = ODR,
                                    grand_tour = GT,
                                    uphill_finish = ifelse(ST %in% c("icon profile p3", "icon profile p5"),1,0)) %>%
                             
                             mutate(pred_break = predict(read_rds("Stored models/breakaway-prediction-model.rds"), 
                                                         as.matrix(tibble(predicted_bs = BS,
                                                                            grand_tour = GT,
                                                                            pred_climb_difficulty = PCD,
                                                                            length = KM,
                                                                            uphill_finish = ifelse(ST %in% c("icon profile p3", "icon profile p5"),1,0),
                                                                            perc_thru = STAGE / LAST_STAGE,
                                                                            total_vert_gain = TVG)))) %>%
                             
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
        tibble(pred_break = predict(read_rds("Stored models/breakaway-prediction-model.rds"), 
                                    as.matrix(tibble(predicted_bs = BS,
                                                     grand_tour = GT,
                                                     pred_climb_difficulty = PCD,
                                                     length = KM,
                                                     uphill_finish = ifelse(ST %in% c("icon profile p3", "icon profile p5"),1,0),
                                                     perc_thru = STAGE / LAST_STAGE,
                                                     total_vert_gain = TVG)))))) %>%
  
  #filter(!is.na(gc_rnk)) %>%
  
  #mutate(GC_helper = ifelse(team %in% c("UAE Team Emirates", "INEOS Grenadiers", "Jumbo-Visma", 
  #                                      "BORA - hansgrohe", "Movistar Team", "Groupama - FDJ"), 
  #                          ifelse(!rider %in% c("Yates Adam", "Thomas Geraint", "Vingegaard Jonas",
  #                                               "Roglic Primoz", "Vlasov Aleksandr", "Pogacar Tadej", 
  #                                               "Mas Enric", "Gaudu David", "Van Aert Wout"), 1, 0), 0)) %>% 
  #mutate(pred = ifelse(GC_helper == 1, pred*0.25, pred)) %>%
  
  mutate(pred = pred/sum(pred, na.rm = T),
         implied_ML = round((1 - pred)/(pred)*100,-2)) 

#
#
#

entire_race <- dbGetQuery(con, sprintf("SELECT * FROM pcs_stage_characteristics WHERE url = '%s'", race_url)) %>%
    unique() %>%
    arrange(as.numeric(stage)) %>%
    rename() %>%
    mutate(last_stage = max(as.numeric(stage)),
           length = length - 200,
           sq_pcd = pred_climb_difficulty^2,
           perc_thru = as.numeric(stage)/last_stage,
           level = ifelse(class %in% c("WC", "2.UWT", "1.UWT"), "WT", "Regular"),
           finalGT = ifelse(grand_tour == 1 & stage == "21", 1, 0),
           cobbles = ifelse(race == "Tour de France" & year == 2022 & stage == 5, 1, 0),
           uphill_finish = ifelse(stage_type %in% c("icon profile p3", "icon profile p5"), 1, 0)) %>%
    mutate(predicted_bs = predict(read_rds("Stored models/bunchsprint-glm-mod.rds"), .),
           predicted_bs = exp(predicted_bs)/(1+exp(predicted_bs)),
           predicted_bs = ifelse(time_trial == 1, 0, predicted_bs),
           length = length+200) %>%
  mutate(uphill_finish = ifelse(stage == "14", 1,
                                ifelse(stage == "19", 0, uphill_finish))) %>%
  cbind(total_vert_gain = c(0,1150,1300,1800,600,2500,2500,2550,3750,2700,
                            4100,4700,2100,3400,2400,3400,3350,4000,1300,400,750))

entire_race = cbind(
  
  pred_breakaway = predict(read_rds("Stored models/breakaway-prediction-model.rds"),
                           as.matrix(entire_race %>%
                                       select(predicted_bs,
                                              grand_tour,
                                              pred_climb_difficulty,
                                              length,
                                              uphill_finish,
                                              perc_thru,
                                              total_vert_gain))),
  
  entire_race) %>%
  
  mutate(pred_breakaway = ifelse(time_trial == 1, 0, pred_breakaway))
    
    

#
#
#

models %>% 
  # penalize riders close on GC (within 4ish minutes)
  mutate(LogRanks = ifelse(as.numeric(gc_rnk) <= 27, LogRanks + 15, LogRanks),
  # penalize riders with GC leaders on their team
         LogRanks = ifelse(team %in% c("INEOS Grenadiers", "Bahrain - Victorious", 
                                       "Team BikeExchange - Jayco", "BORA - hansgrohe", 
                                       "Trek - Segafredo"), LogRanks + 10, LogRanks)) %>%
  # use dumb formula to generate probabilities of winning
  mutate(x = (1 / LogRanks)^2,
         y = x / sum(x, na.rm = T),
         y = ifelse(y < 0.003, 0, y),
         y = y / sum(y, na.rm = T)) %>% arrange(desc(y)) -> preds

#
#
#

xpath = '/html/body/div[1]/div[1]/div[8]/div[1]/div[2]/div[1]/ul/li[1]/div[2]/table'

how_good_is_breakaway <- paste0('https://www.procyclingstats.com/', str_replace(race_url,"/gc",""), "/stage-", STAGE, "/live/livestats") %>%
  
  read_html() %>%
  html_nodes(xpath = xpath) %>%
  html_table() %>%
  .[[1]] %>%
  rename(rider = X4) %>%
  select(rider) %>%
  mutate(rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT"),
         rider = str_to_title(rider))

#

BREAKAWAY <- riders %>%
  
  left_join(
    
    most_recent_models %>% filter(Type == "LogRanks"), by = c("rider")
    
  ) %>%
  left_join(how_good_is_breakaway %>% mutate(breakaway = 1)) %>%
  
  select(rider, team, gc_rnk, gc_time, breakaway, random_intercept, pcd_impact, bunchsprint_impact) %>%
  
  mutate(predicted_rank = exp(mean(log(seq(1,nrow(riders),1))) + random_intercept + (pcd_impact * PCD) + (bunchsprint_impact * BS))) %>%
  
  arrange(breakaway) %>%
  
  mutate(predicted_rank = predicted_rank / sum(predicted_rank) * n())

#
#
#

skill_logranks <- dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact, one_day_race
             FROM lme4_rider_logranks r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_logranks r
             WHERE test_or_prod = 'prod'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'") %>%
  mutate(rider = ifelse(rider == "O Connor Ben", "O'connor Ben", rider)) %>%
  mutate(sprints = exp(random_intercept + 4.3 + bunchsprint_impact),
         mountains = exp(random_intercept + 4.3 + (20 * pcd_impact)),
         classics = exp(random_intercept + 4.3 + (4.5 * pcd_impact) + one_day_race))

#

sprint_2ndlevel <- dbGetQuery(con, "SELECT rider, random_intercept, r.date
             FROM lme4_rider_sprintlevel2_logranks r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_sprintlevel2_logranks
             
             ) x ON r.Date = x.Date") %>%
  mutate(rider = ifelse(rider == "O Connor Ben", "O'connor Ben", rider)) %>%
  mutate(sprints = exp(random_intercept + 2.5))

#

skill_logranks <- skill_logranks %>%
  left_join(sprint_2ndlevel, by = c("rider")) %>%
  mutate(sprints = ifelse(is.na(sprints.y), sprints.x, sprints.y)) %>%
  
  select(rider, sprints, mountains, classics)

#

time_trials <- dbGetQuery(con, "SELECT rider, random_intercept+modelintercept+(15*tvg_impact) as random_intercept
             FROM lme4_rider_timetrial r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_timetrial r
             WHERE test_or_prod = 'prod'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'") %>%
  
  mutate(rider = ifelse(rider == "O Connor Ben", "O'connor Ben", rider)) %>%
  mutate(time_trials = exp(random_intercept))

#

breakaways <- dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact, model_intercept
             FROM lme4_rider_breakaway r
             JOIN (
             
             SELECT DISTINCT max(Date) as Date
             FROM lme4_rider_breakaway r
             
             ) x ON r.Date = x.Date") %>%
  mutate(rider = ifelse(rider == "Oconnor Ben", "O'connor Ben", rider)) %>%
  mutate(mtn_break = random_intercept + model_intercept + (20 * pcd_impact),
         doomed_break = random_intercept + model_intercept + (2 * pcd_impact) + bunchsprint_impact,
         hilly_break = random_intercept + model_intercept + (5.5 * pcd_impact),
         mtn_break = exp(mtn_break)/(1+exp(mtn_break)),
         doomed_break = exp(doomed_break)/(1+exp(doomed_break)),
         hilly_break = exp(hilly_break)/(1+exp(hilly_break)))

#

TEAM = c('UAE Team Emirates', "Jumbo-Visma", "BORA - hansgrohe")

input <- riders %>%
  #filter(team %in% TEAM) %>%
  left_join(skill_logranks, by = c("rider")) %>%
  left_join(time_trials %>% select(rider, time_trials), by = c("rider")) %>%
  left_join(breakaways %>% select(rider, doomed_break, hilly_break, mtn_break), by = c("rider")) %>%
  mutate(time_trials = ifelse(is.na(time_trials), 40, time_trials)) %>%
  left_join(dbGetQuery(con, "SELECT r.date, rider, top7_wtd, top3_wtd, perc_max, races 
                       FROM rider_gc_rankings g
                       JOIN (
             
                       SELECT DISTINCT max(date) as date
                       FROM rider_gc_rankings
                       
                       ) r ON r.date = g.date") %>%
              mutate(top_wtd = (top7_wtd + top3_wtd)/2), by = c("rider")) %>%
  select(rider, team, sprints, classics, mountains, time_trials, doomed_break, hilly_break, mtn_break, gc_rating = top_wtd) %>%
  mutate(sprints = ifelse(sprints > 150, 150, sprints),
         mountains = ifelse(mountains > 150, 150, mountains),
         classics = ifelse(classics > 150, 150, classics),
         time_trials = ifelse(time_trials > 150, 150, time_trials))

#
#
#

helpers <- input %>% 
  group_by(team) %>%
  mutate(mtn_team = rank(mountains, ties.method = "first"), 
         mtn_team = ifelse(mtn_team <= 5, mountains, NA)) %>%  
  mutate(cls_team = rank(classics, ties.method = "first"), 
         cls_team = ifelse(cls_team <= 5, classics, NA)) %>%
  mutate(brk_team = rank(desc(mtn_break), ties.method = "first"), 
         brk_team = ifelse(brk_team <= 3, mtn_break, NA)) %>%
  ungroup() %>% 
  
  group_by(team) %>% 
  summarize(mtn_team = mean(mtn_team, na.rm = T), 
            cls_team = mean(cls_team, na.rm = T),
            brk_team = mean(brk_team, na.rm = T),
            best_sprinter = min(sprints, na.rm = T),
            n = n()) %>%
  ungroup()

#

input %>%
  
  ggplot(aes(x = sprints, y = mountains, label = rider, color = time_trials < 10))+
  geom_point(size = 4)+
  ggrepel::geom_label_repel(color = "black")+
  scale_y_reverse()+
  scale_x_reverse()+
  geom_blank(data = expand_grid(sprints = c(0,80), mountains = c(0,80), rider = 'Blank', time_trials = c(0,80)))+
  scale_color_manual(values = c("black", "orange"),
                     name = 'Elite ITT')

input %>%
  
  ggplot(aes(x = mountains, y = gc_rating, label = rider, color = time_trials < 10))+
  geom_point(size = 4)+
  ggrepel::geom_label_repel(color = "black")+
  scale_x_reverse()+
  geom_blank(data = expand_grid(mountains = c(0,80), gc_rating = c(0,200), rider = 'Blank', time_trials = c(0,80)))+
  scale_color_manual(values = c("black", "orange"),
                     name = 'Elite ITT')

input %>%
  
  ggplot(aes(x = doomed_break, y = mtn_break, label = rider))+
  geom_point(size = 4)+
  ggrepel::geom_label_repel(color = "black", max.overlaps = 10)+
  geom_blank(data = expand_grid(sprints = c(0,80), mountains = c(0,80), rider = 'Blank', time_trials = c(0,80),
                                doomed_break = c(0,0.25), mtn_break = c(0,0.3)))+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Probability of breakaway in flat stage",
       y = "Probability of breakaway in mountain stage")

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

# 2018-07-07, 2019-07-05, 2020-08-28, 2021-06-26

DATE = '2020-08-28'
race_url = 'race/tour-de-france/2020'
URL <- paste0('https://www.procyclingstats.com/', race_url, '/startlist')


#

riders <- extract_startlist(URL)

skill_logranks <- dbGetQuery(con, sprintf("SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact, one_day_race
             FROM lme4_rider_logranks r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_logranks r
             WHERE test_or_prod = 'prod' AND date <= '%s'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'", DATE)) %>%
  mutate(rider = ifelse(rider == "O Connor Ben", "O'connor Ben", rider)) %>%
  mutate(sprints = exp(random_intercept + 4.3 + bunchsprint_impact),
         mountains = exp(random_intercept + 4.3 + (20 * pcd_impact)),
         classics = exp(random_intercept + 4.3 + (4.5 * pcd_impact) + one_day_race))

#

sprint_2ndlevel <- dbGetQuery(con, sprintf("SELECT rider, random_intercept, r.date
             FROM lme4_rider_sprintlevel2_logranks r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_sprintlevel2_logranks
             WHERE date <= '%s'
             
             ) x ON r.Date = x.Date", DATE)) %>%
  mutate(rider = ifelse(rider == "O Connor Ben", "O'connor Ben", rider)) %>%
  mutate(sprints = exp(random_intercept + 2.5))

#

skill_logranks <- skill_logranks %>%
  left_join(sprint_2ndlevel, by = c("rider")) %>%
  mutate(sprints = ifelse(is.na(sprints.y), sprints.x, sprints.y)) %>%
  
  select(rider, sprints, mountains, classics)

#

time_trials <- dbGetQuery(con, sprintf("SELECT rider, random_intercept+modelintercept+(15*tvg_impact) as random_intercept
             FROM lme4_rider_timetrial r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_timetrial r
             WHERE test_or_prod = 'prod' AND date <= '%s'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'", DATE)) %>%
  
  mutate(rider = ifelse(rider == "O Connor Ben", "O'connor Ben", rider)) %>%
  mutate(time_trials = exp(random_intercept))

#

breakaways <- dbGetQuery(con, sprintf("SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact, model_intercept
             FROM lme4_rider_breakaway r
             JOIN (
             
             SELECT DISTINCT max(Date) as Date
             FROM lme4_rider_breakaway r
             WHERE date <= '%s'
             ) x ON r.Date = x.Date", DATE)) %>%
  mutate(rider = ifelse(rider == "Oconnor Ben", "O'connor Ben", rider)) %>%
  mutate(mtn_break = random_intercept + model_intercept + (20 * pcd_impact),
         doomed_break = random_intercept + model_intercept + (2 * pcd_impact) + bunchsprint_impact,
         hilly_break = random_intercept + model_intercept + (5 * pcd_impact),
         mtn_break = exp(mtn_break)/(1+exp(mtn_break)),
         doomed_break = exp(doomed_break)/(1+exp(doomed_break)),
         hilly_break = exp(hilly_break)/(1+exp(hilly_break)))

#

input <- riders %>%
  #filter(team %in% TEAM) %>%
  left_join(skill_logranks, by = c("rider")) %>%
  left_join(time_trials %>% select(rider, time_trials), by = c("rider")) %>%
  #left_join(breakaways %>% select(rider, doomed_break, hilly_break, mtn_break), by = c("rider")) %>%
  mutate(time_trials = ifelse(is.na(time_trials), 40, time_trials)) %>%
  left_join(dbGetQuery(con, sprintf("SELECT r.date, rider, top7_wtd, top3_wtd, perc_max, races 
                       FROM rider_gc_rankings g
                       JOIN (
             
                       SELECT DISTINCT max(date) as date
                       FROM rider_gc_rankings
                       WHERE date <= '%s'
                       
                       ) r ON r.date = g.date", DATE)) %>%
              mutate(top_wtd = (top7_wtd + top3_wtd)/2), by = c("rider")) %>%
  select(rider, team, sprints, classics, mountains, time_trials, gc_rating = top_wtd) %>%
  mutate(sprints = ifelse(sprints > 150, 150, sprints),
         mountains = ifelse(mountains > 150, 150, mountains),
         classics = ifelse(classics > 150, 150, classics),
         time_trials = ifelse(time_trials > 150, 150, time_trials))

#
#
#

helpers <- input2021 %>%
  
  group_by(team) %>%
  mutate(mtn_team = rank(mountains, ties.method = "first"), 
         mtn_team = ifelse(mtn_team <= 5 & mtn_team > 1, mountains, NA)) %>%  
  mutate(cls_team = rank(classics, ties.method = "first"), 
         cls_team = ifelse(cls_team <= 5, classics, NA)) %>%
  mutate(tt_team = rank(time_trials, ties.method = "first"), 
         tt_team = ifelse(tt_team <= 5 & tt_team > 1, time_trials, NA)) %>%
  ungroup() %>% 
  
  group_by(team) %>% 
  summarize(mtn_team = mean(mtn_team, na.rm = T), 
            cls_team = mean(cls_team, na.rm = T),
            tt_team = mean(tt_team, na.rm = T),
            best_sprinter = min(sprints, na.rm = T),
            n = n()) %>%
  ungroup()
