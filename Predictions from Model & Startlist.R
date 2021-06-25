
library(tidyverse)
library(rvest)
library(DBI)

DBI::dbDisconnect(con)

con <- dbConnect(RMySQL::MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

URL <- 'https://www.procyclingstats.com/race/tour-de-france/2021/startlist/'

TVG <- 2800 #2800
KM <- 198 #198
UphillFinish = 1
ST <- "icon profile p3"
STAGE = 1
PV = 76 #75

PCD1 <- predict(read_rds("Stored models/pcd-pv-mod.rds"), tibble(pv = PV))
PCD2 <- predict(read_rds("Stored models/pcd-icon-mod.rds"), tibble(stage_type = ST, class_level = 4))
PCD3 <- predict(read_rds("Stored models/strava-elev-mod.rds"), tibble(tvg = TVG / KM, strava_elevation = TVG, stage_type = ST))

PCD = (PCD1 + PCD2 + PCD3) / 3

BS <- predict(read_rds("Stored models/bunchsprint-glm-mod.rds"), 
              tibble(pred_climb_difficulty = PCD,
                     finalGT = 0,
                     grand_tour = 1,
                     length = KM - 200,
                     cobbles = 0,
                     one_day_race = 0,
                     perc_thru = STAGE / 21,
                     uphill_finish = UphillFinish,
                     sq_pcd = PCD ^ 2
                     
                     ))

BS <- exp(BS)/(1+exp(BS))

#

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

#

most_recent_models <- rbind(
  
  dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact
             FROM lme4_rider_logranks r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_logranks r
             WHERE test_or_prod = 'prod'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'") %>% mutate(Type = 'LogRanks'),
  
  dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact
             FROM lme4_rider_timelost r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_logranks r
             WHERE test_or_prod = 'prod'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'") %>% mutate(Type = 'TimeLost'),
  
  dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact
             FROM lme4_rider_pointswhenopp r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_pointswhenopp r
             WHERE test_or_prod = 'prod'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'") %>% mutate(Type = 'PointsWhenOpp'),
  
  dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact
             FROM lme4_rider_points r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_points r
             WHERE test_or_prod = 'prod'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'") %>% mutate(Type = 'Points'),
  
  dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact
             FROM lme4_rider_wins r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_wins r
             WHERE test_or_prod = 'prod'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'") %>% mutate(Type = 'Wins'),
  
  dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact
             FROM lme4_rider_teamleader r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_teamleader r
             WHERE test_or_prod = 'prod'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'") %>% mutate(Type = 'Leader')
  
)

#
#
#

models <- riders %>%
  
  left_join(
    
    most_recent_models, by = c("rider")
    
  ) %>%
  
  mutate(coef = ifelse(Type == "Points", random_intercept + (pcd_impact * PCD) + (bunchsprint_impact * BS) + 0.012,
                       ifelse(Type == "PointsWhenOpp", random_intercept + (pcd_impact * PCD) + (bunchsprint_impact * BS) + 0.012,
                              ifelse(Type == "LogRanks",  random_intercept + (pcd_impact * PCD) + (bunchsprint_impact * BS),
                                     ifelse(Type == "TimeLost", random_intercept + (pcd_impact * PCD) + (bunchsprint_impact * BS),
                                            ifelse(Type == "Leader", exp(random_intercept + (pcd_impact * PCD) + (bunchsprint_impact * BS) - 2),
                                                   ifelse(Type == "Wins", exp(random_intercept + (pcd_impact * PCD) + (bunchsprint_impact * BS) - 6.5), 
                                                          ifelse(Type == "WhenOpp", exp(random_intercept + (pcd_impact * PCD) + (bunchsprint_impact * BS) - 2), NA))))))),
         coef = ifelse(Type %in% c("Success", "Leader", "WhenOpp"), coef / (1+coef), coef)) %>%
  
  mutate(coef = ifelse(is.na(coef),
                       ifelse(Type %in% c("Points", "PointsWhenOpp"), 0.01,
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
  ungroup()

#
#
#

wide_models <- models %>%
  filter(!is.na(adj)) %>%
  select(rider, team, Type, adj) %>%
  spread(Type, adj) %>%

  mutate(rankPoints = rank(desc(Points), ties.method = "min"),
         rankPointsWhenOpp = rank(desc(PointsWhenOpp), ties.method = "min"),
         rankLog = rank(desc(LogRanks), ties.method = "min"),
         rankTimeLost = rank(TimeLost, ties.method = "min"),
         rankWins = rank(desc(Wins), ties.method = "min")) %>%

  mutate(rankOverall = 4 / ((1/rankPoints)+(1/rankLog)+(1/rankPointsWhenOpp)+(1/rankWins)))

#

ggplot(wide_models, 
       aes(x = sqrt(rankTimeLost), y = sqrt(rankPointsWhenOpp), label = rider))+
  geom_abline(slope=1, intercept=0, color = 'pink', size=2)+
  geom_text(alpha=0.5)+
  scale_x_reverse()+
  scale_y_reverse()

#
#
#

all_models <- riders %>%
  
  group_by(team) %>%
  filter(rank(team, ties.method = "first") <= 8) %>%
  ungroup() %>%
  
  left_join(
    
    most_recent_models, by = c("rider")
    
  ) %>%
  
  mutate(climb_coef = ifelse(Type == "Points", random_intercept + (pcd_impact * 12) + (bunchsprint_impact * 0.01) + 0.012,
                       ifelse(Type == "PointsWhenOpp", random_intercept + (pcd_impact * 12) + (bunchsprint_impact * 0.01) + 0.012,
                              ifelse(Type == "LogRanks",  random_intercept + (pcd_impact * 12) + (bunchsprint_impact * 0.01),
                                     ifelse(Type == "TimeLost", random_intercept + (pcd_impact * 12) + (bunchsprint_impact * 0.01),
                                            ifelse(Type == "Leader", exp(random_intercept + (pcd_impact * 12) + (bunchsprint_impact * 0.01) - 2), 
                                                   ifelse(Type == "WhenOpp", exp(random_intercept + (pcd_impact * 12) + (bunchsprint_impact * 0.01) - 2), NA)))))),
         climb_coef = ifelse(Type %in% c("Success", "Leader", "WhenOpp"), climb_coef / (1+climb_coef), climb_coef)) %>%
  
  mutate(climb_coef = ifelse(is.na(climb_coef),
                       ifelse(Type %in% c("Points", "PointsWhenOpp"), 0.01,
                              ifelse(Type == "LogRanks", min(climb_coef, na.rm = T),
                                     ifelse(Type == "Leader", 0.02,
                                            ifelse(Type %in% c("Success","WhenOpp"), 0.01,
                                                   ifelse(Type == "TimeLost", max(climb_coef, na.rm = T), NA))))), climb_coef)) %>%
  
  mutate(sprint_coef = ifelse(Type == "Points", random_intercept + (pcd_impact * 1) + (bunchsprint_impact * 0.9) + 0.012,
                             ifelse(Type == "PointsWhenOpp", random_intercept + (pcd_impact * 1) + (bunchsprint_impact * 0.9) + 0.012,
                                    ifelse(Type == "LogRanks",  random_intercept + (pcd_impact * 1) + (bunchsprint_impact * 0.9),
                                           ifelse(Type == "TimeLost", random_intercept + (pcd_impact * 1) + (bunchsprint_impact * 0.9),
                                                  ifelse(Type == "Leader", exp(random_intercept + (pcd_impact * 1) + (bunchsprint_impact * 0.9) - 2), 
                                                         ifelse(Type == "WhenOpp", exp(random_intercept + (pcd_impact * 1) + (bunchsprint_impact * 0.9) - 2), NA)))))),
         sprint_coef = ifelse(Type %in% c("Success", "Leader", "WhenOpp"), sprint_coef / (1+sprint_coef), sprint_coef)) %>%
  
  mutate(sprint_coef = ifelse(is.na(sprint_coef),
                             ifelse(Type %in% c("Points", "PointsWhenOpp"), 0.01,
                                    ifelse(Type == "LogRanks", min(sprint_coef, na.rm = T),
                                           ifelse(Type == "Leader", 0.02,
                                                  ifelse(Type %in% c("Success","WhenOpp"), 0.01,
                                                         ifelse(Type == "TimeLost", max(sprint_coef, na.rm = T), NA))))), sprint_coef)) %>%
  
  mutate(hills_coef = ifelse(Type == "Points", random_intercept + (pcd_impact * 5) + (bunchsprint_impact * 0.25) + 0.012,
                              ifelse(Type == "PointsWhenOpp", random_intercept + (pcd_impact * 5) + (bunchsprint_impact * 0.25) + 0.012,
                                     ifelse(Type == "LogRanks",  random_intercept + (pcd_impact * 5) + (bunchsprint_impact * 0.5),
                                            ifelse(Type == "TimeLost", random_intercept + (pcd_impact * 5) + (bunchsprint_impact * 0.25),
                                                   ifelse(Type == "Leader", exp(random_intercept + (pcd_impact * 5) + (bunchsprint_impact * 0.25) - 2), 
                                                          ifelse(Type == "WhenOpp", exp(random_intercept + (pcd_impact * 5) + (bunchsprint_impact * 0.25) - 2), NA)))))),
         hills_coef = ifelse(Type %in% c("Success", "Leader", "WhenOpp"), hills_coef / (1+hills_coef), hills_coef)) %>%
  
  mutate(hills_coef = ifelse(is.na(hills_coef),
                              ifelse(Type %in% c("Points", "PointsWhenOpp"), 0.01,
                                     ifelse(Type == "LogRanks", min(hills_coef, na.rm = T),
                                            ifelse(Type == "Leader", 0.02,
                                                   ifelse(Type %in% c("Success","WhenOpp"), 0.01,
                                                          ifelse(Type == "TimeLost", max(hills_coef, na.rm = T), NA))))), hills_coef)) %>%
  
  select(rider, team, Type, hills_coef, sprint_coef, climb_coef) %>%
  
  group_by(Type) %>%
  mutate(hillsrank = rank(hills_coef, ties.method = "min"),
            sprintrank = rank(sprint_coef, ties.method = "min"),
            climbrank = rank(climb_coef, ties.method = "min")) %>%
  ungroup() %>%
  
  filter(Type %in% c("TimeLost"))

#
#
#

team_ranks <- all_models %>%
  
  select(team, hillsrank, climbrank, hills_coef, climb_coef) %>%
  gather(type, rank, -team) %>%
  
  group_by(team, type) %>%
  mutate(rk = rank(rank, ties.method = "min")) %>%
  ungroup() %>%
  
  mutate(top5 = ifelse(rk <= 5, rank, NA)) %>%
  
  group_by(team, type) %>%
  summarize(harm = sum(1 / rank, na.rm = T),
            best = min(rank, na.rm = T),
            median = median(rank, na.rm = T),
            mean = mean(rank, na.rm = T),
            top5 = mean(top5, na.rm = T),
            runners = n()) %>%
  ungroup() %>%
  
  mutate(harm = round(runners / (harm), 0)) %>%
  select(-runners) %>%
  
  mutate(stat = ifelse(str_detect(type, "coef"), round(top5 + mean,0)/2, harm)) %>%
  select(type, stat, team) %>%
  spread(type, stat)

#
#
#
#
#
#
#

recent_perf <- dbGetQuery(con, "SELECT rider, date, (act_leader - exp_leader) as rel_leader, 
    exp_leader, act_leader, exp_rank, act_rank, error_rank, races, days_of_comps
               FROM performance_last10races_vsmodel") %>%
  mutate(date = as.Date(date)) %>%
  filter(date == max(date))

win_prob_model <- read_rds("Stored models/final_win_prob_model.rds")
breakaway_model <- read_rds("Stored models/breakaway_glm.rds")

BREAKAWAY_PRED <- predict(breakaway_model, 
                          tibble(pred_climb_difficulty = PCD, predicted_bs = BS, uphill_finish = UphillFinish, grand_tour = 1))

BREAKAWAY_PRED = exp(BREAKAWAY_PRED) / (1+exp(BREAKAWAY_PRED))

# overwrite
BREAKAWAY_PRED <- 0.1

#

applying_next_level <- models %>%
  
  filter(Type %in% c("Leader", "PointsWhenOpp")) %>%
  
  select(rider, team, Type, adj) %>%
  spread(Type, adj) %>%
  
  rename(pred_pointswhenopp = PointsWhenOpp,
         shrunk_teamldr = Leader) %>%
  mutate(rel_age = 0) %>%
  
  group_by(team) %>%
  mutate(No1_Team = rank(desc(shrunk_teamldr), ties.method = "min") == 1,
         No1_Team = as.numeric(No1_Team)) %>%
  ungroup() %>%
  
  mutate(rk_pointswhenopp = rank(desc(pred_pointswhenopp), ties.method = 'min')) %>%
  
  left_join(
    
    recent_perf, by = c('rider')
    
  ) %>%
  
  mutate(error_rank = ifelse(is.na(error_rank), 0, error_rank),
         rel_leader = ifelse(is.na(rel_leader), 0, rel_leader),
         races = ifelse(is.na(races), 0, races),
         days_of_comps = ifelse(is.na(days_of_comps), 0, days_of_comps)) %>%
  
  # center error rank and adjust for <10 races
  mutate(error_rank = error_rank / (1 / races) * 0.1) %>%
  
  mutate(error_rank = error_rank - mean(error_rank, na.rm = T)) %>%
  
  mutate(log_rk_pwo = log(rk_pointswhenopp + 1),
         ppwo = pred_pointswhenopp * -1) %>%
  
  mutate(pred_break_win = BREAKAWAY_PRED) %>%
  
  mutate(rider_match = str_to_title(rider)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, date as dob FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)), by = c("rider_match" = "rider")) %>%
  
  mutate(age = as.numeric(as.Date(date)-as.Date(dob))/365.25) %>%

  mutate(rel_age = age - mean(age, na.rm = T),
         rel_age = ifelse(is.na(rel_age), 0, rel_age)) %>%
  
  select(-dob, -age, -rider_match)

#

win_probabilities <- cbind(
  
  win_probability = predict(win_prob_model, applying_next_level),
  
  applying_next_level
  
) %>%
  
  mutate(win_probability = exp(win_probability) / (1 + exp(win_probability)),
         win_probability = win_probability / sum(win_probability)) %>%
  
  select(-days_of_comps, -races, -exp_leader, -act_leader,
         -exp_rank, -act_rank, -ppwo, -pred_break_win)
