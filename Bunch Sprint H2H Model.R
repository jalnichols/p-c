#

library(tidyverse)
library(RMySQL)
library(BradleyTerry2)
library(lme4)

#

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#
#
#

All_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf
                       WHERE time_trial = 0 AND year >= 2014") %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  
  mutate(points_per_opp = ifelse(tm_pos == 1, points_finish, NA),
         sof_per_opp = ifelse(tm_pos == 1, sof, NA),
         pred_climb_diff_opp = ifelse(tm_pos == 1, pred_climb_difficulty, NA),
         pred_climb_diff_succ = ifelse(points_finish > 0, pred_climb_difficulty, NA),
         team_ldr = ifelse(tm_pos == 1, 1, 0)) %>%
  
  mutate(date = as.Date(date)) %>%
  
  select(-stage_name, -speed, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th,
         -time_trial, -gc_winner, -gc_pos, -parcours_value, -stage_type) %>%
  
  filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC", "Olympics")) |
           (class %in% c("2.1", "1.1") & Tour == "Europe Tour") | 
           (sof > 0.2 & class %in% c("2.2", "1.2", "2.2U", "1.2U", "2.Ncup", "1.Ncup", "JR")) |
           (sof > 0.1 & !class %in% c("2.2", "1.2", "2.2U", "1.2U", "2.Ncup", "1.Ncup", "JR"))) %>%
  unique()

#
#
#

# based on the below which plots finishing positions of elite sprinters in Bunch Sprint races
# most of the time these elite sprinters are finishing in top 15 or top 30
# so if we limit to top 30 we capture sprinters who attempted to sprint

All_data %>%   
  filter(bunch_sprint == 1) %>%
  filter(!is.na(rnk)) %>%
  filter(year >= 2019) %>% 
  filter(rider %in% c("Bennett Sam", "Viviani Elia", "Ackermann Pascal", "Sagan Peter", "Ewan Caleb")) %>% 
  
  filter(gain_1st == 0) %>%
  
  ggplot(aes(x = rnk))+
  geom_histogram(binwidth = 5, color = "white")+
  scale_x_continuous(breaks = seq(0,200,5))

sprint_h2h_data <- All_data %>%
  
  filter(bunch_sprint == 1) %>%
  filter(!is.na(rnk)) %>%
  filter(rnk < 31)

#
#
#

sprint_h2h <- sprint_h2h_data %>%
  
  rename(rnk1 = rnk,
         rider1 = rider,
         team1 = master_team) %>%
  
  inner_join(
    
    sprint_h2h_data %>%
      select(rider2 = rider,
             stage, race, year,
             team2 = master_team,
             rnk2 = rnk), by = c("stage", "race", "year")) %>%
  
  filter(!rider1 == rider2) %>%
  
  mutate(adv1 = ifelse(rnk1 < rnk2, 1, 0),
         adv2 = ifelse(rnk1 > rnk2, 1, 0)) %>%
  
  filter(year >= 2014) %>%
  
  group_by(rider1) %>%
  mutate(r1_n = n()) %>%
  ungroup() %>%
  
  group_by(rider2) %>%
  mutate(r2_n = n()) %>%
  ungroup() %>%
  
  filter(!is.na(date))

#

sprint_h2h_team <- All_data %>%
  
  filter(bunch_sprint == 1) %>%
  filter(!is.na(rnk)) %>%
  filter(tm_pos == 1) %>%
  
  rename(rnk1 = rnk,
         team1 = master_team) %>%
  
  inner_join(
    
    All_data %>%
      
      filter(bunch_sprint == 1) %>%
      filter(!is.na(rnk)) %>%
      filter(tm_pos == 1) %>%
      
      rename(rnk2 = rnk,
             team2 = master_team), by = c("stage", "race", "year", "class", "date")) %>%
  
  filter(!team1 == team2) %>%
  
  mutate(adv1 = ifelse(rnk1 < rnk2, 1, 0),
         adv2 = ifelse(rnk1 > rnk2, 1, 0)) %>%
  
  filter(year >= 2014) %>%
  
  group_by(team1) %>%
  mutate(r1_n = n()) %>%
  ungroup() %>%
  
  group_by(team2) %>%
  mutate(r2_n = n()) %>%
  ungroup() %>%
  
  filter(!is.na(date))

#
#
#

df_list <- vector("list", 5)
ranef_list <- vector("list", 5)
team_list <- vector("list", 5)

# for loops by year

for(y in 1:4) {
  
  cutoff <- as.Date(paste0(y + 2015, "-06-30"))
  
  #
  
  training <- sprint_h2h %>%
    
    filter(date < cutoff & date >= (cutoff - 915)) %>%
    
    mutate(cohort = y,
           type = 'training') %>%
    
    filter((r1_n > 170 & r2_n > 170))
  
  #
  
  testing <- sprint_h2h %>%
    
    filter(date >= cutoff & date < (cutoff + 180)) %>%
    
    mutate(cohort = y,
           type = 'testing') %>%
    
    filter((r1_n > 170 & r2_n > 170))
  
  #
  
  # h2h_glmer <- glmer(adv1 ~ (1 | rider1) + (1 | rider2), 
  #                    data = training,
  #                    family = "binomial")
  
  h2h_glmer <- glmer(adv1 ~ (1 | rider1) + (1 | rider2) + (1 | team1) + (1 | team2),
                     data = training,
                     family = "binomial")
  
  ranefs <- ranef(h2h_glmer)[[1]] %>%
    rownames_to_column() %>%
    janitor::clean_names() %>%
    rename(rider = rowname)
  
  teams <- ranef(h2h_glmer)[[3]] %>%
    rownames_to_column() %>%
    janitor::clean_names() %>%
    rename(team = rowname)
  
  preds <- cbind(coef = predict(h2h_glmer, testing %>%
                                  filter(rider1 %in% ranefs$rider & rider2 %in% ranefs$rider &
                                           team1 %in% teams$team & team2 %in% teams$team)),
                 testing %>%
                   filter(rider1 %in% ranefs$rider & rider2 %in% ranefs$rider &
                            team1 %in% teams$team & team2 %in% teams$team)) %>%
    
    mutate(prob = exp(coef) / (1+exp(coef)))
  
  df_list[[y]] <- preds
  ranef_list[[y]] <- ranefs
  team_list[[y]] <- teams
  
}

#
#
#

model_results2_5_tm <- bind_rows(df_list)
rider_results2_5_tm <- bind_rows(ranef_list)
team_results2_5_tm <- bind_rows(team_list)

#

model_results0_5 %>%
  summarize(sum((adv1-prob)^2)/n()) # brier score for half a year 

model_results1_5 %>%
  summarize(sum((adv1-prob)^2)/n()) # brier score for 1.5 years

model_results2_5 %>%
  summarize(sum((adv1-prob)^2)/n()) # brier score for 2.5 years

model_results2_5_tm %>%
  summarize(sum((adv1-prob)^2)/n()) # brier score for 2.5 years with team

#
#
#

model_results2_5_tm %>%
  group_by(f = floor(prob / 0.0714), cohort) %>% 
  summarize(prob = mean(prob), 
            adv1 = mean(adv1), 
            n=n()) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = prob, y = adv1, color = as.factor(cohort+2015)))+
  
  geom_point(color = "black")+
  geom_smooth(method = 'lm', se = F)+
  labs(x = "modeled probability", y = "actual results", title = "Bunch Sprint h2h model", color = "Model Year")+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)

#

ggsave("bunch-sprint-h2h-model-eval-0_5years.png", height = 7, width = 8)

#
#
#

sprintBT <- sprint_h2h %>%
  
  group_by(rider1, rider2) %>%
  summarize(win1 = sum(adv1, na.rm = T),
            win2 = sum(adv2, na.rm = T)) %>%
  ungroup()

#
#
#

mod1 <- BradleyTerry2::BTm(cbind(win1, win2), rider1, rider2,
                           formula = ~ rider, id = "rider",
                           data = sprint_h2h)

#
#
#

library(lme4)

h2h_glmer <- glmer(adv1 ~ (1 | rider1) + (1 | rider2), 
                   
                   data = sprint_h2h,
                   
                   family = binomial("logit"),
                   nAGQ=0,
                   control=lme4::glmerControl(optimizer = "nloptwrap"))

#
#
#

riders_impact_sprint <- ranef(h2h_glmer)[[1]] %>% 
  rownames_to_column() %>% 
  janitor::clean_names() %>%
  mutate(adv_10th = exp(intercept - 2)/(1+exp(intercept-2)))

#
#
#
#
#
#
#

sprint_h2h <- sprint_h2h %>%
  
  select(rider1, rider2, adv1, adv2, date, race, stage, year, r1_n, r2_n, rnk1, rnk2, team1, team2)

sprint_h2h_team <- sprint_h2h_team %>%
  
  select(adv1, adv2, date, race, stage, year, r1_n, r2_n, rnk1, rnk2, team1, team2)

#

dates_to_pull <- expand_grid(months = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11'),
                             years = c('2016', '2017', '2018', '2019', '2020')) %>%
  
  mutate(days = ifelse(months == "11", '10',
                       ifelse(months == '01', "15", '01'))) %>%
  
  mutate(date = as.Date(paste0(years, "-", months, "-", days))) %>%
  
  filter(date < '2020-04-01' | date > '2020-07-15') %>%
  
  filter(!date %in% as.Date(c("2019-11-10", "2018-11-10", "2017-11-10", "2016-11-10")))

#

df_list <- vector("list", length(dates_to_pull$months))
ranef_list <- vector("list", length(dates_to_pull$months))
tm_list <- vector("list", length(dates_to_pull$months))

# for loops by year

for(y in 1:length(df_list)) {
  
  cutoff <- dates_to_pull$date[[y]]
  
  if(cutoff > '2020-01-01') {
    
    end = 300
    
  } else {
    
    end = 180
    
  }
  
  if(cutoff > '2020-07-01') {
    
    start = 670
    
  } else {
    
    start = 550
    
  }
  
  #
  
  team_data <- sprint_h2h_team %>%
    
    filter(date < cutoff & date >= (cutoff - start)) %>%
    
    group_by(team1) %>%
    mutate(r1_n = n()) %>%
    ungroup() %>%
    
    group_by(team2) %>%
    mutate(r2_n = n()) %>%
    ungroup() %>%
    
    filter((r1_n >= 1000 & r2_n >= 1000))
  
  #
  
  tm_bs_glmer <- glmer(adv1 ~ (1 | team1) + (1 | team2), 
                     data = team_data,
                     family = binomial("logit"),
                     nAGQ=0,
                     control=lme4::glmerControl(optimizer = "nloptwrap"))
  
  tm_ranefs <- ranef(tm_bs_glmer)[[1]] %>%
    rownames_to_column() %>%
    janitor::clean_names() %>%
    rename(team = rowname) %>%
    
    mutate(pred_date = cutoff)
  
  #
  
  tm_list[[y]] <- tm_ranefs
  
  #
  
  training <- sprint_h2h %>%
    
    filter(date < cutoff & date >= (cutoff - start)) %>%
    
    group_by(rider1) %>%
    mutate(r1_n = n()) %>%
    ungroup() %>%
    
    group_by(rider2) %>%
    mutate(r2_n = n()) %>%
    ungroup() %>%
    
    filter((r1_n >= 250 & r2_n >= 250))
  
  training <- cbind(training,
                    
                    pred_team = predict(tm_bs_glmer, training, allow.new.levels = TRUE)) %>%
    
    mutate(pred_team = exp(pred_team)/(1+exp(pred_team)))

  #
  
    testing <- sprint_h2h %>%
      
      filter(date >= cutoff & date < (cutoff + end)) %>%
      
      group_by(rider1) %>%
      mutate(r1_n = n()) %>%
      ungroup() %>%
      
      group_by(rider2) %>%
      mutate(r2_n = n()) %>%
      ungroup() %>%
      
      filter((r1_n >= 250 & r2_n >= 250))

  #
  
  tictoc::tic()
  
   h2h_glmer <- glmer(adv1 ~ (1 | rider1) + (1 | rider2) + pred_team, 
                      data = training,
                      family = binomial("logit"),
                      nAGQ=0,
                      control=lme4::glmerControl(optimizer = "nloptwrap"))

  ranefs <- ranef(h2h_glmer)[[1]] %>%
    rownames_to_column() %>%
    janitor::clean_names() %>%
    rename(rider = rowname) %>%
    
    mutate(pred_date = cutoff)

  tictoc::toc()
  
  if(cutoff > '2020-08-01') {
    
  } else {
    
    testing <- cbind(testing,
                     
                     pred_team = predict(tm_bs_glmer, testing, allow.new.levels = TRUE)) %>%
      
      mutate(pred_team = exp(pred_team)/(1+exp(pred_team)))
    
  preds <- cbind(coef = predict(h2h_glmer, testing %>%
                                  filter(rider1 %in% ranefs$rider & rider2 %in% ranefs$rider)),
                 testing %>%
                   filter(rider1 %in% ranefs$rider & rider2 %in% ranefs$rider)) %>%
    
    mutate(prob = exp(coef) / (1+exp(coef))) %>%
    
    mutate(pred_date = cutoff)
  
  df_list[[y]] <- preds
  
  }
  
  ranef_list[[y]] <- ranefs
  
}

#
#
#

model_results <- bind_rows(df_list) %>%
  
  filter(pred_date < '2020-09-01')
  
# model is fairly well calibrated, but could benefit from regression at tails
# regress 10% to 50% using regr_prob below and it is perfectly matched

model_results %>% 
  mutate(regr_prob = ((prob * 9)+(0.5))/10) %>%
  
  group_by(f = floor(prob / 0.02) / 50) %>% 
  summarize(pred = mean(prob, na.rm = T), 
            act = mean(adv1, na.rm = T), 
            n = n()) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = pred, y = act))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Predicted probability", 
       y = "Actual probability", 
       title = "Bunch sprint predictive model performance")+
  geom_blank(data = tibble(pred = c(0,1), act = c(0,1)))

model_results %>% 
  mutate(regr_prob = ((prob * 9)+(0.5))/10) %>%
  
  summarize(BRIER_RAW = mean((adv1-prob)^2),
            BRIER_REGR = mean((adv1-regr_prob)^2))

#
#
#

rider_results<- bind_rows(ranef_list) %>%
  group_by(pred_date) %>% 
  mutate(rk = rank(-intercept, ties.method = "min")) %>% 
  ungroup() %>% 
  
  mutate(top10 = ifelse(rk <= 10, intercept, NA)) %>% 
  
  group_by(pred_date) %>% 
  mutate(top10 = mean(top10, na.rm = T)) %>% 
  ungroup() %>% 
  
  mutate(top10 = ifelse(rk > 10, top10, ((top10*9) - intercept)/9)) %>%
  
  mutate(prob = exp(intercept - top10)/(1+exp(intercept - top10)))

#
#
#

sprint_h2h <- sprint_h2h %>%
  
  select(rider1, rider2, adv1, adv2, date, race, stage, year, r1_n, r2_n, rnk1, rnk2, team1, team2)

sprint_h2h_team <- sprint_h2h_team %>%
  
  select(adv1, adv2, date, race, stage, year, r1_n, r2_n, rnk1, rnk2, team1, team2)

#

dates_to_pull <- dbReadTable(con, "stage_data_perf") %>%
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC", "Olympics")) |
           (class %in% c("2.1", "1.1") & Tour == "Europe Tour") | 
           (sof > 0.2 & class %in% c("2.2", "1.2", "2.2U", "1.2U", "2.Ncup", "1.Ncup", "JR")) |
           (sof > 0.1 & !class %in% c("2.2", "1.2", "2.2U", "1.2U", "2.Ncup", "1.Ncup", "JR"))) %>%
  
  filter(year > 2015 & year <= 2021) %>%
  select(date) %>%
  unique() %>%
  filter(!is.na(date)) %>%
  
  mutate(date = as.Date(date)) %>%
  
  rbind(tibble(date = as.Date(lubridate::today()))) %>%
  
  mutate(date = as.Date(date, origin = '1970-01-01'))

#

tictoc::tic()

df_list <- vector("list", length(dates_to_pull$date))
tm_list <- vector("list", length(dates_to_pull$date))

# for loops by year

for(y in 1:length(df_list)) {
  
  cutoff <- dates_to_pull$date[[y]]
  
  if(cutoff > '2020-01-01') {
    
    end = 300
    
  } else {
    
    end = 180
    
  }
  
  if(cutoff > '2020-07-01') {
    
    start = 670
    
  } else {
    
    start = 550
    
  }
  
  #
  
  team_data <- sprint_h2h_team %>%
    
    filter(date < cutoff & date >= (cutoff - start)) %>%
    
    group_by(team1) %>%
    mutate(r1_n = n()) %>%
    ungroup() %>%
    
    group_by(team2) %>%
    mutate(r2_n = n()) %>%
    ungroup() %>%
    
    filter((r1_n >= 1000 & r2_n >= 1000))
  
  #
  
  tm_bs_glmer <- glmer(adv1 ~ (1 | team1) + (1 | team2), 
                       data = team_data,
                       family = binomial("logit"),
                       nAGQ=0,
                       control=lme4::glmerControl(optimizer = "nloptwrap"))
  
  tm_ranefs <- ranef(tm_bs_glmer)[[1]] %>%
    rownames_to_column() %>%
    janitor::clean_names() %>%
    rename(team = rowname) %>%
    
    mutate(pred_date = cutoff)
  
  #
  
  tm_list[[y]] <- tm_ranefs
  
  #
  
  training <- sprint_h2h %>%
    
    filter(date < cutoff & date >= (cutoff - start)) %>%
    
    group_by(rider1) %>%
    mutate(r1_n = n()) %>%
    ungroup() %>%
    
    group_by(rider2) %>%
    mutate(r2_n = n()) %>%
    ungroup() %>%
    
    filter((r1_n >= 250 & r2_n >= 250))
  
  training <- cbind(training,
                    
                    pred_team = predict(tm_bs_glmer, training, allow.new.levels = TRUE)) %>%
    
    mutate(pred_team = exp(pred_team)/(1+exp(pred_team)))

  #
  
  h2h_glmer <- glmer(adv1 ~ (1 | rider1) + (1 | rider2) + pred_team, 
                     data = training,
                     family = binomial("logit"),
                     nAGQ=0,
                     control=lme4::glmerControl(optimizer = "nloptwrap"))
  
  ranefs <- ranef(h2h_glmer)[[1]] %>%
    rownames_to_column() %>%
    janitor::clean_names() %>%
    rename(rider = rowname) %>%
    
    mutate(pred_date = cutoff)
  
  print(y)
  
  print(head(ranefs %>% arrange(-intercept)))
  
  ranef_list[[y]] <- ranefs
  
}

tictoc::toc()

#
#
#

rider_results<- bind_rows(ranef_list) %>%
  group_by(pred_date) %>% 
  mutate(rk = rank(-intercept, ties.method = "min")) %>% 
  ungroup() %>% 
  
  mutate(top10 = ifelse(rk <= 10, intercept, NA)) %>% 
  
  group_by(pred_date) %>% 
  mutate(top10 = mean(top10, na.rm = T)) %>% 
  ungroup() %>% 
  
  mutate(top10 = ifelse(rk > 10, top10, ((top10*9) - intercept)/9)) %>%
  
  mutate(prob = exp(intercept - top10)/(1+exp(intercept - top10)))

#

dbWriteTable(con, 
             "lme4_rider_bunchsprinth2h",
             rider_results %>% select(rider, intercept, date = pred_date, prob_vs_top10 = prob, rank = rk),
             row.names = F, 
             append = TRUE)

#

team_bs_results <- bind_rows(tm_list) %>% 
  
  rename(master_team = team) %>%
  
  group_by(date = pred_date) %>% 
  mutate(team_rank = rank(-intercept, ties.method = "min")) %>%
  ungroup()

#

dbWriteTable(con, 
             "lme4_team_bunchsprinth2h",
             team_bs_results,
             row.names = F, 
             append = TRUE)

#
#
#
#
#

sprint_h2h %>% 
  inner_join(dbGetQuery(con, "SELECT date as pred_date, rider, intercept as int1
                        FROM lme4_rider_bunchsprinth2h"), by = c("date" = "pred_date", "rider1" = "rider")) %>% 
  
  inner_join(dbGetQuery(con, "SELECT date as pred_date, rider, intercept as int2
                        FROM lme4_rider_bunchsprinth2h"), by = c("date" = "pred_date", "rider2" = "rider")) %>% 
  
  inner_join(
    dbGetQuery(con, "SELECT master_team, date, intercept AS tmint1 FROM lme4_team_bunchsprinth2h"), 
    by = c("date", "team1" = "master_team")) %>% 
  
  inner_join(
    dbGetQuery(con, "SELECT master_team, date, intercept AS tmint2 FROM lme4_team_bunchsprinth2h"),
    by = c("date", "team2" = "master_team")) -> daily_BS_testing

#

daily_BS_testing %>%
  mutate(prob = ((int1 + tmint1) - (int2 + tmint2)), 
         prob = exp(prob)/(1+exp(prob)), regr_prob = ((3*prob)+0.5)/4) %>% 
  
  group_by(f = floor(prob/0.05)*0.05) %>% 
  summarize(act = mean(adv1), 
            pred = mean(prob), 
            regr_pred = mean(regr_prob), 
            n = n()) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = pred, y = act))+
  geom_point()+
  geom_abline(slope=1, intercept = 0)+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(breaks = seq(0,1,0.1), labels = scales::percent)+
  labs(x = "predicted", y = 'actual', title = "Unregressed predictor has large errors")

#

daily_BS_testing %>%
  mutate(prob = ((int1 + tmint1) - (int2 + tmint2)), 
         prob = exp(prob)/(1+exp(prob)), regr_prob = ((3*prob)+0.5)/4) %>% 
  
  group_by(f = floor(prob/0.05)*0.05) %>% 
  summarize(act = mean(adv1), 
            pred = mean(prob), 
            regr_pred = mean(regr_prob), 
            n = n()) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = regr_pred, y = act))+
  geom_point()+
  geom_abline(slope=1, intercept = 0)+
  scale_x_continuous(breaks = seq(0,1,0.1), labels = scales::percent)+
  scale_y_continuous(breaks = seq(0,1,0.1), labels = scales::percent)+
  labs(x = "predicted", y = 'actual', title = "Regressing 25% to mean fits well")

#
#
#
#
#

BS_data <- All_data %>%
  
  filter(bunch_sprint == 1 & year >= 2016) %>%
  
  left_join(dbGetQuery(con, "SELECT date, rider, intercept
                        FROM lme4_rider_bunchsprinth2h") %>%
              mutate(date = as.Date(date)), by = c("date", "rider")) %>%
  
  group_by(stage, race, year, class, date) %>%
  mutate(bs_rank = rank(-intercept, ties.method = 'min')) %>%
  ungroup()
  

# the Nth best sprinter wins what percentage of BS races?

# the best sprinter wins 26% of races followed by 2nd best 13%, 3rd best 10%
# 77% of BS are won by top 10 sprinters including all races which meet BS criteria

BS_data %>% 
  group_by(bs_rank) %>%
  summarize(win_rate = mean(rnk==1, na.rm = T), 
            podium = mean(rnk<=3, na.rm = T),
            n = n()) %>% 
  arrange(bs_rank)

.# BS win prob model based on intercept behind best sprinter / 5th best sprinter / 10th best sprinter

BS_data %>%
  
  filter(rnk <= 30 & gain_1st <= 5) %>%
  
  group_by(stage, race, year, class, date) %>%
  mutate(bs_rank = rank(-intercept, ties.method = 'min')) %>%
  filter(sum(!is.na(intercept)) >= 10) %>%
  ungroup() %>%
  
  mutate(best_intercept = ifelse(bs_rank == 1, intercept, NA),
         win_intercept = ifelse(rnk == 1, is.na(intercept), NA)) %>%
  
  group_by(race, year, stage, class, date) %>%
  mutate(best_intercept = mean(best_intercept, na.rm = T),
         win_intercept = mean(win_intercept, na.rm = T)) %>%
  filter(win_intercept == 0) %>%
  ungroup() %>%
  
  mutate(rel_to_best = best_intercept - intercept) %>%
  
  mutate(win = ifelse(rnk == 1, 1, 0)) %>%

  glm(win ~ rel_to_best + log(bs_rank), data = ., family = "binomial") %>%
  
  summary()
