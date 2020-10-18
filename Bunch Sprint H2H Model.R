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
  
  filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC")) |
           (class %in% c("2.1", "1.1") & Tour == "Europe Tour") | 
           (sof > 0.25 & class %in% c("2.1", "1.1"))) %>%
  unique()

#
#
#

sprint_h2h_data <- All_data %>%
  
  filter(bunch_sprint == 1) %>%
  filter(!is.na(rnk)) %>%
  filter(rnk < 21)

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
                   
                   family = "binomial")

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

months <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10')

years <- c('2016', '2017', '2018', '2019', '2020')

df_list <- vector("list", length(months) * length(years))
ranef_list <- vector("list", length(months) * length(years))

# for loops by year

for(y in 41:length(df_list)) {
  
  Y = floor((y-1) / length(months))+1
  M = ((y-1) %% length(months))+1
  
  cutoff <- as.Date(paste0(years[[Y]], "-", months[[M]], "-01"))
  
  if(cutoff %in% as.Date(c('2020-01-01', '2020-02-01', '2020-03-01', '2020-07-01',
                   '2020-08-01', '2020-09-01', '2020-10-01'))) {
    
    end = 360
    
  } else {
    
    end = 180
    
  }
  
  if(cutoff %in% as.Date(c('2020-01-01', '2020-02-01', '2020-03-01', '2020-07-01',
                           '2020-08-01', '2020-09-01', '2020-10-01'))) {
    
    start = 720
    
  } else {
    
    start = 550
    
  }
  
  #
  
  training <- sprint_h2h %>%
    
    filter(date < cutoff & date >= (cutoff - start)) %>%
    
    group_by(rider1) %>%
    mutate(r1_n = n()) %>%
    ungroup() %>%
    
    group_by(rider2) %>%
    mutate(r2_n = n()) %>%
    ungroup() %>%
    
    filter((r1_n > 170 & r2_n > 170))
  
  #
  
  if(cutoff %in% as.Date(c('2020-09-01', '2020-10-01'))) {
    
  testing <- sprint_h2h %>%
    
    filter(date >= cutoff & date < (cutoff + end)) %>%
    
    group_by(rider1) %>%
    mutate(r1_n = n()) %>%
    ungroup() %>%
    
    group_by(rider2) %>%
    mutate(r2_n = n()) %>%
    ungroup()
  
  }
  
  else {
    
    testing <- sprint_h2h %>%
      
      filter(date >= cutoff & date < (cutoff + end)) %>%
      
      group_by(rider1) %>%
      mutate(r1_n = n()) %>%
      ungroup() %>%
      
      group_by(rider2) %>%
      mutate(r2_n = n()) %>%
      ungroup() %>%
      
      filter((r1_n > 170 & r2_n > 170))
    
  }
  
  #
  
  tictoc::tic()
  
   h2h_glmer <- glmer(adv1 ~ (1 | rider1) + (1 | rider2), 
                      data = training,
                      family = "binomial")

  ranefs <- ranef(h2h_glmer)[[1]] %>%
    rownames_to_column() %>%
    janitor::clean_names() %>%
    rename(rider = rowname) %>%
    
    mutate(pred_date = cutoff)

  tictoc::toc()
  
  preds <- cbind(coef = predict(h2h_glmer, testing %>%
                                  filter(rider1 %in% ranefs$rider & rider2 %in% ranefs$rider)),
                 testing %>%
                   filter(rider1 %in% ranefs$rider & rider2 %in% ranefs$rider)) %>%
    
    mutate(prob = exp(coef) / (1+exp(coef))) %>%
    
    mutate(pred_date = cutoff)
  
  df_list[[y]] <- preds
  ranef_list[[y]] <- ranefs
  
}

#
#
#

model_results <- bind_rows(df_list)

rider_results<- bind_rows(ranef_list) %>%
  group_by(pred_date) %>% 
  mutate(rk = rank(-intercept, ties.method = "min")) %>% 
  ungroup() %>% 
  
  mutate(top10 = ifelse(rk <= 10, intercept, NA)) %>% 
  
  group_by(pred_date) %>% 
  mutate(top10 = mean(top10, na.rm = T)) %>% 
  ungroup() %>% 
  
  mutate(prob = exp(intercept - top10)/(1+exp(intercept - top10)))

#
#
#

sprinters_in_races <- All_data %>%
  
  filter(bunch_sprint == 1) %>%
  
  mutate(join_date = as.Date(paste0(lubridate::year(date), "-", lubridate::month(date), "-01"))) %>%
  
  inner_join(
    
    rider_results, by = c("join_date" = "pred_date", "rider")
    
  ) %>%
  
  group_by(race, stage, year, team) %>%
  mutate(spr_team_rk = rank(-intercept, ties.method = "min")) %>%
  ungroup() %>%
  
  group_by(race, stage, year) %>%
  mutate(spr_race_rk = rank(-intercept, ties.method = "min")) %>%
  ungroup()