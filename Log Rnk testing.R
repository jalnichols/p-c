
library(tidyverse)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

sdp <- dbGetQuery(con, "SELECT rider, stage, race, year, class, date, length, team, master_team,
                  rnk, pred_climb_difficulty, bunch_sprint, tm_pos, one_day_race, time_trial, sof,
                  sof_limit
                  FROM stage_data_perf
                  WHERE year >= 2017 & class <> 'JR'") %>%
  mutate(stage = as.character(stage)) 

dx <- sdp %>% 
  filter(time_trial == 0) %>%
  filter(!class %in% c("NC", "JR")) %>%
  mutate(rnk1 = ifelse(rnk >= 200, NA, rnk)) %>%
  
  group_by(stage, race, year, class, date) %>%
  mutate(rnk = ifelse(rnk==200, max(rnk1)+1, rnk)) %>%
  ungroup() %>%
  
  mutate(log_rnk = log(rnk)) %>%
  
  filter(!is.na(sof)) %>%
  filter(!is.na(log_rnk)) %>%
  
  group_by(rider, year) %>% 
  filter(min(rnk)<=5) %>%
  filter(n() > 19) %>%
  mutate(sof = sof - mean(sof, na.rm = T),
         log_rnk = mean(log_rnk, na.rm = T) - log_rnk) %>%
  do(broom::tidy(lm(log_rnk ~ sof, data = .))) %>%
  ungroup()

#

dx %>% group_by(term) %>% summarize(median(estimate), mean(estimate))

# MEDIAN impact of -2.14 per 1 sof

# a player who has an avg log_rnk of 33rd at average sof race is 23rd at a race -0.25 sof worse than average and
# 48th at a race +0.25 sof better than average

log_rnks <- sdp %>% 
  filter(time_trial == 0) %>%
  filter(!class %in% c("NC", "JR")) %>%
  mutate(rnk1 = ifelse(rnk >= 200, NA, rnk)) %>%
  
  group_by(stage, race, year, class, date) %>%
  mutate(rnk = ifelse(rnk==200, max(rnk1)+1, rnk)) %>%
  ungroup() %>%
  
  mutate(log_rnk = log(rnk),
         sof_delta = ((sof - 0.5) * -2.14)) %>%
  
  filter(!is.na(sof)) %>%
  filter(!is.na(log_rnk)) %>%
  
  group_by(rider, year) %>%
  filter(n() > 9) %>%
  summarize(avg_log_rnk = mean(log_rnk, na.rm = T), 
            sof_delta = mean(sof_delta, na.rm = T),
            sof = mean(sof, na.rm = T), 
            races = n()) %>%
  ungroup() %>%
  
  mutate(avg_rnk = exp(avg_log_rnk + sof_delta))

#

TTs <- sdp %>% 
  filter(time_trial == 1) %>%
  filter(!class %in% c("NC", "JR")) %>%
  mutate(rnk1 = ifelse(rnk >= 200, NA, rnk)) %>%
  
  group_by(stage, race, year, class, date) %>%
  mutate(rnk = ifelse(rnk==200, max(rnk1)+1, rnk)) %>%
  ungroup() %>%
  
  mutate(log_rnk = log(rnk),
         sof_delta = ((sof - 0.5) * -2.14)) %>%
  #log_rnk = log_rnk + sof_delta) %>%
  
  filter(!is.na(sof)) %>%
  filter(!is.na(log_rnk)) %>%
  
  group_by(rider, year) %>%
  filter(n() > 0) %>%
  summarize(avg_log_rnk = mean(log_rnk, na.rm = T), 
            sof_delta = mean(sof_delta, na.rm = T),
            sof = mean(sof, na.rm = T), 
            races = n()) %>%
  ungroup() %>%
  
  mutate(avg_rnk = exp(avg_log_rnk + sof_delta))
