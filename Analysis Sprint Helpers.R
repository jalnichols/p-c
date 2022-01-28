
library(tidyverse)
library(RMySQL)


DBI::dbDisconnect(con)

con <- dbConnect(RMySQL::MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

All_data <- dbReadTable(con, "stage_data_perf") %>%
  
  filter(time_trial == 0) %>%
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  
  mutate(points_per_opp = ifelse(tm_pos == 1, points_finish, NA),
         sof_per_opp = ifelse(tm_pos == 1, sof, NA),
         pred_climb_diff_opp = ifelse(tm_pos == 1, pred_climb_difficulty, NA),
         pred_climb_diff_succ = ifelse(points_finish > 0, pred_climb_difficulty, NA),
         team_ldr = ifelse(tm_pos == 1, 1, 0),
         team_ldrRESTR = ifelse(tm_pos == 1 & rnk <= 10, 1, 0)) %>%
  
  mutate(date = as.Date(date)) %>%
  
  select(-speed, -gain_3rd, -gain_5th, -gain_10th, -gain_40th,
         -time_trial, -gc_winner, -gc_pos, -parcours_value, -stage_type,
         -avg_alt, -missing_profile_data) %>%
  
  filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC", "Olympics")) |
           (class %in% c("2.1", "1.1") & Tour == "Europe Tour")) %>%
  unique() %>% 
  
  mutate(final_group = ifelse(bunch_sprint == 1, ifelse(gain_1st <= 5, 1, 0), ifelse(rnk <= 20 | gain_20th == 0, 1, 0))) %>%
  
  select(-gain_1st, -gain_20th) %>%
  
  mutate(points_finish = (1 / (rnk + 1)) * (sof_limit / 5),
         points_finish = ifelse(rnk <= (sof_limit * 5), points_finish, 0))

#
#
#

who_contributed <- All_data %>%
  
  filter(bunch_sprint == 1) %>%

  group_by(team, stage, race, year, class, date, length) %>%
  mutate(best_rnk = min(rnk, na.rm = T),
         did_win = ifelse(best_rnk == 1, 1, 0)) %>%
  ungroup() %>%
  
  mutate(gap_from_winner = ifelse(rnk == 200, 999, total_seconds - win_seconds)) %>%
  
  group_by(stage, race, year, class, date, length) %>%
  mutate(starting_gap = ifelse(gap_from_winner > 0, rnk, NA),
         starting_gap = min(starting_gap, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(gap_from_winner = ifelse(rnk > starting_gap, 999, gap_from_winner))

#


win_rate_all <- who_contributed %>%
  
  group_by(rider, year) %>%
  summarize(win_rate = mean(did_win, na.rm = T),
            personal_wins = mean(rnk == 1, na.rm = T),
            world_tour = mean(class %in% c("WT", "1.UWT", "2.UWT")),
            races = n()) %>%
  ungroup() %>%
  
  mutate(team_wins = win_rate - personal_wins)

#

win_rate <- who_contributed %>%
  
  mutate(with = ifelse(gap_from_winner <= 0, did_win, NA),
         without = ifelse(gap_from_winner > 0, did_win, NA),
         with_rnk = ifelse(gap_from_winner <= 0, rnk, NA),
         wo_rnk = ifelse(gap_from_winner > 0, rnk, NA)) %>%
  
  group_by(rider) %>%
  summarize(win_rate = mean(with, na.rm = T),
            without = mean(without, na.rm = T),
            still_there = mean(gap_from_winner == 0, na.rm = T),
            personal_wins = mean(rnk == 1, na.rm = T),
            world_tour = mean(class %in% c("WT", "1.UWT", "2.UWT")),
            with_rnk = median(with_rnk, na.rm = T),
            wo_rnk = median(wo_rnk, na.rm = T),
            races = n()) %>%
  ungroup() %>%
  
  mutate(team_wins = win_rate - personal_wins,
         wowy = win_rate - without)

#
#
#
#
#

top_sprinters <- dbGetQuery(con, "SELECT rider, (random_intercept + bunchsprint_impact) AS prediction, Date
                             FROM lme4_rider_logranks") %>%
  
  group_by(Date) %>%
  mutate(sprint_rank = percent_rank(desc(prediction))) %>%
  ungroup() %>%
  
  mutate(Date = as.Date(Date)) %>%
  
  filter(Date > '2016-07-02')

#

who_races_with_best_sprinters <- who_contributed %>%
  
  filter(date > '2016-07-01') %>%
  
  left_join(top_sprinters, by = c("date" = "Date", "rider")) %>%
  
  mutate(prediction = ifelse(is.na(prediction), median(prediction, na.rm = T), prediction)) %>%
  
  group_by(team, stage, race, year, class, date, length) %>%
  mutate(best_sprinter = min(prediction, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(master_team, year) %>%
  mutate(rel_best = best_sprinter - mean(best_sprinter, na.rm = T)) %>%
  ungroup()

#

who_races_with_best_sprinters %>%
  filter(date > '2016-07-01') %>%
  group_by(rider) %>% 
  summarize(Rel = mean(rel_best*-1, na.rm = T), 
            is_best = mean(best_sprinter == prediction, na.rm = T), 
            races = n()) %>% 
  ungroup() -> xyz

#

WOWY_DQS <- who_races_with_best_sprinters %>%
  
  filter(master_team == "Quick Step") %>%
  
  select(stage, race, year, class, date, length, best_sprinter, best_rnk, master_team, rider) %>%
  mutate(present = 1) %>%
  
  group_by(rider) %>%
  filter(n() >= 30) %>%
  ungroup() %>%
  
  spread(rider, present) %>%
  gather(rider, present, -c(stage, race, year, class, date, length, best_sprinter, best_rnk, master_team)) %>%
  mutate(present = ifelse(is.na(present), 0, present)) %>%
  spread(rider, present) %>%
  
  janitor::clean_names()

#

impact_mod <- glm(win ~ .,
    family = "binomial",
   data = WOWY_DQS %>% mutate(win = ifelse(best_rnk == 1, 1, 0)) %>%
     select(-c(stage, race, year, class, date, length, best_rnk, master_team)))

coef(impact_mod) %>% enframe() -> impacts

#

all_df <- WOWY_DQS %>% 
  mutate(win = ifelse(best_rnk == 1, 1, 0)) %>%
  select(-c(stage, race, class, date, length, best_rnk, master_team)) %>%
  mutate(best_sprinter = best_sprinter*-1)

y_train <- all_df %>% filter(year < 2022) %>% select(win) %>% as.matrix()
y_test <- all_df %>% filter(year == 2022) %>% select(win) %>% as.matrix()

x_train <- all_df %>% filter(year < 2022) %>% select(-win, -year) %>% as.matrix()
x_test <- all_df %>% filter(year == 2022) %>% select(-win, -year) %>% as.matrix()

#

lambdas <- 10^seq(1.25, -5, by = -.1)

fit.lasso <- glmnet::cv.glmnet(x_train, 
                               y_train,
                               alpha = 1, 
                               lambda = lambdas,
                               family = "binomial",
                               nfolds = 5)

fit.ridge <- glmnet::cv.glmnet(x_train, 
                               y_train,
                               alpha = 0, 
                               lambda = lambdas,
                               family = "binomial",
                               nfolds = 5)

#

lambda_best_lasso <- fit.lasso$lambda.min 

# run the lasso model

lasso_model_all <- glmnet::glmnet(x_train, 
                                  y_train,
                                  alpha = 1, 
                                  lambda = lambda_best_lasso, 
                                  standardize = TRUE,
                                  family = "binomial")

#

lambda_best_ridge <- fit.ridge$lambda.min 

# run the lasso model

ridge_model_all <- glmnet::glmnet(x_train, 
                                  y_train,
                                  alpha = 0, 
                                  lambda = lambda_best_ridge, 
                                  standardize = TRUE,
                                  family = "binomial")

#
#
#
#
#
#
#

poss_teams <- who_races_with_best_sprinters %>%
  
  group_by(master_team) %>%
  summarize(races = n_distinct(stage, race, year, class, date, length)) %>%
  ungroup() %>%
  
  filter(master_team != "X") %>%
  filter(races >= 190) %>%
  
  filter(master_team %in% c("Lotto Soudal", "Mitchelton", "Quick Step",
                            "NTT", "FDJ", "Cofidis", "BORA", "Jumbo Visma",
                            "Bahrain McLaren", "BMC Racing", "Katusha",
                            "Sunweb", "Alpecin Fenix"))

result_list <- vector("list", length(poss_teams$master_team))
lasso_results <- result_list

for(t in 1:length(poss_teams$master_team)) {
  
  TRUE_WOWY <- who_races_with_best_sprinters %>%
    
    filter(master_team %in% poss_teams$master_team[[t]]) %>%
    
    select(stage, race, year, class, date, length, best_sprinter, best_rnk, master_team, rider) %>%
    mutate(present = "in_race") %>%
    
    group_by(rider) %>%
    filter(n() >= 30) %>%
    ungroup() %>%
    
    spread(rider, present) %>%
    gather(rider, present, -c(stage, race, year, class, date, length, best_sprinter, best_rnk, master_team)) %>%
    mutate(present = ifelse(is.na(present), "not_in_race", present)) %>%
    spread(rider, present) %>%
    
    janitor::clean_names() %>%
    
    mutate(win = ifelse(best_rnk == 1, 1, 0)) %>%
    
    gather(rider, present, -c(stage, race, year, class, date, length, best_sprinter, best_rnk, master_team, win)) %>%
    
    group_by(rider, present) %>%
    summarize(win_rate = mean(win, na.rm = T),
              best_sprinter = mean(best_sprinter, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(master_team = poss_teams$master_team[[t]])
  
  #
  
  WOWY_compared <- inner_join(
    TRUE_WOWY %>%
      select(-best_sprinter) %>%
      spread(present, win_rate) %>%
      mutate(WIN_RATE = in_race - not_in_race),
    TRUE_WOWY %>%
      select(-win_rate) %>%
      spread(present, best_sprinter) %>%
      mutate(Sprint_Qual = in_race - not_in_race), by = c("rider", "master_team"))
  
  result_list[[t]] <- WOWY_compared
  
  #
  
  all_df <- who_races_with_best_sprinters %>%
    
    filter(team != "") %>%
    
    select(stage, race, year, class, date, length, best_sprinter, best_rnk, team, rider) %>%
    mutate(present = 1) %>%
    
    group_by(rider) %>%
    filter(n() >= 80) %>%
    ungroup() %>%
    
    spread(rider, present) %>%
    gather(rider, present, -c(stage, race, year, class, date, length, best_sprinter, best_rnk, team)) %>%
    mutate(present = ifelse(is.na(present), 0, present)) %>%
    spread(rider, present) %>%
    
    janitor::clean_names() %>% 
    mutate(win = ifelse(best_rnk == 1, 1, 0),
           level = case_when(class %in% c('1.1', "2.1", "CC") ~ "X.1",
                             class %in% c("1.Pro", "2.Pro", "1.HC", "2.HC") ~ "Pro",
                             class %in% c("Olympics", "WC", '1.UWT', "2.UWT", "WT") ~ "WT",
                             TRUE ~ class)) %>%
    mutate(level_present = 1) %>%
    spread(level, level_present) %>%
    gather(level, level_present, `Pro`:`X.1`) %>%
    mutate(level_present = ifelse(is.na(level_present), 0, level_present)) %>%
    spread(level, level_present) %>%
    select(-c(stage, race, class, date, length, best_rnk, team)) %>%
    mutate(best_sprinter = best_sprinter*-1)
  
  y_train <- all_df %>% filter(year < 2022) %>% select(win) %>% as.matrix()
  y_test <- all_df %>% filter(year == 2022) %>% select(win) %>% as.matrix()
  
  x_train <- all_df %>% filter(year < 2022) %>% select(-win, -year) %>% as.matrix()
  x_test <- all_df %>% filter(year == 2022) %>% select(-win, -year) %>% as.matrix()
  
  
  lambdas <- 10^seq(1.25, -5, by = -.1)
  
  fit.lasso <- glmnet::cv.glmnet(x_train, 
                                 y_train,
                                 alpha = 1, 
                                 lambda = lambdas,
                                 family = "binomial",
                                 nfolds = 5)
  
  #
  
  tmp <- coef(fit.lasso, s = "lambda.min")

  coefs <- data.frame(rider = tmp@Dimnames[[1]][tmp@i + 1], coefficient = tmp@x)
  
  i <- coefs %>% filter(rider == "(Intercept)") %>%
    .[1,2]
  
  bs <- coefs %>% filter(rider == "best_sprinter") %>%
    .[1,2]
  
  #
  
  predictions <- who_races_with_best_sprinters %>%
    
    filter(year == 2021) %>%
    
    filter(team != "") %>%
    
    select(stage, race, year, class, date, length, best_sprinter, best_rnk, team, rider) %>%
    mutate(present = 1) %>%
    
    spread(rider, present) %>%
    janitor::clean_names() %>%
    gather(rider, present, -c(stage, race, year, class, date, length, best_sprinter, best_rnk, team)) %>%
    filter(!is.na(present)) %>%
    
    mutate(win = ifelse(best_rnk == 1, 1, 0)) %>%
    mutate(best_sprinter = best_sprinter*-1) %>%
    
    left_join(coefs, by = c("rider")) %>%
    
    group_by(stage, race, year, class, date, length, best_sprinter, best_rnk, team) %>%
    summarize(total_coefs = sum(coefficient, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(prediction = exp(total_coefs + i + (best_sprinter * bs))/(1+exp(total_coefs + i + (best_sprinter * bs))),
           neutral = exp(i + (best_sprinter * bs))/(1+exp(i + (best_sprinter * bs)))) %>%
    
    group_by(stage, race, year, class, date, length) %>%
    mutate(prediction = prediction / sum(prediction, na.rm = T),
           neutral = neutral / sum(neutral, na.rm = T),
           dumb = 1 / n()) %>%
    ungroup()
  
  predictions %>% mutate(win = ifelse(best_rnk == 1, 1, 0)) %>% 
    summarize(mean((prediction-win)^2, na.rm = T), 
              mean((neutral-win)^2, na.rm = T),
              mean((dumb-win)^2, na.rm = T))

}

#

sprint_impact <- bind_rows(result_list)

lasso_impact <- bind_rows(lasso_results)


#
#
#
#
#

poss_teams <- who_races_with_best_sprinters %>%
  
  filter(year >= 2020) %>%
  
  group_by(master_team) %>%
  summarize(races = n_distinct(stage, race, year, class, date, length)) %>%
  ungroup() %>%
  
  filter(master_team != "X") %>%
  filter(races >= 60)

matched_list <- vector("list", length(poss_teams$master_team))
all_r_list <- matched_list

for(t in 1:length(poss_teams$master_team)) {

  All_data %>% 
    filter(bunch_sprint == 1 & year >= 2020 & master_team == poss_teams$master_team[[t]]) %>%
    
    group_by(stage, race, year, class, date) %>% 
    mutate(best_rnk = min(rnk, na.rm = T)) %>% 
    ungroup() %>%
    
    select(stage, race, year, class, date, rider, best_rnk) %>% 
    
    mutate(present = 1) %>% 
    
    spread(rider, present) %>% 
    
    gather(rider, present, -c(stage, race, year, class, date, best_rnk)) %>% 
    
    mutate(present = ifelse(is.na(present), 0, present)) %>%
    
    spread(rider, present) -> fdj
  
  #
  
  riders_to_look <- All_data %>% 
    filter(bunch_sprint == 1 & year >= 2020 & master_team == poss_teams$master_team[[t]]) %>% 
    group_by(rider) %>% 
    count() %>%
    ungroup() %>%
    
    filter(n >= 15)
  
  #
  
  r_results <- vector("list", length(riders_to_look$rider))
  
  for(r in 1:length(riders_to_look$rider)) {
    
    R <- str_replace_all(
      str_replace_all(
        str_replace_all(
          str_to_lower(
            str_replace_all(riders_to_look$rider[[r]], " ", "_")), "'", ""), "-","_"),"\\?","_")
    
    r_results[[r]] <- fdj %>%
      janitor::clean_names() %>% 
      filter(.[R] == 1) %>%
      #select(stage, race, year, class, date, best_rnk) %>%
      
      gather(rider, present, -c(stage, race, year, class, date, best_rnk)) %>% 
      
      group_by(rider) %>% 
      summarize(in_race = mean(present, na.rm = T),
                wins = mean(present == 1 & best_rnk == 1, na.rm = T),
                races = n_distinct(stage, race, year, class, date)) %>% 
      ungroup() %>%
      
      mutate(master_team = poss_teams$master_team[[t]],
             matched_with = riders_to_look$rider[[r]])
    
  }
  
  all_r_list[[t]] <- bind_rows(r_results)
  
  matched_list[[t]] <- fdj
  
}

#

all_matched_riders <- bind_rows(all_r_list) %>%
  filter(rider != str_replace_all(
    str_replace_all(
      str_replace_all(
        str_to_lower(
          str_replace_all(matched_with, " ", "_")), "'", ""), "-","_"),"\\?","_"))

#

top_sprinters_teammmates <- all_matched_riders %>%
  
  inner_join(top_sprinters %>% 
               filter(Date > '2020-01-01') %>% 
               group_by(rider) %>% 
               summarize(sprint_ability = mean(prediction, na.rm = T)) %>% 
               ungroup(), by = c("matched_with" = "rider")) %>%
  
  mutate(wins = (wins*races)/(races*in_race))

#
#
#
#
#

fdj %>%
  janitor::clean_names() %>% 
  
  group_by(theuns_edward, stuyven_jasper, mullen_ryan, pedersen_mads, kirsch_alex) %>% 
  summarize(n = n(), 
            wins = mean(best_rnk == 1)) %>% 
  ungroup() -> fdj_x

#