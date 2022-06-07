
library(tidyverse)
library(lme4)
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

All_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf
                       WHERE time_trial = 0 AND team_time_trial = 0 AND year >= 2014") %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  
  mutate(points_per_opp = ifelse(tm_pos == 1, points_finish, NA),
         sof_per_opp = ifelse(tm_pos == 1, sof, NA),
         pred_climb_diff_opp = ifelse(tm_pos == 1, pred_climb_difficulty, NA),
         pred_climb_diff_succ = ifelse(points_finish > 0, pred_climb_difficulty, NA),
         team_ldr = ifelse(tm_pos == 1, 1, 0)) %>%
  
  mutate(date = as.Date(date)) %>%
  
  select(-speed, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th,
         -time_trial, -gc_winner, -gc_pos, -parcours_value, -stage_type) %>%
  
  filter((class %in% c("2.HC", "2.Pro", "2.UWT", "1.UWT", "1.HC", "1.Pro", "WT", "WC", "CC", "Olympics")) |
           (class %in% c("2.1", "1.1") & tour == "Europe Tour") | 
           (sof > 0.2 & class %in% c("2.2", "1.2", "2.2U", "1.2U", "2.Ncup", "1.Ncup", "JR")) |
           (sof > 0.1 & !class %in% c("2.2", "1.2", "2.2U", "1.2U", "2.Ncup", "1.Ncup", "JR"))) %>%
  unique()

#
#
#


# Model using SOP ---------------------------------------------------------

predictions_using_logrk_model <- All_data %>%
  
  select(-gain_gc, -total_vert_gain, -uphill_finish, -cobbles,
         -avg_alt, -points_finish,
         -success, -missing_profile_data, -leader_rating,
         -points_per_opp, -sof_per_opp, -pred_climb_diff_opp,
         -pred_climb_diff_succ) %>%
  
  filter(bunch_sprint == 1 & tm_pos == 1 & rnk <= 25) %>%
  
  mutate(predicted_bs = 1) %>%
  
  mutate(stage_join = as.character(stage)) %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT DISTINCT date FROM lme4_rider_logranks WHERE test_or_prod = 'prod'") %>%
      mutate(date = as.Date(date)), by = c("date")
    
  ) %>%
  
  unique() %>%
  
  left_join(
    
    dbReadTable(con, "lme4_rider_logranks")  %>%
      filter(test_or_prod == "prod") %>%
      select(-test_or_prod) %>%
      unique() %>%
      mutate(rider = str_to_title(rider)) %>%
      mutate(date = as.Date(date)) %>%
      
      filter(!is.na(one_day_race)) %>%
      
      mutate(level_data = ifelse(is.na(pcd_impact), "just_rider",
                                 ifelse(is.na(bunchsprint_impact), "pcd_added",
                                        ifelse(is.na(one_day_race), "bs_added", "odr_added")))) %>%
      
      # the standard deviations of random intercept and pcd impact both vary widely (increase as you move from 2015 to 2020)
      # we adjust here
      group_by(date, level_data) %>%
      mutate(pcd_impact_new = (pcd_impact - mean(pcd_impact, na.rm = T)) / sd(pcd_impact, na.rm = T),
             random_intercept_new = (random_intercept - mean(random_intercept, na.rm = T)) / sd(random_intercept, na.rm = T)) %>%
      ungroup() %>%
      
      # this transforms them back to input into the regression equation
      mutate(pcd_impact = pcd_impact_new * sd(pcd_impact, na.rm = T),
             random_intercept = random_intercept_new * sd(random_intercept, na.rm = T)) %>%
      
      select(-pcd_impact_new, -random_intercept_new) %>%
      
      rename(pcd_logrk_impact = pcd_impact,
             bs_logrk_impact = bunchsprint_impact,
             rand_logrk_impact = random_intercept,
             odr_logrk_impact = one_day_race) %>%
      
      mutate(pcd_logrk_impact = ifelse(is.na(pcd_logrk_impact), 0, pcd_logrk_impact),
             bs_logrk_impact = ifelse(is.na(bs_logrk_impact), 0, bs_logrk_impact),
             odr_logrk_impact = ifelse(is.na(odr_logrk_impact), 0, odr_logrk_impact)
      ) %>%
      
      filter(date >= as.Date('2016-07-01')), by = c("rider", "date")
    
  ) %>%
  
  # 0.97 is SD and 3.9 is Mean -- intercept is -0.6 and sof is 1.15 with an avg of 0.36 so -0.2 is left-over
  mutate(pred_rank = exp(-0.2 + ((
    ((rand_logrk_impact + 
        (predicted_bs * bs_logrk_impact) + 
        (one_day_race * odr_logrk_impact) + 
        (pcd_logrk_impact * 2))*-1) / 0.97)+3.9))) %>%
  
  mutate(normal_pred_rank = exp((rand_logrk_impact + 
                                (1 * bs_logrk_impact) + 
                                (one_day_race * odr_logrk_impact) + 
                                (pcd_logrk_impact * pred_climb_difficulty) + 3.8)),
         elite_sprinter = exp((-3 + (0.1 * pred_climb_difficulty) + 3.8))) %>%

  select(-rand_logrk_impact, -pcd_logrk_impact, -bs_logrk_impact, 
       -odr_logrk_impact) %>%
  
  unique() %>%
  
  mutate(rider_match = str_to_title(rider)) %>%
  
  mutate(class_small = case_when(class %in% c("2.1", "1.1", "CC") ~ ".1", 
                                 class %in% c("2.2", "1.2") ~ ".2", 
                                 class %in% c("WC", "2.UWT", "1.UWT") ~ "WT", 
                                 class %in% c("1.Pro", "1.HC", "2.Pro", "2.HC") ~ ".Pro", 
                                 TRUE ~ class))

#
#
#

who_did_you_race_against <- predictions_using_logrk_model %>%
  
  # pcd impacts change how "good" sprinter is predicted to be
  mutate(elite_sprinter = exp((-3 + (0.1 * pred_climb_difficulty) + 3.8)),
         adjust_to_elite = normal_pred_rank / elite_sprinter * mean(elite_sprinter, na.rm = T)) %>%
  
  arrange(rnk) %>%
  
  group_by(stage, race, year, class, date, length) %>%
  mutate(sof_log_rk = mean(1 / adjust_to_elite, na.rm = T),
         sum_sof_log_rk = sum(1 / adjust_to_elite, na.rm = T),
         sum_sof_log_rk = sum_sof_log_rk - (1 / adjust_to_elite),
         sof_log_rk = ((sof_log_rk*n()) - (1/adjust_to_elite)) / (n()-1),
         new_rnk = rank(rnk, ties.method = "first")) %>%
  ungroup()

#
#
#

All_dates <- All_data %>%
  select(date) %>%
  unique() %>%
  mutate(date = as.Date(date, origin = '1970-01-01')) %>%
  arrange(desc(date))

Dates_to_use <- All_dates %>%
  anti_join(dbGetQuery(con, "SELECT DISTINCT date FROM lme4_rider_sprintlevel2_logranks") %>%
              mutate(date = as.Date(date))) %>%
  filter(date > "2017-03-31") %>%
  rbind(tibble(date = as.Date(lubridate::today()+1))) %>%
  arrange(desc(date))

#
#
#

for(b in 1:length(Dates_to_use$date)) {
  
  if(Dates_to_use$date[[b]] > '2016-06-30') {
    
    # go back 4 months further in 2020 post-lockdown
    
    if(Dates_to_use$date[[b]] > '2020-04-01' & Dates_to_use$date[[b]] < '2022-08-01') {
      
      howfar = 120
      
    } else {
      
      howfar = 0
      
    }
    
    # one day before predicting date and two years back
    maxD <- as.Date(Dates_to_use$date[[b]]) - 1
    minD <- maxD - 730 - howfar
    
    # set-up different time length data-sets
    
    dx <- who_did_you_race_against %>%
      
      filter(between(date, minD, maxD)==TRUE) %>%
      
      mutate(rnk1 = ifelse(new_rnk >= 200, NA, rnk)) %>%
      
      group_by(stage, race, year, class, date) %>%
      mutate(rnk = ifelse(new_rnk==200, max(rnk1, na.rm = T)+1, new_rnk)) %>%
      ungroup() %>%
      
      mutate(new_log_rnk = log(rnk) + (-2.14 * (sof-0.5))) %>%
      
      mutate(avg_sof_limit = mean(sof_limit, na.rm = T)) %>%
      
      mutate(rel_rnk = rnk / (sof_limit / avg_sof_limit)) %>%
      
      mutate(log_rnk = log(rel_rnk),
             log_rnk1 = log(rnk)) %>%
      
      # SD = 0.98
      # MN = 3.81
      
      group_by(race, class, stage, year, date) %>%
      mutate(log_rnk = (log_rnk - mean(log_rnk1, na.rm = T)) / sd(log_rnk1, na.rm = T) * -1) %>%
      ungroup() %>%
      
      mutate(win = ifelse(rnk == 1, 1, 0))
    
    #
    #
    #

    d_small <- dx %>% filter(between(date, All_dates$date[[b+180]], maxD)==TRUE)
    
    #
    #
    #
    
    model <- d_small %>%
      
      group_by(rider) %>%
      filter(n() >= 5) %>%
      ungroup() %>%
      
      lme4::lmer(log(new_rnk) ~ (1 | rider) + sum_sof_log_rk,
                 
                 data = .)
    
    ranefs_regular <- lme4::ranef(model)[[1]] %>% rownames_to_column() %>%
      rename(rider = rowname, random_intercept = `(Intercept)`) %>%
      mutate(date = maxD + 1)
    
    #
    # ################################################
    #
    
    dbWriteTable(con, "lme4_rider_sprintlevel2_logranks", ranefs_regular, append = TRUE, row.names = FALSE)
    
    print(model@beta[[1]])
    
    #
    #
    #
    #
    
    print(b)
    
  }
  
}

#

dbGetQuery(con, "SELECT * FROM lme4_rider_sprintlevel2_logranks WHERE date >= '2020-01-01'") -> BSRK

BSRK %>% filter(rider == "Bennett Sam") %>% ggplot(aes(x = as.Date(date), y = random_intercept*-1))+geom_point()

#
#
#

strongest_sprint_fields <- who_did_you_race_against %>%  
  
  mutate(class_small = ifelse(race %in% c("tour de france"), "TDF", class_small)) %>%
  #mutate(class_small = ifelse(grand_tour == 1, "Grand Tour", class_small)) %>%
  
  group_by(stage, race, year, class, date, length, class_small, grand_tour) %>%
  summarize(sum_sof_log_rk = sum(1 / adjust_to_elite, na.rm = T)) %>%
  ungroup()

#

strongest_sprint_fields %>% 
  filter(class_small %in% c("WT", ".Pro", ".1", "TDF")) %>% 
  filter(year >= 2020) %>%
  
  group_by(class_small) %>%
  mutate(sof = mean(sum_sof_log_rk)) %>% 
  ungroup() %>%
  
  ggplot(aes(x = sum_sof_log_rk, fill = class_small))+
  geom_histogram(binwidth = 0.1, color = "black")+
  facet_wrap(~reorder(class_small, desc(sof)), ncol = 1, scales = "free_y")+
  guides(fill = F)+
  theme(strip.background = element_blank(), 
        strip.text = element_text(size = 15), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        panel.grid.minor = element_blank())+
  labs(x = "Strength of Sprint Field", 
       y = "", 
       title = 'Distribution of sprint fields by Class')+
  
  scale_fill_manual(values = c("gray70", "#00539F", "gold", "#37B36C"), guide = F)

#

model <- who_did_you_race_against %>%
  
  filter(date > '2020-04-01') %>%
  
  group_by(rider) %>%
  filter(n() >= 16) %>%
  ungroup() %>%
  
  mutate(wt = 1 / (10 + as.numeric(lubridate::today() - date))) %>%
  
  lme4::lmer(log(new_rnk) ~ (1 | rider) + sum_sof_log_rk,
             
             #weights = wt,
             
             data = .)
  
ranefs_regular <- lme4::ranef(model)[[1]] %>% rownames_to_column()

#

sprint_stats <- who_did_you_race_against %>% 
  filter(year >= 2020) %>% 
  
  inner_join(ranefs_regular %>%
               filter(rank(`(Intercept)`, ties.method = "min") <= 20), by = c("rider" = "rowname")) %>% 
  
  group_by(rider) %>% 
  summarize(fields_faced = mean(sum_sof_log_rk, na.rm = T),
            sprints_as_leader = n(),
            wins = sum(rnk == 1, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(win_rate = wins / sprints_as_leader)

#

sprint_stats %>% 
  ggplot(aes(x = fields_faced, y = win_rate, label = rider, color = (rider == "Jakobsen Fabio")))+
  geom_point(size=3)+
  
  geom_vline(xintercept = c(1.07,1.86), alpha = 0.33, size = 2)+
  
  ggrepel::geom_text_repel()+
  labs(x = "Quality of sprinters faced", y = "Win Rate",
       title = "Top Sprinters (2020-22)", 
       subtitle = "...in bunch sprints, as leader, contesting sprint")+
  
  scale_y_continuous(labels = scales::percent)+
  
  theme(axis.text = element_text(size = 14), 
        plot.title = element_text(size = 20))+
  
  scale_color_manual(values = c("black", "#00539F"), guide = FALSE)

#
#
#

gam_ranefs <- who_did_you_race_against %>%
  
  filter(date > '2020-01-01') %>%
  
  group_by(rider) %>%
  filter(n() >= 16) %>%
  ungroup() %>%
  
  mutate(win = ifelse(rnk == 1, 1, 0)) %>%
  
  select(rider, win, sum_sof_log_rk, race, stage, year, class, date, rnk, class_small) %>%
  
  mutate(in_race = 1) %>%
  
  mgcv::gam(win ~ rider + s(sum_sof_log_rk, k = 5), 
            data = .,
            family = "binomial")

#

summary(gam_ranefs)

#

predicted_data <- tibble(rider = gam_ranefs$model$rider %>% unique()) %>%
  
  expand_grid(sum_sof_log_rk = seq(0.5,4,0.5)) %>%
  
  mutate(coef = mgcv::predict.gam(gam_ranefs, .),
         predicted_win = exp(coef)/(1+exp(coef)))

#
#
#

gam_log_rank <- who_did_you_race_against %>%
  
  filter(date > '2020-01-01') %>%
  
  group_by(rider) %>%
  filter(n() >= 16) %>%
  ungroup() %>%
  
  mutate(win = ifelse(rnk == 1, 1, 0)) %>%
  
  select(rider, win, sum_sof_log_rk, race, stage, year, class, date, rnk, class_small) %>%
  
  mutate(in_race = 1) %>%
  
  mgcv::gam(log(rnk) ~ rider + s(sum_sof_log_rk, k = 5), 
            data = .)

#

summary(gam_log_rank)

#

predicted_ranks <- who_did_you_race_against %>%
  
  filter(date > '2020-01-01') %>%
  
  group_by(rider) %>%
  filter(n() >= 16) %>%
  ungroup() %>%
  
  select(rider) %>%
  unique() %>%
  
  expand_grid(sum_sof_log_rk = seq(0.5,4,0.5)) %>%
  
  mutate(coef = mgcv::predict.gam(gam_log_rank, .),
         predicted_rank = exp(coef))

# H2H Model Part ----------------------------------------------------------

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
  filter(rnk < 31) %>%
  filter(team_ldr == 1) %>%
  mutate(master_team = ifelse(master_team == "X", team, master_team))

#
#
#

sprint_h2h <- sprint_h2h_data %>%
  
  filter(year >= 2019) %>%
  
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
  filter(!team1 == team2) %>%
  
  mutate(adv1 = ifelse(rnk1 < rnk2, 1, 0),
         adv2 = ifelse(rnk1 > rnk2, 1, 0)) %>%
  
  group_by(rider1) %>%
  mutate(r1_n = n()) %>%
  ungroup() %>%
  
  group_by(rider2) %>%
  mutate(r2_n = n()) %>%
  ungroup() %>%
  
  filter(!is.na(date)) %>%
  
  unique() %>%
  
  inner_join(
  
    dbGetQuery(con, "SELECT rider, Date, (random_intercept + bunchsprint_impact) as BS_impact1, pcd_impact as pcd_impact1
               FROM lme4_rider_logranks
               WHERE Date > '2019-01-01'") %>%
      mutate(BS_impact1 = BS_impact1 + 4.35,
             Date = as.Date(Date)), by = c("rider1" = "rider", "date" = "Date")) %>%
  
  mutate(BS_impact1 = BS_impact1 + (pred_climb_difficulty*pcd_impact1)) %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT rider, Date, (random_intercept + bunchsprint_impact) as BS_impact2, pcd_impact as pcd_impact2
               FROM lme4_rider_logranks
               WHERE Date > '2019-01-01'") %>%
      mutate(BS_impact2 = BS_impact2 + 4.35,
             Date = as.Date(Date)), by = c("rider2" = "rider", "date" = "Date")) %>%
  
  mutate(BS_impact2 = BS_impact2 + (pred_climb_difficulty*pcd_impact2))

#
#
#

h2h_glmer <- glmer(adv1 ~ (1 | rider1) + BS_impact2,
                   data = sprint_h2h %>% 
                     filter(date > '2021-01-01') %>%
                     group_by(rider1) %>% 
                     filter(n_distinct(stage, race, class, year) > 5) %>% 
                     ungroup(),
                   family = "binomial")

summary(h2h_glmer)

ranefs <- ranef(h2h_glmer)[[1]] %>%
  rownames_to_column() %>%
  janitor::clean_names() %>%
  rename(rider = rowname)

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

teammate_data <- All_data %>%
  
  filter(bunch_sprint == 1) %>%
  filter(!is.na(rnk)) %>%
  
  # only take those within 5 seconds of winner
  filter(gain_1st <= 5) %>%
  
  select(date, race, stage, year, team=master_team, rider) %>%
  unique()

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
                             years = c('2016', '2017', '2018', '2019', '2020', '2021')) %>%
  
  mutate(days = ifelse(months == "11", '10',
                       ifelse(months == '01', "15", '01'))) %>%
  
  mutate(date = as.Date(paste0(years, "-", months, "-", days))) %>%
  
  filter(date < '2020-04-01' | date > '2020-07-15') %>%
  
  filter(!date %in% as.Date(c("2019-11-10", "2018-11-10", "2017-11-10", "2016-11-10"))) %>%
  
  filter(date < '2021-07-01')

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
  
  # team_data <- sprint_h2h_team %>%
  #   filter(date < cutoff & date >= (cutoff - start)) %>%
  #   group_by(team1) %>%
  #   mutate(r1_n = n()) %>%
  #   ungroup() %>%
  #   group_by(team2) %>%
  #   mutate(r2_n = n()) %>%
  #   ungroup() %>%
  #   filter((r1_n >= 1000 & r2_n >= 1000))
  
  #
  
  # tm_bs_glmer <- glmer(adv1 ~ (1 | team1) + (1 | team2), 
  #                    data = team_data,
  #                    family = binomial("logit"),
  #                    nAGQ=0,
  #                    control=lme4::glmerControl(optimizer = "nloptwrap"))
  
  # tm_ranefs <- ranef(tm_bs_glmer)[[1]] %>%
  #   rownames_to_column() %>%
  #   janitor::clean_names() %>%
  #   rename(team = rowname) %>%
  #   mutate(pred_date = cutoff)
  
  #
  
  # tm_list[[y]] <- tm_ranefs
  
  #
  
  training <- sprint_h2h %>%
    filter(date < cutoff & date >= (cutoff - start)) %>%
    group_by(rider1) %>%
    mutate(r1_n = n()) %>%
    ungroup() %>%
    group_by(rider2) %>%
    mutate(r2_n = n()) %>%
    ungroup() %>%
    filter((r1_n >= 100 & r2_n >= 100))
  
  #training <- cbind(training,
  #                  pred_team = predict(tm_bs_glmer, training, allow.new.levels = TRUE)) %>%
  #  mutate(pred_team = exp(pred_team)/(1+exp(pred_team)))

  #
  
    testing <- sprint_h2h %>%
      
      filter(date >= cutoff & date < (cutoff + end)) %>%
      
      group_by(rider1) %>%
      mutate(r1_n = n()) %>%
      ungroup() %>%
      
      group_by(rider2) %>%
      mutate(r2_n = n()) %>%
      ungroup() %>%
      
      filter((r1_n >= 50 & r2_n >= 50))

  #
  
  tictoc::tic()
  
   # h2h_glmer <- glmer(adv1 ~ (1 | rider1) + (1 | rider2) + pred_team, 
   #                    data = training,
   #                    family = binomial("logit"),
   #                    nAGQ=0,
   #                    control=lme4::glmerControl(optimizer = "nloptwrap"))

  # ranefs <- ranef(h2h_glmer)[[1]] %>%
  #   rownames_to_column() %>%
  #   janitor::clean_names() %>%
  #   rename(rider = rowname) %>%
  #   mutate(pred_date = cutoff)

  h2h_glmer <- glmer(adv1 ~ (1 | rider1) + (1 | rider2), 
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
  
  if(cutoff > '2020-12-01') {
    
  } else {
    
    # testing <- cbind(testing,
    #                  pred_team = predict(tm_bs_glmer, testing, allow.new.levels = TRUE)) %>%
    #   mutate(pred_team = exp(pred_team)/(1+exp(pred_team)))
    
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
  
  filter(pred_date < '2020-12-01')
  
# model is fairly well calibrated, but could benefit from regression at tails
# regress 10% to 50% using regr_prob below and it is perfectly matched

model_results %>% 
  mutate(regr_prob = ((prob * 8)+(0.5))/9) %>%
  
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
  mutate(regr_prob = ((prob * 8)+(0.5))/9) %>%
  
  summarize(BRIER_RAW = mean((adv1-prob)^2),
            BRIER_REGR = mean((adv1-regr_prob)^2))

#
#
#

rider_results <- bind_rows(ranef_list) %>%
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
  
  # team_data <- sprint_h2h_team %>%
  #   
  #   filter(date < cutoff & date >= (cutoff - start)) %>%
  #   
  #   group_by(team1) %>%
  #   mutate(r1_n = n()) %>%
  #   ungroup() %>%
  #   
  #   group_by(team2) %>%
  #   mutate(r2_n = n()) %>%
  #   ungroup() %>%
  #   
  #   filter((r1_n >= 1000 & r2_n >= 1000))
  # 
  # #
  # 
  # tm_bs_glmer <- glmer(adv1 ~ (1 | team1) + (1 | team2), 
  #                      data = team_data,
  #                      family = binomial("logit"),
  #                      nAGQ=0,
  #                      control=lme4::glmerControl(optimizer = "nloptwrap"))
  # 
  # tm_ranefs <- ranef(tm_bs_glmer)[[1]] %>%
  #   rownames_to_column() %>%
  #   janitor::clean_names() %>%
  #   rename(team = rowname) %>%
  #   
  #   mutate(pred_date = cutoff)
  # 
  # #
  # 
  # tm_list[[y]] <- tm_ranefs
  
  #
  
  training <- sprint_h2h %>%
    
    filter(date < cutoff & date >= (cutoff - start)) %>%
    
    group_by(rider1) %>%
    mutate(r1_n = n()) %>%
    ungroup() %>%
    
    group_by(rider2) %>%
    mutate(r2_n = n()) %>%
    ungroup() %>%
    
    filter((r1_n >= 100 & r2_n >= 100))
  
  # training <- cbind(training,
  #                   
  #                   pred_team = predict(tm_bs_glmer, training, allow.new.levels = TRUE)) %>%
  #   
  #   mutate(pred_team = exp(pred_team)/(1+exp(pred_team)))

  #
  
  h2h_glmer <- glmer(adv1 ~ (1 | rider1) + (1 | rider2), #+ pred_team, 
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
  
  left_join(dbGetQuery(con, "SELECT date, rider, intercept, prob_vs_top10
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

#
#
#
#
#

strength_of_sprinting <- BS_data %>%
  
  group_by(stage, race, year, class, date) %>%
  mutate(bs_rank = rank(-intercept, ties.method = 'min')) %>%
  filter(sum(!is.na(intercept)) >= 10) %>%
  ungroup() %>%
  
  mutate(best_intercept = ifelse(bs_rank == 1, intercept, NA),
         win_intercept = ifelse(rnk == 1, is.na(intercept), NA)) %>%
  
  group_by(stage, race, year, class, date, team) %>%
  filter(bs_rank == min(bs_rank, ties.method = "first")) %>%
  ungroup() %>%
  
  group_by(race, year, stage, class, date) %>%
  summarize(sprinter_points = sum(prob_vs_top10, na.rm = T)) %>%
  ungroup()


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

sprint_h2h <- sprint_h2h %>%
  
  select(rider1, rider2, adv1, adv2, date, race, stage, year, r1_n, r2_n, rnk1, rnk2, team1, team2) %>%
  
  group_by(team1, stage, race, year, date) %>% 
  mutate(tm_best = rank(rnk1, ties.method = "min")) %>%
  mutate(tmldr1 = ifelse(tm_best == min(tm_best, na.rm = T), 1, 0)) %>% 
  ungroup() %>% 
  
  group_by(team2, stage, race, year, date) %>% 
  mutate(tm_best = rank(rnk2, ties.method = "min")) %>%
  mutate(tmldr2 = ifelse(tm_best == min(tm_best, na.rm = T), 1, 0)) %>% 
  ungroup() %>%
  
  mutate(type = ifelse(tmldr1 == 1,
                       ifelse(tmldr2 == 1, "both leaders", 'not matched'),
                       ifelse(tmldr2 == 1, "not matched", "both helpers"))) %>%
  
  filter(type == "both leaders") %>%
  
  select(-type, -tm_best)

#

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
  
  filter(year > 2015 & year <= 2019) %>%
  select(date) %>%
  unique() %>%
  filter(!is.na(date)) %>%
  
  mutate(date = as.Date(date)) %>%
  
  rbind(tibble(date = as.Date(lubridate::today()))) %>%
  
  mutate(date = as.Date(date, origin = '1970-01-01')) %>%
  
  group_by(m = lubridate::month(date), y = lubridate::year(date)) %>% 
  filter(rank(m, ties.method = "random")==1) %>% 
  ungroup() %>% 
  filter(m < 10 & m > 1)

#

tictoc::tic()

df_list <- vector("list", length(dates_to_pull$date))
tm_list <- vector("list", length(dates_to_pull$date))
mod_list <- vector("list", length(dates_to_pull$date))

# for loops by year

for(y in 1:length(df_list)) {
  
  cutoff <- dates_to_pull$date[[y]]
  
  end = 366
  
  start = 731
  
  # find who will have teammates to match with later on
  
  sprint_h2h %>%
    
    filter(date < cutoff & date >= (cutoff - start)) %>%
    
    group_by(rider1) %>%
    mutate(r1_n = n()) %>%
    ungroup() %>%
    
    group_by(rider2) %>%
    mutate(r2_n = n()) %>%
    ungroup() %>%
    
    filter((r1_n >= 250 & r2_n >= 250)) %>%
    
    select(team1) %>%
    unique() -> valid_teams_train
  
  sprint_h2h %>%
    
    filter(date >= cutoff & date < (cutoff + end)) %>%
    
    group_by(rider1) %>%
    mutate(r1_n = n()) %>%
    ungroup() %>%
    
    group_by(rider2) %>%
    mutate(r2_n = n()) %>%
    ungroup() %>%
    
    filter((r1_n >= 250 & r2_n >= 250)) %>%
    
    select(team1) %>%
    unique()-> valid_teams_test

  # find correct teammates in race with required # of appearances in final of BS
  
  tmate_df_train <- teammate_data %>%
    
    filter(date < cutoff & date >= (cutoff - start)) %>%
    
    group_by(rider) %>%
    filter(n() >= 30) %>%
    ungroup() %>%
    
    mutate(inrace = 1) %>%
    filter(team %in% valid_teams_test$team1 & team %in% valid_teams_train$team1)
  
  tmate_df_test <- teammate_data %>%
    
    filter(date >= cutoff & date < (cutoff + end)) %>%
    
    filter(rider %in% 
             (tmate_df_train$rider)) %>%
    
    mutate(inrace = 1) %>%
    filter(team %in% valid_teams_test$team1 & team %in% valid_teams_train$team1)
  
  tmate_df_train <- tmate_df_train %>%
    
    filter(rider %in%
             (tmate_df_test$rider))

  # and combine with each teammate in top 30 of BS
  
  impact_model_test <- rbind(
    
    sprint_h2h %>%
      
      filter(date >= cutoff & date < (cutoff + end)) %>%
      
      group_by(rider1) %>%
      mutate(r1_n = n()) %>%
      ungroup() %>%
      
      group_by(rider2) %>%
      mutate(r2_n = n()) %>%
      ungroup() %>%
      
      filter((r1_n >= 250 & r2_n >= 250)) %>%
      
      left_join(tmate_df_test, by = c("race", "stage", 'year', 'date', 'team1' = "team")) %>%
      mutate(rider = ifelse(is.na(rider), "No teammates", rider),
             inrace = ifelse(is.na(inrace), 1, inrace)),
    
    sprint_h2h %>%
      
      filter(date >= cutoff & date < (cutoff + end)) %>%
      
      group_by(rider1) %>%
      mutate(r1_n = n()) %>%
      ungroup() %>%
      
      group_by(rider2) %>%
      mutate(r2_n = n()) %>%
      ungroup() %>%
      
      filter((r1_n >= 250 & r2_n >= 250)) %>%
      
      left_join(tmate_df_test, by = c("race", "stage", 'year', 'date', 'team2' = "team")) %>%
      mutate(rider = ifelse(is.na(rider), "No teammates2", rider),
             inrace = ifelse(is.na(inrace), 1, inrace)) %>%
      mutate(inrace = ifelse(inrace == 1, -1, inrace))) %>%
    
    unique()
  
  #
  
  impact_model_train <- rbind(
    
    sprint_h2h %>%
      
      filter(date < cutoff & date >= (cutoff - start)) %>%
      
      group_by(rider1) %>%
      mutate(r1_n = n()) %>%
      ungroup() %>%
      
      group_by(rider2) %>%
      mutate(r2_n = n()) %>%
      ungroup() %>%
      
      filter((r1_n >= 250 & r2_n >= 250)) %>%
      
      left_join(tmate_df_train, by = c("race", "stage", 'year', 'date', 'team1' = "team")) %>%
      mutate(rider = ifelse(is.na(rider), "No teammates", rider),
             inrace = ifelse(is.na(inrace), 1, inrace)),
    
    sprint_h2h %>%
      
      filter(date < cutoff & date >= (cutoff - start)) %>%
      
      group_by(rider1) %>%
      mutate(r1_n = n()) %>%
      ungroup() %>%
      
      group_by(rider2) %>%
      mutate(r2_n = n()) %>%
      ungroup() %>%
      
      filter((r1_n >= 250 & r2_n >= 250)) %>%
      
      left_join(tmate_df_train, by = c("race", "stage", 'year', 'date', 'team2' = "team")) %>%
      mutate(rider = ifelse(is.na(rider), "No teammates2", rider),
             inrace = ifelse(is.na(inrace), 1, inrace)) %>%
      mutate(inrace = ifelse(inrace == 1, -1, inrace))) %>% 
    
    unique() %>%
    
    filter(rider %in% impact_model_test$rider)
  
  #
  
  impact_model_test <- impact_model_test %>%
    
    filter(rider %in% impact_model_train$rider) %>%
    
    spread(rider, inrace) %>%
    
    filter(str_sub(rider1,1,1) > str_sub(rider2,1,1))
  
  impact_model_train <- impact_model_train %>%

    spread(rider, inrace) %>%
    
    filter(str_sub(rider1,1,1) > str_sub(rider2,1,1))
  
  #
  
  x = as.matrix(impact_model_train %>% 
                  select(-rider1, -rider2, -adv1, -adv2, -date, -race, -stage, -year, -r1_n, -r2_n, -rnk1, -rnk2, -team1, -team2,
                         -tmldr1, -tmldr2))
  y_train = impact_model_train$adv1
  
  x_test = as.matrix(impact_model_test %>% 
                       select(-rider1, -rider2, -adv1, -adv2, -date, -race, -stage, -year, -r1_n, -r2_n, -rnk1, -rnk2, -team1, -team2,
                              -tmldr1, -tmldr2))
  y_test = impact_model_test$adv1
  
  x[is.na(x)] <- 0
  
  x_test[is.na(x_test)] <- 0
  
  x <- x[, colnames(x_test)]
  
  # run to find optimal lambda
  
  tictoc::tic()
  
  lambdas <- 10^seq(0, -6, by = -.25)
  
  # Setting alpha = 1 implements lasso regression
  lasso_reg <- glmnet::cv.glmnet(x, 
                                 y_train,
                                 alpha = 1, 
                                 lambda = lambdas,
                                 family = "binomial",
                                 nfolds = 5)
  
  # Best 
  lambda_best <- lasso_reg$lambda.min 
  
  # lambda around 0.0002
  
  tictoc::toc()
  
  # run the lasso model
  
  tictoc::tic()
  
  lasso_model_all <- glmnet::glmnet(x, 
                                    y_train,
                                    alpha = 1, 
                                    lambda = lambda_best, 
                                    standardize = TRUE,
                                    family = "binomial")
  
  tictoc::toc()
  
  #
  
  cbind(coef = lasso_model_all$beta@x,
        rider = lasso_model_all$beta@Dimnames[[1]][lasso_model_all$beta@i+1]) %>% 
    as_tibble() %>%
    mutate(coef = as.numeric(coef)) -> all_coefs
  
  #
  
  df_list[[y]] <- cbind(
    
    pred = predict(obj = lasso_model_all, newx = x_test),
    
    impact_model_test[, 1:16]
    
  ) %>%
    
    mutate(cutoff = cutoff)
  
  mod_list[[y]] <- lasso_model_all
  
  tm_list[[y]] <- all_coefs %>%
    
    mutate(cutoff = cutoff)
  
  #
  
  rm(x)
  rm(x_test)
  rm(y)
  rm(y_test)
  rm(impact_model_test)
  rm(impact_model_train)
  rm(tmate_df_test)
  rm(tmate_df_train)
  rm(lasso_model_all)
  rm(lasso_reg)
  
  gc()

}

tictoc::toc()

#

all_coefs <- bind_rows(tm_list)

#

all_preds <- bind_rows(df_list) %>% 
  
  mutate(prob = exp(s0)/(1+exp(s0))) # %>%
  
  # find where the sprinter ranked in team on that day
  #inner_join(sprint_h2h %>%
               
  #             group_by(team1, stage, race, year, date) %>% 
  #             mutate(tm_best = rank(rnk1, ties.method = "min")) %>%
  #             mutate(tmldr1 = ifelse(tm_best == min(tm_best, na.rm = T), 1, 0)) %>% 
  #             ungroup() %>% 
               
  #             group_by(team2, stage, race, year, date) %>% 
  #             mutate(tm_best = rank(rnk2, ties.method = "min")) %>%
  #             mutate(tmldr2 = ifelse(tm_best == min(tm_best, na.rm = T), 1, 0)) %>% 
  #             ungroup() %>%
               
  #             select(rider1, rider2, tmldr1, tmldr2, stage, race, year, date), by = c("rider1", "rider2", "stage", "race", "year", "date"))

results <- all_preds %>% 
  
  #mutate(type = ifelse(tmldr1 == 1,
  #                     ifelse(tmldr2 == 1, "both leaders", 'not matched'),
  #                     ifelse(tmldr2 == 1, "not matched", "both helpers"))) %>%
  
  group_by(f = floor(prob/0.05)) %>% #, type) %>% 
  summarize(act = mean(adv1, na.rm = T), 
            pred = mean(prob, na.rm = T), 
            n = n()) %>% 
  ungroup()

#

results %>% 
  ggplot(aes(x = pred, y = act))+
  geom_abline(slope = 1, intercept = 0)+
  geom_point(size=3)

