
library(tidyverse)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

stage_data <- dbReadTable(con, "pcs_stage_data")


# Extrapolate Climbing Data -----------------------------------------------

#
# this model predicts actual climb difficulty based on the parcours value in the PCS data
# this is good for stages that PCS rates in terms of parcours value, but which F-R doesn't have the climb
# data broken out into categorized climbs
#

stage_data %>% 
  
  filter(rnk == 1) %>% 
  
  mutate(est = ifelse(str_detect(parcours_value, "\\*"),1,0), 
         pv = as.numeric(parcours_value)) %>% 
  
  select(-gain_1st, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th, 
         -gc_seconds, -total_seconds, -win_seconds, -tm_pos, -rel_speed, 
         -top_variance, -variance, -speed) %>%
  
  filter(missing_profile_data == 0) %>% 
  filter((pv > 0 & act_climb_difficulty > 0) | 
           (pv==0 & act_climb_difficulty==0)) %>%
  
  lm(act_climb_difficulty ~ pv, data = .) %>%
  summary()

# act_climb_difficulty = 3.9 + (0.096 * parcours_value) -- R^2 = 0.68

# for F-R profile data stages we can use stage type + final 1km gradient

stage_data %>% 
  
  filter(rnk == 1) %>% 
  
  mutate(est = ifelse(str_detect(parcours_value, "\\*"),1,0), 
         pv = as.numeric(parcours_value)) %>% 
  
  select(-gain_1st, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th, 
         -gc_seconds, -total_seconds, -win_seconds, -tm_pos, -rel_speed, 
         -top_variance, -variance, -speed) %>%
  
  filter(missing_profile_data == 0 & act_climb_difficulty > 0) %>%
  filter(!stage_type == "icon profile p0") %>%
  lm(act_climb_difficulty ~ stage_type + final_1km_gradient, data = .) %>%
  summary()

# p1 = 4, p2 = 5.5, p3 = 4, p4 = 16, p5 = 21 + 25 * final_1km_grade

# and for stages w/o F-R we can use stage_type to estimate act_climb_difficulty

stage_data %>% 
  
  filter(rnk == 1) %>% 
  
  mutate(est = ifelse(str_detect(parcours_value, "\\*"),1,0), 
         pv = as.numeric(parcours_value)) %>% 
  
  select(-gain_1st, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th, 
         -gc_seconds, -total_seconds, -win_seconds, -tm_pos, -rel_speed, 
         -top_variance, -variance, -speed) %>%
  
  filter(missing_profile_data == 0 & act_climb_difficulty > 0) %>%
  filter(!stage_type == "icon profile p0") %>%
  lm(act_climb_difficulty ~ stage_type, data = .) %>%
  summary()

# p1 = 4, p2 = 5, p3 = 5, p4 = 16, p5 = 23

#

stage_data <- stage_data %>%
  select(-climbing_final_20km, -final_5km_elev, -final_5km_gradient, -final_1km_elev,
         -perc_gain_end, -time_at_1500m, -total_elev_change, -highest_point) %>%
  
  inner_join(
    stage_data %>%
      select(climbing_final_20km, final_5km_elev, final_5km_gradient, final_1km_elev,
             perc_gain_end, time_at_1500m, total_elev_change, highest_point,
             rider, stage, year, race) %>%
      group_by(rider, stage, year, race) %>%
      nest() %>%
      ungroup(), by = c("rider", "stage", "year", "race")
    
  ) %>%
  
  left_join(
    
    read_csv("master-teams.csv"), by = c("team", "year")
    
  )

#
#
# RIDER SIMILARITY
#
#

rider_matches_sim <- stage_data %>%
  
  filter(rnk < 11) %>%
  
  group_by(rider) %>%
  filter(n() > 19) %>%
  ungroup() %>%
  
  select(rider) %>%
  unique() %>%
  
  inner_join(stage_data %>%
               filter(year > 2016) %>%
               select(race, stage, year, rider, rnk, total_seconds)) %>%
  
  inner_join(stage_data %>%
               
               filter(rnk < 11) %>%
               
               group_by(rider) %>%
               filter(n() > 19) %>%
               ungroup() %>%
               
               select(rider) %>%
               unique() %>%
               
               inner_join(stage_data %>%
                            filter(year > 2016) %>%
                            select(race, stage, year, rider, rnk, total_seconds)), by = c("race", "stage", "year")) %>%
  
  filter(!is.na(rnk.x)) %>%
  filter(!is.na(rnk.y)) %>%
  
  filter((rnk.x < 21 | rnk.y < 21) | (abs(total_seconds.x - total_seconds.y) > 29)) %>%
  
  filter(!(rider.x == rider.y)) %>%
  mutate(log_diff = (log(rnk.x) - log(rnk.y))^2,
         sqrt_time = sqrt(abs(total_seconds.x - total_seconds.y))) %>%
  
  group_by(rider1 = rider.x,
           rider2 = rider.y) %>%
  summarize(avg_log_square_diff = mean(log_diff, na.rm = T),
            avg_sqrt_seconds = mean(sqrt_time, na.rm = T),
            matches = n()) %>%
  ungroup() %>%
  
  filter(matches > 14) %>%
  
  mutate(value = (avg_log_square_diff - mean(avg_log_square_diff)) / sd(avg_log_square_diff)) %>%
  
  select(rider1,rider2,value) %>%
  
  spread(rider2, value) %>%
  gather(rider2, value, -rider1) %>%
  mutate(value = ifelse(is.na(value),0,value)) %>%
  spread(rider2, value)

# run principal components

prc <- prcomp(rider_matches_sim %>%
                select(-rider1))

prc_xxx <- cbind(rider_matches_sim %>%
                   select(rider1),
                 prc$x)

# kmeans set-up
factoextra::fviz_nbclust(scale(prc_xxx[,2:4]), kmeans, method = c("silhouette"))

km_list <- vector("list", 7)

for(k in 3:9) {
  
  km <- kmeans(scale(prc_xxx[, 2:4]), centers = k)
  
  km_xxx <- cbind(prc_xxx[ , 1:4],
                  cl = km$cluster) %>%
    as_tibble() %>%
    rename(rider = rider1) %>%
    inner_join(
      
      stage_data %>%
        filter(year >2016) %>%
        group_by(rider) %>%
        summarize(n_od = mean(rnk < 11 & one_day_race == TRUE, na.rm = T),
                  n_tt = mean(rnk < 11 & time_trial == TRUE, na.rm = T),
                  n_wt = mean(rnk < 11 & class %in% c("2.UWT", "1.UWT", "WC"), na.rm=T),
                  n_gt = mean(grand_tour == 1, na.rm = T),
                  n_fl = mean(rnk < 11 & !is.na(act_climb_difficulty) & act_climb_difficulty < 6, na.rm = T),
                  n_cl = mean(rnk < 11 & !is.na(act_climb_difficulty) & act_climb_difficulty > 24, na.rm = T),
                  n_hl = mean(rnk < 11 & !is.na(act_climb_difficulty) & act_climb_difficulty <24.1 & act_climb_difficulty>5.9, na.rm = T),
                  n = mean(rnk < 11, na.rm = T)) %>%
        ungroup(), by = c("rider")
      
    ) %>%
    
    mutate(clusters = k)
  
  km_list[[k]] <- km_xxx
  
}

all_kmeans <- bind_rows(km_list)

# plot kmeans rider plot

ggplot(all_kmeans %>%
         filter(clusters > 3), aes(x = PC3, y = PC2, color = as.factor(cl)))+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_point(size = 4)+
  geom_label(data = all_kmeans %>%
               filter(clusters > 3) %>%
               group_by(cl, clusters) %>%
               filter(rank(-n, ties.method = "first")<4) %>%
               ungroup() %>%
               ungroup(), aes(x = PC3, y = PC2, label = rider))+
  facet_wrap(~clusters)

# climb / flat ability of top 10
climb_flat <- stage_data %>% 
  filter(rnk<11) %>% 
  inner_join(all_kmeans %>% 
               filter(clusters == 8) %>%
               select(rider,PC2), by = c("rider")) %>%
  group_by(stage, race, year) %>% 
  summarize(m = mean(PC2, na.rm = T), 
            n = n()) %>% 
  mutate(m = (n*m)/(n+3)) %>%
  ungroup()

# correlation of success to PC2
corr_climb_flat <- stage_data %>%
  filter(!is.na(rnk)) %>%
  inner_join(all_kmeans %>% 
               filter(clusters == 8) %>%
               select(rider,PC2), by = c("rider")) %>%
  group_by(stage, race, year) %>%
  filter(n()>19) %>%
  do(broom::tidy(lm(log(rnk) ~ PC2, data = .))) %>%
  ungroup()


#
#
# STRENGTH OF FIELD
#
#

riders_stages <- stage_data %>%
  
  #filter(time_trial == FALSE) %>%
  
  filter(rnk < 11) %>%
  
  select(rider1 = rider, stage, race, year) %>%
  unique() %>%
  
  inner_join(
    
    stage_data %>%
      
      #filter(time_trial == FALSE) %>%
      
      filter(rnk < 11) %>%   
      
      select(rider2 = rider, stage, race, year) %>%
      unique(), by = c("stage", "race", "year")	
    
  ) %>%
  
  filter(!(rider1 == rider2)) %>%
  unique()

#
#
#

matches <- riders_stages %>%
  
  filter(year > 2016) %>%
  
  group_by(rider1, rider2) %>%
  summarize(n = n()) %>%
  ungroup() %>% 
  
  filter(n > 2) %>% 
  
  group_by(rider1) %>%
  mutate(n1 = n()) %>%
  ungroup() %>%
  
  group_by(rider2) %>%
  mutate(n2 = n()) %>%
  ungroup() %>%
  
  filter(n1 > 1 & n2 > 1) %>%
  
  select(rider1, rider2, n) %>%
  
  spread(rider2, n) %>%
  
  gather(rider2, n, -rider1) %>%
  
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  
  spread(rider2, n)

#
# PCA
#

pxxx <- prcomp(matches[, 2:ncol(matches)], scale = TRUE, center = TRUE)

# two PCs capture 26%

# PC1 is the sprinter vs mountains classifier
# PC2 is strength of competition
# PC6 in Time trialing (tho Valverde #1 is odd)
# PC3 is like classics-esque

rxxx <- cbind(
  pxxx$x, 
  matches %>%
    select(rider1)) %>%
  select(rider1, PC1, PC2, PC3, PC4, PC6)

ggplot(rxxx, aes(x = PC1, y = PC2, label = rider1))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_label()+
  scale_fill_viridis_c()+
  labs(x = "PC1: climber vs sprinter", y = "PC2: strong field vs weak field")

#

riders_top10s <- stage_data %>%
  filter(rnk < 11) %>% 
  
  select(rider, stage, race, year) %>%
  unique() %>%
  
  inner_join(rxxx %>% 
               select(rider = rider1, PC1, PC2, PC3, PC6)) %>%
  group_by(stage, race, year) %>%
  summarize(type = sum(PC1, na.rm = T),
            sof = sum(PC2, na.rm = T),
            tt = sum(PC6, na.rm = T),
            classics = sum(PC3, na.rm = T),
            n = n()) %>%
  ungroup()

#

fields <- stage_data %>%
  
  select(stage, race, year, rider, class, grand_tour, date, rnk, time_trial) %>%
  unique() %>%
  
  inner_join(rxxx %>% 
               select(rider = rider1, PC1, PC2)) %>% 
  group_by(race, stage, year) %>% 
  filter(rank(rnk, ties.method = "first") < 26) %>%
  ungroup() %>%
  
  group_by(stage, race, year, class, grand_tour, date) %>%
  summarize(tot = mean(PC2, na.rm = T), 
            n = n()) %>%
  ungroup() %>% 
  
  separate(class, c("type", "class"), sep = ".") %>%
  
  mutate(class = ifelse(is.na(class), "UWT", class), 
         type = ifelse(type == "WT", NA, type)) %>%
  
  mutate(monument = ifelse(str_detect(race, "sanremo") | 
                             str_detect(race, "tour des flandres") | 
                             str_detect(race, "roubaix") | 
                             str_detect(race, "bastogne") | 
                             str_detect(race, "il lombardia"), TRUE, FALSE)) %>%
  
  mutate(tot = ifelse(n < 25, (tot * n) + (-1.5 * (25- n)), tot * 25))

#
#
# take in top 200 UWT riders

top_200_WT <- stage_data %>%
  
  inner_join(
    
    fields %>%
      select(stage, race, year, tot), by = c("stage", "race", "year")
    
  ) %>%
  
  #filter(tot > 25) %>%
  
  group_by(rider) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  
  filter(rank(-count, ties.method = 'min') < 201)

# take in all events

perf_by_level_data <- stage_data %>%
  
  inner_join(
    
    fields %>%
      select(stage, race, year, tot) %>%
      filter(year > 2016), by = c("stage", "race", "year")
    
  ) %>%
  
  inner_join(
    
    top_200_WT %>%
      select(rider), by = c("rider")
    
  ) %>%
  
  select(race, year, sof = tot, stage, rnk, rider, gain_1st) %>%
  
  unique()

# set up model

model_list <- vector("list", 20)
glmer_list <- vector("list", 20)

for(m in 1:20) {
  
  d <- perf_by_level_data %>%
    
    mutate(top = ifelse(rnk < (m + 1), 1, 0))
  
  glm <- glm(top ~ sof, data = d, family = binomial(link = "logit"))
  
  model_list[[m]] <- cbind(fit = glm$fitted.values, 
                           glm$data %>% filter(!is.na(rnk))) %>%
    mutate(thresh = m)
  
  #glmer <- lme4::glmer(top ~ (1 | race:year), data = d, family = binomial(link = "logit"))
  
  #glmer_list[[m]] <- summary(glmer)$coefficients %>%
  #  as_tibble()
  
}

glm_data <- bind_rows(model_list) %>%
  select(sof, fit, thresh) %>%
  unique()

#

ggplot(glm_data, aes(x = sof, y = fit, color = as.factor(thresh)))+
  geom_vline(xintercept = 175)+
  geom_hline(yintercept = 0.1)+
  geom_point(size = 1)+
  geom_label(data = glm_data %>% group_by(thresh) %>% filter(sof == min(sof, na.rm = T)) %>% ungroup(),
             aes(x = sof, y = fit, label = thresh),
             size = 4)+
  #scale_color_manual(values = c("black", "orange"), guide = F)+
  scale_y_continuous(labels = scales::percent)+
  guides(color = F)

# TDF top 8

limits <- glm_data %>% 
  filter(sof > 170 & sof < 180 & thresh == 8) %>%
  summarize(min(fit),
            max(fit),
            median(fit))

# this shows the threshold by sof for 10.7% chance (top 12 for average TDF and top 5 for average worst field)

filter(glm_data, fit > 0.105 & fit < 0.11) %>% 
  ggplot(aes(x = sof, y = thresh))+geom_point()+geom_smooth(method = "lm")+geom_smooth(color = 'red')

# this shows threshold by sof for 5% chance (top 5-6 for average TDF and top 2 for average worst field)

filter(glm_data, fit > 0.0475 & fit < 0.0525) %>% 
  ggplot(aes(x = sof, y = thresh))+geom_point()+geom_smooth(method = "lm")+geom_smooth(color = 'red')

# this shows threshold by sof for 9% chance (top 10 for average TDF and top 4 for average worst field)

filter(glm_data, fit > 0.0875 & fit < 0.0925) %>% 
  ggplot(aes(x = sof, y = thresh))+geom_point()+geom_smooth(method = "lm")+geom_smooth(color = 'red')

# this shows threshold by limits above

glm_data %>% filter(fit > (limits[, 3] - 0.003) & fit < (limits[, 3] + 0.003)) %>% 
  ggplot(aes(x = sof, y = thresh))+geom_point()+geom_smooth(method = "lm")+geom_smooth(color = 'red')

#
#
# filter to TDF top 8 and scale down or up

limits_actual <- glm_data %>%
  
  filter(fit > (limits[, 3] - 0.003) & fit < (limits[, 3] + 0.003)) %>%
  
  lm(thresh ~ sof, data = .)

#
#
# actual limits

stage_data_perf <- stage_data %>%
  
  # the limit is predicted from the limits_actual model above

  inner_join(
    
    cbind(
      
      limit = predict(limits_actual, fields %>% rename(sof = tot)),
      fields %>% select(stage, race, year, sof = tot)
      
      
    ), by = c("stage", "race", "year")
    
  ) %>%
  
  mutate(rnk = ifelse(is.na(rnk), 200, rnk)) %>%
  
  # round the limit to nearest integer
  mutate(limit = round(limit, 0)) %>%

  # success is any finish better than or equal to the limit
  mutate(success = ifelse(rnk < (limit + 1), 1, 0)) %>%
  
  # find the slowest success and scale off that
  mutate(success_time = ifelse(success == 1, total_seconds, NA)) %>%
  
  # find the slowest success
  group_by(stage, race, year) %>%
  mutate(success_time = max(success_time, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(rel_success = total_seconds - success_time)

#
#
#
#
# strength of field for successes (basically, who did you beat and how good were they?)
  
fields_success <- stage_data_perf %>%
  
  filter(success == 1) %>%
  
  select(stage, race, year, rider, class, grand_tour, date, rnk) %>%
  unique() %>%
  
  left_join(rxxx %>% 
               select(rider = rider1, PC1, PC2)) %>% 
  
  # -4 is the lowest PC2 the model produces
  mutate(PC2 = ifelse(is.na(PC2), -4, PC2)) %>%
  
  group_by(stage, race, year, class, grand_tour, date) %>%
  summarize(tot = mean(PC2, na.rm = T), 
            n = n()) %>%
  ungroup() %>% 
  
  separate(class, c("type", "class"), sep = ".") %>%
  
  mutate(class = ifelse(is.na(class), "UWT", class), 
         type = ifelse(type == "WT", NA, type)) %>%
  
  mutate(monument = ifelse(str_detect(race, "sanremo") | 
                             str_detect(race, "tour des flandres") | 
                             str_detect(race, "roubaix") | 
                             str_detect(race, "bastogne") | 
                             str_detect(race, "il lombardia"), TRUE, FALSE))

#
#
# Climbing difficulty

# consider four parts:
# 1) concentration (toughest climb difficulty)
# 2) actual climb difficulty (sum of climb difficulty)
# 3) summit finish TRUE or FALSE
# 4) last climb (final climb difficulty)

stage_climb_difficulty <- stage_data_perf %>%
  
  select(stage, race, year, summit_finish, act_climb_difficulty = raw_climb_difficulty, last_climb, concentration) %>%
  
  unique() %>%
  
  gather(stat, value, -stage, -race, -year, -summit_finish) %>%
  
  group_by(stat) %>%
  mutate(sd = sd(value, na.rm = T),
         avg = mean(value, na.rm = T)) %>%
  ungroup() %>%
  
  # 20% are summit finishes, SD = 40%
  # average stage is 10 actual climb difficulty, SD = 11 (so TDF mountains are 2 sigmas)
  # average last climb is 3.5, sd = 4 (so cat 1 final climbs are 2 sigmas)
  # average toughest climb is 4.5, sd = 5 (so HC climbs are 2 sigmas)
  
  # calculate zscores and spread out
  mutate(zscr = (value - avg) / sd) %>%
  
  select(-value, -sd, -avg) %>%
  
  spread(stat, zscr) %>%
  
  # calculate ensemble score, scaling differently depending on whether it's a summit finish
  mutate(ensemble = ifelse(summit_finish == 1, (0.5 * last_climb) + (0.3 * act_climb_difficulty) + (0.2 * concentration),
                           (0.2 * last_climb) + (0.3 * act_climb_difficulty) + (0.5 * concentration)))

#
#
# combine into stage data

stage_data_perf <- stage_data_perf %>%
  
  inner_join(
    
    stage_climb_difficulty %>%
      select(stage, race, year, climb_score = ensemble), by = c("stage", "race", "year")
    
  )

#
#
# calculate max effort as function of climb score

climbing_records_model <- read_csv("climbing-records-2017-19.csv") %>%
  janitor::clean_names() %>%
  
  rename(length = km,
         gradient = grade) %>%
  
  lm(km_per_hr ~ length * gradient, data = .)

#

setup_stage_max_effort <- stage_data_perf %>%
  
  select(stage, race, year, concentration) %>%
  
  unique() %>%
  
    left_join(
      
      dbReadTable(con, "flamme_rouge_climbs") %>%
        mutate(year = as.numeric(year)) %>%
        
        mutate(race = tolower(race)) %>%
        
        left_join(dbReadTable(con, "flamme_rouge_characteristics") %>%
                    mutate(year = as.numeric(year)) %>%
                    select(race, stage, year, stage_length = length) %>%
                    mutate(race = tolower(race)), by = c("race", "stage", "year")) %>%
        
        group_by(stage, race, year) %>%
        mutate(position_highest = max(model_category, na.rm = T),
               last_climb = max(end_distance, na.rm = T)) %>%
        ungroup() %>%
        
        mutate(position_highest = ifelse(position_highest == model_category, end_distance / stage_length, NA),
               last_climb = ifelse(last_climb == end_distance, model_category, NA)) %>%
        
        mutate(summit_finish = ifelse(abs(end_distance - stage_length) < 4, TRUE, FALSE)) %>%
        mutate(summit_finish = ifelse(race == "tour de romandie" & stage == 4 &
                                        year == 2019 & climb_name == "Torgon", TRUE, summit_finish)) %>%
        
        # increase KOM points by 25% if summit finish
        mutate(basic_kom_points = model_category,
               kom_points = ifelse(summit_finish == TRUE, model_category * 1.25, model_category)) %>%
      
      group_by(stage, race, year) %>%
      filter(kom_points == max(kom_points, na.rm = T)) %>%
      ungroup(), by = c("stage", "race", "year")) %>%
  
  select(stage, race, year, concentration, length, gradient)

#

stage_max_effort <- cbind(
  
  setup_stage_max_effort,
  
  pred = predict(climbing_records_model, setup_stage_max_effort)
  
) %>%
  
  filter(!is.na(length)) %>%
  
  mutate(climbing_time = (length / pred) * 60) %>%
  
  select(-concentration) %>%
  
  inner_join(
    
    stage_climb_difficulty, by = c("stage", "race", "year")
    
  )

# stage max effort model (eg, translate climb_score to expected climbing time)
# eg, this stage has a 15 minute max effort

max_eff_mod <- lm(climbing_time ~ ensemble, data = stage_max_effort)

# re-combine with stage_data_perf

stage_data_perf <- stage_data_perf %>%
  
  left_join(
    
    cbind(
      
      stage_max_effort %>%
        select(-pred, -length, -gradient, -summit_finish, -act_climb_difficulty, -last_climb, -concentration),
      
      pred = predict(max_eff_mod, stage_max_effort)
      
    ) %>%
      select(stage, race, year, pred), by = c("stage", "race", "year")) %>%
  
  mutate(max_effort = ifelse(is.na(pred), 1, pred)) %>%
  
  select(-pred) %>%
  
  unique() %>%
  
  select(-gain_1st, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th)

#
#
# rider climbing performance

library(brms)

in_contact <- stage_data_perf %>%
  
  mutate(rnk = ifelse(is.na(rnk), 150, rnk)) %>%
   
  mutate(W = ifelse(rnk == 1,1,0), 
         T3 = ifelse(rnk < 4,1,0),
         T5 = ifelse(rnk < 6,1,0),
         x2_5 = ifelse(rnk > 1 & rnk < 6,1,0),
         SUCC = success) %>% 
  
  group_by(rider) %>% 
  filter(n() > 49) %>%
  ungroup() %>%
  
  gather(category, result, (W:SUCC))

#

mod_gam <- brm(
  
  # does this model contain a random intercept for rider or just a random slope?
  bf(result ~ 
       s(max_effort, k = 5, by = rider) + 1),
  
  data = in_contact %>%
    filter(category == "SUCC"),
  
  family = bernoulli(),
  
  iter = 5000,
  warmup = 1000,
  chains = 1,
  cores = 1)



model <- lme4::glmer(result ~ (1 + max_effort | rider) + 1,
                      
                      family = binomial("logit"),
                      
                      data = in_contact %>%
                        filter(category == "SUCC"))

#

mod4 <- gamm4::gamm4(result ~ s(max_effort, k = 5) + 1,
                     
                     random = ~ (1 + max_effort | rider),
                     
                     family = binomial("logit"),
                     
                     data = in_contact %>%
                       filter(category == "SUCC"))

#

preds4 <- expand.grid(rider = ranef$rowname,
                      max_effort = c(0,10,20,30,40,50)) %>%
  
  cbind(
    
    coef = predict(mod4$gam,
            expand.grid(rider = ranef$rowname,
                        max_effort = c(0,10,20,30,40,50)))
    
  ) %>%
  
  mutate(pred = exp(coef)/(1+exp(coef)))

  
#

summary(mod_gam)
  
ranefs <- brms::ranef(mod_gam) %>%
  .[[1]] %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  janitor::clean_names() %>%
  mutate(prob = exp(estimate_intercept - 4.13)/(1+exp(estimate_intercept - 4.13)))
  

#

R = "Mollema Bauke"

feed_in <- stage_data_perf %>%
  
  mutate(rnk = ifelse(is.na(rnk), 150, rnk)) %>%
  
  mutate(W = ifelse(rnk == 1,1,0), 
         T3 = ifelse(rnk < 4,1,0),
         T5 = ifelse(rnk < 6,1,0),
         x2_5 = ifelse(rnk > 1 & rnk < 6,1,0)) %>% 
  
  group_by(rider) %>% 
  filter(n() > 99) %>%
  ungroup() %>%
  
  gather(category, result, W:x2_5) %>%
  
  filter(category == "T5" & rider == R) %>%
  
  inner_join(
    
    in_contact %>%
      filter(category == "T5" & rider == R) %>%
      spread(term, estimate) %>%
      janitor::clean_names(), by = c("rider", "category")
    
  ) %>%
  
  mutate(x = (log_max_effort * log(max_effort)) + (intercept),
         pred = exp(x)/(1+exp(x)))

#

ggplot() +
  
  geom_point(data = feed_in,
             aes(x = max_effort, y = result))+
  
  geom_point(data = feed_in,
             aes(x = max_effort, y = pred), color = "red")

#
#
#
#
#
#
# 

# Seconds gained/lost by success

seconds_by_position_mod <- stage_data_perf %>%
  
  mutate(pos_vs_succ = rnk - limit) %>%
  
  filter(!is.na(rnk)) %>%
  filter(pos_vs_succ < 200) %>%
  filter(rel_success > -5000) %>%
  
  lm(rel_success ~ pos_vs_succ + max_effort:pos_vs_succ + 0, data = .)

#

seconds_gained <- stage_data_perf %>%
  
  mutate(pos_vs_succ = rnk - limit) %>%

  filter(!is.na(rnk)) %>%
  filter(pos_vs_succ < 200) %>%
  filter(rel_success > -5000) %>%
  
  select(-variance, -top_variance, -rel_speed, -gain_1st, -gain_3rd,
         -gain_5th, -gain_10th, -gain_20th, -gain_40th) %>%
  
  cbind(
    
    pred = predict(seconds_by_position_mod,
                   
                   stage_data_perf %>%
                     
                     mutate(pos_vs_succ = rnk - limit) %>%

                     filter(!is.na(rnk)) %>%
                     filter(pos_vs_succ < 200) %>%
                     filter(rel_success > -5000))
    
  )

#
#
# seconds gained aggregate

seconds_gained_total <- seconds_gained %>%
  
  mutate(pred = ifelse(pred < 0, pred, 0)) %>%
  
  unique() %>%
  
  group_by(rider, year) %>%
  summarize(seconds_gained = mean(pred, na.rm = T),
            successes = sum(pred < 0, na.rm = T),
            races = n()) %>%
  ungroup() %>%
  
  mutate(succ_perc = successes / races)

#
#
#
#
#
#


# Rider vs Rider Model ----------------------------------------------------

rider_vs_rider_data <- stage_data %>%
  
  filter(year > 2016) %>%
  
  # replace DNFs with rnk > 5
  mutate(rnkx = ifelse(is.na(rnk), 100, rnk)) %>%
  
  mutate(time_trial = ifelse(str_detect(stage_name, "ITT") | stage_name == "Prologue", TRUE, FALSE),
         climbing = ifelse(raw_climb_difficulty > 15, TRUE, FALSE),
         grand_tour = ifelse(race %in% c("giro d'italia", "tour de france", "vuelta a espana"), TRUE, FALSE)) %>%
  
  filter(grand_tour == TRUE & (climbing == TRUE | time_trial == TRUE)) %>%
  
  group_by(rider, race, year) %>%
  mutate(med_tm_pos = median(tm_pos, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(rider) %>%
  #filter(mean(rnkx < 6, na.rm = T) > 0.05 & n() > 39 | mean(rnkx < 6, na.rm = T) > 0.1 & n() > 19) %>%
  filter(n() > 4 & sum(rnkx < 6, na.rm = T) > 0) %>%
  ungroup() %>%
  
  filter(!is.na(rnk)) %>%
  
  select(rider, total_seconds, race, stage, year, time_trial, climbing, grand_tour, med_tm_pos)

#

matched_comparison <- rider_vs_rider_data %>%
  
  inner_join(
    
    rider_vs_rider_data %>%
      select(-time_trial, -climbing, -grand_tour), by = c("stage", "race", "year")
    
  ) %>%
  
  filter(rider.x != rider.y) %>%
  
  mutate(diff = total_seconds.y > total_seconds.x,
         tm_pos_diff = med_tm_pos.y - med_tm_pos.x)

#

library(lme4)

# this model measures beating your opponents
binomial_model <- glmer(diff ~ (1 | rider.x) + (1 | rider.y) + tm_pos_diff, 
                              
                              family = "binomial",
                              
                              data = matched_comparison %>%
                                filter(grand_tour == TRUE))

summary(binomial_model)

# this method measures the percentage you're beating or losing to your opponents
ratio_model <- lmer(diff ~ (1 | rider.x) + (1 | rider.y) + tm_pos_diff, 
                    
                    data = matched_comparison %>%
                      filter(grand_tour == TRUE) %>%
                      mutate(diff = total_seconds.x - total_seconds.y,
                             diff = diff / total_seconds.x))

summary(ratio_model)

#

ranefs_binom <- ranef(binomial_model)[["rider.x"]] %>%
  rownames_to_column()

ranefs_time <- ranef(ratio_model)[["rider.x"]] %>%
  rownames_to_column()

#
#
# peak 2019

peak_season <- stage_data_perf %>%
  
  # replace DNFs with rnk > 5
  mutate(rnkx = ifelse(is.na(rnk), 100, rnk)) %>%
  
  mutate(time_trial = ifelse(str_detect(stage_name, "ITT") | stage_name == "Prologue", TRUE, FALSE),
         climbing = ifelse(raw_climb_difficulty > 24 | 
                             concentration > 6 | 
                             (concentration > 4 & raw_climb_difficulty > 15), TRUE, FALSE),
         grand_tour = ifelse(race %in% c("giro d'italia", "tour de france", "vuelta a espana"), TRUE, FALSE)) %>%
  
  filter((climbing == TRUE | time_trial == TRUE)) %>%
  
  group_by(rider, race, year) %>%
  mutate(med_tm_pos = median(tm_pos, na.rm = T)) %>%
  ungroup() %>%
  
  # join with sof for successes and those points are the points you earn for success
  inner_join(
    
    fields_success %>%
      select(stage, race, year, tot), by = c("stage", "race", "year")
    
  ) %>%
  
  # -5 is the stand-in for non-success as -4 is lowest successful
  # assign full points for 1st, half for 2nd, etc
  mutate(success_points = ifelse(success == 1, tot / rnk, -5)) %>%
  
  mutate(climbing = ifelse(time_trial == TRUE, FALSE, climbing),
         time_trial = ifelse(climbing == TRUE, FALSE, time_trial)) %>%
  
  group_by(rider, year, time_trial, climbing) %>%
  filter(rank(-success_points, ties.method = "first") < 4) %>%
  ungroup() %>%
  
  filter(!is.na(rnk)) %>%
  
  select(race, stage, year, rider, climbing, time_trial, success_points) %>%
  
  group_by(rider, year, time_trial, climbing) %>%
  summarize(points = sum(success_points, na.rm = T),
            stages = n()) %>%
  ungroup()

#

matched_peak <- peak_season %>%
  
  inner_join(
    
    peak_season %>%
      select(-time_trial, -climbing, -grand_tour), by = c("stage", "race", "year")
    
  ) %>%
  
  filter(rider.x != rider.y) %>%
  
  mutate(diff = total_seconds.y > total_seconds.x,
         tm_pos_diff = med_tm_pos.y - med_tm_pos.x)

#

# this model measures beating your opponents
binomial_p_model <- glmer(diff ~ (1 | rider.x) + (1 | rider.y) + tm_pos_diff, 
                        
                        family = "binomial",
                        
                        data = matched_peak)

summary(binomial_p_model)

# this method measures the percentage you're beating or losing to your opponents
ratio_p_model <- lmer(diff ~ (1 | rider.x) + (1 | rider.y) + tm_pos_diff, 
                    
                    data = matched_peak %>%
                      mutate(diff = total_seconds.x - total_seconds.y,
                             diff = diff / total_seconds.x))

summary(ratio_p_model)

#

ranefs_p_binom <- ranef(binomial_p_model)[["rider.x"]] %>%
  rownames_to_column()

ranefs_p_time <- ranef(ratio_p_model)[["rider.x"]] %>%
  rownames_to_column()

#
#
#
#
# Time lost to winner vs stage finish

time_lost_model <- lm(vs_winner ~ places_back + 0,
                      
                      data = stage_data %>%
                        
                        filter(raw_climb_difficulty > 24 | 
                                 concentration > 6 | 
                                 (concentration > 4 & raw_climb_difficulty > 15)) %>%
                        filter(year > 2016) %>%
                        
                        mutate(vs_winner = total_seconds - win_seconds,
                               places_back = rnk - 1) %>%
                        
                        filter(!is.na(rnk)) %>%
                        filter(places_back < 51)
                      
)

#

stage_data %>%
  
  filter(raw_climb_difficulty > 24 | 
           concentration > 6 | 
           (concentration > 4 & raw_climb_difficulty > 15)) %>%
  filter(year > 2016) %>%
  
  mutate(vs_winner = total_seconds - win_seconds,
         places_back = rnk - 1) %>%     filter(!is.na(rnk)) %>%
  filter(places_back < 51) %>% 
  
  ggplot(aes(x = places_back, y = vs_winner))+
  geom_point(alpha = 0.1)+
  geom_smooth(size = 1.5, color = "gold")+
  geom_smooth(size = 1.5, color = "red", method = "lm", formula = y ~ x + 0)+
  labs(x = "Places behind winner", y = "Seconds lost to winner", title = "Climbing stage separation")

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


# Metric Calculation for Sprint / Climb / TT ------------------------------

sprint_rankings <- stage_data_perf %>%
  
  mutate(x1 = ifelse(rnk == 1, total_seconds, NA),
         x50 = ifelse(rnk < 51, total_seconds, NA)) %>%
  
  group_by(stage, race, year) %>%
  mutate(best = min(x1, na.rm = T),
         x50th = max(x50, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(gap = (x50th - best)) %>%
  
  select(-best, -x50th, -x50, -x1) %>%
  
  # filtering to Gaviria, Sagan, Viviani, Groenewegen, Ewan, and Bennett
  # the average gap for their top 5 finishes is 33 seconds
  # 90% are inside 60 seconds from 1st to 50th
  filter(gap < 61) %>%
  
  mutate(date = as.Date(date))

#

dates <- stage_data_perf %>%
  
  filter(year > 2016) %>%
  filter(!is.na(date)) %>%
  select(date) %>%
  unique() %>%
  mutate(date = date + 1) %>%
  as.list() %>%
  .[[1]]

#

week_sprint_list <- vector("list", length(dates))

for(w in 1:length(dates)) {
  
  DATES <- as.Date(dates[[w]])
  
  df <- sprint_rankings %>%
    
    filter(date < DATES & date > (DATES - 731)) %>%
    
    mutate(weight = 1 / as.numeric((10 + (DATES - date)))) %>%
    
    group_by(rider) %>%
    summarize(raw_success = mean(success, na.rm = T),
              wt_success = sum(success * weight, na.rm = T) / sum(weight, na.rm = T),
              raw_win = mean(rnk == 1, na.rm = T),
              wt_win = sum((rnk == 1) * weight, na.rm = T) / sum(weight, na.rm = T),
              stages = n()) %>%
    ungroup() %>%
    
    mutate(daily_date = DATES)
  
  week_sprint_list[[w]] <- df
  
  print(w)
  
}

#
#
#

sprint_rankings_daily <- bind_rows(week_sprint_list) %>%
  
  # regress with 0.25 success in 7 results (3.6% success rate)
  mutate(regr_success = ((raw_success * stages) + 0.25) / (stages + 7),
         regr_wt_success = ((wt_success * stages) + 0.25) / (stages + 7),
         regr_win = ((raw_win * stages) + 0.25) / (stages + 35)) %>%
  
  select(-wt_win) %>%
  
  gather(metric, value, -rider, -daily_date, -stages) %>%
  
  group_by(metric, daily_date) %>%
  mutate(rank = rank(-value, ties.method = "min")) %>%
  ungroup()

#

dbWriteTable(con, "daily_sprint_rankings", sprint_rankings_daily %>%
               filter(metric %in% c("regr_win", "regr_success")), overwrite = TRUE, row.names = FALSE)

#
#
#

# Domestiques/Lead-out man rankings
# below almost certainly overrates team impact (eg, higher team coef for TT stages vs non-TT stages)

team_rider_performance <- stage_data %>%
  
  filter(!is.na(rnk)) %>%
  
  group_by(stage, race, year, team, master_team) %>%
  mutate(best = min(rnk, na.rm = T)) %>%
  ungroup() %>%
  
  #filter(rnk != 1) %>%
  
  filter(class %in% c("1.UWT", "2.UWT", "2.HC", "1.HC")) %>%
  
  mutate(team_win = ifelse(best == 1, 1, 0),
         leader = ifelse(rnk == best, 1, 0)) %>%
  
  filter(time_trial == 0) %>%
  
  lme4::glmer(team_win ~ (1 | master_team) + (1 | rider) + leader, family = "binomial", data = .)

#

riders <- lme4::ranef(team_rider_performance)[[1]] %>%
  rownames_to_column()

master_team <- lme4::ranef(team_rider_performance)[[2]] %>%
  rownames_to_column()
