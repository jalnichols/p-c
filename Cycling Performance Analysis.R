
library(tidyverse)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

stage_data <- dbReadTable(con, "pcs_stage_data")

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
    
  )

#
#
# STRENGTH OF FIELD
#
#

riders_stages <- stage_data %>%
  
  filter(time_trial == FALSE) %>%
  
  filter(rnk < 11) %>%
  
  select(rider1 = rider, stage, race, year) %>%
  unique() %>%
  
  inner_join(
    
    stage_data %>%
      
      filter(time_trial == FALSE) %>%
      
      filter(rnk < 11) %>%   
      
      select(rider2 = rider, stage, race, year) %>%
      unique(), by = c("stage", "race", "year")	
    
  ) %>%
  
  filter(!(rider1 == rider2)) %>%
  unique()

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

rxxx <- cbind(
  pxxx$x, 
  matches %>%
    select(rider1)) %>%
  select(rider1, PC1, PC2, PC3, PC4)

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
               select(rider = rider1, PC1)) %>%
  group_by(stage, race, year) %>%
  summarize(tot = sum(PC1, na.rm = T), 
            n = n()) %>%
  ungroup()

#

fields <- stage_data %>%
  
  select(stage, race, year, rider, class, grand_tour, date, rnk) %>%
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
  
  separate(class, c("type", "class"), by = ".") %>%
  
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
  cbind(
    
    limit = predict(limits_actual, 
                    stage_data %>%
                      inner_join(
                        
                        fields %>%
                          select(stage, race, year, sof = tot), by = c("stage", "race", "year")))
    
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
  
  separate(class, c("type", "class"), by = ".") %>%
  
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
  
  select(stage, race, year, summit_finish, act_climb_difficulty, last_climb, concentration) %>%
  
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
        mutate(race = ifelse(race %in% c("la vuelta ciclista a espana", "la vuelta a espana"), "vuelta a espana", race)) %>%
        
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
  
  select(-pred)

#
#
# rider climbing performance

in_contact <- stage_data_perf %>%
  
  mutate(rnk = ifelse(is.na(rnk), 150, rnk)) %>%
   
  mutate(W = ifelse(rnk == 1,1,0), 
         T3 = ifelse(rnk < 4,1,0),
         T5 = ifelse(rnk < 6,1,0),
         x2_5 = ifelse(rnk > 1 & rnk < 6,1,0)) %>% 
  
  group_by(rider) %>% 
  filter(n() > 99) %>%
  ungroup() %>%
  
  gather(category, result, W:x2_5) %>%
  
  group_by(rider, category) %>%
  do(broom::tidy(glm(result ~ log(max_effort), data = ., family = "binomial"))) %>%
  ungroup() %>%
  
  select(rider, term, estimate, category)

#

R = "Sagan Peter"

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
