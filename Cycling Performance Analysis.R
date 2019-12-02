
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
  
  # calculate ensemble score, give 1.5 points if summit, -0.5 if not
  # then scale rest of score
  mutate(ensemble = ifelse(summit_finish == 1, 1.5 + (0.4 * last_climb) + (0.4 * act_climb_difficulty) + (0.2 * concentration),
                           -0.5 + (0.15 * last_climb) + (0.5 * act_climb_difficulty) + (0.35 * concentration)))

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
# rider climbing performance

library(lme4)

climb_rider_model <- glmer(success ~ (1 | rider / climb_score), 
                           
                           data = stage_data_perf %>%
                             filter(year > 2016) %>%
                             mutate(success = ifelse(rel_success == 0, 1, success)) %>%
                             group_by(rider) %>%
                             filter(n() > 99) %>%
                             ungroup(),
                           family = "binomial")
