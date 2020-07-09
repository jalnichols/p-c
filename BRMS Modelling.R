
library(tidyverse)
library(RMySQL)
library(brms)

#

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#
#
#

# bring in data

stage_data_perf <- dbReadTable(con, "stage_data_perf")

#
# CODE BELOW SETS UP models in Stan which predict success given rider/pred_climb_difficulty
#
# AND tm_pos == 1 given rider/pred_climb_difficulty
#
# AND success given weight:pred_climb_difficulty + rider/pred_climb_difficulty
#

mod_data <- stage_data_perf %>%
  
  filter(time_trial == 0) %>%
  
  filter(year >= 2018 & 
           (class %in% c("1.UWT", "WC", "CC", "Olympics", "2.UWT") |
           sof >= 0.20) &
           !is.na(pred_climb_difficulty)) %>%
  mutate(success = ifelse(is.na(success), 0, success)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
      filter(!is.na(weight)) %>%
      mutate(rider = str_to_title(rider)), by = c("rider")
    
  ) %>%
  mutate(weight = ifelse(is.na(weight), median(weight, na.rm = T), weight)) %>%
  
  group_by(race, stage, year) %>%
  mutate(weight = weight - mean(weight, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(team_ldr = ifelse(tm_pos == 1, 1, 0)) %>%
  mutate(team_ldr = ifelse(is.na(team_ldr), 0, team_ldr))

#

get_prior(success ~ (1 + pred_climb_difficulty | rider),
          
          data = mod_data)

#

prior1 <- c(
  prior(normal(0, 3), class = Intercept),
  prior(lkj(2), class = cor),
  prior(cauchy(0, 10), class = sd)
)

#

mod1 <- brm(success ~ (1 + pred_climb_difficulty | rider),
            
            data = mod_data,
            
            family = bernoulli(),
            
            prior = prior1,
            
            warmup = 2000, iter = 7000, chains = 1)

summary(mod1)

point_est_intercept = brms::fixef(mod1)

mod1_ranefs <- brms::ranef(mod1) %>%
  .[[1]] %>% 
  as.data.frame() %>% 
  rownames_to_column() %>%
  inner_join(mod_data %>%
               group_by(rider) %>%
               count() %>%
               ungroup(), by = c("rowname" = "rider"))

preds <- cbind(
  
  pred = predict(mod1, mod_data %>%
                   select(rider, stage, race, year, pred_climb_difficulty, date, team, success)),
  
  mod_data %>%
    select(rider, stage, race, year, pred_climb_difficulty, date, team, success)
  
)

#
# add in weight/pcd interaction
#
# impacts here are roughly that 57 kg riders out-perform 90 kg riders by about 0.25 in terms of coef on pcd = 20
# and vice-versa for 90 kg riders vs 57 kg riders on pcd = 1
#
#

get_prior(success ~ pred_climb_difficulty:weight + (1 + pred_climb_difficulty | rider),
          
          data = mod_data)

# not sure how or whether to set prior for pcd:weight
# setting a wide prior centered on -2 for overall intercept as most are NOT successful in race

prior3 <- c(
  prior(normal(-2, 5), class = Intercept),
  prior(lkj(2), class = cor),
  prior(cauchy(0, 10), class = sd)
)

# THIS TOOK 18 hours for 6000 iterations

mod3 <- brm(success ~ pred_climb_difficulty:weight + (1 + pred_climb_difficulty | rider),
            
            data = mod_data,
            
            family = bernoulli(),
            
            prior = prior3,
            
            warmup = 1000, iter = 6000, chains = 1, cores = 4)

summary(mod3)

# fixefs

weight_pcd = brms::fixef(mod3)

# ranefs

mod3_ranefs <- brms::ranef(mod3) %>%
  .[[1]] %>% 
  as.data.frame() %>% 
  rownames_to_column() %>%
  inner_join(mod_data %>%
               group_by(rider) %>%
               count() %>%
               ungroup(), by = c("rowname" = "rider"))

#
# WHO WILL BE TEAM LEADER?
#

mod2_data <- stage_data_perf %>%
  filter(year >= 2018 & 
           class %in% c("1.HC", "1.Pro", "1.UWT", "WC", "CC", "Olympics",
                        "2.HC", "2.Pro", "2.UWT",
                        "1.1", "2.1") &
           !is.na(pred_climb_difficulty)) %>%
  mutate(team_ldr = ifelse(tm_pos == 1, 1, 0)) %>%
  mutate(team_ldr = ifelse(is.na(team_ldr), 0, team_ldr))

#

get_prior(team_ldr ~ (1 + pred_climb_difficulty | rider),
          
          data = mod2_data)

#

prior2 <- c(
  prior(normal(0, 3), class = Intercept),
  prior(lkj(2), class = cor),
  prior(cauchy(0, 10), class = sd)
)

#

mod2 <- brm(success ~ (1 + pred_climb_difficulty | rider),
            
            data = mod2_data,
            
            family = bernoulli(),
            
            prior = prior2,
            
            warmup = 2000, iter = 7000, chains = 1)

summary(mod2)

coef(mod2)

mod2_ranefs <- brms::ranef(mod2) %>%
  .[[1]] %>% 
  as.data.frame() %>% 
  rownames_to_column() %>%
  inner_join(mod2_data %>%
               group_by(rider) %>%
               count() %>%
               ungroup(), by = c("rowname" = "rider"))

preds <- cbind(
  
  pred = predict(mod1, mod1_data %>%
                   select(rider, stage, race, year, pred_climb_difficulty, date, team, success)),
  
  mod1_data %>%
    select(rider, stage, race, year, pred_climb_difficulty, date, team, success)
  
)