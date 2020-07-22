
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
# CODE BELOW SETS UP models in Stan which predict success given rider/pred_climb_difficulty
#
# AND tm_pos == 1 given rider/pred_climb_difficulty
#
# AND success given weight:pred_climb_difficulty + rider/pred_climb_difficulty
#

mod_data <- dbReadTable(con, "stage_data_perf") %>%
  
  filter(time_trial == 0) %>%
  
  filter(year >= 2015 & year <= 2017 &
           (class %in% c("1.UWT", "WC", "CC", "Olympics", "2.UWT") |
           sof >= 0.20) &
           !is.na(pred_climb_difficulty)) %>%
  mutate(success = ifelse(is.na(success), 0, success)) %>%
  
  mutate(rider = str_to_title(tolower(rider))) %>%
  
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
  mutate(team_ldr = ifelse(is.na(team_ldr), 0, team_ldr)) %>%
  
  mutate(win = ifelse(rnk == 1, 1, 0),
         win = ifelse(is.na(rnk), 0, win))

#

get_prior(success ~ (1 + pred_climb_difficulty | rider),
          
          data = mod_data)

#

prior1 <- c(
  prior(normal(-2, 3), class = Intercept),
  prior(lkj(2), class = cor),
  prior(cauchy(0, 4), class = sd)
)

# 4 cores, 4 chains, 1500 iters per chain takes 2 hours to run

mod1 <- brm(success ~ (1 + pred_climb_difficulty | rider),
            
            data = mod_data,
            
            family = bernoulli(),
            
            prior = prior1,
            
            warmup = 500, iter = 1500, chains = 4, cores = 4)

summary(mod1)

brms::fixef(mod1)

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
#
# use LMER
#
#

mod4 <- lme4::glmer(team_ldr ~ (1 + pred_climb_difficulty | rider),
            
            data = mod_data,
            
            family = binomial("logit"))

#
# WHO WILL BE TEAM LEADER?
#

get_prior(team_ldr ~ (1 + pred_climb_difficulty | rider),
          
          data = mod_data)

#

prior2 <- c(
  prior(normal(0, 3), class = Intercept),
  prior(lkj(2), class = cor),
  prior(cauchy(0, 10), class = sd)
)

#

mod2 <- brm(team_ldr ~ (1 + pred_climb_difficulty | rider),
            
            data = mod_data,
            
            family = bernoulli(),
            
            prior = prior2,
            
            warmup = 500, iter = 3000, chains = 1, cores = 4)

summary(mod2)

fixef(mod2)

mod2_ranefs <- brms::ranef(mod2) %>%
  .[[1]] %>% 
  as.data.frame() %>% 
  rownames_to_column() %>%
  inner_join(mod_data %>%
               group_by(rider) %>%
               count() %>%
               ungroup(), by = c("rowname" = "rider"))

preds <- cbind(
  
  pred = predict(mod2, mod_data %>%
                   select(rider, stage, race, year, pred_climb_difficulty, date, team, success)),
  
  mod_data %>%
    select(rider, stage, race, year, pred_climb_difficulty, date, team, success)
  
)

#
#
# Add in SOF, probably not a big deal considering success already includes implicit sof adjustment
#
#

get_prior(success ~ (1 + pred_climb_difficulty | rider) + sof,
          
          data = mod_data)

#

prior5 <- c(
  prior(normal(0, 3), class = Intercept),
  prior(uniform(-10, 10), class = b, coef = sof),
  prior(lkj(2), class = cor),
  prior(cauchy(0, 10), class = sd)
)

#

mod5 <- brm(success ~ (1 + pred_climb_difficulty | rider) + sof,
            
            data = mod_data,
            
            family = bernoulli(),
            
            prior = prior5,
            
            warmup = 500, iter = 2000, chains = 4, cores = 4)

# this takes 2.5 hours to run, but sof adds nothing to the model (-0.01 coef with 0.11 error)

summary(mod5)

brms::fixef(mod5)

mod5_ranefs <- brms::ranef(mod5) %>%
  .[[1]] %>% 
  as.data.frame() %>% 
  rownames_to_column() %>%
  inner_join(mod_data %>%
               group_by(rider) %>%
               count() %>%
               ungroup(), by = c("rowname" = "rider"))

#
#
# Win model using SOF
#
#

#
#
# Add in SOF to win model, might add something
#
#

get_prior(win ~ (1 + pred_climb_difficulty | rider) + sof,
          
          data = mod_data)

#

prior6 <- c(
  prior(normal(0, 3), class = Intercept),
  prior(uniform(-10, 10), class = b, coef = sof),
  prior(lkj(2), class = cor),
  prior(cauchy(0, 10), class = sd)
)

# this model ran and shows about a -2 coef for the highest sof races (typically TDF stages) vs 0.2 race

mod6 <- brm(win ~ (1 + pred_climb_difficulty | rider) + sof,
            
            data = mod_data,
            
            family = bernoulli(),
            
            prior = prior6,
            
            warmup = 500, iter = 2000, chains = 4, cores = 4)

summary(mod6)

brms::fixef(mod6)

mod6_ranefs <- brms::ranef(mod6) %>%
  .[[1]] %>% 
  as.data.frame() %>% 
  rownames_to_column() %>%
  inner_join(mod_data %>%
               group_by(rider) %>%
               count() %>%
               ungroup(), by = c("rowname" = "rider"))

#
#
# model points_finish -- this is absolute shit as I can't get the distribution correct
#
#

get_prior(points_finish ~ (1 + pred_climb_difficulty | rider),
          
          data = mod_data,
          
          family = Beta())

#

prior8 <- c(
  prior(normal(0, 3), class = Intercept),
  prior(lkj(2), class = cor),
  prior(cauchy(0, 10), class = sd)
)

#

mod8 <- brm(points_finish ~ (1 + pred_climb_difficulty | rider),
            
            data = mod_data %>%
              mutate(points_finish = ifelse(points_finish >= 1, 0.999, points_finish)),
            
            family = zero_inflated_beta(),
            
            prior = prior8,
            
            warmup = 400, iter = 1600, chains = 4, cores = 4)

summary(mod8)

brms::fixef(mod8)

mod8_ranefs <- brms::ranef(mod8) %>%
  .[[1]] %>% 
  as.data.frame() %>% 
  rownames_to_column() %>%
  inner_join(mod_data %>%
               group_by(rider) %>%
               count() %>%
               ungroup(), by = c("rowname" = "rider"))


preds <- cbind(
  
  pred = predict(mod8, mod_data %>%
                   select(rider, stage, race, year, pred_climb_difficulty, date, team, success) %>%
                   filter(race == "uae tour" & year == 2020)),
  
  mod_data %>%
    select(rider, stage, race, year, pred_climb_difficulty, date, team, success) %>%
    filter(race == "uae tour" & year == 2020)
  
)


#
#
# LME4 Model
#
#

mod7 <- lme4::glmer(success ~ (1 + pred_climb_difficulty | rider) + races,
            
            data = mod_data %>%
              group_by(rider) %>%
              mutate(races = n()) %>%
              ungroup(),
            
            family = binomial("logit"))

summary(mod7)

lme4::fixef(mod7)

mod7_ranefs <- lme4::ranef(mod7) %>%
  .[[1]] %>% 
  as.data.frame() %>% 
  rownames_to_column() %>%
  inner_join(mod_data %>%
               group_by(rider) %>%
               count() %>%
               ungroup(), by = c("rowname" = "rider"))

