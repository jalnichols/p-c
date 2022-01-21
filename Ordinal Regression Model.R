
library(tidyverse)
library(RMySQL)
library(ordinal)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

stage_data_perf <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE year > 2019") 

# it takes 2600 seconds for 71k rows with 13 ordered factors

tictoc::tic()

mod2 = clmm(rnk_grp ~ (1 + pred_climb_difficulty | rider) + 0, 
           
           stage_data_perf %>% 
             
             group_by(rider) %>% filter(n() >= 50 & min(rnk) <= 50) %>% ungroup() %>%
             
             mutate(rnk_grp = ifelse(rnk <= 10, round(100/rnk,0),
                                     ifelse(rnk <= 25, 6,
                                            ifelse(rnk <= 75, 2, 1)))) %>%
             
             filter(!class %in% c("1.2", "2.2", "2.Ncup", "1.Ncup", "2.2U", "1.2U", "JR", "NC")) %>%
             filter(rnk != 200) %>%
          
             mutate(rnk_grp = as.factor(rnk_grp)),
           
           link = "logit", Hess = TRUE)

tictoc::toc()

#
#
#

ranefs <- ordinal::ranef.clmm(mod2)[[1]] %>% rownames_to_column()

#
#
#

predictions_2_pro <- stage_data_perf %>% 
  filter(class == "2.Pro") %>%
  
  inner_join(ranefs %>%
               rename(pcd_impact = pred_climb_difficulty) %>%
               janitor::clean_names(), by = c("rider" = "rowname")) %>%

  mutate(pcd_impact = pcd_impact * pred_climb_difficulty,
         total_impact = intercept + pcd_impact,
         coef = exp(total_impact)/(1+exp(total_impact)))  
  
