
library(tidyverse)
library(RMySQL)

#

stage_data <- dbReadTable(con, "pcs_stage_data")

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