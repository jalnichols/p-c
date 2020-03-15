
library(tidyverse)
library(fs)

#

all_odds <- fs::dir_info("odds/") %>%
  
  filter(!path == "odds/matchups.csv") %>%
  filter(!(str_detect(path, "giroditalia") |
             str_detect(path, "flanders") | 
             str_detect(path, "milansanremo") |
             str_detect(path, "roubaix") | 
             str_detect(path, "tourdefrance"))) 

#

odds_list <- vector("list", length(all_odds$path))

for(i in 1:length(all_odds$path)) {
  
  df <- read_csv(all_odds$path[[i]], col_names = FALSE) %>%
    
    .[, 1:3] %>%
    
    rename(rider = X1,
           highest = X2,
           lowest = X3) %>%
    mutate(highest = as.numeric(highest),
           lowest = as.numeric(lowest)) %>%
    
    mutate(info = str_replace(all_odds$path[[i]], "odds/", "")) %>%
    separate(info, c("race", "year", "stage"), sep = "\\-") %>%
    mutate(stage = str_replace(stage, ".csv", "")) %>%
    filter(!rider %in% c("name", "Name", "NAME", "RIDER")) %>%
    filter(!is.na(highest)) %>%
    filter(!is.na(lowest)) %>%
    
    mutate(implied_h = 1 / highest,
           implied_l = 1 / lowest,
           implied_avg = (implied_h + implied_l) / 2) %>%
    mutate(decimal = 1 / implied_avg) %>%
    filter(!is.na(rider))
  
  output <- df %>%
    select(rider, decimal, race, stage, year) %>%
    spread(rider, decimal)
  
  append <- output[,1:3]
  
  conv <- implied::implied_probabilities(output[,4:length(output)], method = 'or')
  
  output <- rbind(output[,4:length(output)],
                  conv$probabilities) %>%
    
    cbind(append) %>%
    cbind(tibble(type = c("odds", "probs"))) %>%
    gather(rider, value, -type, -race, -stage, -year) %>%
    spread(type, value) %>%
    mutate(margin = conv$margin,
           odds_ratio = conv$odds_ratios)
  
  odds_list[[i]] <- output
  
}

#

odds_df <- bind_rows(odds_list) %>%
  
  left_join(
    
    read_csv("odds-riders.csv"), by = c("rider")
    
  ) %>%
  
  mutate(year = 2020) %>%
  
  left_join(
    
    read_csv("odds-races.csv"), by = c("race")) %>%
  
  select(race = pcs_race, stage, year, rider = pcs_rider, odds, probs)

#

write_csv(odds_df, "odds_db.csv")
