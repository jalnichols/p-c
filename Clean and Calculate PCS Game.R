
library(tidyverse)
library(lubridate)
library(rvest)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#
#
#

pcs_game_picks <- dbGetQuery(con, "SELECT * FROM pcs_game_picks") %>%
  
  group_by(stage, race, year) %>%
  mutate(selections = sum(number_picks, na.rm = T)) %>%
  ungroup() %>%
  
  filter(selections > 499) %>%
  
  # roughly 95% of slots are filled
  mutate(selections = selections / 0.9,
         rate = number_picks / selections * 5,
         rate = ifelse(rate > 0.98, 0.98, rate)) %>%
  
  select(-selections, -result, -url)

pcs_game_picks$picked_rider <-  str_to_title(tolower(pcs_game_picks$picked_rider))
pcs_game_picks$picked_rider <- iconv(pcs_game_picks$picked_rider, from="UTF-8", to = "ASCII//TRANSLIT")
pcs_game_picks$race <- str_to_title(tolower(pcs_game_picks$race))
pcs_game_picks$race <- iconv(pcs_game_picks$race, from="UTF-8", to = "ASCII//TRANSLIT")
pcs_game_picks$race <- tolower(pcs_game_picks$race)

#
#
#
#
#

# ODDS

odds <- read_csv("odds_db.csv")

#

combined <- odds %>%
  
  filter(!stage == "GC") %>%
  mutate(stage = as.numeric(stage),
         race = tolower(race)) %>%
  
  left_join(
    
    pcs_game_picks, by = c("rider" = "picked_rider", "race", "year", "stage")
    
  )

#

valid <- combined %>% 
  filter(!is.na(rate)) %>% 
  
  group_by(race, stage, year) %>%
  mutate(rk = rank(-probs, ties.method = "min")) %>% 
  ungroup() %>% 
  
  filter(!race == "uae tour") %>%
  
  filter(probs > 0.005)

#

basic_mod <- lm(rate ~ log(probs) + (rk < 6), data = valid)

summary(basic_mod)

preds <- cbind(
  
  pred = predict(basic_mod, valid),
  valid,
  resid = residuals(basic_mod))

#

