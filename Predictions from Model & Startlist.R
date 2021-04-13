
library(tidyverse)
library(rvest)
library(DBI)

con <- dbConnect(RMySQL::MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

URL <- 'https://www.procyclingstats.com/race/scheldeprijs/2021/startlist/'

PCD = 0
BS = 0.95

#

page <- URL %>%
  read_html() 

#

startlist <- page %>%
  html_nodes('ul.startlist_v3') %>%
  html_nodes('a')

riders <- startlist %>%
  html_nodes('span') %>%
  html_text() %>%
  enframe(name = NULL) %>%
  mutate(value = iconv(value, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
  mutate(value = str_to_title(tolower(value)))

teams <- cbind(
  startlist %>%
    html_text() %>%
    enframe(name = NULL),
  startlist %>%
    html_attr(name = "href") %>%
    enframe(name = NULL) %>%
    rename(url = value)) %>%
  filter(value != "") %>%
  mutate(team_2nd = ifelse(str_detect(url, "team"), value, NA)) %>%
  fill(team_2nd, .direction = "down") %>%
  mutate(value = iconv(value, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
  mutate(value = str_to_title(tolower(value))) %>%
  select(-url) %>%
  rename(team = team_2nd)

riders <- cbind(riders %>% rename(rider = value)) %>%
  inner_join(teams, by = c("rider" = "value"))

#

most_recent_models <- rbind(
  dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact
             FROM lme4_rider_success r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_success r
             WHERE test_or_prod = 'prod'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'") %>% mutate(Type = 'Success'),
  
  dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact
             FROM lme4_rider_logranks r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_logranks r
             WHERE test_or_prod = 'prod'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'") %>% mutate(Type = 'LogRanks'),
  
  dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact
             FROM lme4_rider_pointswhenopp r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_pointswhenopp r
             WHERE test_or_prod = 'prod'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'") %>% mutate(Type = 'PointsWhenOpp'),
  
  dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact
             FROM lme4_rider_points r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_points r
             WHERE test_or_prod = 'prod'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'") %>% mutate(Type = 'Points'),
  
  dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact
             FROM lme4_rider_teamleader r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_teamleader r
             WHERE test_or_prod = 'prod'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'") %>% mutate(Type = 'Leader'),
  
  
  dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, r.Date, bunchsprint_impact
             FROM lme4_rider_succwhenopp r
             JOIN (
             
             SELECT max(Date) as Date
             FROM lme4_rider_succwhenopp r
             WHERE test_or_prod = 'prod'
             
             ) x ON r.Date = x.Date
             WHERE test_or_prod = 'prod'") %>% mutate(Type = 'WhenOpp')
  
)

#
#
#

models <- riders %>%
  
  left_join(
    
    most_recent_models, by = c("rider")
    
  ) %>%

  mutate(coef = ifelse(Type == "Points", random_intercept + (pcd_impact * PCD) + (bunchsprint_impact * BS) + 0.012,
                       ifelse(Type == "PointsWhenOpp", random_intercept + (pcd_impact * PCD) + (bunchsprint_impact * BS) + 0.012,
                       ifelse(Type == "LogRanks",  random_intercept + (pcd_impact * PCD) + (bunchsprint_impact * BS),
                              ifelse(Type == "Success", exp(random_intercept + (pcd_impact * PCD) + (bunchsprint_impact * BS) - 4.5),
                                     ifelse(Type == "Leader", exp(random_intercept + (pcd_impact * PCD) + (bunchsprint_impact * BS) - 2), 
                                            ifelse(Type == "WhenOpp", exp(random_intercept + (pcd_impact * PCD) + (bunchsprint_impact * BS) - 2), NA)))))),
         coef = ifelse(Type %in% c("Success", "Leader", "WhenOpp"), coef / (1+coef), coef)) %>%
  
  mutate(coef = ifelse(is.na(coef),
                       ifelse(Type %in% c("Points", "PointsWhenOpp"), 0.01,
                              ifelse(Type == "LogRanks", min(coef, na.rm = T),
                                     ifelse(Type == "Leader", 0.02,
                                            ifelse(Type %in% c("Success","WhenOpp"), 0.01, NA)))), coef)) %>%
  group_by(team, Type) %>%
  mutate(coef = ifelse(Type == "Leader", ifelse(coef <= 0.05, 0, coef), coef)) %>%
  ungroup() %>%
  
  group_by(team, Type) %>%
  mutate(adj = ifelse(Type == "Leader", coef / sum(coef), coef)) %>%
  ungroup()

#
#
#

wide_models <- models %>%
  filter(!is.na(adj)) %>%
  select(rider, team, Type, adj) %>%
  spread(Type, adj) %>%

  mutate(rankPoints = rank(-Points, ties.method = "min"),
         rankPointsWhenOpp = rank(-PointsWhenOpp, ties.method = "min"),
         rankLog = rank(-LogRanks, ties.method = "min"),
         rankSuccess = rank(-Success, ties.method = "min")) %>%

  mutate(rankOverall = 4 / ((1/rankPoints)+(1/rankLog)+(1/rankPoints)+(1/rankPointsWhenOpp)))
