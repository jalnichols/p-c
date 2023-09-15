library(tidyverse)
library(DBI)

dbDisconnect(con)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      port = 5432,
                      host = 'localhost',
                      dbname = "cycling",
                      user = "postgres",
                      password = "braves")
#

pcs_pts_season <- dbGetQuery(con, "SELECT DISTINCT rider, season, pcs_points FROM pcs_points_season_rider")

racedays <- dbGetQuery(con, "SELECT DISTINCT rider, stage, race, url, year, class, date, length, time_trial
                       FROM stage_data_perf
                       WHERE year >= 2011") %>%
  
  mutate(rider = case_when(rider == "Skjelmose Jensen Mattias" ~ "Skjelmose Mattias",
                           rider == "Wright Alfred" ~ "Wright Fred",
                           TRUE ~ rider)) %>%
  
  group_by(rider, year) %>%
  summarize(racedays = n()) %>%
  ungroup()

#

per_raceday <- pcs_pts_season %>%
  
  left_join(racedays, by = c("rider", "season" = "year")) %>%
  mutate(pcs_points = ifelse(is.na(pcs_points), 0, pcs_points)) %>%
  mutate(per_raceday = pcs_points / racedays) %>%
  
  filter(!is.na(racedays)) %>%
  
  group_by(rider, season) %>%
  summarize(racedays = max(racedays, na.rm = T),
            pcs_points = max(pcs_points, na.rm = T)) %>%
  ungroup()

#

years = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022,2023)
last_three_years <- vector("list", length(years))
last_two_years <- vector("list", length(years))
last_one_years <- vector("list", length(years))

for(y in years) {
  
  last_three_years[[y-(min(years)-1)]] <- per_raceday %>%
    
    filter(between(season, y-2, y)) %>%
    
    group_by(rider) %>%
    summarize(pcs_points = sum(pcs_points, na.rm = T),
              racedays = sum(racedays, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(per_raceday = pcs_points / racedays,
           latest_year = y)
  
  last_two_years[[y-(min(years)-1)]] <- per_raceday %>%
    
    filter(between(season, y-1, y)) %>%
    
    group_by(rider) %>%
    summarize(pcs_points = sum(pcs_points, na.rm = T),
              racedays = sum(racedays, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(per_raceday = pcs_points / racedays,
           latest_year = y)
  
  last_one_years[[y-(min(years)-1)]] <- per_raceday %>%
    
    filter(season == y) %>%
    
    group_by(rider) %>%
    summarize(pcs_points = sum(pcs_points, na.rm = T),
              racedays = sum(racedays, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(per_raceday = pcs_points / racedays,
           latest_year = y)
}

#

last_three_years <- bind_rows(last_three_years) %>%
  
  mutate(rider_match = str_to_title(rider)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, date as dob FROM rider_attributes") %>%
      filter(!is.na(dob)) %>%
      mutate(rider = str_to_title(rider)) %>%
      unique(), by = c("rider_match" = "rider")) %>%
  
  mutate(age = as.numeric(as.Date(paste0(latest_year-1,"-01-01"))-as.Date(dob))/365.25) %>%
  
  select(-rider_match, -dob)

last_two_years <- bind_rows(last_two_years)

last_one_years <- bind_rows(last_one_years)

#

x2023_teams <- dbGetQuery(con, "SELECT * FROM pcs_season_team WHERE season = 2024") %>%
  unique() %>%
  rbind(tibble(team = c("AG2R Citroen Team", "INEOS Grenadiers", "AG2R Citroen Team", "Lidl - Trek",
                        "Astana Qazaqstan Team"),
               level = c("WT", "WT", "WT", "WT", "WT"),
               rider = c("Bennett Sam", "Rodriguez Carlos", "Lafay Victor", "Oomen Sam", "Ballerini Davide"),
               rider_url = "",
               season = 2024)) %>%
  
  #filter(!(rider == "Scaroni Christian" & team == "Astana Qazaqstan Team ")) %>%
  
  left_join(last_three_years %>%
              unique() %>%
              mutate(latest_year = latest_year + 1), by = c("rider", "season" = "latest_year")) %>%
  
  left_join(last_two_years %>%
              unique() %>%
              mutate(latest_year = latest_year + 1), by = c("rider", "season" = "latest_year")) %>%
  
  left_join(last_one_years %>%
              unique() %>%
              mutate(latest_year = latest_year + 1), by = c("rider", "season" = "latest_year")) %>%
  
  mutate(per_raceday.x = ifelse(is.na(per_raceday.x), 0, per_raceday.x),
         per_raceday.y = ifelse(is.na(per_raceday.y), 0, per_raceday.y),
         per_raceday = ifelse(is.na(per_raceday), 0, per_raceday)) %>%
  unique()

#

retired <- c("Colbrelli Sonny", "Valverde Alejandro", "Porte Richie", 
             "Martin Dan", "Nibali Vincenzo", "Terpstra Niki",
             "Gilbert Philippe", "Rolland Pierre", "Greipel Andre",
             "Dumoulin Tom", "Rebellin Davide", "Hivert Jonathan",
             "Geniez Alexandre", "Visconti Giovanni", "Langeveld Sebastian",
             "Nieve Mikel", "Kangert Tanel", "Brandle Matthias", "Rosa Diego",
             "Claeys Dimitri", "Roux Anthony", "Machado Tiago",
             "Meyer Cameron", "Finetto Mauro", "Van Staeyen Michael",
             "Dowsett Alex",
             
             "Pinot Thibaut", "Van Avermaet Greg", "Mader Gino",
             "Vanmarcke Sep", "Dennis Rohan")

blacklist <- c("Quintana Nairo", "Lopez Miguel Angel")

unsigned <- last_three_years %>%
  filter(latest_year == 2023) %>%
  filter(!rider %in% x2023_teams$rider) %>%
  filter(racedays >= 100) %>%
  filter(!rider %in% retired) %>%
  filter(!rider %in% blacklist) %>%
  arrange(desc(per_raceday))

#

team_quality <- x2023_teams %>%
  
  filter(level %in% c("PRT", "WT")) %>%
  
  group_by(team, level) %>%
  summarize(straight_l3 = mean(per_raceday.x, na.rm = T),
            straight_l2 = mean(per_raceday.y, na.rm = T),
            straight_l1 = mean(per_raceday, na.rm = T),
            riders = n()) %>%
  ungroup()

#
#
#
#
#
#
#
#

# Eval Prior

racedays_predictor <- dbGetQuery(con, "SELECT * FROM pcs_season_team WHERE season < 2023 AND season > 2013") %>%
  unique() %>%
  
  left_join(last_three_years %>%
              unique() %>%
              mutate(latest_year = latest_year + 1), by = c("rider", "season" = "latest_year")) %>%
  
  left_join(last_two_years %>%
              unique() %>%
              mutate(latest_year = latest_year + 1), by = c("rider", "season" = "latest_year")) %>%
  
  left_join(last_one_years %>%
              unique() %>%
              mutate(latest_year = latest_year + 1), by = c("rider", "season" = "latest_year")) %>%
  
  mutate(per_raceday.x = ifelse(is.na(per_raceday.x), 0, per_raceday.x),
         per_raceday.y = ifelse(is.na(per_raceday.y), 0, per_raceday.y),
         per_raceday = ifelse(is.na(per_raceday), 0, per_raceday),
         racedays.x = ifelse(is.na(racedays.x), 0, racedays.x),
         racedays.y = ifelse(is.na(racedays.y), 0, racedays.y),
         racedays = ifelse(is.na(racedays), 0, racedays)) %>%
  unique() %>%
  
  left_join(racedays %>%
              rename(next_racedays = racedays), by = c("season" = "year", "rider"))

#

predict_next_racedays <- racedays_predictor %>%
  mutate(level = ifelse(level %in% c("PCT", "PRT"), "PRT",
                        ifelse(level %in% c("P-CRO", "CT", "CLUB"), "CT", level))) %>%
  mutate(covid = ifelse(season == 2020, 1, 0), 
         vs_28 = abs(age-28), 
         vs_28 = ifelse(is.na(vs_28), 0, vs_28),
         age = ifelse(is.na(age), 28, age),
         racedays_l3 = racedays.x / 3,
         racedays_l2 = racedays.y / 2,
         racedays_l1 = racedays,
         relyr = season - mean(season, na.rm = T)) %>% 
  
  lm(next_racedays ~ racedays_l3 + racedays_l2 + racedays_l1 + covid + vs_28 + age + level + relyr, data = .)

summary(predict_next_racedays)

#

x2023_racedays <- x2023_teams %>%
  mutate(level = ifelse(level %in% c("PCT", "PRT"), "PRT",
                        ifelse(level %in% c("P-CRO", "CT", "CLUB"), "CT", level))) %>%
  mutate(covid = ifelse(season == 2020, 1, 0), 
         vs_28 = abs(age-28), 
         vs_28 = ifelse(is.na(vs_28), 0, vs_28),
         age = ifelse(is.na(age), 28, age),
         racedays_l3 = racedays.x / 3,
         racedays_l2 = racedays.y / 2,
         racedays_l1 = racedays,
         relyr = season - mean(season, na.rm = T)) %>% 
  
  mutate(predict_2023_racedays = predict(predict_next_racedays, .),
         predicted_points = per_raceday.x * predict_2023_racedays)

#


# Eval Prior

pts_predictor <- dbGetQuery(con, "SELECT * FROM pcs_season_team WHERE season < 2023 AND season > 2013") %>%
  unique() %>%
  
  left_join(last_three_years %>%
              unique() %>%
              mutate(latest_year = latest_year + 1), by = c("rider", "season" = "latest_year")) %>%
  
  left_join(last_two_years %>%
              unique() %>%
              mutate(latest_year = latest_year + 1), by = c("rider", "season" = "latest_year")) %>%
  
  left_join(last_one_years %>%
              unique() %>%
              mutate(latest_year = latest_year + 1), by = c("rider", "season" = "latest_year")) %>%
  
  mutate(per_raceday.x = ifelse(is.na(per_raceday.x), 0, per_raceday.x),
         per_raceday.y = ifelse(is.na(per_raceday.y), 0, per_raceday.y),
         per_raceday = ifelse(is.na(per_raceday), 0, per_raceday),
         racedays.x = ifelse(is.na(racedays.x), 0, racedays.x),
         racedays.y = ifelse(is.na(racedays.y), 0, racedays.y),
         racedays = ifelse(is.na(racedays), 0, racedays)) %>%
  unique() %>%

  left_join(per_raceday %>%
              mutate(next_per_raceday = pcs_points/racedays) %>%
              select(rider, season, next_per_raceday), by = c("season", "rider")) %>%
  
  mutate(next_per_raceday = ifelse(is.na(next_per_raceday), 0, next_per_raceday))

#

predict_next_perraceday <- pts_predictor %>%
  mutate(level = ifelse(level %in% c("PCT", "PRT"), "PRT",
                        ifelse(level %in% c("P-CRO", "CT", "CLUB"), "CT", level))) %>%
  mutate(covid = ifelse(season == 2020, 1, 0), 
         vs_28 = abs(age-28), 
         vs_28 = ifelse(is.na(vs_28), 0, vs_28),
         age = ifelse(is.na(age), 28, age),
         racedays_l3 = racedays.x / 3,
         racedays_l2 = racedays.y / 2,
         racedays_l1 = racedays,
         regr_l3 = (per_raceday.x * racedays_l3) / (racedays_l3 + 10),
         regr_l2 = (per_raceday.y * racedays_l2) / (racedays_l2 + 10),
         regr_l1 = (per_raceday * racedays_l1) / (racedays_l1 + 10)) %>% 
  
  rename(per_raceday_l3 = per_raceday.x,
         per_raceday_l2 = per_raceday.y,
         per_raceday_l1 = per_raceday) %>%
  
  lm(next_per_raceday ~ vs_28 + age + regr_l3 + regr_l2 + regr_l1 + level, data = .)

summary(predict_next_perraceday)

#

x2023_final_preds <- x2023_teams %>%
  mutate(level = ifelse(level %in% c("PCT", "PRT"), "PRT",
                        ifelse(level %in% c("P-CRO", "CT", "CLUB"), "CT", level))) %>%
  mutate(covid = ifelse(season == 2020, 1, 0), 
         vs_28 = abs(age-28), 
         vs_28 = ifelse(is.na(vs_28), 0, vs_28),
         age = ifelse(is.na(age), 28, age),
         racedays_l3 = racedays.x / 3,
         racedays_l2 = racedays.y / 2,
         racedays_l1 = racedays,
         pcs_points_l1 = ifelse(is.na(pcs_points), 0, pcs_points),
         relyr = season - mean(season, na.rm = T),
         regr_l3 = (per_raceday.x * racedays_l3) / (racedays_l3 + 10),
         regr_l2 = (per_raceday.y * racedays_l2) / (racedays_l2 + 10),
         regr_l1 = (per_raceday * racedays_l1) / (racedays_l1 + 10)) %>% 
  
  rename(per_raceday_l3 = per_raceday.x,
         per_raceday_l2 = per_raceday.y,
         per_raceday_l1 = per_raceday) %>%
  
  mutate(predict_2024_perraceday = predict(predict_next_perraceday, .),
         predict_2024_racedays = predict(predict_next_racedays, .),
         predict_2024_perraceday = ifelse(predict_2024_perraceday <= 0, 0, predict_2024_perraceday),
         predicted_points = predict_2024_perraceday * predict_2024_racedays,
         
         project_improve = predicted_points - pcs_points_l1,
         
         predict_2024_racedays = ifelse(is.na(predict_2024_racedays), min(predict_2024_racedays, na.rm = T), predict_2024_racedays)) %>%
  select(-age) %>%
  mutate(rider_match = str_to_title(rider)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, date as dob FROM rider_attributes") %>%
      filter(!is.na(dob)) %>%
      mutate(rider = str_to_title(rider)) %>%
      unique() %>%
      group_by(rider) %>%
      summarize(dob = median(as.Date(dob), na.rm = T)) %>%
      ungroup(), by = c("rider_match" = "rider")) %>%
  
  mutate(age = as.numeric(as.Date(paste0(2024,"-06-01"))-as.Date(dob))/365.25) %>%
  
  select(-rider_match, -dob)

#

team_quality <- x2023_final_preds %>%
  
  filter(level %in% c("PRT", "WT")) %>%
  unique() %>%
  group_by(team) %>%
  mutate(rank = rank(desc(predicted_points), ties.method = "first"),
         rank = ifelse(rank <= 20, predicted_points, 0)) %>%
  ungroup() %>%
  
  mutate(predicted_points = ifelse(is.na(predicted_points), 0.1, predicted_points + 0.1)) %>%
  
  group_by(team = str_trim(team), level) %>%
  summarize(total_points = sum(predicted_points, na.rm = T),
            top_20 = sum(rank, na.rm = T),
            current_rider_2023 = sum(pcs_points_l1, na.rm = T),
            racedays = sum(predict_2023_racedays, na.rm = T),
            concentration = reldist::gini(x = predicted_points),
            qual_age = sum(predicted_points * age, na.rm = T)/sum(predicted_points, na.rm = T),
            age = sum(age * predict_2023_racedays, na.rm = T)/sum(predict_2023_racedays, na.rm = T),
            riders = n()) %>%
  ungroup() %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT DISTINCT rider, team, season, pcs_points FROM pcs_points_season_rider WHERE season = 2023") %>%
      group_by(team = str_trim(team), season) %>%
      summarize(x2023_points = sum(pcs_points, na.rm = T)) %>%
      ungroup() %>%
      mutate(team = case_when(team == "Quick-Step Alpha Vinyl Team" ~ "Soudal - Quick Step",
                              team == "Lotto Soudal" ~ "Lotto-Dstny",
                              team == "Intermarche - Wanty - Gobert Materiaux" ~ "Intermarche - Circus - Wanty",
                              team == "Team BikeExchange - Jayco" ~ "Team Jayco AlUla",
                              team == "Bardiani-CSF-Faizane" ~ "Green Project-Bardiani CSF-Faizane",
                              team == "Sport Vlaanderen - Baloise" ~ "Team Flanders - Baloise",
                              TRUE ~ team)), by = c("team")) %>%
  
  mutate(vs_2023_season = total_points - x2023_points,
         current_riders_delta = total_points - current_rider_2023)

#
#
#
#
#

teams_data <- dbReadTable(con, "pcs_linked_teams") %>%
  filter(as.numeric(linked_season >= 2010) & as.numeric(linked_season) <= 2023) %>%
  mutate(linked_team_name = stringi::stri_trans_general(str = linked_team_name, id = "Latin-ASCII"),
         linked_season = as.numeric(linked_season),
         team = str_trim(team)) %>%
  unique()

teams_data <- teams_data %>%
  
  filter(season == linked_season) %>%
  
  inner_join(
    teams_data %>%
      group_by(team) %>% 
      mutate(master_team = ifelse(max(linked_season) == linked_season, linked_team_name, NA)) %>%
      ungroup() %>%
      arrange(team_url, desc(linked_team_url)) %>%
      group_by(team_url) %>%
      fill(master_team, .direction = "down") %>%
      ungroup() %>%
      select(team_url, master_team) %>%
      unique(), by = c("team_url")) %>% 
  select(master_team, team = linked_team_name, season = linked_season) %>% 
  unique() %>%
  filter(!is.na(master_team))

#

prior_year_leadership <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE year >= 2011") %>%
  mutate(rider = case_when(rider == "O Connor Ben" ~ "O'connor Ben",
                           rider == "O Brien Kelland" ~ "O'brien Kelland",
                           rider == "Ghirmay Hailu Biniam" ~ "Girmay Biniam",
                           rider == "Girmay Hailu Biniam" ~ "Girmay Biniam",
                           rider == "O Donnell Bailey" ~ "O'donnell Bailey",
                           rider == "D Heygere Gil" ~ "D'heygere Gil",
                           rider == "Skjelmose Jensen Mattias" ~ "Skjelmose Mattias",
                           rider == "Wright Alfred" ~ "Wright Fred",
                           TRUE ~ rider)) %>%

  mutate(rider = str_trim(rider),
         rider = str_to_title(rider)) %>%
  filter(!str_detect(url, "-itt")) %>%
  filter(time_trial == 0 & team_time_trial == 0) %>%
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  mutate(uphill_finish = ifelse(is.na(uphill_finish), mean(uphill_finish, na.rm = T), uphill_finish)) %>%
  
  mutate(team_ldr = ifelse(tm_pos == 1, 1, 0)) %>%
  
  mutate(date = as.Date(date)) %>%
  
  select(-speed, -gain_3rd, -gain_5th, -gain_10th, -gain_40th,
         -time_trial, -gc_winner, -gc_pos, -parcours_value, -stage_type,
         -avg_alt, -missing_profile_data) %>%
  
  mutate(race_type = ifelse(bunch_sprint == 1, "BS",
                            ifelse(uphill_finish == 1 | pred_climb_difficulty >= 6, "Hills", "Flat")))

pyl_riders <- prior_year_leadership %>%
  group_by(rider, race_type, year) %>%
  summarize(race_days = n(),
            leadership = sum(team_ldr, na.rm = T),
            perc = mean(team_ldr, na.rm = T),
            scaled_leadership = sum(1 / tm_pos, na.rm = T)/n()) %>%
  ungroup()

pyl_riders_flat <- prior_year_leadership %>%
  group_by(rider, race_type, year) %>%
  summarize(scaled_leadership = sum(rnk <= 10, na.rm = T),
            racedays = n()) %>%
  ungroup() %>%
  
  mutate(scaled_leadership = ((scaled_leadership) + 0.5)/(racedays + 10)) %>%
  select(-racedays) %>%
  spread(race_type, scaled_leadership, fill = 0.05) %>%
  mutate(Tot = Hills+BS+Flat) %>%
  mutate(Hills = Hills / (Tot),
         BS = BS / (Tot),
         Flat = Flat / (Tot))

pyl_teams <- prior_year_leadership %>%
  select(-team) %>%
  inner_join(dbGetQuery(con, "SELECT * FROM pcs_season_team_riders WHERE season > 2011") %>%
               unique() %>%
               select(team, level, rider, season) %>%
               rbind(dbGetQuery(con, "SELECT * FROM pcs_season_team WHERE season = 2023") %>%
                       unique() %>%
                       select(team, level, rider, season)) %>%
               mutate(year = season), by = c("year", "rider")) %>%
  filter(!class %in% c("CC", "NC", "WC")) %>%
  select(team, level, season, url, stage, length, date, class, race, race_type, year) %>%
  unique() %>%
  
  group_by(team, level, race_type, year) %>%
  summarize(distinct_race_days = n()) %>%
  ungroup() %>%
  
  mutate(season = year + 1)

#
#
#

leadership_on_team <- dbGetQuery(con, "SELECT * FROM pcs_season_team_riders WHERE season > 2011") %>%
  unique() %>%
  select(team, level, rider, season) %>%
  rbind(dbGetQuery(con, "SELECT * FROM pcs_season_team WHERE season = 2023") %>%
      unique() %>%
        select(team, level, rider, season)) %>%
  mutate(team = str_trim(team)) %>%
  inner_join(teams_data, by = c("team", "season")) %>%
  
  inner_join(pyl_riders %>%
              mutate(year = year + 1), by = c("rider", "season" = "year")) %>%
  
  inner_join(pyl_teams %>%
               mutate(team = str_trim(team)) %>%
               inner_join(teams_data, by = c("team", "year" = "season")) %>%
               select(-team, -level), by = c("master_team", "season", "race_type")) %>%
  
  mutate(perc = ifelse(is.na(perc), 0.1, perc),
         leadership = ifelse(is.na(leadership), 0, leadership),
         scaled_leadership = ifelse(is.na(scaled_leadership), 0.25, scaled_leadership),
         race_days = ifelse(is.na(race_days), 1, race_days)) %>%
  
  group_by(team, season, level, race_type) %>%
  summarize(leadership = sum(leadership),
            scaled_leadership = sum(scaled_leadership * race_days) / sum(race_days),
            team_racedays = mean(distinct_race_days, na.rm = T),
            riders = n()) %>%
  ungroup() %>%
  
  mutate(perc = leadership / team_racedays)

#
#
#

riders_on_team <- dbGetQuery(con, "SELECT * FROM pcs_season_team_riders WHERE season > 2011") %>%
  unique() %>%
  select(team, level, rider, season) %>%
  rbind(dbGetQuery(con, "SELECT * FROM pcs_season_team WHERE season = 2023") %>%
          unique() %>%
          select(team, level, rider, season)) %>%
  mutate(team = str_trim(team)) %>%
  
  inner_join(leadership_on_team %>%
               select(team, season, level, leadership_saturation = perc, race_type), by = c("team", "season", "level")) %>%
  
  left_join(pyl_riders_flat %>%
              mutate(year = year + 1) %>%
              select(-Tot) %>%
              gather(race_type, affinity, -c(year, rider)), by = c("season" = "year", "rider", "race_type")) %>%
  
  mutate(affinity = ifelse(is.na(affinity), 1 / 3, affinity)) %>%
  
  group_by(rider, level, team, season) %>%
  summarize(leadership_saturation = sum(leadership_saturation * (affinity), na.rm = T)) %>%
  ungroup()
