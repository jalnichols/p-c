

library(tidyverse)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

stage_data_perf <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE year = 2021") 


#

leaders_in_2021 <- stage_data_perf %>%
  
  filter(time_trial == 0 & !class %in% c("WC", "Olympics", "CC")) %>%
  
  mutate(level_team = ifelse(master_team %in% c("Israel Startup Nation", "Cofidis", "AG2R", "Wanty Gobert",
                                                "UAE Team", "Quick Step", "FDJ", "Trek", "BORA", "Movistar",
                                                "Sunweb", "Astana", "NTT", "Sky", "Mitchelton Scott",
                                                "Lotto Soudal", "Jumbo Visma", "Bahrain McLaren", "EF Education Firs"), "WT",
                             ifelse(master_team %in% c("Alpecin Fenix", "Arkea Samsic", "Direct Energie", "Vital Concept",
                                                       "Wallonie Bruxelles", "Euskaltel - Euskadi", "Bardiani",
                                                       "Caja Rural", "Androni", "Kern Pharma", "Burgos",
                                                       "Uno X Norway", "Gazprom", "Rally", "Eolo Kometa", 
                                                       "Vini Zabu", "Novo Nordisk", "Delko", 
                                                       "Sport Vlaanderen"), "PT", "CT"))) %>%
  
  group_by(rider) %>%
  summarize(races = n_distinct(stage, race, year, class, date, length),
            leaders = sum(tm_pos == 1, na.rm = T),
            leaders_top = sum(tm_pos == 1 & rnk <= 20, na.rm = T),
            leaders_wt = sum(tm_pos == 1 & level_team %in% c("WT"), na.rm = T),
            leaders_pt = sum(tm_pos == 1 & level_team %in% c("PT"), na.rm = T),
            leaders_lower = sum(tm_pos == 1 & !level_team %in% c("WT", "PT"), na.rm = T)) %>%
  ungroup() %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT DISTINCT rider, level, team FROM team_rosters_season WHERE Season = 2022") %>%
      mutate(team = str_trim(team)), by = c("rider")) %>%
  
  mutate(master_team =  case_when(team == "AG2R Citroen Team" ~ "AG2R",
                                  team == "Astana Qazaqstan Team" ~ "Astana",
                                  team == "Bahrain - Victorious" ~ "Bahrain McLaren",
                                  team == "BORA - hansgrohe" ~ "BORA",
                                  team == "Cofidis" ~ "Cofidis", 
                                  team == "EF Education-EasyPost" ~ "EF Education Firs",
                                  team == "Groupama - FDJ" ~ "FDJ",
                                  team == "INEOS Grenadiers" ~ "Sky",
                                  team == "Intermarche - Wanty - Gobert Materiaux" ~ "Wanty Gobert", 
                                  team == "Israel - Premier Tech" ~ "Israel Startup Nation",
                                  team == "Lotto Soudal" ~ "Lotto Soudal",
                                  team == "Movistar Team" ~ "Movistar",
                                  team == "Quick-Step Alpha Vinyl Team" ~ "Quick Step", 
                                  team == "Team BikeExchange Jayco" ~ "Mitchelton Scott",
                                  team == "Team DSM" ~ "Sunweb",
                                  team == "Team Jumbo-Visma" ~ "Jumbo Visma", 
                                  team == "Team Qhubeka NextHash" ~ "Team Qhubeka NextHash",
                                  team == "Trek - Segafredo" ~ "Trek",
                                  team == "UAE-Team Emirates" ~ "UAE Team",
                                  team == "Alpecin-Fenix" ~ "Alpecin Fenix",
                                  team == "B&B Hotels - KTM" ~ "Vital Concept",
                                  team == "Bardiani-CSF-Faizane" ~ "Bardiani",
                                  team == "Bingoal Pauwels Sauces WB" ~ "Wallonie Bruxelles",
                                  team == "Burgos-BH" ~ "Burgos",
                                  team == "Caja Rural - Seguros RGA" ~ "Caja Rural",
                                  team == "Drone Hopper -  Androni Giocattoli" ~ "Androni", 
                                  team == "EOLO-Kometa" ~ "Eolo Kometa",
                                  team == "Equipo Kern Pharma" ~ "Kern Pharma", 
                                  team == "Euskaltel - Euskadi" ~ "Euskaltel - Euskadi",
                                  team == "Gazprom - RusVelo" ~ "Gazprom",
                                  team == "Human Powered Health" ~ "Rally",
                                  team == "Sport Vlaanderen - Baloise" ~ "Sport Vlaanderen",
                                  team == "Team Arkea Samsic" ~ "Arkea Samsic",
                                  team == "Team Novo Nordisk" ~ "Novo Nordisk",
                                  team == "Team TotalEnergies" ~ "Direct Energie",
                                  team == "Uno-X Pro Cycling Team" ~ "Uno X Norway",
                                  TRUE ~ team)) %>%
  inner_join(
    stage_data_perf %>%
      
      filter(time_trial == 0 & tm_pos == 1) %>%
      
      mutate(master_team = ifelse(master_team == "X", team, master_team)) %>%
      
      group_by(master_team) %>%
      summarize(distinct_races = n_distinct(stage, race, year, class, date, length),
                leader_top_20s = sum(rnk <= 20, na.rm = T)) %>%
      ungroup(), by = c("master_team"))
    
#
#
#
#
#

projecting_for_2022 <- leaders_in_2021 %>%
  
  group_by(team, level) %>%
  summarize(leaders_2021 = sum(leaders, na.rm = T),
            leaders_top_2021 = sum(leaders_top, na.rm = T),
            leaders_wt_2021 = sum(leaders_wt, na.rm = T),
            leaders_pt_2021 = sum(leaders_pt, na.rm = T),
            leaders_lower_2021 = sum(leaders_lower, na.rm = T),
            distinct_races = mean(distinct_races, na.rm = T),
            leader_top_20s = mean(leader_top_20s, na.rm = T),
            riders = n()) %>%
  ungroup() %>%
  
  mutate(own_level_leaders = ifelse(level == "WT", leaders_wt_2021/leaders_2021,
                                    ifelse(level == "PRT", (leaders_wt_2021+leaders_pt_2021)/leaders_2021, NA))) %>%
  
  mutate(leadership_opps_ratio = leaders_2021/distinct_races,
         leadership_opps_ratio2 = leaders_top_2021/leader_top_20s,
         leadership_opps_ratio3 = (leaders_wt_2021+leaders_pt_2021)/distinct_races,
         leadership_opps_ratio4 = ifelse(level == "WT",
                                         (leaders_wt_2021 + (0.67*leaders_pt_2021) + (0.33*leaders_lower_2021))/distinct_races,
                                         ((1.5 * leaders_wt_2021) + (leaders_pt_2021) + (0.5*leaders_lower_2021))/distinct_races))

#
#
#

leaders_hillmtns_in_2021 <- stage_data_perf %>%
  
  filter(time_trial == 0 & !class %in% c("WC", "Olympics", "CC") & pred_climb_difficulty > 5) %>%
  
  mutate(level_team = ifelse(master_team %in% c("Israel Startup Nation", "Cofidis", "AG2R", "Wanty Gobert",
                                                "UAE Team", "Quick Step", "FDJ", "Trek", "BORA", "Movistar",
                                                "Sunweb", "Astana", "NTT", "Sky", "Mitchelton Scott",
                                                "Lotto Soudal", "Jumbo Visma", "Bahrain McLaren", "EF Education Firs"), "WT",
                             ifelse(master_team %in% c("Alpecin Fenix", "Arkea Samsic", "Direct Energie", "Vital Concept",
                                                       "Wallonie Bruxelles", "Euskaltel - Euskadi", "Bardiani",
                                                       "Caja Rural", "Androni", "Kern Pharma", "Burgos",
                                                       "Uno X Norway", "Gazprom", "Rally", "Eolo Kometa", 
                                                       "Vini Zabu", "Novo Nordisk", "Delko", 
                                                       "Sport Vlaanderen"), "PT", "CT"))) %>%
  
  group_by(rider) %>%
  summarize(races = n_distinct(stage, race, year, class, date, length),
            leaders = sum(tm_pos == 1, na.rm = T),
            leaders_top = sum(tm_pos == 1 & rnk <= 20, na.rm = T),
            leaders_wt = sum(tm_pos == 1 & level_team %in% c("WT"), na.rm = T),
            leaders_pt = sum(tm_pos == 1 & level_team %in% c("PT"), na.rm = T),
            leaders_lower = sum(tm_pos == 1 & !level_team %in% c("WT", "PT"), na.rm = T)) %>%
  ungroup() %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT DISTINCT rider, level, team FROM team_rosters_season WHERE Season = 2022") %>%
      mutate(team = str_trim(team)), by = c("rider")) %>%
  
  mutate(master_team =  case_when(team == "AG2R Citroen Team" ~ "AG2R",
                                  team == "Astana Qazaqstan Team" ~ "Astana",
                                  team == "Bahrain - Victorious" ~ "Bahrain McLaren",
                                  team == "BORA - hansgrohe" ~ "BORA",
                                  team == "Cofidis" ~ "Cofidis", 
                                  team == "EF Education-EasyPost" ~ "EF Education Firs",
                                  team == "Groupama - FDJ" ~ "FDJ",
                                  team == "INEOS Grenadiers" ~ "Sky",
                                  team == "Intermarche - Wanty - Gobert Materiaux" ~ "Wanty Gobert", 
                                  team == "Israel - Premier Tech" ~ "Israel Startup Nation",
                                  team == "Lotto Soudal" ~ "Lotto Soudal",
                                  team == "Movistar Team" ~ "Movistar",
                                  team == "Quick-Step Alpha Vinyl Team" ~ "Quick Step", 
                                  team == "Team BikeExchange Jayco" ~ "Mitchelton Scott",
                                  team == "Team DSM" ~ "Sunweb",
                                  team == "Team Jumbo-Visma" ~ "Jumbo Visma", 
                                  team == "Team Qhubeka NextHash" ~ "Team Qhubeka NextHash",
                                  team == "Trek - Segafredo" ~ "Trek",
                                  team == "UAE-Team Emirates" ~ "UAE Team",
                                  team == "Alpecin-Fenix" ~ "Alpecin Fenix",
                                  team == "B&B Hotels - KTM" ~ "Vital Concept",
                                  team == "Bardiani-CSF-Faizane" ~ "Bardiani",
                                  team == "Bingoal Pauwels Sauces WB" ~ "Wallonie Bruxelles",
                                  team == "Burgos-BH" ~ "Burgos",
                                  team == "Caja Rural - Seguros RGA" ~ "Caja Rural",
                                  team == "Drone Hopper -  Androni Giocattoli" ~ "Androni", 
                                  team == "EOLO-Kometa" ~ "Eolo Kometa",
                                  team == "Equipo Kern Pharma" ~ "Kern Pharma", 
                                  team == "Euskaltel - Euskadi" ~ "Euskaltel - Euskadi",
                                  team == "Gazprom - RusVelo" ~ "Gazprom",
                                  team == "Human Powered Health" ~ "Rally",
                                  team == "Sport Vlaanderen - Baloise" ~ "Sport Vlaanderen",
                                  team == "Team Arkea Samsic" ~ "Arkea Samsic",
                                  team == "Team Novo Nordisk" ~ "Novo Nordisk",
                                  team == "Team TotalEnergies" ~ "Direct Energie",
                                  team == "Uno-X Pro Cycling Team" ~ "Uno X Norway",
                                  TRUE ~ team)) %>%
  left_join(
    stage_data_perf %>%
      
      filter(time_trial == 0 & tm_pos == 1 & pred_climb_difficulty > 5) %>%
      
      mutate(master_team = ifelse(master_team == "X", team, master_team)) %>%
      
      group_by(master_team) %>%
      summarize(distinct_races = n_distinct(stage, race, year, class, date, length),
                leader_top_20s = sum(rnk <= 20, na.rm = T)) %>%
      ungroup(), by = c("master_team"))

#


projecting_hillmtns_for_2022 <- leaders_hillmtns_in_2021 %>%
  
  group_by(team, level) %>%
  summarize(leaders_2021 = sum(leaders, na.rm = T),
            leaders_top_2021 = sum(leaders_top, na.rm = T),
            leaders_wt_2021 = sum(leaders_wt, na.rm = T),
            leaders_pt_2021 = sum(leaders_pt, na.rm = T),
            leaders_lower_2021 = sum(leaders_lower, na.rm = T),
            distinct_races = mean(distinct_races, na.rm = T),
            leader_top_20s = mean(leader_top_20s, na.rm = T),
            riders = n()) %>%
  ungroup() %>%
  
  mutate(own_level_leaders = ifelse(level == "WT", leaders_wt_2021/leaders_2021,
                                    ifelse(level == "PRT", (leaders_wt_2021+leaders_pt_2021)/leaders_2021, NA))) %>%
  
  mutate(leadership_opps_ratio = leaders_2021/distinct_races,
         leadership_opps_ratio2 = leaders_top_2021/leader_top_20s,
         leadership_opps_ratio3 = (leaders_wt_2021+leaders_pt_2021)/distinct_races,
         leadership_opps_ratio4 = ifelse(level == "WT",
                                         (leaders_wt_2021 + (0.67*leaders_pt_2021) + (0.33*leaders_lower_2021))/distinct_races,
                                         ((1.5 * leaders_wt_2021) + (leaders_pt_2021) + (0.5*leaders_lower_2021))/distinct_races))

#
#
#

leaders_flats_in_2021 <- stage_data_perf %>%
  
  filter(time_trial == 0 & !class %in% c("WC", "Olympics", "CC") & pred_climb_difficulty <= 5) %>%
  
  mutate(level_team = ifelse(master_team %in% c("Israel Startup Nation", "Cofidis", "AG2R", "Wanty Gobert",
                                                "UAE Team", "Quick Step", "FDJ", "Trek", "BORA", "Movistar",
                                                "Sunweb", "Astana", "NTT", "Sky", "Mitchelton Scott",
                                                "Lotto Soudal", "Jumbo Visma", "Bahrain McLaren", "EF Education Firs"), "WT",
                             ifelse(master_team %in% c("Alpecin Fenix", "Arkea Samsic", "Direct Energie", "Vital Concept",
                                                       "Wallonie Bruxelles", "Euskaltel - Euskadi", "Bardiani",
                                                       "Caja Rural", "Androni", "Kern Pharma", "Burgos",
                                                       "Uno X Norway", "Gazprom", "Rally", "Eolo Kometa", 
                                                       "Vini Zabu", "Novo Nordisk", "Delko", 
                                                       "Sport Vlaanderen"), "PT", "CT"))) %>%
  
  group_by(rider) %>%
  summarize(races = n_distinct(stage, race, year, class, date, length),
            leaders = sum(tm_pos == 1, na.rm = T),
            leaders_top = sum(tm_pos == 1 & rnk <= 20, na.rm = T),
            leaders_wt = sum(tm_pos == 1 & level_team %in% c("WT"), na.rm = T),
            leaders_pt = sum(tm_pos == 1 & level_team %in% c("PT"), na.rm = T),
            leaders_lower = sum(tm_pos == 1 & !level_team %in% c("WT", "PT"), na.rm = T)) %>%
  ungroup() %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT DISTINCT rider, level, team FROM team_rosters_season WHERE Season = 2022") %>%
      mutate(team = str_trim(team)), by = c("rider")) %>%
  
  mutate(master_team =  case_when(team == "AG2R Citroen Team" ~ "AG2R",
                                  team == "Astana Qazaqstan Team" ~ "Astana",
                                  team == "Bahrain - Victorious" ~ "Bahrain McLaren",
                                  team == "BORA - hansgrohe" ~ "BORA",
                                  team == "Cofidis" ~ "Cofidis", 
                                  team == "EF Education-EasyPost" ~ "EF Education Firs",
                                  team == "Groupama - FDJ" ~ "FDJ",
                                  team == "INEOS Grenadiers" ~ "Sky",
                                  team == "Intermarche - Wanty - Gobert Materiaux" ~ "Wanty Gobert", 
                                  team == "Israel - Premier Tech" ~ "Israel Startup Nation",
                                  team == "Lotto Soudal" ~ "Lotto Soudal",
                                  team == "Movistar Team" ~ "Movistar",
                                  team == "Quick-Step Alpha Vinyl Team" ~ "Quick Step", 
                                  team == "Team BikeExchange Jayco" ~ "Mitchelton Scott",
                                  team == "Team DSM" ~ "Sunweb",
                                  team == "Team Jumbo-Visma" ~ "Jumbo Visma", 
                                  team == "Team Qhubeka NextHash" ~ "Team Qhubeka NextHash",
                                  team == "Trek - Segafredo" ~ "Trek",
                                  team == "UAE-Team Emirates" ~ "UAE Team",
                                  team == "Alpecin-Fenix" ~ "Alpecin Fenix",
                                  team == "B&B Hotels - KTM" ~ "Vital Concept",
                                  team == "Bardiani-CSF-Faizane" ~ "Bardiani",
                                  team == "Bingoal Pauwels Sauces WB" ~ "Wallonie Bruxelles",
                                  team == "Burgos-BH" ~ "Burgos",
                                  team == "Caja Rural - Seguros RGA" ~ "Caja Rural",
                                  team == "Drone Hopper -  Androni Giocattoli" ~ "Androni", 
                                  team == "EOLO-Kometa" ~ "Eolo Kometa",
                                  team == "Equipo Kern Pharma" ~ "Kern Pharma", 
                                  team == "Euskaltel - Euskadi" ~ "Euskaltel - Euskadi",
                                  team == "Gazprom - RusVelo" ~ "Gazprom",
                                  team == "Human Powered Health" ~ "Rally",
                                  team == "Sport Vlaanderen - Baloise" ~ "Sport Vlaanderen",
                                  team == "Team Arkea Samsic" ~ "Arkea Samsic",
                                  team == "Team Novo Nordisk" ~ "Novo Nordisk",
                                  team == "Team TotalEnergies" ~ "Direct Energie",
                                  team == "Uno-X Pro Cycling Team" ~ "Uno X Norway",
                                  TRUE ~ team)) %>%
  inner_join(
    stage_data_perf %>%
      
      filter(time_trial == 0 & tm_pos == 1 & pred_climb_difficulty <= 5) %>%
      
      mutate(master_team = ifelse(master_team == "X", team, master_team)) %>%
      
      group_by(master_team) %>%
      summarize(distinct_races = n_distinct(stage, race, year, class, date, length),
                leader_top_20s = sum(rnk <= 20, na.rm = T)) %>%
      ungroup(), by = c("master_team"))

#


projecting_flats_for_2022 <- leaders_flats_in_2021 %>%
  
  group_by(team, level) %>%
  summarize(leaders_2021 = sum(leaders, na.rm = T),
            leaders_top_2021 = sum(leaders_top, na.rm = T),
            leaders_wt_2021 = sum(leaders_wt, na.rm = T),
            leaders_pt_2021 = sum(leaders_pt, na.rm = T),
            leaders_lower_2021 = sum(leaders_lower, na.rm = T),
            distinct_races = mean(distinct_races, na.rm = T),
            leader_top_20s = mean(leader_top_20s, na.rm = T),
            riders = n()) %>%
  ungroup() %>%
  
  mutate(own_level_leaders = ifelse(level == "WT", leaders_wt_2021/leaders_2021,
                                    ifelse(level == "PRT", (leaders_wt_2021+leaders_pt_2021)/leaders_2021, NA))) %>%
  
  mutate(leadership_opps_ratio = leaders_2021/distinct_races,
         leadership_opps_ratio2 = leaders_top_2021/leader_top_20s,
         leadership_opps_ratio3 = (leaders_wt_2021+leaders_pt_2021)/distinct_races,
         leadership_opps_ratio4 = ifelse(level == "WT",
                                         (leaders_wt_2021 + (0.67*leaders_pt_2021) + (0.33*leaders_lower_2021))/distinct_races,
                                         ((1.5 * leaders_wt_2021) + (leaders_pt_2021) + (0.5*leaders_lower_2021))/distinct_races))