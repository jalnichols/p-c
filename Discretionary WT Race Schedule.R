
library(tidyverse)
library(RMySQL)

dbDisconnect(con)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

All_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE year IN (2019, 2020, 2021)")

pcs_points <- dbGetQuery(con, "SELECT * FROM pcs_stage_pts WHERE year IN (2019, 2020, 2021)")

#

OnlyWT_Teams <- All_data %>%
  
  mutate(master_team = ifelse(master_team == "EF Education Firs", "EF Education First", master_team)) %>%
  
  filter(master_team %in% c("Quick Step",
                            "BORA",
                            "Trek",
                            "UAE Team",
                            "Astana",
                            "Sunweb",
                            "NTT",
                            "Sky",
                            "Movistar",
                            "Bahrain McLaren",
                            "AG2R",
                            "Lotto Soudal",
                            "Mitchelton Scott",
                            "FDJ",
                            "Jumbo Visma",
                            "Cofidis",
                            "Israel Startup Nation",
                            "EF Education First",
                            "BMC Racing",
                            "Wanty Gobert",
                            "Arkea Samsic",
                            "Direct Energie",
                            "Alpecin Fenix",
                            "Katusha")) %>%
  
  filter(!class %in% c("NC", "1.UWT", "2.UWT", "CC", "WC", "2.Ncup", "1.Ncup", "1.2", "2.2", "1.2U", "2.2U")) %>%
  
  filter(tm_pos == 1) %>%
  
  mutate(one_day_race = ifelse(str_sub(class,1,1) == "1",1,0))

#
#
#

discretionary_races <- OnlyWT_Teams %>%
  
  filter(!(master_team == 'Cofidis' & year < 2020)) %>%
  filter(!(master_team == 'Israel Startup Nation' & year < 2020)) %>%
  filter(!(master_team == 'Alpecin Fenix' & year < 2021)) %>% 
  filter(!(master_team == 'Direct Energie' & year < 2021)) %>% 
  filter(!(master_team == 'Arkea Samsic' & year < 2021)) %>% 
  filter(!(master_team == 'Wanty Gobert' & year < 2021)) %>% 
  
  mutate(ODRs = ifelse(one_day_race == 1, race, ""),
         StageRace = ifelse(one_day_race == 0, race, "")) %>%
  
  group_by(year) %>%
  mutate(unique_ODRs = n_distinct(ODRs)-1,
         unique_stageRaces = n_distinct(StageRace)-1) %>%
  ungroup() %>%
  
  group_by(master_team, year) %>%
  summarize(ODR_rate = mean(one_day_race, na.rm = T),
            ODR_BS_rate = mean(one_day_race == 1 & bunch_sprint == 1, na.rm = T),
            bunchsprints = mean(bunch_sprint, na.rm = T),
            PCD = mean(pred_climb_difficulty, na.rm = T),
            Mountain_races = mean(pred_climb_difficulty > 6, na.rm = T),
            one_day_races = sum(one_day_race, na.rm = T),
            unique_races = n_distinct(race, year, class),
            possibleODR = max(unique_ODRs),
            possibleStage = max(unique_stageRaces),
            pro_hc = mean(class %in% c("1.Pro", "2.Pro", "1.HC", "2.HC")),
            sof = mean(sof, na.rm = T),
            race_days = n()) %>%
  ungroup() %>%
  
  mutate(stage_races = unique_races - one_day_races)

#

discretionary_races %>%
  
  select(master_team, year, 
         `One day races (%)` = ODR_rate, 
         `Bunch Sprints (%)` = bunchsprints, 
         `Climbing Races (%)` = Mountain_races) %>%
  
  gather(stat, value, -c("master_team", "year")) %>%
  
  filter(year == 2021) %>%
  
  mutate(master_team = case_when(master_team == "Wanty Gobert" ~ "Intermarche Wanty",
                                 master_team == "Sky" ~ "INEOS",
                                 master_team == "EF Education First" ~ "EF - Nippo",
                                 master_team == "Mitchelton Scott" ~ "BikeExchange",
                                 master_team == "NTT" ~ "Qhubeka",
                                 master_team == "Bahrain McLaren" ~ "Bahrain",
                                 master_team == "Sunweb" ~ "Team DSM",
                                 TRUE ~ master_team)) %>%
  
  ggplot()+
  geom_vline(xintercept = 1)+
  geom_point(aes(y = value, x = 1), color = "#F70909", size = 3)+
  ggrepel::geom_text_repel(aes(y = value, x = 1, label = master_team), angle = 0)+
  
  facet_wrap(~stat, scales = "free_y", nrow = 1)+
  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = 'white', face = "bold", size = 14))+
  
  scale_y_continuous(breaks = scales::pretty_breaks(7),
                     labels = scales::percent_format())+
  
  labs(x = "",
       y = "",
       title = "Discretionary races (non-WT) for 2021 teams")

#
#
#
#
#

PCS_PTS_Discipline <- pcs_points %>%
  
  mutate(stage = as.character(stage)) %>%
  mutate(race = str_to_lower(race),
         rider = str_to_title(rider)) %>%
  mutate(pcs_pts = ifelse(is.na(pcs_pts), 0, pcs_pts)) %>%
  
  left_join(All_data %>%
              select(master_team, stage, race, year, class, date, length, rider,
                     pred_climb_difficulty, bunch_sprint, rnk), 
            by = c("rider", "stage", "race", "year", "class", "rnk")) %>%
  
  mutate(master_team = ifelse(master_team == "EF Education Firs", "EF Education First", master_team)) %>%
  
  filter(master_team %in% c("Quick Step",
                            "BORA",
                            "Trek",
                            "UAE Team",
                            "Astana",
                            "Sunweb",
                            "NTT",
                            "Sky",
                            "Movistar",
                            "Bahrain McLaren",
                            "AG2R",
                            "Lotto Soudal",
                            "Mitchelton Scott",
                            "FDJ",
                            "Jumbo Visma",
                            "Cofidis",
                            "Israel Startup Nation",
                            "EF Education First",
                            "BMC Racing",
                            "Wanty Gobert",
                            "Direct Energie",
                            "Arkea Samsic",
                            "Alpecin Fenix",
                            "Katusha")) %>%
  
  filter(!class %in% c("NC", "2.Ncup", "1.Ncup", "1.2", "2.2", "1.2U", "2.2U")) %>%
  
  mutate(one_day_race = ifelse(str_sub(class,1,1) == "1",1,0))

#
#
#

perf_by_discipline <- PCS_PTS_Discipline %>%
  
  filter(!(master_team == 'Cofidis' & year < 2020)) %>%
  filter(!(master_team == 'Israel Startup Nation' & year < 2020)) %>%
  filter(!(master_team == 'Alpecin Fenix' & year < 2021)) %>%  
  filter(!(master_team == 'Arkea Samsic' & year < 2021)) %>%  
  filter(!(master_team == 'Direct Energie' & year < 2021)) %>%  
  filter(!(master_team == 'Wanty Gobert' & year < 2021)) %>% 
  
  mutate(master_team = case_when(master_team == "Wanty Gobert" ~ "Intermarche Wanty",
                                 master_team == "Sky" ~ "INEOS",
                                 master_team == "EF Education First" ~ "EF - Nippo",
                                 master_team == "Mitchelton Scott" ~ "BikeExchange",
                                 master_team == "NTT" ~ "Qhubeka",
                                 master_team == "Bahrain McLaren" ~ "Bahrain",
                                 master_team == "Sunweb" ~ "Team DSM",
                                 TRUE ~ master_team)) %>%
  
  mutate(race_type = ifelse(bunch_sprint == 1, 
                            ifelse(one_day_race == 1, "ODR/BS", "BS"),
                            ifelse(pred_climb_difficulty >= 6, "CLIMB",
                                   ifelse(one_day_race == 1, "ODR",
                                          ifelse(time_trial == 1, "TT", "RACE"))))) %>%
  
  group_by(master_team, year, race_type) %>%
  summarize(PCS_PTS = sum(pcs_pts, na.rm = T),
            race_days = n_distinct(stage, race, year, class)) %>%
  ungroup() %>%
  
  group_by(master_team, year) %>%
  mutate(PERC_PCS_PTS = PCS_PTS / sum(PCS_PTS),
         PCS_PTS_total = sum(PCS_PTS),
         race_days_total = sum(race_days)) %>%
  ungroup() %>%
  
  mutate(PCS_PTS_raceday = PCS_PTS/race_days,
         PCS_PTS_other_raceday = (PCS_PTS_total - PCS_PTS) / (race_days_total - race_days))

#
#
#

perf_by_discipline_rider <- PCS_PTS_Discipline %>%
  
  #filter(!(master_team == 'Cofidis' & year < 2020)) %>%
  #filter(!(master_team == 'Israel Startup Nation' & year < 2020)) %>%
  #filter(!(master_team == 'Alpecin Fenix' & year < 2021)) %>%  
  #filter(!(master_team == 'Arkea Samsic' & year < 2021)) %>%  
  #filter(!(master_team == 'Direct Energie' & year < 2021)) %>%  
  #filter(!(master_team == 'Wanty Gobert' & year < 2021)) %>% 
  
  mutate(master_team = case_when(master_team == "Wanty Gobert" ~ "Intermarche Wanty",
                                 master_team == "Sky" ~ "INEOS",
                                 master_team == "EF Education First" ~ "EF - Nippo",
                                 master_team == "Mitchelton Scott" ~ "BikeExchange",
                                 master_team == "NTT" ~ "Qhubeka",
                                 master_team == "Bahrain McLaren" ~ "Bahrain",
                                 master_team == "Sunweb" ~ "Team DSM",
                                 TRUE ~ master_team)) %>%
  
  mutate(race_type = ifelse(bunch_sprint == 1, 
                            ifelse(one_day_race == 1, "ODR", "BS"),
                            ifelse(pred_climb_difficulty >= 6, "CLIMB",
                                   ifelse(one_day_race == 1, "ODR",
                                          ifelse(time_trial == 1, "TT", "RACE"))))) %>%
  
  group_by(rider, race_type) %>%
  summarize(PCS_PTS = sum(pcs_pts, na.rm = T),
            race_days = n_distinct(stage, race, year, class)) %>%
  ungroup() %>%
  
  group_by(rider) %>%
  mutate(PERC_PCS_PTS = PCS_PTS / sum(PCS_PTS),
         PCS_PTS_total = sum(PCS_PTS),
         race_days_total = sum(race_days)) %>%
  ungroup() %>%
  
  mutate(PCS_PTS_raceday = PCS_PTS/race_days,
         perc_racedays = race_days/race_days_total)

#

perf_by_discipline_rider %>%
  filter(race_type == "ODR" & race_days_total >= 80) %>%
  
  ggplot(aes(x = perc_racedays, y = PCS_PTS_raceday, label = rider))+
  geom_text()+
  labs(x = "Percentage of racedays in ODR", y = "PCS Pts per raceday", title = "2019-21 PCS Pts per race day in ODR")+scale_x_continuous(labels = scales::percent)
                     