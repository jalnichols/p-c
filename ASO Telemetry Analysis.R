
library(tidyverse)
library(rjson)
library(RMySQL)
library(DBI)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

STAGE <- 19

d <- readr::read_rds(paste0('Stage Telemetry/tdf-stage', STAGE, '-telemetry.rds'))

#

results_list <- vector("list", length(d))

for(x in 1:length(d)) {
  
  if(length(d[[x]]) > 3) {
    
    time_stamp <- d[[x]]$TimeStampEnd
    
    grp_list <- vector("list", length(d[[x]]$Riders))
    
    for(g in 1:length(d[[x]]$Riders)) {
      
      df <- tibble(bib = d[[x]]$Riders[[g]]$Bib,
                   pos_road = d[[x]]$Riders[[g]]$PositionInTheRace,
                   behind_leader = d[[x]]$Riders[[g]]$GapToFirstRiderT,
                   behind_ahead = d[[x]]$Riders[[g]]$GapToPreviousRiderT,
                   speed = d[[x]]$Riders[[g]]$CurrentSpeed,
                   segment = d[[x]]$Riders[[g]]$CurrentSegmentId,
                   distance_left = d[[x]]$Riders[[g]]$DistanceToFinish,
                   rolling_speed = d[[x]]$Riders[[g]]$CurrentSpeedRollAvg,
                   gradient = d[[x]]$Riders[[g]]$Gradient,
                   yellow = d[[x]]$Riders[[g]]$HasYellowJersey)
      
      grp_list[[g]] <- df
      
    }
    
    res <- bind_rows(grp_list) %>%
      mutate(time = time_stamp,
             block = x)
    
    results_list[[x]] <- res
    
  }
  
}

#
#
#

relevant_distances <- tibble(
  
  start = c(29, 86, 8, 19, 11, 51.5, 33, 12, 43, 23.5),
  end = c(19, 74, 0, 0, 0, 38, 0, 0, 31, 17.5),
  
  climb = c("Lautaret to Galibier", "Izoard final 12km", "Planche Belles Filles",
            "Tourmalet all", "Prat d'Albis", "Iseran", "Vall Thorens full",
            "Val Thorens last 12km", "Ancizan all", "Final climb"),
  
  stage = c(18, 18, 6, 14, 15, 19, 20, 20, 12, 5)
  
)


#
#
#

all_data <- bind_rows(results_list) %>%
  
  mutate(actual_time = lubridate::parse_date_time(str_sub(time, 1, 19), orders = "%d/%m/%y HMS")) %>%
  
  arrange(bib, actual_time) %>%
  
  mutate(dl_025 = ifelse(distance_left < 0.25, actual_time, NA)) %>%
  
  group_by(bib) %>%
  filter(actual_time <= min(dl_025, na.rm = T)) %>%
  ungroup() %>%
  
  #mutate(on_front = ifelse(distance_left > 43, "QSP",
  #                         ifelse(distance_left > 8.2, "Jumbo",
  #                         ifelse(distance_left > 6.6, "FDJ", "Pinot")))) %>%
  mutate(yellow_secs = ifelse(yellow == TRUE, behind_leader, NA)) %>%
  
  group_by(block) %>%
  mutate(yellow_secs = max(yellow_secs, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(in_yellow_grp = ifelse(abs(behind_leader - yellow_secs) < 6, 1, 0)) %>%
  
  group_by(block) %>%
  mutate(size_yellow_grp = sum(in_yellow_grp, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(block, t = floor(bib / 10)) %>%
  mutate(tm_in_yellow = sum(in_yellow_grp, na.rm = T)) %>%
  ungroup() %>%
  
  select(-dl_025, -t) %>%
  
  left_join(
    
    paste0("https://www.procyclingstats.com/race/tour-de-france/", 2019, "/stage-1/results") %>%
      xml2::read_html() %>% 
      rvest::html_nodes('div.resultCont ') %>%
      rvest::html_nodes('table') %>%
      rvest::html_table() %>%
      .[[1]] %>% 
      select(bib = BIB, rider = Rider, team = Team) %>% 
      unique() %>%
      mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team))), by = c("bib")
    
  )

#

dist_vs_time <- expand_grid(dist = seq(0.1,10,0.1), speed = seq(10,50,1)) %>% mutate(tm_needed = (dist / speed)*3600)
 
#
#
# ANALYSIS PARAMETERS

start_d <- 50.25
end_d <- 37.75

#

yellow_grp <- all_data %>% 
  filter(distance_left >= 37.5 & distance_left <= 51.5 & in_yellow_grp == 1) %>% 
  group_by(bib, rider) %>% 
  count(sort = TRUE)

#
#
#

choose_riders <- c(61, 31, 2, 21)
choose_riders <- c(2, 21, 81, 61, 101)
choose_riders <- c(21, 108, 72, 15, 115, 107)

choose_riders <- c(53, 51, 2, 21, 191)

choose_riders <- c(51, 211, 65)

choose_riders <- c(6,8,3,1,2)

choose_riders <- c(2, 21, 65, 108, 1)

#

ggplot(all_data %>% 
         filter(distance_left >= end_d & distance_left <= start_d & bib %in% choose_riders),
       aes(x = distance_left, 
           y = rolling_speed, 
           color = as.factor(rider)))+
  geom_point()+
  geom_smooth(se = F, span = 0.35)+
  scale_x_reverse()+
  labs(x = "KMs left (Galibier summit is 19km, Lautaret summit is 29km)", 
       y = "Speed rolling average in KM/H", 
       title = "Quintana vs breakaway on Galibier (2019 TDF St18)")+
  scale_color_discrete(name = "")
  
#
#
#

r <- c(131, 21, 1, 2, 61, 65, 81, 51, 31, 12, 71, 25, 6, 8)

ggplot(all_data %>% filter(distance_left > 0 & distance_left < 8 & bib %in% r), 
       aes(x = distance_left, y = in_yellow_grp, color = as.factor(bib)))+
  
  geom_point()+
  facet_wrap(~bib)

#

r <- c(21)

ggplot(all_data %>% filter(distance_left > 0 & distance_left < 65 & bib %in% r & block < 402), 
       aes(x = distance_left, y = size_yellow_grp, color = as.factor(on_front)))+
  
  geom_point()

#

summary(all_data %>% filter(distance_left > 0 & distance_left < 19 & bib %in% r) %>% lm(speed ~ gradient + on_front, data = .))

#
#
#
#

# speed plot for riders

all_data %>% filter(distance_left < 9 & block < 457 & speed > 0) %>% filter(bib %in% c(51, 21, 1, 65, 81)) %>% ggplot(aes(x = distance_left * 1000, y = (speed * 1000) / 60, color = as.factor(bib)))+geom_vline(xintercept = c(8200, 6600, 5400, 4100, 2000, 0), linetype = "dashed")+geom_smooth(se = F)+labs(x = "meters to finish", y = "meters per minute", title = "speed of main contenders + Landa")+scale_color_manual(values = c("dark red", "blue", "red", "#00E1F8", "black"), name = 'Rider')

# seconds lost by dropped KM

ggplot(d %>% filter(Abbr != "XX"), 
       aes(x = Dropped, y = Seconds))+
  geom_point(size = 4)+
  labs(x = "dropped by Pinot with N kilometers left", 
       y = "Seconds lost to Pinot", 
       title = "Size of time losses by KM when dropped", 
       subtitle = "on stage 15 Prat d'Albis")+
  geom_abline(intercept = 0, slope = 13.5, size = 1.5, color = "#F8DC00")+
  coord_cartesian(ylim = c(0,300), xlim = c(0,10))+
  ggrepel::geom_label_repel(data = d %>% filter(!Abbr %in% c("X", "XX")), aes(x = Dropped, y = Seconds, label = Abbr))

# block gap comparisons

block_comparison <- all_data %>% 
  
  filter(distance_left < 42 & block < 461) %>%
  
  filter(bib %in% c(65, 51, 1, 21)) %>%
  
  mutate(finish = ifelse(distance_left == 0, block, NA)) %>%
  
  group_by(bib) %>%
  mutate(finish = min(finish, na.rm = T)) %>%
  ungroup() %>%
  
  filter(block <= finish) %>%
  
  select(bib, block, distance_left) %>%
  
  spread(bib, distance_left) %>%
  
  janitor::clean_names()