
library(tidyverse)
library(rjson)

#

d <- readr::read_rds('tdf-stage20-telemetry.rds')

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

all_data <- bind_rows(results_list) %>%
  
  mutate(actual_time = lubridate::parse_date_time(str_sub(time, 1, 19), orders = "%d/%m/%y HMS")) %>%
  
  mutate(on_front = ifelse(distance_left > 43, "QSP",
                           ifelse(distance_left > 8.2, "Jumbo",
                           ifelse(distance_left > 6.6, "FDJ", "Pinot")))) %>%
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
  ungroup()

#

r <- c(131, 21, 1, 2, 61, 65, 81, 51, 31, 12, 71, 25, 6, 8)

ggplot(all_data %>% filter(distance_left > 0 & distance_left < 11 & bib %in% r & block < 402), 
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