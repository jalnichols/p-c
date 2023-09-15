library(tidyverse)
library(DBI)

Sys.sleep(36000)

dbDisconnect(con)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      port = 5432,
                      host = 'localhost',
                      dbname = "cycling",
                      user = "postgres",
                      password = "braves")

# now kick off everything

#base_url = 'https://racecenter.criterium-du-dauphine.fr/api/'

#telemetry_api <- 'https://racecenter.lavuelta.es/api/'

#telemetry_api <- 'https://racecenter.letour.fr/api/'

base_url <- 'https://racecenter.letourfemmes.fr/api/'

#base_url <- 'https://racecenter.letour.fr/api/'

telemetry_api <- paste0(base_url, 'telemetryCompetitor-2023?xdt=245feyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9eyJrc3AiOiJNMkkxTXpreU1qVmtZVEJqIiwiaWF0IjoxNjg4MTczNTA2fQeXg9hSHo8FbOVm8UVOdweFxzh0ffCPC-iyWd9s0QDsw')

STAGE <- 8
step = 1

cols <- dbListFields(con, "telemetry_tdf2020")

while(step < 6000) {
  
  json_data <- jsonlite::fromJSON(telemetry_api)
  
  if(nrow(json_data) == 0) { 
    print('waiting 60 seconds')
    Sys.sleep(60)
  } else if(as.numeric(json_data$StageId) != as.numeric(paste0(STAGE,"00"))) {
    print('waiting 60 seconds')
    Sys.sleep(60) 
  } else if(json_data$RaceStatus == FALSE) {
    print('waiting 60 seconds')
    Sys.sleep(60)
    step = step+25
  } else {
    
    json_data <- json_data %>%
      select(-YGPW) %>%
      unnest(cols = c(Riders))
    
    if(nrow(json_data) <= 1) {} else {
      
      json_df <- json_data %>%
        select(-LatLon, -`_origin`) %>%
        rename(timestamp = TimeStamp, status = Status, bib = Bib, longitude = Longitude, latitude = Latitude,
               kmtofinish = kmToFinish, winddir = WindDir, degc = degC, kphwind = kphWind, kphavg = kphAvg,
               riderwinddir = RiderWindDir, course = Course, jersey = Jersey, pos = Pos, gradient = Gradient,
               sectofirstrider = secToFirstRider, stageid = StageId, racename = RaceName, racestatus = RaceStatus,
               `_updatedat` = `_updatedAt`) %>%
        #select(-racestatus, -course) %>%
        mutate(racestatus = NA, course = NA) %>%
        mutate(riderwinddir = round(riderwinddir,0))
      
      DBI::dbWriteTable(con, "telemetry_tdf2020", json_df %>% select(cols), row.names = F, append = TRUE)
      
      print(nrow(json_df))
      
    }
    
    if(min(json_df$kmToFinish) < 3 & min(json_df$kmToFinish) > 0) {
      
      Sys.sleep(1)
      
      step = step + 1
      
    } else {
      
      Sys.sleep(5)
      
      step = step + 1
      
    }
    
    print(min(json_df$kmtofinish))
    
  }
  
}

# run lines 4 to 52

#

tictoc::tic()

all_stages <- dbGetQuery(con, "SELECT Bib, Longitude, Latitude, degC, kmToFinish, Gradient,
                         Pos, sectoFirstRider, StageId, RaceName, TimeStamp, windDir, kphWind
                         FROM telemetry_tdf2020 WHERE RaceName IN ('TDF 2023') AND stageid IN ('1600')") %>% 
  unique() %>%
  
  mutate(bib = ifelse(bib == 13 & racename == 'TDF 2023', 19, bib))

tictoc::toc()

#

riders <- paste0(base_url, 'allCompetitors-2023') %>%
  readLines() %>%
  jsonlite::fromJSON() %>%
  select(Bib = bib, firstname, lastname, lastnameshort) %>%
  mutate(RaceName = 'TDF 2023') %>%
  filter(!is.na(Bib)) %>%
  mutate(GC = ifelse(Bib %in% c(1,3,11,19,21,25,27,31,37,41,48,62,65,66,71,81,
                                83,91,114,121,131,135,141,151,161,171,195,204), "GC",
                     ifelse(Bib %in% c(211,201,191,181,177,164,148,157,122,
                                       111,106,86,76,78,64,56,45,18,6,5), "Sprinter", "Helper")))
  
  # rbind(paste0(base_url, 'allCompetitors-2020') %>%
  #         readLines() %>%
  #         jsonlite::fromJSON() %>%
  #         select(Bib = bib, firstname, lastname, lastnameshort) %>%
  #         mutate(RaceName = 'TDF 2020') %>%
  #         mutate(GC = ifelse(Bib %in% c(1, 11, 131, 31, 22, 51, 61, 71, 81,
  #                                       91, 94, 101, 104, 141, 161, 171), "GC","Helper"))) %>%
  # 
  # rbind(paste0(base_url, 'allCompetitors-2022') %>%
  #         readLines() %>%
  #         jsonlite::fromJSON() %>%
  #         select(Bib = bib, firstname, lastname, lastnameshort) %>%
  #         mutate(RaceName = 'CDD 2022') %>%
  #         mutate(GC = ifelse(Bib %in% c(1, 11, 131, 31, 22, 51, 61, 71, 81,
  #                                       91, 94, 101, 104, 141, 161, 171), "GC","Helper")))

#

for(s in 15:16) {
  
  for(y in c(2023)) {
    
    climbs <- paste0(base_url, 'checkpoint-', y, '-', s) %>%
      #readLines() %>%
      jsonlite::fromJSON() %>%
      janitor::clean_names()
    
    cls <- colnames(climbs)
    
    list_climbs <- vector("list", length(cls)-8)
    
    for(x in 1:(length(cls)-8)) {
      
      df <- climbs[,x]
      
      if(length(df$checkpointSummits[[1]]) == 0) {} else {
        list_climbs[[x]] <- df
      }
    }
    
    all <- list_climbs %>%
      bind_rows() %>%
      rename(kmThru = length) %>%
      unnest(cols = "checkpointSummits") %>%
      mutate(startThru = kmThru - (length/1000)) %>%
      unnest(cols = "summit") %>%
      select(stage_dist = kmThru, latitude, longitude, climb_length = length, climb_gradient = state, 
             climb_name = name, summit_altitude = altitude, category = code) %>%
      mutate(climb_length = climb_length/1000,
             stage = s,
             year = y)
    
    dbWriteTable(con, "tdf_telemetry_climbs", all, row.names = F, append = T)
    
  }
  
}

#
#
#

stages_data <- paste0(base_url, 'stage-2023') %>%
  jsonlite::fromJSON() %>%
  select(stage, length) %>%
  mutate(year = 2023)
  rbind(
    paste0(base_url, 'stage-2020') %>%
      jsonlite::fromJSON() %>%
      select(stage, length) %>%
      mutate(year=2020)
  ) %>%
  rbind(
    paste0(base_url, 'stage-2022') %>%
      jsonlite::fromJSON() %>%
      select(stage, length) %>%
      mutate(year=2022)
  ) %>%
  rbind(
    paste0(base_url, 'stage-2021') %>%
      jsonlite::fromJSON() %>%
      select(stage, length) %>%
      mutate(year=2021)
  )


#
#
#

all_stages %>%
  
  rename(Bib = bib, RaceName = racename,
         kmToFinish = kmtofinish, TimeStamp = timestamp,
         StageId = stageid) %>%
  
  inner_join(riders, by = c("Bib", "RaceName")) %>%
  
  filter(kmToFinish > 0) %>% 
  
  mutate(primoz = ifelse(Bib == "1", kmToFinish, NA)) %>% 
  
  group_by(TimeStamp) %>%
  mutate(primoz = mean(primoz, na.rm = T)) %>% 
  ungroup() %>%
  
  mutate(to_roglic = ifelse(abs(kmToFinish - primoz) < 0.1, 1, 
                            ifelse(kmToFinish < primoz, 1, 0)),
         near_roglic = ifelse(abs(kmToFinish - primoz) < 0.2, 1, 0)) %>% 
  
  filter(kmToFinish > 0) %>% 
  
  mutate(valid = ifelse(to_roglic == 1, kmToFinish, NA)) %>%
  
  group_by(Bib, firstname, lastnameshort, GC, StageId, RaceName) %>% 
  summarize(to_roglic = mean(to_roglic, na.rm = T),
            near_roglic = mean(near_roglic, na.rm = T),
            furthest = min(valid, na.rm = T),
            stages = n_distinct(StageId),
            stamps = n()) %>%
  ungroup()  -> survival_w_roglic

#

all_race_activities <- fs::dir_info("FR-routes/backfill/") %>%
  mutate(file_type = ifelse(str_detect(path, ".csv"), "csv",
                            ifelse(str_detect(path, ".rds"), "rds", "unk"))) %>%
  mutate(id = str_replace(path, ".csv", ""),
         id = str_replace(id, ".rds", ""),
         id = str_replace(id, "FR-routes/backfill/fr-", ""),
         id = str_replace(id, "climbs-", ""),
         id = str_replace(id, "route-", ""),
         id = str_replace(id, "cobbles-", "")) %>%
  select(id, file_type) %>%
  unique() %>%
  inner_join(dbGetQuery(con, "SELECT DISTINCT * FROM new_fr_stages") %>%
               mutate(id = str_replace(url, "https://www.la-flamme-rouge.eu/maps/loadtrack/", "")) %>%
               select(url, id, length, race, stage) %>%
               unique())

ID = 506704

FILE <- paste0("FR-routes/backfill/fr-route-", ID, ".csv")
data_lists <- read_csv(FILE) %>%
  mutate(maxDist = max(distance),
         kmToFinish = maxDist - distance)

profile <- ggplot()+
  geom_line(data = data_lists %>% filter(kmToFinish < 25),
            aes(x = kmToFinish, y = altitude), color = "black")+
  scale_x_reverse()+
  labs(x = "KM left", y = "altitude (m)")

results <- jsonlite::fromJSON("https://racecenter.letour.fr/api/rankingTypeTrial-2023-16?xdt=245feyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9eyJrc3AiOiJPR1ptT1RCbVpXRTBNRFpqIiwiaWF0IjoxNjg2MTczMzQ2fQRU-R0As-APwwA3Q2bua2N1q98SG0tNvQHojKXGOD_oI")

itt_results <- results[[3]][[3]] %>%
  mutate(time = absolute/1000)

#itt_results <- tibble(bib = c(1,19,11,95,27, 71),
#                      time = c(14330,14335,14330,14431,14325, 14431))

#rider_perf <- 
  
all_stages %>%
  
  rename(Bib = bib, RaceName = racename,
         kmToFinish = kmtofinish, TimeStamp = timestamp,
         StageId = stageid) %>%
  
  inner_join(riders, by = c("Bib", "RaceName")) %>%
  
  inner_join(itt_results %>% select(bib, time), by = c("Bib" = "bib")) %>%
  
  mutate(lastname = case_when(lastname == "RODRIGUEZ CANO" ~ "RODRIGUEZ",
                              lastname == "POGAÄŒAR" ~ "POGACAR",
                              TRUE ~ lastname)) %>%
  
  group_by(Bib) %>%
  mutate(zero = ifelse(kmToFinish == 0, TimeStamp, NA),
         zero = ifelse(TimeStamp == min(zero, na.rm = T), 1, 0)) %>%
  ungroup() %>%
  
  filter(kmToFinish > 0 | zero == 1) %>% 

  filter(kmToFinish < 50) %>%
  
  group_by(Bib) %>%
  mutate(max_timestamp = max(TimeStamp, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(jonas = ifelse(Bib == "1",  time - (max_timestamp - TimeStamp), NA)) %>% 
  
  group_by(x100meterSegment = floor(kmToFinish/0.1)*0.1) %>%
  mutate(jonas = mean(jonas, na.rm = T)) %>% 
  ungroup() %>%
  
  mutate(rel_rider_time = time - (max_timestamp - TimeStamp)) %>%
  
  mutate(gap_to_jonas = jonas - rel_rider_time) %>% 
  
  group_by(kmToFinish = floor(kmToFinish/0.5)*0.5, Bib, firstname, lastname, GC) %>%
  summarize(gap_to_jonas = mean(gap_to_jonas, na.rm = T)) %>%
  ungroup() %>%
  
  filter(!is.nan(gap_to_jonas)) %>%
  
  filter(Bib %in% c(1,11,27,71)) %>%
  
  ggplot(aes(x = kmToFinish, y = gap_to_jonas, color = paste0(firstname, " ", lastname)))+
  geom_rect(aes(xmin = 25, xmax = 12, ymin = -150, ymax = 15), fill = "gray80", color = "transparent")+
  geom_rect(aes(xmin = 12, xmax = 0, ymin = -150, ymax = 15), fill = "gray90", color = "transparent")+
  #geom_smooth(span = 0.5, se = F)+
  geom_line()+
  #geom_path()+
  geom_point(size=3)+
  scale_x_reverse()+
  scale_color_manual(values = c("#C50018", "#37B36C", "gold", "white", "blue", "purple", "#FF00C1"), name = "")+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16)
        )+
  labs(x = "KM left", y = "Seconds behind Pogacar",
       title = "Joux Plane to finish (Stage 14)")+
  coord_cartesian(ylim = c(-120, 5))+
  annotate(geom = "label", x = 19, y = -105, label = "climb")+
  annotate(geom = "label", x = 6, y = -105, label = "downhill")

library(patchwork)

ggp <- rider_perf / profile + 
  plot_layout(heights = unit(c(5.5, 1), c('in', 'null')))

#

all_stages %>%
  
  filter(StageId %in% c("0300", "0400", "0600", "1000") & RaceName == "TDF 2021") %>%

  filter(kmToFinish < 5 & kmToFinish > 0) %>%
  
  group_by(Bib, f = floor(kmToFinish)) %>% 
  
  summarize(median_pos = median(Pos, na.rm = T),
            top10 = mean(Pos<=10, na.rm = T), 
            appear = n(),
            stages = n_distinct(StageId)) %>% 
  ungroup() %>%
  
  inner_join(riders %>% filter(RaceName == "TDF 2021") %>% select(-GC, -lastnameshort, -RaceName), by = ("Bib")) -> positions

#
#
#

tdf_koms <- dbGetQuery(con, "SELECT * FROM climbs_from_telemetry WHERE Race = 'Tour de France' AND year = 2020") %>%
  
  mutate(StageId = ifelse(stage < 10, paste0("0", stage, "00"), paste0(stage, "00"))) %>%
  
  arrange(-perc_thru) %>%
  
  group_by(StageId) %>%
  mutate(climbing_KMs = cumsum(length)) %>%
  ungroup()

# look at 1) climbing KMs not total KMs
# look at 2) only those dropped on final climb

library(lme4)

time_lost_model <- lmer(gain_gc ~ (0 + climbing_KMs | StageId) + climbing_KMs + 0, 
   data = survival_w_roglic %>%
     filter(!is.na(gain_gc)) %>%
     filter(!is.na(furthest)) %>%
     filter(!furthest == "Inf") %>%
     filter((to_roglic - near_roglic)<0.5) %>%
     filter(StageId %in% c("0400", "0600", '0800', "0900", '1300', '1500', '1600', '1700', '1800')) %>%
     
     inner_join(tdf_koms %>%
                  select(StageId, start_km, end_km, climbing_KMs, length), by = ("StageId")) %>% 
     
     mutate(furthest = round(furthest,1)) %>% 
     
     mutate(climbing_KMs = ifelse(furthest > start_km, climbing_KMs, 
                                  ifelse(furthest < end_km, 0, climbing_KMs - (start_km - furthest)))) %>%
     
     group_by(rider, StageId) %>%
     filter(climbing_KMs == max(climbing_KMs, na.rm = T)) %>% 
     mutate(n=n()) %>%
     ungroup() %>% 
     select(-end_km, -start_km, -length) %>% 
     unique() %>%
     
     mutate(last_10_km = ifelse(climbing_KMs <= 10, 1, 0))
     
     )

summary(time_lost_model)

ranef(time_lost_model)[[1]] %>% rownames_to_column()

#
#
#

All_list <- vector("list", 13)

Ss <- c("0900", "0800", "0600", "0400", "1300", '1400', '1500', '1600', '1700', '1800', '1200', '1900')

K = all_stages %>%
  
  group_by(Bib, StageId) %>%
  summarize(MX = max(kmToFinish)) %>%
  ungroup() %>%
  
  group_by(StageId) %>%
  summarize(MX = quantile(MX, probs = 0.9)) %>%
  ungroup() %>%
  
  filter(StageId %in% Ss)

riders_in_race <- tibble(StageId = Ss,
                         RidersTotal = c(168, 172, 172, 172, 160, 158, 157, 156, 152, 150, 161, 147))

for(S in 1:12) { 
  
  k = K %>%
    filter(StageId == Ss[[S]]) %>%
    select(MX) %>%
    .[[1]] %>%
    floor()
  
  list13 <- vector("list", k)
  
  for(x in 1:k) { 
    
    list13[[x]] <- expand_grid(
      
      ClusterType = survival_w_roglic$type %>% unique(),
      kmToFinish = x
      
    ) %>%
      
      mutate(perc = 0,
             StageId = Ss[[S]],
             n = NA) %>%
      
      rbind(
        
        survival_w_roglic %>%
          filter(StageId==Ss[[S]] & (to_roglic - near_roglic)<0.5) %>%
          group_by(ClusterType = type) %>%
          mutate(n=n()) %>% 
          ungroup() %>%
          
          filter(furthest < x) %>% 
          
          group_by(ClusterType) %>%
          summarize(perc = n() / mean(n, na.rm = T),
                    n=mean(n)) %>%
          ungroup() %>%
          mutate(kmToFinish = x,
                 StageId = Ss[[S]])) %>%
      
      group_by(ClusterType, kmToFinish) %>%
      filter(perc == max(perc, na.rm = T)) %>%
      ungroup()

  }
  
  df <- bind_rows(list13)
  
  ggplot()+
    
    geom_rect(data = tdf_koms %>%
                 filter(StageId == Ss[[S]]) %>%
                   filter(start_km < k),
               aes(xmin = start_km, xmax = end_km, ymin = 0, ymax = 1),
               
               fill = "yellow", color = "transparent", alpha = 0.5)+
    
    geom_line(data = df, aes(x = kmToFinish, y = perc, color = ClusterType), size=1)+
    
    scale_x_reverse(breaks = seq(0,ceiling(k/10)*10, ceiling(k/10)))+
    scale_y_continuous(labels = scales::percent)+
    labs(y = "Survival with Roglic", 
         subtitle = "climbs highlighted in yellow",
         title = paste0("Survival by Rider Type: Stage ", str_sub(Ss[[S]],1,2)))+
    scale_color_manual(values = c("#F02108", "gray40", "#F0A608",
                                  "#37B36C", "#16BEF2", "#162CF2"), name = "Rider Type")+
    theme(
      
      panel.grid.minor = element_blank(),
      axis.text = element_text(size=15)
      
    )
  
  ggsave(paste0("survival-prob-tdf-xxx-", Ss[[S]], ".png"), height = 7, width = 10)
  
  All_list[[S]] <- bind_rows(list13)
  
}

#

Most_Selective_Climbs <- tdf_koms %>%
  mutate(kmToFinish = start_km) %>%
  mutate(kmToFinish = ceiling(kmToFinish)) %>%
  select(StageId, kmToFinish, length, gradient, start_km, end_km, grouping) %>%
  
  mutate(end_km = ifelse(end_km == 0, 1, end_km)) %>%
  
  inner_join(bind_rows(All_list) %>%
               rename(perc_START = perc), by = c("kmToFinish", "StageId")) %>%
  
  mutate(kmToFinish = end_km) %>%
  mutate(kmToFinish = ceiling(kmToFinish)) %>%
  
  inner_join(bind_rows(All_list) %>%
               select(-n) %>%
               rename(perc_END = perc), by = c("kmToFinish", "StageId", "ClusterType")) %>%
  
  mutate(perc_change = perc_START / perc_END) %>%
  
  group_by(StageId, ClusterType) %>%
  mutate(n = max(n, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(grouping, StageId, start_km, end_km, length) %>%
  mutate(all_change = (sum(n * perc_START, na.rm = T) / sum(n, na.rm = T)) / (sum(n * perc_END, na.rm = T) / sum(n, na.rm = T))) %>%
  ungroup()
  
summary(Most_Selective_Climbs %>% filter(ClusterType == "Climber") %>% lm(all_change ~ gradient * length + end_km, data = .))
  

#
#
#
#


all_stages %>%
  
  inner_join(riders, by = c("Bib")) %>%
  
  filter(StageId %in% c("0900", "0800", "0600", "0400", "0200", "1300", '1500', '1600', '1700', '1800')) %>%
  
  mutate(primoz = ifelse(Bib == "11", kmToFinish, NA)) %>% 
  
  group_by(TimeStamp) %>%
  mutate(primoz = mean(primoz, na.rm = T)) %>% 
  ungroup() %>%
  
  mutate(to_roglic = ifelse(abs(kmToFinish - primoz) < 0.2, 1, 
                            ifelse(kmToFinish < primoz, 1, 0))) %>% 
  
  filter(Gradient > 3 & kmToFinish < 50 & kmToFinish > 0) %>% 
  
  group_by(Bib, firstname, lastnameshort, GC) %>% 
  summarize(to_roglic = mean(to_roglic, na.rm = T),
            stages = n_distinct(StageId),
            stamps = n()) %>%
  ungroup() -> inyellowpack

#

ggplot(inyellowpack %>% 
         filter(to_roglic > 0.66) %>%
         filter(!Bib %in% c("132", "134")), 
       aes(x = reorder(lastnameshort,to_roglic),
           y = to_roglic, 
           fill = as.factor(GC)))+
  geom_point(shape = 21, size = 3, color = "black")+
  coord_flip()+
  scale_fill_manual(values = c("#FFFF00", "gray"))+
  scale_y_continuous(labels = scales::percent_format(accuracy=1), breaks = seq(0.67, 1, 0.03))+
  
  labs(y = "% of climbing kilometers with Roglic",
       x = "",
       title = "Top GC contenders have spent >95% of critical moments together",
       subtitle = "on climbs, last 50km, stages 2, 4, 6, 8, 9")+
  
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15, face = "bold"))

ggsave("top-gc-with-roglic-thru-12.png", height = 6, width = 12)

#
#
#

climbs <- dbGetQuery(con, "SELECT * FROM tdf_telemetry_climbs") %>%
  mutate(climb_length = ifelse(year >= 2023, climb_length*1000, climb_length)) %>%
  inner_join(stages_data, by = c('stage', 'year')) %>%
  mutate(StageId = ifelse(nchar(stage)==1, paste0("0", stage, "00"), paste0(stage,"00")),
         RaceName = paste0("TDF ", year)) %>%
  mutate(kmToFinish = length - stage_dist) %>%
  
  mutate(startKM = 0.1+(kmToFinish + climb_length/1000),
         endKM = (kmToFinish-0.1))

#

downhills <- read_csv("TDF-telemetry-dh.csv") %>%
  inner_join(stages_data, by = c('stage', 'year')) %>%
  mutate(StageId = ifelse(nchar(stage)==1, paste0("0", stage, "00"), paste0(stage,"00")),
         RaceName = paste0("TDF ", year)) %>%
  mutate(startKM = length - startKM,
         endKM = length - endKM)

#

climbing <- all_stages %>%

  rename(kmToFinish = kmtofinish,
         TimeStamp = timestamp,
         Bib = bib,
         RaceName = racename,
         StageId = stageid,
         Gradient = gradient,
         degC = degc) %>%
  
  mutate(within_01 = ifelse(kmToFinish <= 0, TimeStamp, NA)) %>%
  
  group_by(StageId, Bib, RaceName) %>%
  mutate(within_01 = min(within_01, na.rm = T)) %>%
  filter(TimeStamp <= within_01) %>%
  ungroup() %>%
  
  filter(!(StageId %in% c("0100", "0200", "0300", "0500", "0700") & RaceName == 'TDF 2020')) %>%  
  
  filter(Gradient > 0 & kmToFinish > 0) %>%

  inner_join(climbs %>%
               select(Climb = climb_name, startKM, endKM, StageId, RaceName) %>%
               group_by(StageId, RaceName) %>%
               mutate(ORDER_FINISH = rank(endKM, ties.method = "first")) %>%
               ungroup(), by = c("StageId", "RaceName")) %>%
  
  filter(kmToFinish < startKM & kmToFinish > endKM) %>%
  mutate(endKM = endKM + 1) %>%
  
  group_by(Bib, StageId, Climb, RaceName, endKM, ORDER_FINISH) %>%
  summarize(min = min(TimeStamp, na.rm=T),
            max = max(TimeStamp, na.rm = T),
            mindist = min(kmToFinish,na.rm=T),
            maxdist = max(kmToFinish, na.rm = T),
            Ftemp = mean(32 + (degC * 1.9), na.rm = T),
            n=n()) %>%
  ungroup() %>%
  
  mutate(seconds = max-min,
         meters = (maxdist - mindist)*1000,
         m_s = meters / seconds,
         kph = (meters / 1000) / (seconds / 60 / 60)) %>%
  
  group_by(StageId, Climb, RaceName) %>%
  mutate(calc_Rel = kph / mean(kph, na.rm = T)) %>%
  ungroup()  %>%
  
  inner_join(riders, by = c("Bib", "RaceName")) %>%
  
  mutate(Climb = iconv(Climb, from="UTF-8", to = "ASCII//TRANSLIT"))
  
  # group_by(StageId) %>%
  # filter((meters / max(meters, na.rm = T) > 0.90)) %>%
  # mutate(rk = rank(-kph, ties.method = "first")) %>%
  # mutate(x20th = ifelse(rk == 10, kph, NA)) %>%
  # mutate(x20th = mean(x20th, na.rm = T)) %>%
  # ungroup() %>%
  # 
  # mutate(RelTo20th = kph / x20th) %>%
  # 
  # group_by(StageId) %>%
  # mutate(max_meters = max(meters, na.rm = T)) %>%
  # ungroup() %>%
  # 
  # mutate(implied_seconds = max_meters / m_s)

#

climbing_performance <- climbing %>% 
  filter(Climb %in% c("Cote de Pike", "Cote de Vivero (361 m)", "Jaizkibel (455 m)", "Col de Marie Blanque (1 035 m)")) %>% 
  
  group_by(Climb, StageId, RaceName, endKM) %>%
  mutate(standard_meters = median(meters, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(standard_seconds = standard_meters/m_s) %>%
  
  group_by(Bib, firstname, lastname, GC) %>%
  filter(abs(1-(meters/standard_meters)) < 0.05) %>%
  summarize(time = sum(standard_seconds, na.rm = T),
            meters_climbed = sum(standard_meters, na.rm = T),
            climbs_measured = n()) %>%
  ungroup() %>%
  
  mutate(kph = (meters_climbed/1000)/(time/3600))

#

downhill <- all_stages %>%
  
  mutate(within_01 = ifelse(kmToFinish <= 0, TimeStamp, NA)) %>%
  
  group_by(StageId, Bib, RaceName) %>%
  mutate(within_01 = min(within_01, na.rm = T)) %>%
  filter(TimeStamp <= within_01) %>%
  ungroup() %>%
  
  filter(!(StageId %in% c("0100", "0200", "0300", "0500", "0700") & RaceName == 'TDF 2020')) %>%  
  
  filter(Gradient < 0 & kmToFinish > 0) %>%
  
  inner_join(downhills %>%
               select(Descent = descent_name, startKM, endKM, StageId, RaceName) %>%
               group_by(StageId, RaceName) %>%
               mutate(ORDER_FINISH = rank(endKM, ties.method = "first")) %>%
               ungroup(), by = c("StageId", "RaceName")) %>%
  
  filter(kmToFinish < startKM & kmToFinish > endKM) %>%
  mutate(endKM = endKM + 1) %>%
  
  group_by(Bib, StageId, Descent, RaceName, endKM, ORDER_FINISH) %>%
  summarize(min = min(TimeStamp, na.rm=T),
            max = max(TimeStamp, na.rm = T),
            mindist = min(kmToFinish,na.rm=T),
            maxdist = max(kmToFinish, na.rm = T),
            Ftemp = mean(32 + (degC * 1.9), na.rm = T),
            n=n()) %>%
  ungroup() %>%
  
  group_by(StageId, Descent, RaceName, endKM, ORDER_FINISH) %>%
  mutate(Position = rank((min), ties.method = "min")) %>%
  ungroup() %>%
  
  mutate(seconds = max-min,
         meters = (maxdist - mindist)*1000,
         m_s = meters / seconds,
         kph = (meters / 1000) / (seconds / 60 / 60)) %>%
  
  group_by(StageId, Descent, RaceName) %>%
  mutate(calc_Rel = kph / mean(kph, na.rm = T)) %>%
  ungroup()  %>%
  
  inner_join(riders, by = c("Bib", "RaceName"))

#

preds <- cbind(
  
  pred = predict(dh_model, downhill %>% filter(calc_Rel < 1.5 & calc_Rel > 0.25 & (maxdist - mindist) > 4)),
  downhill %>% filter(calc_Rel < 1.5 & calc_Rel > 0.25 & (maxdist - mindist) > 4)
  
) %>%
  
  mutate(relative = calc_Rel - pred)

preds %>% 
  
  group_by(Bib, RaceName, firstname, lastname) %>% 
  summarize(raw = median(calc_Rel, na.rm = T), 
            adj = median(relative, na.rm = T),
            dhs = n(),
            ) %>% ungroup() -> ranking

#
#
#

descending <- all_stages %>%
  
  mutate(within_01 = ifelse(kmToFinish < 0.1, TimeStamp, NA)) %>%
  
  group_by(StageId, Bib) %>%
  mutate(within_01 = min(within_01, na.rm = T)) %>%
  filter(TimeStamp <= within_01) %>%
  ungroup() %>%
  
  filter(kmToFinish > 0) %>%
  
  filter((StageId == "0800" & kmToFinish < 36.5 & kmToFinish > 20.5) |
           (StageId == "0800" & kmToFinish > 3.5 & kmToFinish < 11) |
           (StageId == "0800" & kmToFinish > 71.5 & kmToFinish < 82) |
           (StageId == "0900" & kmToFinish > 55 & kmToFinish < 76) |
           (StageId == "0900" & kmToFinish > 6.5 & kmToFinish < 18.5) |
           (StageId == "1200" & kmToFinish > 18 & kmToFinish < 26) |
           (StageId == "1300" & kmToFinish > 5.5 & kmToFinish < 11.5) |
           (StageId == "1500" & kmToFinish > 32 & kmToFinish < 43) |
           (StageId == "1600" & kmToFinish > 57 & kmToFinish < 69.5) |
           (StageId == "1600" & kmToFinish > 12.5 & kmToFinish < 21) |
           (StageId == "1700" & kmToFinish > 35 & kmToFinish < 63.5) |
           (StageId == '1800' & kmToFinish > 41.5 & kmToFinish < 57.5) |
           (StageId == '1800' & kmToFinish > 15.5 & kmToFinish < 30) |
           (StageId == '1800' & kmToFinish > 0 & kmToFinish < 10)) %>%
  
  mutate(Segment = ifelse(StageId == '1800',
                          ifelse(kmToFinish > 41.5, "A",
                                 ifelse(kmToFinish > 15.5, "B", "C")),
                          ifelse(StageId == '1600',
                                 ifelse(kmToFinish > 21, "A", "B"),
                                 ifelse(StageId == '0900',
                                        ifelse(kmToFinish > 18.5, "A", "B"),
                                        ifelse(StageId == "0800",
                                               ifelse(kmToFinish > 36.5, "A",
                                                      ifelse(kmToFinish > 20.5, "B", "C")), "A"))))) %>%
  
  group_by(Bib, StageId, Segment) %>%
  summarize(min = min(TimeStamp, na.rm=T),
            max = max(TimeStamp, na.rm = T),
            mindist = min(kmToFinish,na.rm=T),
            maxdist = max(kmToFinish, na.rm = T),
            n=n()) %>%
  ungroup() %>%
  
  mutate(seconds = max-min,
         meters = (maxdist - mindist)*1000,
         m_s = meters / seconds,
         kph = (meters / 1000) / (seconds / 60 / 60)) %>%
  
  group_by(StageId, Segment) %>%
  mutate(calc_Rel = kph / mean(kph, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(StageId, Segment) %>%
  #filter((meters / max(meters, na.rm = T) > 0.90)) %>%
  mutate(rk = rank(-kph, ties.method = "first")) %>%
  mutate(x20th = ifelse(rk == 10, kph, NA)) %>%
  mutate(x20th = mean(x20th, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(RelTo20th = kph / x20th) %>%
  
  group_by(StageId, Segment) %>%
  mutate(max_meters = max(meters, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(implied_seconds = max_meters / m_s)

#

ggplot(
  
  climbing %>%
    inner_join(riders) %>%
    filter(Bib %in% c('11', '14', '61', '71', '81', '101', '161', '141', '121', '131', '94')),
  
  aes(x = implied_seconds / 60, y = kph, color = str_sub(StageId,1,2)))+
  
  geom_point(size=4, alpha = 0.5)+
  geom_smooth(method = "lm", alpha = 0.5)+
  
  labs(x = "climb duration (mins)",
       y = "climbing speed (km/h)",
       color = "Stage",
       title = "Climbing speeds in 2020 TDF")+
  
  theme(axis.text = element_text(size = 16))

#

ggplot(
  
  climbing %>%
    inner_join(riders) %>%
    
    mutate(top5 = ifelse(Bib %in% c('11', '131', '61', '101', '94'), 1, 0)) %>%
    
    group_by(GC, StageId) %>%
    filter(top5 == 1 | rank(-kph, ties.method = "first") <= 5) %>%
    ungroup(),
  
  aes(y = str_sub(StageId,1,2),  x = kph, color = GC))+

  geom_boxplot()+
  
  labs(y = "Stage",
       x = "climbing speed (km/h)",
       color = "Stage",
       title = "Climbing speeds in 2020 TDF")+
  
  theme(axis.text = element_text(size = 16))

#

top_groups <- climbing %>%
  inner_join(riders) %>%
  
  mutate(top5 = ifelse(Bib %in% c('11', '131', '61', '101', '94'), 1, 0)) %>%
  
  mutate(type = ifelse(top5 == 1, "Top 5\nFinishers",
                       ifelse(GC == "GC", "Other GC\nRiders", "Remaining\nPeloton"))) %>%
  
  group_by(type, StageId) %>%
  filter(top5 == 1 | rank(-kph, ties.method = "first") <= 5) %>%
  ungroup() %>%
  
  group_by(type, StageId) %>%
  summarize(avg = mean(kph, na.rm = T)) %>%
  ungroup()


top_groups$type <- factor(top_groups$type, levels = c("Top 5\nFinishers", "Other GC\nRiders", "Remaining\nPeloton"))

#

ggplot(
  
  top_groups,
  
  aes(y = str_sub(StageId,1,2),  x = avg, fill = type))+
  
  geom_point(size = 7, shape = 21, stroke = 2, color = "black", alpha = 0.5)+
  
  labs(y = "Stage",
       x = "climbing speed (km/h)",
       title = "Climbing speeds in 2020 TDF",
       subtitle = "Top 5 on each decisive climb from each group")+
  
  scale_x_continuous(breaks = c(17.5, 20, 22.5, 25, 27.5))+
  
  theme(axis.text = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        legend.position = "bottom")+
  
  scale_fill_manual(values = c("gold", "gray30", "dark red"), name = "")

ggsave("climbing-speeds-tdf-2020.png", height = 7, width = 11)

#

interesting <- climbing %>%
  
  mutate(Bib = as.character(Bib)) %>%
  
  left_join(tibble(StageId = c('0200', '0200', '0200',
                               '0600', '0600', '0600',
                               '0800', '0800', '0800',
                               '0900', '0900',
                               '1300', '1300', '1300',
                               '1500', '1500', '1500',
                               '1600', '1600', '1600',
                               '1700', '1700', '1700',
                               '1800', '1800', '1800'),
                   
                   Bib = c('161', '204', '41',
                           '146', '132', '124',
                           '118', '106', '36',
                           '1', '204',
                           '24', '28', '76',
                           '16', '62', '141',
                           '24', '3', '58',
                           '141', '16', '3',
                           '3', '5', '18'),
                   Keep = 1), by = c("StageId", "Bib")) %>%
  
  filter(Keep == 1 | Bib %in% c('94', '11', '101', '131', '61')) %>%
  
  mutate(stage_winner = ifelse(StageId == '0200' & Bib == '41', 1,
                               ifelse(StageId == '0400' & Bib == '11', 1,
                                      ifelse(StageId == '0600' & Bib == '146', 1,
                                             ifelse(StageId == '0800' & Bib == '36', 1,
                                                    ifelse(StageId == '0900' & Bib == '131', 1,
                                                           ifelse(StageId == '1300' & Bib == '76', 1,
                                                                  ifelse(StageId == '1500' & Bib == '131', 1,
                                                                         ifelse(StageId == '1600' & Bib == '24', 1,
                                                                                ifelse(StageId == '1700' & Bib == '141', 1,
                                                                                       ifelse(StageId == '1800' & Bib == '5', 1, 0)))))))))))

#

ggplot(
  
  interesting %>%
    mutate(top5 = ifelse(is.na(Keep), "GC Top 5", "Other riders")),
  
  aes(y = str_sub(StageId,1,2),  x = kph, label = Bib, fill = top5, shape = as.factor(stage_winner)))+
  
  geom_point(size = 4, alpha = 0.5, color = "black")+
  ggrepel::geom_label_repel(color = "black", fill = "white", size = 4)+
  
  labs(y = "Stage",
       x = "climbing speed (km/h)",
       title = "Climbing speeds in 2020 TDF",
       subtitle = "Top 5 on each decisive climb from each group // stage winner as triangle")+
  
  scale_x_continuous(breaks = c(15, 17.5, 20, 22.5, 25, 27.5, 30))+
  
  scale_shape_manual(values = c(21, 24))+
  
  theme(axis.text = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"))+
  
  scale_fill_manual(values = c("gold", "dark red"), name = "")+
  
  guides(shape = F)

#

ggsave("climbing-speeds-tdf-2020.png", height = 6.2, width = 11)

#

total <- climbing %>% 
  mutate(primoz = ifelse(Bib == '11', seconds, NA)) %>% 
  
  group_by(StageId) %>% 
  mutate(primoz = mean(primoz, na.rm = T)) %>% 
  ungroup() %>% 
  
  group_by(Bib) %>% 
  summarize(climbing_speed = mean(RelTo20th, na.rm = T), 
            n = n(), 
            seconds = sum(primoz, na.rm = T)) %>% 
  ungroup() %>% 
  
  mutate(primoz = ifelse(Bib == '11', climbing_speed, NA), 
         primoz = mean(primoz, na.rm = T)) %>%
  
  mutate(diff = climbing_speed - primoz, diff = seconds * diff)

#

ggplot(total %>% inner_join(riders) %>%
         filter(n==4) %>%
         filter(!Bib %in% c("132", "134")), 
       aes(x = climbing_speed-1,
           fill = as.factor(GC), group = paste0(lastnameshort, firstname)))+
  geom_histogram(binwidth = 0.02, color = "black")+
  scale_fill_manual(values = c("#FFFF00", "gray"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "relative climbing speed vs average",
       y = "",
       title = "best climbers have been ~27% better than average",
       subtitle = "final climbs of stages 4, 6, 8, 9")+
  
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15, face = "bold"), axis.text.y = element_blank(), axis.ticks.y = element_blank())

#

sprinting <- all_stages %>%
  
  filter(kmToFinish > 0) %>%
  
  filter((StageId == "0700" & kmToFinish < 2) |
           (StageId == "1000" & kmToFinish < 2) |
           (StageId == "1100" & kmToFinish < 2)) %>%
  
  group_by(Bib, StageId) %>%
  summarize(speed = mean(kph, na.rm = T),
            min = min(TimeStamp, na.rm=T),
            max = max(TimeStamp, na.rm = T),
            mindist = min(kmToFinish,na.rm=T),
            maxdist = max(kmToFinish, na.rm = T),
            n=n()) %>%
  ungroup() %>%
  
  mutate(seconds = max-min,
         meters = (maxdist - mindist)*1000,
         m_s = meters / seconds,
         kph = (meters / 1000) / (seconds / 60 / 60)) %>%
  
  group_by(StageId) %>%
  mutate(calc_Rel = kph - mean(kph, na.rm = T)) %>%
  ungroup()

#
#
#
#
#

final_sprint_11 <- all_stages %>%
  filter(StageId == "1100" & kmToFinish > 0 & kmToFinish < 3)

ggplot(final_sprint_11 %>%
         filter(Bib %in% c("43", "48", "151", "21", "18")),
       aes(x = Longitude, y = Latitude, color = as.factor(Bib)))+
  
  geom_point()+
  
  facet_wrap(~TimeStamp)

#

final_sprint_11 %>%
  filter(Bib %in% c("43", "48", "151", "21", "18", "211", "203")) %>%
  
  group_by(TimeStamp) %>%
  mutate(Rel = kmToFinish - min(kmToFinish, na.rm = T)) %>%
  ungroup() -> abc

#
#
#
#
#
#
#

puy_mary <- all_stages %>%
  
  inner_join(riders %>%
               mutate(Rider = paste0(firstname, " ", lastnameshort))) %>%

  filter(kmToFinish > 0) %>%
  
  filter((StageId == "1300" & kmToFinish > 0.1 & kmToFinish < 15)) %>%
  
  arrange(Rider, TimeStamp) %>%
  
  group_by(Rider, StageId) %>%
  mutate(dist = (lag(kmToFinish) - kmToFinish),
         midpt = ((kmToFinish)+(lag(kmToFinish)))/2,
         time = TimeStamp - lag(TimeStamp)) %>%
  ungroup() %>%
  
  mutate(kph_calc = dist / (time / 60 / 60)) %>%
  
  filter(GC == "GC" | Bib %in% c("24", "28", "76", "41", "97", "77", "171", "133",
                                 "56", "218", "44", "132")) %>%
  
  group_by(K = floor(midpt / 0.05)) %>%
  filter(n() > 5) %>%
  mutate(Rel = kph_calc - mean(kph_calc, na.rm = T)) %>%
  ungroup() %>%
  
  filter(Bib %in% c("76", "1", "11", "24", "131")) %>%
  
  filter(abs(Rel) < 15)

#

ggplot(puy_mary %>% filter(kmToFinish < 5.4),
       
       aes(x = kmToFinish, y = Rel, color = as.factor(lastnameshort)))+
  geom_hline(yintercept = 0)+
  #geom_line()+
  
  geom_smooth(se=F, method = "loess", span = 0.4, size = 1.5, alpha = 0.5)+
  
  #geom_point()+
  
  scale_color_manual(values = c("black", "blue", "#F179B1", "dark red",
                                "gold", "#37B36C"), name = "")+
  
  scale_x_reverse()+
  
  geom_point(data = puy_mary %>%
               filter(kmToFinish < 5.4) %>%
              select(kmToFinish, Gradient) %>%
              unique(), aes(x = kmToFinish, y = 9, fill = Gradient), alpha = 0.5, shape = 21, size = 4, color = "transparent")+
  
  scale_fill_gradient2(low = "blue", mid = "white", high = "red")


#
#
#
#
#
#
#
#

jumbo_on_climbs <- all_stages %>%
  
  mutate(within_01 = ifelse(kmToFinish < 0.1, TimeStamp, NA)) %>%
  
  group_by(StageId, Bib) %>%
  mutate(within_01 = min(within_01, na.rm = T)) %>%
  filter(TimeStamp <= within_01) %>%
  ungroup() %>%
  
  filter(Gradient > 0 & kmToFinish > 0) %>%
  
  # Orcieres Merlette St4
  # Lusette St6
  # Peyresource St8
  # Marie Blanque St9
  # Puy Mary and Neronne St13
  filter((StageId == "0400" & kmToFinish < 10.5 & kmToFinish > 0) |
           (StageId == "0200" & kmToFinish > 9 & kmToFinish < 14.5) |
           (StageId == "0600" & kmToFinish > 13.5 & kmToFinish < 25) |
           (StageId == "0800" & kmToFinish > 10 & kmToFinish < 19.5) |
           (StageId == "0900" & kmToFinish > 18.5 & kmToFinish < 26) |
           (StageId == "1300" & kmToFinish > 0 & kmToFinish < 5.4) |
           (StageId == '1500' & kmToFinish > 0 & kmToFinish < 17.7) |
           (StageId == '1600' & kmToFinish > 20 & kmToFinish < 33) |
           (StageId == '1700' & kmToFinish > 0 & kmToFinish < 22) |
           (StageId == '1800' & kmToFinish > 31.5 & kmToFinish < 37.5)) %>%
  
  filter((Bib >= 11 & Bib <= 18) | Bib %in% c(61, 101, 141, 131, 62, 63, 148, 1, 5, 81, 121)) %>%
  
  arrange(StageId, Bib, TimeStamp) %>%
  
  group_by(StageId, Bib) %>%
  mutate(dist_diff = 1000 * (lag(kmToFinish) - kmToFinish),
         time_diff = TimeStamp - lag(TimeStamp)) %>%
  ungroup() %>%
  
  filter(dist_diff >= 0 & time_diff > 0) %>%
  
  mutate(ms_diff = dist_diff / time_diff,
         ms_diff = ifelse(ms_diff > 10, 10, ms_diff)) %>%
  
  mutate(top5 = ifelse(Bib %in% c(11,131,141,101,61), ms_diff, NA)) %>%
  
  group_by(TimeStamp) %>%
  mutate(top5 = mean(top5, na.rm = T)) %>%
  ungroup() %>%
  
  filter(!is.na(top5)) %>% 
  
  select(top5, ms_diff, dist_diff, time_diff, Bib, kmToFinish, Gradient, StageId, TimeStamp) %>%
  
  mutate(VsPR = (ms_diff / top5)-1) %>%
  
  filter(VsPR < 1) %>%
  
  mutate(VsPR = ifelse(VsPR < -3, -3, VsPR)) %>%
  
  mutate(speed_last_5 = ((VsPR) + lag(VsPR, 1) + lag(VsPR, 2) + lag(VsPR, 3) + lag(VsPR, 4)) / 5,
         raw_speed_last_5 = ((ms_diff) + lag(ms_diff, 1) + lag(ms_diff, 2) + lag(ms_diff, 3) + lag(ms_diff, 4)) / 5,
         leaders_speed_last_5 = ((top5) + lag(top5, 1) + lag(top5, 2) + lag(top5, 3) + lag(top5, 4)) / 5,
         Gradient_last_5 = ((Gradient) + lag(Gradient, 1) + lag(Gradient, 2) + lag(Gradient, 3) + lag(Gradient)) / 5) %>%
  
  inner_join(riders)

#

top5_speed_vs_gradient <- lm(raw_speed_last_5 ~ Gradient_last_5, 
                             data = jumbo_on_climbs %>% 
                               filter(Bib %in% c('11','101','131','61','94')))
                               
summary(top5_speed_vs_gradient)

jumbo_on_climbs <- cbind(
  
  pred_speed_grade = predict(top5_speed_vs_gradient, jumbo_on_climbs),
  
  jumbo_on_climbs) %>%
  
  mutate(leaders_vs_expected = leaders_speed_last_5 / pred_speed_grade)

#

ggplot(jumbo_on_climbs %>% filter(StageId == '1500' & Bib %in% c('12','14','16','18', '62', '148')), 
       
       aes(x = (((TimeStamp / 3600)-444024) * 60)-25357, 
           y = speed_last_5, 
           color = as.factor(lastnameshort)))+
  
  geom_hline(yintercept = 0)+
  geom_line(size = 1, color = "black")+
  geom_point(size=2)+
  
  facet_wrap(~lastnameshort)+
  
  labs(x = "minutes from start of climb",
       y = "percent faster/slower than GC leaders",
       title = 'Domestiques on Grand Colombier',
       subtitle = "speed relative to GC leaders")+
  
  scale_y_continuous(labels = scales::percent)

#
#
#

ggplot(jumbo_on_climbs %>% filter(StageId == '1500' & Bib %in% c('1', '81')), 
       
       aes(x = (((TimeStamp / 3600)-444024) * 60)-25357, 
           y = speed_last_5, 
           color = as.factor(lastnameshort)))+
  
  geom_hline(yintercept = 0)+
  geom_line(size = 1, color = "black")+
  geom_point()+
  
  facet_wrap(~lastnameshort, ncol = 1)+
  
  labs(x = "minutes from start of climb",
       y = "percent faster/slower than GC leaders",
       title = 'Bernal & Quintana on Grand Colombier',
       subtitle = "speed relative to GC leaders")+
  
  scale_y_continuous(labels = scales::percent)+
  
  scale_color_manual(values = c("navy", "dark red"), guide=F)+
  
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
  )

#

ggplot(jumbo_on_climbs %>% filter(StageId == '1500' & Bib %in% c('11')) %>%
         
         mutate(mins_climb = (((TimeStamp / 3600)-444024) * 60)-25357) %>%
         
         mutate(section = ifelse(mins_climb < 24, 'WvA',
                                 ifelse(mins_climb < 30, 'Bennett',
                                        ifelse(mins_climb < 42, "TomDom", "Finish")))), 
       
       aes(x = mins_climb, 
           y = leaders_speed_last_5 * 3600 / 1000, 
           color = as.factor(section)))+
  
  geom_line(size = 1, color = "black")+
  geom_point(size=3)+
  
  labs(x = "minutes from start of climb",
       y = "KM / hour",
       title = 'GC group on Grand Colombier',
       subtitle = "rolling average of speed last 2 minutes")+
  
  guides(color = FALSE)+
  
  theme(axis.text = element_text(size = 16),
        plot.title = element_text(size = 24, face = "bold"),
        )

ggsave("gc-group-grand-colombier.png", height = 6.2, width = 11)

#

ggplot(jumbo_on_climbs %>%
         
         filter(StageId == '1800' & Bib %in% c('11')) %>%

         group_by(StageId) %>%
         mutate(mins_climb = (TimeStamp - min(TimeStamp, na.rm = T)) / 60) %>%
         ungroup(), 
       
       aes(x = mins_climb, 
           y = leaders_vs_expected))+
  
  geom_hline(yintercept = 1)+
  
  geom_point(size=3, color = "gold")+
  geom_smooth(color = "#404040", size = 2, se = F, span = 0.4)+
  
  scale_y_continuous(labels = scales::percent)+
  
  labs(x = "minutes from start of climb",
       y = "vs expected speed based on gradient",
       title = 'GC group on Stage 16 St. Nizier Climb',
       subtitle = "relative to expected speed (>100% is faster than expected)")+
  
  guides(color = FALSE)+
  
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
  )

ggsave("gc-group-4-orc-merl.png", height = 6.2, width = 11)

#
#
#
#
#

leaders_speed <- jumbo_on_climbs %>%
  
  filter(Bib %in% c(11, 131, 18)) %>%
  
  group_by(StageId, lastnameshort, Bib) %>%
  
  summarize(speed_own = mean(raw_speed_last_5, na.rm = T),
            vs_gradient = median(raw_speed_last_5 / pred_speed_grade, na.rm = T),
            measurements = n()) %>%
  ungroup()

#
#
#
#
#
#
#
#

climbing <- all_stages %>%
  
  mutate(within_01 = ifelse(kmToFinish < 0.1, TimeStamp, NA)) %>%
  
  group_by(StageId, Bib) %>%
  mutate(within_01 = min(within_01, na.rm = T)) %>%
  filter(TimeStamp <= within_01) %>%
  ungroup() %>%
  
  filter(Gradient > 0 & kmToFinish > 0) %>%
  
  mutate(primoz = ifelse(Bib == "11", kmToFinish, NA)) %>% 
  
  group_by(TimeStamp) %>%
  mutate(primoz = mean(primoz, na.rm = T)) %>% 
  ungroup() %>%
  
  mutate(to_roglic = ifelse(abs(kmToFinish - primoz) < 0.1, 1, 
                            ifelse(kmToFinish < primoz, 1, 0)),
         near_roglic = ifelse(abs(kmToFinish - primoz) < 0.2, 1, 0)) %>%
  
  # Orcieres Merlette St4
  # Lusette St6
  # Peyresource St8
  # Marie Blanque St9
  # Puy Mary and Neronne St13
  filter((StageId == "0400" & kmToFinish < 10.5 & kmToFinish > 0) |
           (StageId == "0200" & kmToFinish > 9 & kmToFinish < 14.5) |
           (StageId == "0600" & kmToFinish > 13.5 & kmToFinish < 25) |
           (StageId == "0800" & kmToFinish > 10 & kmToFinish < 19.5) |
           (StageId == "0900" & kmToFinish > 18.5 & kmToFinish < 26) |
           (StageId == "1300" & kmToFinish > 0 & kmToFinish < 16) |
           (StageId == '1500' & kmToFinish > 0 & kmToFinish < 17.7) |
           (StageId == '1600' & kmToFinish > 0 & kmToFinish < 33) |
           (StageId == '1700' & kmToFinish > 0 & kmToFinish < 22) |
           (StageId == '1800' & kmToFinish > 31.5 & kmToFinish < 37.5)) %>%
  
  mutate(near_roglic = ifelse(is.na(near_roglic), 0, near_roglic)) %>%
  
  group_by(StageId, Bib) %>%
  mutate(finished = max(TimeStamp, na.rm = T),
         final_KM = min(kmToFinish, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(Bib, near_roglic, StageId) %>%
  summarize(min = min(TimeStamp, na.rm=T),
            max = max(TimeStamp, na.rm = T),
            mindist = min(kmToFinish,na.rm=T),
            maxdist = max(kmToFinish, na.rm = T),
            finished = mean(finished, na.rm = T),
            final_KM = mean(final_KM, na.rm = T),
            n=n()) %>%
  ungroup() %>%
  
  mutate(seconds = max-min,
         meters = (maxdist - mindist)*1000,
         m_s = meters / seconds,
         kph = (meters / 1000) / (seconds / 60 / 60))

#
#
#
#
#

v_list <- vector("list", 150)

k_list <- seq(32,38,0.25)

for(k in 1:length(k_list)) { 
  
  all_stages %>%
    
    filter(Bib == 11 & StageId == '1800') %>%
    
    mutate(within_01 = ifelse(kmToFinish < 0.1, TimeStamp, NA)) %>%
    
    group_by(StageId, Bib) %>%
    mutate(within_01 = min(within_01, na.rm = T)) %>%
    filter(TimeStamp <= within_01) %>%
    ungroup() %>%
    
    filter(Gradient > 0 & kmToFinish > 0) %>%
    
    mutate(primoz = ifelse(Bib == "11", kmToFinish, NA)) %>% 
    
    group_by(TimeStamp) %>%
    mutate(primoz = mean(primoz, na.rm = T)) %>% 
    ungroup() %>%
    
    mutate(to_roglic = ifelse(abs(kmToFinish - primoz) < 0.1, 1, 
                              ifelse(kmToFinish < primoz, 1, 0)),
           near_roglic = ifelse(abs(kmToFinish - primoz) < 0.2, 1, 0)) %>%

    filter((StageId == "0400" & kmToFinish < 10.5 & kmToFinish > 0) |
             (StageId == "0200" & kmToFinish > 9 & kmToFinish < 14.5) |
             (StageId == "0600" & kmToFinish > 13.5 & kmToFinish < 25) |
             (StageId == "0800" & kmToFinish > 10 & kmToFinish < 19.5) |
             (StageId == "0900" & kmToFinish > 18.5 & kmToFinish < 26) |
             (StageId == "1300" & kmToFinish > 0 & kmToFinish < 6) |
             (StageId == '1500' & kmToFinish > 0 & kmToFinish < 17.7) |
             (StageId == '1600' & kmToFinish > 0 & kmToFinish < 36) |
             (StageId == '1700' & kmToFinish > 0 & kmToFinish < 22) |
             (StageId == '1800' & kmToFinish > 31.5 & kmToFinish < 37.5)) %>%
    
    mutate(near_roglic = ifelse(is.na(near_roglic), 0, near_roglic)) %>% 
    
    filter(StageId=='1800') %>% 
    
    filter(kmToFinish < k_list[[k]]) %>%  
    summarize(min = min(TimeStamp, na.rm=T),
              max = max(TimeStamp, na.rm = T),
              mindist = min(kmToFinish,na.rm=T),
              maxdist = max(kmToFinish, na.rm = T)) %>% 
    mutate(seconds = max-min,
           meters = (maxdist - mindist)*1000,
           m_s = meters / seconds,
           kph = (meters / 1000) / (seconds / 60 / 60)) %>% 
    
    mutate(KM_Mark = k_list[[k]]) -> v_list[[k]]
  
}

rog <- bind_rows(v_list) %>% select(KM_Mark, kph)

cl <- climbing %>% filter(StageId == '1800') %>% filter(near_roglic == 0 | mindist > 1) %>% group_by(Bib) %>% filter(n()==2) %>% ungroup()

#
#
#
#
#
#
#
#
#

Stage0500 <- read_csv("stage0500-2021-tdf.csv")

all_stages %>% 
  
  mutate(join_km = plyr::round_any(kmToFinish, 0.02)) %>%
  
  left_join(Stage0500 %>%
              mutate(kmto = plyr::round_any(kmto, 0.02)), by = c("join_km" = "kmto")) %>%
  
  filter(kmToFinish > 0) %>%
  
  arrange(Bib, TimeStamp) %>% 
  
  group_by(Bib) %>% 
  mutate(dist_delta = lag(kmToFinish) - kmToFinish,
         time_delta = TimeStamp - lag(TimeStamp),
         alt_delta = altitude - lag(altitude),
         spd_delta = dist_delta / (time_delta/3600)) %>%
  ungroup() %>% 
  
  filter(Bib %in% c(1,11,101,51,22,65,136,201,73,111,114,81,188,21,44,12)) %>% 
  
  #filter(dist_delta > 0) %>% 
  
  mutate(GradientSmooth = alt_delta / (dist_delta*1000)) %>% 
  
  filter(spd_delta > 0 & spd_delta < 90) %>%
  inner_join(riders, by = c("Bib")) -> segments

#

riders <- 'https://racecenter.letour.fr/api/allCompetitors-2021' %>%
  readLines() %>%
  jsonlite::fromJSON() %>%
  select(Bib = bib, firstname, lastname, lastnameshort) %>%
  
  mutate(name = paste0(firstname, " ", lastname)) %>%
  
  mutate(GC = ifelse(Bib %in% c(1, 11, 21, 22, 24, 26, 37, 51, 61, 65, 72, 73, 81, 91,
                                111, 161, 172, 181), "GC","Helper"))


#

segments %>% 
  inner_join(riders, by = c("Bib")) %>% 
  
  mutate(km_dummy = round(kmToFinish,0)) %>%
  
  lme4::lmer(spd_delta ~ kmToFinish + 
               GradientSmooth + 
               (1 + GradientSmooth | name), data = .) -> mm_tt

summary(mm_tt)

lme4::ranef(mm_tt)[[1]] %>% 
  rownames_to_column() %>%
  janitor::clean_names() %>%
  arrange(desc(intercept))

pred_segments <- cbind(
  
  pred_spd = predict(mm_tt, segments %>% mutate(name = "X"), allow.new.levels = T),
  
  segments
) %>%
  
  mutate(rel_speed = spd_delta - pred_spd) %>%
  
  mutate(rough_km = plyr::round_any(kmToFinish, 0.25)) %>%
  
  group_by(rough_km) %>%
  mutate(sims = n(),
         spd_vs_field = spd_delta - mean(spd_delta)) %>%
  ungroup()

#

r_list <- vector("list", 110)

segs = seq(0,27.2,0.5)

for(k in 1:54) {

  d <- pred_segments %>%
    filter(kmToFinish >= (segs[[k]]-0.1) & kmToFinish < (segs[[k+1]]+0.1)) %>%
    
    group_by(Bib, name) %>%
    summarize(minKM = min(kmToFinish, na.rm = T),
              maxKM = max(kmToFinish, na.rm = T),
              minTM = min(TimeStamp, na.rm = T),
              maxTM = max(TimeStamp, na.rm = T),
              RelSpeed = sum(spd_vs_field * dist_delta) / sum(dist_delta)) %>%
    ungroup() %>%
    
    mutate(Segment = segs[[k]])
  
  r_list[[k]] <- d
  
}

bind_rows(r_list) -> xyz


#
#
#


