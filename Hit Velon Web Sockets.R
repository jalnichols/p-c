
library(tidyverse)
library(websocket)
library(rvest)
library(jsonlite)
library(DBI)
library(RMySQL)

#

races <- jsonlite::fromJSON("https://race.velon.cc/liveUpcomingRacesForMultiRaces")

races_data <- races$liveUpcomingRacesForMultiRaces$upcoming

startTime <- lubridate::as_datetime(races_data$anticipatedStartTime$utc[[1]])
endTime <- lubridate::as_datetime(races_data$anticipatedEndTime$utc[[1]])
stageId <- races_data$stageID[[1]]
raceId <- races_data$raceId[[1]]

#

wsSendString = paste0('{"type":"group","eventId":', raceId, ',"stageId":', stageId, ',"jsonpack":false}')

time_until_start = lubridate::as_datetime(startTime) - lubridate::now(tzone = 'UTC')

UUU <- attributes(time_until_start)$units

if(UUU == "hours") {
  time_until_start <- as.numeric(time_until_start)*3600
} else if(UUU == "minutes") {
  time_until_start <- as.numeric(time_until_start)*60
} else if(UUU == "seconds") {
  time_until_start <- as.numeric(time_until_start)
} else {
  time_until_start = 0
}

if(time_until_start <= 0) { time_until_start = 0}

Sys.sleep(round(time_until_start,0))

#

result_list <- vector("list", 10000)

x=0

ws <- WebSocket$new("wss://digital.velon.cc/", autoConnect = FALSE,
                    
                    headers = list('Sec-WebSocket-Key' = "0JZJhFaiEoCqdzLNBn+uYw=="))

ws$onOpen(function(event) {
  
  ws$send(wsSendString)
})

ws$onMessage(function(event) {
  
  # need to replace \ in JSON
  # convert from JSON
  # extract json$groups[[1]][[1]] to get data.frame
  # add json$time, json$stageId, json$eventId
  # store that data
  
  if(x == 0) {
    x <<- x+1
  } else {
    
    pre <- event$data
    
    json <- pre %>% fromJSON()
    
    result_list[[x]] <<- json
    
    x <<- x+1
    
  }
  
  print(x)
  
  if(lubridate::now(tzone = "UTC") > lubridate::as_datetime(endTime+1800)) {
    
    ws$close()
    
  }
    
})

ws$connect()

#
#
#
#
#
#

# RUN JUST LINES 64 to 98

result_list <- result_list[lengths(result_list) != 0]

data_list <- vector("list", length(result_list))

for(v in 1:length(result_list)) {
  
  json <- result_list[[v]]
  
  if(length(json) < 3) {
  
  } else {
    
    race_data <- tibble(eventId = json$eventId,
                        stageId = json$stageId,
                        utcTime = json$time$utc,
                        epochTime = json$time$epochTime)
    
    groups_info <- json$groups %>%
      select(groupId, riders) %>%
      unnest(cols = c("riders")) %>%
      
      cbind(race_data) %>%
      mutate(sequence = v)
    
    data_list[[v]] <- groups_info
    
  }

}

#

telemetry <- bind_rows(data_list) %>%
  group_by(sequence) %>%
  filter(mean(is.na(distanceToGo)) < 1) %>%
  ungroup()

#write_csv(telemetry, "D:/Jake/Documents/Velon Telemetry/tirreno-stage7-2022-velon-telemetry.csv")

#
#
#
#
#

dbDisconnect(con)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

start_list <- DBI::dbGetQuery(con, "SELECT bib, rider, team 
                         FROM pcs_all_startlists 
                         WHERE race = 'tirreno-adriatico' AND year = 2022")

calc_power <- telemetry %>% 
  filter(distanceToGo > 0) %>% 
  
  group_by(bibNumber, split = ifelse(distanceToGo > 31500 & distanceToGo < 36500, "carpegna1", 
                                     ifelse(distanceToGo > 12500 & distanceToGo < 17500, "carpegna2", "rest"))) %>% 
  summarize(wattskg = mean(wattsPerKg, na.rm = T), 
            avg_Power = mean(power, na.rm = T), 
            seqs = n(), 
            furthest = min(distanceToGo), 
            shortest = max(distanceToGo),
            validpower = sum(!is.na(power)),
            nonzero = sum(power > 0), 
            start = min(elapsedTime), 
            end = max(elapsedTime), 
            MED = median(wattsPerKg, na.rm = T), 
            time_above_avg = mean(wattsPerKg > wattskg, na.rm = T),
            Q95 = quantile(wattsPerKg, probs = 0.95, na.rm = T),
            Q90 = quantile(wattsPerKg, probs = 0.9, na.rm = T),
            Q10 = quantile(wattsPerKg, probs = 0.1, na.rm = T)) %>%
  
  mutate(peaks = Q90 - MED,
         troughs = MED - Q10,
         abs_peaks = Q95 - MED, 
         dist = shortest-furthest) %>%
  
  inner_join(start_list, by = c("bibNumber" = "bib"))

#

calc_power %>% 
  filter(wattskg > 0.1, Q10 > 0, shortest > 141500, furthest < 127500, split == "echelons") %>%
  
  ggplot(aes(x = reorder(rider, peaks+troughs), 
             xend = reorder(rider,peaks+troughs), 
             y = Q90, 
             yend = Q10, 
             color = wattskg))+
  geom_segment(size=1.5)+
  labs(x = "", 
       y = "Watts / KG",
       title = "difference between 10th & 90th pctile efforts",
       subtitle = "Jebel Jais full climb")+
  geom_point(aes(x = reorder(rider,peaks+troughs), y = MED), color = "black", size=2)+
  coord_flip()+
  scale_color_viridis_c(guide = F)

#

telemetry %>% 
  filter(!is.na(power)) %>%
  filter(!is.na(wattsPerKg)) %>% 
  filter(power >= 0 & wattsPerKg >= 0) %>% 
  
  group_by(bibNumber) %>% 
  mutate(weight = median(power)/median(wattsPerKg, na.rm  = T)) %>% 
  ungroup() %>%
  
  select(bibNumber, powerThreshold, weight) %>% 
  
  group_by(bibNumber, powerThreshold, weight) %>% 
  count() %>% 
  ungroup() %>% 
  
  group_by(bibNumber, weight) %>% 
  filter(n == max(n)) %>% 
  ungroup() %>% 
  
  mutate(thresh_wattsk = powerThreshold/weight) -> thresh

#
#
#
#

library(tidyverse)

#

riders <- read_csv("tds-2021-riders.csv")

telemetry <- read_csv("tour-de-suisse-8-2021-velon-telemetry.csv") %>%
  mutate(kmToFinish = distanceToGo / 1000) %>%
  
  left_join(riders %>% rename(name = rider, teamName = team), by = c("bibNumber")) %>%
  
  rename(Bib = bibNumber,
         StageId = stageId) %>%
  
  select(-assumed, -avgWattsPerKg, -avgCadence, -avgPower, -avgSpeed,
         -eventId, -powerPer, -maxCadence, -maxPower,
         -heartrate, -maxSpeed, -hrPer, -distance) %>%
  
  filter(kmToFinish > 0)

#

telemetry %>%

  mutate(RIDER = ifelse(Bib == "1", kmToFinish, NA)) %>% 
  
  group_by(epochTime) %>%
  mutate(RIDER = mean(RIDER, na.rm = T)) %>% 
  ungroup() %>%
  
  mutate(to_rider = ifelse(abs(kmToFinish - RIDER) < 0.5, 1, 
                            ifelse(kmToFinish < RIDER, 1, 0)),
         near_rider = ifelse(abs(kmToFinish - RIDER) < 1, 1, 0)) %>% 
  
  filter(kmToFinish > 0) %>% 
  
  mutate(valid = ifelse(to_rider == 1, kmToFinish, NA)) -> survival

#

survival %>%
  
  group_by(Bib, name, teamName, StageId) %>% 
  summarize(to_rider = mean(to_rider, na.rm = T),
            near_rider = mean(near_rider, na.rm = T),
            furthest = round(min(valid, na.rm = T),1),
            stages = n_distinct(StageId),
            stamps = n()) %>%
  ungroup() -> survival_w_rider

#

survival %>%
  
  group_by(RIDER, StageId) %>%
  summarize(in_touch = mean(to_rider, na.rm = T),
            near = mean(near_rider, na.rm = T),
            sample = n()) %>%
  ungroup() %>%
  
  ggplot(aes(x = RIDER, y = in_touch))+
  geom_point()+
  scale_x_reverse()+
  geom_smooth(se=F)

#
#
#
#
#

climbing <- telemetry %>%
  
  mutate(within_01 = ifelse(kmToFinish < 0.2, epochTime, NA)) %>%
  
  group_by(StageId, Bib) %>%
  mutate(within_01 = min(within_01, na.rm = T)) %>%
  filter(epochTime <= within_01) %>%
  ungroup() %>%
  
  filter(kmToFinish > 0) %>%

  filter((StageId == "255970" & kmToFinish < 51 & kmToFinish > 35) |
           (StageId == "255971" & kmToFinish < 7 & kmToFinish > 2.5) |
           (StageId == '255973' & kmToFinish < 16 & kmToFinish > 0) |
           (StageId == '255979' & kmToFinish < 53 & kmToFinish > 38) |
           (StageId == '255980' & kmToFinish < 21.5 & kmToFinish > 10.5) |
           (StageId == '255980' & kmToFinish < 21.5 & kmToFinish > 10.5) |
           (StageId == '255984' & kmToFinish < 17.5 & kmToFinish > 0) |
           (StageId == '255988' & kmToFinish < 9 & kmToFinish > 0) |
           (StageId == '255986' & kmToFinish < 13 & kmToFinish > 0) |
           ((StageId == '255976' & kmToFinish < 6 & kmToFinish > 0))) %>%
  
  group_by(Bib, name, StageId) %>%
  summarize(mindist = min(kmToFinish,na.rm=T),
            maxdist = max(kmToFinish, na.rm = T),
            max = max(utcTime, na.rm = T),
            min = min(utcTime, na.rm = T),
            n=n()) %>%
  ungroup() %>%
  
  mutate(seconds = as.numeric(max-min)*60,
         meters = (maxdist - mindist)*1000,
         m_s = meters / seconds,
         kph = (meters / 1000) / (seconds / 60 / 60)) %>%
  
  group_by(StageId) %>%
  mutate(calc_Rel = kph / mean(kph, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(StageId) %>%
  filter((meters / max(meters, na.rm = T) > 0.90)) %>%
  mutate(rk = rank(-kph, ties.method = "first")) %>%
  mutate(x20th = ifelse(rk == 10, kph, NA)) %>%
  mutate(x20th = mean(x20th, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(RelTo20th = kph / x20th) %>%
  
  group_by(StageId) %>%
  mutate(max_meters = max(meters, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(implied_seconds = max_meters / m_s)

#
#
#
#
#


climbing <- telemetry %>%
  
  mutate(within_01 = ifelse(kmToFinish < 0.1, epochTime, NA)) %>%
  
  group_by(StageId, Bib) %>%
  mutate(within_01 = min(within_01, na.rm = T)) %>%
  filter(epochTime <= within_01) %>%
  ungroup() %>%
  
  filter(kmToFinish > 0) %>%
  
  #filter((StageId == '255979' & kmToFinish < 66 & kmToFinish > 60)) %>% # initial gravel section
  
  #filter((StageId == '255979' & kmToFinish <= 60 & kmToFinish >= 53)) %>% # after 1st section to 2nd section
  
  #filter((StageId == '255979' & kmToFinish <= 53 & kmToFinish >= 38)) %>% # main climb to KOM point
  
  #filter((StageId == '255979' & kmToFinish <= 38 & kmToFinish >= 26.5)) %>% # downhill to next gravel section
  
  #filter((StageId == '255979' & kmToFinish <= 26.5 & kmToFinish >= 19)) %>% # 3rd gravel section
  
  #filter((StageId == '255979' & kmToFinish <= 19 & kmToFinish >= 15)) %>% # downhill section to last gravel section
  
  #filter((StageId == '255979' & kmToFinish <= 15 & kmToFinish >= 9)) %>% # last gravel section
  
  filter((StageId == '255979' & kmToFinish <= 9 & kmToFinish >= 4.5)) %>% # last climb

  group_by(Bib, name, StageId) %>%
  summarize(mindist = min(kmToFinish,na.rm=T),
            maxdist = max(kmToFinish, na.rm = T),
            max = max(utcTime, na.rm = T),
            min = min(utcTime, na.rm = T),
            n=n()) %>%
  ungroup() %>%
  
  mutate(seconds = as.numeric(max-min)*60,
         meters = (maxdist - mindist)*1000,
         m_s = meters / seconds,
         kph = (meters / 1000) / (seconds / 60 / 60)) %>%
  
  group_by(StageId) %>%
  mutate(calc_Rel = kph / mean(kph, na.rm = T)) %>%
  ungroup() 

#
#
#

