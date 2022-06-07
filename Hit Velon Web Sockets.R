
library(tidyverse)
library(websocket)
library(rvest)
library(jsonlite)
library(DBI)

dbDisconnect(con)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      port = 5432,
                      host = 'localhost',
                      dbname = "cycling",
                      user = "postgres",
                      password = "braves")

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

#ws$onMessage(function(event) {
  
  # need to replace \ in JSON
  # convert from JSON
  # extract json$groups[[1]][[1]] to get data.frame
  # add json$time, json$stageId, json$eventId
  # store that data
  
#  if(x == 0) {
#    x <<- x+1
#  } else {
    
#    pre <- event$data
    
#    json <- pre %>% fromJSON()
    
#    result_list[[x]] <<- json
    
#    x <<- x+1
    
#  }
  
#  print(x)
    
#})

ws$onMessage(function(event) {
  
  # need to replace \ in JSON
  # convert from JSON
  # extract json$groups[[1]][[1]] to get data.frame
  # add json$time, json$stageId, json$eventId
  # store that data
  
  pre <- event$data
  
  json <- pre %>% fromJSON()
  
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
      select(eventId, stageId, epochTime, utcTime, elapsedTime, riderId, groupId, distanceToGo, distance, 
             latitude, longitude, powerThreshold, bibNumber, speed, riderStatus, riderId, redZone, power, 
             gradient, cadence, wattsPerKg)
    
    dbWriteTable(con, "velon_telemetry", groups_info, append = TRUE, row.names = FALSE)
    
    print("wrote data")
    
  }
  
})

ws$connect()

print(ws$readyState())

later::run_now(timeoutSecs = 10)

# this checks whether connection is open and if so sleeps for 60 seconds before checking again

while(ws$readyState() == 1) {
  later::run_now(timeoutSecs = 60)
}

# once connection is not Open anymore, it tries to reconnect and restarts process

if(ws$readyState() != 1) {
  
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
    
    pre <- event$data
    
    json <- pre %>% fromJSON()
    
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
        select(eventId, stageId, epochTime, utcTime, elapsedTime, riderId, groupId, distanceToGo, distance, 
               latitude, longitude, powerThreshold, bibNumber, speed, riderStatus, riderId, redZone, power, 
               gradient, cadence, wattsPerKg)
      
      dbWriteTable(con, "velon_telemetry", groups_info, append = TRUE, row.names = FALSE)
      
      print("wrote data")
      
    }
    
  })
  
  ws$connect()
  
  print(ws$readyState())
  
  later::run_now(timeoutSecs = 10)
  
  while(ws$readyState() == 1) {
    later::run_now(timeoutSecs = 60)
  }
  
  # and this handles up to two disconnections
  
  if(ws$readyState() != 1) {
    
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
      
      pre <- event$data
      
      json <- pre %>% fromJSON()
      
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
          select(eventId, stageId, epochTime, utcTime, elapsedTime, riderId, groupId, distanceToGo, distance, 
                 latitude, longitude, powerThreshold, bibNumber, speed, riderStatus, riderId, redZone, power, 
                 gradient, cadence, wattsPerKg)
        
        dbWriteTable(con, "velon_telemetry", groups_info, append = TRUE, row.names = FALSE)
        
        print("wrote data")
        
      }
      
    })
    
    ws$connect()
    
    print(ws$readyState())
    
    later::run_now(timeoutSecs = 10)
    
    while(ws$readyState() == 1) {
      later::run_now(timeoutSecs = 60)
    }
    
    print("DISCONNECTED!!")
    
  }
  
}

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

#write_csv(telemetry, "C:/Users/Jake Nichols/Documents/Old D Drive/Velon Telemetry/giro-ditalia-8-2022-velon-telemetry.csv")

#
#
#
#
#

telemetry <- dbGetQuery(con, "SELECT * FROM velon_telemetry") %>%
  filter(stageId == 268408)

start_list <- DBI::dbGetQuery(con, "SELECT bib, rider, team 
                         FROM pcs_all_startlists 
                         WHERE race LIKE '%italia%' AND year = 2022")

#

telemetry %>%
  filter(!is.na(distanceToGo)) %>%
  filter(distanceToGo > 0) %>%
  
  inner_join(start_list, by = c("bibNumber" = "bib")) %>%
  select(distanceToGo, distance, latitude, longitude, 
         bibNumber, timestamp = epochTime, gradient, eventId, stageId,
         rider, team) -> positionTelemetry

#

calc_power <- telemetry %>% 
  filter(distanceToGo > 0) %>%
  
  mutate(section = case_when(distanceToGo > 104000 ~ "for breakaway",
                             distanceToGo > 70000 ~ "bottom superga 1",
                             distanceToGo > 49000 ~ "top maddelena 1",
                             distanceToGo > 33500 ~ "bottom superga 2",
                             distanceToGo > 13500 ~ "top maddelena 2",
                             TRUE ~ "to finish")) %>%
  
  group_by(bibNumber, section) %>% 
  summarize(wattskg = mean(wattsPerKg, na.rm = T), 
            #avg_Power = mean(power, na.rm = T), 
            seqs = n(), 
            furthest = min(distanceToGo), 
            shortest = max(distanceToGo),
            validpower = sum(!is.na(power)),
            #nonzero = sum(power > 0), 
            start = min(elapsedTime), 
            end = max(elapsedTime), 
            #MED = median(wattsPerKg, na.rm = T), 
            #time_above_avg = mean(wattsPerKg > wattskg, na.rm = T),
            #Q95 = quantile(wattsPerKg, probs = 0.95, na.rm = T),
            #Q90 = quantile(wattsPerKg, probs = 0.9, na.rm = T),
            #Q10 = quantile(wattsPerKg, probs = 0.1, na.rm = T)
            ) %>%
  
  mutate(#peaks = Q90 - MED,
         #troughs = MED - Q10,
         #abs_peaks = Q95 - MED, 
         dist = shortest-furthest,
         time = end - start,
         speed = (dist/1000)/(time/3600)) %>%
  
  inner_join(start_list, by = c("bibNumber" = "bib")) %>%
  
  mutate(groupType = ifelse(bibNumber %in% c(81,109, 14, 86, 51, 92, 17, 173), "break",
                            ifelse(bibNumber %in% c(1, 41, 191, 125, 211, 66), "final six",
                                   ifelse(bibNumber %in% c(181, 201, 61, 161, 31, 123,
                                                           168, 143, 204, 147, 91, 101,
                                                           71, 64, 67, 4, 43, 46, 213, 6,
                                                           45), "GC", "others"))))

#

TT_segments <- positionTelemetry %>%
  mutate(grouping = ifelse(distance < 1000, "first straight",
                           ifelse(distance < 2400, "four corners",
                                  ifelse(distance < 3100, "second straight",
                                         ifelse(distance < 4400, "five corners",
                                                ifelse(distance < 5200, "third straight",
                                                       ifelse(distance < 6500, "bridge",
                                                              ifelse(distance < 7600, "fourth straight",
                                                                     ifelse(distance < 8000, "before climb",
                                                                            ifelse(distance < 8900, "climb",
                                                                                   "finishing stretch"))))))))),
         type = ifelse(str_detect(grouping, "straight") | grouping == "finishing stretch", "straight",
                       ifelse(grouping %in% c("bridge", "before climb",
                                              "four corners", "five corners"), "corners", "climb"))) %>%
  group_by(rider, stageId, eventId, grouping, bibNumber, team, type) %>%
  summarize(start_time = min(timestamp, na.rm = T), 
         end_time = max(timestamp, na.rm = T),
         start_distance = min(distance),
         end_distance = max(distance)) %>%
  ungroup() %>% 
  
  mutate(elapsedTime = end_time - start_time,
         elapsedDistance = end_distance - start_distance,
         speed = (elapsedDistance / 1000) / (elapsedTime / 3600)) %>%
  
  group_by(stageId, eventId, grouping) %>%
  mutate(speed_rk = rank(desc(speed), ties.method = "min")) %>%
  ungroup() %>%
  
  group_by(rider, stageId, eventId, type, bibNumber, team) %>%
  summarize(speed = sum(speed * elapsedTime)/ sum(elapsedTime),
            segments = n()) %>%
  ungroup() %>%
  
  group_by(stageId, eventId, type) %>%
  mutate(speed_rk = rank(desc(speed), ties.method = "min")) %>%
  ungroup()

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

