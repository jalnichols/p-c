
library(tidyverse)
library(websocket)
library(rvest)
library(jsonlite)

result_list <- vector("list", 10000)

x=0

ws <- WebSocket$new("wss://digital.velon.cc/", autoConnect = FALSE,
                    
                    headers =list('Sec-WebSocket-Key' = "dKFYbfysty+FGC2/SoHFwQ=="))

ws$onOpen(function(event) {
  
  ws$send('{"type":"group","eventId":49276,"stageId":255626,"jsonpack":false}')
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
  
  if(lubridate::now() > '2021-04-04 11:30:00 AM') {
    
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
#

result_list <- result_list[lengths(result_list) != 0]

data_list <- vector("list", length(result_list))

for(v in 1:length(result_list)) {
  
  json <- result_list[[v]]
  
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

#
#
#
#

telemetry <- bind_rows(data_list)

write_csv(telemetry, "rvv-2021-velon-telemetry.csv")
