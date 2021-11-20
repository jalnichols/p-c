library(tidyverse)
library(rjson)

#

url <- 'https://fep-api.dimensiondata.com/v2/stages/344/rider-telemetry' # stage 15 = 339

# enter the estimated end of stage time below
# enter file to save data in below

list_jsons <- vector("list", 2000)

t = 1

while(lubridate::now() < '2019-07-27 12:00:00 EDT') {
  
  d <- fromJSON(
    
    readLines(url)
    
  )
  
  list_jsons[[t]] <- d
  
  print(t)
  
  t = t + 1
  
  if(t %% 30 == 0) {
    
    saveRDS(list_jsons, paste0(t, "tdf-stage20-telemetry.rds"))
    
  }
  
  Sys.sleep(runif(1, 10, 30))
  
}
  
#

saveRDS(list_jsons, "tdf-stage20-telemetry.rds")

#test <- readRDS("liege-bastogne-liege-telemetry.rds")

#
#
#
#
#
#

# ANALYSIS

d <- readr::read_rds('C:/Users/Jake/Documents/R Code/p-c/Stage Telemetry/tdf-stage12-telemetry.rds')

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
      mutate(time = time_stamp)
    
    results_list[[x]] <- res
    
  }
  
}

#

all_data <- bind_rows(results_list) %>%
  
  mutate(actual_time = lubridate::dmy_hms(str_sub(time,1,19)))
           
           