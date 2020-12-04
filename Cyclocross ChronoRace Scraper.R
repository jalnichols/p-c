
library(tidyverse)
library(rvest)
library(RMySQL)

#

FIELD <- 'https://prod.chronorace.be/api/results/report/search/1188649379050937/21439/0/100'
 
field_file <- jsonlite::fromJSON(readLines(FIELD))

riders <- field_file$Matches

colnames(riders)[[1]] <- "FinPos"
colnames(riders)[[2]] <- "RiderName"
colnames(riders)[[4]] <- "Team"
colnames(riders)[[6]] <- "TotalTime"

riders <- riders %>%
  select(FinPos, RiderName, Team, TotalTime) %>%
  mutate(FinPos = parse_number(FinPos),
         FinPos = ifelse(is.na(FinPos), "DNF", FinPos)) %>%
  mutate(TotalTime = (3600 * as.numeric(str_sub(TotalTime,1,1))) +
           (60 * as.numeric(str_sub(TotalTime,3,4))) +
           (1 * as.numeric(str_sub(TotalTime,6,7))))
   
#

RESULTS <- 'https://prod.chronorace.be/api/results/xco/20201128_cro/overview/ME'

#

race_data_file <- jsonlite::fromJSON(readLines(RESULTS))

Laps <- race_data_file$MaxLaps

StartLoop <- race_data_file$StartLoop

DistanceStart <- race_data_file$Distance1

DistanceLap <- race_data_file$DistanceN

StartDateTime <- race_data_file$RaceStartDateTime

CurrentSituation <- race_data_file$OverviewData$CurrentSituation %>%
  select(TotalTime, LapTime, LapLocation) %>% 
  unique() %>% 
  mutate(TotalTime = ifelse(LapLocation == 0, TotalTime, TotalTime - LapTime), 
         TotalTime = floor(TotalTime / 1000)) %>%
  
  cbind(Dossard = race_data_file$OverviewData$Dossard) %>%
  
  select(Dossard, TotalTime)

Riders <- riders %>%
  
  inner_join(CurrentSituation, by = c("TotalTime"))

#

riders_data <- race_data_file$OverviewData %>%
  unnest(LapTimes, cols = "laptimes") %>%
  
  group_by(Pos) %>%
  mutate(LapNumber = rank(Pos, ties.method = "first")) %>%
  ungroup() %>%
  
  mutate(LapTimes = LapTimes / 1000) %>%
  
  select(FinPos = Pos, StrPos, Rider = Dossard, LapTimes, LapNumber) %>%
  
  mutate(StartLoop = StartLoop,
         DistanceStart = DistanceStart,
         DistanceLap = DistanceLap,
         RaceDateTime = StartDateTime,
         Laps = Laps) %>%
  
  mutate(LapNumber = ifelse(LapNumber == 1, 0, LapNumber-1)) %>%
  
  arrange(Rider, LapNumber) %>%
  
  group_by(Rider) %>%
  mutate(TimeThru = cumsum(LapTimes)) %>%
  ungroup() %>%
  
  group_by(LapNumber) %>%
  mutate(LeaderTime = min(TimeThru)) %>%
  ungroup() %>%
  
  mutate(Gap = TimeThru - LeaderTime) %>%
  
  inner_join(CurrentSituation, by = c("Rider" = "Dossard")) %>%
  
  mutate(StrPos = ifelse(is.na(parse_number(StrPos)), "DNF", parse_number(StrPos))) %>%
  
  inner_join(riders, by = c("TotalTime", "StrPos" = "FinPos"))

#
#
#

