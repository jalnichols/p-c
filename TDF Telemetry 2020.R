library(tidyverse)
library(DBI)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

telemetry_api <- 'https://racecenter.letour.fr/api/telemetryCompetitor-2020'

STAGE <- 10

step = 1

while(step < 800) {

  json_df <- jsonlite::fromJSON(telemetry_api) %>%
    select(-YGPW) %>%
    unnest(cols = c(Riders)) %>%
    select(-LatLon)
  
  DBI::dbWriteTable(con, "telemetry_tdf2020", json_df, row.names = F, append = TRUE)

  if(min(json_df$kmToFinish) < 3) {

    Sys.sleep(3)

    step = step + 0.15

  } else {
    
    Sys.sleep(20)
    
    step = step + 1
    
   }
  
  print(min(json_df$kmToFinish))

}

#

all_stages <- dbReadTable(con, "telemetry_tdf2020") %>% unique()


#

st10 <- all_stages %>% filter(StageId == "0900")

#
#
#
#

sprint <- st10 %>% group_by(TimeStamp) %>% filter(min(kmToFinish)>0) %>% ungroup() %>% filter(kmToFinish < 0.51 & kmToFinish > 0)

sprint %>% group_by(Bib) %>% summarize(m = mean(kph, na.rm = T), Pos = median(Pos, na.rm = T), n = n(), leading = mean(Pos < 6, na.rm = T)) %>% ungroup() %>% arrange(-m) -> spr

final3km <- st10 %>% group_by(TimeStamp) %>% filter(min(kmToFinish)>0.5) %>% ungroup() %>% filter(kmToFinish > 0.5 & kmToFinish < 3.01)

final3km %>% group_by(Bib) %>% summarize(m = mean(kph, na.rm = T), medPos = median(Pos, na.rm = T), n = n(), leading = mean(Pos < 6, na.rm = T)) %>% ungroup() %>% arrange(-m) -> f3k

#
#
#

all_stages %>% filter(StageId %in% c("0900", "0800")) %>% mutate(primoz = ifelse(Bib == "11", kmToFinish, NA)) %>% group_by(TimeStamp) %>% mutate(primoz = mean(primoz, na.rm = T)) %>% ungroup() %>% mutate(to_roglic = ifelse(abs(kmToFinish - primoz) < 0.2, 1, 0)) %>% filter(Gradient > 3) %>% group_by(Bib) %>% summarize(to_roglic = mean(to_roglic, na.rm = T)) %>% ungroup() -> inyellowpack