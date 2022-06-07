
library(tidyverse)
library(rjson)

#

stages <- c("2", "4", "6")

FILES <- fs::dir_info("C:/Users/Jake/Documents/R Code/p-c/Stage Telemetry/") %>%
  filter(str_sub(path, 1, 3) == "tdf") %>%
  filter(str_detect(path, "step"))

#

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

for(s in 1:length(FILES$path)) {
  
  clean <- readr::melt_tsv(FILES$path[[s]]) %>% filter(str_detect(value, "telemetryCompetitor-2020"))
  
  for(r in 1:length(clean$value)) {
    
    js <- jsonlite::fromJSON(str_replace(clean$value[[r]], "data: ", "")) %>% .$data
    
    df <- js$Riders %>%
      jsonlite::flatten() %>%
      as_tibble() %>%
      
      mutate(TimeStamp = js$TimeStamp,
             StageId = js$StageId,
             RaceName = js$RaceName,
             RaceStatus = js$RaceStatus,
             `_id` = js$`_id`,
             `_bind` = js$`_bind`,
             `_updatedAt` = js$`_updatedAt`,
             `_parent` = js$`_parent`,
             `_key` = js$`_key`) %>%
      
      select(-LatLon)
      
    DBI::dbWriteTable(con, "telemetry_tdf2020", df, row.names = F, append = TRUE)
    
    print(paste0(s,"-", r))
    
  }

}






