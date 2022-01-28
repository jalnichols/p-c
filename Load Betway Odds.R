library(tidyverse)
library(RMySQL)

#

dbDisconnect(con)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

betway <- fs::dir_info("C:/Users/Jake/Documents/R Code/p-c/odds/betway/")

for(f in 1:length(betway$path)) {
  
  page <- jsonlite::fromJSON(betway$path[[f]])
  
  outcomes <- page$Outcomes %>%
    
    select(-HandicapDisplay, -Handicap, -SortIndex, -CouponName, -GroupCName, -RowId, -ColumnId,
           -OddsNum, -OddsDen, -OddsDecimalDisplay, -IsDisplay, -Version) %>%
    
    inner_join(page$Markets %>% select(Id, Title, EventId, EachWayFractionDen, EachWayPosition), by = c("MarketId" = "Id", "EventId")) %>%
    
    mutate(EventName = page$Event$EventName,
           Date = page$Event$Date,
           Time = page$Event$Time,
           DownloadedAt = betway$birth_time[[f]])
  
  #
  
  dbWriteTable(con, "betway_cycling_odds", outcomes, append = TRUE, row.names = FALSE)
  
}
