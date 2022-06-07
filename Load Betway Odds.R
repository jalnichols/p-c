library(tidyverse)
library(DBI)

dbDisconnect(con)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      port = 5432,
                      host = 'localhost',
                      dbname = "cycling",
                      user = "postgres",
                      password = "braves")

#

betway <- fs::dir_info("C:/Users/Jake Nichols/Documents/R Code/p-c/odds/betway/")

#

dbSendQuery(con, "DELETE FROM betway_cycling_odds")

#

for(f in 1:length(betway$path)) {
  
  page <- jsonlite::fromJSON(betway$path[[f]])
  
  outcomes <- page$Outcomes %>%
    
    select(-HandicapDisplay, -Handicap, -SortIndex, -CouponName, -GroupCName, -RowId, -ColumnId,
           -OddsNum, -OddsDen, -OddsDecimalDisplay, -IsDisplay, -Version) %>%
    
    inner_join(page$Markets %>% select(Id, Title, EventId, EachWayFractionDen, EachWayPosition), by = c("MarketId" = "Id", "EventId")) %>%
    
    mutate(EventName = page$Event$EventName,
           Date = page$Event$Date,
           Time = page$Event$Time,
           DownloadedAt = betway$modification_time[[f]])
  
  #
  
  dbWriteTable(con, 
               "betway_cycling_odds", 
               outcomes %>%
                 rename(id = Id,
                        isactive = IsActive,
                        oddsdecimal = OddsDecimal,
                        eventid = EventId,
                        marketid = MarketId,
                        betname = BetName,
                        title = Title,
                        eachwayfractionden = EachWayFractionDen,
                        eachwayposition = EachWayPosition,
                        eventname = EventName,
                        date = Date,
                        time = Time,
                        downloadedat = DownloadedAt) %>%
                 mutate(isactive = as.numeric(isactive)), 
               append = TRUE, row.names = FALSE)
  
}

#

all_odds <- dbReadTable(con, "betway_cycling_odds")
