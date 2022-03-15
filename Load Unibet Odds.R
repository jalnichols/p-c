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

betway <- fs::dir_info("C:/Users/Jake/Documents/R Code/p-c/odds/unibet-odds/")

#

dbSendQuery(con, "DELETE FROM unibet_cycling_odds")

#

for(f in 1:length(betway$path)) {
  
  page <- read_rds(betway$path[[f]])
  
  #
  
  event_data <- tibble(unibetId = page$events$id,
                       name = page$events$name,
                       start_time = page$events$start, 
                       accessed_at = betway$birth_time[[f]][[1]])
  
  #
  
  offers <- page$betOffers
  
  bet_outcomes <- page$betOffers$outcomes
  
  #
  
  if(length(offers) == 0) {} else {
    
    for(x in 1:nrow(offers)) {
      
      if("participantId" %in% names(bet_outcomes[[x]])) {
        
        outcomes <- bet_outcomes[[x]] %>%
          
          select(riderId = participantId, label, odds) %>%
          mutate(odds = odds/1000,
                 unibetId = page$betOffers$eventId[[x]]) %>%
          
          cbind(offers[x,] %>% 
                  select(betOfferType, marketId = id) %>% 
                  unnest(cols = c("betOfferType")) %>%
                  select(betType = name, marketId)) %>%
          
          inner_join(event_data, by = c("unibetId"))
        
        #
        
        dbWriteTable(con, "unibet_cycling_odds", outcomes, append = TRUE, row.names = FALSE)
        
      }
    }
    
  }
}

#

all_odds <- dbReadTable(con, "unibet_cycling_odds") %>%
  
  left_join(
    read_csv("rider-matching-unibet.csv") %>%
      rename(rider = new_label) %>%
      select(rider, riderId), by = c("riderId")
  )

#
#
#
#
#


