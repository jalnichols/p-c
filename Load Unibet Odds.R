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

betway <- fs::dir_info("C:/Users/Jake/Documents/R Code/p-c/odds/unibet-odds/") %>%
  
  mutate(file_path = str_replace(path, "C:/Users/Jake/Documents/R Code/p-c/odds/unibet-odds/", ""),
         accessed_at = str_replace_all(str_sub(file_path, 12,30), "_", ":"),
         accessed_at = lubridate::as_datetime(accessed_at),
         eventId = str_sub(file_path, 1, 10))

#

#dbSendQuery(con, "DELETE FROM unibet_cycling_odds")

#

for(f in 1:length(betway$path)) {
  
  page <- read_rds(betway$path[[f]])
  
  #
  
  event_data <- tibble(unibetId = page$events$id,
                       name = page$events$name,
                       start_time = page$events$start, 
                       accessed_at = betway$accessed_at[[f]][[1]])
  
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
                  select(criterion) %>% 
                  unnest(cols = c("criterion")) %>%
                  select(betSpecType = label)) %>%
          
          cbind(offers[x,] %>% 
                  select(betOfferType, marketId = id) %>% 
                  unnest(cols = c("betOfferType")) %>%
                  select(betType = name, marketId)) %>%
          
          inner_join(event_data, by = c("unibetId")) %>%
          
          mutate(file_path = str_replace(betway$path[[f]], "C:/Users/Jake/Documents/R Code/p-c/odds/unibet-odds/", ""))
        
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

all_odds %>% 
  filter(name == 'Milano - Sanremo 2022' & betType == "Winner" & betSpecType == "Winner") %>%
  
  filter(!is.na(rider)) %>%
  
  group_by(accessed_at) %>% 
  mutate(implied = 1/odds,
         implied = implied / sum(implied, na.rm = T)) %>% 
  ungroup() %>% 
  
  filter(rider %in% c("Van Aert Wout", "Asgreen Kasper", "Van Der Poel Mathieu", "Ganna Filippo",
                      "Pedersen Mads", "Pidcock Thomas", "Alaphilippe Julian",
                      "Ewan Caleb", "Pogacar Tadej", "Mohoric Matej")) %>%
  
  group_by(rider, 
           date = as.Date(lubridate::as_datetime(accessed_at))) %>% 
  summarize(odds = mean(implied)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = date, y = odds, color = rider))+
  
  geom_point(size=4, alpha = 0.5)+
  geom_line()+
  scale_y_continuous(labels = scales::percent)+labs(x = "", y = "implied probability", title = "Paris Roubaix Odds")

#
#
#

all_odds %>% 
  filter(name == 'General Classification (Tour de France 2022)') %>%
  
  group_by(accessed_at) %>% 
  mutate(implied = 1/odds,
         implied = implied / sum(implied, na.rm = T)) %>% 
  ungroup() %>% 
  
  filter(rider %in% c("Pogacar Tadej", "Roglic Primoz", "Thomas Geraint", "Quintana Nairo", 'Pinot Thibaut', "Mas Enric", "Martinez Daniel Felipe", "Carapaz Richard", "Vingegaard Jonas", "Haig Jack")) %>%
  
  group_by(rider, date = as.Date(lubridate::as_datetime(accessed_at))) %>% 
  summarize(odds = mean(implied)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = date, y = odds, color = rider))+
  
  geom_vline(xintercept = c(as.Date('2022-03-05'), as.Date('2022-03-12'), as.Date('2022-03-19')))+
  
  geom_point(size=4, alpha = 0.5)+
  geom_line()+
  scale_y_continuous(labels = scales::percent)+labs(x = "", y = "implied probability", title = "Paris Roubaix Odds")
