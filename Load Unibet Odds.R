library(tidyverse)
library(DBI)

dbDisconnect(con)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      port = 5432,
                      host = 'localhost',
                      dbname = "cycling",
                      user = "postgres",
                      password = "braves")

# Most unprocessed files are of label "Margin between Winner and 2nd placed"

betway <- fs::dir_info("C:/Users/Jake Nichols/Documents/R Code/p-c/odds/unibet-odds/") %>%
  
  mutate(file_path = str_replace(path, "C:/Users/Jake Nichols/Documents/R Code/p-c/odds/unibet-odds/", ""),
         accessed_at = ifelse(str_detect(file_path, "cyclo"), str_replace_all(str_sub(file_path, 18,36), "_", ":"), str_replace_all(str_sub(file_path, 12,30), "_", ":")),
         accessed_at = lubridate::as_datetime(accessed_at),
         eventId = str_sub(file_path, 1, 10)) %>%
  
  anti_join(dbGetQuery(con, "SELECT DISTINCT file_path as path FROM unibet_cycling_odds")) %>%
  
  arrange(desc(birth_time)) %>%
  
  select(path, birth_time, accessed_at) %>%
  unique()

#

for(f in 1:length(betway$path)) {
  
  page <- read_rds(betway$path[[f]])
  
  event_data <- tibble(unibetId = page$events$id,
                       name = page$events$name,
                       start_time = page$events$start, 
                       accessed_at = betway$accessed_at[[f]][[1]],
                       race_status = page$events$state)
  
  #
  
  offers <- page$betOffers
  
  bet_outcomes <- page$betOffers$outcomes
  
  #
  
  if(length(offers) == 0) {} else {
    
    for(x in 1:nrow(offers)) {
      
      if("participantId" %in% names(bet_outcomes[[x]])) {
       
        outcomes <- bet_outcomes[[x]]
        
        if("participant" %in% names(outcomes) & !"label" %in% names(outcomes)) {
          outcomes <- outcomes %>% rename(label = participant)
        }
        
        outcomes <- outcomes %>%
          
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
          
          mutate(file_path = str_replace(betway$path[[f]], "C:/Users/Jake/Documents/R Code/p-c/odds/unibet-odds/", "")) %>%
          
          rename(riderid = riderId,
                 marketid = marketId,
                 unibetid = unibetId,
                 bettype = betType,
                 betspectype = betSpecType)
        
        #
        
        dbWriteTable(con, "unibet_cycling_odds", outcomes, append = TRUE, row.names = FALSE)
        
        print(f)
      }
    }
    
  }

}

#

all_odds <- dbReadTable(con, "unibet_cycling_odds") %>%
  
  left_join(
    read_delim("rider-matching-unibet.csv") %>%
      rename(rider = new_label) %>%
      select(rider, riderId), by = c("riderid" = "riderId")
  )

#

matchups <- all_odds %>%
  filter(bettype == "Head to Head") %>%
  mutate(accessed_at = lubridate::as_datetime(accessed_at),
         start_time = lubridate::as_datetime(start_time)) %>% 
  
  filter(accessed_at < start_time) %>%
  
  group_by(marketid, betspectype, bettype, name, rider, riderid) %>% 
  filter(accessed_at == max(accessed_at, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(marketid, betspectype, bettype, name) %>%
  mutate(other_riderId = ifelse(riderid == max(riderid), min(riderid), max(riderid))) %>%
  ungroup() %>%
  
  left_join(
    read_delim("rider-matching-unibet.csv") %>%
      rename(rider = new_label) %>%
      select(other_rider = rider, other_riderId = riderId), by = c("other_riderId")
  ) %>%
  unique()
 
#

library(lme4)

lmer_mod <- matchups %>%
  mutate(implied = (1/odds)/1.08) %>%
  filter(!(str_detect(name, "Classification"))) %>%
  filter(!(str_detect(name, "Time Trial"))) %>%
  filter(!(str_detect(name, "Specials"))) %>%
  filter(!(str_detect(name, "Prologue"))) %>%
  group_by(rider) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  lmer(implied ~ (1 | other_rider) + (1 | rider),
       data = .)
  
ranefs_rider <- ranef(lmer_mod)[[2]] %>% rownames_to_column()
ranefs_oppts <- ranef(lmer_mod)[[1]] %>% rownames_to_column()

ranefs_combined <- ranefs_rider %>% janitor::clean_names() %>%
  rbind(ranefs_oppts %>% janitor::clean_names() %>% mutate(intercept = intercept*-1)) %>%
  
  group_by(rider = rowname) %>%
  summarize(intercept = mean(intercept)) %>%
  ungroup()

#
  

all_odds %>% 
  mutate(accessed_at = lubridate::as_datetime(accessed_at),
         start_time = lubridate::as_datetime(start_time)) %>% 
  
  filter(accessed_at < start_time) %>%
  
  group_by(marketId, betSpecType, betType, name, rider, riderId) %>% 
  filter(accessed_at == max(accessed_at, na.rm = T)) %>%
  ungroup() -> final_prices

#
#
#
#
#

all_odds %>% 
  filter(name == 'General Classification (Tour de France 2022)' & bettype == "Winner" & betspectype == "Winner") %>%

  filter(!is.na(rider)) %>%
  unique() %>%
  
  group_by(accessed_at) %>% 
  mutate(implied = 1/odds,
         implied = implied^0.9 / sum(implied^0.9, na.rm = T)) %>% 
  ungroup() %>% 
  
  filter(rider %in% c("Roglic Primoz","Mas Enric", "O'connor Ben",
                      "Pogacar Tadej", "Vlasov Aleksandr", "Thomas Geraint",
                      "Martinez Daniel Felipe", "Vingegaard Jonas", "Haig Jack",
                      "Yates Adam", "Quintana Nairo", "Uran Rigoberto",
                      "Fuglsang Jakob")) %>%
  
  #filter(accessed_at > (lubridate::now() - 86400)) %>%
  
  group_by(rider, 
           date = as.Date(lubridate::as_datetime(accessed_at))) %>%
  summarize(odds = mean(implied)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = date, y = odds, color = rider))+
  
  geom_point(size=2, alpha = 0.5)+
  geom_line()+
  scale_y_continuous(labels = scales::percent)+labs(x = "", y = "implied probability", title = "RVV Odds")

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

#
#
#
#

all_odds %>% 
  filter(name == "Stage 13 (Giro d'Italia 2022)" & bettype == "Winner" & betspectype == "Winner") %>%
  
  filter(!is.na(rider)) %>%
  
  group_by(accessed_at) %>% 
  mutate(implied = 1/odds,
         implied = implied / sum(implied, na.rm = T)) %>% 
  ungroup() %>% 
  
  group_by(rider, 
           date = lubridate::as_datetime(accessed_at)) %>% 
  summarize(odds = mean(implied)) %>% 
  ungroup() %>% 
  
  group_by(date) %>%
  mutate(odds = odds / sum(odds, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(group = ifelse(rider %in% c("Eenkhoorn Pascal", "Maestri Mirco", "van den Berg Julius", "Prodhomme Nicolas"),
                        "break", "bunch")) %>%
  
  group_by(group, date) %>%
  summarize(odds = sum(odds)) %>%
  ungroup() %>%
  
  filter(date > '2022-05-20 07:30:00') %>%
  
  ggplot(aes(x = date, y = odds, color = group))+
  
  geom_point(size=4, alpha = 0.5)+
  geom_line()+
  scale_y_continuous(labels = scales::percent)+labs(x = "", y = "implied probability", title = "Stage 13 Odds")+
  
  scale_color_manual(values = c("black", "pink"))

#
#
#

all_odds %>% 
  filter(name == "Stage 8 (Giro d'Italia 2022)" & bettype == "Winner" & betspectype == "Winner") %>%
  
  filter(!is.na(rider)) %>%
  
  group_by(accessed_at) %>% 
  mutate(implied = 1/odds,
         implied = implied / sum(implied, na.rm = T)) %>% 
  ungroup() %>% 
  
  group_by(rider, 
           date = lubridate::as_datetime(accessed_at)) %>% 
  summarize(odds = mean(implied)) %>% 
  ungroup() %>% 
  
  group_by(date) %>%
  mutate(odds = odds / sum(odds, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(group = ifelse(rider %in% c("Vanhoucke Harm", "De Gendt Thomas", "Arcas Jorge",
                                     "Gabburo Davide", "Van Der Poel Mathieu", "Poels Wout",
                                     "Ravanelli Simone", "Ghirmay Hailu Biniam", "Schmid Mauro",
                                     "Martin Guillaume", "Felline Fabio", "Vendrame Andrea",
                                     "Moniquet Sylvain", "Ulissi Diego"),
                        "break", "bunch")) %>%
  
  group_by(group, date) %>%
  summarize(odds = sum(odds)) %>%
  ungroup() %>%
  
  filter(date > '2022-05-14 06:30:00') %>%
  
  ggplot(aes(x = date, y = odds, color = group))+
  
  geom_point(size=4, alpha = 0.5)+
  geom_line()+
  scale_y_continuous(labels = scales::percent)+labs(x = "", y = "implied probability", title = "Stage 8 Odds")+
  
  scale_color_manual(values = c("black", "pink"))

#
#
#
#


all_odds %>% 
  filter(name == "General Classification (Giro d'Italia 2022)" & bettype == "Winner" & betspectype == "Winner") %>%
  
  filter(!is.na(rider)) %>% 
  filter(accessed_at < '2022-05-29T11:00:49.000000+0000') %>% 
  filter(accessed_at > '2022-05-04T11:00:49.000000+0000') %>%
  
  group_by(accessed_at) %>% 
  mutate(implied = 1/odds,
         implied = implied / sum(implied, na.rm = T)) %>% 
  ungroup() %>% 
  
  group_by(rider, 
           date = lubridate::as_datetime(accessed_at)) %>% 
  summarize(odds = mean(implied)) %>% 
  ungroup() %>% 
  
  group_by(date) %>%
  mutate(odds = odds / sum(odds, na.rm = T)) %>%
  ungroup() %>%
  
  filter(rider %in% c("Carapaz Richard", "Hindley Jai", "Bardet Romain",
                      "Almeida Joao", "Yates Simon", "Landa Mikel")) %>%
  
  ggplot(aes(x = date, y = odds, color = rider))+
  
  geom_point(size=4, alpha = 0.5)+
  geom_line()+
  scale_y_continuous(labels = scales::percent)+
  scale_x_datetime()+
  labs(x = "", y = "implied probability", title = "Giro GC odds")+
  
  scale_color_manual(values = c("white", "black", "navy", "pink", "orange", "blue"))
