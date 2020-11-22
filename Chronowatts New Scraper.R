
library(tidyverse)
library(rvest)
library(RMySQL)

#

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

CLIMBS <- seq(19,63,1)

CLIMBS <- CLIMBS[-17]

#

for(c in 42:length(CLIMBS)) {
  
  url_format <- paste0('https://www.chronoswatts.com/lib/ajax/tableauPerfCol.php?langue=fr&idPC=', CLIMBS[[c]], '&mode=full&best=nonbest&idEtape=0') %>%
    
    read_html()
  
  df <- url_format %>%
    html_nodes('table') %>%
    html_table() %>%
    .[[1]] %>%
    
    .[, c(1, 2, 4, 6, 7, 8)] %>%
    
    janitor::clean_names() %>%
    
    filter(!str_detect(coureur, 'Autres performances'))
  
  df_href <- url_format %>%
    html_nodes('table') %>%
    html_nodes('a') %>%
    html_attr(name = "href") %>%
    enframe(name = NULL) %>%
    
    filter(str_detect(value, "cyclistes"))
  
  #
  
  watts <- cbind(df, df_href) %>%
    
    rename(rank = number,
           rider = coureur,
           race = epreuve,
           time = temps,
           rider_url = value,
           watts = watts_etalon) %>%
    
    mutate(watts = ifelse(watts == '-', NA, watts)) %>%
    
    fill(watts, .direction = 'down') %>%
    
    mutate(watts = as.numeric(watts)) %>%
    
    mutate(climb_number = CLIMBS[[c]])
  
  #
  
  climb_data <- paste0('https://www.chronoswatts.com/lib/ajax/detailsAscension.php?langue=fr&idPC=', CLIMBS[[c]]) %>%
    
    read_html()
  
  climb_name <- climb_data %>%
    html_nodes('p') %>%
    html_text()
  
  #
  
  climb_df <- climb_data %>%
    
    html_nodes('table') %>%
    html_table(fill = TRUE) %>%
    .[[1]] %>%
    mutate(climb_name = climb_name) %>%
    
    select(-X3, -X4) %>%
    
    spread(X1, X2) %>%
    
    select(climb_name, Distance, Gradient = Pourcentage) %>%
    mutate(climb_number = CLIMBS[[c]],
           Distance = str_trim(str_replace(Distance, "km", "")),
           Gradient = str_trim(str_replace(Gradient, "%", "")))
  
  #
  
  dbWriteTable(con, "chronowatts_performances", watts, append = TRUE, row.names = FALSE)
  
  dbWriteTable(con, "chronowatts_climbs", climb_df, append = TRUE, row.names = FALSE)
  
  print(climb_name)
  
}

#
#
#
#
#

all <- dbGetQuery(con, "SELECT p.*, c.climb_name, c.Distance, c.Gradient FROM chronowatts_performances p JOIN chronowatts_climbs c ON p.climb_number = c.climb_number") %>%
  
  mutate(Distance = ifelse(climb_name == "Arcalis", 10.1, Distance),
         Gradient = ifelse(climb_name == "Arcalis", 7.1, Gradient)) %>%
  
  mutate(Time = (as.numeric(str_sub(time,1,2))*60) + (as.numeric(str_sub(time,4,5))))
