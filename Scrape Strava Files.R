
library(tidyverse)
library(rvest)

#

page <- 'https://www.strava.com/pros' %>%
  read_html()

pro <- 
  
  cbind(
    
    page %>%
  html_nodes('li') %>%
  html_nodes('a.minimal') %>%
  html_attr(name = "href") %>%
    enframe(name = NULL) %>%
    rename(url = value),
  
  page %>%
    html_nodes('li') %>%
    html_nodes('a.minimal') %>%
    html_text() %>%
    enframe(name = NULL) %>%
    rename(name = value)) %>%
  
  filter(name %in% c("Pascal Ackermann", "Thomas De Gendt", "Chad Haga", "Chris Hamilton",
                     "Arnaud Demare", "Laurens ten Dam", "tobias ludvigsson", "Nans Peters",
                     "Nico Denz", "Valentin Madouas", "Luca Covili", "Paolo Simion", "Ted King",
                     "Robert Gesink", "Niki Terpstra Racing", "Jérémy Roy", "Romain Bardet", 
                     "Alex Dowsett", "Lawson Craddock", "Wilco Kelderman", "David Lopez", 
                     "Michal Kwiatkowski", "Greg Van Avermaet", "Alexander Kristoff", "Damiano Caruso",
                     "Ben O'Connor", "Nicolas Edet", "Danilo Wyss", "Eros Capecchi", "Davide Villella",
                     "Jai Hindley", "Joe Dombrowski", "Nathan Brown", "Reto Hollenstein", "Pavel Sivakov",
                     "Paul Martens", "Egan Bernal", "Giulio Ciccone", "Jhonatan Narvaez", "Jack Haig", 
                     "Lilian Calmejane", "Oliver Naesen", "Rudy Molard", "Tejay van Garderen", "Winner Anacona"))



# now scrape activity lists

pros_list <- vector("length", pro$value)

for(x in 1:length(pro$value)) {
  
  pid <- 'tdegendt'
  
  p_page <- paste0('https://www.strava.com/pros/', pid) %>%
    
    read_html()
  
  d <- p_page %>%
    
    html_nodes('body') %>%
    html_nodes('li') %>%
    html_nodes('a') %>%
    html_attr(name = 'href') %>%
    as_tibble() %>%
    filter(str_detect(value, "activities")) %>%
    separate(value, c("id", "drop"), sep = "#") %>%
    select(activity_id = id) %>%
    unique() %>%
    mutate(rider = pid)
  
  if(length(d$activity_id) > 0) {
    
    # I get to the point with this where I can find the top right table listing summary stats
    # Watts, miles, etc but not the segment data from the bottom
    
    pgsession <- html_session(paste0("https://www.strava.com/login?activity_id=", 
                                     str_replace(d$activity_id, "/activities/", "")))
    
    pgform <- html_form(pgsession)[[1]]
    
    filled_form <- set_values(
      
      pgform, 
      email = "nichols.jacob@gmail.com", 
      password = "braves")
    
    submit_form(pgsession, filled_form)
    
    #
    
    url_page <- paste0('https://www.strava.com/', d$activity_id[[1]])
    
    act_page <- jump_to(pgsession, url_page) %>%
      read_html()
    
    act_name <- act_page %>%
      html_nodes('div')
    
  }
  
}
  