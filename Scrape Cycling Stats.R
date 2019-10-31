

library(tidyverse)
library(lubridate)
library(rvest)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

# Find Races --------------------------------------------------------------

events_list <- vector("list", 7)

#

for(y in 1:7) {

  year = y + 2012
  
  url <- paste0('https://www.procyclingstats.com/races.php?year=', year ,'&circuit=1&ApplyFilter=Filter')
  
  #
  
  
  if(year == 2019) {
    
    evts <- url %>%
      
      read_html() %>%
      
      html_nodes('table') %>%
      
      html_table(dec = ",") %>%
      
      .[[1]] %>%
      
      filter(Winner != "")
    
    #
    
  events <- cbind(
  
    evts,
  
    x <- url %>%
      
      read_html() %>%
      
      html_nodes('a') %>%
      
      html_attr(name = "href") %>%
      
      enframe(name = NULL) %>%
      
      .[-(1:99),] %>%
      
      filter(str_detect(value, "race/")) %>%
      
      filter(str_detect(value, as.character(year))) %>%
      
      filter(!str_detect(value, "stage-")) %>%
      filter(!str_detect(value, "result")) %>%
      filter(!(str_detect(value, "2019/"))) %>%
      
      unique() %>%
      
      .[1:length(evts$Race),]
    
      ) %>%
    
    mutate(year = year) %>%
    
    rename(url = value) %>%
    
    mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2)))))
  
  } else {
    
    events <- cbind(
      
      url %>%
        
        read_html() %>%
        
        html_nodes('table') %>%
        
        html_table(dec = ",") %>%
        
        .[[1]],
      
      x <- url %>%
        
        read_html() %>%
        
        html_nodes('a') %>%
        
        html_attr(name = "href") %>%
        
        tibble::enframe(name = NULL) %>%
        
        filter(str_detect(value, "race/")) %>%
        
        filter(str_detect(value, as.character(year)))
      
    ) %>%
      
      mutate(year = year) %>%
      
      rename(url = value) %>%
      
      mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2)))))
    
  }
  
  events_list[[y]] <- events
    
}

#

all_events <- bind_rows(events_list) %>%
  
  # add WC Road Race
  rbind(
    
    tibble::tibble(Date = c('2018-09-30', '2017-09-24', '2016-10-16', '2015-09-27', '2014-09-28', '2013-09-29', '2019-09-30'),
                   Race = "World Championships",
                   Winner = c("VALVERDE Alejandro", "SAGAN Peter", "SAGAN Peter", "SAGAN Peter",
                              "KWIATKOWSKI Michal", "COSTA Rui", "PEDERSEN Mads"),
                   Class = "WC",
                   url = c('race/world-championship/2018', 'race/world-championship/2017', 'race/world-championship/2016',
                           'race/world-championship/2015', 'race/world-championship/2014', 'race/world-championship/2013',
                           'race/world-championship/2019'),
                   year = c(2018, 2017, 2016, 2015, 2014, 2013, 2019))) %>%
  
  # add WC ITT
  rbind(
    
    tibble::tibble(Date = c("26.09", "20.09", "12.10", "23.09", "24.09", "25.09", "27.09"),
                   Race = "World Championships ITT",
                   Winner = c("DENNIS Rohan", "DUMOULIN Tom", "MARTIN Tony", "KIRYIENKA Vasil",
                              "WIGGINS Bradley", "MARTIN Tony", "DENNIS Rohan"),
                   Class = "WC",
                   url = c('race/world-championship-itt/2018', 'race/world-championship-itt/2017', 'race/world-championship-itt/2016',
                           'race/world-championship-itt/2015', 'race/world-championship-itt/2014', 'race/world-championship-itt/2013',
                           'race/world-championship-itt/2019'),
                   year = c(2018, 2017, 2016, 2015, 2014, 2013, 2019)) %>%
      mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2)))))) %>%
  
  rbind(
    
    tibble::tibble(Date = c("06.02", "31.01", "03.02", "04.02", "05.02"),
                   Race = c("Dubai Tour", "Dubai Tour", "Dubai Tour", "Dubai Tour", "Dubai Tour"),
                   Winner = c("VIVIANI Elia", "KITTEL Marcel", "KITTEL Marcel",
                              "CAVENDISH Mark", "PHINNEY Taylor"),
                   Class = c("2.HC", "2.HC", "2.HC", "2.HC", "2.1"),
                   url = c('race/dubai-tour/2018', 'race/dubai-tour/2017', 'race/dubai-tour/2016', 
                           'race/dubai-tour/2015', 'race/dubai-tour/2014'),
                   year = c(2018, 2017, 2016, 2015, 2014)) %>%
      mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2)))))) %>%
      
      rbind(
        
        tibble::tibble(Date = c("06.02", "31.01", "03.02", "04.02", "05.02", "16.02", "21.02"),
                       Race = c("Tour of Oman", "Tour of Oman", "Tour of Oman",
                                "Tour of Oman", "Tour of Oman", "Tour of Oman", "Tour of Oman"),
                       Winner = c("LUTSENKO Alexey", "HERMANS Ben", "NIBALI Vincenzo", 
                                  "VALLS Rafael", "FROOME Chris", "FROOME Chris", "LUTSENKO Alexey"),
                       Class = c("2.HC", "2.HC", "2.HC", "2.HC", "2.HC", "2.HC", "2.HC"),
                       url = c('race/tour-of-oman/2018', 'race/tour-of-oman/2017', 'race/tour-of-oman/2016', 
                               'race/tour-of-oman/2015', 'race/tour-of-oman/2014', 'race/tour-of-oman/2013',
                               'race/tour-of-oman/2019'),
                       year = c(2018, 2017, 2016, 2015, 2014, 2013, 2019)) %>%
          mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2)))))) %>%
          
          rbind(
            
            tibble::tibble(Date = c("06.08", "31.07", "01.08", "03.08", "06.08","06.08","12.08"),
                           Race = c("Tour of Utah", "Tour of Utah", "Tour of Utah", 
                                    "Tour of Utah", "Tour of Utah", "Tour of Utah",
                                    "Tour of Utah"),
                           Winner = c("KUSS Sepp", "BRITTON Rob", "MORTON Lachlan",
                                      "DOMBROWSKI Joe", "DANIELSON Tom", "DANIELSON Tom",
                                      "HERMANS Ben"),
                           Class = c("2.HC", "2.HC", "2.HC", "2.HC", "2.1", "2.1", "2.1"),
                           url = c('race/tour-of-utah/2018', 'race/tour-of-utah/2017', 'race/tour-of-utah/2016', 
                                   'race/tour-of-utah/2015', 'race/tour-of-utah/2014', 'race/tour-of-utah/2013',
                                   'race/tour-of-utah/2019'),
                           year = c(2018, 2017, 2016, 2015, 2014, 2013, 2019)) %>%
              mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2)))))) %>%
  
  rbind(
    
    tibble::tibble(Date = c("06.02", "12.02", "17.06", "27.01", "21.01", "23.01", "20.10", "21.10", "22.10"),
                   Race = c("Tour of Colombia", "Tour of Colombia", "Mont Ventoux Denivele Challenge",
                            "Vuelta a San Juan Internacional", "Vuelta a San Juan Internacional",
                            "Vuelta Ciclista a la Provincia de San Juan",
                            "Japan Cup", "Japan Cup", "Japan Cup"),
                   Winner = c("BERNAL Egan", "LOPEZ Miguel Angel", "HERRADA Jesus",
                              "Anacona Winner", "Najar Gonzalo", "Mollema Bauke",
                              "MOLLEMA Bauke", "POWER Robert", "CANOLO Marco"),
                   Class = c("2.1", "2.1", "1.1", "2.1", "2.1", "2.1", "1.HC", "1.HC", "1.HC"),
                   url = c('race/colombia-21/2018', 'race/colombia-21/2019', 'race/mont-ventoux-denivele-challenge/2019',
                           'race/vuelta-ciclista-a-la-provincia-de-san-juan/2019', 
                           'race/vuelta-ciclista-a-la-provincia-de-san-juan/2018',
                           'race/vuelta-ciclista-a-la-provincia-de-san-juan/2017',
                           'race/japan-cup/2019', 'race/japan-cup/2018', 'race/japan-cup/2017'),
                   year = c(2018, 2019, 2019, 2019, 2018, 2017, 2019, 2018, 2017)) %>%
      mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2))))))

#
# Add 2.HC Europe races
#

events_list <- vector("list", 7)

#

for(y in 1:7) {
  
  year = y + 2012
  
  url <- paste0('https://www.procyclingstats.com/races.php?year=', year ,'&circuit=13&class=2.HC&filter=Filter')
  
  #
  
  
  if(year == 2019) {
    
    evts <- url %>%
      
      read_html() %>%
      
      html_nodes('table') %>%
      
      html_table(dec = ",") %>%
      
      .[[1]] %>%
      
      filter(Winner != "")
    
    #
    
    events <- cbind(
      
      evts,
      
      x <- url %>%
        
        read_html() %>%
        
        html_nodes('a') %>%
        
        html_attr(name = "href") %>%
        
        enframe(name = NULL) %>%
        
        .[-(1:99),] %>%
        
        filter(str_detect(value, "race/")) %>%
        
        filter(str_detect(value, as.character(year))) %>%
        
        filter(!str_detect(value, "stage-")) %>%
        filter(!str_detect(value, "result")) %>%
        filter(!(str_detect(value, "2019/"))) %>%
        
        unique() %>%
        
        .[1:length(evts$Race),]
      
    ) %>%
      
      mutate(year = year) %>%
      
      rename(url = value) %>%
      
      mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2)))))
    
  } else {
    
    events <- cbind(
      
      url %>%
        
        read_html() %>%
        
        html_nodes('table') %>%
        
        html_table(dec = ",") %>%
        
        .[[1]],
      
      x <- url %>%
        
        read_html() %>%
        
        html_nodes('a') %>%
        
        html_attr(name = "href") %>%
        
        tibble::enframe(name = NULL) %>%
        
        filter(str_detect(value, "race/")) %>%
        
        filter(str_detect(value, as.character(year)))
      
    ) %>%
      
      mutate(year = year) %>%
      
      rename(url = value) %>%
      
      mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2)))))
    
  }
  
  events_list[[y]] <- events
  
}

#

all_events <- bind_rows(events_list) %>%
  
  rbind(all_events)

#
# Add 1.HC Europe races
#

events_list <- vector("list", 7)

#

for(y in 1:7) {
  
  year = y + 2012
  
  url <- paste0('https://www.procyclingstats.com/races.php?year=', year ,'&circuit=13&class=1.HC&filter=Filter')
  
  #
  
  
  if(year == 2019) {
    
    evts <- url %>%
      
      read_html() %>%
      
      html_nodes('table') %>%
      
      html_table(dec = ",") %>%
      
      .[[1]] %>%
      
      filter(Winner != "")
    
    #
    
    events <- cbind(
      
      evts,
      
      x <- url %>%
        
        read_html() %>%
        
        html_nodes('a') %>%
        
        html_attr(name = "href") %>%
        
        enframe(name = NULL) %>%
        
        .[-(1:99),] %>%
        
        filter(str_detect(value, "race/")) %>%
        
        filter(str_detect(value, as.character(year))) %>%
        
        filter(!str_detect(value, "stage-")) %>%
        filter(!str_detect(value, "result")) %>%
        filter(!(str_detect(value, "2019/"))) %>%
        
        unique() %>%
        
        .[1:length(evts$Race),]
      
    ) %>%
      
      mutate(year = year) %>%
      
      rename(url = value) %>%
      
      mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2)))))
    
  } else {
    
    events <- cbind(
      
      url %>%
        
        read_html() %>%
        
        html_nodes('table') %>%
        
        html_table(dec = ",") %>%
        
        .[[1]],
      
      x <- url %>%
        
        read_html() %>%
        
        html_nodes('a') %>%
        
        html_attr(name = "href") %>%
        
        tibble::enframe(name = NULL) %>%
        
        filter(str_detect(value, "race/")) %>%
        
        filter(str_detect(value, as.character(year)))
      
    ) %>%
      
      mutate(year = year) %>%
      
      rename(url = value) %>%
      
      mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2)))))
    
  }
  
  events_list[[y]] <- events
  
}

#

all_events <- bind_rows(events_list) %>%
  
  rbind(all_events)

# Add 2.1 Europe events

events_list <- vector("list", 7)

#

for(y in 1:7) {
  
  year = y + 2012
  
  url <- paste0('https://www.procyclingstats.com/races.php?year=', year ,'&circuit=13&class=2.1&filter=Filter')
  
  #
  
  
  if(year == 2019) {
    
    evts <- url %>%
      
      read_html() %>%
      
      html_nodes('table') %>%
      
      html_table(dec = ",") %>%
      
      .[[1]] %>%
      
      filter(Winner != "")
    
    #
    
    events <- cbind(
      
      evts,
      
      x <- url %>%
        
        read_html() %>%
        
        html_nodes('a') %>%
        
        html_attr(name = "href") %>%
        
        enframe(name = NULL) %>%
        
        .[-(1:99),] %>%
        
        filter(str_detect(value, "race/")) %>%
        
        filter(str_detect(value, as.character(year))) %>%
        
        filter(!str_detect(value, "stage-")) %>%
        filter(!str_detect(value, "result")) %>%
        filter(!(str_detect(value, "2019/"))) %>%
        
        unique() %>%
        
        .[1:length(evts$Race),]
      
    ) %>%
      
      mutate(year = year) %>%
      
      rename(url = value) %>%
      
      mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2)))))
    
  } else {
    
    events <- cbind(
      
      url %>%
        
        read_html() %>%
        
        html_nodes('table') %>%
        
        html_table(dec = ",") %>%
        
        .[[1]],
      
      x <- url %>%
        
        read_html() %>%
        
        html_nodes('a') %>%
        
        html_attr(name = "href") %>%
        
        tibble::enframe(name = NULL) %>%
        
        filter(str_detect(value, "race/")) %>%
        
        filter(str_detect(value, as.character(year)))
      
    ) %>%
      
      mutate(year = year) %>%
      
      rename(url = value) %>%
      
      mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2)))))
    
  }
  
  events_list[[y]] <- events
  
}

#

all_events <- bind_rows(events_list) %>%
  
  rbind(all_events)

# Add 1.1 Europe events

events_list <- vector("list", 7)

#

for(y in 1:7) {
  
  year = y + 2012
  
  url <- paste0('https://www.procyclingstats.com/races.php?year=', year ,'&circuit=13&class=1.1&filter=Filter')
  
  #
  
  
  if(year == 2019) {
    
    evts <- url %>%
      
      read_html() %>%
      
      html_nodes('table') %>%
      
      html_table(dec = ",") %>%
      
      .[[1]] %>%
      
      filter(Winner != "")
    
    #
    
    events <- cbind(
      
      evts,
      
      x <- url %>%
        
        read_html() %>%
        
        html_nodes('a') %>%
        
        html_attr(name = "href") %>%
        
        enframe(name = NULL) %>%
        
        .[-(1:99),] %>%
        
        filter(str_detect(value, "race/")) %>%
        
        filter(str_detect(value, as.character(year))) %>%
        
        filter(!str_detect(value, "stage-")) %>%
        filter(!str_detect(value, "result")) %>%
        filter(!(str_detect(value, "2019/"))) %>%
        
        unique() %>%
        
        .[1:length(evts$Race),]
      
    ) %>%
      
      mutate(year = year) %>%
      
      rename(url = value) %>%
      
      mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2)))))
    
  } else {
    
    events <- cbind(
      
      url %>%
        
        read_html() %>%
        
        html_nodes('table') %>%
        
        html_table(dec = ",") %>%
        
        .[[1]],
      
      x <- url %>%
        
        read_html() %>%
        
        html_nodes('a') %>%
        
        html_attr(name = "href") %>%
        
        tibble::enframe(name = NULL) %>%
        
        filter(str_detect(value, "race/")) %>%
        
        filter(str_detect(value, as.character(year)))
      
    ) %>%
      
      mutate(year = year) %>%
      
      rename(url = value) %>%
      
      mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2)))))
    
  }
  
  events_list[[y]] <- events
  
}

#

all_events <- bind_rows(events_list) %>%
  
  rbind(all_events)

#

miss_events <- all_events

curr_events <- dbReadTable(con, "all_races")

all_events <- miss_events %>% 
  filter(!url %in% curr_events$url)

#

dbWriteTable(con, 
             "all_races", 
             all_events,
             row.names = FALSE, 
             append = TRUE)

#

# Max Stage ---------------------------------------------------------------

# selector for stage for loop below

selector <- 'body > div.wrapper > div.content > div:nth-child(1) > div.ESNav.stages > a:nth-child(3)'

#

stages_list <- vector("list", length(all_events$url))
gc_list <- vector("list", length(all_events$url))
  
#
  
tictoc::tic()

for(e in 1:length(all_events$url)) {  
  
  # go to generic event page and find which stage is listed last
  
  page <- paste0('https://www.procyclingstats.com/', all_events$url[[e]]) %>%
    
    read_html()
  
  # pull in last stage
  
  ms <- page %>%
    html_nodes(selector) %>%
    html_attr(name = "href")
  
  # pull in GC results
  
  gc <- page %>%
    html_nodes('table') %>%
    html_table()
  
  # assign to gc list
  
  gc_list[[e]] <- gc
  
  # if it's empty, it's a one day race
  # otherwise find the max stage to scrape
  
  if(identical(ms, character(0)) == TRUE) {
    
    max_stage <- 1
    
  } else{
    
    max_stage = str_sub(ms, nchar(ms) - 1, nchar(ms))
    
  }
  
  # then write for each race
  
  max_stage <- as.numeric(str_replace_all(max_stage, "-", ""))
  
  MAX <- tibble::tibble(MAX = max_stage,
                        url = all_events$url[[e]])
  
  stages_list[[e]] <- MAX
  
  print(e)
  
  Sys.sleep(runif(1, 1, 5))
  
}

tictoc::toc()

#

#readr::write_rds(gc_list, "GC_List.rds")

# extract stage types

stage_types <- 'https://www.procyclingstats.com/race/uae-tour/2019/gc/overview' %>%
  read_html() %>%
  html_nodes('span') %>%
  html_attr(name = "class") %>%
  enframe(name = NULL) %>% 
  filter(str_detect(value, "icon profile"))

#

all_stages <- all_events %>%
  
  inner_join(
    
    bind_rows(stages_list), by = c("url")
    
  ) %>%
  
  # some stages have stage #a and #b, ignore them
  filter(!is.na(MAX)) %>%
  
  unique() %>%
  
  filter(!(url %in% c('race/60th-tour-de-picardie/2016'))) %>%
  filter(!(str_detect(url, "hammer-")))

# Stage Data for each Race ------------------------------------------------

# at current 2013-19 levels it takes 80 minutes to import the data
# but we've doubled the data-set

races_list <- vector("list", 1000)

#

prologues <- c("race/tirreno-adriatico/2015", "race/tour-de-suisse/2015")
  
tictoc::tic()

#

for(r in 1:length(all_stages$url)) {
  
  race_url <- all_stages$url[[r]]
  
  stage_race_list <- vector("list", all_stages$MAX[[r]])
  
  #
  
  for(s in 1:all_stages$MAX[[r]]) {
    
    # these stages were cancelled or are TTTs so we ignore them
    # check again for TTT later
    
    if((race_url == "race/paris-nice/2016" & s == 3) | 
       (race_url == "race/tirreno-adriatico/2016" & s == 5) |
       (race_url == "race/tour-de-pologne/2016" & s == 6) | 
       (race_url == "race/giro-d-italia/2013" & s == 19) |
       (race_url == "race/dauphine/2015" & s == 3) |
       (race_url == "race/tour-de-france/2018" & s == 3) |
       (race_url == "race/vuelta-a-espana/2016" & s == 1) |
       (race_url == "race/vuelta-a-espana/2017" & s == 1) |
       (race_url == "race/tour-de-france/2015" & s == 9) |
       (race_url == "race/dauphine/2018" & s == 1) |
       (race_url == "race/tour-de-suisse/2018" & s == 1) |
       (race_url == "race/binckbank-tour/2016" & s == 5) |
       (race_url == "race/tirreno-adriatico/2018" & s == 1) |
       (race_url == "race/tirreno-adriatico/2016" & s == 1) |
       (race_url == "race/tirreno-adriatico/2017" & s == 1) |
       (race_url == "race/volta-a-catalunya/2017" & s == 2) |
       (race_url == "race/giro-d-italia/2018" & s == 21) |
       (race_url == "race/vuelta-a-espana/2015" & s == 1)) {
      
      stage <- tibble::tibble(rnk = as.character(NA),
                              rider = "Cancelled / TTT",
                              team = as.character(NA),
                              win_seconds = 0,
                              time_seconds = 0,
                              length = 0,
                              speed = 0) %>%
        mutate(stage = s,
               distance = NA,
               stage_name = NA,
               race = all_stages$Race[[r]],
               year = all_stages$year[[r]],
               date = all_stages$Date[[r]])
      
    } else {
      
      # check if the stage is a prologue, stage in stage race, or one day race
      
      if(all_stages$MAX[[r]] == 1) {
        
        url <- paste0('https://www.procyclingstats.com/', race_url)
        
      } else if(s == 1 & race_url %in% prologues) {
        
        url <- paste0('https://www.procyclingstats.com/', race_url, "/prologue")
        
      } else {
        
        url <- paste0('https://www.procyclingstats.com/', race_url, "/stage-", as.character(s))
        
      }
      
      # scrape the HTML for the page for multiple use

      page <- url %>%
        read_html()
   
      # bring in the list of tables
   
      d <- page %>%
        html_nodes('table') %>%
        html_table()
      
      if(length(d) == 0) {
        
        
      } else {
   
      # find which table to choose by searching for the one generically displayed (will be stage standings)
      
      choose <- page %>%
        html_nodes('div') %>%
        html_attr(name = "class") %>%
        enframe(name = NULL) %>%
        filter(value %in% c("resultCont hide", "resultCont ")) %>%
        tibble::rowid_to_column() %>%
        filter(value == "resultCont ") %>%
        .[[1]]
   
      # bring in stage characteristics
      
      characteristics <- page %>%
        html_nodes('h2') %>%
        html_nodes('span') %>%
        html_text()
   
      if(length(characteristics) == 4) {
   
        distance = characteristics[[4]]
        stage_name = characteristics[[2]]
   
      } else {
     
        distance = NA
        stage_name = "one day race"
     
      }
   
      # if it's a TTT or some other error, just move on
      
      if(str_detect(stage_name, "TTT") | choose > length(d)) {
     
        stage <- tibble::tibble(rnk = as.character(NA),
                                rider = "Cancelled / TTT / anomalous",
                                team = as.character(NA),
                                win_seconds = 0,
                                time_seconds = 0,
                                length = 0,
                                speed = 0) %>%
        mutate(stage = s,
               distance = NA,
               stage_name = NA,
               race = all_stages$Race[[r]],
               year = all_stages$year[[r]],
               date = all_stages$Date[[r]])
     
        print(paste0("error in stage", s, "race", race_url))
     
        # otherwise continue with processing
        
        } else {
     
        # bring in stage distance again
          
          length <- page %>%
            html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "red", " " ))]') %>% 
            html_text() 
          
          if(identical(length, character(0)) == TRUE) {
            
            length = NA
            
          } else {
            
            length <- length %>% .[[2]] %>%
              str_replace_all("k","") %>%
              enframe(name = NULL) %>%
              #mutate(value = ifelse(nchar(value) == 5, str_sub(value, 2,4),
              #                      ifelse(nchar(value) == 6, str_sub(value, 2,5), str_sub(value, 2,6)))) %>%
              mutate(value = parse_number(value)) %>%
              mutate(value = as.numeric(value)) %>%
              as.data.frame() %>%
              .[1,1]
            
          }
          
          # bring in KOM link if it's a stage race
          
          if(tolower(stage_name) %in% c("prologue", "one day race") | str_detect(stage_name, "ITT")) {
            
            kom = 0
            
            kom_all = tibble(Rider = "blank", Pnt = 0)
            
          } else {
            
            # final stages screw this part up so get a different URL
            
            if(s == all_stages$MAX[[r]]) {            
            #if(s == stages_list[[r]]$MAX) {
            
              k <- paste0('https://www.procyclingstats.com/', race_url, "/kom") %>%
                read_html()
            
            } else {
              
              k <- paste0(url, "-kom") %>%
                read_html()
              
            }
            
            # now we can pull out KOM table and extract total KOM points to date
            
            select <- k %>%
              html_nodes('div') %>%
              html_attr(name = "class") %>%
              enframe(name = NULL) %>%
              filter(value %in% c("resultCont hide", "resultCont ")) %>%
              tibble::rowid_to_column() %>%
              filter(value == "resultCont ") %>%
              .[[1]]
            
            # some events don't have KOM
            
            if(is_empty(select)) {
              
              kom = 0
              
              kom_all = tibble(Rider = "blank", Pnt = 0)
              
            } else {
            
              # we have to pull in full standings to make proper comparisons in case riders DNF
              # if someone's points have been erased, replace with estimate of points
              
            kom_all <- k %>%
              html_nodes('table') %>%
              html_table() %>%
              .[[select]] %>%
              janitor::clean_names() %>%
              rename(Pnt = pnt) %>%
              mutate(Pnt = {if("Pnt" %in% names(.)) Pnt else 0}) %>%
              mutate(Pnt = ifelse(is.na(Pnt), lag(Pnt), Pnt)) %>%
              mutate(lagPnt = ifelse(is.na(lag(Pnt)), Pnt, lag(Pnt))) %>%
              filter(!(Pnt > lagPnt)) %>%
              select(Rider = rider, Pnt)

            kom <- kom_all %>%
              summarize(total = sum(Pnt, na.rm = T)) %>%
              .[[1]]
              
          }
            
          }
          
          # in case the winner's time is missing (eg, their stupid doping shit with Armstrong) bring in speed
          
          spd <- page %>% html_nodes('div.res-right') %>% html_text()
          
          x <- str_locate(spd, 'Avg. speed winner:') %>% .[[2]]
          
          SPD_WIN <- readr::parse_number(str_sub(spd, x+1, x+7))
          
          # now actually process the stage including getting times correct
          
          stage <- d[[choose]] %>%
            janitor::clean_names()
          
          if(race_url == "race/tour-de-france/2019" & s == 19) {
            
          stage <- stage %>%
            mutate(del = 1) %>%
            mutate(dnf = rnk) %>%
            mutate(rnk = rank(del, ties.method = "first")) %>%
            mutate(rnk = ifelse(dnf %in% c("OTL", "DNF", "DSQ", "NQ"), dnf, rnk)) %>%
            select(-del, -dnf)
            
          } 
          
          stage <- stage %>%
            
            # this processes time
            mutate(time = ifelse(time == ",,,,", NA, time),
                   time = ifelse(time == "--", NA, time)) %>%
            mutate(time = str_replace_all(time, ",", "")) %>%
            
            mutate(time = ifelse(time %in% c("0:00", "0:000:00"), 0, time)) %>%
            
            mutate(nch = nchar(time),
                   d1 = str_sub(time, 1, nch / 2),
                   d2 = str_sub(time, (nch / 2) + 1, nch),
                   dup = ifelse(d1 == d2, 1, 0)) %>%
            mutate(time = ifelse(dup == 0, time, d1)) %>%
            select(-d1, -d2, -dup, -nch) %>%
            mutate(win_time = ifelse(rnk == 1, time, NA)) %>%
            mutate(win_time = max(win_time, na.rm = T)) %>% 
            mutate(time = ifelse(rnk == 1, 0, time)) %>%
            
            select(rnk, rider, age, team, win_time, time) %>%
            
            mutate(count_colons = str_count(win_time, ":"),
                   count_colons1 = str_count(time, ":")) %>%
            
            tidyr::separate(win_time, c("h","m","s"), sep = ":") %>%
            tidyr::separate(time, c("h1","m1","s1"), sep = ":") %>%
            
            mutate(s1 = ifelse(is.na(s1), 0, s1),
                   m1 = ifelse(is.na(m1), 0, m1)) %>%
            
            mutate(s = ifelse(count_colons == 1, m, s),
                   m = ifelse(count_colons == 1, h, m),
                   h = ifelse(count_colons == 1, 0, h),
                   s1 = ifelse(count_colons1 == 1, m1, s1),
                   m1 = ifelse(count_colons1 == 1, h1, m1),
                   h1 = ifelse(count_colons1 == 1, 0, h1)) %>%
            
            mutate(speed_winner = SPD_WIN) %>%
            
            mutate(h = as.numeric(h),
                   m = as.numeric(m),
                   s = as.numeric(s),
                   h1 = as.numeric(h1),
                   m1 = as.numeric(m1),
                   s1 = as.numeric(s1),
                   win_seconds = (h * 3600) + (m * 60) + s,
                   time_seconds = (h1 * 3600) + (m1 * 60) + s1,
                   valid = ifelse(is.na(win_seconds), FALSE, TRUE),
                   win_seconds = ifelse(is.na(win_seconds), (length / speed_winner) * 60 * 60, win_seconds)) %>%
            
            fill(time_seconds) %>%
            
            mutate(time_seconds = ifelse(rnk == 1, 0, time_seconds),
                   total_seconds = ifelse(valid == TRUE, win_seconds + time_seconds,
                                          ifelse(rnk == 1, win_seconds, time_seconds))) %>%
            
            mutate(total_seconds = ifelse(total_seconds == 0, (lag(total_seconds) + lead(total_seconds)) / 2, total_seconds)) %>%
            
            select(rnk, rider, team, win_seconds, total_seconds) %>%
            
            mutate(length = length,
                   distance = distance,
                   stage_name = stage_name,
                   kom_progression = kom) %>%
            
            mutate(stage = s,
                   race = all_stages$Race[[r]],
                   year = all_stages$year[[r]],
                   date = all_stages$Date[[r]]) %>%
            
            mutate(rnk = as.character(rnk)) %>%
            
            mutate(kom_all = 0)
          
          # nest actual KOM standings
          
          stage$kom_all[[1]] <- kom_all %>%
            nest()
          
        }
          
        }
      
    }
    
    # write to the stage list for this race
    stage_race_list[[s]] <- stage
    
    Sys.sleep(1)
    
  }
  
  # write to the race list for this race
  races_list[[r]] <- bind_rows(stage_race_list)
  
  print(race_url)
  
  Sys.sleep(3)
  
}

tictoc::toc()

# process stage data

library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

#races_list <- c(read_rds("pcs-races-list-10-19-19.rds"), races_list)

races_list <- races_list[lengths(races_list) != 0]

for(f in 1:length(races_list)) { 
  
  df <- races_list[[f]] %>%
    mutate(date = as.character(date))
  
  df$race <- iconv(df$race, from="UTF-8", to = "ASCII//TRANSLIT")
  df$rider <- iconv(df$rider, from="UTF-8", to = "ASCII//TRANSLIT")
  
  df_list[[f]] <- df
  
  }

dbWriteTable(con,
             
             "pcs_stage_raw",
             
             bind_rows(df_list) %>%
               select(-kom_all),
             
             row.names = FALSE,
             
             overwrite = TRUE
             
             )


#
#
# Bring together all stage data -------------------------------------------

library(tidyverse)
library(lubridate)
library(rvest)
library(RMySQL)

#

races_list <- read_rds("pcs-races-list-10-19-19.rds")

# some races don't award KOM points at all (UAE TOUR / Tour of Oman)
# 
# kom_stages <- bind_rows(races_list) %>% filter(!(race == "Giro d'Italia" & year == 2013))
#   
# kom_stages$race <- iconv(kom_stages$race, from="UTF-8", to = "ASCII//TRANSLIT")
#   
# kom_stages <- kom_stages %>%
# 
#   filter(rnk == 1) %>%
#   select(stage, race, date, year, kom_all)
# 
# # some KOM all prints out 0 instead of having a data list column
# 
# new_kom_list <- vector("list", length(kom_stages$kom_all))
# 
# for(k in 1:length(kom_stages$kom_all)) {
#   
#   res <- kom_stages$kom_all[[k]]
#   
#   if(is.double(res)) {
#     
#     print(k)
# 
#   } else {
#     
#     new_kom_list[[k]] <- kom_stages[k, ]
#     
#   }
#   
# }
# 
# kom_stages_new <- bind_rows(new_kom_list)
# 
# # with empty list columns cleaned, continue
# 
# kom_stages <- kom_stages_new %>%
#   unnest() %>% 
#   unnest() %>% 
#   arrange(year, race, Rider, -stage) %>% 
#   
#   # Tour of Cal 2018 is error
#   mutate(Pnt = ifelse(stage == 1 & race == "Amgen Tour of California" & year == 2018, 0, Pnt)) %>%
#   mutate(Pnt = ifelse(Pnt < 0, 1, Pnt)) %>%
#   
#   mutate(diff = ifelse(lead(Rider) == Rider, Pnt - lead(Pnt), Pnt)) %>%
#   mutate(diff = ifelse(stage == 1, Pnt, diff)) %>%
#   
#   group_by(stage, race, date, year) %>% 
#   summarize(kom_points = sum(diff, na.rm = T)) %>%
#   ungroup() %>%
#   
#   # fix a couple stages
#   mutate(kom_points = ifelse(stage == 4 & race == "Tour de Suisse" & year == 2018, 15,
#                              ifelse(stage == 5 & race == "Tour de Suisse" & year == 2018, 140,
#                                     ifelse(year == 2017 & race == "Santos Tour Down Under",
#                                            ifelse(stage %in% c(3,4), 25,
#                                                   ifelse(stage == 5, 100, kom_points)), kom_points)))) %>%
#   mutate(kom_points = ifelse(race == "Criterium du Dauphine" & year == 2013,
#                              ifelse(stage == 2, 100,
#                                     ifelse(stage == 3, 30, kom_points)), kom_points)) %>%
#   # stage 5 Catalunya is wrong
#   mutate(kom_points = ifelse(race == "Volta Ciclista a Catalunya" & year == 2013,
#                              ifelse(stage == 5, 60, kom_points), kom_points)) %>%
#   
#   group_by(race, year) %>%
#   mutate(pct_kom = kom_points / sum(kom_points, na.rm = T),
#          kom_of_max = kom_points / max(kom_points, na.rm = T)) %>%
#   ungroup()

#kom_data <- bind_rows(races_list) %>%
  #filter(rnk == 1) %>%
  #select(stage, race, kom_progression, year) %>%
  #arrange(year, race, stage) %>%
  #mutate(kom_progression = ifelse(race == lag(race),
  #                                ifelse((is.na(kom_progression) | kom_progression == 0),
  #                                       lag(kom_progression), kom_progression), 
  #                                ifelse(is.na(kom_progression), 0, kom_progression))) %>%
  # fix wonky data
  #mutate(kom_progression = ifelse(stage == 4 & race == "Tirreno-Adriatico" & year == 2013, kom_progression - 40,
  #                                 ifelse(stage == 2 & race == "Crit?rium du Dauphin?" & year == 2013, kom_progression + 30,
  #                                        ifelse(stage == 6 & race == "Tour de Suisse" & year == 2013, kom_progression - 112,
  #                                               kom_progression)))) %>%
  #mutate(kom_points = ifelse(kom_progression == 0, 0,
  #                           ifelse(race == lag(race), kom_progression - lag(kom_progression), kom_progression))) %>%
  #mutate(kom_points = ifelse(is.na(kom_points), 0, kom_points)) %>%
  #
  #select(-kom_progression) %>%
  
  #group_by(race, year) %>%
  #mutate(pct_kom = kom_points / sum(kom_points, na.rm = T),
  #       kom_of_max = kom_points / max(kom_points, na.rm = T)) %>%
  #ungroup()

#

library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

f_r_climbs <- dbReadTable(con, "flamme_rouge_climbs") %>%
  mutate(year = as.numeric(year))

f_r_data <- dbReadTable(con, "flamme_rouge_characteristics") %>%
  mutate(year = as.numeric(year))

c_s_data <- dbReadTable(con, "cycling_stages_characteristics") %>%
  mutate(year = as.numeric(year))

c_s_objs <- ""

#

stage_data <- dbReadTable(con, "pcs_stage_raw")

stage_data$race <- iconv(stage_data$race, from="UTF-8", to = "ASCII//TRANSLIT")
  
stage_data <- stage_data %>%
  
  mutate(distance = str_replace(distance, "\\(", ""),
         distance = str_replace(distance, "\\)", ""),
         distance = as.numeric(str_replace(distance, "k", ""))) %>%
  
  mutate(length = ifelse(is.na(length), distance, length)) %>%
  
  mutate(length = ifelse(year == 2013 & race == 'E3 Prijs Vlaanderen - Harelbeke', 206, length)) %>%
  
  mutate(rider = str_replace(rider, team, "")) %>%
  mutate(finished = ifelse(rnk %in% c("DNF", "OTL", "DNS", "NQ", "DSQ"), NA, total_seconds)) %>%
  mutate(total_seconds = ifelse(total_seconds > 30000, NA, total_seconds)) %>%
  
  group_by(stage, race, year) %>%
  mutate(last_place = max(total_seconds, na.rm = T)) %>%
  ungroup() %>%
  
  filter(!rnk %in% c("DNS", "DSQ", "NQ")) %>%
  
  mutate(total_seconds = ifelse(rnk %in% c("OTL"), last_place, total_seconds)) %>%
  
  mutate(total_seconds = ifelse(is.na(total_seconds), last_place, total_seconds)) %>%
  
  mutate(total_seconds = ifelse(rnk == "DNF", NA, total_seconds)) %>%
  
  select(-last_place, -finished, -time_seconds) %>%
  mutate(speed = length / ((total_seconds) / 3600)) %>%
  
  mutate(variance_valid = ifelse((total_seconds - win_seconds) < 2400, total_seconds, NA)) %>%
  
  mutate(rnk = as.numeric(rnk)) %>%
  mutate(t10_time = ifelse(!is.na(rnk), total_seconds, NA)) %>%
  mutate(x10 = ifelse(rnk == 10, total_seconds, NA),
         x20 = ifelse(rnk == 20, total_seconds, NA),
         x40 = ifelse(rnk == 40, total_seconds, NA),
         x3 = ifelse(rnk == 3, total_seconds, NA),
         x5 = ifelse(rnk == 5, total_seconds, NA),
         top_var = ifelse(rnk < 41, total_seconds, NA)) %>%
  
  group_by(stage, race, year) %>%
  mutate(variance = round(sd(variance_valid, na.rm = T),0),
         top_variance = round(sd(top_var, na.rm = T),0),
         x10 = mean(x10, na.rm = T),
         x20 = mean(x20, na.rm = T),
         x40 = mean(x40, na.rm = T),
         x3 = mean(x3, na.rm = T),
         x5 = mean(x5, na.rm = T),
         mean_time = mean(t10_time, na.rm = T),
         med_time = median(t10_time, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(qual_time = (x10 + x20 + x40) / 3) %>%

  mutate(rel_time = ifelse(variance == 0, 0, (total_seconds - ((med_time + mean_time) / 2)) / variance),
         rel_qual = ifelse(top_variance == 0, 0, (total_seconds - qual_time) / top_variance),
         rel_speed = (total_seconds - x10) / length,
         gain_1st = (total_seconds - win_seconds),
         gain_3rd = (total_seconds - x3),
         gain_5th = (total_seconds - x5),
         gain_10th = (total_seconds - x10),
         gain_20th = (total_seconds - x20),
         gain_40th = (total_seconds - x40)) %>%
  
  filter(!str_detect(stage_name, "TTT")) %>%
  
  select(-t10_time, -x10, -x20, -x40, -top_var, -x3, -x5) %>%
  
  mutate(date = as.Date(paste0(year, "-", str_sub(date, 4, 5), "-", as.numeric(str_sub(date, 1, 2)))) + (stage - 1)) %>%
  
  mutate(time_trial = ifelse(stage_name %in% c("Time trial", "Prologue") | str_detect(stage_name, "ITT"), TRUE, FALSE)) %>%
  
  # joins with KOM stage data from above
  # this rates every stages KOM difficulty relative to toughest KOM stage of that race
  
  #left_join(
  #  
  #  kom_stages %>%
  #    select(-date, -kom_points), by = c("race", "stage", "year")) %>%
  
  mutate(grand_tour = ifelse(race == "Tour de France", TRUE,
                             ifelse(race == "Giro d'Italia", TRUE,
                                    ifelse(str_sub(race, 1, 10) == "Vuelta a E", TRUE,
                                           ifelse(str_sub(race, 1, 11) == "La Vuelta c" & year == 2018, TRUE, FALSE))))) %>%
  
  
  mutate(one_day_race = ifelse(stage_name == "One day race", TRUE, FALSE)) %>%
  
  # this links every non-grand tour's strongest KOM stage which I've manually rated
  # in terms of climbing difficulty (with 5 = primarily flat, 75 = toughest grand tour stage)
  #left_join(
  #  
  #  read_csv("cycling-queen-stages.csv") %>%
  #    select(-stage_name, -HC, -X1, -X2), by = c("stage", "race", "year")
  #  
  #) %>%
  
  # replace the negative KOM errors
  # 
  
  #group_by(race, year) %>%
  #mutate(kom_max = max(climb_difficulty, na.rm = T)) %>%
  #ungroup() %>%
  
  #mutate(kom_max = ifelse(grand_tour == TRUE, 75, 
  #                        ifelse(is.na(kom_max), 0, kom_max))) %>%
  
  # now that we know the typical level of a race and the gradiations off of that level
  # we can rate the climb difficulty relative to the toughest grand tour stage
  # eg for 2018 TDF a 75 stage was 19th - Aspin > Tourmalet > Aubisque
  
  #mutate(climb_difficulty = ifelse(is.na(climb_difficulty), kom_of_max * kom_max, climb_difficulty),
  #       climb_difficulty = ifelse(stage_name == "One day race", kom_max, climb_difficulty)) %>%
  
  #mutate(climb_difficulty = ifelse(is.na(climb_difficulty) | 
  #                                   climb_difficulty == "NaN" | 
  #                                   climb_difficulty == "-Inf", 0, climb_difficulty)) %>%
  
  mutate(climb_difficulty = NA) %>%

  # find rider position in team for stage
  
  group_by(team, stage, race, year) %>%
  mutate(tm_pos = rank(rnk, ties.method = "first")) %>%
  ungroup()

#
# Performance relative to GC
#

stage_data$rider <- iconv(stage_data$rider, from="UTF-8", to = "ASCII//TRANSLIT")

winners <- dbReadTable(con, "all_races") 

winners$Winner <-  str_to_title(tolower(winners$Winner))
winners$Winner <- iconv(winners$Winner, from="UTF-8", to = "ASCII//TRANSLIT")
winners$Race <- str_to_title(tolower(winners$Race))
winners$Race <- iconv(winners$Race, from="UTF-8", to = "ASCII//TRANSLIT")
winners$Race <- tolower(winners$Race)
winners$Date <- as.Date(winners$Date)

#
# find the GC winner performance in stage
#

gc_performance <- winners %>%

  select(race = Race,
         date = Date,
         winner = Winner,
         class = Class,
         url, year) %>%
  
  inner_join(
    
    stage_data %>%
      select(-date) %>%
      mutate(rider = str_to_title(tolower(rider))) %>%
      mutate(race = tolower(race)), by = c("race", "year", "winner" = "rider")
    
  ) %>%
  
  select(gc_winner = winner, race, year, stage, gc_seconds = total_seconds, class, date)

# combine with GC performance data and calc Rel to GC

stage_data <- stage_data %>%
  
  select(-date) %>%
  
  mutate(race = tolower(race)) %>%
  
  left_join(
    
    gc_performance, by = c("year", "race", "stage")
    
  ) %>%
  
  mutate(date = date + (stage - 1)) %>%
  
  select(-mean_time, -med_time, -qual_time, -rel_qual, -pct_kom, -kom_of_max,
         -climb_difficulty, -kom_max, -variance_valid, -distance, -kom_progression,
         -rel_time) %>%
  
  mutate(gain_gc = total_seconds - gc_seconds) %>%
  
  mutate(gc_pos = ifelse(rider == gc_winner, rnk, NA)) %>%
  
  group_by(race, stage, year) %>%
  mutate(gc_pos = mean(gc_pos, na.rm = T) + 5) %>%
  ungroup() %>%
  
  mutate(back_5 = ifelse(gc_pos == rnk, total_seconds, NA)) %>%
  
  group_by(race, stage, year) %>%
  mutate(back_5_seconds = mean(back_5, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(gain_back_5 = total_seconds - back_5_seconds) %>%
  
  select(-back_5)

# Join with FR climb and profile data

stage_data <- stage_data %>%
  
  mutate(race = ifelse(race %in% c("la vuelta ciclista a espana", "la vuelta a espana"), "vuelta a espana", race)) %>%
  
  unique() %>%
  
  left_join(
    
    f_r_data %>%
      select(-length, -slug) %>%
      mutate(race = tolower(race)) %>%
      mutate(race = ifelse(race == "la route d'occitanie" & year == 2019,
                           "la route d'occitanie - la depeche du midi", race)), by = c("race", "stage", "year")
    
  ) %>%
  
  left_join(
    
    f_r_climbs %>%
      
      mutate(race = tolower(race)) %>%
      mutate(race = ifelse(race %in% c("la vuelta ciclista a espana", "la vuelta a espana"), "vuelta a espana", race),
             race = ifelse(race == "la route d'occitane", "la route d'occitanie", race)) %>%
      
      left_join(f_r_data %>%
                  select(race, stage, year, stage_length = length) %>%
                  mutate(race = tolower(race)) %>%
                  mutate(race = ifelse(race == "la route d'occitanie" & year == 2019,
                                       "la route d'occitanie - la depeche du midi", race)), by = c("race", "stage", "year")) %>%
      
      group_by(stage, race, year) %>%
      mutate(position_highest = max(model_category, na.rm = T),
             last_climb = max(end_distance, na.rm = T)) %>%
      ungroup() %>%
      
      mutate(position_highest = ifelse(position_highest == model_category, end_distance / stage_length, NA),
             last_climb = ifelse(last_climb == end_distance, model_category, NA)) %>%
      
      mutate(summit_finish = ifelse(abs(end_distance - stage_length) < 2, TRUE, FALSE)) %>%
      mutate(summit_finish = ifelse(race == "tour de romandie" & stage == 4 &
                                      year == 2019 & climb_name == "Torgon", TRUE, summit_finish)) %>%
      
      # increase KOM points by 25% if summit finish
      mutate(basic_kom_points = model_category,
             kom_points = ifelse(summit_finish == TRUE, model_category * 1.25, model_category),
             climbing_end = ifelse((stage_length - end_distance) < 20.1, kom_points, NA)) %>%
      
      group_by(race, stage, year) %>%
      summarize(cat_climb_length = sum(end_distance - start_distance, na.rm = T),
                concentration = max(kom_points, na.rm = T),
                number_cat_climbs = sum(kom_points >= 1, na.rm = T),
                climbing_final_20km = sum(climbing_end, na.rm = T),
                raw_climb_difficulty = sum(basic_kom_points, na.rm = T),
                act_climb_difficulty = sum(kom_points, na.rm = T),
                last_climb = max(last_climb, na.rm = T),
                position_highest = mean(position_highest, na.rm = T),
                summit_finish = max(summit_finish, na.rm = T)) %>%
      ungroup(), by = c("race", "stage", "year")
    
  ) %>%
  
  mutate(cat_climb_length = ifelse(is.na(cat_climb_length),
                                   ifelse(is.na(total_elev_change), NA, 0), cat_climb_length),
         number_cat_climbs = ifelse(is.na(number_cat_climbs), 0, number_cat_climbs),
         concentration = ifelse(is.na(concentration),
                                ifelse(is.na(total_elev_change), NA, 1),
                                ifelse(is.na(concentration), 1, concentration)),
         climbing_final_20km = ifelse(is.na(climbing_final_20km),
                                      ifelse(is.na(total_elev_change), NA, 0), climbing_final_20km),
         raw_climb_difficulty = ifelse(is.na(raw_climb_difficulty),
                                       ifelse(is.na(total_elev_change), NA, 0), raw_climb_difficulty),
         act_climb_difficulty = ifelse(is.na(act_climb_difficulty),
                                       ifelse(is.na(total_elev_change), NA, 0), act_climb_difficulty),
         position_highest = ifelse(is.na(position_highest),
                                   ifelse(is.na(total_elev_change), NA, 0), position_highest),
         last_climb = ifelse(is.na(last_climb),
                             ifelse(is.na(total_elev_change), NA, 0), last_climb)) %>%
  
  filter(!is.na(act_climb_difficulty)) %>%
  
  # apply strength of field basic
  mutate(level = ifelse(race == "tour de france", 1,
                        ifelse(race %in% c("world championships", "giro d'italia", "vuelta a espana",
                                           "milano - sanremo", "liege - bastogne liege", 
                                           "ronde van vlaanderen / tour des flandres", 
                                           "ronde van vlaanderen - tour des flandres",
                                           "il lombardia", "paris - roubaix", "la fleche wallonne",
                                           "amstel gold race"), 0.5,
                               ifelse(race %in% c("criterium du dauphine", "paris - nice",
                                                  "tirreno - adriatico", "tour de suisse",
                                                  "gent-wevelgem in flanders fields",
                                                  "clasica ciclista san sebastian",
                                                  "strade bianche", "volta ciclista a catalunya",
                                                  "tour de romandie", "itzulia basque country",
                                                  "vuelta a pais vasco"), 0.25,
                                      ifelse(class %in% c("1.UWT", "2.UWT"), 0,
                                             ifelse(class %in% c("1.HC", "2.HC"), -0.25,
                                                    ifelse(class %in% c("1.1", "2.1"), -0.5, -1.0))))))) %>%
  filter(!(is.na(act_climb_difficulty))) %>%
  
  mutate(position_highest = ifelse(position_highest > 1, 1, position_highest),
         position_highest = ifelse(is.na(position_highest), median(position_highest, na.rm = T), position_highest),
         summit_finish = ifelse(is.na(summit_finish), 0, summit_finish)) %>%
  
  unique() %>%
  
  filter(!(race == "abu dhabi tour" & stage == 5))

#
#
# STRENGTH OF FIELD
#
#

riders_stages <- stage_data %>%
  
  filter(time_trial == FALSE) %>%
  
  filter(rnk < 11) %>%
  
  select(rider1 = rider, stage, race, year) %>%
  unique() %>%
  
  inner_join(
    
    stage_data %>%
      
      filter(time_trial == FALSE) %>%
      
      filter(rnk < 11) %>%   
      
      select(rider2 = rider, stage, race, year) %>%
      unique(), by = c("stage", "race", "year")	
    
  ) %>%
  
  filter(!(rider1 == rider2)) %>%
  unique()

#

matches <- riders_stages %>%
  
  filter(year > 2016) %>%
  
  group_by(rider1, rider2) %>%
  summarize(n = n()) %>%
  ungroup() %>% 
  
  filter(n > 2) %>% 
  
  group_by(rider1) %>%
  mutate(n1 = n()) %>%
  ungroup() %>%
  
  group_by(rider2) %>%
  mutate(n2 = n()) %>%
  ungroup() %>%
  
  filter(n1 > 1 & n2 > 1) %>%
  
  select(rider1, rider2, n) %>%
  
  spread(rider2, n) %>%
  
  gather(rider2, n, -rider1) %>%
  
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  
  spread(rider2, n)

#
# PCA
#

pxxx <- prcomp(matches[, 2:ncol(matches)], scale = TRUE, center = TRUE)

# two PCs capture 26%

# PC1 is the sprinter vs mountains classifier

rxxx <- cbind(
  pxxx$x, 
  matches %>%
    select(rider1)) %>%
  select(rider1, PC1, PC2, PC3, PC4)

ggplot(rxxx, aes(x = PC1, y = PC2, label = rider1))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_label()+
  scale_fill_viridis_c()+
  labs(x = "PC1: climber vs sprinter", y = "PC2: strong field vs weak field")

#

riders_top10s <- stage_data %>%
  filter(rnk < 11) %>% 
  
  select(rider, stage, race, year) %>%
  unique() %>%
  
  inner_join(rxxx %>% 
               select(rider = rider1, PC1)) %>%
  group_by(stage, race, year) %>%
  summarize(tot = sum(PC1, na.rm = T), 
            n = n()) %>%
  ungroup()

#

fields <- stage_data %>%
  
  select(stage, race, year, rider, class, grand_tour, date, rnk) %>%
  unique() %>%
  
  inner_join(rxxx %>% 
               select(rider = rider1, PC1, PC2)) %>% 
  group_by(race, stage, year) %>% 
  filter(rank(rnk, ties.method = "first") < 26) %>%
  ungroup() %>%
  
  group_by(stage, race, year, class, grand_tour, date) %>%
  summarize(tot = mean(PC2, na.rm = T), 
            n = n()) %>%
  ungroup() %>% 
  
  separate(class, c("type", "class"), by = ".") %>%
  
  mutate(class = ifelse(is.na(class), "UWT", class), 
         type = ifelse(type == "WT", NA, type)) %>%
  
  mutate(monument = ifelse(str_detect(race, "sanremo") | 
                             str_detect(race, "tour des flandres") | 
                             str_detect(race, "roubaix") | 
                             str_detect(race, "bastogne") | 
                             str_detect(race, "il lombardia"), TRUE, FALSE)) %>%
  
  mutate(tot = ifelse(n < 25, (tot * n) + (-1.5 * (25- n)), tot * 25))

#





  
#
#
#
# Analysis Part Below
#
#
#

test_climbs_stage_outcomes <- stage_data %>%
  mutate(race = ifelse(race == "la vuelta ciclista a espana", "vuelta a espana", race)) %>%
  
  left_join(
    
    f_r_climbs %>%
      
      mutate(race = tolower(race)) %>%
      mutate(race = ifelse(race == "la vuelta a espana", "vuelta a espana", race),
             race = ifelse(race == "la route d'occitane", "la route d'occitanie", race)) %>%
      
      left_join(f_r_data %>%
                  select(race, stage, year, stage_length = length) %>%
                  mutate(race = tolower(race)) %>%
                  mutate(race = ifelse(race == "la route d'occitanie" & year == 2019,
                                       "la route d'occitanie - la depeche du midi", race)), by = c("race", "stage", "year")) %>%

      mutate(summit_finish = ifelse(abs(end_distance - stage_length) < 2, TRUE, FALSE)) %>%
      mutate(summit_finish = ifelse(race == "tour de romandie" & stage == 4 &
                                      year == 2019 & climb_name == "Torgon", TRUE, summit_finish)) %>%
      mutate(last_50km = ifelse(abs(end_distance - stage_length) < 51, TRUE, FALSE)) %>%
      filter(last_50km == TRUE) %>%
      group_by(race, stage, year) %>%
      mutate(two_highest = ifelse(rank(-model_category, ties.method = "min") < 2, TRUE, FALSE)) %>%
      ungroup(), by = c("race", "stage", "year")) %>%
  
  inner_join(
    
    f_r_data %>%
      mutate(race = tolower(race)) %>%
      mutate(race = ifelse(race == "la route d'occitanie" & year == 2019,
                           "la route d'occitanie - la depeche du midi", race)) %>%
      select(stage, race, year) %>%
      unique(), by = c("stage", "race", "year")
    
  ) %>%
  
  filter(summit_finish == FALSE | is.na(summit_finish)) %>%
  
  mutate(model_category = ifelse(is.na(model_category), 0.1, model_category),
         gradient = ifelse(is.na(gradient), 0.01, gradient),
         length.y = ifelse(is.na(length.y), 0.5, length.y)) %>%
  
  group_by(race, stage, year) %>%
  mutate(within60 = mean(gain_gc < 60, na.rm = T),
            within30 = mean(gain_gc < 30, na.rm = T),
            within10 = mean(gain_gc < 10, na.rm = T)) %>%
  ungroup() %>%
  
  filter(rnk == 1 & year > 2016 & class %in% c("1.UWT", "2.UWT")) %>%
  
  group_by(race, stage, year) %>% 
  mutate(n_last_50km = sum(last_50km, na.rm = T), 
         n_two_highest = sum(two_highest, na.rm = T), 
         n_both = sum(last_50km == TRUE & two_highest == TRUE, na.rm = T),
         n_all = n()) %>% 
  ungroup() %>%
  mutate(wt1 = 1 / n_last_50km, 
         wt2 = 1 / n_two_highest, 
         wt3 = 1 / n_both,
         wt4 = 1 / n_all) %>%
  
  filter(time_trial == FALSE & is.na(stage_length) == FALSE)
  
#

gam_mod <- mgcv::gam(within30 ~ s(gradient) + s(model_category), 
                     data = test_climbs_stage_outcomes %>%
                       filter(two_highest == TRUE), weights = wt2)
  
fitted <- cbind(pred = predict(gam_mod, test_climbs_stage_outcomes %>% filter(two_highest == TRUE)), 
                test_climbs_stage_outcomes %>% filter(two_highest == TRUE)) 

# utilizes the PCA riders work from below

finishers <- test_climbs_stage_outcomes %>%
  
  inner_join(
    
    stage_data %>%
      filter(rnk < 11) %>% 
      inner_join(rxxx %>% 
                   select(rider = rider1, PC1)) %>%
      group_by(stage, race, year) %>%
      summarize(tot = sum(PC1, na.rm = T), 
                sd = sd(PC1, na.rm = T),
                n = n()) %>%
      ungroup(), by = c("race", "stage", "year"))

gam_mod <- mgcv::gam(tot ~ s(model_category), 
                     data = finishers %>%
                       filter(two_highest == TRUE), weights = wt2)

fitted <- cbind(pred = predict(gam_mod, finishers %>% filter(two_highest == TRUE)), 
                finishers %>% filter(two_highest == TRUE)) 

finishers <- test_climbs_stage_outcomes %>%
  select(-rnk, -one_day_race) %>%
  inner_join(
    
    stage_data %>%
      inner_join(rxxx %>% 
                   select(rider = rider1, PC1, PC3)), by = c("race", "stage", "year")) %>% 
  mutate(t10 = ifelse(rnk < 11, 1, 0)) %>% 
  
  # cutoffs were PC1 > 7 for sprinters, PC1 < -2 for climbers, PC3 > 1.67 for classic specialists
  filter(PC1 < -2)


gam_mod <- mgcv::gam(t10 ~ s(model_category) + s(gradient), 
                     data = finishers %>% group_by(stage, race, year) %>% mutate(n = 1 / n()) %>% ungroup() %>%
                       filter(two_highest == TRUE), weights = n, family = "binomial")

fitted <- cbind(
  
  pred = predict(gam_mod, 
                 
                 tibble(model_category = c(1.5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 16), 
                        gradient = 0.065)), 
  
  tibble(model_category = c(1.5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 16))) %>%
  as_tibble() %>% 
  mutate(perc = exp(pred)/ (1+exp(pred)))

#

stage_data <- stage_data %>%
  
  mutate(race = ifelse(race %in% c("la vuelta ciclista a espana", "la vuelta a espana"), "vuelta a espana", race)) %>%
  
  unique() %>%
  
  left_join(
    
    f_r_data %>%
      select(-length, -slug) %>%
      mutate(race = tolower(race)) %>%
      mutate(race = ifelse(race == "la route d'occitanie" & year == 2019,
                           "la route d'occitanie - la depeche du midi", race)), by = c("race", "stage", "year")
    
  ) %>%
  
  left_join(
    
    f_r_climbs %>%
      
      mutate(race = tolower(race)) %>%
      mutate(race = ifelse(race %in% c("la vuelta ciclista a espana", "la vuelta a espana"), "vuelta a espana", race),
             race = ifelse(race == "la route d'occitane", "la route d'occitanie", race)) %>%
      
      left_join(f_r_data %>%
                  select(race, stage, year, stage_length = length) %>%
                  mutate(race = tolower(race)) %>%
                  mutate(race = ifelse(race == "la route d'occitanie" & year == 2019,
                                       "la route d'occitanie - la depeche du midi", race)), by = c("race", "stage", "year")) %>%
      
      group_by(stage, race, year) %>%
      mutate(position_highest = max(model_category, na.rm = T),
             last_climb = max(end_distance, na.rm = T)) %>%
      ungroup() %>%
      
      mutate(position_highest = ifelse(position_highest == model_category, end_distance / stage_length, NA),
             last_climb = ifelse(last_climb == end_distance, model_category, NA)) %>%
      
      mutate(summit_finish = ifelse(abs(end_distance - stage_length) < 2, TRUE, FALSE)) %>%
      mutate(summit_finish = ifelse(race == "tour de romandie" & stage == 4 &
                                      year == 2019 & climb_name == "Torgon", TRUE, summit_finish)) %>%
      
      # increase KOM points by 25% if summit finish
      mutate(basic_kom_points = model_category,
             kom_points = ifelse(summit_finish == TRUE, model_category * 1.25, model_category),
             climbing_end = ifelse((stage_length - end_distance) < 20.1, kom_points, NA)) %>%
      
      group_by(race, stage, year) %>%
      summarize(cat_climb_length = sum(end_distance - start_distance, na.rm = T),
                concentration = max(kom_points, na.rm = T),
                number_cat_climbs = sum(kom_points >= 1, na.rm = T),
                climbing_final_20km = sum(climbing_end, na.rm = T),
                raw_climb_difficulty = sum(basic_kom_points, na.rm = T),
                act_climb_difficulty = sum(kom_points, na.rm = T),
                last_climb = max(last_climb, na.rm = T),
                position_highest = mean(position_highest, na.rm = T),
                summit_finish = max(summit_finish, na.rm = T)) %>%
      ungroup(), by = c("race", "stage", "year")
    
  ) %>%
  
  mutate(cat_climb_length = ifelse(is.na(cat_climb_length),
                                   ifelse(is.na(total_elev_change), NA, 0), cat_climb_length),
         number_cat_climbs = ifelse(is.na(number_cat_climbs), 0, number_cat_climbs),
         concentration = ifelse(is.na(concentration),
                                   ifelse(is.na(total_elev_change), NA, 1),
                                ifelse(is.na(concentration), 1, concentration)),
         climbing_final_20km = ifelse(is.na(climbing_final_20km),
                                       ifelse(is.na(total_elev_change), NA, 0), climbing_final_20km),
         raw_climb_difficulty = ifelse(is.na(raw_climb_difficulty),
                                       ifelse(is.na(total_elev_change), NA, 0), raw_climb_difficulty),
         act_climb_difficulty = ifelse(is.na(act_climb_difficulty),
                                   ifelse(is.na(total_elev_change), NA, 0), act_climb_difficulty),
         position_highest = ifelse(is.na(position_highest),
                                   ifelse(is.na(total_elev_change), NA, 0), position_highest),
         last_climb = ifelse(is.na(last_climb),
                             ifelse(is.na(total_elev_change), NA, 0), last_climb)) %>%
  
  filter(!is.na(act_climb_difficulty)) %>%
  
  # apply strength of field basic
  mutate(level = ifelse(race == "tour de france", 1,
                        ifelse(race %in% c("world championships", "giro d'italia", "vuelta a espana",
                                           "milano - sanremo", "liege - bastogne liege", 
                                           "ronde van vlaanderen / tour des flandres", 
                                           "ronde van vlaanderen - tour des flandres",
                                           "il lombardia", "paris - roubaix", "la fleche wallonne",
                                           "amstel gold race"), 0.5,
                                           ifelse(race %in% c("criterium du dauphine", "paris - nice",
                                                              "tirreno - adriatico", "tour de suisse",
                                                              "gent-wevelgem in flanders fields",
                                                              "clasica ciclista san sebastian",
                                                              "strade bianche", "volta ciclista a catalunya",
                                                              "tour de romandie", "itzulia basque country",
                                                              "vuelta a pais vasco"), 0.25,
                                                  ifelse(class %in% c("1.UWT", "2.UWT"), 0,
                                                         ifelse(class %in% c("1.HC", "2.HC"), -0.25,
                                                                ifelse(class %in% c("1.1", "2.1"), -0.5, -1.0))))))) %>%
  filter(!(is.na(act_climb_difficulty))) %>%
  
  mutate(position_highest = ifelse(position_highest > 1, 1, position_highest),
         position_highest = ifelse(is.na(position_highest), median(position_highest, na.rm = T), position_highest),
         summit_finish = ifelse(is.na(summit_finish), 0, summit_finish)) %>%
  
  unique() %>%
  
  filter(!(race == "abu dhabi tour" & stage == 5))

#
#
#
#
#
#


# Analysis Prep -----------------------------------------------------------

# analysis of stages

set.seed(17)

stages_pca <- prcomp(stage_data %>%
                       filter(rnk == 1) %>%
                       filter(time_trial == FALSE) %>%
                       select(summit_finish, act_climb_difficulty, concentration,
                              climbing_final_20km, position_highest, number_cat_climbs,
                              total_elev_change, final_1km_gradient, highest_point,
                              last_climb), center = TRUE, scale = TRUE)

summary(stages_pca)

stages_pca$rotation

# generally can capture 90% of variance with 5 PCs and 70% with 3 PCs

# 1st PC = long, slow, high variance, tough climbing, not time trial
# 2nd PC = one day race
# 3rd PC = grand tour, long, fast, flat
# 4th PC = not grand tour, not ITT, not one day race

# partitioning of stages

# SPRINT STAGES
# MOUNTAIN STAGES
# INTERMEDIATE STAGES
# ONE DAY RACES
# TIME TRIALS
# UPHILL TIME TRIALS

stage_partition_data <- stage_data %>%
  filter(rnk == 1) %>%
  filter(time_trial == FALSE) %>%
  select(summit_finish, act_climb_difficulty, concentration,
         climbing_final_20km, position_highest, number_cat_climbs,
         total_elev_change, final_1km_gradient, highest_point, last_climb,
         grand_tour, one_day_race, time_trial, stage, race, year, winner = rider) %>%
  
  cbind(
    
    stages_pca$x
    
  ) %>%
  
  select(summit_finish, act_climb_difficulty, concentration,
         climbing_final_20km, position_highest, number_cat_climbs,
         total_elev_change, final_1km_gradient, highest_point,
         last_climb,
         grand_tour, one_day_race, time_trial, stage, race, year, winner, PC1, PC2, PC3, PC4, PC5) %>%
  
  rbind(
    
    stage_data %>%
      filter(rnk == 1) %>%
      filter(time_trial == TRUE) %>%
      select(summit_finish, act_climb_difficulty, concentration,
             climbing_final_20km, position_highest, number_cat_climbs,
             total_elev_change, final_1km_gradient, highest_point,
             last_climb,
             grand_tour, one_day_race, time_trial, stage, race, year, winner = rider) %>%
      mutate(PC1 = NA,
             PC2 = NA,
             PC3 = NA,
             PC4 = NA,
             PC5 = NA)
    
  ) %>%
  
  mutate(stage_climbing = ifelse(summit_finish == 0, act_climb_difficulty, act_climb_difficulty - (last_climb)))

# kmeans analysis

km_stages <- kmeans(stage_partition_data %>% 
                      filter(!is.na(PC1)) %>% 
                      select(stage_climbing, total_elev_change, concentration, position_highest) %>%
                      .[, 1:4], 3)

km_stages$centers

km_parts <- cbind(
  
  cl = km_stages$cluster, 
  
  stage_partition_data %>% 
    filter(!is.na(PC1)))

km_brkdwn <- km_parts %>% 
  group_by(cl) %>% 
  summarize(mean(grand_tour), 
            mean(act_climb_difficulty),
            mean(climbing_final_20km), mean(position_highest),
            mean(final_1km_gradient), mean(total_elev_change), mean(concentration),
            mean(number_cat_climbs), mean(summit_finish), mean(highest_point),
            mean(one_day_race), mean(PC1), mean(PC2), mean(PC3), mean(PC4))

km_parts %>% 
  gather(stat, value, -cl, -race, -year, -stage, -winner) %>%
  ggplot(aes(x = stat, y = value, fill = as.factor(cl)))+
    geom_boxplot()+
    facet_wrap(~stat, scales = "free")+
    scale_fill_manual(values = c("dark red", "navy", "gold", "white"), name = "cluster")+
    labs(x = "", y = "", title = "Cycling stage partitioning")+
    theme(plot.title = element_text(face = "bold"))

#
#
# cluster rider types

km_parts <- km_parts %>%
  
  left_join(
    
    riders_top10s %>%
      select(stage, race, year, rider_cl = tot), by = c("race", "stage", "year")
    
  ) %>%
  
  mutate(rider_cl = ifelse(is.na(rider_cl), 0, rider_cl)) %>%
  
  # this is to account for certain stages having big climbs at the beginning and nothing else
  mutate(adj_concentration = ifelse(position_highest < 0.50, (position_highest / 0.50) * concentration, concentration))

# classification problem

library(rpart)

classification_data <- readr::read_csv("training-classifiers.csv") %>%
  
  inner_join(
    
    km_parts, by = c("race", "stage", "year")
    
  ) %>%
  
  mutate(type = ifelse(type %in% c("cobbles", "cobbled"), "sprint", type))

#

training <- classification_data %>%
  
  select(type, act_climb_difficulty, length, variance, top_variance, number_cat_climbs, position_highest,
         speed, gain_gc, total_elev_change, final_1km_gradient, concentration,
         climbing_final_20km, summit_finish, PC1, PC2, PC3, PC4)

#

tree_model <- rpart(type ~ act_climb_difficulty + concentration + length + speed + top_variance + position_highest + 
                      total_elev_change + final_1km_gradient + number_cat_climbs + climbing_final_20km + summit_finish,
                    data = training)

pc_tree_model <- rpart(type ~ PC1 + PC2 + PC3 + PC4, data = training)

#

apply_tree_model <- km_parts %>%
  
  mutate(type = ifelse(concentration > 8,
                       ifelse(act_climb_difficulty > 22, "mountain", 
                              ifelse(concentration > 12, 
                                     ifelse((concentration / act_climb_difficulty) > 0.5 &
                                              position_highest < 0.33, "intermediate", "mountain"), "intermediate")),
                       ifelse(number_cat_climbs > 2.5 & concentration > 4,
                              ifelse(final_1km_gradient > 0.03, "sharp_finish", "intermediate"),
                              ifelse(final_1km_gradient > 0.03, "sharp_finish", "sprint"))),
         type = ifelse(race %in% c("amstel gold race", "e3 harelbeke", "e3 binckbank classic", 
                                   "record bank e3 harelbeke", "omloop het nieuwsblad", "gent-wevelgem in flanders fields",
                                   "omloop het nieuwsblad elite", "dwars door vlaanderen / a travers la flandre", 
                                   "dwars door vlaanderen - a travers la flandre", "milano - sanremo", 
                                   "liege - bastogne - liege", "il lombardia", "ronde van vlaanderen / tour des flandres",
                                   "ronde van vlaanderen - tour des flandres", "clasica ciclista san sebastian"), "intermediate", type),
         type = ifelse(race %in% c("paris-roubaix", "paris - roubaix") | 
                         (race == "tour de france" & 
                            ((year == 2014 & stage == 5) | 
                               (year == 2015 & stage == 4) | 
                               (year == 2018 & stage == 9))), "cobbles", type),
         type = ifelse(race %in% c("la fleche wallone", "strade bianche"), "sharp_finish", type),
         type = ifelse(race %in% c("dubai tour") & year %in% c(2017, 2018) & stage == 4, "sharp_finish", type),
         type = ifelse(race == "vuelta a espana" & year == 2015 & stage == 4, "sharp_finish", type),
         type = ifelse(race == 'tour of utah' & year == 2017 & stage == 7, "intermediate", type),
         type = ifelse(race == "tour de france" & year == 2016 & stage == 10, "intermediate", type))

#
#
# Random Forests

library(randomForest)

training <- readr::read_csv("training-classifiers.csv") %>%
  
  inner_join(
    
    km_parts, by = c("race", "stage", "year")
    
  ) %>%
  
  mutate(type = ifelse(type %in% c("cobbles", "cobbled", "sharp finish"), "sprint", type),
         type = as.factor(type))

#

model_list <- vector("list", 100)
predict_list <- model_list
importance_list <- model_list
confusion_list <- model_list
oob_list <- model_list

for(t in 1:length(model_list)) {

train <- sample(1:nrow(training), nrow(training) / 2)

#

NTREE <- 250

rf_model <- randomForest(type ~ stage_climbing + position_highest +   
                           total_elev_change + number_cat_climbs +  
                           rider_cl,
                         data = training, subset = train,
                         ntrees = NTREE,
                         mtry = 2, importance = TRUE)

rf_predict <- cbind(training[-train,], pred = predict(rf_model, newdata = training[-train, ])) %>%
  select(stage, year, race, type, pred) %>% 
  mutate(match = ifelse(pred == type, 1, 0))

model_list[[t]] <- rf_model
predict_list[[t]] <- rf_predict %>%
  mutate(iteration = t)
oob_list[[t]] <- tibble(oob = rf_model$err.rate[, 1][[NTREE]],
                        iteration = t)

}

prds <- bind_rows(predict_list) %>%
  group_by(stage, year, race, pred, type) %>% 
  count() %>% 
  ungroup() %>% 
  
  group_by(stage, year, race, pred) %>%
  mutate(perc = n / sum(n), 
         diff = n()) %>%
  ungroup()

#

bind_rows(oob_list) %>% summarize(mean(oob))

#

for(m in 1:length(model_list)) {
  
  importance_list[[m]] <- importance(model_list[[m]]) %>%
    as.data.frame() %>%
    rownames_to_column()
  
  confusion_list[[m]] <- model_list[[m]]$confusion %>%
    as.data.frame() %>%
    rownames_to_column()
  
}

importances <- bind_rows(importance_list) %>%
  
  group_by(rowname) %>%
  summarize(mean(MeanDecreaseAccuracy))

confusion_matrix <- bind_rows(confusion_list) %>%
  mutate(n = intermediate+mountain+`sharp finish`+sprint) %>% 
  mutate(perc_int = intermediate / n,
         perc_sharp = `sharp finish` / n, 
         perc_spr = sprint / n, 
         perc_mtn = mountain / n) %>% 
  gather(stat, value, perc_int:perc_mtn) %>%
  group_by(rowname, stat) %>% 
  summarize(rate = sum(n * value, na.rm = T) / sum(n, na.rm = T)) %>% 
  spread(stat, rate)

# Gradient Boosting

library(xgboost)

training <- readr::read_csv("training-classifiers.csv") %>%
  
  inner_join(
    
    km_parts, by = c("race", "stage", "year")
    
  ) %>%
  
  mutate(type = ifelse(type %in% c("cobbles", "cobbled"), "sprint", type),
         type = as.factor(type)) %>%
  mutate(class_type = ifelse(type == "sprint", 0,
                             ifelse(type == "sharp finish", 1,
                                    ifelse(type == "intermediate", 2, 3))))

# roughly 1 minute per run

model_list <- vector("list", 50)
predict_list <- model_list
importance_list <- model_list
confusion_list <- model_list
oob_list <- model_list

for(t in 1:length(model_list)) {
  
  train <- sample(1:nrow(training), nrow(training) / 2)
  
  # create train and test sets // label is the classification type
  
  xgb.train <- xgb.DMatrix(
    
    data = as.matrix(training[train, ] %>%
                       select(summit_finish, act_climb_difficulty, adj_concentration,
                              climbing_final_20km, position_highest, number_cat_climbs, 
                              total_elev_change, final_1km_gradient, rider_cl)),
    label = training[train, ]$class_type)
  
  xgb.test <- xgb.DMatrix(
    
    data = as.matrix(training[-train, ] %>%
                       select(summit_finish, act_climb_difficulty, adj_concentration,
                              climbing_final_20km, position_highest, number_cat_climbs, 
                              total_elev_change, final_1km_gradient, rider_cl)),
    label = training[-train, ]$class_type)
  
  # tune parameters
  
  params <- list(
    
    booster = "gbtree",
    eta = 0.01,
    max_depth = 3,
    gamma = 1,
    subsample = 1,
    colsample_bytree = 0.75,
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    num_class = 4
    
  )
  
  # model
  
  gbm_model <- xgb.train(params = params,
                         data = xgb.train,
                         nrounds = 5000,
                         nthreads = 4,
                         early_stopping_rounds = 10,
                         watchlist = list(val1 = xgb.train,
                                          val2 = xgb.test),
                         verbose = 1)
  
  # predictions
  
  gbm_predict = cbind(
    
    training[-train, ],
    
    pred = predict(gbm_model, as.matrix(training[-train, ] %>%
                                          select(summit_finish, act_climb_difficulty, adj_concentration,
                                                 climbing_final_20km, position_highest, number_cat_climbs, 
                                                 total_elev_change, final_1km_gradient, rider_cl)), reshape=T))  %>% 
    select(race, year, stage, type, sprint = pred.1, sharp = pred.2, inter = pred.3, mtn = pred.4)
  
  #
  
  model_list[[t]] <- gbm_model
  predict_list[[t]] <- gbm_predict %>%
    mutate(iteration = t) %>%
    gather(pred_type, pred, sprint:mtn) %>%
    group_by(race, year, stage) %>% 
    mutate(best = ifelse(max(pred) == pred, "best", "other")) %>%
    ungroup()
  oob_list[[t]] <- tibble(oob = gbm_model$best_score,
                          iteration = t)
  
}

xgb_prds <- bind_rows(predict_list) %>%
  
  group_by(race, stage, year, type, pred_type) %>%
  summarize(avg = mean(pred, na.rm = T), 
            min = min(pred, na.rm = T), 
            max = max(pred, na.rm = T), 
            best = mean(best == "best", na.rm = T), 
            n = n()) %>% 
  ungroup()

bind_rows(oob_list) %>% summarize(mean(oob))

for(m in 1:length(model_list)) {
  
  importance_list[[m]] <- xgb.importance(model = model_list[[m]]) %>%
    as.data.frame() %>%
    rownames_to_column()
  
}

xgb_importances <- bind_rows(importance_list) %>%
  
  group_by(Feature) %>%
  summarize(mean(Gain),
            mean(Cover),
            mean(Frequency))

#
#
#
#
# assign stage types

# xgboost



# decision trees

stage_partition_data <- stage_partition_data %>%
  
  left_join(
    
    apply_tree_model %>%
      select(type, race, year, stage), by = c("race", "stage", "year")
    
  ) %>%
  
  mutate(type = ifelse(is.na(PC1), "time_trial", type)) %>%

  rename(stage_type = type)

# TTs uphill

tt_uphill <- stage_data %>%
  filter(time_trial == TRUE & rnk == 1) %>% 
  mutate(tot_grade = total_vert_gain / (length * 1000)) %>% 
  arrange(-tot_grade) %>%
  
  # -2.73 below is average rel_speed for average TT winner
  mutate(factor_adj = rel_speed / (mean(rel_speed, na.rm = T)))

summary(lm(factor_adj ~ tot_grade, data = tt_uphill))

# performance weighting

# evaluative metrics

# TIME TRIALS is rel_speed
# SPRINT STAGES is pts (log of finish position)
# MOUNTAIN STAGES is rel_speed
# INTERMEDIATE & ONE DAY is rel_qual or maybe pts

# what is the correct weighting of performance time periods?

# long-term ability (last 3 years, weighted for age?)
# season ability (everything since start of the year, 6 months?)
# recent form (performance in last month, last 3 or 5 efforts, in that race for grand tours)

stage_performance_data <- stage_partition_data %>%
  
  select(race, stage, year, stage_type) %>%
  
  inner_join(
    
    stage_data, by = c("race", "stage", "year")
    
  ) %>%
  
  mutate(lrnk = ifelse(rnk > 10, 11, rnk)) %>%
  
  mutate(pts = ((1 / (log10(lrnk + 1))) - (1 / log10(12))) / 2.40) %>%
  
  select(-lrnk) %>%
  
  mutate(adj_rel_speed = ifelse(time_trial == TRUE,
                                rel_speed / (0.61 + (31.6 * (total_vert_gain / (length * 1000)))), NA)) %>%
  
  #mutate(adj_factor = ifelse(stage_type == "mountain",
  #                           (0.0393 * act_climb_difficulty) - 0.0357, NA)) %>%
  
  mutate(performance = ifelse(stage_type == "time_trial", adj_rel_speed * -1,
                              #ifelse(stage_type == "mountain", ((gain_10th * -1) / act_climb_difficulty / adj_factor),
                              ifelse(stage_type == "mountain", ((gain_back_5 * -1) / act_climb_difficulty),
                                     ifelse(stage_type == "intermediate", pts,
                                            ifelse(stage_type == "sharp_finish", pts,
                                                   ifelse(stage_type == "sprint", pts,
                                                          ifelse(stage_type == "cobbles", pts, NA))))))) %>%
  
  mutate(type = stage_type,
         name = rider) %>%
  
  unique()

# cluster finishers

test_perf_data <- stage_performance_data %>%
  filter(type %in% c("mountain", "intermediate")) %>%
  mutate(is_gc = gc_winner == rider,
         is_win = rnk == 1) %>%
  select(rider, rnk, total_seconds, race, stage, year, is_gc, is_win, gain_gc, gain_1st) %>%
  mutate(grp_gc = abs(gain_gc) < 11,
         grp_1st = abs(gain_1st) < 11) %>%
  filter(!((is.na(grp_gc) | (is.na(grp_1st)) | (is.na(total_seconds)))))

lumpy_stages <- test_perf_data %>%
  select(stage, race, year) %>%
  unique()

test_list <- vector("list", length(lumpy_stages$stage))

#

for(s in 1:length(lumpy_stages$stage)) {
  
  d <- test_perf_data %>%
    filter(stage == lumpy_stages$stage[[s]] & year == lumpy_stages$year[[s]] & race == lumpy_stages$race[[s]]) %>%
    mutate(z_time = (mean(total_seconds, na.rm = T) - total_seconds) - sd(total_seconds, na.rm = T))
  
  #cl_list <- vector('list', 10)
  
  #for(c in 1:10) {
    
    gap_stat <- cluster::clusGap(d %>% 
                          select(grp_gc, grp_1st, total_seconds = z_time) %>%
                          mutate(total_seconds = total_seconds + runif(n(),min = -0.01, 0.01)),
                        FUN = kmeans, nstart = 25,
                        K.max = 10, B = 25)

    ideal = cluster::maxSE(f = gap_stat$Tab[, "gap"], SE.f = gap_stat$Tab[, "SE.sim"])
    
    if(ideal < 2) {
      
      ideal = 2
      
    }
    
    xyz <- kmeans(d %>% 
                    select(grp_gc, grp_1st, total_seconds = z_time) %>%
                    mutate(total_seconds = total_seconds + runif(n(),min = -0.01, 0.01)), centers = ideal)
    
    #cl_list[[c]] <- tibble(cl = c,
    #            within = xyz$withinss,
    #             between = xyz$betweenss,
    #             ideal = ideal)

  #}
  
  test_list[[s]] <- cbind(d, cl = xyz$cluster) %>%
    mutate(ideal_cl = ideal) %>%
    mutate(race = lumpy_stages$race[[s]],
           year = lumpy_stages$year[[s]],
           stage = lumpy_stages$stage[[s]])
  
}

how_many_clusters <- bind_rows(test_list)

# analysis

stage_performance_data %>%
  filter(rnk == 1 & type == "mountain") %>%
  
  gather(stat, value, c(gain_3rd:gain_20th, gain_gc, gain_back_5)) %>%
  
  ggplot(aes(x = act_climb_difficulty, y = value, color = stat))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", color = "black")+
  facet_wrap(~stat, scales = "free_y")+
  labs(x = "actual climb difficulty", 
       y = "seconds gained / KM by stage winner", 
       title = "Impact of KOM difficulty on separation of peloton")+
  theme(strip.text = element_text(size = 16),
        axis.text = element_text(size = 16))+
  scale_color_manual(guide = F, values = c("dark red", "navy", "#37B36C", "orange", "black", "light blue"))

#

stage_performance_data %>%
  filter(rnk == 1 & type == "mountain") %>%
  
  group_by(race, year) %>%
  mutate(last = percent_rank(stage)) %>%
  ungroup() %>%
  
  gather(stat, value, c(gain_3rd:gain_20th, gain_gc, gain_back_5)) %>%
  
  group_by(stat) %>%
  mutate(rel_value = value / median(value, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(stat) %>%
  do(broom::tidy(lm(rel_value ~  act_climb_difficulty, data = .))) %>%
  ungroup()

#

inter_step <- stage_performance_data %>% 

  mutate(performance = ifelse(stage_type %in% c("sprint", "sharp_finish", "intermediate", "cobbles") & performance == 0,
                              performance + runif(n(), min = -0.00001, max = 0.00001), performance)) %>%
  
  group_by(stage_type, rider) %>%
  summarize(average = median(performance, na.rm = T), 
            n = n()) %>%
  ungroup() %>%
  group_by(rider) %>% 
  mutate(n = sum(n)) %>% 
  ungroup() %>% 
  filter(n > 39) %>%
  group_by(stage_type) %>% 
  mutate(rk = rank(-average, ties.method = "min")) %>%
  ungroup() %>% 
  filter(rk < 501)

#

performance_data <- stage_performance_data %>%
  
  mutate(performance = ifelse(stage_type %in% c("sprint", "sharp_finish", "intermediate", "cobbles") & performance == 0,
                              performance + runif(n(), min = -0.00001, max = 0.00001), performance)) %>%
  
  inner_join(
    
    inter_step %>%
      select(rider, stage_type), by = c("stage_type", "rider")
    
  ) %>%
  
  unique() %>%
  
  group_by(name, type) %>%
  
  nest()

# what is required to win a Grand Tour?

grand_tour_winner_performance <- tibble(
  
  rider = c("Froome Chris", "Nibali Vincenzo", "Froome Chris",
            "Froome Chris", "Froome Chris", "Thomas Geraint"),
  year = c(2013, 2014, 2015, 2016, 2017, 2018),
  race = c("tour de france")) %>%
  
  rbind(
    
    tibble(
      
      rider = c("Nibali Vincenzo", "Quintana Nairo", "Contador Alberto",
                "Nibali Vincenzo", "Dumoulin Tom", "Froome Chris",
                "Carapaz Richard"),
      year = c(2013, 2014, 2015, 2016, 2017, 2018, 2019),
      race = c("giro d'italia")
    ),
    
    tibble(
      
      rider = c("Horner Chris", "Contador Alberto", "Aru Fabio",
                "Quintana Nairo", "Froome Chris", "Yates Simon"),
      year = c(2013, 2014, 2015, 2016, 2017, 2018),
      race = c("vuelta a espana")
    )) %>%
  
  inner_join(
    
    performance_data %>%
      unnest(), by = c("rider", "year", "race")
    
  ) %>%
  
  group_by(stage_type) %>%
  summarize(median = median(performance, na.rm = T),
            mean = mean(performance, na.rm = T),
            n = n()) %>%
  ungroup()

# all race dates

unique_GT_stages <- stage_performance_data %>%
  
  filter(rnk == 1) %>%
  
  mutate(mtn = ifelse(stage_type == "mountain", 1, 0)) %>%
  
  group_by(race, year) %>%
  
  filter(grand_tour == TRUE | 
           (sum(mtn) > 1 & race %in% c("tour de suisse", "criterium du dauphine", "amgen tour of california",
                                       "paris - nice","volta ciclista a catalunya", "tirreno-adriatico"))) %>%
  
  ungroup() %>%
  
  group_by(race, year) %>%
  mutate(rd = rank(stage, ties.method = "first")) %>%
  ungroup() %>%
  
  mutate(rd = ifelse(rd == 1, date, NA)) %>%
  
  group_by(race, year) %>%
  mutate(date = min(date, na.rm = T)) %>%
  ungroup() %>%
  
  select(-rd) %>%
  
  filter(year > 2014) %>%
  
  select(race, year, date) %>%
  unique() %>%
  
  mutate(week_id = ((lubridate::year(date) - 2000) * 52) + lubridate::week(date))

# this truncates performance to ignore performances outside +5/-5
# it applies crude strength of peloton adjustments too

performance_data <- stage_performance_data %>%
  
  mutate(performance = ifelse(stage_type %in% c("sprint", "sharp_finish", "intermediate", "cobbles") & performance == 0,
                              performance + runif(n(), min = -0.00001, max = 0.00001), performance)) %>%
  
  inner_join(
    
    inter_step %>%
      select(rider, stage_type), by = c("stage_type", "rider")
    
  ) %>%
  
  unique() %>%
  
  mutate(performance_raw = performance) %>%
  
  mutate(performance = ifelse(stage_type == "mountain",
                              ifelse(performance < -10, -10,
                                     ifelse(performance > 5, 5, performance)), performance)) %>%
  
  mutate(performance = ifelse(stage_type == "mountain",
                              ifelse(race %in% c("vuelta a espana", "giro d'italia",
                                                 "liege - bastogne liege", "milano - sanremo",
                                                 "ronde van vlaanderen / tour des flandres", 
                                                 "ronde van vlaanderen - tour des flandres",
                                                 "il lombardia", "paris - roubaix"), performance + 0.25,
                                     ifelse(race %in% c("tour de france"), performance + 0.75,
                                            ifelse(race %in% c("criterium du dauphine",
                                                               "tour de suisse",
                                                               "world championships",
                                                               "amstel gold race",
                                                               "la fleche wallonne",
                                                               "clasica ciclista san sebastian",
                                                               "strade bianche",
                                                               "gent-wevelgem in flanders fields"), performance, performance - 0.25))), performance)) %>%
  
  mutate(importance = ifelse(race %in% c("vuelta a espana", "giro d'italia",
                                         "liege - bastogne liege", "milano - sanremo",
                                         "ronde van vlaanderen / tour des flandres", 
                                         "ronde van vlaanderen - tour des flandres",
                                         "il lombardia", "paris - roubaix"), 1,
                                     ifelse(race %in% c("tour de france"), 1.5,
                                            ifelse(race %in% c("criterium du dauphine",
                                                               "tour de suisse",
                                                               "world championships",
                                                               "amstel gold race",
                                                               "la fleche wallonne",
                                                               "clasica ciclista san sebastian",
                                                               "strade bianche",
                                                               "gent-wevelgem in flanders fields"), 0.75, 0.5)))) %>%
  
  mutate(week_id = ((lubridate::year(date) - 2000) * 52) + lubridate::week(date)) %>%
  
  group_by(name, type) %>%
  
  nest()
  
tictoc::tic()

# long-term performance

long_term_perf_fun <- function(x) {
  
  p_list <- vector("list", length(unique_GT_stages$DATE))
  
  for(d in 1:length(unique_GT_stages$DATE)) {
    
    DATE <- unique_GT_stages$DATE[[d]]
    WK <- unique_GT_stages$week_id[[d]]
    
    df <- x %>%
      filter(date <= DATE & date >= (DATE - 1100)) %>%
      mutate(date = DATE) %>%
      mutate(top_75 = ifelse(percent_rank(performance) > 0.249, performance, NA),
             weight = (1 / abs(0.5 - percent_rank(performance))),
             weight = ifelse(percent_rank(performance) == 0.5, 10, weight),
             weighted = performance * weight,
             weighted_importance = weight * importance * performance,
             importance_perf = importance * performance,
             wt5 = 1 / (WK - week_id + 5),
             wt10 = 1 / (WK - week_id + 10),
             wt20 = 1 / (WK - week_id + 20),
             wt50 = 1 / (WK - week_id + 50)
             ) %>%
      group_by(rider, stage_type, date) %>%
      summarize(mean = mean(performance, na.rm = T),
                raw = mean(performance_raw, na.rm = T),
                median = median(performance, na.rm = T),
                top_75 = mean(top_75, na.rm = T),
                weighted = sum(weighted, na.rm = T) / sum(weight, na.rm = T),
                wt_imp = sum(weighted_importance, na.rm = T) / sum(weight * importance, na.rm = T),
                importance = sum(importance_perf, na.rm = T) / sum(importance, na.rm = T),
                wt5 = sum(wt5 * performance, na.rm = T) / sum(wt5, na.rm = T),
                wt10 = sum(wt10 * performance, na.rm = T) / sum(wt10, na.rm = T),
                wt20 = sum(wt20 * performance, na.rm = T) / sum(wt20, na.rm = T),
                wt50 = sum(wt50 * performance, na.rm = T) / sum(wt50, na.rm = T),
                stages = n()) %>%
      ungroup()
    
    p_list[[d]] <- df
    
  }
  
  results <- bind_rows(p_list)

}

lt_performance <- purrr::map_df(.x = performance_data$data, .f = long_term_perf_fun)

# short term perf

short_term_perf_fun <- function(x, DATE) {
  
  p_list <- vector("list", length(unique_GT_stages$DATE))
  
  for(d in 1:length(unique_GT_stages$DATE)) {
    
    DATE <- unique_GT_stages$DATE[[d]]
    WK <- unique_GT_stages$week_id[[d]]
    
    df <- x %>%
      filter(date <= DATE & date >= (DATE - 375)) %>%
      mutate(date = DATE) %>%
      mutate(top_75 = ifelse(percent_rank(performance) > 0.249, performance, NA),
             weight = (1 / abs(0.5 - percent_rank(performance))),
             weight = ifelse(percent_rank(performance) == 0.5, 10, weight),
             weighted = performance * weight,
             weighted_importance = weight * importance * performance,
             importance_perf = importance * performance,
             wt5 = 1 / (WK - week_id + 5),
             wt10 = 1 / (WK - week_id + 10),
             wt20 = 1 / (WK - week_id + 20),
             wt50 = 1 / (WK - week_id + 50)
      ) %>%
      group_by(rider, stage_type, date) %>%
      summarize(mean = mean(performance, na.rm = T),
                raw = mean(performance_raw, na.rm = T),
                median = median(performance, na.rm = T),
                top_75 = mean(top_75, na.rm = T),
                weighted = sum(weighted, na.rm = T) / sum(weight, na.rm = T),
                wt_imp = sum(weighted_importance, na.rm = T) / sum(weight * importance, na.rm = T),
                importance = sum(importance_perf, na.rm = T) / sum(importance, na.rm = T),
                wt5 = sum(wt5 * performance, na.rm = T) / sum(wt5, na.rm = T),
                wt10 = sum(wt10 * performance, na.rm = T) / sum(wt10, na.rm = T),
                wt20 = sum(wt20 * performance, na.rm = T) / sum(wt20, na.rm = T),
                wt50 = sum(wt50 * performance, na.rm = T) / sum(wt50, na.rm = T),
                stages = n()) %>%
      ungroup() %>%
      mutate(top_75 = ifelse(is.na(top_75) | top_75 == "NaN", mean, top_75))
    
    
    p_list[[d]] <- df
    
  }
  
  results <- bind_rows(p_list)
  
}

st_performance <- purrr::map_df(.x = performance_data$data, .f = short_term_perf_fun)

tictoc::toc()

# short term perf

recent_perf_fun <- function(x, DATE) {
  
  p_list <- vector("list", length(unique_GT_stages$DATE))
  
  for(d in 1:length(unique_GT_stages$DATE)) {
    
    DATE <- unique_GT_stages$DATE[[d]]
    WK <- unique_GT_stages$week_id[[d]]
    
    df <- x %>%
      filter(date <= DATE & date >= (DATE - 91)) %>%
      mutate(date = DATE) %>%
      mutate(top_75 = ifelse(percent_rank(performance) > 0.249, performance, NA),
             weight = (1 / abs(0.5 - percent_rank(performance))),
             weight = ifelse(percent_rank(performance) == 0.5, 10, weight),
             weighted = performance * weight,
             weighted_importance = weight * importance * performance,
             importance_perf = importance * performance,
             wt5 = 1 / (WK - week_id + 5),
             wt10 = 1 / (WK - week_id + 10),
             wt20 = 1 / (WK - week_id + 20),
             wt50 = 1 / (WK - week_id + 50)
      ) %>%
      group_by(rider, stage_type, date) %>%
      summarize(mean = mean(performance, na.rm = T),
                raw = mean(performance_raw, na.rm = T),
                median = median(performance, na.rm = T),
                top_75 = mean(top_75, na.rm = T),
                weighted = sum(weighted, na.rm = T) / sum(weight, na.rm = T),
                wt_imp = sum(weighted_importance, na.rm = T) / sum(weight * importance, na.rm = T),
                importance = sum(importance_perf, na.rm = T) / sum(importance, na.rm = T),
                wt5 = sum(wt5 * performance, na.rm = T) / sum(wt5, na.rm = T),
                wt10 = sum(wt10 * performance, na.rm = T) / sum(wt10, na.rm = T),
                wt20 = sum(wt20 * performance, na.rm = T) / sum(wt20, na.rm = T),
                wt50 = sum(wt50 * performance, na.rm = T) / sum(wt50, na.rm = T),
                stages = n()) %>%
      ungroup() %>%
      mutate(top_75 = ifelse(is.na(top_75) | top_75 == "NaN", mean, top_75))
    
    p_list[[d]] <- df
    
  }
  
  results <- bind_rows(p_list)
  
}

rec_performance <- purrr::map_df(.x = performance_data$data, .f = recent_perf_fun)

#
#
#
#
#
#
#
#

eval_both <- stage_performance_data %>%
  
  select(race, year, date, stage, rnk, stage_type, rider) %>%
  
  group_by(race, year) %>%
  mutate(rd = rank(stage, ties.method = "first")) %>%
  ungroup() %>%
  
  mutate(rd = ifelse(rd == 1, date, NA)) %>%
  
  group_by(race, year) %>%
  mutate(date = min(date, na.rm = T)) %>%
  ungroup() %>%
  
  select(-rd) %>%
  
  mutate(date = date + 1) %>%
  
  inner_join(lt_performance %>%
               gather(stat, value, mean:wt50) %>%
               select(rider, stage_type, date, stat,
                      lt_stages = stages, lt_value = value), by = c("rider", "stage_type", "date")) %>%
  
  left_join(st_performance %>%
               gather(stat, value, mean:wt50) %>%
               select(rider, stage_type, date, stat,
                      st_stages = stages, st_value = value), by = c("rider", "stage_type", "date", "stat")) %>%
  
  mutate(st_value = ifelse(st_stages < 5, (((5 - st_stages) * lt_value) + (st_value * st_stages)) / 5, st_value)) %>%
  mutate(st_value = ifelse(is.na(st_stages), lt_value - 1, st_value)) %>%
  
  filter(lt_stages > 4) %>%
  
  group_by(race, stage, year, stat) %>%
  mutate(lt_value = lt_value - mean(lt_value, na.rm = T),
         st_value = st_value - mean(st_value, na.rm = T),
         ) %>%
  ungroup() %>%
  
  mutate(rnk = ifelse(is.na(rnk), 200, rnk)) %>%
  
  mutate(win = ifelse(rnk == 1, 1, 0), 
         t3 = ifelse(rnk < 4, 1, 0)) %>%
  
  group_by(stage, race, year) %>%
  filter(min(rnk, na.rm = T) == 1) %>%
  ungroup() %>%
  
  unique()

#

coefs <- eval_both %>%
  
  filter(stage_type %in% c("sprint", "time_trial", "mountain", "intermediate")) %>%
  
  group_by(stage_type, stat) %>% 
  do(broom::tidy(glm(win ~ lt_value, data = ., family = "binomial")))

#

coefs_st <- eval_both %>%
  
  filter(stat != "top_75") %>%
  
  filter(stage_type %in% c("sprint", "time_trial", "mountain", "intermediate")) %>%
  
  group_by(stage_type, stat) %>% 
  do(broom::tidy(glm(win ~ st_value, data = ., family = "binomial")))

#

coefs_both <- eval_both %>%
  
  filter(stat != "top_75") %>%
  
  filter(stage_type %in% c("sprint", "time_trial", "mountain", "intermediate")) %>%
  
  group_by(stage_type, stat) %>% 
  do(broom::tidy(glm(win ~ st_value + lt_value, data = ., family = "binomial")))

# eval of MTN model

m <- eval_both %>% 
  filter(stage_type == "mountain") %>% 
  
  group_by(stage, race, year) %>%
  filter(min(rnk, na.rm = T) == 1) %>%
  ungroup() %>%

  inner_join(
    
    coefs %>%
      select(stage_type, stat, term, estimate) %>%
      spread(term, estimate) %>%
      janitor::clean_names() %>%
      rename(estimate = lt_value), by = c("stage_type", "stat")
    
  ) %>%

  mutate(coef = intercept + (estimate * lt_value), 
         probs = (exp(coef) / (1 + exp(coef)))) %>%
  mutate(win = ifelse(is.na(win), 0, win)) %>%
  
  group_by(stage, race, year, stat) %>%
  mutate(probs = ifelse(is.na(probs) | probs == "NaN", min(probs, na.rm = T), probs)) %>%
  mutate(min = min(probs, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(probs = probs - min) %>%
  
  group_by(stage, race, year, stat) %>%
  mutate(probs = probs / sum(probs)) %>%
  ungroup()

# eval of INT model

m <- eval_both %>% 
  filter(stage_type == "intermediate") %>% 
  inner_join(
    
    coefs %>%
      select(stage_type, stat, term, estimate) %>%
      spread(term, estimate) %>%
      janitor::clean_names() %>%
      rename(estimate = lt_value), by = c("stage_type", "stat")
    
  ) %>%
  
  mutate(coef = intercept + (estimate * lt_value), 
         probs = (exp(coef) / (1 + exp(coef)))) %>%
  mutate(win = ifelse(is.na(win), 0, win)) %>%
  
  group_by(stage, race, year, stat) %>%
  mutate(probs = ifelse(is.na(probs) | probs == "NaN", min(probs, na.rm = T), probs)) %>%
  mutate(min = min(probs, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(probs = probs - min) %>%
  
  group_by(stage, race, year, stat) %>%
  mutate(probs = probs / sum(probs)) %>%
  ungroup()

# eval of SPR model

m <- eval_both %>% 
  filter(stage_type == "sprint") %>% 
  inner_join(
    
    coefs %>%
      select(stage_type, stat, term, estimate) %>%
      spread(term, estimate) %>%
      janitor::clean_names() %>%
      rename(estimate = lt_value), by = c("stage_type", "stat")
    
  ) %>%
  
  mutate(coef = intercept + (estimate * lt_value), 
         probs = (exp(coef) / (1 + exp(coef)))) %>%
  mutate(win = ifelse(is.na(win), 0, win)) %>%
  
  group_by(stage, race, year, stat) %>%
  mutate(probs = ifelse(is.na(probs) | probs == "NaN", min(probs, na.rm = T), probs)) %>%
  mutate(min = min(probs, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(probs = probs - min) %>%
  
  group_by(stage, race, year, stat) %>%
  mutate(probs = probs / sum(probs)) %>%
  ungroup()

# eval of TT model

m <- eval_both %>% 
  filter(stage_type == "time_trial") %>% 
  inner_join(
    
    coefs %>%
      select(stage_type, stat, term, estimate) %>%
      spread(term, estimate) %>%
      janitor::clean_names() %>%
      rename(estimate = lt_value), by = c("stage_type", "stat")
    
  ) %>%
  
  mutate(coef = intercept + (estimate * lt_value), 
         probs = (exp(coef) / (1 + exp(coef)))) %>%
  mutate(win = ifelse(is.na(win), 0, win)) %>%
  
  group_by(stage, race, year, stat) %>%
  mutate(probs = ifelse(is.na(probs) | probs == "NaN", min(probs, na.rm = T), probs)) %>%
  mutate(min = min(probs, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(probs = probs - min) %>%
  
  group_by(stage, race, year, stat) %>%
  mutate(probs = probs / sum(probs)) %>%
  ungroup()

# set up ROC / AUC evaluation

s_list <- c("median", "mean", "raw", "top_75", "wt_imp", "weighted", "importance", "wt5", "wt10", "wt20", "wt50")

for(x in 1:11) {
  
  labels <- m %>% filter(stat == s_list[[x]]) %>% select(win) %>% as.list() %>% .[[1]]
  scores <- m %>% filter(stat == s_list[[x]]) %>% select(probs) %>% as.list() %>% .[[1]]
  
  roc_obj <- pROC::roc(labels, scores)
  print(s_list[[x]])
  print(pROC::auc(roc_obj))
  print(paste0("Brier score = ", mean((labels - scores)^2)))
  
  print(pROC::ggroc(roc_obj))
  
}

#
# eval GC races
#

eval_GC_MTN <- stage_performance_data %>%

  select(race, year, date, stage, gain_gc, stage_type, rider) %>%
  
  group_by(race, year) %>%
  mutate(rd = rank(stage, ties.method = "first")) %>%
  ungroup() %>%
  
  mutate(rd = ifelse(rd == 1, date, NA)) %>%
  
  group_by(race, year) %>%
  mutate(date = min(date, na.rm = T)) %>%
  ungroup() %>%
  
  select(-rd) %>%
  
  mutate(date = date + 1) %>%
  
  filter(stage_type == "mountain") %>%
  
  group_by(stage_type, rider, year, race, date) %>%
  summarize(total_gain_gc = sum(gain_gc, na.rm = T),
            stages = n()) %>%
  ungroup() %>%
  
  inner_join(lt_performance %>%
               gather(stat, value, mean:wt50) %>%
               select(rider, stage_type, date, stat,
                      lt_stages = stages, lt_value = value), by = c("rider", "stage_type", "date")) %>%

  filter(lt_stages > 4) %>%
  
  group_by(race, year, stat) %>%
  mutate(lt_value = lt_value - mean(lt_value, na.rm = T),
         rk = rank(total_gain_gc, ties.method = "min")
  ) %>%
  ungroup() %>%

  mutate(best = rk == 1,
         top_3 = rk < 4) %>%
  
  unique()

#

GC_race_coefs <- eval_GC_MTN %>%
  
  group_by(stat) %>% 
  do(broom::tidy(glm(best ~ lt_value, data = ., family = "binomial")))

GC_race_coefs <- eval_GC_MTN %>%
  
  mutate(avg_gain_gc = total_gain_gc / stages) %>%
  
  group_by(stat) %>% 
  do(broom::tidy(lm(avg_gain_gc ~ lt_value + race, data = .)))

#

m <- eval_GC_MTN %>% 
  
  filter(lt_stages > 4) %>%

  inner_join(
    
    GC_race_coefs %>%
      select(stat, term, estimate) %>%
      spread(term, estimate) %>%
      janitor::clean_names() %>%
      rename(estimate = lt_value), by = c("stat")
    
  ) %>%
  
  mutate(coef = intercept + (estimate * lt_value), 
         probs = (exp(coef) / (1 + exp(coef)))) %>%
  
  mutate(best = ifelse(is.na(best), 0, best)) %>%
  
  group_by(race, year, stat) %>%
  mutate(probs = ifelse(is.na(probs) | probs == "NaN", min(probs, na.rm = T), probs)) %>%
  mutate(min = min(probs, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(probs = probs - min) %>%
  
  group_by(race, year, stat) %>%
  mutate(probs = probs / sum(probs)) %>%
  ungroup()

#

s_list <- c("median", "mean", "raw", "top_75", "wt_imp", "weighted", "importance", "wt5", "wt10", "wt20", "wt50")

for(x in 1:11) {
  
  labels <- m %>% filter(stat == s_list[[x]]) %>% select(best) %>% as.list() %>% .[[1]]
  scores <- m %>% filter(stat == s_list[[x]]) %>% select(probs) %>% as.list() %>% .[[1]]
  
  roc_obj <- pROC::roc(labels, scores)
  print(s_list[[x]])
  print(pROC::auc(roc_obj))
  print(paste0("Brier score = ", mean((labels - scores)^2)))
  
  print(pROC::ggroc(roc_obj))
  
}

#
# TT
#

eval_GC_TT <- stage_performance_data %>%
  
  select(race, year, date, stage, gain_10th, stage_type, rider) %>%
  
  group_by(race, year) %>%
  mutate(rd = rank(stage, ties.method = "first")) %>%
  ungroup() %>%
  
  mutate(rd = ifelse(rd == 1, date, NA)) %>%
  
  group_by(race, year) %>%
  mutate(date = min(date, na.rm = T)) %>%
  ungroup() %>%
  
  select(-rd) %>%
  
  mutate(date = date + 1) %>%
  
  filter(stage_type == "time_trial") %>%
  
  group_by(stage_type, rider, year, race, date) %>%
  summarize(total_gain_tt = sum(gain_10th, na.rm = T),
            stages = n()) %>%
  ungroup() %>%
  
  inner_join(lt_performance %>%
               gather(stat, value, mean:importance) %>%
               select(rider, stage_type, date, stat,
                      lt_stages = stages, lt_value = value), by = c("rider", "stage_type", "date")) %>%
  
  group_by(race, year, stat) %>%
  mutate(lt_value = lt_value - mean(lt_value, na.rm = T),
         rk = rank(total_gain_tt, ties.method = "min")
  ) %>%
  ungroup() %>%
  
  mutate(best = rk == 1,
         top_3 = rk < 4) %>%
  
  unique()

#

TT_race_coefs <- eval_GC_TT %>%
  
  group_by(stat) %>% 
  do(broom::tidy(glm(best ~ lt_value, data = ., family = "binomial")))

TT_race_coefs <- eval_GC_TT %>%
  
  mutate(avg_gain_tt = total_gain_tt / stages) %>%
  
  group_by(stat) %>% 
  do(broom::tidy(lm(avg_gain_tt ~ lt_value, data = .)))

#

m <- eval_GC_MTN %>% 
  
  inner_join(
    
    GC_race_coefs %>%
      select(stat, term, estimate) %>%
      spread(term, estimate) %>%
      janitor::clean_names() %>%
      rename(estimate = lt_value), by = c("stat")
    
  ) %>%
  
  mutate(coef = intercept + (estimate * lt_value), 
         probs = (exp(coef) / (1 + exp(coef)))) %>%
  
  mutate(best = ifelse(is.na(best), 0, best)) %>%
  
  group_by(race, year, stat) %>%
  mutate(min = min(probs, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(probs = probs - min) %>%
  
  group_by(race, year, stat) %>%
  mutate(probs = probs / sum(probs)) %>%
  ungroup()

#

s_list <- c("median", "mean", "raw", "top_75", "wt_imp", "weighted", "importance")

for(x in 1:7) {
  
  labels <- m %>% filter(stat == s_list[[x]]) %>% select(best) %>% as.list() %>% .[[1]]
  scores <- m %>% filter(stat == s_list[[x]]) %>% select(probs) %>% as.list() %>% .[[1]]
  
  roc_obj <- pROC::roc(labels, scores)
  print(s_list[[x]])
  print(pROC::auc(roc_obj))
  
  print(pROC::ggroc(roc_obj))
  
}


#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

# Grand Tour time gaps

grand_tour_time_gaps <- tibble(
  
  rider = c("Froome Chris", "Nibali Vincenzo", "Froome Chris",
            "Froome Chris", "Froome Chris", "Thomas Geraint"),
  year = c(2013, 2014, 2015, 2016, 2017, 2018),
  race = c("tour de france")) %>%
  
  rbind(
    
    tibble(
      
      rider = c("Nibali Vincenzo", "Quintana Nairo", "Contador Alberto",
                "Nibali Vincenzo", "Dumoulin Tom", "Froome Chris",
                "Carapaz Richard"),
      year = c(2013, 2014, 2015, 2016, 2017, 2018, 2019),
      race = c("giro d'italia")
    ),
    
    tibble(
      
      rider = c("Horner Chris", "Contador Alberto", "Aru Fabio",
                "Quintana Nairo", "Froome Chris", "Yates Simon"),
      year = c(2013, 2014, 2015, 2016, 2017, 2018),
      race = c("vuelta a espana")
    )) %>%
  
  inner_join(
    
    stage_performance_data %>%
      select(rider, total_seconds, stage, race, time_trial, year,
             performance, stage_type, gain_gc), by = c("rider", "year", "race")
    
  ) %>%
  
  mutate(TT = ifelse(time_trial == TRUE, total_seconds, NA),
         MTN = ifelse(climb_difficulty > 24.99, total_seconds, NA),
         TT_rel = ifelse(time_trial == TRUE, performance, NA),
         MTN_rel = ifelse(climb_difficulty > 24.99, performance, NA)) %>%
  
  group_by(race, year, rider, stage) %>%
  summarize(total_time = sum(total_seconds, na.rm = T),
            total_rel = mean(rel_speed, na.rm = T),
            TT_time = sum(TT, na.rm = T),
            MTN_time = sum(MTN, na.rm = T),
            TT_rel = mean(TT_rel, na.rm = T),
            MTN_rel = mean(MTN_rel, na.rm = T),
            stages = n()) %>%
  ungroup()

#

grand_tour_vs_winner <- stage_performance_data %>%
  
  filter(grand_tour == TRUE) %>%
  
  mutate(TT = ifelse(time_trial == TRUE, gain_gc, NA),
         MTN = ifelse(stage_type == "mountain", gain_gc, NA),
         TT_rel = ifelse(time_trial == TRUE, rel_speed, NA),
         MTN_rel = ifelse(stage_type == "mountain", performance, NA),
         OTH = ifelse(stage_type %in% c("mountain", "time_trial"), NA, gain_gc),
         OTH_pts = ifelse(stage_type %in% c("mountain", "time_trial"), NA, pts)) %>%
  
  mutate(won = gc_winner == rider) %>%
  
  group_by(race, year, rider, won) %>%
  summarize(total_time = sum(gain_gc, na.rm = T),
            OTH = sum(OTH, na.rm = T),
            OTH_pts = mean(OTH_pts, na.rm = T),
            TT_time = sum(TT, na.rm = T),
            MTN_time = sum(MTN, na.rm = T),
            TT_rel = median(TT_rel, na.rm = T),
            MTN_rel = median(MTN_rel, na.rm = T),
            stages = n()) %>%
  ungroup() %>%
  
  mutate(req = ifelse(won == TRUE, stages, NA)) %>%
  
  group_by(race, year) %>%
  mutate(req = mean(req, na.rm = T)) %>%
  ungroup() %>%
  
  filter(stages == req) %>%
  
  gather(stat, value, total_time:MTN_rel) %>%
  
  group_by(race, year, stat) %>%
  mutate(rk = ifelse(stat %in% c("MTN_rel", "OTH_pts"), rank(-value, ties.method = "min"), rank(value, ties.method = "min"))) %>%
  ungroup()

#

runners_up_GT <- tibble(
  
  rider = c("Quintana Nairo", "Peraud Jean-Christophe", "Quintana Nairo",
            "Bardet Romain", "Uran Rigoberto", "Dumoulin Tom"),
  year = c(2013, 2014, 2015, 2016, 2017, 2018),
  race = c("tour de france")) %>%
  
  rbind(
    
    tibble(
      
      rider = c("Uran Rigoberto", "Uran Rigoberto", "Aru Fabio",
                "Chaves Esteban", "Quintana Nairo", "Dumoulin Tom",
                "Nibali Vincenzo"),
      year = c(2013, 2014, 2015, 2016, 2017, 2018, 2019),
      race = c("giro d'italia")
    ),
    
    tibble(
      
      rider = c("Nibali Vincenzo", "Froome Chris", "Rodriguez Joaquim",
                "Froome Chris", "Nibali Vincenzo", "Mas Enric"),
      year = c(2013, 2014, 2015, 2016, 2017, 2018),
      race = c("vuelta a espana")
    )) %>%
  
  inner_join(
    
    grand_tour_vs_winner, by = c("rider", "year", "race"))

#

gaps_by_week <- stage_performance_data %>%
  
  filter(grand_tour == TRUE) %>%
  
  mutate(TT_time = ifelse(time_trial == TRUE, gain_gc, NA),
         MTN_time = ifelse(stage_type == "mountain", gain_gc, NA),
         TT_rel = ifelse(time_trial == TRUE, rel_speed, NA),
         total_time = gain_gc,
         MTN_rel = ifelse(stage_type == "mountain", performance, NA),
         OTH_time = ifelse(stage_type %in% c("mountain", "time_trial"), NA, gain_gc),
         OTH_pts = ifelse(stage_type %in% c("mountain", "time_trial"), NA, pts)) %>%
  
  mutate(won = gc_winner == rider) %>%
  
  mutate(wk = floor((stage-1) / 7)) %>%
  
  group_by(race, year, rider) %>%
  mutate(total_time = sum(total_time, na.rm = T)) %>%
  ungroup() %>%
  
  filter(total_time < 601) %>%
  
  gather(stat, value, TT_time:OTH_pts) %>%
  
  group_by(wk, stat, race, year, rider) %>%
  summarize(difference = sum(value, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(wk, stat, race, year) %>%
  summarize(difference = mean(difference, na.rm = T)) %>%
  ungroup() %>%
  
  filter(!difference == "NaN") %>%
  mutate(race = ifelse(str_detect(race, "giro"), "Italy", 
                       ifelse(str_detect(race, "france"), "TDF", "Vuelta"))) %>%
  group_by(race, stat, wk) %>%
  summarize(difference = mean(difference, na.rm = T))

#
#
#
#
#
#
#

# 2019 TDF

tdf_2019 <- tibble(stage = seq(1,21,1),
                   race = "tour de france",
                   year = 2019) %>%

  left_join(
    
    f_r_data %>%
      select(-length, -slug) %>%
      mutate(race = tolower(race)) %>%
      mutate(race = ifelse(race == "la route d'occitanie" & year == 2019,
                           "la route d'occitanie - la depeche du midi", race)), by = c("race", "stage", "year")
    
  ) %>%
  
  left_join(
    
    f_r_climbs %>%
      
      mutate(race = tolower(race)) %>%
      mutate(race = ifelse(race == "la vuelta a espana", "vuelta a espana", race),
             race = ifelse(race == "la route d'occitane", "la route d'occitanie", race)) %>%
      
      left_join(f_r_data %>%
                  select(race, stage, year, stage_length = length) %>%
                  mutate(race = tolower(race)) %>%
                  mutate(race = ifelse(race == "la route d'occitanie" & year == 2019,
                                       "la route d'occitanie - la depeche du midi", race)), by = c("race", "stage", "year")) %>%
      
      mutate(summit_finish = ifelse(abs(end_distance - stage_length) < 2, TRUE, FALSE)) %>%
      mutate(summit_finish = ifelse(race == "tour de romandie" & stage == 4 &
                                      year == 2018 & climb_name == "Torgon", TRUE, summit_finish)) %>%
      
      mutate(kom_points = ifelse(summit_finish == TRUE, model_category * 2, model_category)) %>%
      
      group_by(race, stage, year) %>%
      summarize(cat_climb_length = sum(end_distance - start_distance, na.rm = T),
                concentration = max(kom_points, na.rm = T),
                number_cat_climbs = sum(kom_points >= 1, na.rm = T),
                act_climb_difficulty = sum(kom_points, na.rm = T)) %>%
      ungroup(), by = c("race", "stage", "year")
    
  ) %>%
  
  mutate(cat_climb_length = ifelse(is.na(cat_climb_length),
                                   ifelse(is.na(total_elev_change), NA, 0), cat_climb_length),
         number_cat_climbs = ifelse(is.na(number_cat_climbs), 0, number_cat_climbs),
         concentration = ifelse(is.na(concentration),
                                ifelse(is.na(total_elev_change), NA, 1),
                                ifelse(is.na(concentration), 1, concentration)),
         act_climb_difficulty = ifelse(is.na(act_climb_difficulty),
                                       ifelse(is.na(total_elev_change), NA, 0), act_climb_difficulty)) %>%
  
  mutate(type = ifelse(concentration > 8,
                       ifelse(act_climb_difficulty > 22, "mountain", 
                              ifelse(concentration > 12, "mountain", "intermediate")),
                       ifelse(number_cat_climbs > 2.5 & concentration > 4,
                              ifelse(final_1km_gradient > 0.0275, "sharp_finish", "intermediate"),
                              ifelse(final_1km_gradient > 0.0275, "sharp_finish", "sprint"))),
         type = ifelse(race %in% c("amstel gold race", "e3 harelbeke", "e3 binckbank classic", 
                                   "record bank e3 harelbeke", "omloop het nieuwsblad", "gent-wevelgem in flanders fields",
                                   "omloop het nieuwsblad elite", "dwars door vlaanderen / a travers la flandre", 
                                   "dwars door vlaanderen - a travers la flandre", "milano - sanremo", 
                                   "liege - bastogne - liege", "il lombardia",
                                   "clasica ciclista san sebastian"), "intermediate", type),
         type = ifelse(race %in% c("paris-roubaix", "paris - roubaix") | 
                         (race == "tour de france" & 
                            ((year == 2014 & stage == 5) | 
                               (year == 2015 & stage == 4) | 
                               (year == 2018 & stage == 9))), "cobbles", type),
         type = ifelse(race %in% c("la fleche wallone", "strade bianche"), "sharp_finish", type),
         type = ifelse(race %in% c("dubai tour") & year %in% c(2017, 2018) & stage == 4, "sharp_finish", type),
         type = ifelse(race == "vuelta a espana" & year == 2015 & stage == 4, "sharp_finish", type),
         type = ifelse(race == 'tour of utah' & year == 2017 & stage == 7, "intermediate", type),
         type = ifelse(race == "tour de france" & year == 2016 & stage == 10, "intermediate", type))

#
#
#
#
#
#

# predicting climbing based on model information

gt_climbing_eval <- stage_performance_data %>% 
  filter(grand_tour & stage_type == "mountain") %>%
  
  group_by(race, year, rider) %>% 
  summarize(to_gc = sum(gain_gc, na.rm = T),
            time = sum(total_seconds, na.rm = T),
            n = n()) %>% 
  ungroup() %>%
  
  group_by(year, race) %>%
  filter(max(n) == n) %>%
  mutate(best = min(time, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(time_lost = time - best)
  
  left_join(unique_GT_stages, by = c("race", "year")) %>%
  
  select(-DATE) %>%
  
  mutate(date = date + 1) %>%
  
  left_join(lt_performance %>% 
              mutate(year = lubridate::year(date)) %>% 
              filter(stage_type == "mountain" & stages > 9) %>%
              select(rider, year, LT = median, date), by = c("rider", "year", "date")) %>% 
  
  left_join(st_performance %>% 
              mutate(year = lubridate::year(date)) %>%
              filter(stage_type == "mountain" & stages > 1) %>%
              select(rider, year, ST = median, date), by = c("rider", "year", "date")) %>% 
  
  group_by(race, year) %>%
  filter(n == max(n)) %>%
  ungroup() %>%
  
  filter(!is.na(LT)) %>%
  
  mutate(ST = ifelse(is.na(ST), LT - 1, ST)) %>%
  
  mutate(PERF = (0.85 * LT) + (0.15 * ST),
         PERF2 = (0.62 * LT) + (0.38 * ST)) %>%
  
  mutate(ST_imp = ST - LT,
         ST_imp = ifelse(ST_imp > 1, 1, ifelse(ST_imp < -1,-1, ST_imp))) %>%
  
  group_by(year, race) %>% 
  mutate(rk_lead = rank(-PERF, ties.method = "min"),
         rk_lead2 = rank(-PERF2, ties.method = "min"),
         rk_perf = rank(to_gc, ties.method = "min"),
         rk_ST = rank(-ST, ties.method = "min"),
         rk_LT = rank(-LT, ties.method = "min"),
         PERF = PERF - mean(PERF, na.rm = T),
         PERF2 = PERF2 - mean(PERF2, na.rm = T)) %>% 
  ungroup()
  
#

summary(glm(win ~ log(rk_LT+1) + ST_imp, data = gt_climbing_eval %>% mutate(win = rk_perf < 4), family = "binomial"))

#

tdf_2019_cl <- lt_performance %>% filter(date == '2019-07-06') %>% mutate(year = lubridate::year(date)) %>% filter(stage_type == "mountain" & stages > 9) %>% select(rider, year, LT = median) %>% left_join(st_performance %>% filter(date == '2019-07-06') %>% mutate(year = lubridate::year(date)) %>% filter(stage_type == "mountain" & stages > 1) %>% select(rider, year, ST = median), by = c("year", "rider")) %>% filter(!is.na(LT)) %>% mutate(ST_imp = (ST - LT), ST_imp = ifelse(ST_imp > 1, 1, ifelse(ST_imp < -1,-1, ST_imp))) %>% mutate(coef = (0.90 * LT) + (0.37 * ST_imp) - 1.46, probs = (exp(coef) / (1+exp(coef))))

#
#
#
#
#
#
#
# impact of highest climb on finishing top 3

in_contact <- stage_performance_data %>%
  filter(!is.na(rnk)) %>% 
  mutate(W = ifelse(rnk == 1,1,0), 
         T3 = ifelse(rnk < 4,1,0), 
         contact = ifelse(gain_gc < 161, 1, 0)) %>% 
  group_by(rider) %>% 
  filter(n() > 99) %>%
  ungroup() %>%
  
  group_by(rider) %>%
  do(broom::tidy(glm(contact ~ concentration, data = ., family = "binomial"))) %>%
  ungroup() %>%
  
  select(rider, term, estimate) %>%
  
  spread(term, estimate) %>% 
  janitor::clean_names() %>%
  mutate(flat = exp((intercept + (0*concentration)))/(1+exp((intercept + (0*concentration)))), 
         cat4 = exp((intercept + (1*concentration)))/(1+exp((intercept + (1*concentration)))),
         cat3 = exp((intercept + (3*concentration)))/(1+exp((intercept + (3*concentration)))),
         wkcat2 = exp((intercept + (5*concentration)))/(1+exp((intercept + (5*concentration)))),
         stcat2 = exp((intercept + (8*concentration)))/(1+exp((intercept + (8*concentration)))),
         cat1 = exp((intercept + (12*concentration)))/(1+exp((intercept + (12*concentration)))),
         HC = exp((intercept + (20*concentration)))/(1+exp((intercept + (20*concentration))))) %>% 
  
  gather(cat, prob, flat:HC) %>% 
  inner_join(tibble(concen = c(0,1,3,5,8,12,20), 
                    cat = c("flat", "cat4", "cat3", "wkcat2", "stcat2", "cat1", "HC")))

#

plot_riders <- in_contact %>%
  filter(rider %in% c("Groenewegen Dylan",
                      "Alaphilippe Julian",
                      "Ewan Caleb",
                      "Roglic Primoz",
                      "Bennett Sam",
                      "Ackermann Pascal",
                      "Lutsenko Alexey",
                      "Teunissen Mike",
                      "Herrada Jesus")) %>%
  
  group_by(rider) %>% 
  mutate(m = sd(prob, na.rm = T)) %>% 
  ungroup()

#

ggplot(plot_riders, 
       
       aes(x = concen, 
           y = prob, 
           label = str_sub(rider, 1, 3),
           fill = reorder(rider, m),
           color = reorder(rider, m)))+
  
  geom_point(size = 3, shape = 21, color = "white", stroke = 1)+
  geom_line(size = 1)+
  ggrepel::geom_label_repel(data = plot_riders %>% 
                              group_by(rider) %>% 
                              filter(prob == max(prob)) %>%
                              ungroup(), show.legend = FALSE,
                            color = "white", size = 4)+ 
  scale_y_continuous(labels = scales::percent)+
  labs(x = "categorized rating of highest climb in stage", 
       y = "modeled probability of finishing in top 3", title = "Top 3 probability by toughest climb")+
  scale_fill_discrete(name = "Rider")+
  scale_color_discrete(name = "Rider")+
  coord_cartesian(xlim = c(0,20))+
  theme(axis.text = element_text(size = 14), 
        legend.text = element_text(size = 14))

#
#
#
#
#
#
#
#

# performance by level of event

# take in top 200 UWT riders

top_200_WT <- stage_data %>%
  
  filter(year > 2016 & class %in% c("1.UWT", "2.UWT")) %>%
  
  group_by(rider) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  
  filter(rank(-count, ties.method = 'min') < 201)

# take in all events

perf_by_level_data <- stage_data %>%
  
  inner_join(
    
    top_200_WT %>%
      select(rider), by = c("rider")
    
  ) %>%
  
  select(race, year, level, stage, rnk, rider, gain_1st) %>%
  
  unique()

# set up model

model_list <- vector("list", 15)
glmer_list <- vector("list", 15)

for(m in 1:15) {
  
  d <- perf_by_level_data %>%
    
    mutate(top = ifelse(rnk < (m + 1), 1, 0))
  
  glm <- glm(top ~ level, data = d, family = binomial(link = "logit"))
  
  model_list[[m]] <- cbind(glm$fitted.values, 
                           glm$data %>% filter(!is.na(rnk))) %>%
    mutate(thresh = m)
  
  #glmer <- lme4::glmer(top ~ (1 | race:year), data = d, family = binomial(link = "logit"))
  
  #glmer_list[[m]] <- summary(glmer)$coefficients %>%
  #  as_tibble()
  
}

glm_data <- bind_rows(model_list) %>%
  select(level, fit = `glm$fitted.values`, thresh) %>%
  unique()

# GAM smoother for performance by max effort needed

gam_data <- stage_data %>%
  
  select(stage, race, year, date, concentration, last_climb, rnk, rider, 
         time_trial, summit_finish, act_climb_difficulty, level) %>%
  
  # set thresholds from the above work such that TDF has 12 spots and lower levels scale from there
  # eg, finishing top 12 in TDF stage is equivalent to top 3 in Tour de l'Ain
  mutate(thresh = ifelse(level == 1, 12,
                         ifelse(level == 0.5, 9,
                                ifelse(level == 0.25, 7,
                                       ifelse(level == 0, 6,
                                              ifelse(level == -0.25, 5,
                                                     ifelse(level == -0.5, 4, 3))))))) %>%
  
  mutate(top = ifelse(rnk < thresh, 1, 0),
         top = ifelse(is.na(top), 0, top)) %>%
  
  left_join(
    
    f_r_climbs %>%
      
      mutate(race = tolower(race)) %>%
      mutate(race = ifelse(race %in% c("la vuelta ciclista a espana", "la vuelta a espana"), "vuelta a espana", race),
             race = ifelse(race == "la route d'occitane", "la route d'occitanie", race)) %>%
      
      left_join(f_r_data %>%
                  select(race, stage, year, stage_length = length) %>%
                  mutate(race = tolower(race)) %>%
                  mutate(race = ifelse(race == "la route d'occitanie" & year == 2019,
                                       "la route d'occitanie - la depeche du midi", race)), by = c("race", "stage", "year")) %>%
      
      mutate(summit_finish = ifelse(abs(end_distance - stage_length) < 2, TRUE, FALSE)) %>%
      mutate(summit_finish = ifelse(race == "tour de romandie" & stage == 4 &
                                      year == 2019 & climb_name == "Torgon", TRUE, summit_finish)) %>%
      
      filter(summit_finish == FALSE) %>%
      
      mutate(last_50km = ifelse(abs(end_distance - stage_length) < 51, TRUE, FALSE)) %>%
      
      filter(last_50km == TRUE) %>%
      
      group_by(race, stage, year) %>%
      filter(rank(-model_category, ties.method = "min") == 1) %>%
      ungroup() %>%
      select(stage, race, year, limit_effort = model_category) %>%
      unique(), by = c("race", "stage", "year")) %>%
  
  mutate(max_effort = ifelse(summit_finish == TRUE, last_climb, rnorm(n(), 0.5, 0.1)),
         limit_effort = ifelse(is.na(limit_effort), rnorm(n(),0.5,0.1), limit_effort)) %>%
  
  filter(time_trial == FALSE) %>%
  
  group_by(rider) %>%
  mutate(n = n(),
         top_5 = sum(top, na.rm = T)) %>%
  ungroup() %>%
  
  filter((n > 74 | 
            rider %in% c("Pogacar Tadej", "Higuita Sergio", "Jakobsen Fabio", "van Aert Wout")) &
           top_5 > 4) %>%
  
  mutate(weight = 365 * (1 / (365 + as.numeric((lubridate::today() - as.Date(date))))))

#

rs <- gam_data %>% select(rider) %>% unique()
ks <- c(3, 4, 5, 7, 10)

rider_list <- vector("list", length(rs$rider %>% unique()))
prds_list <- rider_list

prds <- tibble(max_effort = c(0.5, 1, 2.5, 5, 7.5, 10, 15,
                              0.5, 1, 2.5, 5, 7.5, 10, 15,
                              0.5, 1, 2.5, 5, 7.5, 10, 15,
                              0.5, 1, 2.5, 5, 7.5, 10, 15,
                              0.5, 1, 2.5, 5, 7.5, 10, 15,
                              0.5, 1, 2.5, 5, 7.5, 10, 15,
                              0.5, 1, 2.5, 5, 7.5, 10, 15,
                              0.5, 1, 2.5, 5, 7.5, 10, 15,
                              0.5, 1, 2.5, 5, 7.5, 10, 15),
               limit_effort = c(0, 0, 0, 0, 0, 0, 0,
                                1.5,1.5,1.5,1.5,1.5,1.5,1.5,
                                2.5,2.5,2.5,2.5,2.5,2.5,2.5,
                                4,4,4,4,4,4,4,
                                5,5,5,5,5,5,5,
                                7,7,7,7,7,7,7,
                                9,9,9,9,9,9,9,
                                12,12,12,12,12,12,12,
                                16,16,16,16,16,16,16))

# run GAM for each rider

for(r in 1:length(rider_list)) {
  
  k_list <- vector("list", 5)
  k_list2 <- k_list
  
  for(K in 1:5) {
    
    mod <- mgcv::gam(top ~ s(max_effort, k = ks[[K]]) + s(limit_effort, k = ks[[K]]),
                     data = gam_data %>% 
                       filter(rider == rs$rider[[r]]),
                     weights = weight,
                     family = "binomial",
                     method = "REML")
  
  fits <- cbind(pred = mod$fitted.values, mod$model) %>%
    mutate(rider = rs$rider[[r]],
           k_value = ks[[K]],
           dev_exl = 1 - (mod$deviance / mod$null.deviance)
           )
  
  preds <- cbind(prds, pred = predict(mod, prds)) %>%
    mutate(rider = rs$rider[[r]],
           k_value = ks[[K]],
           dev_exl = 1 - (mod$deviance / mod$null.deviance)
    )
  
  k_list[[K]] <- fits
  k_list2[[K]] <- preds
  
  }
  
  rider_list[[r]] <- bind_rows(k_list)
  prds_list[[r]] <- bind_rows(k_list2)
  
}

#

rider_gam_fits <- bind_rows(rider_list)

rider_gam_preds <- bind_rows(prds_list) %>%
  mutate(coef = pred,
         pred = (exp(coef)) / (1+exp(coef)))

#

ggplot(rider_gam_preds %>% 
         filter(rider == "Pogacar Tadej") %>% 
         filter(k_value == 5) %>% 
         gather(effort, value, max_effort:limit_effort) %>%
         inner_join(
           
           rider_gam_preds %>% 
             filter(rider == "Pogacar Tadej") %>% 
             filter(k_value == 5)) %>%
             mutate(opposite = ifelse(effort == "max_effort", limit_effort, max_effort)) %>%
             select(-limit_effort, -max_effort), 
       
       aes(x = value, y = pred, color = opposite))+
  geom_point()+
  facet_wrap(~effort, scales = "free_x")+
  scale_color_gradientn(colors = c("orange", "gray", "#37B36C"), guide = F)

