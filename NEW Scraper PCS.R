

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

pull_from_schedule <- c(
  
  # WORLD TOUR
  'https://www.procyclingstats.com/races.php?year=2019&circuit=1&class=&filter=Filter',
  
  # EUROPE
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=2.1&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=1.1&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=2.2&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=1.2&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=2.HC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=1.HC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=2.Pro&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=1.Pro&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=CC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2013&circuit=21&class=&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2013&circuit=26&class=&filter=Filter',
  
  # OLYMPICS AND WORLD CHAMPS
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=3&class=Olympics&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=2&class=WC&filter=Filter',
  
  # some of these world champs/olympics are hiding in continental tours
  'https://www.procyclingstats.com/races.php?year=2016&circuit=18&class=Olympics&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2016&circuit=12&class=WC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2015&circuit=18&class=WC&filter=Filter',
  
  # NAT'L Champs
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=NC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=11&class=NC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=12&class=NC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=14&class=NC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=18&class=NC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=&class=NC&filter=Filter',
  
  # ASIA
  'https://www.procyclingstats.com/races.php?year=2019&circuit=12&class=1.HC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=12&class=2.HC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=12&class=1.Pro&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=12&class=2.Pro&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=12&class=1.1&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=12&class=2.1&filter=Filter',
  
  # AFRICA
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=11&class=2.1&filter=Filter',
  
  # OCEANIA
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=14&class=2.1&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=14&class=2.1&filter=Filter',
  
  # AMERICAS
  'https://www.procyclingstats.com/races.php?year=2019&circuit=18&class=2.1&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=18&class=2.HC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=18&class=1.Pro&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=18&class=2.Pro&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2020&circuit=13&class=WC&filter=Filter')

#

store_from_schedule <- vector("list", length(pull_from_schedule))

pull_years = 1

current_year = year(today())
start_year = 2020 # set to 2012 to pull 2013, 2019 to pull 2020

#
# pull in each type and then each year
#

for(t in 1:length(pull_from_schedule)) {
  
  year_list <- vector("list", pull_years)
  
  # pull in each year
  for(y in 1:length(year_list)) {
    
    year = start_year + y
    
    url <- paste0(
      str_sub(pull_from_schedule[[t]], 1, 47),
      year,
      str_sub(pull_from_schedule[[t]], 52, nchar(pull_from_schedule[[t]])))
    
    # if it's current year handle differently
    
    if(year == current_year) {
      
      page <- url %>%
        read_html()
      
      # bring in all events
      evts <- page %>%
        html_nodes('table') %>%
        html_table(dec = ",") %>%
        .[[1]] %>%
        .[, c(1, 3:5)]
      
      # if no events, skip to next
      
      if(length(evts$Date) == 0) {
        
      } else {
        
        # calculate events which have started and which have winners
        # exclude those which have not started AND those with blank winners which have ended
        evts <- evts %>%
          mutate(DateEnd = as.Date(paste0(year, "-", str_sub(Date, nchar(Date)-1, nchar(Date)), "-", as.numeric(str_sub(Date, nchar(Date)-4, nchar(Date)-3))))) %>%
          mutate(DateStart = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2))))) %>%
          filter(!(DateStart > lubridate::today() | (Winner == "" & DateEnd < lubridate::today()))) %>%
          count() %>%
          as.list() %>%
          .[[1]]
        
        # if no events qualify, skip to next
        if(evts == 0) {
        } else {
          
          # if events do qualify, filter to remove cancelled events
          events <- cbind(
            
            page %>%
              html_nodes('table.basic') %>%
              html_table(dec = ",") %>%
              .[[1]] %>%
              .[, c(1, 3:5)] %>%
              mutate(DateEnd = as.Date(paste0(year, "-", str_sub(Date, nchar(Date)-1, nchar(Date)), "-", as.numeric(str_sub(Date, nchar(Date)-4, nchar(Date)-3))))) %>%
              mutate(DateStart = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2))))) %>%
              filter(!(DateStart > lubridate::today() | (Winner == "" & DateEnd < lubridate::today()))),
            
            value = cbind(page %>%
                            html_nodes('table.basic') %>%
                            html_nodes('a') %>%
                            html_attr(name = "href") %>%
                            enframe(name = NULL) %>%
                            filter(str_detect(value, "race/")) %>%
                            filter(str_detect(value, as.character(year))) %>%
                            filter(!str_detect(value, "stage-")) %>%
                            filter(!str_detect(value, "result")) %>%
                            filter(!(str_detect(value, "2020/"))) %>%
                            #unique() %>%
                            rename(url = value),
                          
                          page %>%
                            html_nodes('table.basic') %>%
                            html_nodes('tbody') %>%
                            html_nodes('tr') %>%
                            html_attr(name = "class") %>%
                            enframe(name = NULL)) %>%
              filter(is.na(value)) %>%
              select(-value) %>%
              .[1:evts,]
            
          ) %>%
            
            mutate(year = year) %>%
            rename(url = value)
          
          year_list[[y]] <- events
          
        }
        
      }
      
    }
    
    # if not current year, handle the same
    else {
      
      page <- url %>%
        read_html()
      
      events <- cbind(
        
        page %>%
          html_nodes('table') %>%
          html_table(dec = ",") %>%
          .[[1]],
        
        page %>%
          html_nodes('table') %>%
          html_nodes('a') %>%
          html_attr(name = "href") %>%
          tibble::enframe(name = NULL) %>%
          filter(str_detect(value, "race/")) %>%
          filter(str_detect(value, as.character(year)))
        
      ) %>%
        
        mutate(year = year) %>%
        rename(url = value)
      
      year_list[[y]] <- events
      
    }
    
  }
  
  store_from_schedule[[t]] <- bind_rows(year_list) %>%
    mutate(type = pull_from_schedule[[t]])
  
}

#
#
# EVENTS
#
#

natl_champs <- c("race/nc-spain",
                 "race/nc-italy",
                 "race/nc-austria",
                 "race/nc-netherlands",
                 "race/nc-belgium",
                 "race/nc-france",
                 "race/nc-germany",
                 "race/nc-great-britain",
                 "race/nc-switserland",
                 "race/nc-czech-republic",
                 "race/nc-denmark",
                 "race/nc-ireland",
                 "race/nc-slovakia",
                 "race/nc-norway",
                 "race/nc-colombia",
                 "race/nc-south-africa",
                 "race/nc-united-states",
                 "race/nc-australia",
                 "race/nc-slovenia")

extra_races <- c("race/world-championship",
                 "race/world-championship-itt",
                 "race/olympic-games",
                 "race/olympic-games-itt",
                 "race/uec-road-european-championships",
                 "race/uec-road-european-championships-itt")

# remove tours without races (eg, Africa Tour 2013)

store_from_schedule <- store_from_schedule[lengths(store_from_schedule) != 0]

#

all_events <- bind_rows(store_from_schedule) %>%
  
  filter(!(str_detect(Race, " WE"))) %>%
  filter(!(str_detect(Race, "Women"))) %>%

  #mutate(type = str_sub(type, 61, nchar(type)),
  #       type = str_replace(type, '&filter=Filter', "")) %>%
  
  #mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2))))) %>%
  
  select(-DateEnd,-Date) %>%
  rename(Date = DateStart) %>%
  
  unique() %>%
  
  mutate(spec_url = str_sub(url, 1, nchar(url)-5)) %>%
  
  # we only want top level national championships, so filter out chaff
  filter(Class != "NC" | (Class == "NC" & spec_url %in% natl_champs)) %>%
  
  # we only want race in extra_races for a lot of categories
  filter(Class != "CC" | (Class == "CC" & spec_url %in% extra_races)) %>%
  filter((!Class %in% c("WC", "Olympics")) | 
           (Class %in% c("WC", "Olympics") & spec_url %in% extra_races)) %>%
  
  filter(lubridate::year(Date) > start_year) %>%
  
  unique() %>%
  
  select(-spec_url) %>%
  mutate(url = str_replace(url, "/startlist/preview", ""))

#
# Write DB Table
#

all_events$Race <- iconv(all_events$Race, from="UTF-8", to = "ASCII//TRANSLIT")
all_events$Winner <- iconv(all_events$Winner, from="UTF-8", to = "ASCII//TRANSLIT")

new_events <- all_events %>%
  anti_join(
    
    dbReadTable(con, "pcs_all_races") %>%
      filter(type != "scraper") %>%
      filter(Winner != ""), by = c("url")
    
  ) %>%
  
  anti_join(
    
    dbReadTable(con, "pcs_all_races") %>%
      filter(type != "scraper") %>%
      filter(Winner != ""), by = c("Race", "year", "Class")
    
  )

all_events <- all_events %>%
  anti_join(
    
    dbReadTable(con, "pcs_all_races") %>%
      filter(type != "scraper"), by = c("url")
    
  ) %>%
  
  anti_join(
    
    dbReadTable(con, "pcs_all_races") %>%
      filter(type != "scraper"), by = c("Race", "year", "Class")
    
  )

print(new_events)

dbWriteTable(con, "pcs_all_races", all_events, row.names = FALSE, append = TRUE)

all_events <- new_events

#
#
# Now extract stage data
#
#

stages_list <- vector("list", length(all_events$url))
gc_list <- vector("list", length(all_events$url))

# this section doesn't work any more

tictoc::tic()

for(e in 1:length(all_events$url)) {  
  
  # go to generic event page and find which stage is listed last
  
  page <- paste0('https://www.procyclingstats.com/', 
                 all_events$url[[e]],
                 '/gc/result/result') %>%
    
    read_html()
  
  page2 <- paste0('https://www.procyclingstats.com/', 
                 all_events$url[[e]],
                 '/gc/stages') %>%
    
    read_html()

  # pull in stages
  
  as <- page2 %>%
    html_nodes('table.basic') %>%
    html_nodes('td') %>%
    html_nodes('a') %>%
    html_attr(name = "href") %>%
    enframe(name = NULL) %>% 
    filter(str_detect(value, 'race/'))
  
  # this part will break if there's multiple races in one year
  
  which_is_gc <- page %>%
    html_nodes('ul.restabs') %>%
    html_nodes('li') %>%
    html_text() %>%
    enframe(name=NULL) %>%
    mutate(isGC = value == "GC") %>%
    rowid_to_column() %>%
    filter(isGC == TRUE) %>%
    .[[1]]
  
  if(is.null(which_is_gc) | length(which_is_gc) == 0) {
    
    gc_winner = tibble(value = "One Day Race")
    
  } else {
  
  gc_winner <- page %>% 
    html_nodes('div.result-cont') %>%
    html_nodes('table') %>%
    html_table() %>%
    .[[which_is_gc]] %>%
    select(Rider,Team) %>%
    mutate(Rider = str_sub(Rider, 1, nchar(Rider)-nchar(Team))) %>%
    .[1,] %>%
    select(value = Rider)
  
  }
  
  # assign to gc list
  gc_list[[e]] <- gc_winner %>%
    mutate(event = all_events$url[[e]])
  
  if(length(as$value) == 0) {
    
    as <- tibble(value = "One Day Race")
    
  }
  
  # stages list
  stages_list[[e]] <- as %>%
    mutate(event = all_events$url[[e]])
  
  print(e)
  
  Sys.sleep(2.5)
  
}

tictoc::toc()

#
# GC winners
#

# when bringing these back in, assign the stage winner of One Day Race to be the gc_winner

gc_winners <- bind_rows(gc_list) %>%
  mutate(gc_winner = value)

gc_winners$gc_winner <- iconv(gc_winners$gc_winner, from="UTF-8", to = "ASCII//TRANSLIT")

dbWriteTable(con, "pcs_gc_winners", gc_winners %>% filter(!is.na(gc_winner)), append = TRUE, row.names = FALSE)

#
#
# Scrape each stage
#
#

all_stages <- all_events %>%
  
   inner_join(
     
     bind_rows(stages_list), by = c("url" = "event")) %>%
  
  filter(!(value %in% c('race/paris-nice/2020/stage-8'))) %>%
  
  unique() %>%
  
  mutate(value = ifelse(value == "One Day Race", url, value),
         value = paste0('https://www.procyclingstats.com/', value)) %>%
  
  group_by(Race, year) %>%
  mutate(s = rank(Race, ties.method = "first")) %>%
  ungroup()

#

all_stages$Race <- iconv(all_stages$Race, from="UTF-8", to = "ASCII//TRANSLIT")
all_stages$Winner <- iconv(all_stages$Winner, from="UTF-8", to = "ASCII//TRANSLIT")

dbWriteTable(con, "pcs_all_stages", all_stages, append = TRUE, row.names = FALSE)

#
#
#
#
#
# pull in directory of HTML downloads (about ~75 stages don't/can't download)
html_stage_dir <- fs::dir_ls("PCS-HTML/")

# download new stages (TRUE) or use old HTML (FALSE)
dl_html <- TRUE

# start scraping process using old data

if(dl_html == FALSE) {
  
  all_stages <- dbReadTable(con, "pcs_all_stages") %>%
    
    filter(year >= 2021)
  
}

#
# for loop for each stage
#

races_list <- vector("list", length(all_stages$Race))

#

tictoc::tic()

#

for(r in 1:length(all_stages$value)) {
  
  f_name <- paste0("PCS-HTML/", str_replace_all(str_replace(all_stages$value[[r]], "https://www.procyclingstats.com/race/", ""), "/", ""))
  
  # match existence of f_name in stage directory
  if(f_name %in% html_stage_dir | dl_html == TRUE) {
  
  race_url <- all_stages$url[[r]]
  
  s = all_stages$s[[r]]
  
  # these stages were cancelled or are TTTs so we ignore them
  # check again for TTT later
  
  if((race_url == "race/paris-nice/2016" & s == 4) | 
     (race_url == "race/tirreno-adriatico/2016" & s == 5) |
     (race_url == "race/tour-de-pologne/2016" & s == 6) | 
     (race_url == "race/giro-d-italia/2013" & s == 19) |
     (race_url == "race/dauphine/2015" & s == 3) |
     (race_url == "race/tour-de-france/2018" & s == 3) |
     (race_url == "race/vuelta-a-espana/2016" & s == 1) |
     (race_url == "race/vuelta-a-espana/2017" & s == 1) |
     (race_url == "race/tour-de-france/2015" & s == 9) |
     (race_url == "race/dauphine/2018" & s == 4) |
     (race_url == "race/tour-de-suisse/2018" & s == 1) |
     (race_url == "race/binckbank-tour/2016" & s == 5) |
     (race_url == "race/tirreno-adriatico/2018" & s == 1) |
     (race_url == "race/tirreno-adriatico/2016" & s == 1) |
     (race_url == "race/tirreno-adriatico/2017" & s == 1) |
     (race_url == "race/volta-a-catalunya/2017" & s == 2) |
     (race_url == "race/giro-d-italia/2018" & s == 21) |
     (race_url == "race/vuelta-a-espana/2015" & s == 1) |
     (race_url == "race/dookola-mazowsza/2020" & s == 1) |
     (race_url == "race/60th-tour-de-picardie/2016" & s == 1) |
     (race_url == "race/baltic-chain-tour/2015" & s == 2) |
     (race_url == 'race/tour-of-mersin/2017' & s == 4) |
     (race_url == 'race/romanian-cycling-tour/2012' & s == 2) |
     (race_url == "race/baltic-chain-tour/2011") |
     (race_url == 'race/volta-a-catalunya/2012' & s == 3) |
     (race_url == 'race/bayern-rundfarht/2012' & s == 2) |
     (race_url == 'race/nc-czech-republic/2011') |
     (race_url == 'race/nc-south-africa/2011') |
     (race_url == 'race/central-european-tour-budapest-gp/2011') |
     (str_detect(race_url, "race/hammer-") == TRUE)) {
    
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
    
    url <- all_stages$value[[r]]
    
    # scrape the HTML for the page for multiple use
    
    f_name <- paste0("PCS-HTML/", str_replace_all(str_replace(all_stages$value[[r]], "https://www.procyclingstats.com/race/", ""), "/", ""))
    
    if(dl_html == TRUE) {
      
      download.file(url, f_name, quiet = TRUE)
      
    }
    
    page <- read_file(f_name) %>%
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
        filter(value %in% c("result-cont hide", "result-cont ")) %>%
        tibble::rowid_to_column() %>%
        filter(value == "result-cont ") %>%
        .[[1]]
      
      # bring in stage characteristics
      characteristics <- page %>%
        html_nodes('ul.infolist') %>%
        html_nodes('li') %>%
        html_text() %>%
        enframe(name = NULL) %>%
        separate(value, into = c("col", "data"), sep = ": ") %>%
        mutate(col = str_trim(col),
               data = str_trim(data))
      
      if(length(choose) == 0) {
        
        choose = 1
        
      }
      
      if(length(characteristics) == 0) {
        
        characteristics = 0
        
      }
      
      distance <- characteristics %>%
        filter(col == "Distance") %>%
        mutate(data = parse_number(data)) %>%
        select(data) %>%
        .[[1]]
      
      if(length(distance) == 0) {
        
        distance = NA
        
      }
      
      stage_name <- page %>%
        html_nodes(xpath = '/html/body/div[1]/div[1]/div[2]/div[2]/span[3]') %>%
        html_text()
      
      if(length(stage_name) == 0) {
        
        stage_name = NA
        
      }
  
      # Characteristics End
      
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
          html_nodes(xpath = '/html/body/div[1]/div[1]/div[2]/div[2]/span[8]') %>%
          html_text() 
        
        if(length(length) == 0) {
          
          length = 150
          
        } else {
          
          length = parse_number(str_replace(length, " km", ""))
          
        }
        
        # in case the winner's time is missing (eg, their stupid doping shit with Armstrong) bring in speed
        
        spd <- characteristics %>%
          filter(col == 'Avg. speed winner') %>%
          mutate(data = parse_number(data)) %>%
          select(data) %>%
          .[[1]]
        
        SPD_WIN <- spd
        
        # bring in parcours type information
        
        parcours <- page %>%
          html_nodes('ul.infolist') %>%
          html_nodes('li') %>%
          html_nodes('span') %>%
          html_attr(name = "class")
        
        profile_value <- characteristics %>%
          filter(col == 'ProfileScore') %>%
          select(data) %>%
          .[[1]]
        
        if(length(parcours) == 0) {
          
          parcours = "icon profile p0"
          
        }
        
        if(length(profile_value) == 0) {
          
          profile_value = NA
          
        }
        
        # bring in actual date / added separate because they started adding time of day
        
        DATE <- characteristics %>%
          filter(col == 'Date') %>%
          select(data) %>%
          separate(data, c("data", "junk"), sep = ",") %>%
          mutate(date = lubridate::dmy(str_trim(data))) %>%
          select(date) %>%
          .[[1]]
        
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
                 stage_type = parcours,
                 parcours_value = profile_value) %>%
          
          mutate(stage = s,
                 stage_number = str_replace(str_replace(all_stages$value[[r]], all_stages$url[[r]], ""), "https://www.procyclingstats.com/", ""),
                 race = all_stages$Race[[r]],
                 year = all_stages$year[[r]],
                 date = DATE,
                 url = all_stages$url[[r]],
                 Class = all_stages$Class[[r]]) %>%
          
          mutate(rnk = as.character(rnk))
        
      }
      
    }
    
    
  }
  # write to the race list for this race
  races_list[[r]] <- stage
  
  print(race_url)
  
  if(dl_html == TRUE) {
    
    Sys.sleep(runif(1, 0.5, 3.5))
    
  }
  
  }
  
}

tictoc::toc()

#
#
# WRITE TO DB
#
#

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

races_list <- races_list[lengths(races_list) != 0]

df_list <- races_list

for(f in 1:length(races_list)) { 
  
  df <- races_list[[f]] %>%
    mutate(date = as.character(date))
  
  df$race <- iconv(df$race, from="UTF-8", to = "ASCII//TRANSLIT")
  df$rider <- iconv(df$rider, from="UTF-8", to = "ASCII//TRANSLIT")
  
  df_list[[f]] <- df
  
}

#
#
#

test_dl <- bind_rows(df_list) %>% unique()

test_dl %>% filter(rnk == 1)

# DONT DELETE WHEN UPDATING
#dbSendQuery(con, "DELETE FROM pcs_stage_raw")

#dbWriteTable(con, "pcs_stage_raw", test_dl, append = TRUE, row.names = FALSE)

#
#
# Scrape the GAME data
#
#

# what races to pull

game_race_list <- vector("list", 4)

pull_in_game <- c("2021")

for(g in 1:length(pull_in_game)) {
  
  page <- paste0('https://www.procyclingstats.com/game.php?s=activated-games&season=',
                 pull_in_game[[g]],
                 '&filter=Filter') %>%
    read_html()
  
  res <- cbind(
    
    page %>%
      html_nodes('table') %>%
      html_table() %>%
      .[[1]] %>%
      as_tibble(),
    
    page %>%
      html_nodes('table') %>%
      html_nodes('a') %>%
      html_attr(name = "href") %>%
      enframe(name = NULL) %>%
      rename(url = value))
  
  game_race_list[[g]] <- res
    
}

#

all_games <- bind_rows(game_race_list) %>%
  
  inner_join(
    
    dbReadTable(con, "pcs_all_stages") %>%
      select(url, value, year, s) %>%
      mutate(match_url = paste0(url, "/game")), by = c("url" = "match_url")
    
  ) %>%
  select(Date, Race, Url = value, year, Stage = s) %>%
  
  mutate(Url = paste0(Url, "/game/most-picked")) %>%
  
  anti_join(dbGetQuery(con, "SELECT stage, race, year FROM pcs_game_picks GROUP BY stage, race, year"), by = c("Stage" = "stage", "Race" = "race", "year"))

#
#
# SCRAPE GAME PREDICTED RIDERS
#
#

#
#
# Initial writing of new tables
#
#

most_picked_list <- vector("list", length(all_games$Url))

for(g in 1:length(most_picked_list)) {
  
  page <- all_games$Url[[g]] %>%
    read_html()
  
  res <- cbind(
    
    page %>%
      html_nodes('table') %>%
      html_table() %>%
      .[[1]] %>%
      as_tibble(),
    
    page %>%
      html_nodes('table') %>%
      html_nodes('a') %>%
      html_attr(name = "href") %>%
      enframe(name = NULL) %>%
      rename(url = value)) 
  
  most_picked_list[[g]] <- res %>%
    
    mutate(Result = as.character(Result)) %>%
    
    mutate(Url = all_games$Url[[g]],
           Race = all_games$Race[[g]],
           year = all_games$year[[g]],
           Stage = all_games$Stage[[g]])
  
  print(g)
  
  Sys.sleep(runif(1,0.5,2.5))
  
}

games_most_picked <- bind_rows(most_picked_list) %>%
  
  janitor::clean_names() %>%
  
  select(picked_rider = rider,
         number_picks,
         result,
         race,
         year,
         stage,
         url = url_2)

dbWriteTable(con, "pcs_game_picks", games_most_picked, append = TRUE, row.names = FALSE)

#
#
# Clean up Stage data and merge with other information
#
#

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

stage_data_raw <- dbReadTable(con, "pcs_stage_raw")

stage_data_raw$race <- iconv(stage_data_raw$race, from="UTF-8", to = "ASCII//TRANSLIT")

stage_data_raw$rider <- iconv(stage_data_raw$rider, from="UTF-8", to = "ASCII//TRANSLIT")

stage_data_raw$team <- stringi::stri_trans_general(str = stage_data_raw$team, 
                                          id = "Latin-ASCII")

#

stage_data_int <- stage_data_raw %>%
  
  rename(class = Class) %>%
  unique() %>%
  
  mutate(stage_number = ifelse(stage_name == "One day race", '1',
                               ifelse(stage_name == "Prologue", '0', 
                                      str_replace(
                                        str_replace(stage_number, "/stage-", ""), "stage-", "")))) %>%
  
  mutate(stage = ifelse(stage_name == "Prologue", '0', 
                        ifelse(stage_name == "One day race", '1', stage_number)))
  
  

stage_data <- stage_data_int %>%
  
  left_join(
    
    stage_data_int %>%
      select(stage, race, year, class) %>%
      mutate(valid = 1) %>%
      unique() %>% 
      spread(stage, valid) %>%
      filter(`0` == 1 & is.na(`1`) & is.na(`1a`)) %>%
      select(race, year, class) %>%
      mutate(stage = '0',
             valid = 1), by = c("race", "stage", "year", "class")
    
  ) %>%
  
  mutate(valid = ifelse(is.na(valid), 0, valid)) %>%
  
  mutate(stage = ifelse(stage == '0',
                        ifelse(valid == 1, '1', stage), stage)) %>%
  
  select(-valid) %>%
  
  mutate(stage = ifelse(is.na(stage) & stage_number == "prologue", '0',
                        ifelse(is.na(stage), '1', stage))) %>%

  mutate(stage = ifelse(stage == "https://www.procyclingstats.com/", 1, stage)) %>%
  
  filter(!race %in% c("World Championships MJ - ITT", "World Championships MJ - Road Race",
                      "World Championships WJ - ITT", "World Championships WJ - Road Race",
                      "World Championships WE - ITT", "World Championships WE - Road Race",
                      'Manavgat Side Junior', 'Grand Prix Manavgat - Side WE')) %>%
  
  mutate(distance = str_replace(distance, "\\(", ""),
         distance = str_replace(distance, "\\)", ""),
         distance = as.numeric(str_replace(distance, "k", ""))) %>%
  
  mutate(length = ifelse(is.na(length), distance, length)) %>%
  
  mutate(length = ifelse(year == 2013 & race == 'E3 Prijs Vlaanderen - Harelbeke', 206, length)) %>%
  
  mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team))) %>%
  
  mutate(finished = ifelse(rnk %in% c("DNF", "OTL", "DNS", "NQ", "DSQ"), NA, total_seconds)) %>%
  mutate(total_seconds = ifelse(total_seconds > 30000, NA, total_seconds)) %>%
  
  group_by(stage, race, year, url, class) %>%
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
  
  group_by(stage, race, year, url, class) %>%
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
  
  mutate(time_trial = ifelse(stage_name %in% c("Time trial", "Prologue") | str_detect(stage_name, "ITT"), TRUE, FALSE)) %>%

  mutate(grand_tour = ifelse(race %in% c("Tour de France", "Giro d'Italia", 
                                         "La Vuelta ciclista a Espana", "Vuelta a Espana"), TRUE, FALSE)) %>%
  
  mutate(one_day_race = ifelse(stage_name == "One day race", TRUE, FALSE)) %>%
  
  mutate(climb_difficulty = NA) %>%
  
  # find rider position in team for stage
  
  group_by(team, stage, race, year, url, class) %>%
  mutate(tm_pos = rank(rnk, ties.method = "first")) %>%
  ungroup() %>%
  
  select(-stage_number)

#

winners <- dbReadTable(con, "pcs_all_races") 

winners$Winner <-  str_to_title(tolower(winners$Winner))
winners$Winner <- iconv(winners$Winner, from="UTF-8", to = "ASCII//TRANSLIT")
winners$Race <- str_to_title(tolower(winners$Race))
winners$Race <- iconv(winners$Race, from="UTF-8", to = "ASCII//TRANSLIT")
winners$Race <- tolower(winners$Race)
winners$Date <- as.Date(winners$Date)

#

gc_performance <- winners %>%
  
  select(race = Race,
         winner = Winner,
         class = Class,
         url, year) %>%

  inner_join(
    
    stage_data %>%
      mutate(rider = str_to_title(tolower(rider))) %>%
      mutate(race = tolower(race)) %>% unique(), by = c("race", "year", "winner" = "rider", "class", "url")
    
  ) %>%
  
  select(gc_winner = winner, race, year, stage, gc_seconds = total_seconds, class, url) %>%
  unique()

#

stage_data <- stage_data %>%
  
  mutate(race = tolower(race)) %>%
  
  left_join(
    
    gc_performance, by = c("year", "race", "stage", "class", "url")
    
  ) %>%
  
  select(-mean_time, -med_time, -qual_time, -rel_qual,
         -climb_difficulty, -variance_valid, -distance,
         -rel_time) %>%
  
  mutate(gain_gc = total_seconds - gc_seconds) %>%
  
  mutate(gc_pos = ifelse(tolower(rider) == tolower(gc_winner), rnk, NA)) %>%
  
  group_by(race, stage, year, url, class) %>%
  mutate(gc_pos = mean(gc_pos, na.rm = T) + 5) %>%
  ungroup() %>%
  
  mutate(back_5 = ifelse(gc_pos == rnk, total_seconds, NA)) %>%
  
  group_by(race, stage, year, url, class) %>%
  mutate(back_5_seconds = mean(back_5, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(gain_back_5 = total_seconds - back_5_seconds) %>%
  
  select(-back_5)

#
#
# Need to clean-up and re-write climbs information before joining

f_r_climbs <- dbReadTable(con, "flamme_rouge_climbs") %>%
  mutate(year = as.numeric(year)) %>%
  mutate(time_climbed = NA) %>%
  unique() %>%
  
  mutate(stage = ifelse(str_detect(stage, "-1"), str_replace(stage, "-1", "a"),
                        ifelse(str_detect(stage, "-2"), str_replace(stage, "-2", "b"), stage))) %>%
  # NEED TO CLEAN UP fr DATA
  filter(!(race %in% c("Eschborn-Frankfurt") & year %in% c(2018, 2019))) %>%
  filter(!(race == 'Liege - Bastogne - Liege' & year == 2018))

#
#
# bring in f_r stage characteristics

f_r_data <- dbReadTable(con, "flamme_rouge_characteristics") %>%
  mutate(year = as.numeric(year)) %>%
  unique() %>%
  
  mutate(stage = ifelse(str_detect(stage, "-1"), str_replace(stage, "-1", "a"),
                        ifelse(str_detect(stage, "-2"), str_replace(stage, "-2", "b"), stage))) %>%
  # NEED TO CLEAN UP fr DATA
  filter(!(race %in% c("Eschborn-Frankfurt") & year %in% c(2018, 2019))) %>%
  filter(!(race == 'Liege - Bastogne - Liege' & year == 2018)) %>%
  filter(!(race == "Chrono des Nations" & year %in% c(2018, 2019) & stage_type == "Plain"))

#
#
# manually bring in some higher profile race climb info

supp_climbs <- read_csv("supplemental-profile-data.csv") %>%
  fill(stage, race, year) %>%
  filter(!(race == "criterium du dauphine" & year == 2014)) %>%
  filter(!(race == "volta ciclista a catalunya" & year == 2015)) %>%
  filter(!(race == "tour de romandie" & year == 2018)) %>%
  filter(!(race == "paris - nice" & year == 2016)) %>%
  
  mutate(stage = as.character(stage)) %>%
  
  anti_join(
    
    f_r_climbs %>%
      select(race, year, stage) %>%
      unique(), by = c("race", 'year', 'stage'))

#
# combine F-R data with PCS data
#

stage_data <- stage_data %>%
  
  unique() %>%
  
  left_join(
    
    read_csv("pcs-fr-mismatched-stages.csv") %>%
      select(stage, race, year, class, fr_stage) %>%
      unique(), by = c("stage", "race", 'year', 'class')
    
  ) %>%
  
  mutate(fr_stage = ifelse(is.na(fr_stage), stage, fr_stage)) %>%
  
  # join with stage characteristic data
  
  left_join(
    
    f_r_data %>%
      select(-length, -slug, -date) %>%
      rename(fr_stage_type = stage_type) %>%
      mutate(race = tolower(race)), by = c("race", "fr_stage" = "stage", "year")
    
  ) %>%
  
  # join with climb level data
  
  left_join(
    
    f_r_climbs %>%
      
      mutate(stage = as.character(stage)) %>%
      
      mutate(race = tolower(race)) %>%
      
      left_join(f_r_data %>%
                  select(race, stage, year, stage_length = length) %>%
                  mutate(race = tolower(race)), by = c("race", "stage", "year")) %>%
      
      # add in specific climbs I've gathered
      rbind(
        
        cbind(supp_climbs %>%
                filter(is.na(category)) %>%
                fill(stage, .direction = "down") %>%
                fill(race, .direction = "down") %>%
                fill(year, .direction = "down") %>%
                mutate(gradient = gradient / 100,
                       vam_poly = (((gradient)^2)*length),
                       alt = (20641 * vam_poly)-722,
                       start_distance = KM_top - length,
                       summit = alt + 1000,
                       time_climbed = NA),
              model_category = mgcv::predict.gam(read_rds('model-climb-difficulty.rds'),
                                                 supp_climbs %>%
                                                   filter(is.na(category)) %>%
                                                   fill(stage, .direction = "down") %>%
                                                   fill(race, .direction = "down") %>%
                                                   fill(year, .direction = "down") %>%
                                                   mutate(gradient = gradient / 100, 
                                                          vam_poly = ((gradient^2)*length),
                                                          alt = (20641 * vam_poly)-722))) %>%
          filter(model_category > 1) %>%
          rename(stage_length = KM_stage,
                 end_distance = KM_top) %>%
          select(-category)) %>%
      
      # when I just have category type, extrapolate
      rbind(
        supp_climbs %>%
          filter(!is.na(category)) %>%
          fill(stage, .direction = "down") %>%
          fill(race, .direction = "down") %>%
          fill(year, .direction = "down") %>%
          mutate(model_category = ifelse(
            category == "5", 16,
            ifelse(category == "1", 10,
                   ifelse(category == "2", 5,
                          ifelse(category == "3", 2.5,
                                 ifelse(category == "4", 1.25, 0)))))) %>%
          filter(model_category > 1) %>%
          rename(stage_length = KM_stage,
                 end_distance = KM_top) %>%
          select(-category) %>%
          mutate(start_distance = NA,
                 time_climbed = NA,
                 summit = NA,
                 alt = NA,
                 vam_poly = NA)) %>%
      
      group_by(stage, race, year) %>%
      mutate(position_highest = max(model_category, na.rm = T),
             last_climb = max(end_distance, na.rm = T)) %>%
      ungroup() %>%
      
      mutate(position_highest = ifelse(position_highest == model_category, end_distance / stage_length, NA),
             last_climb = ifelse(last_climb == end_distance, model_category, NA)) %>%
      
      mutate(summit_finish = ifelse(abs(end_distance - stage_length) < 4, TRUE, FALSE)) %>%
      mutate(summit_finish = ifelse(race == "tour de romandie" & stage == 4 &
                                      year == 2019 & climb_name == "Torgon", TRUE, summit_finish)) %>%
      
      # increase KOM points by 25% if summit finish
      mutate(basic_kom_points = model_category,
             distance_left = stage_length - end_distance,
             distance_left = ifelse(distance_left < 0, 0, distance_left),
             # use a decay model for kom points
             kom_points = 1.02*exp(-0.02*distance_left),
             kom_points = ifelse(kom_points > 1, 1, ifelse(kom_points < 0.1, 0.1, kom_points)),
             kom_points = kom_points * model_category,
             #kom_points = ifelse(summit_finish == TRUE, model_category * 1.25, model_category),
             climbing_end = ifelse((stage_length - end_distance) < 20.1, kom_points, NA)) %>%
      
      group_by(race, stage, year) %>%
      filter(rank(-kom_points, ties.method = "first")<11) %>%
      summarize(cat_climb_length = sum(end_distance - start_distance, na.rm = T),
                concentration = max(kom_points, na.rm = T),
                number_cat_climbs = sum(kom_points >= 0, na.rm = T),
                climbing_final_20km = sum(climbing_end, na.rm = T),
                raw_climb_difficulty = sum(basic_kom_points, na.rm = T),
                act_climb_difficulty = sum(kom_points, na.rm = T),
                last_climb = max(last_climb, na.rm = T),
                position_highest = mean(position_highest, na.rm = T),
                summit_finish = max(summit_finish, na.rm = T)) %>%
      ungroup(), by = c("race", "fr_stage" = "stage", "year")
    
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
  
  mutate(position_highest = ifelse(position_highest > 1, 1, position_highest),
         position_highest = ifelse(is.na(position_highest), median(position_highest, na.rm = T), position_highest),
         summit_finish = ifelse(is.na(summit_finish), 0, summit_finish)) %>%
  
  mutate(missing_profile_data = ifelse(is.na(act_climb_difficulty), TRUE,
                                       ifelse(is.na(total_vert_gain), TRUE, FALSE))) %>%
  
  unique()

# final cleanup before writing

stage_data <- stage_data %>%
  mutate(time_trial = as.numeric(time_trial),
         grand_tour = as.numeric(grand_tour),
         one_day_race = as.numeric(one_day_race),
         missing_profile_data = as.numeric(missing_profile_data)) %>%
  select(-fr_stage) %>%
  filter(!(str_detect(url, "81st-sch"))) %>%
  
  # world championships rr 2016 joins twice onto profile data
  
  filter(!(race == 'elfstedenronde' & url == 'race/bruges-cycling-classic/2018')) %>%
  filter(!(race == 'elfstedenronde' & url == 'race/bruges-cycling-classic/2019')) %>%
  filter(!(race == 'carrefour market heistse pijl' & url == 'race/heistse-pijl/2017')) %>%
  filter(!(race == 'market heistse pijl' & url == 'race/heistse-pijl/2018')) %>%
  filter(!(race == 'heistse pijl - heist op den berg' & url == 'race/heistse-pijl/2016')) %>%
  filter(!(race == 'heylen vastgoed heistse pijl' & url == 'race/heistse-pijl/2019')) %>%
  filter(!(race == 'ride bruges (bruges cycling classic)' & url == 'race/circuit-des-xi-villes/2017')) %>%
  filter(!(race == 'riga - jurmala grand prix	' & url == 'race/jurmala-gp/2013')) %>%
  
  filter(!(race == "brussels cycling classic" & year == 2017)) %>%
  filter(!(race == "la poly normonde")) %>%
  filter(!(race == 'manavgat side junior')) %>%
  
  filter(!(race == "dubai tour" & year == 2014 & class == "2.HC")) %>%

  unique() %>% 
  
  mutate(rider = str_to_title(rider))

# Write the cleaned-up data to database

#dbSendQuery(con, "DELETE FROM pcs_stage_data")

dbWriteTable(con, "pcs_stage_data", 
             
             stage_data, 
             
             append = TRUE, row.names = FALSE)

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

# Pull in startlists ------------------------------------------------------


library(tidyverse)
library(lubridate)
library(rvest)
library(RMySQL)

dbDisconnect(con)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

all_races <- dbGetQuery(con, "SELECT DISTINCT race, year, class, date, url, stage, stage_name, one_day_race
                        FROM pcs_stage_data
                        WHERE year > 2013") %>%
  
  arrange(-year) %>%
  filter(class %in% c("WC", "1.UWT", "2.UWT", "2.1", "1.1", "1.Pro", "2.Pro", "1.HC", "2.HC")) %>%
  
  filter(stage != 0) %>%
  
  group_by(race, year, class) %>%
  filter(stage == min(stage, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(stage_name = str_sub(stage_name, 1, 8)) %>%
  
  anti_join(dbGetQuery(con, "SELECT race, year, url FROM pcs_all_startlists"), by = c("race", "year", "url"))

#
# THIS IS PROBABLY BROKEN
#

for(r in 1:length(all_races$url)) {
  
  if(all_races$one_day_race[[r]] == 0) {
  
  f_name <- paste0("PCS-HTML/", str_replace_all(str_replace(all_races$url[[r]], "race/", ""), "/", ""), "stage-", 
                   str_trim(str_replace(all_races$stage_name[[r]], "Stage ", "")))
  
  } else {
  
  f_name <- paste0("PCS-HTML/", str_replace_all(str_replace(all_races$url[[r]], "race/", ""), "/", ""))
  
  }
  
  page <- read_file(f_name) %>%
    read_html()
  
  # bring in the list of tables
  
  d <- page %>%
    html_nodes('table') %>%
    html_table()
  
  if(length(d) == 0) {
    
  } else if(length(d) > 1) {
    
    # find which table to choose by searching for the one generically displayed (will be stage standings)
    choose <- page %>%
      html_nodes('div') %>%
      html_attr(name = "class") %>%
      enframe(name = NULL) %>%
      filter(value %in% c("result-cont hide", "result-cont")) %>%
      tibble::rowid_to_column() %>%
      .[[1]] %>%
      .[[1]]
  
  } else {
    
    choose <- 1
    
  }
  
  #
  
  startlist <- d[[choose]]
  
  if(max((colnames(startlist) == "BIB") == TRUE) == 1) {
  
  startlist <- startlist %>%
    
    .[, 1:7] %>%
    
    select(rider = Rider,
           team = Team,
           bib = BIB) %>%
    unique() %>%
    
    mutate(race = all_races$race[[r]],
           year = all_races$year[[r]],
           url = all_races$url[[r]])
  
  startlist$race <- iconv(startlist$race, from="UTF-8", to = "ASCII//TRANSLIT")
  
  startlist$rider <- iconv(startlist$rider, from="UTF-8", to = "ASCII//TRANSLIT")
  
  startlist$team <- stringi::stri_trans_general(str = startlist$team, 
                                                id = "Latin-ASCII")
    
  startlist <- startlist %>%
    
    mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team)))
  
  #
  
  dbWriteTable(con, "pcs_all_startlists", startlist, row.names = FALSE, append = TRUE)
  
  print(r)
  
  }
  
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

# Pull in KMs breakaway ---------------------------------------------------


library(tidyverse)
library(lubridate)
library(rvest)
library(RMySQL)

dbDisconnect(con)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

all_races <- dbGetQuery(con, "SELECT DISTINCT race, year, class, date, url, stage, stage_name, one_day_race
                        FROM pcs_stage_data
                        WHERE year > 2019 AND time_trial = 0") %>%
  
  arrange(-year) %>%
  filter(class %in% c("WC", "1.UWT", "2.UWT") | (year == 2021 & !class %in% c("1.2", "2.2"))) %>%
  
  mutate(stage_name = str_sub(stage_name, 1, 8)) %>%
  
  anti_join(dbGetQuery(con, "SELECT race, year, stage FROM pcs_km_breakaway"), by = c("race", "year", "stage"))

#

for(r in 1:length(all_races$url)) {
  
  if(all_races$one_day_race[[r]] == 0) {
    
    page <- paste0("https://www.procyclingstats.com/", 
                   all_races$url[[r]], 
                   "/stage-", 
                   str_replace(all_races$stage_name[[r]], "Stage ", ""), 
                   "/today/kms-in-the-break")
    
  } else {
    
    page <- paste0("https://www.procyclingstats.com/", 
                   all_races$url[[r]], 
                   "/today/kms-in-the-break")
    
  }
  
  page <- page %>%
    read_html()
  
  # bring in the list of tables
  
  d <- page %>%
    html_nodes('table') %>%
    html_table()
  
  if(length(d) > 0) {
    
    breaks <- d %>%
      .[[1]]
    
    breaks <- breaks %>%
      
      janitor::clean_names() %>%
      
      select(-number) %>%
      
      mutate(race = all_races$race[[r]],
             year = all_races$year[[r]],
             stage = all_races$stage[[r]],
             url = all_races$url[[r]])
    
    breaks$race <- iconv(breaks$race, from="UTF-8", to = "ASCII//TRANSLIT")
    
    breaks$rider <- iconv(breaks$rider, from="UTF-8", to = "ASCII//TRANSLIT")
    
    #
    
    dbWriteTable(con, "pcs_km_breakaway", breaks, row.names = FALSE, append = TRUE)
    
    print(r)
    
    print(breaks)
    
  }
  
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


# Scrape GC rankings ------------------------------------------------------

# pull in directory of HTML downloads (about ~75 stages don't/can't download)
html_stage_dir <- fs::dir_ls("PCS-HTML/")

#html_stage_dir <- fs::dir_ls("PCS-HTML-GT/")

# download new stages (TRUE) or use old HTML (FALSE)
dl_html <- FALSE

# start scraping process using old data

if(dl_html == FALSE) {
  
  all_stages <- dbReadTable(con, "pcs_all_stages") %>%
    
    filter(!(url %in% c("race/60th-tour-de-picardie/2016"))) %>%
    
    anti_join(dbGetQuery(con, "SELECT DISTINCT stage as s, race as Race, year FROM pcs_stage_by_stage_gc"), by = c("s", "Race", "year")) %>%
    
    filter(year == 2021)
  
  all_stages <- all_stages %>%
    mutate(path = paste0("PCS-HTML-GT/", 
                         str_replace_all(str_replace(value, "https://www.procyclingstats.com/race/", ""), "/", ""))) %>%
    filter(path %in% html_stage_dir) %>%
    
    filter(!(value %in% c("race/giro-d-italia/2001/stage-18", "race/giro-d-italia/2018/stage-21",
                          "race/vuelta-a-espana/1991/stage-11", "race/vuelta-a-espana/1993/stage-12",
                          "race/vuelta-a-espana/2002/stage-13")))
  
}

#
# for loop for each stage
#

tictoc::tic()

for(r in 1:length(all_stages$value)) {
  
  f_name <- paste0("PCS-HTML/", str_replace_all(str_replace(all_stages$value[[r]], "https://www.procyclingstats.com/race/", ""), "/", ""))
  
  # match existence of f_name in stage directory
  if((f_name %in% html_stage_dir) & (str_sub(all_stages$Class[[r]],1,1)=="2")) {
    
    race_url <- all_stages$url[[r]]
    
    s = all_stages$s[[r]]
    
    url <- all_stages$value[[r]]
    
    # scrape the HTML for the page for multiple use
    
    f_name <- paste0("PCS-HTML/", str_replace_all(str_replace(all_stages$value[[r]], "https://www.procyclingstats.com/race/", ""), "/", ""))
    
    page <- read_file(f_name) %>%
      read_html()
    
    # bring in the list of tables
    
    d <- page %>%
      html_nodes('table') %>%
      html_table()
    
    if(length(d) == 0) {
      
      
    } else {
      
      res <- vector("list", length(d))
      
      for(n in 1:length(d)) {
        
        df <- d[[n]]
        
        cn <- colnames(df)
        
        ## if a column name is 'GC', write the data to the list
        ## this is because young rider also has GC
        if('TRUE' %in% str_detect("GC", cn)) {
          
          res[[n]] <- df %>%
            rowid_to_column() %>%
            mutate(rows = max(rowid)) %>%
            select(-rowid) %>%
            mutate(n = n) %>%
            select(n, rows) %>%
            unique()
          
        }
      }
      
      chooser <- bind_rows(res) %>%
        filter(max(rows)==rows)
      
      # sometimes if the first stage is a TTT (2014 Vuelta)
      # the stage page won't have GC column so just find the GC page and use that
      if(length(chooser)==0) {
        
        res <- vector("list", length(d))
        
        for(n in 1:length(d)) {
          
          df <- d[[n]]
          
          cn <- colnames(df)
          
          ## if a column name is 'GC', write the data to the list
          ## this is because young rider also has GC
          if('TRUE' %in% str_detect("Rider", cn)) {
            
            res[[n]] <- df %>%
              rowid_to_column() %>%
              mutate(rows = max(rowid)) %>%
              select(-rowid) %>%
              mutate(n = n) %>%
              select(n, rows) %>%
              unique()
            
          }
        }
        
        chooser <- bind_rows(res) %>%
          filter(max(rows)==rows)
        
        #
        
        stage_GC <- d[[chooser$n[[1]]]] %>%
          janitor::clean_names() %>%
          
          select(gc_rnk = rnk, gc_time = time, rider, team) %>%
          
          mutate(stage = s,
                 race = all_stages$Race[[r]],
                 year = all_stages$year[[r]],
                 date = all_stages$Date[[r]]) %>%
          
          mutate(gc_rnk = as.character(gc_rnk)) %>%
          
          filter(!is.na(gc_rnk)) %>%
          
          mutate(race = iconv(race, from="UTF-8", to = "ASCII//TRANSLIT"),
                 rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
          
          mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team)))
        
      } else {
        
        # now actually process the stage including getting times correct
        
        stage_GC <- d[[chooser$n[[1]]]] %>%
          janitor::clean_names() %>%
          
          select(gc_rnk = gc, gc_time = timelag, rider, team) %>%
          
          mutate(stage = s,
                 race = all_stages$Race[[r]],
                 year = all_stages$year[[r]],
                 date = all_stages$Date[[r]]) %>%
          
          mutate(gc_rnk = as.character(gc_rnk)) %>%
          
          filter(!is.na(gc_rnk)) %>%
          
          mutate(race = iconv(race, from="UTF-8", to = "ASCII//TRANSLIT"),
                 rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
          
          mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team)))
        
      }
      
    }
    
    dbWriteTable(con,
                 "pcs_stage_by_stage_gc",
                 stage_GC,
                 row.names = FALSE,
                 append = TRUE)
    
    print(str_sub(f_name, 10, nchar(f_name)))
    
  } else {
    
    race_url <- all_stages$url[[r]]
    
    s = all_stages$s[[r]]
    
    url <- all_stages$value[[r]]
    
    # scrape the HTML for the page for multiple use
    
    download.file(url, f_name, quiet = TRUE)
    
    f_name <- paste0("PCS-HTML/", str_replace_all(str_replace(all_stages$value[[r]], "https://www.procyclingstats.com/race/", ""), "/", ""))
    
    page <- read_file(f_name) %>%
      read_html()
    
    # bring in the list of tables
    
    d <- page %>%
      html_nodes('table') %>%
      html_table()
    
    if(length(d) == 0) {
      
      
    } else if('TRUE' %in% (page %>% html_nodes('div.res-right') %>% html_nodes('a') %>% html_text() %>% str_detect("TTT"))) {
      
      res <- vector("list", length(d))
      
      for(n in 1:length(d)) {
        
        df <- d[[n]]
        
        cn <- colnames(df)
        
        ## if a column name is 'GC', write the data to the list
        ## this is because young rider also has GC
        if('TRUE' %in% str_detect("Rider", cn)) {
          
          res[[n]] <- df %>%
            rowid_to_column() %>%
            mutate(rows = max(rowid)) %>%
            select(-rowid) %>%
            mutate(n = n) %>%
            select(n, rows) %>%
            unique()
          
        }
      }
      
      chooser <- bind_rows(res) %>%
        filter(max(rows)==rows)
      
      #
      
      stage_GC <- d[[chooser$n[[1]]]] %>%
        janitor::clean_names() %>%
        
        select(gc_rnk = rnk, gc_time = time, rider, team) %>%
        
        mutate(stage = s,
               race = all_stages$Race[[r]],
               year = all_stages$year[[r]],
               date = all_stages$Date[[r]]) %>%
        
        mutate(gc_rnk = as.character(gc_rnk)) %>%
        
        filter(!is.na(gc_rnk)) %>%
        
        mutate(race = iconv(race, from="UTF-8", to = "ASCII//TRANSLIT"),
               rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
        
        mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team)))
      
    } else {
      
      res <- vector("list", length(d))
      
      for(n in 1:length(d)) {
        
        df <- d[[n]]
        
        cn <- colnames(df)
        
        ## if a column name is 'GC', write the data to the list
        ## this is because young rider also has GC
        if('TRUE' %in% str_detect("GC", cn)) {
          
          res[[n]] <- df %>%
            rowid_to_column() %>%
            mutate(rows = max(rowid)) %>%
            select(-rowid) %>%
            mutate(n = n) %>%
            select(n, rows) %>%
            unique()
          
        }
      }
      
      chooser <- bind_rows(res) %>%
        filter(max(rows)==rows)
      
      # sometimes if the first stage is a TTT (2014 Vuelta)
      # the stage page won't have GC column so just find the GC page and use that
      if(length(chooser)==0) {
        
        res <- vector("list", length(d))
        
        for(n in 1:length(d)) {
          
          df <- d[[n]]
          
          cn <- colnames(df)
          
          ## if a column name is 'GC', write the data to the list
          ## this is because young rider also has GC
          if('TRUE' %in% str_detect("Rider", cn)) {
            
            res[[n]] <- df %>%
              rowid_to_column() %>%
              mutate(rows = max(rowid)) %>%
              select(-rowid) %>%
              mutate(n = n) %>%
              select(n, rows) %>%
              unique()
            
          }
        }
        
        chooser <- bind_rows(res) %>%
          filter(max(rows)==rows)
        
        #
        
        stage_GC <- d[[chooser$n[[1]]]] %>%
          janitor::clean_names() %>%
          
          select(gc_rnk = rnk, gc_time = time, rider, team) %>%
          
          mutate(stage = s,
                 race = all_stages$Race[[r]],
                 year = all_stages$year[[r]],
                 date = all_stages$Date[[r]]) %>%
          
          mutate(gc_rnk = as.character(gc_rnk)) %>%
          
          filter(!is.na(gc_rnk)) %>%
          
          mutate(race = iconv(race, from="UTF-8", to = "ASCII//TRANSLIT"),
                 rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
          
          mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team)))
        
      } else {
        
        # now actually process the stage including getting times correct
        
        stage_GC <- d[[chooser$n[[1]]]] %>%
          janitor::clean_names() %>%
          
          select(gc_rnk = gc, gc_time, rider, team) %>%
          
          mutate(stage = s,
                 race = all_stages$Race[[r]],
                 year = all_stages$year[[r]],
                 date = all_stages$Date[[r]]) %>%
          
          mutate(gc_rnk = as.character(gc_rnk)) %>%
          
          filter(!is.na(gc_rnk)) %>%
          
          mutate(race = iconv(race, from="UTF-8", to = "ASCII//TRANSLIT"),
                 rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
          
          mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team)))
        
      }
      
    }
    
    dbWriteTable(con,
                 "pcs_stage_by_stage_gc",
                 stage_GC,
                 row.names = FALSE,
                 append = TRUE)
    
    print(str_sub(f_name, 10, nchar(f_name)))
    
  }
}

tictoc::toc()

#
#
#

stage_by_stage_GC <- dbReadTable(con, "pcs_stage_by_stage_gc") %>%
  
  mutate(rider = str_to_title(tolower(rider)),
         gc_time = str_replace(gc_time, "//+", ""),
         gc_time = str_replace(gc_time, ",,", ""),
         old_gc_time = gc_time,
         gcn = nchar(gc_time)) %>% 
  
  mutate(duplic = ifelse(str_sub(gc_time, 1, floor(gcn/2)) == str_sub(gc_time, ceiling(gcn/2)+1, gcn), TRUE, FALSE),
         gc_time = ifelse(duplic == TRUE, str_sub(gc_time, 1, floor(gcn/2)), gc_time)) %>%
  
  separate(gc_time, into = c("hours","minutes", "seconds"), sep = ":") %>%
  mutate(h = is.na(seconds),
         seconds = ifelse(h == TRUE, minutes, seconds),
         minutes = ifelse(h == TRUE, hours, minutes),
         hours = ifelse(h == TRUE, 0, hours),
         seconds = as.numeric(seconds),
         minutes = as.numeric(minutes),
         hours = as.numeric(hours),
         total_seconds_back = (seconds + (minutes*60) + (hours*3600)),
         total_seconds_back = ifelse(gc_rnk == "1", 0, total_seconds_back)) %>%
  
  filter(!gc_rnk == "") %>%
  
  mutate(gc_rnk = as.numeric(gc_rnk)) %>%
  
  filter(!is.na(gc_rnk)) %>%
  
  group_by(race, year) %>%
  mutate(final = ifelse(max(stage, na.rm = T)==stage, gc_rnk, NA),
         stages_left = max(stage, na.rm = T)-stage) %>%
  ungroup() %>%
  
  group_by(rider, race, year) %>%
  mutate(final = mean(final, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(race, year) %>%
  mutate(final = ifelse(is.na(final) | final == "NaN", max(final, na.rm = T)+1, final)) %>%
  ungroup()

#
# just knowing stages left and gc rank, what is win probability?
# gc leader with 1 stage left is ~34%
#

basic_win_glm <- glm(win ~ stages_left * log(gc_rnk+1),
                        family = "binomial",
                        data = stage_by_stage_GC %>%
                          mutate(win = ifelse(final == 1, 1, 0)) %>%
                          filter(stages_left > 0))

#

basic_pred <- cbind(
  
  stage_by_stage_GC %>%
  mutate(win = ifelse(final < 4, 1, 0)) %>%
  filter(stages_left > 0),
  
  pred = predict(basic_win_glm, stage_by_stage_GC %>%
                   mutate(win = ifelse(final < 4, 1, 0)) %>%
                   filter(stages_left > 0))) %>%
  
  mutate(pred = exp(pred) / (1+exp(pred))) %>%
  
  group_by(stage, race, year) %>%
  mutate(pred = pred / sum(pred, na.rm = T)) %>%
  ungroup()

#

time_win_glm <- glm(win ~ stages_left * log(gc_rnk+1),
                     family = "binomial",
                     data = stage_by_stage_GC %>%
                       mutate(win = ifelse(final == 1, 1, 0)) %>%
                       filter(stages_left > 0))

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


# Scrape PCS Rankings -----------------------------------------------------

race_data <- dbReadTable(con, "pcs_all_races") %>%
  mutate(Race = iconv(Race, from="UTF-8", to = "ASCII//TRANSLIT"),
         Circuit = str_sub(type, 53, nchar(type))) %>%
  separate(Circuit, c("Circuit", "delete"), sep = "&") %>%
  mutate(Circuit = str_replace(Circuit, "circuit=", "")) %>%
  
  inner_join(
    
    tibble(Tour = c("Missing", "Americas Tour", "Europe Tour", "World Championships", "World Tour", "Asia Tour",
                    "Oceania Tour", "Africa Tour", "Olympic Games"),
           Circuit = c("", "18", "13", "2", "1", "12", "14", "11", "3")), by = c("Circuit")
    
  ) %>%
  
  mutate(Tour = ifelse(str_detect(Race, "National Champ"), "National Championship", Tour)) %>%
  
  select(Race, Tour, URL = url, Year = year) %>%
  
  filter(Year > 2013) %>%
  
  filter(!(str_detect(URL, "race/81st-schaal-schels/"))) %>% 
  filter(!(str_detect(URL, "race/heistse-pijl/"))) %>%
  filter(!(str_detect(URL, "race/bruges-cycling-classic/"))) %>%
  filter(!(str_detect(URL, "race/grote-prijs-jean-pierre-monsere/2019")))

#
#
#

html_stage_dir <- fs::dir_ls("PCS-HTML/")

#

tictoc::tic()

for(r in 1:length(race_data$URL)) {
  
  f_name <- paste0("PCS-HTML/", str_replace_all(str_replace(race_data$URL[[r]], "https://www.procyclingstats.com/race/", ""), "/", ""), "pcs-ranks")
  
  # match existence of f_name in stage directory
  if(f_name %in% html_stage_dir) {
    
    race_url <- paste0("https://www.procyclingstats.com/race/", race_data$URL[[r]], "/startlist/riders-ranked")
    
    # no need to download, proceed to clean and store
    
    f_name <- paste0("PCS-HTML/", str_replace_all(str_replace(race_data$URL[[r]], "https://www.procyclingstats.com/race/", ""), "/", ""), "pcs-ranks")
    
    page <- read_file(f_name) %>%
      read_html()
    
    # bring in the list of tables
    
    d <- page %>%
      html_nodes('table') %>%
      html_table()
    
    sprint_ranks_race <- d[[1]] %>%
      janitor::clean_names() %>%
      
      select(rider,
             points) %>%
      
      mutate(race = race_data$Race[[r]],
             year = race_data$Year[[r]],
             url = race_data$URL[[r]]) %>%
      
      mutate(race = iconv(race, from="UTF-8", to = "ASCII//TRANSLIT"),
             rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT"))
    
    #
    
    dbWriteTable(con,
                 "pcs_ranks_race",
                 sprint_ranks_race,
                 row.names = FALSE,
                 append = TRUE)
    
    print(str_sub(f_name, 10, nchar(f_name)))
    
  } else {
    
    # if the file is not download, download and do everything the same
    
    race_url <- paste0("https://www.procyclingstats.com/race/", race_data$URL[[r]], "/startlist/riders-ranked")
    
    # no need to download, proceed to clean and store
    
    download.file(race_url, f_name, quiet = TRUE)
    
    f_name <- paste0("PCS-HTML/", str_replace_all(str_replace(race_data$URL[[r]], "https://www.procyclingstats.com/race/", ""), "/", ""), "pcs-ranks")

    page <- read_file(f_name) %>%
      read_html()
    
    # bring in the list of tables
    
    d <- page %>%
      html_nodes('table') %>%
      html_table()
    
    sprint_ranks_race <- d[[1]] %>%
      janitor::clean_names() %>%
      
      select(rider,
             points) %>%
      
      mutate(race = race_data$Race[[r]],
             year = race_data$Year[[r]],
             url = race_data$URL[[r]]) %>%
      
      mutate(race = iconv(race, from="UTF-8", to = "ASCII//TRANSLIT"),
             rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT"))
    
    #
    
    dbWriteTable(con,
                 "pcs_ranks_race",
                 sprint_ranks_race,
                 row.names = FALSE,
                 append = TRUE)
    
    print(str_sub(f_name, 10, nchar(f_name)))
    
  }
  
  Sys.sleep(runif(1,1,5))
  
}

tictoc::toc()

#
#
#
#
#
#
#
#
#


all_stages <- dbReadTable(con, "pcs_all_stages") %>%
  
  filter(!(url %in% c("race/60th-tour-de-picardie/2016"))) %>%
  
  filter(Date > as.Date('2017-01-01')) %>%
  
  group_by(Race, url, year) %>%
  filter(s == max(s, na.rm = T)) %>%
  filter(max(s)>3 & max(s)<8) %>%
  ungroup() %>%
  
  filter(Class %in% c("2.UWT", "2.HC", "2.1"))

#

tictoc::tic()

for(r in 1:length(all_stages$value)) {
  
  f_name <- paste0("PCS-HTML/", str_replace_all(str_replace(all_stages$value[[r]], "https://www.procyclingstats.com/race/", ""), "/", ""))
  
  # match existence of f_name in stage directory
  if((f_name %in% html_stage_dir) & (str_sub(all_stages$Class[[r]],1,1)=="2")) {
    
    race_url <- all_stages$url[[r]]
    
    s = all_stages$s[[r]]
    
    url <- all_stages$value[[r]]
    
    # scrape the HTML for the page for multiple use
    
    f_name <- paste0("PCS-HTML/", str_replace_all(str_replace(all_stages$value[[r]], "https://www.procyclingstats.com/race/", ""), "/", ""))
    
    page <- read_file(f_name) %>%
      read_html()
    
    # bring in the list of tables
    
    d <- page %>%
      html_nodes('table') %>%
      html_table()
    
    tabs <- page %>%
      html_nodes('ul.restabs') %>%
      html_nodes('a') %>%
      html_text()
    
    if(length(d) == 0) {
      
      
    } else {

      KOM = tabs %>%
        enframe(name = NULL) %>%
        rownames_to_column() %>%
        filter(value == "KOM") %>%
        .[1,1] %>%
        .[[1]] %>%
        as.numeric()
      
      PTS = tabs %>%
        enframe(name = NULL) %>%
        rownames_to_column() %>%
        filter(value == "Points") %>%
        .[1,1] %>%
        .[[1]] %>%
        as.numeric()
      
      TM = tabs %>%
        enframe(name = NULL) %>%
        rownames_to_column() %>%
        filter(value == "Teams") %>%
        .[1,1] %>%
        .[[1]] %>%
        as.numeric()
      
      YTH = tabs %>%
        enframe(name = NULL) %>%
        rownames_to_column() %>%
        filter(value == "Youth") %>%
        .[1,1] %>%
        .[[1]] %>%
        as.numeric()
      
      GC = tabs %>%
        enframe(name = NULL) %>%
        rownames_to_column() %>%
        filter(value == "GC") %>%
        .[1,1] %>%
        .[[1]] %>%
        as.numeric()
      
      # teams
      
      # team <- bind_rows(d[[TM]]) %>%
      #   
      #   rename(rnk = Rnk,
      #          team = Team) %>%
      #   mutate(Type = "Team") %>%
      #   
      #   mutate(stage = s,
      #          race = all_stages$Race[[r]],
      #          year = all_stages$year[[r]],
      #          date = all_stages$Date[[r]]) %>%
      #   
      #   mutate(race = iconv(race, from="UTF-8", to = "ASCII//TRANSLIT"),
      #          rider = as.numeric(NA),
      #          points = as.numeric(NA)) %>%
      #   
      #   select(rider, rnk, team, Type, stage, race, year, date, points)
      # 
      # dbWriteTable(con, "pcs_jerseys_final", team, row.names = F, append = TRUE)
      # 
      # GC
      
      gc <- bind_rows(d[[GC]]) %>%

        rename(rnk = Rnk,
               rider = Rider,
               team = Team) %>%
        mutate(Type = "GC") %>%
        
        mutate(stage = s,
               race = all_stages$Race[[r]],
               year = all_stages$year[[r]],
               date = all_stages$Date[[r]]) %>%
        
        mutate(race = iconv(race, from="UTF-8", to = "ASCII//TRANSLIT"),
               rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
        
        mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team)),
               points = as.numeric(NA)) %>%
        
        select(rider, rnk, team, Type, stage, race, year, date, points)
      
      dbWriteTable(con, "pcs_jerseys_final", gc, row.names = F, append = TRUE)
      # 
      # # mountains
      # 
      # mountains <- bind_rows(d[[KOM]]) %>%
      #   
      #   mutate(PntCount = sum(!is.na(Pnt)),
      #          TodayCount = sum(!is.na(Today))) %>%
      #   
      #   mutate(points = ifelse(PntCount >= TodayCount, Pnt, Today)) %>%
      #   
      #   rename(rnk = Rnk,
      #          rider = Rider,
      #          team = Team) %>%
      #   mutate(Type = "Mountains") %>%
      #   
      #   mutate(stage = s,
      #          race = all_stages$Race[[r]],
      #          year = all_stages$year[[r]],
      #          date = all_stages$Date[[r]]) %>%
      #   
      #   mutate(race = iconv(race, from="UTF-8", to = "ASCII//TRANSLIT"),
      #          rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
      #   
      #   mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team))) %>%
      #   
      #   select(rider, rnk, team, Type, stage, race, year, date, points)
      # 
      # dbWriteTable(con, "pcs_jerseys_final", mountains, row.names = F, append = TRUE)
      # 
      # # points
      # 
      # points <- bind_rows(d[[PTS]]) %>%
      #   
      #   mutate(PntCount = sum(!is.na(Pnt)),
      #          TodayCount = sum(!is.na(Today))) %>%
      #   
      #   mutate(points = ifelse(PntCount >= TodayCount, Pnt, Today)) %>%
      #   
      #   rename(rnk = Rnk,
      #          rider = Rider,
      #          team = Team) %>%
      #   mutate(Type = "Points") %>%
      #   
      #   mutate(stage = s,
      #          race = all_stages$Race[[r]],
      #          year = all_stages$year[[r]],
      #          date = all_stages$Date[[r]]) %>%
      #   
      #   mutate(race = iconv(race, from="UTF-8", to = "ASCII//TRANSLIT"),
      #          rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
      #   
      #   mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team))) %>%
      #   
      #   select(rider, rnk, team, Type, stage, race, year, date, points)
      # 
      # dbWriteTable(con, "pcs_jerseys_final", points, row.names = F, append = TRUE)
      
    }
    
  }
  
  print(paste0(all_stages$Race[[r]], all_stages$year[[r]]))
  
}

tictoc::toc()
