

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

all_events <- tibble(race = "Tour de France",
                     year = seq(1989, 2018, 1)) %>%
  mutate(url = paste0('race/tour-de-france/', year, '/'))

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
  
  Sys.sleep(runif(1, 5, 15))
  
}

tictoc::toc()

#
#
#

all_stages <- all_events %>%
  
  inner_join(
    
    bind_rows(stages_list), by = c("url")
    
  ) %>%
  
  # some stages have stage #a and #b, ignore them
  filter(!is.na(MAX)) %>%
  
  unique()

# prologues

pro_list <- c(1989:1999, 2001, 2002, 2003,
              2004, 2006, 2007, 2010, 2012)

for(p in 1:19) {
  
  url <- paste0('https://www.procyclingstats.com/race/tour-de-france/', pro_list[[p]], '/prologue')
  
  pro_list[[p]] <- url
   
}

# Stage Data for each Race ------------------------------------------------

# at current 2013-19 levels it takes 80 minutes to import the data
# but we've doubled the data-set

races_list <- vector("list", 1000)

stage_type_list <- vector("list", length(all_stages$url))

#

tictoc::tic()

#

for(r in 1:length(all_stages$url)) {
  
  race_url <- all_stages$url[[r]]
  
  stage_race_list <- vector("list", all_stages$MAX[[r]])
  
  stage_types <- paste0('https://www.procyclingstats.com/', all_events$url[[r]], '/overview') %>%
    read_html() %>%
    html_nodes('span') %>%
    html_attr(name = "class") %>%
    enframe(name = NULL) %>% 
    filter(str_detect(value, "icon profile")) %>%
    rownames_to_column() %>%
    rename(stage = rowname,
           stage_type = value)
  
  stage_type_list[[r]] <- stage_types
  
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
        
        if(s == 1 & race_url == "race/tour-de-france/2016/") {
          
          choose = 2
          
        }
        
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
            start_end = NA
            
          } else if(race_url == "race/tour-de-france/2003/" & s == 1) {
            
            length = 168
            start_end = 'Saint-Denis-Montgeron - Meaux'
            
          } else if(race_url == "race/tour-de-france/2003/" & s == 13) {
            
            length = 197.5
            start_end = 'Toulouse - Ax 3 Domaines'
            
          } else if(race_url == "race/tour-de-france/2003/" & s == 15) {
            
            length = 159.5
            start_end = 'Bagnères-de-Bigorre - Luz-Ardiden'
            
          } else if(race_url == "race/tour-de-france/2006/" & s == 15) {
            
            length = 187
            start_end = "Gap - Alpe d Huez"
            
          } else {
            
            start_end = length %>% .[[1]] %>%
              str_trim()
            
            length <- length %>% .[[2]] %>%
              str_replace_all("k","") %>%
              as_tibble() %>%
              mutate(value = ifelse(nchar(value) == 5, str_sub(value, 2,4),
                                    ifelse(nchar(value) == 6, str_sub(value, 2,5), str_sub(value, 2,6)))) %>%
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
                mutate(Pnt = {if("Pnt" %in% names(.)) Pnt else 0}) %>%
                mutate(Pnt = ifelse(is.na(Pnt), lag(Pnt), Pnt)) %>%
                mutate(lagPnt = ifelse(is.na(lag(Pnt)), Pnt, lag(Pnt))) %>%
                filter(!(Pnt > lagPnt)) %>%
                janitor::clean_names() %>%
                select(Rider = rider, Pnt = pnt)
              
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
            janitor::clean_names() %>%
            
            # this processes time
            mutate(time = ifelse(time == ",,,,", NA, time),
                   time = ifelse(time == "--", NA, time)) %>%
            mutate(time = str_replace_all(time, ",", "")) %>%
            
            mutate(time = ifelse(time %in% c("0:00", "0:000:00"), NA, time)) %>%
            
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
            
            mutate(total_seconds = ifelse(total_seconds == 0, (lag(total_seconds) + lead(total_seconds)) / 2, time_seconds)) %>%
            
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
            
            mutate(kom_all = 0,
                   cities = start_end)
          
          # nest actual KOM standings
          
          stage$kom_all[[1]] <- kom_all %>%
            nest()
          
        }
        
      }
      
    }
    
    # write to the stage list for this race
    stage_race_list[[s]] <- stage
    
    Sys.sleep(runif(1,1,3))
    
  }
  
  # write to the race list for this race
  races_list[[r]] <- bind_rows(stage_race_list)
  
  print(race_url)
  
  Sys.sleep(runif(1, 1, 5))
  
}

tictoc::toc()

#
# scrape prologues
#

prologue_data_list <- vector("list", length(pro_list))

for(s in 1:length(pro_list)) {
    
    # scrape the HTML for the page for multiple use
    
    page <- pro_list[[s]] %>%
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
      
      if(s == 1 & race_url == "race/tour-de-france/2016/") {
        
        choose = 2
        
      }
      
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
                 race = NA,
                 year = NA,
                 date = NA)
        
        print(paste0("error in stage", s, "race", race_url))
        
        # otherwise continue with processing
        
      } else {
        
        # bring in stage distance again
        
        length <- page %>%
          html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "red", " " ))]') %>% 
          html_text() 
        
        if(identical(length, character(0)) == TRUE) {
          
          length = NA
          start_end = NA
          
        } else if(race_url == "race/tour-de-france/2003/" & s == 1) {
          
          length = 168
          start_end = 'Saint-Denis-Montgeron - Meaux'
          
        } else if(race_url == "race/tour-de-france/2003/" & s == 13) {
          
          length = 197.5
          start_end = 'Toulouse - Ax 3 Domaines'
          
        } else if(race_url == "race/tour-de-france/2003/" & s == 15) {
          
          length = 159.5
          start_end = 'Bagnères-de-Bigorre - Luz-Ardiden'
          
        } else if(race_url == "race/tour-de-france/2006/" & s == 15) {
          
          length = 187
          start_end = "Gap - Alpe d Huez"
          
        } else {
          
          start_end = length %>% .[[1]] %>%
            str_trim()
          
          length <- length %>% .[[2]] %>%
            str_replace_all("k","") %>%
            as_tibble() %>%
            mutate(value = ifelse(nchar(value) == 5, str_sub(value, 2,4),
                                  ifelse(nchar(value) == 6, str_sub(value, 2,5), str_sub(value, 2,6)))) %>%
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
              mutate(Pnt = {if("Pnt" %in% names(.)) Pnt else 0}) %>%
              mutate(Pnt = ifelse(is.na(Pnt), lag(Pnt), Pnt)) %>%
              mutate(lagPnt = ifelse(is.na(lag(Pnt)), Pnt, lag(Pnt))) %>%
              filter(!(Pnt > lagPnt)) %>%
              janitor::clean_names() %>%
              select(Rider = rider, Pnt = pnt)
            
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
          janitor::clean_names() %>%
          
          # this processes time
          mutate(time = ifelse(time == ",,,,", NA, time),
                 time = ifelse(time == "--", NA, time)) %>%
          mutate(time = str_replace_all(time, ",", "")) %>%
          
          mutate(time = ifelse(time %in% c("0:00", "0:000:00"), NA, time)) %>%
          
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
          
          mutate(total_seconds = ifelse(total_seconds == 0, (lag(total_seconds) + lead(total_seconds)) / 2, time_seconds)) %>%
          
          select(rnk, rider, team, win_seconds, total_seconds) %>%
          
          mutate(length = length,
                 distance = distance,
                 stage_name = stage_name,
                 kom_progression = kom) %>%
          
          mutate(stage = "Prologue",
                 race = "Tour de France",
                 year = str_sub(str_replace(pro_list[[s]], "https://www.procyclingstats.com/race/tour-de-france/", ""), 1, 4),
                 date = NA) %>%
          
          mutate(rnk = as.character(rnk)) %>%
          
          mutate(kom_all = 0,
                 cities = start_end)
        
        # nest actual KOM standings
        
        stage$kom_all[[1]] <- kom_all %>%
          nest()
        
      }
    }
    
    prologue_data_list[[s]] <- stage
    
}
  

#
#
#
#

stage_qualities <- read_csv("stage-types-89-18.csv") %>% 
  filter(Type == "Stage") %>% 
  gather(year, stage_type, x1989:x2019) %>%
  mutate(year = as.numeric(str_replace(year, "x", ""))) %>%
  filter(!is.na(stage_type))

stage_kom <- read_csv("stage-types-89-18.csv") %>% 
  filter(Type == "KOM") %>% 
  gather(year, kom_points, x1989:x2019) %>%
  mutate(year = as.numeric(str_replace(year, "x", "")),
         kom_points = as.numeric(kom_points)) %>%
  filter(!is.na(kom_points))

#

stage_data <- bind_rows(races_list) %>% 
  
  select(-speed) %>%
  select(-time_seconds, -kom_progression, -kom_all) %>%
  
  mutate(race = "Tour de France") %>%
  
  rbind(
    
    bind_rows(prologue_data_list) %>%
      select(-date, -kom_all, -kom_progression) %>%
      mutate(year = as.numeric(year),
             stage = 0)
    
  )
  
#

stage_data$race <- iconv(stage_data$race, from="UTF-8", to = "ASCII//TRANSLIT")
stage_data$rider <- iconv(stage_data$rider, from="UTF-8", to = "ASCII//TRANSLIT")

stage_data <- stage_data %>%
  
  mutate(distance = str_replace(distance, "\\(", ""),
         distance = str_replace(distance, "\\)", ""),
         distance = as.numeric(str_replace(distance, "k", ""))) %>%
  
  mutate(length = ifelse(is.na(length), distance, length)) %>%
  
  # Vino stripped St 13 2007
  mutate(win_seconds = ifelse(stage == 13 & year == 2007, 3994, win_seconds),
         total_seconds = ifelse(stage == 13 & year == 2007 & rider == "Vinokourov Alexandre", 0, total_seconds),
         total_seconds = ifelse(stage == 13 & year == 2007 & rider == "Evans CadelPredictor - Lotto", 74, total_seconds)) %>%
  
  # Landis stripped ST 17 2006
  mutate(win_seconds = ifelse(stage == 17 & year == 2006, 19416, win_seconds),
         total_seconds = ifelse(stage == 17 & year == 2006 & rider == "Landis Floyd", 0, total_seconds),
         total_seconds = ifelse(stage == 17 & year == 2006 & rider == "Sastre CarlosCSC ProTeam", 342, total_seconds)) %>%
  
  mutate(total_seconds = total_seconds + win_seconds) %>%
  
  mutate(team = ifelse(team == "", NA, team)) %>%
  
  mutate(rider = ifelse(is.na(team), rider, str_replace(rider, team, ""))) %>%
  mutate(finished = ifelse(rnk %in% c("DNF", "OTL", "DNS", "NQ", "DSQ"), NA, total_seconds)) %>%
  mutate(total_seconds = ifelse(total_seconds > 30000, NA, total_seconds)) %>%
  
  group_by(stage, race, year) %>%
  mutate(last_place = max(total_seconds, na.rm = T)) %>%
  ungroup() %>%
  
  filter(!rnk %in% c("DNS", "DSQ")) %>%
  
  mutate(total_seconds = ifelse(rnk %in% c("DNF", "OTL", "NQ"), last_place, total_seconds)) %>%
  
  mutate(total_seconds = ifelse(is.na(total_seconds), last_place, total_seconds)) %>%
  
  select(-last_place, -finished) %>%
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
  
  mutate(time_trial = ifelse(stage_name %in% c("Time trial", "Prologue") | str_detect(stage_name, "ITT"), TRUE, FALSE)) %>%
  
  left_join(
    
    stage_kom %>%
      select(-Type), by = c("year", "stage" = "Stage")
  ) %>%
  
  left_join(
    
    stage_qualities %>%
      select(-Type), by = c("year", "stage" = "Stage")
  ) %>%
  
  select(-variance_valid) %>%
  
  group_by(stage, year) %>%
  mutate(rnk = rank(total_seconds, ties.method = "first")) %>%
  ungroup()

#
#
# Race GC
#
#

race_gc_list <- vector("list", length(gc_list))

for(g in 1:length(gc_list)) {
  
  d <- bind_rows(gc_list[[g]][[2]]) %>%
    
    mutate(Rider = str_replace(Rider, Team, "")) %>%
    
    select(gc = Rnk, gc_rider = Rider) %>%
    mutate(gc = as.numeric(gc)) %>%
    mutate(year = 1988 + g) %>%
    
    mutate(gc = rank(gc, ties.method = "first"))
  
  race_gc_list[[g]] <- d
  
}

#

race_gc_data <- bind_rows(race_gc_list)

race_gc_data$gc_rider <- iconv(race_gc_data$gc_rider, from="UTF-8", to = "ASCII//TRANSLIT")

#
#
#

stage_analysis_data <- stage_data %>%
  
  left_join(
    
    race_gc_data %>%
      filter(gc < 6), by = c("rider" = "gc_rider", "year")
    
  ) %>%
  
  mutate(gc_winner = ifelse(gc == 1, total_seconds, NA),
         gc_top_5 = ifelse(gc < 6, total_seconds, NA)) %>%
  
  group_by(stage, year) %>%
  mutate(gc_winner = mean(gc_winner, na.rm = T),
         gc_top_5 = median(gc_top_5, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(rel_gc = total_seconds - gc_winner,
         rel_t5 = total_seconds - gc_top_5)