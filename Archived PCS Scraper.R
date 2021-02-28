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
      
      evts <- page %>%
        html_nodes('table') %>%
        html_table(dec = ",") %>%
        .[[1]] %>%
        .[, c(1, 3:5)] %>%
        filter(Winner != "") %>%
        count() %>%
        as.list() %>%
        .[[1]]
      
      if(evts == 0) {
        
        
      } else {
      
      events <- cbind(
        
       page %>%
          html_nodes('table') %>%
          html_table(dec = ",") %>%
          .[[1]] %>%
         .[, c(1, 3:5)] %>%
          filter(Winner != ""),
        
        page %>%
         html_nodes('table') %>%
          html_nodes('a') %>%
          html_attr(name = "href") %>%
          enframe(name = NULL) %>%
          #.[-(1:99),] %>%
          filter(str_detect(value, "race/")) %>%
          filter(str_detect(value, as.character(year))) %>%
          filter(!str_detect(value, "stage-")) %>%
          filter(!str_detect(value, "result")) %>%
          filter(!(str_detect(value, "2020/"))) %>%
          unique() %>%
          .[1:evts,]
        
      ) %>%
        
        mutate(year = year) %>%
        rename(url = value)
      
      year_list[[y]] <- events
      
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

  #mutate(type = str_sub(type, 61, nchar(type)),
  #       type = str_replace(type, '&filter=Filter', "")) %>%
  
  mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2))))) %>%
  
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
  
  select(-spec_url) 

#
# Write DB Table
#

all_events$Race <- iconv(all_events$Race, from="UTF-8", to = "ASCII//TRANSLIT")
all_events$Winner <- iconv(all_events$Winner, from="UTF-8", to = "ASCII//TRANSLIT")

new_events <- all_events %>%
  anti_join(
    
    dbReadTable(con, "pcs_all_races") %>%
      filter(type != "scraper"), by = c("url")
    
  )

all_events <- new_events

dbWriteTable(con, "pcs_all_races", new_events, row.names = FALSE, append = TRUE)

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
    html_nodes('body > div.wrapper > div.content > div.statDivLeft > ul.list.table') %>%
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
  
  if(is.null(which_is_gc)) {
    
    gc_winner = tibble(value = "One Day Race")
    
  } else {
  
  gc_winner <- page %>% 
    html_nodes('div.resultCont') %>%
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

dbWriteTable(con, "pcs_gc_winners", gc_winners, append = TRUE, row.names = FALSE)

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
    
    filter(year >= 2013)
  
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
        filter(value %in% c("resultCont hide", "resultCont ")) %>%
        tibble::rowid_to_column() %>%
        filter(value == "resultCont ") %>%
        .[[1]]
      
      # bring in stage characteristics
      characteristics <- page %>%
        html_nodes('h2') %>%
        html_nodes('span') %>%
        html_text()
      
      # Characteristics start
      if(length(characteristics) == 4) {
        
        distance = characteristics[[4]]
        stage_name = characteristics[[2]]
        
      } else {
        
        distance = NA
        stage_name = "one day race"
        
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
          html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "red", " " ))]') %>% 
          html_text() 
        
        if(identical(length, character(0)) == TRUE) {
          
          length = NA
          
        } else if(length(length) == 1) {
          
          length = 150 
          
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
        
        # in case the winner's time is missing (eg, their stupid doping shit with Armstrong) bring in speed
        
        spd <- page %>% html_nodes('div.res-right') %>% html_text()
        
        x <- str_locate(spd, 'Avg. speed winner:') %>% .[[2]]
        
        SPD_WIN <- readr::parse_number(str_sub(spd, x+1, x+7))
        
        # bring in parcours type information
        
        parcours <- page %>%
          html_nodes('span.icon') %>%
          html_attr(name = "class") %>%
          enframe(name = NULL) %>%
          filter(str_detect(value, "profile")) %>%
          str_trim() %>%
          as.list() %>%
          .[[1]]
        
        profile_value <- page %>%
          html_nodes('div.res-right') %>%
          html_nodes('a') %>%
          html_text() %>%
          enframe(name = NULL) %>%
          as.list() %>%
          .[[1]] %>%
          .[[1]]
        
        if(length(parcours) == 0) {
          
          parcours = NA
          
        }
        
        if(length(profile_value) == 0) {
          
          profile_value = NA
          
        }
        
        # bring in actual date
        
        DATE <- page %>%
          html_nodes('div.res-right') %>%
          html_text() %>%
          str_replace('Race informationDate: ', '') %>%
          str_replace('Avg.', "-") %>%
          enframe(name = NULL) %>%
          separate(value, into = c("date", "junk"), sep = "-") %>%
          select(date) %>%
          mutate(date = lubridate::dmy(str_trim(date))) %>%
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