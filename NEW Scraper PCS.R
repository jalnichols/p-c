

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
  'https://www.procyclingstats.com/races.php?year=2019&circuit=1&class=1.UWT&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=1&class=2.UWT&filter=Filter',
  
  # EUROPE
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=2.1&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=1.1&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=2.HC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=1.HC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=2.Pro&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=13&class=1.Pro&filter=Filter',
  
  # ASIA
  'https://www.procyclingstats.com/races.php?year=2019&circuit=12&class=1.HC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=12&class=2.HC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=12&class=1.Pro&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=12&class=2.Pro&filter=Filter',
  
  # AMERICAS
  'https://www.procyclingstats.com/races.php?year=2019&circuit=18&class=2.1&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=18&class=2.HC&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=18&class=1.Pro&filter=Filter',
  
  'https://www.procyclingstats.com/races.php?year=2019&circuit=18&class=2.Pro&filter=Filter',
  
  # WORLD CHAMPS
  'https://www.procyclingstats.com/races.php?year=2019&circuit=2&class=&filter=Filter')

#

store_from_schedule <- vector("list", length(pull_from_schedule))

pull_years = 7

current_year = 2020

#
# pull in each type and then each year
#

for(t in 1:length(pull_from_schedule)) {
  
  year_list <- vector("list", pull_years)
  
  # pull in each year
  for(y in 1:length(year_list)) {
    
    year = 2012 + y
    
    url <- paste0(
      str_sub(pull_from_schedule[[t]], 1, 47),
      year,
      str_sub(pull_from_schedule[[t]], 52, nchar(pull_from_schedule[[t]])))
    
    # if it's current year handle differently
    
    if(year == current_year) {
      
      page <- url %>%
        read_html()
      
      events <- cbind(
        
       page %>%
          html_nodes('table') %>%
          html_table(dec = ",") %>%
          .[[1]] %>%
          filter(Winner != ""),
        
        page %>%
         html_nodes('table') %>%
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
        rename(url = value)
      
      
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
      
    }
    
    year_list[[y]] <- events
    
  }
  
  store_from_schedule[[t]] <- bind_rows(year_list) %>%
    mutate(type = pull_from_schedule[[t]])
  
}

#
# str_detect(Race, "WE") | str_detect(Race, "WJ") | str_detect(Race, "TTT") | str_detect(Race, "U23") | str_detect(Race, "MJ") | str_detect(Race, "ITT")
#

all_events <- bind_rows(store_from_schedule) %>%
  
  mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2))))) %>%
  
  # manually add WC ITT and the Dubai Tour when it was 2.1
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
      mutate(type = "Manual") %>%
      mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2)))))) %>%
  
  rbind(
    
    tibble::tibble(Date = c("05.02"),
                   Race = c("Dubai Tour"),
                   Winner = c("PHINNEY Taylor"),
                   Class = c("2.HC", "2.HC", "2.HC", "2.HC", "2.1"),
                   url = c('race/dubai-tour/2014'),
                   year = c(2014)) %>%
      mutate(type = "Manual") %>%
      mutate(Date = as.Date(paste0(year, "-", str_sub(Date, 4, 5), "-", as.numeric(str_sub(Date, 1, 2))))))

#
# Write DB Table
#

all_events$Race <- iconv(all_events$Race, from="UTF-8", to = "ASCII//TRANSLIT")
all_events$Winner <- iconv(all_events$Winner, from="UTF-8", to = "ASCII//TRANSLIT")

new_events <- all_events %>%
  anti_join(
    
    dbReadTable(con, "all_races"), by = c("url")
    
  )

all_events <- new_events

dbWriteTable(con, "all_races", new_events, append = TRUE, row.names = FALSE)

#
#
# Now extract stage data
#
#

stages_list <- vector("list", length(all_events$url))
gc_list <- vector("list", length(all_events$url))

#

tictoc::tic()

for(e in 1:length(all_events$url)) {  
  
  # go to generic event page and find which stage is listed last
  
  page <- paste0('https://www.procyclingstats.com/', 
                 all_events$url[[e]],
                 '/gc/stages/winners') %>%
    
    read_html()

  # pull in stages
  
  as <- page %>%
    html_nodes('body > div.wrapper > div.content > div.statDivLeft > ul.list.table') %>%
    html_nodes('a') %>%
    html_attr(name = "href") %>%
    enframe(name = NULL) %>% 
    filter(str_detect(value, 'race/'))
  
  # pull in GC winner
  
  gc_winner <- page %>% 
    html_nodes('body > div.wrapper > div.content > div.statDivLeft > ul.list.table') %>%
    html_nodes('a') %>%
    html_text() %>%
    enframe(name = NULL) %>%
    .[nrow(.), ]
  
  if(length(gc_winner) == 0) {
    
    gc_winner = tibble(value = "One Day Race")
    
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
  
  Sys.sleep(1)
  
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
    
    bind_rows(stages_list), by = c("url" = "event")
    
  ) %>%
  
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
# for loop for each stage
#

races_list <- vector("list", length(all_stages$Race))

#

tictoc::tic()

#

for(r in 2495:length(all_stages$value)) {
  
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
     (race_url == "race/60th-tour-de-picardie/2016" & s == 1) |
     (str_detect(race_url, "hammer-") == TRUE)) {
    
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
    
    #page <- url %>%
    #  read_html()
    
    f_name <- paste0("PCS-HTML/", str_replace_all(str_replace(all_stages$value[[r]], "https://www.procyclingstats.com/race/", ""), "/", ""))
    
    download.file(url, f_name, quiet = TRUE)
                  
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
          
          mutate(rnk = as.character(rnk))
        
      }
      
    }
    
    
  }
  # write to the race list for this race
  races_list[[r]] <- stage
  
  print(race_url)
  
  Sys.sleep(runif(1, 0.5, 3.5))
  
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

for(f in 1:length(races_list)) { 
  
  df <- races_list[[f]] %>%
    mutate(date = as.character(date))
  
  df$race <- iconv(df$race, from="UTF-8", to = "ASCII//TRANSLIT")
  df$rider <- iconv(df$rider, from="UTF-8", to = "ASCII//TRANSLIT")
  
  df_list[[f]] <- df
  
}

#

dbWriteTable(con, "pcs_stage_raw", bind_rows(df_list), append = TRUE, row.names = FALSE)

stage_data <- dbReadTable(con, "pcs_stage_raw")

#
#
# Scrape the GAME data
#
#

# what races to pull

game_race_list <- vector("list", 4)

pull_in_game <- c("2016", "2017", "2018", "2019")

for(g in 1:length(pull_in_game)) {
  
  page <- paste0('https://www.procyclingstats.com/game.php?p=info&s=activated-games&season=',
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
    
    all_stages %>%
      select(url, value, year, s) %>%
      mutate(match_url = paste0(url, "/game")), by = c("url" = "match_url")
    
  ) %>%
  select(Date, Race, Url = value, year, Stage = s) %>%
  
  mutate(Url = paste0(Url, "/game/most-picked"))

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
    
    mutate(Url = all_games$Url[[g]],
           Race = all_games$Race[[g]],
           year = all_games$year[[g]],
           Stage = all_games$Stage[[g]])
  
  print(g)
  
  Sys.sleep(runif(1,0.5,2.5))
  
}

games_most_picked <- bind_rows(most_picked_list) %>%
  
  janitor::clean_names() %>%
  
  select(picked_rider = rider_team,
         number_picks,
         result,
         race,
         year,
         stage,
         url = url_2)

dbWriteTable(con, "pcs_game_picks", games_most_picked, overwrite = TRUE, row.names = FALSE)

# make sure to clean up the formatting on these if necessary

#gc_winners <- bind_rows(gc_list) %>%
#  mutate(gc_winner = value)

#gc_winners$gc_winner <- iconv(gc_winners$gc_winner, from="UTF-8", to = "ASCII//TRANSLIT")

#dbWriteTable(con, "pcs_gc_winners", gc_winners, overwrite = TRUE, row.names = FALSE)

#all_events$Race <- iconv(all_events$Race, from="UTF-8", to = "ASCII//TRANSLIT")
#all_events$Winner <- iconv(all_events$Winner, from="UTF-8", to = "ASCII//TRANSLIT")

#dbWriteTable(con, "pcs_all_races", all_events, overwrite = TRUE, row.names = FALSE)

#all_stages$Race <- iconv(all_stages$Race, from="UTF-8", to = "ASCII//TRANSLIT")
#all_stages$Winner <- iconv(all_stages$Winner, from="UTF-8", to = "ASCII//TRANSLIT")

#dbWriteTable(con, "pcs_all_stages", all_stages, overwrite = TRUE, row.names = FALSE)

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

#

stage_data <- stage_data_raw %>%
  
  mutate(stage = ifelse(stage_name == "Prologue", 0, stage)) %>%
  
  group_by(race, year) %>%
  mutate(prologue_exists = min(stage, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(stage_number = ifelse(prologue_exists == 0,
                               ifelse(stage == 0, 0, stage - 1), stage)) %>%
  
  filter(!race %in% c("World Championships MJ - ITT", "World Championships MJ - Road Race",
                      "World Championships U23 - ITT", "World Championships U23 - Road Race",
                      "World Championships WJ - ITT", "World Championships WJ - Road Race",
                      "World Championships WE - ITT", "World Championships WE - Road Race")) %>%
  
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
  
  select(-t10_time, -x10, -x20, -x40, -top_var, -x3, -x5, -date) %>%
  
  mutate(time_trial = ifelse(stage_name %in% c("Time trial", "Prologue") | str_detect(stage_name, "ITT"), TRUE, FALSE)) %>%

  mutate(grand_tour = ifelse(race %in% c("Tour de France", "Giro d'Italia", 
                                         "La Vuelta ciclista a Espana", "Vuelta a Espana"), TRUE, FALSE)) %>%
  
  mutate(one_day_race = ifelse(stage_name == "One day race", TRUE, FALSE)) %>%
  
  mutate(climb_difficulty = NA) %>%
  
  # find rider position in team for stage
  
  group_by(team, stage, race, year) %>%
  mutate(tm_pos = rank(rnk, ties.method = "first")) %>%
  ungroup()

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
         date = Date,
         winner = Winner,
         class = Class,
         url, year) %>%
  
  inner_join(
    
    stage_data %>%
      mutate(rider = str_to_title(tolower(rider))) %>%
      mutate(race = tolower(race)), by = c("race", "year", "winner" = "rider")
    
  ) %>%
  
  select(gc_winner = winner, race, year, stage, gc_seconds = total_seconds, class, date)

#

stage_data <- stage_data %>%
  
  mutate(race = tolower(race)) %>%
  
  left_join(
    
    gc_performance, by = c("year", "race", "stage")
    
  ) %>%
  
  mutate(date = date + (stage - 1)) %>%
  
  select(-mean_time, -med_time, -qual_time, -rel_qual,
         -climb_difficulty, -variance_valid, -distance,
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

# Need to clean-up and re-write climbs information before joining

f_r_climbs <- dbReadTable(con, "flamme_rouge_climbs") %>%
  mutate(year = as.numeric(year))

f_r_data <- dbReadTable(con, "flamme_rouge_characteristics") %>%
  mutate(year = as.numeric(year))

#
# combine F-R data with PCS data
#


stage_data <- stage_data %>%
  
  unique() %>%
  
  mutate(race = ifelse(race %in% c("la vuelta ciclista a espana", "la vuelta a espana"), "vuelta a espana", race)) %>%
  
  unique() %>%
  
  # join with stage characteristic data
  
  left_join(
    
    f_r_data %>%
      select(-length, -slug) %>%
      mutate(race = tolower(race)), by = c("race", "stage", "year")
    
  ) %>%
  
  # join with climb level data
  
  left_join(
    
    f_r_climbs %>%
      
      mutate(race = tolower(race)) %>%
      mutate(race = ifelse(race %in% c("la vuelta ciclista a espana", "la vuelta a espana"), "vuelta a espana", race)) %>%
      
      left_join(f_r_data %>%
                  select(race, stage, year, stage_length = length) %>%
                  mutate(race = tolower(race)), by = c("race", "stage", "year")) %>%
      
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
  
  filter(!(is.na(act_climb_difficulty))) %>%
  
  mutate(position_highest = ifelse(position_highest > 1, 1, position_highest),
         position_highest = ifelse(is.na(position_highest), median(position_highest, na.rm = T), position_highest),
         summit_finish = ifelse(is.na(summit_finish), 0, summit_finish)) %>%
  
  unique()

#
# MISSING TOURS
#

pcs_missing_from_fr <- stage_data_raw %>%
  
  select(stage, race, year) %>%
  mutate(race = tolower(race)) %>%
  unique() %>%
  
  anti_join(
    
    stage_data %>%
      select(stage, race, year) %>%
      mutate(race = tolower(race)) %>%
      unique()
    
  )

# Write the cleaned-up data to database

dbWriteTable(con, "pcs_stage_data", 
             
             stage_data %>%
               mutate(stage = stage_number) %>%
               select(-kom_progression,
                      -prologue_exists,
                      -stage_number), 
             
             overwrite = TRUE, append = FALSE, row.names = FALSE)

