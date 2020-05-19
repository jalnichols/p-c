

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

all_events <- rbind(
  
  tibble(race = "Tour de France",
                     year = seq(1990, 2019, 1)) %>%
  mutate(url = paste0('race/tour-de-france/', year, '/')),
  
  tibble(race = "Giro d'Italia",
         year = seq(1990, 2019, 1)) %>%
    mutate(url = paste0('race/giro-d-italia/', year, '/')),
  
  tibble(race = "Vuelta a Espana",
         year = seq(1990, 2019, 1)) %>%
    mutate(url = paste0('race/vuelta-a-espana/', year, '/')))

# Max Stage ---------------------------------------------------------------

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
  rename(gc_winner = value) %>%
  mutate(gc_winner = ifelse(gc_winner == "" & str_detect(event, "tour-de-france"), "ARMSTRONG Lance", gc_winner))

#
#
#

all_stages <- all_events %>%
  
  inner_join(
    
    bind_rows(stages_list), by = c("url" = "event")
    
  ) %>%
  
  unique()

#
#
#

# Stage Data for each Race ------------------------------------------------

# pull in directory of HTML downloads (about ~75 stages don't/can't download)
html_stage_dir <- fs::dir_ls("PCS-HTML-GT/") %>%
  as.list()

# download new stages (TRUE) or use old HTML (FALSE)
dl_html <- FALSE

# delete missing stages

if(dl_html == FALSE) {
  
  all_stages <- all_stages %>%
    mutate(path = paste0("PCS-HTML-GT/", 
                         str_replace_all(str_replace(value, "https://www.procyclingstats.com/race/", ""), "/", ""))) %>%
    filter(path %in% html_stage_dir) %>%
    
    filter(!(value %in% c("race/giro-d-italia/2001/stage-18", "race/giro-d-italia/2018/stage-21",
                          "race/vuelta-a-espana/1991/stage-11", "race/vuelta-a-espana/1993/stage-12",
                          "race/vuelta-a-espana/2002/stage-13")))
  
}

#

races_list <- vector("list", 1000)

stage_type_list <- vector("list", length(all_stages$url))

#

tictoc::tic()

#

for(r in 1312:length(all_stages$value)) {
  
  f_name <- paste0("PCS-HTML-GT/", str_replace_all(str_replace(all_stages$value[[r]], "https://www.procyclingstats.com/race/", ""), "/", ""))
  
  # match existence of f_name in stage directory
  if(f_name %in% html_stage_dir | dl_html == TRUE) {
    
    race_url <- all_stages$url[[r]]
    
    s = 0
    
    url <- paste0("https://www.procyclingstats.com/", all_stages$value[[r]])
    
    # scrape the HTML for the page for multiple use
    
    f_name <- paste0("PCS-HTML-GT/", str_replace_all(str_replace(all_stages$value[[r]], "https://www.procyclingstats.com/race/", ""), "/", ""))
    
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
        start_end = characteristics[[3]]
        
      } else {
        
        distance = NA
        stage_name = "one day race"
        start_end = NA
        
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
                 start_end = NA,
                 race = all_stages$Race[[r]],
                 year = all_stages$year[[r]],
                 url = all_stages$url[[r]],
                 race_url = all_stages$value[[r]])
        
        print(paste0("error in stage", s, "race", race_url))
        
        # otherwise continue with processing
        
      } else {
        
        # bring in stage distance again
        
        length <- page %>%
          html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "red", " " ))]') %>% 
          html_text() 
        
        if(identical(length, character(0)) == TRUE) {
          
          length = NA
          
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
                 start_end = start_end,
                 parcours_value = profile_value) %>%
          
          mutate(stage = s,
                 race = all_stages$race[[r]],
                 year = all_stages$year[[r]],
                 url = all_stages$url[[r]],
                 race_url = all_stages$value[[r]]) %>%
          
          mutate(rnk = as.character(rnk))
        
      }
      
    }
    
    # write to the race list for this race
    races_list[[r]] <- stage
    
    print(url)
    
    if(dl_html == TRUE) {
      
      Sys.sleep(runif(1, 0.5, 3.5))
      
    }
    
  }
  
}

tictoc::toc()

#
# check data
#

races_list <- races_list[lengths(races_list) != 0]

df_list <- races_list

for(f in 1:length(races_list)) { 
  
  df <- races_list[[f]]
  
  if(df$rider == "Cancelled / TTT / anomalous") {
    
    
  } else {
    
    df$race <- iconv(df$race, from="UTF-8", to = "ASCII//TRANSLIT")
    df$rider <- iconv(df$rider, from="UTF-8", to = "ASCII//TRANSLIT")
    
    df_list[[f]] <- df
    
  }
  
}

#
#
#
#
#

test_dl <- bind_rows(df_list) %>%
  
  separate(start_end, c("start_city", "end_city"), sep = "›") %>%
  mutate(start_city = str_trim(start_city),
         end_city = str_trim(end_city)) %>%
  mutate(stage = str_replace(stage_name, "Stage ", ""),
         stage = ifelse(stage == "Prologue", 0, as.numeric(str_sub(stage,1,2)))) %>%
  
  unique() %>%
  
  # ported over from NEW Scraper PCS.R

  mutate(distance = str_replace(distance, "\\(", ""),
         distance = str_replace(distance, "\\)", ""),
         distance = as.numeric(str_replace(distance, "k", ""))) %>%
  
  mutate(length = ifelse(is.na(length), distance, length)) %>%

  mutate(win_seconds = round(win_seconds, 0),
         total_seconds = round(total_seconds, 0)) %>%
  
  # fix errors
  mutate(total_seconds = ifelse(stage == 19 & year == 1997 & race == "Tour de France",
                                ifelse(rnk %in% c(10,11), 14597, total_seconds), total_seconds),
         total_seconds = ifelse(stage == 20 & year == 2006 & race == "Tour de France",
                                ifelse(rnk > 1, total_seconds-14212, total_seconds), total_seconds),
         total_seconds = ifelse(stage == 18 & year == 2007 & race == "Giro d'Italia",
                                ifelse(rnk %in% c(1,3), 16371, total_seconds), total_seconds)) %>%
  
  mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team))) %>%
  mutate(finished = ifelse(rnk %in% c("DNF", "OTL", "DNS", "NQ", "DSQ"), NA, total_seconds)) %>%
  mutate(total_seconds = ifelse(total_seconds > 30000, NA, total_seconds)) %>%

  filter(!rnk %in% c("DNS", "DSQ", "NQ")) %>%
  
  mutate(total_seconds = ifelse(rnk == "DNF", NA, total_seconds)) %>%
  
  mutate(total_seconds = ifelse(total_seconds < win_seconds, total_seconds + win_seconds, total_seconds)) %>%
  
  select(-finished, -time_seconds) %>%
  mutate(speed = length / ((total_seconds) / 3600)) %>%
  
  mutate(variance_valid = ifelse((total_seconds - win_seconds) < 2400, total_seconds, NA)) %>%
  
  group_by(stage_name, race, year) %>%
  mutate(last_place = max(total_seconds, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(total_seconds = ifelse(rnk %in% c("OTL"), last_place, total_seconds)) %>%
  mutate(total_seconds = ifelse(is.na(total_seconds), last_place, total_seconds)) %>%
  
  select(-last_place) %>%
  
  mutate(rnk = as.numeric(rnk)) %>%

  group_by(stage_name, race, year) %>%
  mutate(rnk = rank(total_seconds, ties.method = "first")) %>%
  ungroup() %>%
  
  mutate(t10_time = ifelse(!is.na(rnk), total_seconds, NA)) %>%
  mutate(x10 = ifelse(rnk == 10, total_seconds, NA),
         x20 = ifelse(rnk == 20, total_seconds, NA),
         x40 = ifelse(rnk == 40, total_seconds, NA),
         x3 = ifelse(rnk == 3, total_seconds, NA),
         x5 = ifelse(rnk == 5, total_seconds, NA),
         top_var = ifelse(rnk < 41, total_seconds, NA)) %>%
  
  group_by(stage_name, race, year) %>%
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

  mutate(climb_difficulty = NA) %>%
  
  # find rider position in team for stage
  
  group_by(team, stage_name, race, year) %>%
  mutate(tm_pos = rank(rnk, ties.method = "first")) %>%
  ungroup() %>%
  
  rename(parcours_type = stage_type) %>%
  
  left_join(
    
    read_csv("stage-types-89-18.csv") %>% 
      filter(Type == "Stage") %>% 
      gather(year, stage_type, x1989:x2019) %>%
      mutate(year = as.numeric(str_replace(year, "x", ""))) %>%
      filter(!is.na(stage_type)) %>%
      select(-Type), by = c("stage" = "Stage", "race" = "Race", "year"))

#
#
#
#
#

stage_qualities <- 

stage_kom <- read_csv("stage-types-89-18.csv") %>% 
  filter(Type == "KOM") %>% 
  gather(year, kom_points, x1989:x2019) %>%
  mutate(year = as.numeric(str_replace(year, "x", "")),
         kom_points = as.numeric(kom_points)) %>%
  filter(!is.na(kom_points))

#

stage_data <- bind_rows(races_list) %>% 
  
  select(-speed) %>%
  select(-time_seconds, -kom_progression, -kom_all)
  
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