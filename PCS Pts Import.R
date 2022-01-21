

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
#
#
#
#
# pull in directory of HTML downloads (about ~75 stages don't/can't download)
html_stage_dir <- fs::dir_ls("D:/Jake/Documents/PCS-HTML/")

# download new stages (TRUE) or use old HTML (FALSE)
dl_html <- FALSE

# races from this year
dbGetQuery(con, "SELECT stage, race, year, date FROM pcs_stage_raw WHERE year = 2021 AND rnk = '1'") -> Y21

# start scraping process using old data

if(dl_html == FALSE) {
  
  all_stages <- dbReadTable(con, "pcs_all_stages") %>%
    filter(Date >= '2021-10-13')
  
}

#
# for loop for each stage
#

races_list <- vector("list", length(all_stages$Race))

#

tictoc::tic()

#

for(r in 781:length(all_stages$value)) {
  
  f_name <- paste0("D:/Jake/Documents/PCS-HTML/", str_replace_all(str_replace(all_stages$value[[r]], "https://www.procyclingstats.com/race/", ""), "/", ""))
  
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
      
      f_name <- paste0("D:/Jake/Documents/PCS-HTML/", str_replace_all(str_replace(all_stages$value[[r]], "https://www.procyclingstats.com/race/", ""), "/", ""))
      
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
        
        stage_name <- page %>%
          html_nodes(xpath = '/html/body/div[1]/div[1]/div[2]/div[2]/span[3]') %>%
          html_text()
        
        if(length(stage_name) == 0) {
          
          stage_name = "missing"
          
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
          
          if(!"rider" %in% colnames(stage)) {
            print("no rider column, possibly a TTT")
          } else if(!"pnt" %in% colnames(stage)) {
            print("no pcs points")
          } else {
            
            stage <- stage %>%
              
              select(rnk, rider, team, pcs_pts = pnt) %>%
              
              mutate(stage = s,
                     stage_number = str_replace(str_replace(all_stages$value[[r]], all_stages$url[[r]], ""), "https://www.procyclingstats.com/", ""),
                     race = all_stages$Race[[r]],
                     year = all_stages$year[[r]],
                     url = all_stages$url[[r]],
                     stage_name = stage_name,
                     Class = all_stages$Class[[r]]) %>%
              
              mutate(rnk = as.character(rnk))
            
            races_list[[r]] <- stage
            
          }
          
        }
        
      }
      
      
    }
    
    if(dl_html == TRUE) {
      
      Sys.sleep(runif(1, 0.5, 3.5))
      
    }
    
  }
  
}

tictoc::toc()

#
# WRITE TO DB
#

races_list <- races_list[lengths(races_list) != 0]

df_list <- races_list

for(f in 1:length(races_list)) { 
  
  if(!"rider" %in% colnames(races_list[[f]])) {
    
    df_list[[f]] <- NA
    
  } else {
    
    if(length(races_list[[f]]$rnk) == 0) {
      
    } else {
      
      df <- races_list[[f]]
      
      df$race <- iconv(df$race, from="UTF-8", to = "ASCII//TRANSLIT")
      df$rider <- iconv(df$rider, from="UTF-8", to = "ASCII//TRANSLIT")
      
      df_list[[f]] <- df
      
    }
    
  }
  
}

df_list <- df_list[lengths(df_list) > 1]

#
#
#

stage_pts_raw <- bind_rows(df_list) %>% unique()

#
#
#

stage_pts_raw$race <- iconv(stage_pts_raw$race, from="UTF-8", to = "ASCII//TRANSLIT")

stage_pts_raw$rider <- iconv(stage_pts_raw$rider, from="UTF-8", to = "ASCII//TRANSLIT")

stage_pts_raw$team <- stringi::stri_trans_general(str = stage_pts_raw$team, 
                                                   id = "Latin-ASCII")

#

pcs_all_races <- dbGetQuery(con, "SELECT DISTINCT url, type FROM pcs_all_races") %>%
  
  mutate(junior = str_detect(type, "circuit=15")) %>%
  select(-type)

#

stage_data_int <- stage_pts_raw %>%
  
  left_join(pcs_all_races, by = c("url")) %>%
  
  mutate(Class = ifelse(is.na(junior), Class,
                        ifelse(junior == TRUE, "JR", Class))) %>%
  
  select(-junior) %>%
  
  rename(class = Class) %>%
  unique() %>%
  
  mutate(stage_number = ifelse(stage_name == "One day race", '1',
                               ifelse(stage_name == "Prologue", '0', 
                                      str_replace(
                                        str_replace(stage_number, "/stage-", ""), "stage-", "")))) %>%
  
  mutate(stage = ifelse(stage_name == "Prologue", '0', 
                        ifelse(stage_name == "One day race", '1', stage_number)))



stage_data <- stage_data_int %>%
  
  mutate(team = ifelse(is.na(team), "", team)) %>%
  
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
                        ifelse(is.na(stage), '1', stage)),
         stage = ifelse(stage == "", 1, stage)) %>%
  
  mutate(stage = ifelse(stage == "https://www.procyclingstats.com/", 1, stage)) %>%
  
  filter(!race %in% c("World Championships WJ - ITT", "World Championships WJ - Road Race",
                      "World Championships WE - ITT", "World Championships WE - Road Race",
                      'Manavgat Side Junior', 'Grand Prix Manavgat - Side WE')) %>%
  
  mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team)),
         rider = str_to_title(rider)) %>%
  
  mutate(rnk = as.numeric(rnk)) %>%
  
  mutate(time_trial = ifelse(stage_name %in% c("Time trial", "Prologue") | str_detect(stage_name, "ITT"), TRUE, FALSE)) %>%
  
  mutate(grand_tour = ifelse(race %in% c("Tour de France", "Giro d'Italia", 
                                         "La Vuelta ciclista a Espana", "Vuelta a Espana"), TRUE, FALSE)) %>%
  
  mutate(one_day_race = ifelse(stage_name == "One day race", TRUE, FALSE)) %>%
  
  select(-stage_number)


#dbWriteTable(con, "pcs_stage_pts", stage_data, append = TRUE, row.names = FALSE)
