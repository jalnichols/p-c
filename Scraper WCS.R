
library(tidyverse)
library(rvest)
library(DBI)
library(RMySQL)

#

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

for(x in 8:8) {
  
  y = 2012 + x
  
  page <- paste0('https://www.worldcyclingstats.com/en/races/', y, '/calendar') %>%
    read_html()
  
  tb <- cbind(
    
    page %>% html_nodes('table') %>% html_nodes('table') %>% html_table() %>% .[[2]] %>% .[2:nrow(.), ],
    
    page %>% html_nodes('table') %>% html_nodes('table') %>% html_nodes('td') %>% html_nodes('a') %>% html_attr(name = "href") %>% 
      enframe(name = NULL) %>% filter(str_detect(value, "en/race")) %>% rename(url = value)
    
  )
  
  colnames(tb) <- c("start_date", "end_date", "category", "class", "blank", "race", "blank2", "winner", "url")
  
  #
  
  mens <- tb %>%
    filter(category %in% c("ME", "MU")) %>%
    mutate(year = y) %>%
    select(-blank, -blank2)
  
  dbWriteTable(con, "wcs_races", mens, row.names = FALSE, append = TRUE)
  
}

#
#
#

all_races <- dbGetQuery(con, "SELECT * FROM wcs_races WHERE year = 2020")

#

for(x in 1:length(all_races$url)) {
  
  page <- paste0(all_races$url[[x]], "/", all_races$year[[x]]) %>%
    read_html()
  
  tables <- cbind(
    page %>%
      html_nodes('table') %>%
      html_nodes(xpath = '//*[@id="profiles"]') %>%
      html_nodes('a') %>%
      html_attr(name = "href") %>%
      enframe(name = NULL) %>%
      rename(stage_url = value) %>%
      unique()) %>%
    
    mutate(url = paste0(all_races$url[[x]], "/", all_races$year[[x]]),
           year = all_races$year[[x]],
           race = all_races$race[[x]])
  
  if(length(tables$stage_url) == 0) {
    
    tables <- tibble(stage_url = paste0(all_races$url[[x]], "/", all_races$year[[x]], "/results"),
                     url = paste0(all_races$url[[x]], "/", all_races$year[[x]]),
                     year = all_races$year[[x]],
                     race = all_races$race[[x]])
    
  }
  
  dbWriteTable(con, "wcs_stages", tables, row.names = FALSE, append = TRUE)
  
  print(x)
  
  Sys.sleep(runif(1,1,5))
  
}

#
#
#

all_stages <- dbGetQuery(con,
                         
                         "SELECT S.stage_url, S.url, R.class, R.year, R.race
                         FROM  wcs_stages S
                         JOIN wcs_races R
                          ON S.url = CONCAT(R.url, '/', R.year)"
                           
                           ) %>%
  mutate(RaceType = ifelse(str_sub(class,1,1)=="1", "One day race", "Stage race")) %>%
  
  filter(RaceType == "Stage race" & year >= 2020)

#

download_html <- TRUE

dir_html <- fs::dir_ls("WCS-HTML/") %>%
  as.list()

#

for(x in 1:length(all_stages$stage_url)) {
  
  DEST = paste0("WCS-HTML/", 
                str_replace_all(
                  str_replace(all_stages$stage_url[[x]], "https://www.worldcyclingstats.com/en/", ""),
                  "/", ""), "-results")
  
  print(paste0(all_stages$race[[x]], " ", all_stages$year[[x]]))
  
  if(download_html == TRUE) {
    
    # if explicitly told to scrape, scrape all
    
    download.file(url = all_stages$stage_url[[x]], 
                  destfile = DEST,
                  quiet = TRUE)
    
    print("re-downloading...")
    
    scraped <- TRUE
    
  } else if(DEST %in% dir_html) {

    # if it's already scraped, don't rescrape unless explicitly told to above
    
    print("already have it")
    
    scraped <- FALSE
    
  } else {
    
    # otherwise pull it in if we don't have it already
    
    download.file(url = all_stages$stage_url[[x]], 
                  destfile = DEST,
                  quiet = TRUE)
    
    print("downloading new...")
    
    scraped <- TRUE
    
  }
  
  # READ IN PAGE AND CHECK FOR EXISTENCE OF CLIMBS AND SPRINTS
  
  page <- DEST %>%
    read_html()
  
  sprints <- page %>%
    html_nodes('table.container') %>%
    html_nodes(xpath = '//*[@id="sprints"]') %>%
    html_nodes('table')
  
  climbs <- page %>%
    html_nodes('table.container') %>%
    html_nodes(xpath = '//*[@id="climbs"]') %>%
    html_nodes('table')
  
  # SPRINTS
  
  if(length(sprints) == 0) {
    
    print("missing Sprints")
    
  } else {
    
    sprints <- sprints %>%
    html_table(fill=TRUE) %>%
      .[[1]] %>%
      select(X1, X3, X4, X5, X7, X9, X10, X11)
    
    colnames(sprints) <- sprints[1, ]
    
    sprints <- sprints[2:nrow(sprints), ]
    
    sprints <- sprints %>%
      select(Km, Name = Sprint, Pos = `#`, Bib = `NA`, Rider, Points) %>%
      mutate(Km = ifelse(Km == '', NA, Km),
             Name = ifelse(Name == '', NA, Name)) %>%
      fill(Km, Name) %>%
      mutate(Type = "Sprint",
             Points = readr::parse_number(Points)) %>%
      mutate(stage_url = all_stages$stage_url[[x]],
             year = all_stages$year[[x]],
             race = all_stages$race[[x]]) %>%
      filter(!is.na(Points)) %>%
      
      mutate(Rider = iconv(Rider, to = "ASCII//TRANSLIT")) %>%
      
      mutate(Rider = str_replace(Rider, ",", "")) %>%
      
      mutate(Rider = case_when(Rider == "Roche Nicholas" ~ "Roche Nicolas",
                               Rider == "Garderen Tejay van" ~ "van Garderen Tejay",
                               Rider == "De La Cruz David" ~ "de la Cruz David",
                               Rider == "Van Der Sande Tosh" ~ "Van der Sande Tosh",
                               Rider == "Hoorn Taco van der" ~ "van der Hoorn Taco",
                               Rider == "De Vos Adam" ~ "de Vos Adam",
                               Rider == "Janse van Rensburg J.." ~ "Janse van Rensburg Jacques",
                               Rider == "Grivko Andrei" ~ "Grivko Andrey",
                               Rider == "Prades Edu" ~ "Prades Eduard",
                               Rider == "De La Parte Victor" ~ "de la Parte Victor",
                               Rider == "Prades Benjami" ~ "Prades Benjamin",
                               Rider == "Poppel Boy van" ~ "van Poppel Boy",
                               Rider == "Poppel Danny van" ~ "van Poppel Danny",
                               Rider == "Dam Laurens ten" ~ "ten Dam Laurens",
                               Rider == "Hagen Edvald Boasson" ~ "Boasson Hagen Edvald",
                               Rider == "Aert Wout van" ~ "van Aert Wout",
                               Rider == "Poel Mathieu van der" ~ "van der Poel Mathieu",
                               Rider == "Baarle Dylan van" ~ "van Baarle Dylan",
                               Rider == "Kort Koen de" ~ "de Kort Koen",
                               Rider == "Riabushenko Aleksandr" ~ "Riabushenko Alexandr",
                               Rider == "Thomson Jay" ~ "Thomson Jay Robert",
                               Rider == "Bak Lars" ~ "Bak Lars Ytting",
                               Rider == "Jansen Amund" ~ "Jansen Amund Grondahl",
                               Rider == "Empel Etienne van" ~ "van Empel Etienne",
                               Rider == "Hansen Lasse" ~ "Hansen Lasse Norman",
                               Rider == "Gebreigzabheir Amanuel" ~ "Ghebreigzabhier Amanuel",
                               Rider == "Juul-Jensen Christop.." ~ "Juul-Jensen Christopher",
                               Rider == "Schmidt Mads" ~ "Wurtz Schmidt Mads",
                               Rider == "Tvetcov Serghei" ~ "?vetcov Serghei",
                               Rider == "Pidcock Thomas	" ~ "Pidcock Thomas",
                               Rider == "Grosu Eduard" ~ "Grosu Eduard-Michael",
                               Rider == "Eiking Odd" ~ "Eiking Odd Christian",
                               Rider == "Dyball Ben" ~ "Dyball Benjamin",
                               Rider == "Hodeg Alvaro" ~ "Hodeg Alvaro Jose",
                               
                               Rider == "Eisenhart TJ" ~ "Eisenhart Taylor",
                               Rider == "Emden Jos van" ~ "van Emden Jos",
                               Rider == "Janse van Rensburg R.." ~ "Janse van Rensburg Reinardt",
                               Rider == "Grmaye Biniam" ~ "Ghirmay Hailu Biniam",
                               
                               TRUE ~ as.character(Rider)))
    
    
    dbWriteTable(con, "wcs_kom_sprints", sprints, row.names = FALSE, append = TRUE)
    
    print("writing Sprints...")
    
  }
  
  # CLIMBS
  
  if(length(climbs) == 0) {
    
    print("missing KOMs")
    
  } else {
    
  climbs <- climbs %>%
    html_table(fill = TRUE) %>%
    .[[1]] %>%
    select(X1, X3, X4, X5, X7, X9, X10, X11)
  
  colnames(climbs) <- climbs[1, ]
  
  climbs <- climbs[2:nrow(climbs), ]
  
  climbs <- climbs %>%
    select(Km, Name = Climb, Pos = `#`, Bib = `NA`, Rider, Points) %>%
    mutate(Km = ifelse(Km == '', NA, Km),
           Name = ifelse(Name == '', NA, Name)) %>%
    fill(Km, Name) %>%
    mutate(Type = "KOM",
           Points = readr::parse_number(Points)) %>%
    mutate(stage_url = all_stages$stage_url[[x]],
           year = all_stages$year[[x]],
           race = all_stages$race[[x]]) %>%
    filter(!is.na(Points)) %>%
    
    mutate(Rider = iconv(Rider, to = "ASCII//TRANSLIT")) %>%
    
    mutate(Rider = str_replace(Rider, ",", "")) %>%
    
    mutate(Rider = case_when(Rider == "Roche Nicholas" ~ "Roche Nicolas",
                             Rider == "Garderen Tejay van" ~ "van Garderen Tejay",
                             Rider == "De La Cruz David" ~ "de la Cruz David",
                             Rider == "Van Der Sande Tosh" ~ "Van der Sande Tosh",
                             Rider == "Hoorn Taco van der" ~ "van der Hoorn Taco",
                             Rider == "De Vos Adam" ~ "de Vos Adam",
                             Rider == "Janse van Rensburg J.." ~ "Janse van Rensburg Jacques",
                             Rider == "Grivko Andrei" ~ "Grivko Andrey",
                             Rider == "Prades Edu" ~ "Prades Eduard",
                             Rider == "De La Parte Victor" ~ "de la Parte Victor",
                             Rider == "Prades Benjami" ~ "Prades Benjamin",
                             Rider == "Poppel Boy van" ~ "van Poppel Boy",
                             Rider == "Poppel Danny van" ~ "van Poppel Danny",
                             Rider == "Dam Laurens ten" ~ "ten Dam Laurens",
                             Rider == "Hagen Edvald Boasson" ~ "Boasson Hagen Edvald",
                             Rider == "Aert Wout van" ~ "van Aert Wout",
                             Rider == "Poel Mathieu van der" ~ "van der Poel Mathieu",
                             Rider == "Baarle Dylan van" ~ "van Baarle Dylan",
                             Rider == "Kort Koen de" ~ "de Kort Koen",
                             Rider == "Riabushenko Aleksandr" ~ "Riabushenko Alexandr",
                             Rider == "Thomson Jay" ~ "Thomson Jay Robert",
                             Rider == "Bak Lars" ~ "Bak Lars Ytting",
                             Rider == "Jansen Amund" ~ "Jansen Amund Grondahl",
                             Rider == "Empel Etienne van" ~ "van Empel Etienne",
                             Rider == "Hansen Lasse" ~ "Hansen Lasse Norman",
                             Rider == "Gebreigzabheir Amanuel" ~ "Ghebreigzabhier Amanuel",
                             Rider == "Juul-Jensen Christop.." ~ "Juul-Jensen Christopher",
                             Rider == "Schmidt Mads" ~ "Wurtz Schmidt Mads",
                             Rider == "Tvetcov Serghei" ~ "?vetcov Serghei",
                             Rider == "Pidcock Thomas	" ~ "Pidcock Thomas",
                             Rider == "Grosu Eduard" ~ "Grosu Eduard-Michael",
                             Rider == "Eiking Odd" ~ "Eiking Odd Christian",
                             Rider == "Dyball Ben" ~ "Dyball Benjamin",
                             Rider == "Hodeg Alvaro" ~ "Hodeg Alvaro Jose",
                             
                             Rider == "Eisenhart TJ" ~ "Eisenhart Taylor",
                             Rider == "Emden Jos van" ~ "van Emden Jos",
                             Rider == "Janse van Rensburg R.." ~ "Janse van Rensburg Reinardt",
                             Rider == "Grmaye Biniam" ~ "Ghirmay Hailu Biniam",
                             
                             TRUE ~ as.character(Rider)))
  
  dbWriteTable(con, "wcs_kom_sprints", climbs, row.names = FALSE, append = TRUE)
  
  print("writing KOMS...")
  
  }
  
  if(scraped == TRUE) {
    
    Sys.sleep(runif(1,1,5))
    
  }
 
}

#
#
#
#
#