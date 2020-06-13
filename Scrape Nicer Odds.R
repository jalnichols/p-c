
library("tidyverse")
library("rvest")
library(DBI)

readRenviron("p-c/.Renviron")

con <- DBI::dbConnect(
  RMariaDB::MariaDB(), 
  user = Sys.getenv("MYSQL_U"), 
  password = Sys.getenv("MYSQL_P"), 
  db = "cycling", 
  host = Sys.getenv("MYSQL_H"))

print("connected to DB")

#
#
# download page

page <- "https://www.nicerodds.co.uk" %>%
  read_html()

# on the left menu each sport is listed
# we find where cycling is listed so we can pull in everything underneath

cycling_num_container <- page %>%
  html_nodes('div') %>%
  html_nodes(xpath = '//*[@id="leftmenu"]') %>%
  html_nodes('a.menu-item.level-1.has-children') %>%
  html_nodes('span') %>%
  html_text() %>%
  enframe() %>%
  filter(value == "Cycling") %>%
  mutate(name = floor(name/2)+1) %>%
  .[1,1] %>%
  .[[1]]

# list all races underneath
# this combines urls and races; filter everything below Old odds
# filters anything not in current year

list_races <- cbind(
  
  page %>%
    html_nodes('div') %>%
    html_nodes(xpath = '//*[@id="leftmenu"]') %>%
    html_nodes('div.level-2-container') %>%
    .[[cycling_num_container]] %>%
    html_nodes('a') %>%
    html_attr(name = "href") %>%
    enframe() %>%
    select(url = value),
  
  page %>%
    html_nodes('div') %>%
    html_nodes(xpath = '//*[@id="leftmenu"]') %>%
    html_nodes('div.level-2-container') %>%
    .[[cycling_num_container]] %>%
    html_nodes('a') %>%
    html_nodes('span') %>%
    html_text() %>%
    enframe() %>%
    filter(!value == "") %>%
    select(race = value)) %>%
  
  mutate(third_level = ifelse(str_sub(race, nchar(race)-3, nchar(race)) %in% c("2018", "2019", "2020", "2021", "2022"), race, NA)) %>%
  fill(third_level) %>%
  mutate(race = ifelse(race==third_level, race, paste0(third_level, " ", race))) %>%
  
  mutate(old = ifelse(str_detect(race, "Old odds"), 1, NA)) %>%
  fill(old) %>%
  filter(is.na(old)) %>%
  filter(str_detect(race, paste0(lubridate::year(lubridate::today())))) %>%
  filter(nchar(url)>5) %>%
  
  mutate(URL = paste0("https://www.nicerodds.co.uk", url)) %>%
  select(-url, -old, -third_level)

#
# Now  hit each URL and pull in data
#

for(u in 1:length(list_races$URL)) {
  
  # pull in webpage
  
  page <- list_races$URL[[u]] %>%
    read_html()
  
  supp <- page %>%
    html_nodes('table.autooddstable_container') %>%
    html_nodes('table.autooddstable') %>%
    html_table(fill=TRUE)
  
  para <- page %>%
    html_nodes(xpath = '//*[@id="mainplh_Content02"]') %>%
    html_nodes('p') %>%
    html_text()
  
  para <- toString(para)
  
  if(length(para)==0) {
    
    para = "No Notes"
    
  }
  
  # the pages list 2019 odds for a lot of races where 2020 isn't ready, so filter those out here
  
  if(str_detect(para, "There is no odds at") == TRUE |
     str_detect(para, "no odds on") == TRUE |
     str_detect(para, "cancelled") == TRUE |
     str_detect(para, "postponed") == TRUE) {
    
    print(paste0("no odds yet for ", list_races$race[[u]]))
    
  } else {
    
    # based on how many columns exist, pull in data
    
    if(length(supp)==0) {
      
      print(paste0("no results for ", list_races$race[[u]]))
      
    } else if(length(supp)==1) {
      
      df <- supp %>%
        .[[1]]
      
      colnames(df) <- c("Rider", "Highest", "Lowest")
      
      df <- df %>%
        mutate(Race = list_races$race[[u]],
               URL = list_races$URL[[u]],
               Year = parse_number(list_races$race[[u]])) %>%
        mutate(Updated = lubridate::now()) %>%
        filter(!is.na(Highest)) %>%
        gather(OddType, Odds, -Rider, -Race, -URL, -Year, -Updated) %>%
        mutate(content_paragraph = para)
      
      DBI::dbWriteTable(con, "nicerodds_raceodds", df, append = TRUE, row.names = FALSE)
      
      print(paste0("high/low results ", list_races$race[[u]]))
      
    } else if(length(supp)>1) {
      
      df <- supp %>%
        .[[1]]
      
      colnames(df) <- c("Rider", "Highest", "Lowest")
      
      df1 <- supp %>%
        .[[2]]
      
      df1[,] <- as.numeric(df1[,ncol(df1)])
      
      df <- cbind(df, df1) %>%
        mutate(Race = list_races$race[[u]],
               URL = list_races$URL[[u]],
               Year = parse_number(list_races$race[[u]])) %>%
        mutate(Updated = lubridate::now()) %>%
        filter(!is.na(Highest)) %>%
        gather(OddType, Odds, -Rider, -Race, -URL, -Year, -Updated) %>%
        mutate(content_paragraph = para)
      
      DBI::dbWriteTable(con, "nicerodds_raceodds", df, append = TRUE, row.names = FALSE)
      
      print(paste0("specific bookmakers ", list_races$race[[u]]))
      
    }
    
  }
  
  Sys.sleep(runif(1, 0.5, 3.5))
  
}

#
#
#
