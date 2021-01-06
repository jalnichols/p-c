
library(pdftools)
library(tidyverse)

#

all_cross <- fs::dir_info("C:/Users/Jake/Downloads/Cyclocross")

list_of_results <- vector("list", length(all_cross$path))

#

for(r in 1:length(all_cross$path)) {

FILE_NAME <- all_cross$path[[r]]

PDF_ALL <- pdf_text(FILE_NAME)

PAGES <- length(PDF_ALL)

#

if(PAGES == 1) {

test <- PDF_ALL %>%
  
  .[[1]] %>%
  
  enframe(name = NULL) %>%
  
  mutate(to = value) %>%

  # split the PDF by \r\n
  mutate(to = strsplit(to, "\r\n")) %>%
  
  # unnest to turn into individual rows
  unnest(to) %>%
  # trim whitespace before and after
  mutate(to = str_trim(to)) %>%
  
  rowid_to_column() %>%
  
  mutate(timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to)) %>%
  
  mutate(rider_uci_string = str_extract(to, '\\d{11}')) %>%
  
  fill(rider_uci_string, .direction = "down") %>%
  
  mutate(time1 = str_trim(str_sub(to, timings, timings + 6))) %>%
  
  mutate(to1 = str_sub(to, timings + 7, nchar(to)),
         timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
         time2 = str_trim(str_sub(to1, timings, timings + 6))) %>%
  
  mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
         timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
         time3 = str_trim(str_sub(to1, timings, timings + 6))) %>%
  
  mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
         timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
         time4 = str_trim(str_sub(to1, timings, timings + 6))) %>%
  
  mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
         timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
         time5 = str_trim(str_sub(to1, timings, timings + 6))) %>%
  
  mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
         timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
         time6 = str_trim(str_sub(to1, timings, timings + 6))) %>%
  
  mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
         timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
         time7 = str_trim(str_sub(to1, timings, timings + 6))) %>%
  
  mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
         timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
         time8 = str_trim(str_sub(to1, timings, timings + 6))) %>%   
  
  mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
         timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
         time9 = str_trim(str_sub(to1, timings, timings + 6))) %>%
  
  mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
         timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
         time10 = str_trim(str_sub(to1, timings, timings + 6))) %>%
  
  mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
         timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
         time11 = str_trim(str_sub(to1, timings, timings + 6))) %>%
  
  mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
         timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
         time12 = str_trim(str_sub(to1, timings, timings + 6))) %>%
  
  mutate(valid12 = ifelse(nchar(time12) == 7, 1, 0),
         valid11 = ifelse(nchar(time11) == 7, 1, 0),
         valid10 = ifelse(nchar(time10) == 7, 1, 0),
         valid9 = ifelse(nchar(time9) == 7, 1, 0)) %>%
  
  mutate(rider_count = n_distinct(rider_uci_string, na.rm = T),
         valid12_count = sum(valid12),
         valid11_count = sum(valid11),
         valid10_count = sum(valid10),
         valid9_count = sum(valid9))

#

RIDER_COUNT <- test$rider_count[[1]]

#

if(test$rider_count[[1]] <= test$valid12_count[[1]]) {
  
  PICK <- c()
  LAST <- 'time12'
  LIMIT = 12
  
} else if(test$rider_count[[1]] <= test$valid11_count[[1]]) {
  
  PICK <- c('time12')
  LAST <- 'time11'
  LIMIT = 11
  
} else if(test$rider_count[[1]] <= test$valid10_count[[1]]) {
  
  PICK <- c('time12', 'time11')
  LAST <- 'time10'
  LIMIT = 10
  
} else if(test$rider_count[[1]] <= test$valid9_count[[1]]) {
  
  PICK <- c('time12', 'time11', 'time10')
  LAST <- 'time9'
  LIMIT = 9
  
} else {
  
  PICK <- c('time12', 'time11', 'time10', 'time9')
  LAST <- 'time8'
  LIMIT = 8
  
}

         

#

riders_unique <- test %>%
  
  filter(!is.na(rider_uci_string)) %>%
  select(rider_uci_string) %>%
  unique() %>%
  
  rowid_to_column()

#

results <- test %>% select(to, timings, rider_uci_string, rowid,
                           to1, time1, time2, time3, time4, time5,
                           time6, time7, time8, time9, time10,
                           time11, time12, valid9, valid10, valid11,
                           valid12, rider_count, valid9_count,
                           valid10_count, valid11_count, valid12_count) %>%
  
  select(-to1, -timings) %>%
  
  gather(lap, time, time1:time12) %>%
  
  filter(!lap %in% PICK) %>%
  
  select(-rider_count, -valid9_count, -valid10_count, -valid11_count, -valid12_count,
         -valid9, -valid10, -valid11, -valid12) %>%
  
  mutate(time = ifelse(nchar(time)==7, time, NA),
         lap = ifelse(nchar(time)==7, lap, NA)) %>%
  
  #group_by(rowid) %>%
  #mutate(LAST = paste0("time", sum(!is.na(lap)))) %>%
  #ungroup() %>%
  
  mutate(last = ifelse(lap == LAST, rowid, NA),
         last = rank(last, ties.method = "first"),
         last = ifelse(last <= RIDER_COUNT, last, NA)) %>%
  
  group_by(rowid) %>%
  mutate(last = mean(last, na.rm = T)) %>%
  ungroup() %>%
  
  select(-rider_uci_string, -to, -LAST) %>%
  
  inner_join(riders_unique, by = c("last" = "rowid"))
  
#

riders_name <- test %>% 
  select(to, rider_uci_string, rowid) %>%
  
  filter(str_detect(to, rider_uci_string)) %>%
  
  mutate(location = regexpr('\\d{11}', to)) %>%
  
  mutate(end_name = regexpr('(\\s)(\\s)(\\s)(\\s)', to)) %>%
  
  mutate(rider_name = str_sub(to, location + 11, end_name - 1),
         rider_name = str_trim(str_replace(rider_name, " BEL", "")),
         rider_name = str_trim(str_replace(rider_name, " NED", "")),
         rider_name = str_trim(str_replace(rider_name, " FRA", "")),
         rider_name = str_trim(str_replace(rider_name, " ESP", "")),
         rider_name = str_trim(str_replace(rider_name, " USA", "")),
         rider_name = str_trim(str_replace(rider_name, " LUX", "")),
         rider_name = str_trim(str_replace(rider_name, " GER", "")),
         rider_name = str_trim(str_replace(rider_name, " ENG", ""))) %>%
  
  select(rider_uci_string, rider_name)

#

PAGE1 <- results %>%
  
  inner_join(riders_name)
         
} else if(PAGES == 2) {
  
  
  test <- PDF_ALL %>%
    
    .[[1]] %>%
    
    enframe(name = NULL) %>%
    
    mutate(to = value) %>%
    
    # split the PDF by \r\n
    mutate(to = strsplit(to, "\r\n")) %>%
    
    # unnest to turn into individual rows
    unnest(to) %>%
    # trim whitespace before and after
    mutate(to = str_trim(to)) %>%
    
    rowid_to_column() %>%
    
    mutate(timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to)) %>%
    
    mutate(rider_uci_string = str_extract(to, '\\d{11}')) %>%
    
    fill(rider_uci_string, .direction = "down") %>%
    
    mutate(time1 = str_trim(str_sub(to, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time2 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time3 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time4 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time5 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time6 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time7 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time8 = str_trim(str_sub(to1, timings, timings + 6))) %>%   
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time9 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time10 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time11 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time12 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(valid12 = ifelse(nchar(time12) == 7, 1, 0),
           valid11 = ifelse(nchar(time11) == 7, 1, 0),
           valid10 = ifelse(nchar(time10) == 7, 1, 0),
           valid9 = ifelse(nchar(time9) == 7, 1, 0)) %>%
    
    mutate(rider_count = n_distinct(rider_uci_string, na.rm = T),
           valid12_count = sum(valid12),
           valid11_count = sum(valid11),
           valid10_count = sum(valid10),
           valid9_count = sum(valid9))
  
  #
  
  RIDER_COUNT <- test$rider_count[[1]]
  
  #
  
  if(test$rider_count[[1]] <= test$valid12_count[[1]]) {
    
    PICK <- c()
    LAST <- 'time12'
    LIMIT = 12
    
  } else if(test$rider_count[[1]] <= test$valid11_count[[1]]) {
    
    PICK <- c('time12')
    LAST <- 'time11'
    LIMIT = 11
    
  } else if(test$rider_count[[1]] <= test$valid10_count[[1]]) {
    
    PICK <- c('time12', 'time11')
    LAST <- 'time10'
    LIMIT = 10
    
  } else if(test$rider_count[[1]] <= test$valid9_count[[1]]) {
    
    PICK <- c('time12', 'time11', 'time10')
    LAST <- 'time9'
    LIMIT = 9
    
  } else {
    
    PICK <- c('time12', 'time11', 'time10', 'time9')
    LAST <- 'time8'
    LIMIT = 8
    
  }
  
  #
  
  riders_unique <- test %>%
    
    filter(!is.na(rider_uci_string)) %>%
    select(rider_uci_string) %>%
    unique() %>%
    
    rowid_to_column()
  
  #
  
  results <- test %>% select(to, timings, rider_uci_string, rowid,
                             to1, time1, time2, time3, time4, time5,
                             time6, time7, time8, time9, time10,
                             time11, time12, valid9, valid10, valid11,
                             valid12, rider_count, valid9_count,
                             valid10_count, valid11_count, valid12_count) %>%
    
    select(-to1, -timings) %>%
    
    gather(lap, time, time1:time12) %>%
    
    filter(!lap %in% PICK) %>%
    
    select(-rider_count, -valid9_count, -valid10_count, -valid11_count, -valid12_count,
           -valid9, -valid10, -valid11, -valid12) %>%
    
    mutate(time = ifelse(nchar(time)==7, time, NA),
           lap = ifelse(nchar(time)==7, lap, NA)) %>%
    
    #group_by(rowid) %>%
    #mutate(LAST = paste0("time", sum(!is.na(lap)))) %>%
    #ungroup() %>%
    
    mutate(last = ifelse(lap == LAST, rowid, NA),
           last = rank(last, ties.method = "first"),
           last = ifelse(last <= RIDER_COUNT, last, NA)) %>%
    
    group_by(rowid) %>%
    mutate(last = mean(last, na.rm = T)) %>%
    ungroup() %>%
    
    select(-rider_uci_string, -to, -LAST) %>%
    
    inner_join(riders_unique, by = c("last" = "rowid"))
  
  #
  
  riders_name <- test %>% 
    select(to, rider_uci_string, rowid) %>%
    
    filter(str_detect(to, rider_uci_string)) %>%
    
    mutate(location = regexpr('\\d{11}', to)) %>%
    
    mutate(end_name = regexpr('(\\s)(\\s)(\\s)(\\s)', to)) %>%
    
    mutate(rider_name = str_sub(to, location + 11, end_name - 1),
           rider_name = str_trim(str_replace(rider_name, " BEL", "")),
           rider_name = str_trim(str_replace(rider_name, " NED", "")),
           rider_name = str_trim(str_replace(rider_name, " FRA", "")),
           rider_name = str_trim(str_replace(rider_name, " ESP", "")),
           rider_name = str_trim(str_replace(rider_name, " USA", "")),
           rider_name = str_trim(str_replace(rider_name, " LUX", "")),
           rider_name = str_trim(str_replace(rider_name, " GER", "")),
           rider_name = str_trim(str_replace(rider_name, " ENG", ""))) %>%
    
    select(rider_uci_string, rider_name)
  
  #
  
  PAGE1 <- results %>%
    
    inner_join(riders_name)
  
  #
  
  test <- PDF_ALL %>%
    
    .[[2]] %>%
    
    enframe(name = NULL) %>%
    
    mutate(to = value) %>%
    
    # split the PDF by \r\n
    mutate(to = strsplit(to, "\r\n")) %>%
    
    # unnest to turn into individual rows
    unnest(to) %>%
    # trim whitespace before and after
    mutate(to = str_trim(to)) %>%
    
    rowid_to_column() %>%
    
    mutate(timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to)) %>%
    
    mutate(rider_uci_string = str_extract(to, '\\d{11}')) %>%
    
    fill(rider_uci_string, .direction = "down") %>%
    
    mutate(time1 = str_trim(str_sub(to, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time2 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time3 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time4 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time5 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time6 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time7 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time8 = str_trim(str_sub(to1, timings, timings + 6))) %>%   
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time9 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time10 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time11 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time12 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(valid12 = ifelse(nchar(time12) == 7, 1, 0),
           valid11 = ifelse(nchar(time11) == 7, 1, 0),
           valid10 = ifelse(nchar(time10) == 7, 1, 0),
           valid9 = ifelse(nchar(time9) == 7, 1, 0)) %>%
    
    mutate(rider_count = n_distinct(rider_uci_string, na.rm = T),
           valid12_count = sum(valid12),
           valid11_count = sum(valid11),
           valid10_count = sum(valid10),
           valid9_count = sum(valid9))
  
  RIDER_COUNT <- test$rider_count[[1]]
  
  #
  
  riders_unique <- test %>%
    
    filter(!is.na(rider_uci_string)) %>%
    select(rider_uci_string) %>%
    unique() %>%
    
    rowid_to_column()
  
  #
  
  results <- test %>% select(to, timings, rider_uci_string, rowid,
                             to1, time1, time2, time3, time4, time5,
                             time6, time7, time8, time9, time10,
                             time11, time12, valid9, valid10, valid11,
                             valid12, rider_count, valid9_count,
                             valid10_count, valid11_count, valid12_count) %>%
    
    select(-to1, -timings) %>%
    
    gather(lap, time, time1:time12) %>%
    
    filter(!lap %in% PICK) %>%
    
    select(-rider_count, -valid9_count, -valid10_count, -valid11_count, -valid12_count,
           -valid9, -valid10, -valid11, -valid12) %>%
    
    mutate(time = ifelse(nchar(time)==7, time, NA),
           lap = ifelse(nchar(time)==7, lap, NA)) %>%
    
    #group_by(rowid) %>%
    #mutate(LAST = paste0("time", sum(!is.na(lap)))) %>%
    #ungroup() %>%
    
    mutate(last = ifelse(lap == LAST, rowid, NA),
           last = rank(last, ties.method = "first"),
           last = ifelse(last <= RIDER_COUNT, last, NA)) %>%
    
    group_by(rowid) %>%
    mutate(last = mean(last, na.rm = T)) %>%
    ungroup() %>%
    
    select(-rider_uci_string, -to, -LAST) %>%
    
    inner_join(riders_unique, by = c("last" = "rowid"))
  
  #
  
  riders_name <- test %>% 
    select(to, rider_uci_string, rowid) %>%
    
    filter(str_detect(to, rider_uci_string)) %>%
    
    mutate(location = regexpr('\\d{11}', to)) %>%
    
    mutate(end_name = regexpr('(\\s)(\\s)(\\s)(\\s)', to)) %>%
    
    mutate(rider_name = str_sub(to, location + 11, end_name - 1),
           rider_name = str_trim(str_replace(rider_name, " BEL", "")),
           rider_name = str_trim(str_replace(rider_name, " NED", "")),
           rider_name = str_trim(str_replace(rider_name, " FRA", "")),
           rider_name = str_trim(str_replace(rider_name, " ESP", "")),
           rider_name = str_trim(str_replace(rider_name, " USA", "")),
           rider_name = str_trim(str_replace(rider_name, " LUX", "")),
           rider_name = str_trim(str_replace(rider_name, " GER", "")),
           rider_name = str_trim(str_replace(rider_name, " ENG", ""))) %>%
    
    select(rider_uci_string, rider_name)
  
  #
  
  PAGE2 <- results %>%
    
    inner_join(riders_name)
 
# 
#
# THREE PAGES -------------------------------------------------------------
#
#

} else {
  
  test <- PDF_ALL %>%
    
    .[[1]] %>%
    
    enframe(name = NULL) %>%
    
    mutate(to = value) %>%
    
    # split the PDF by \r\n
    mutate(to = strsplit(to, "\r\n")) %>%
    
    # unnest to turn into individual rows
    unnest(to) %>%
    # trim whitespace before and after
    mutate(to = str_trim(to)) %>%
    
    rowid_to_column() %>%
    
    mutate(timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to)) %>%
    
    mutate(rider_uci_string = str_extract(to, '\\d{11}')) %>%
    
    fill(rider_uci_string, .direction = "down") %>%
    
    mutate(time1 = str_trim(str_sub(to, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time2 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time3 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time4 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time5 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time6 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time7 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time8 = str_trim(str_sub(to1, timings, timings + 6))) %>%   
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time9 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time10 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time11 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time12 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(valid12 = ifelse(nchar(time12) == 7, 1, 0),
           valid11 = ifelse(nchar(time11) == 7, 1, 0),
           valid10 = ifelse(nchar(time10) == 7, 1, 0),
           valid9 = ifelse(nchar(time9) == 7, 1, 0)) %>%
    
    mutate(rider_count = n_distinct(rider_uci_string, na.rm = T),
           valid12_count = sum(valid12),
           valid11_count = sum(valid11),
           valid10_count = sum(valid10),
           valid9_count = sum(valid9))
  
  #
  
  RIDER_COUNT <- test$rider_count[[1]]
  
  #
  
  if(test$rider_count[[1]] <= test$valid12_count[[1]]) {
    
    PICK <- c()
    LAST <- 'time12'
    LIMIT = 12
    
  } else if(test$rider_count[[1]] <= test$valid11_count[[1]]) {
    
    PICK <- c('time12')
    LAST <- 'time11'
    LIMIT = 11
    
  } else if(test$rider_count[[1]] <= test$valid10_count[[1]]) {
    
    PICK <- c('time12', 'time11')
    LAST <- 'time10'
    LIMIT = 10
    
  } else if(test$rider_count[[1]] <= test$valid9_count[[1]]) {
    
    PICK <- c('time12', 'time11', 'time10')
    LAST <- 'time9'
    LIMIT = 9
    
  } else {
    
    PICK <- c('time12', 'time11', 'time10', 'time9')
    LAST <- 'time8'
    LIMIT = 8
    
  }
  
  #
  
  riders_unique <- test %>%
    
    filter(!is.na(rider_uci_string)) %>%
    select(rider_uci_string) %>%
    unique() %>%
    
    rowid_to_column()
  
  #
  
  results <- test %>% select(to, timings, rider_uci_string, rowid,
                             to1, time1, time2, time3, time4, time5,
                             time6, time7, time8, time9, time10,
                             time11, time12, valid9, valid10, valid11,
                             valid12, rider_count, valid9_count,
                             valid10_count, valid11_count, valid12_count) %>%
    
    select(-to1, -timings) %>%
    
    gather(lap, time, time1:time12) %>%
    
    filter(!lap %in% PICK) %>%
    
    select(-rider_count, -valid9_count, -valid10_count, -valid11_count, -valid12_count,
           -valid9, -valid10, -valid11, -valid12) %>%
    
    mutate(time = ifelse(nchar(time)==7, time, NA),
           lap = ifelse(nchar(time)==7, lap, NA)) %>%
    
    mutate(last = ifelse(lap == LAST, rowid, NA),
           last = rank(last, ties.method = "first"),
           last = ifelse(last <= RIDER_COUNT, last, NA)) %>%
    
    group_by(rowid) %>%
    mutate(last = mean(last, na.rm = T)) %>%
    ungroup() %>%
    
    select(-rider_uci_string, -to) %>%
    
    inner_join(riders_unique, by = c("last" = "rowid"))
  
  #
  
  riders_name <- test %>% 
    select(to, rider_uci_string, rowid) %>%
    
    filter(str_detect(to, rider_uci_string)) %>%
    
    mutate(location = regexpr('\\d{11}', to)) %>%
    
    mutate(end_name = regexpr('(\\s)(\\s)(\\s)(\\s)', to)) %>%
    
    mutate(rider_name = str_sub(to, location + 11, end_name - 1),
           rider_name = str_trim(str_replace(rider_name, " BEL", "")),
           rider_name = str_trim(str_replace(rider_name, " NED", "")),
           rider_name = str_trim(str_replace(rider_name, " FRA", "")),
           rider_name = str_trim(str_replace(rider_name, " ESP", "")),
           rider_name = str_trim(str_replace(rider_name, " USA", "")),
           rider_name = str_trim(str_replace(rider_name, " LUX", "")),
           rider_name = str_trim(str_replace(rider_name, " GER", "")),
           rider_name = str_trim(str_replace(rider_name, " ENG", ""))) %>%
    
    select(rider_uci_string, rider_name)
  
  #
  
  PAGE1 <- results %>%
    
    inner_join(riders_name)
  
  #
  
  test <- PDF_ALL %>%
    
    .[[2]] %>%
    
    enframe(name = NULL) %>%
    
    mutate(to = value) %>%
    
    # split the PDF by \r\n
    mutate(to = strsplit(to, "\r\n")) %>%
    
    # unnest to turn into individual rows
    unnest(to) %>%
    # trim whitespace before and after
    mutate(to = str_trim(to)) %>%
    
    rowid_to_column() %>%
    
    mutate(timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to)) %>%
    
    mutate(rider_uci_string = str_extract(to, '\\d{11}')) %>%
    
    fill(rider_uci_string, .direction = "down") %>%
    
    mutate(time1 = str_trim(str_sub(to, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time2 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time3 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time4 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time5 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time6 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time7 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time8 = str_trim(str_sub(to1, timings, timings + 6))) %>%   
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time9 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time10 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time11 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time12 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(valid12 = ifelse(nchar(time12) == 7, 1, 0),
           valid11 = ifelse(nchar(time11) == 7, 1, 0),
           valid10 = ifelse(nchar(time10) == 7, 1, 0),
           valid9 = ifelse(nchar(time9) == 7, 1, 0)) %>%
    
    mutate(rider_count = n_distinct(rider_uci_string, na.rm = T),
           valid12_count = sum(valid12),
           valid11_count = sum(valid11),
           valid10_count = sum(valid10),
           valid9_count = sum(valid9))
  
  RIDER_COUNT <- test$rider_count[[1]]
  
  #
  
  riders_unique <- test %>%
    
    filter(!is.na(rider_uci_string)) %>%
    select(rider_uci_string) %>%
    unique() %>%
    
    rowid_to_column()
  
  #
  
  results <- test %>% select(to, timings, rider_uci_string, rowid,
                             to1, time1, time2, time3, time4, time5,
                             time6, time7, time8, time9, time10,
                             time11, time12, valid9, valid10, valid11,
                             valid12, rider_count, valid9_count,
                             valid10_count, valid11_count, valid12_count) %>%
    
    select(-to1, -timings) %>%
    
    gather(lap, time, time1:time12) %>%
    
    filter(!lap %in% PICK) %>%
    
    select(-rider_count, -valid9_count, -valid10_count, -valid11_count, -valid12_count,
           -valid9, -valid10, -valid11, -valid12) %>%
    
    mutate(time = ifelse(nchar(time)==7, time, NA),
           lap = ifelse(nchar(time)==7, lap, NA)) %>%
    
    #group_by(rowid) %>%
    #mutate(LAST = paste0("time", sum(!is.na(lap)))) %>%
    #ungroup() %>%
    
    mutate(last = ifelse(lap == LAST, rowid, NA),
           last = rank(last, ties.method = "first"),
           last = ifelse(last <= RIDER_COUNT, last, NA)) %>%
    
    group_by(rowid) %>%
    mutate(last = mean(last, na.rm = T)) %>%
    ungroup() %>%
    
    select(-rider_uci_string, -to, -LAST) %>%
    
    inner_join(riders_unique, by = c("last" = "rowid"))
  
  #
  
  riders_name <- test %>% 
    select(to, rider_uci_string, rowid) %>%
    
    filter(str_detect(to, rider_uci_string)) %>%
    
    mutate(location = regexpr('\\d{11}', to)) %>%
    
    mutate(end_name = regexpr('(\\s)(\\s)(\\s)(\\s)', to)) %>%
    
    mutate(rider_name = str_sub(to, location + 11, end_name - 1),
           rider_name = str_trim(str_replace(rider_name, " BEL", "")),
           rider_name = str_trim(str_replace(rider_name, " NED", "")),
           rider_name = str_trim(str_replace(rider_name, " FRA", "")),
           rider_name = str_trim(str_replace(rider_name, " ESP", "")),
           rider_name = str_trim(str_replace(rider_name, " USA", "")),
           rider_name = str_trim(str_replace(rider_name, " LUX", "")),
           rider_name = str_trim(str_replace(rider_name, " GER", "")),
           rider_name = str_trim(str_replace(rider_name, " ENG", ""))) %>%
    
    select(rider_uci_string, rider_name)
  
  #
  
  PAGE2 <- results %>%
    
    inner_join(riders_name)
  
  #
  
  test <- PDF_ALL %>%
    
    .[[3]] %>%
    
    enframe(name = NULL) %>%
    
    mutate(to = value) %>%
    
    # split the PDF by \r\n
    mutate(to = strsplit(to, "\r\n")) %>%
    
    # unnest to turn into individual rows
    unnest(to) %>%
    # trim whitespace before and after
    mutate(to = str_trim(to)) %>%
    
    rowid_to_column() %>%
    
    mutate(timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to)) %>%
    
    mutate(rider_uci_string = str_extract(to, '\\d{11}')) %>%
    
    fill(rider_uci_string, .direction = "down") %>%
    
    mutate(time1 = str_trim(str_sub(to, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time2 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time3 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time4 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time5 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time6 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time7 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time8 = str_trim(str_sub(to1, timings, timings + 6))) %>%   
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time9 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time10 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time11 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(to1 = str_sub(to1, timings + 7, nchar(to)),
           timings = regexpr("(0:)(\\d{2})(:)(\\d{2})", to1),
           time12 = str_trim(str_sub(to1, timings, timings + 6))) %>%
    
    mutate(valid12 = ifelse(nchar(time12) == 7, 1, 0),
           valid11 = ifelse(nchar(time11) == 7, 1, 0),
           valid10 = ifelse(nchar(time10) == 7, 1, 0),
           valid9 = ifelse(nchar(time9) == 7, 1, 0)) %>%
    
    mutate(rider_count = n_distinct(rider_uci_string, na.rm = T),
           valid12_count = sum(valid12),
           valid11_count = sum(valid11),
           valid10_count = sum(valid10),
           valid9_count = sum(valid9))
  
  RIDER_COUNT <- test$rider_count[[1]]
  
  #
  
  riders_unique <- test %>%
    
    filter(!is.na(rider_uci_string)) %>%
    select(rider_uci_string) %>%
    unique() %>%
    
    rowid_to_column()
  
  #
  
  results <- test %>% select(to, timings, rider_uci_string, rowid,
                             to1, time1, time2, time3, time4, time5,
                             time6, time7, time8, time9, time10,
                             time11, time12, valid9, valid10, valid11,
                             valid12, rider_count, valid9_count,
                             valid10_count, valid11_count, valid12_count) %>%
    
    select(-to1, -timings) %>%
    
    gather(lap, time, time1:time12) %>%
    
    filter(!lap %in% PICK) %>%
    
    select(-rider_count, -valid9_count, -valid10_count, -valid11_count, -valid12_count,
           -valid9, -valid10, -valid11, -valid12) %>%
    
    mutate(time = ifelse(nchar(time)==7, time, NA),
           lap = ifelse(nchar(time)==7, lap, NA)) %>%
    
    #group_by(rowid) %>%
    #mutate(LAST = paste0("time", sum(!is.na(lap)))) %>%
    #ungroup() %>%
    
    mutate(last = ifelse(lap == LAST, rowid, NA),
           last = rank(last, ties.method = "first"),
           last = ifelse(last <= RIDER_COUNT, last, NA)) %>%
    
    group_by(rowid) %>%
    mutate(last = mean(last, na.rm = T)) %>%
    ungroup() %>%
    
    select(-rider_uci_string, -to, -LAST) %>%
    
    inner_join(riders_unique, by = c("last" = "rowid"))
  
  #
  
  riders_name <- test %>% 
    select(to, rider_uci_string, rowid) %>%
    
    filter(str_detect(to, rider_uci_string)) %>%
    
    mutate(location = regexpr('\\d{11}', to)) %>%
    
    mutate(end_name = regexpr('(\\s)(\\s)(\\s)(\\s)', to)) %>%
    
    mutate(rider_name = str_sub(to, location + 11, end_name - 1),
           rider_name = str_trim(str_replace(rider_name, " BEL", "")),
           rider_name = str_trim(str_replace(rider_name, " NED", "")),
           rider_name = str_trim(str_replace(rider_name, " FRA", "")),
           rider_name = str_trim(str_replace(rider_name, " ESP", "")),
           rider_name = str_trim(str_replace(rider_name, " USA", "")),
           rider_name = str_trim(str_replace(rider_name, " LUX", "")),
           rider_name = str_trim(str_replace(rider_name, " GER", "")),
           rider_name = str_trim(str_replace(rider_name, " ENG", ""))) %>%
    
    select(rider_uci_string, rider_name)
  
  #
  
  PAGE3 <- results %>%
    
    inner_join(riders_name)
  
}

#
#
#

ALL_RESULTS <- rbind(PAGE1, PAGE2, PAGE3) %>%
  
  mutate(event = FILE_NAME)

list_of_results[[r]] <- ALL_RESULTS

}