
library(tidyverse)
library(DBI)
library(rvest)

#
#
# pull in each race and check for Live Stats

checker <- 'https://www.procyclingstats.com/race/tour-de-france/2019/stage-11' %>%
  
  read_html() %>%
  html_nodes('a') %>%
  html_text() %>%
  enframe(name = NULL) %>%
  filter(str_detect(value, "LiveStats"))

if(length(checker)>1) {
  
  url_to_pull <- 'https://www.procyclingstats.com/race/tour-de-france/2019/stage-11' %>%
    
    read_html() %>%
    html_nodes('a') %>%
    html_attr(name = "href") %>%
    enframe(name = NULL) %>%
    filter(str_detect(value, "/livestats"))
  
}


#
#
# this simply pulls in un-structured list of all comments in the Live Stats page on PCS

page <- 'https://www.procyclingstats.com/race/tour-de-france/2019/stage-3/today/livestats' %>%
  
  read_html()

df <- cbind(
  
  page %>%
    html_nodes('ul.timeline2') %>%
    html_nodes('li') %>%
    html_nodes('div.txt') %>%
    html_text(),
  page %>%
    html_nodes('ul.timeline2') %>%
    html_nodes('li') %>%
    html_nodes('div.bol') %>%
    html_text())

