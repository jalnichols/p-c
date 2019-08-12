
library(tidyverse)
library(rvest)

#

cyclist <- 'http://www.chronoswatts.com/cyclistes/1/' %>%
  read_html() %>%
  html_nodes('select') %>%
  html_nodes('option') %>%
  html_attr(name = "value")

data_list <- vector("list", length(cyclist))

#

for(c in 1:length(cyclist)) {
  
  d <- paste0('http://www.chronoswatts.com/cyclistes/', cyclist[[c]], '/') %>%
    
    read_html() %>%
    
    html_nodes('table') %>%
    
    html_table()
  
  if(length(d) > 2) {
  
  bio <- d[[2]]
  
  data <- d[[3]]
  
  colnames(data)[4] <- "Drop"
  colnames(data)[8] <- "Watts"
  
  data_list[[c]] <- data %>%
    cbind(name = bio[1,2]) %>%
    mutate(watts_kg = as.numeric(`Watts/kg`),
           minutes = ((as.numeric(str_sub(Temps, 1, 2)) * 60) + (as.numeric(str_sub(Temps, 4, 5)))) / 60,
           watts = as.numeric(Watts)) %>%
    select(name,
           climb = Col,
           event = Epreuve,
           date = Date,
           minutes,
           watts_kg,
           watts)
  
  }
  
}

#


watts_data <- bind_rows(data_list) %>% 
  mutate(weight = watts / watts_kg) %>%
  
  group_by(name) %>% 
  mutate(weight = mean(weight, na.rm = T)) %>%
  ungroup() %>% 
  
  mutate(year = as.numeric(str_sub(date, 7, 10)), period = ifelse(year < 2003, "EPO", ifelse(year > 2007, "Passport", "Middle"))) %>%
  
  mutate(new_watts_kg = watts / weight,
         period_impact = ifelse(period == "Middle", -0.276, ifelse(period == "Passport", -0.499, 0)),
         x_watts_kg = (-0.5695*log(minutes)) + (period_impact) + 8.2925)