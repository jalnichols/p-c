

library(tidyverse)
library(lubridate)
library(rvest)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#
#
#
# extract GCs for stage races
#
#

GC_races <- dbGetQuery(con, "SELECT * FROM pcs_all_races") %>%
  filter(year > 2013) %>%
  filter(str_sub(Class, 1,1) == "2") %>%
  mutate(file_name = paste0("GC-", str_replace_all(url, "/", "")))
  #filter(str_sub(url, 6, nchar(url)-5) %in% c("vuelta-a-espana", "giro-d-italia", "tour-de-france", "volta-a-catalunya", 
                                              # "itzulia-basque-country", "tour-de-romandie", "tour-de-suisse", 
                                              # "tour-of-california", "tirreno-adriatico", "uae-tour", 'paris-nice', 
                                              # 'ruta-del-sol', 'volta-ao-algarve', 'la-route-d-occitanie',
                                              # 'tour-cycliste-international-la-provence', 'vuelta-a-la-comunidad-valenciana',
                                              # 'criterium-international', 'tour-of-the-alps', 'vuelta-a-burgos',
                                              # 'tour-of-oman', 'tour-de-san-luis', 
                                              # 'vuelta-ciclista-a-la-provincia-de-san-juan',
                                              # 'colombia-21', 'dauphine'))

#

PCS_HTML <- fs::dir_info("D:/Jake/Documents/PCS-HTML/") %>%
  mutate(file_name = str_replace(path, "D:/Jake/Documents/PCS-HTML/", "")) %>%
  filter(str_sub(file_name,1,2) == "GC") %>%
  unique()

# 

need_to_scrape <- GC_races %>%
  filter(!file_name %in% PCS_HTML$file_name) %>%
  filter(!str_detect(url, "race/hammer")) %>%
  unique() %>%
  mutate(url = ifelse(str_detect(url, "/binckbank-tour/"), str_replace(url, "/binckbank-tour/", "/benelux-tour/"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/tour-du-haut-var/"), str_replace(url, "/tour-du-haut-var/", "/tour-des-alpes-maritimes-et-du-var/"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/driedaagse-vd-panne/"), str_replace(url, "/driedaagse-vd-panne/", "/oxyclean-classic-brugge-de-panne/"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/uae-tour/2017"), str_replace(url, "/uae-tour/2017", "/abu-dhabi-tour/2017"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/uae-tour/2018"), str_replace(url, "/uae-tour/2018", "/abu-dhabi-tour/2018"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/uae-tour/2016"), str_replace(url, "/uae-tour/2016", "/abu-dhabi-tour/2016"), url)) %>%
  mutate(url = ifelse(str_detect(url, "/uae-tour/2015"), str_replace(url, "/uae-tour/2015", "/abu-dhabi-tour/2015"), url))

#

dl_html = TRUE

for(g in 1:length(need_to_scrape$file_name)) {
  
  f_name <- need_to_scrape$file_name[[g]]
  URL <- paste0("https://www.procyclingstats.com/", need_to_scrape$url[[g]] , "/gc/result/result")
  
  if(dl_html == TRUE) {
    
    download.file(URL, f_name, quiet = TRUE)
    
  }
  
  page <- read_file(f_name) %>%
    read_html()
  
  stage_chars <- page %>% html_nodes('ul.infolist') %>% html_text() %>% enframe(name = NULL) %>%
    separate(value, paste0("v", seq(1,15,1)), sep = "\r\n") %>% gather(stat, value) %>% select(-stat) %>%
    separate(value, c("stat", "value"), sep = ":") %>%
    mutate(stat = str_trim(stat), value = str_trim(value))
  
  DATE <- stage_chars %>%
    filter(stat == "Date") %>%
    select(value) %>%
    .[[1]] %>%
    lubridate::dmy()
  
  RACE <- page %>% html_nodes('div.main') %>% html_nodes('h1') %>% html_text()
  
  tables <- page %>%
    html_nodes('div.result-cont') %>%
    html_nodes('table') %>%
    html_table()
  
  result_cont <- page %>%
    html_nodes('ul.restabs') %>%
    html_nodes('a') %>%
    html_attr(name = 'href') %>%
    enframe(name = NULL) %>%
    rowid_to_column() %>%
    filter(str_detect(value, "/gc")) %>%
    select(rowid) %>%
    .[[1]]
  
  if(f_name == "GC-raceherald-sun-tour2014") {
    result_cont = 1
  }
  
  df <- tables[[result_cont]] %>%
    
    janitor::clean_names() %>%
    
    select(rnk, rider, team, pnt) %>%
    
    mutate(url = need_to_scrape$url[[g]],
           race = RACE,
           date = DATE,
           year = need_to_scrape$year[[g]])
  
  dbWriteTable(con, "pcs_gc_results", df, append=T, row.names = F)
  
  rm(df)
  
  Sys.sleep(runif(1,2,6))
  
  print(paste0(RACE,g))
  
}

#
#
#

pcs_gc_results <- dbReadTable(con, "pcs_gc_results") %>%
  
  filter(year >= 2014) %>%
  
  mutate(pnt = ifelse(is.na(pnt), 0, pnt)) %>%  
  mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team)),
         rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT"),
         rider = str_to_title(rider),
         race = str_to_lower(race)) %>%
  mutate(date = ifelse(url == "race/tour-de-langkawi/2020", "2020-02-08",
                           ifelse(url == "race/sibiu-cycling-tour/2019", '2019-08-01', date)),
         date = as.Date(date)) %>%
  mutate(pnt = ifelse(rnk > 25, 0, pnt)) %>%
  
  mutate(present = 1) %>%
  
  right_join(dbGetQuery(con, "SELECT DISTINCT url, year, rider
                        FROM stage_data_perf
                        WHERE year > 2013 AND class IN ('2.1', '2.HC', '2.UWT', '2.2U', '2.2', '2.Ncup', '2.Pro')") %>%
              mutate(url = ifelse(str_detect(url, "/binckbank-tour/"), str_replace(url, "/binckbank-tour/", "/benelux-tour/"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/tour-du-haut-var/"), str_replace(url, "/tour-du-haut-var/", "/tour-des-alpes-maritimes-et-du-var/"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/driedaagse-vd-panne/"), str_replace(url, "/driedaagse-vd-panne/", "/oxyclean-classic-brugge-de-panne/"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2017"), str_replace(url, "/uae-tour/2017", "/abu-dhabi-tour/2017"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2018"), str_replace(url, "/uae-tour/2018", "/abu-dhabi-tour/2018"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2016"), str_replace(url, "/uae-tour/2016", "/abu-dhabi-tour/2016"), url)) %>%
              mutate(url = ifelse(str_detect(url, "/uae-tour/2015"), str_replace(url, "/uae-tour/2015", "/abu-dhabi-tour/2015"), url)) %>%
              mutate(data = 1)) %>%
  
  mutate(pnt = ifelse(is.na(pnt), 0, pnt)) %>%
  
  # adjust for U23
  mutate(pnt = ifelse(str_sub(url, 1, nchar(url)-6) %in% c("race/giro-ciclistico-d-italia", "race/tour-de-l-avenir"), pnt*4, pnt)) %>%
  
  group_by(url) %>%
  mutate(date = max(date, na.rm = T)) %>%
  ungroup() %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT DISTINCT stage, url, year, pred_climb_difficulty
                        FROM stage_data_perf
                        WHERE year > 2013 AND time_trial = 0 AND 
               class IN ('2.1', '2.HC', '2.UWT', '2.2U', '2.2', '2.Ncup', '2.Pro')") %>%
      group_by(url, year) %>%
      summarize(avg_pcd = mean(pred_climb_difficulty, na.rm = T),
                total_stages = n()) %>%
      ungroup() %>%
      mutate(url = ifelse(str_detect(url, "/binckbank-tour/"), str_replace(url, "/binckbank-tour/", "/benelux-tour/"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/tour-du-haut-var/"), str_replace(url, "/tour-du-haut-var/", "/tour-des-alpes-maritimes-et-du-var/"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/driedaagse-vd-panne/"), str_replace(url, "/driedaagse-vd-panne/", "/oxyclean-classic-brugge-de-panne/"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2017"), str_replace(url, "/uae-tour/2017", "/abu-dhabi-tour/2017"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2018"), str_replace(url, "/uae-tour/2018", "/abu-dhabi-tour/2018"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2016"), str_replace(url, "/uae-tour/2016", "/abu-dhabi-tour/2016"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2015"), str_replace(url, "/uae-tour/2015", "/abu-dhabi-tour/2015"), url))
    
  ) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT DISTINCT stage, url, year, length
                        FROM stage_data_perf
                        WHERE year > 2013 AND time_trial = 1 AND 
               class IN ('2.1', '2.HC', '2.UWT', '2.2U', '2.2', '2.Ncup', '2.Pro')") %>%
      group_by(url, year) %>%
      summarize(time_trial_kms = sum(length, na.rm = T),
                total_TT = n()) %>%
      ungroup() %>%
      mutate(url = ifelse(str_detect(url, "/binckbank-tour/"), str_replace(url, "/binckbank-tour/", "/benelux-tour/"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/tour-du-haut-var/"), str_replace(url, "/tour-du-haut-var/", "/tour-des-alpes-maritimes-et-du-var/"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/driedaagse-vd-panne/"), str_replace(url, "/driedaagse-vd-panne/", "/oxyclean-classic-brugge-de-panne/"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2017"), str_replace(url, "/uae-tour/2017", "/abu-dhabi-tour/2017"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2018"), str_replace(url, "/uae-tour/2018", "/abu-dhabi-tour/2018"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2016"), str_replace(url, "/uae-tour/2016", "/abu-dhabi-tour/2016"), url)) %>%
      mutate(url = ifelse(str_detect(url, "/uae-tour/2015"), str_replace(url, "/uae-tour/2015", "/abu-dhabi-tour/2015"), url))
    
  ) %>%
  
  mutate(time_trial_kms = ifelse(is.na(time_trial_kms), 0, time_trial_kms),
         total_TT = ifelse(is.na(total_TT), 0, total_TT)) %>%
  
  group_by(url, year) %>%
  mutate(max_pnt = max(pnt, na.rm = T)) %>%
  ungroup()

#
#
#

store_list <- vector("list", 400)

y <- pcs_gc_results %>%
  
  select(date) %>%
  unique() %>%
  
  filter(date > '2016-01-01') %>%
  .[[1]]

for(x in 1:length(y)) {
  
  D <- as.Date(y[[x]])
  
  gc_ratings <- pcs_gc_results %>%
    filter(date > (D-1100) & date < D) %>%

    mutate(wt = (1 / (as.numeric(D - date) + 365))/0.00273,
           wtd = wt*pnt) %>%
    
    group_by(rider) %>%
    mutate(t7 = ifelse(rank(desc(pnt), ties.method = "first") <= 7, pnt, NA),
           t7_wtd = ifelse(rank(desc(wtd), ties.method = "first") <= 7, wtd, NA)) %>%
    ungroup() %>%
    
    group_by(rider) %>%
    summarize(avg = mean(pnt, na.rm = T),
              top7 = mean(t7, na.rm = T),
              max = mean(max_pnt, na.rm = T),
              wtd = mean(wtd, na.rm = T),
              top7_wtd = mean(t7_wtd, na.rm = T),
              races = n()) %>%
    ungroup() %>%
    
    mutate(perc_max = avg/max)
  
  store_list[[x]] <- gc_ratings %>%
    mutate(D = D)
  
}

#

grand_tour_gc_ratings <- bind_rows(store_list) %>%
  
  mutate(year = lubridate::year(D)) %>%
  
  filter(!is.na(rider))

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

New_GC_races <- expand_grid(
  
  url = c("vuelta-a-espana", "giro-d-italia", "tour-de-france", "volta-a-catalunya", 
          "itzulia-basque-country", "tour-de-romandie", "tour-de-suisse", 
          "tour-of-california", "tirreno-adriatico", 'paris-nice', 
          'ruta-del-sol', 'volta-ao-algarve', 'la-route-d-occitanie',
          'tour-cycliste-international-la-provence', 'vuelta-a-la-comunidad-valenciana',
          'criterium-international', 'tour-of-the-alps', 'vuelta-a-burgos',
          'tour-of-oman','dauphine', 'tour-de-l-avenir'
  ),
  
  year = seq(1980,2013,1)
  
) %>%
  
  mutate(url = paste0("race/", url, "/", year))

#

GC_all <- GC_races %>%
  
  select(url, year) %>%
  
  rbind(New_GC_races) %>%
  
  #filter(url %in% as.list(dbGetQuery(con, "SELECT url FROM pcs_gc_results_OLD") %>% unique() %>% .[[1]])) %>%
  
  rbind(
    
    tibble(url = 'euskal-bizikleta',
           year = seq(1987,2004,1)) %>%
      mutate(url = paste0("race/", url, "/", year)),
    tibble(url = 'tour-mediterraneen',
           year = seq(1980,2011,1)) %>%
      mutate(url = paste0("race/", url, "/", year)),
    tibble(url = 'gp-du-midi-libre',
           year = seq(1980,2002,1)) %>%
      mutate(url = paste0("race/", url, "/", year)),
    tibble(url = 'setmana-catalana',
           year = seq(1981,2005,1)) %>%
      mutate(url = paste0("race/", url, "/", year)),
    tibble(url = 'vuelta-ciclista-a-la-region-de-murcia',
           year = seq(1985,2013,1)) %>%
      mutate(url = paste0("race/", url, "/", year)),
    tibble(url = 'circuit-sarthe',
           year = seq(1980,2013,1)) %>%
      mutate(url = paste0("race/", url, "/", year)),
    tibble(url = 'vuelta-a-castilla-y-leon',
           year = seq(1985,2013,1)) %>%
      mutate(url = paste0("race/", url, "/", year)),
    tibble(url = 'vuelta-asturias',
           year = seq(1980,2013,1)) %>%
      mutate(url = paste0("race/", url, "/", year)),
    tibble(url = 'vuelta-ciclista-a-la-rioja',
           year = seq(1980,2008,1)) %>%
      mutate(url = paste0("race/", url, "/", year)),
    tibble(url = 'coors-classic',
           year = seq(1980,1988,1)) %>%
      mutate(url = paste0("race/", url, "/", year)),
    tibble(url = 'tour-du-pont',
           year = seq(1989,1996,1)) %>%
      mutate(url = paste0("race/", url, "/", year))
  )

#

dl_html <- TRUE

if(dl_html == TRUE) {
  
  GC_all <- GC_all %>%
    
    filter(!(url %in% as.list(dbGetQuery(con, "SELECT url FROM pcs_gc_results") %>% unique() %>% .[[1]]))) %>%
    filter(!(str_detect(url, "tour-of-california") & (year < 2006))) %>%
    filter(!(str_detect(url, "tour-cycliste-international-la-provence") & (year < 2014))) %>%
    filter(!(str_detect(url, "vuelta-a-la-comunidad-valenciana") & (year < 2016) & (year > 2008))) %>%
    filter(!(str_detect(url, "vuelta-ciclista-a-la-region-de-murcia") & (year < 2019) & (year > 2012))) %>%
    filter(!(str_detect(url, "tour-of-the-alps") & (year == 1986))) %>%
    filter(!(str_detect(url, "vuelta-a-burgos") & (year == 1980))) %>%
    filter(!(str_detect(url, "vuelta-a-burgos") & (year == 2006))) %>%
    filter(!(str_detect(url, "tour-de-l-avenir") & (year == 1991))) %>%
    filter(!(str_detect(url, "tour-of-oman") & (year < 2010))) %>%
    filter(!(str_detect(url, "vuelta-a-castilla-y-leon") & (year == 1990))) %>%
    filter(!(str_detect(url, "vuelta-ciclista-a-la-rioja") & (year > 2008)))
  
}

#
#
#

for(g in 1:length(GC_all$url)) {
  
  f_name <- paste0("PCS-HTML/", "GC-", str_replace_all(GC_all$url[[g]], "/", ""))
  
  if(dl_html == TRUE) {
    
    download.file(paste0("https://www.procyclingstats.com/", GC_all$url[[g]] , "/result/result"), f_name, quiet = TRUE)
    
  }
  
  page <- read_file(f_name) %>%
    read_html()
  
  DATE <- page %>% html_nodes('div.content') %>% html_nodes('div.res-right') %>% html_text()
  
  x <- str_locate(DATE, 'Date:') %>% .[[2]]
  y <- str_locate(DATE, toString(GC_all$year[[g]])) %>% .[[2]]
  
  DATE <- str_sub(DATE, x+1, y) %>% str_trim()
  
  RACE <- page %>% html_nodes('div.entry') %>% html_nodes('h1') %>% html_text()
  
  tables <- page %>%
    html_nodes('div.resultCont ') %>%
    html_nodes('table') %>%
    html_table()
  
  if(GC_all$url[[g]] %in% c('race/volta-a-catalunya/1993',
                            'race/criterium-international/2000',
                            'race/criterium-international/2002',
                            'race/criterium-international/2003',
                            'race/criterium-international/2004',
                            'race/criterium-international/2005',
                            'race/criterium-international/2006',
                            'race/criterium-international/2007',
                            'race/criterium-international/2008',
                            'race/criterium-international/2009',
                            'race/criterium-international/2010',
                            'race/criterium-international/2011',
                            'race/criterium-international/2012',
                            'race/tour-de-france/1987',
                            'race/tour-de-l-avenir/1988',
                            'race/tour-mediterraneen/2006',
                            'race/vuelta-asturias/1986',
                            'race/circuit-sarthe/2000',
                            'race/setmana-catalana/1992')) {
    
    n = 3
    
  } else if(GC_all$url[[g]] %in% c('race/itzulia-basque-country/2002',
                                   'race/itzulia-basque-country/2003',
                                   'race/tour-de-romandie/1991',
                                   'race/tour-de-romandie/1992',
                                   'race/tour-de-romandie/1993',
                                   'race/tour-de-romandie/1994',
                                   'race/tour-de-romandie/1995',
                                   'race/tour-de-suisse/1992',
                                   'race/tour-de-suisse/1993',
                                   'race/la-route-d-occitanie/2007',
                                   'race/tour-mediterraneen/1993',
                                   'race/tour-mediterraneen/1994',
                                   'race/tour-mediterraneen/1995',
                                   'race/tour-mediterraneen/1996',
                                   'race/tour-mediterraneen/1998',
                                   'race/tour-mediterraneen/2003',
                                   'race/circuit-sarthe/1983')) {
    
    n = 1
    
  } else if(GC_all$url[[g]] %in% c('race/itzulia-basque-country/2002',
                                   'race/volta-ao-algarve/2012',
                                   'race/tour-of-oman/2012')) {
    
    n = 4
    
  } else {
    
    n = 2
    
  }
  
  df <- tables[[n]] %>%
    
    janitor::clean_names() %>%
    
    select(rnk, rider, team, pnt, time_won_lost) %>%
    select(rnk, rider, team, pnt) %>%
    
    mutate(url = GC_all$url[[g]],
           race = RACE,
           date = DATE,
           year = GC_all$year[[g]])
  
  dbWriteTable(con, "pcs_gc_results", df, append=T, row.names = F)
  
  rm(df)
  
}

#

pcs_gc_results <- dbReadTable(con, "pcs_gc_results") %>%
  
  group_by(url, year) %>%
  mutate(printed_position = rank(url, ties.method = "first")) %>%
  ungroup() %>%
  
  ## if you would've received points based on expunged result, we give them back
  ## this is to make an accurate recreation of favorites at each point in time
  mutate(rnk = ifelse(is.na(rnk) & printed_position < 16, printed_position, rnk),
         rnk = ifelse(is.na(rnk), 199, rnk)) %>%
  
  select(-printed_position) %>%
  
  mutate(pnt = ifelse(is.na(pnt), 0, pnt)) %>%  
  mutate(rider = str_sub(rider, 1, nchar(rider)-nchar(team))) %>% 
  mutate(rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
  mutate(r = str_sub(url, 6, nchar(url)-5)) %>%
  
  mutate(class = ifelse(r %in% c('tour-de-france'), "A",
                        ifelse(r %in% c('vuelta-a-espana', 'giro-d-italia'), "B",
                               ifelse(r %in% c("tour-de-suisse", "dauphine", 'paris-nice'), "C",
                                      ifelse(r %in% c("tour-de-romandie", "itzulia-basque-country",
                                                      'volta-a-catalunya', 'tirreno-adriatico',
                                                      'criterium-international'), "D", "E"))))) %>%
  
  left_join(
    
    read_csv("my-gc-points.csv"), by = c("rnk", "class")
    
  ) %>%
  
  mutate(my_points = ifelse(is.na(my_points), 0, my_points),
         date = dmy(date)) %>%
  unique()

#
# leading up to date
#

store_list <- vector("list", 150)

y <- pcs_gc_results %>%
  
  filter(r %in% c("vuelta-a-espana", "tour-de-france", "giro-d-italia")) %>%
  
  select(date) %>%
  unique() %>%
  
  filter(date > '1983-01-01') %>%
  mutate(date = date - 24) %>%
  .[[1]]

for(x in 1:length(y)) {
  
  D <- as.Date(y[[x]])
  
  gc_ratings <- pcs_gc_results %>%
    filter(date > (D-1825) & date < D) %>%
    
    mutate(wt = (1 / ((as.numeric(D - date)) + 730)) * 731,
           wtd = wt*my_points) %>%
    
    group_by(rider) %>%
    filter(rank(-wtd, ties.method = "first") < 6) %>%
    ungroup() %>%
    
    group_by(rider) %>%
    summarize(tot = sum(wtd, na.rm = T),
              best = max(wtd, na.rm = T),
              n = n()) %>%
    ungroup()
  
  store_list[[x]] <- gc_ratings %>%
    mutate(D = D)
  
}

#

grand_tour_gc_ratings <- bind_rows(store_list) %>%
  
  mutate(year = lubridate::year(D)) %>%
  
  rename(Best_GC_Points = best,
         Total_GC_Points = tot,
         Races = n,
         date = D)

dbWriteTable(con, "gc_grandtour_preranks", grand_tour_gc_ratings, row.names = F, append = TRUE)

#

gc_ratings_going_into_TDF <- bind_rows(store_list) %>%
  mutate(edition = year(D)) %>%
  inner_join(dbReadTable(con, "startlists") %>%
               unique() %>%
               filter(race == "tour-de-france"), by = c("rider", "edition" = "year")) %>%
  group_by(edition) %>%
  
  mutate(tot = ifelse(n < 5, (tot)/5, tot)) %>%
  
  mutate(rk = rank(-tot, ties.method = "first"))

#

eval_tdf_perf <- gc_ratings_going_into_TDF %>%
  
  left_join(
    
    pcs_gc_results %>%
      select(year, url, rider, rnk) %>%
      filter(str_detect(url, "tour-de-france")), by = c("edition" = "year", "rider")
    
  ) %>%
  
  left_join(dbReadTable(con, "startlists") %>%
              unique() %>%
              filter(race == "giro-d-italia") %>%
              select(rider, year) %>%
              mutate(st=1), by = c("rider", "edition" = "year")) %>%
  
  mutate(giro = ifelse(is.na(st), 0, 1)) %>%
  
  mutate(podium = ifelse(is.na(rnk), 0,
                         ifelse(rnk < 4,1,0)),
         winner = ifelse(is.na(rnk), 0,
                         ifelse(rnk == 1,1,0))) %>%
  
  filter(edition>1991 & edition<2020) %>%
  
  group_by(team, edition) %>%
  mutate(tm_leader = rank(-tot, ties.method = "min")) %>%
  ungroup() %>%
  
  mutate(tm_leader = ifelse(tm_leader == 1, 1, 0)) %>%
  
  group_by(edition) %>%
  mutate(tot_behind = max(tot, na.rm = T) - tot,
         best_behind = max(best, na.rm = T) - best) %>%
  ungroup()

#

podium <- glm(podium ~ log(rk) + best_behind + giro, data = eval_tdf_perf, family = "binomial")

summary(podium)

win <- glm(winner ~ (log(rk)) + best_behind + giro, data = eval_tdf_perf, family = "binomial")

summary(win)

# correlation between GCs

corr_gc_tdf <- pcs_gc_results %>%
  
  unique() %>%
  
  group_by(rider) %>%
  filter(n()>14) %>%
  filter(min(rnk, na.rm = T)==1) %>%
  ungroup() %>%
  
  select(year, rider, rnk, url) %>%
  
  inner_join(
    
    pcs_gc_results %>%
      
      unique() %>%
      
      filter(str_detect(url, "tour-de-france")) %>%
      select(year, rider, rnk, url), by = c("rider")
    
  ) %>%
  
  filter(url.x != url.y) %>%
  rename(url = url.x) %>%
  
  mutate(race = str_sub(url, 6, nchar(url)-5)) %>%
  
  group_by(race) %>%
  summarize(cor = cor(x = rnk.x, y = rnk.y, method = "spearman"))

#

y = seq(2019, 2019, 1)

for(x in 1:length(y)) {
  
  url <- paste0("https://www.procyclingstats.com/race/tour-de-france/", y[[x]] , "/startlist/alphabetical-with-filters") %>%
    read_html()
  
  st <- url %>%
    html_nodes('table') %>%
    html_table() %>%
    .[[1]] %>%
    .[, 1:3] %>%
    select(rider = Ridername,
           team = Team,
           bib = `#`) %>%
    mutate(rider = iconv(rider, from="UTF-8", to = "ASCII//TRANSLIT"),
           year = y[[x]],
           race = 'tour-de-france')
  
  dbWriteTable(con, "pcs_startlists", st, overwrite = T, row.names = F)
  
}

#
#
#
#
#
#
#
#
#

# compare GC results

pcs_gc_results %>% 
  filter(rnk < 200 & rnk > 0) %>%
  select(rnk, rider, url, race, year) %>% 
  unique() %>% 
  
  inner_join(pcs_gc_results %>% 
               filter(rnk < 200 & rnk > 0) %>%
               select(rnk, rider, url, race, year) %>% 
               unique(), by = c("year", "rider")) %>% 
  
  filter(!(url.x == url.y)) %>% 
  
  separate(url.x, c("junk", "url_race.x", "y"), sep = "\\/") %>% 
  select(-junk, -y) %>% 
  
  separate(url.y, c("junk", "url_race.y", "y"), sep = "\\/") %>%
  select(-junk, -y) -> joined_gc

#

remove_dnfs <- joined_gc %>% 
  filter(!(is.na(rnk.x)) | !(is.na(rnk.y))) %>% 
  
  mutate(log.x = log(rnk.x), log.y = log(rnk.y)) %>% 
  
  mutate(diff = log.x - log.y)

#

keep_dnfs <- joined_gc %>% 
  group_by(year, url_race.x) %>%
  filter(!max(rnk.x, na.rm = T) %in% c(Inf, -Inf)) %>%
  mutate(rnk.x = ifelse(is.na(rnk.x), max(rnk.x, na.rm = T)+1, rnk.x)) %>%
  ungroup() %>%
  
  group_by(year, url_race.y) %>%
  filter(!max(rnk.y, na.rm = T) %in% c(Inf, -Inf)) %>%
  mutate(rnk.y = ifelse(is.na(rnk.y), max(rnk.y, na.rm = T)+1, rnk.y)) %>%
  ungroup() %>%
  
  mutate(log.x = log(rnk.x), log.y = log(rnk.y)) %>% 
  
  mutate(diff = log.x - log.y)


#

compared_GC <- remove_dnfs %>%
  
  group_by(url_race.x, url_race.y) %>%
  summarize(mean_diff = mean(diff, na.rm = T),
            comps = n()) %>%
  ungroup() %>%
  
  # calculate difference as if race.y had finished 5th
  mutate(transformed_5th = exp(mean_diff+1.6),
         transformed_25th = exp(mean_diff+3.2))

#

compared_GC_w_dnfs <- keep_dnfs %>%
  
  group_by(url_race.x, url_race.y) %>%
  summarize(mean_diff = mean(diff, na.rm = T),
            comps = n()) %>%
  ungroup() %>%
  
  # calculate difference as if race.y had finished 5th
  mutate(transformed_5th = exp(mean_diff+1.6),
         transformed_25th = exp(mean_diff+3.2))

#

correlated_GC <- remove_dnfs %>%
  
  group_by(race.x, race.y) %>%
  summarize(corr = cor(log.x, log.y, use = 'pairwise.complete.obs', method = "spearman"),
            comps = n()) %>%
  ungroup()

#
#
#

UCI_point_scales <- keep_dnfs %>%
  
  select(url_race.x, year) %>%
  unique() %>%
  
  anti_join(dbGetQuery(con, "SELECT DISTINCT url, year FROM uci_gc_points"), by = c("url_race.x" = "url", 'year')) %>%
  
  mutate(url_pull = paste0("race", url_race.x, year)) %>%
  
  mutate(url_pull = case_when(url_race.x == "abu-dhabi-tour" & year <= 2018 ~ paste0("raceuae-tour", year),
                              url_race.x == "benelux-tour" & year <= 2020 ~ paste0("racebinckbank-tour", year),
                              url_race.x == "tour-des-alpes-maritimes-et-du-var" & year <= 2020 ~ paste0("racetour-du-haut-var", year),
                              url_race.x == "oxyclean-classic-brugge-de-panne" & year <= 2017 ~ paste0("racedriedaagse-vd-panne", year),
  
                              TRUE ~ url_pull)) %>%
  
  filter(!url_pull %in% c('raceherald-sun-tour2014', 'raceadriatica-ionica-race2018'))

#

for(g in 1:length(UCI_point_scales$url_pull)) {
  
  f_name <- paste0("D:/Jake/Documents/PCS-HTML/GC-", UCI_point_scales$url_pull[[g]])
  
  page <- read_file(f_name) %>%
    read_html()
  
  stage_chars <- page %>% html_nodes('ul.infolist') %>% html_text() %>% enframe(name = NULL) %>%
    separate(value, paste0("v", seq(1,15,1)), sep = "\r\n") %>% gather(stat, value) %>% select(-stat) %>%
    separate(value, c("stat", "value"), sep = ":") %>%
    mutate(stat = str_trim(stat), value = str_trim(value))
  
  DATE <- stage_chars %>%
    filter(stat == "Date") %>%
    select(value) %>%
    .[[1]] %>%
    lubridate::dmy()
  
  RACE <- page %>% html_nodes('div.main') %>% html_nodes('h1') %>% html_text()
  
  tables <- page %>%
    html_nodes('div.result-cont') %>%
    html_nodes('table') %>%
    html_table()
  
  result_cont <- page %>%
    html_nodes('ul.restabs') %>%
    html_nodes('a') %>%
    html_attr(name = 'href') %>%
    enframe(name = NULL) %>%
    rowid_to_column() %>%
    filter(str_detect(value, "/gc")) %>%
    select(rowid) %>%
    .[[1]]
  
  if(f_name == "GC-raceherald-sun-tour2014") {
    result_cont = 1
  }
  
  df <- tables[[result_cont]] %>%
    
    janitor::clean_names() %>%
    
    select(rnk, uci) %>%
    
    mutate(url = UCI_point_scales$url_race.x[[g]],
           year = UCI_point_scales$year[[g]]) %>%
    filter(!is.na(uci))
  
  dbWriteTable(con, "uci_gc_points", df, append=TRUE, row.names = F)
  
  print(paste0(RACE,g))
  
}

#
#
#

UCI_point_results <- dbReadTable(con, "uci_gc_points") %>%
  
  group_by(year) %>%
  mutate(relative = uci / mean(uci, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(url, rnk) %>%
  summarize(uci_relative = mean(relative, na.rm = T)) %>%
  ungroup()

#

compared_GC_w_dnfs %>%
  filter(comps >= 50) %>% 
  
  inner_join(UCI_point_results, by = c("url_race.x" = "url")) %>%
  inner_join(UCI_point_results %>% filter(rnk == 5), by = c("url_race.y" = "url")) %>% 
  
  filter(rnk.x >= (transformed_5th - 1) & rnk.x <= (transformed_5th + 1)) %>% 
  
  group_by(url_race.x, url_race.y, comps, transformed_5th) %>%
  summarize(uci_relative.x = mean(uci_relative.x), 
            uci_relative.y = mean(uci_relative.y), 
            rnk.x = mean(rnk.x)) %>% 
  ungroup() %>%
  rename(points_race_y = uci_relative.y,
         points_race_x = uci_relative.x) %>% 
  
  mutate(advantage_race_y = points_race_y / points_race_x) -> joined_with_UCI

#

joined_with_UCI %>%
  group_by(url_race.y) %>%
  summarize(rel_adv = mean(advantage_race_y),
            comps = n()) %>%
  ungroup() %>%
  filter(comps >= 15) %>%
  arrange(rel_adv)

joined_with_UCI %>%
  group_by(url_race.y) %>%
  summarize(rel_adv = mean(advantage_race_y),
            comps = n()) %>%
  ungroup() %>%
  filter(comps >= 15) %>%
  arrange(desc(rel_adv))

#
#
#
#
#
#
#
#

gc_fields_by_race <- pcs_gc_results %>%
  
  select(rider, url, year, date) %>%
  unique() %>%
  
  inner_join(grand_tour_gc_ratings %>%
               select(D, rider, top7_wtd, perc_max, races), by = c("date" = "D", "rider"))

#

gc_fields_by_race %>% 
  group_by(url, year) %>%
  filter(rank(desc(top7_wtd), ties.method = "first") <= 25) %>%
  summarize(avg = mean(top7_wtd), 
            riders = n()) %>% 
  ungroup() %>% 
  
  group_by(year) %>% 
  mutate(rel = avg/mean(avg, na.rm = T)) %>%
  ungroup() -> best_gc_fields

#

gc_races_ranked <- best_gc_fields %>%
  
  separate(url, c("R", "race", "Y"), sep = "\\/") %>%
  
  group_by(race_url = race) %>%
  summarize(avg_rel = mean(rel, na.rm = T),
            races = n()) %>%
  ungroup()

#

joined_with_UCI %>%
  group_by(url_race.y) %>%
  summarize(rel_adv = mean(advantage_race_y),
            comps = n()) %>%
  ungroup() %>%
  filter(comps >= 15) %>%
  
  inner_join(gc_races_ranked, by = c("url_race.y" = "race_url")) %>%
  
  ggplot(aes(x = avg_rel, y = rel_adv, label = url_race.y))+

  geom_hline(yintercept = 1.3, color = "gray65", size = 2)+
  #geom_smooth(se=F, method = "lm", color = "gray65", size=2)+  
  geom_text()+
  labs(x = "strength of GC field",
       y = "points advantage in race vs others",
       title = "Grand tours have strongest fields and largest rewards")

#
#
#
#
#

predictiveness_gc_rankings <- pcs_gc_results %>%
  
  select(rider, url, year, date, rnk) %>%
  unique() %>%
  
  inner_join(grand_tour_gc_ratings %>%
               select(D, rider, top7_wtd, perc_max, races), by = c("date" = "D", "rider")) %>%
  
  group_by(url, year) %>%
  mutate(behind_best = max(top7_wtd, na.rm = T) - top7_wtd,
         gc_rnk = rank(desc(top7_wtd), ties.method = "min")) %>%
  ungroup()

#

gc_mod_preds <- predictiveness_gc_rankings %>%
  mutate(rnk = ifelse(is.na(rnk), 200, rnk)) %>% 
  filter(rnk <= 200 & rnk > 0) %>% 
  mutate(win = ifelse(rnk==1,1,0)) %>% 
  
  glm(win ~ top7_wtd + log(gc_rnk), data = ., family = "binomial")

#

applied_predictions <- predictiveness_gc_rankings %>%

  mutate(rnk = ifelse(is.na(rnk), 200, rnk)) %>% 
  filter(rnk <= 200 & rnk > 0) %>% 
  mutate(win = ifelse(rnk==1,1,0)) %>%
  
  mutate(pred = predict(gc_mod_preds, .),
         pred = exp(pred)/(1+exp(pred))) %>%
  
  group_by(url, year) %>%
  mutate(pred = pred - min(pred, na.rm = T)) %>%
  ungroup() %>%
  
  #group_by(url, team, year) %>%
  #mutate(tmrnk = rank(desc(pred), ties.method = "first"),
  #       total = sum(pred),
  #       pred = ifelse(tmrnk <= 2, pred/total, 0)) %>%
  #ungroup() %>%
  
  group_by(url, year) %>%
  mutate(pred = pred / sum(pred)) %>%
  ungroup()
