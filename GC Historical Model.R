

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
  filter(str_sub(url, 6, nchar(url)-5) %in% c("vuelta-a-espana", "giro-d-italia", "tour-de-france", "volta-a-catalunya", 
                                              "itzulia-basque-country", "tour-de-romandie", "tour-de-suisse", 
                                              "tour-of-california", "tirreno-adriatico", "uae-tour", 'paris-nice', 
                                              'ruta-del-sol', 'volta-ao-algarve', 'la-route-d-occitanie',
                                              'tour-cycliste-international-la-provence', 'vuelta-a-la-comunidad-valenciana',
                                              'criterium-international', 'tour-of-the-alps', 'vuelta-a-burgos',
                                              'tour-of-oman', 'tour-de-san-luis', 
                                              'vuelta-ciclista-a-la-provincia-de-san-juan',
                                              'colombia-21', 'dauphine'))

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
