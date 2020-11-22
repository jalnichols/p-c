
library(tidyverse)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

# PCS FR Matcher

pcs <- dbGetQuery(con, "SELECT year, race, date, class, max(stage) as stages 
                  FROM pcs_stage_raw GROUP BY race, year, date, class") %>%
  
  filter(!is.na(class)) %>%
  
  mutate(date = as.Date(date),
         class = ifelse(class == "UWT",
                        ifelse(stages > 1, "2.UWT", "1.UWT"), class),
         class = ifelse(class == "1.2U23", "1.2U", class)) %>%
  
  group_by(race, year) %>%
  mutate(date = min(date, na.rm = T)) %>%
  ungroup() %>%
  
  unique() %>%
  
  filter(!race %in% c("World Championships WE - Road Race", "World Championships WJ - Road Race",
                      "World Championships U23 - Road Race", "World Championships MJ - ITT",
                      "World Championships WE - ITT", "World Championships WJ - ITT",
                      "World Championships MJ - Road Race", "World Championships U23 - ITT",
                      "Tour of Almaty", "Tour de Taiwan", "Tour of Japan", "Tour de Korea",
                      "Tour of Antalya", "Tour of China I", "Tour of China II", "Tour of Indonesia",
                      "Tour of Fuzhou", "Tour of Iran (Azarbaijan)", "Tour of Peninsular",
                      "Tour of Taihu Lake", "UEC Road European Championships - ITT")) %>%
  filter(year > 2018) %>%
  filter(date <= '2020-12-01') %>%
  
  filter(!class == 'NC')

#

fr <- dbGetQuery(con, "SELECT race, year, date FROM fr_stages GROUP BY race, year, date") %>%
  
  inner_join(dbReadTable(con, "fr_races")) %>%
  
  filter(year > 2018) %>%
  
  select(date, race, year, class, tour, stages) %>%
  
  mutate(date = str_replace_all(date, "Monday", ""),
         date = str_replace_all(date, "Tuesday", ""),
         date = str_replace_all(date, "Wednesday", ""),
         date = str_replace_all(date, "Thursday", ""),
         date = str_replace_all(date, "Friday", ""),
         date = str_replace_all(date, "Saturday", ""),
         date = str_replace_all(date, "Sunday", ""),
         date = str_trim(date)) %>%
  separate(date, c("day", "month", "year2"), sep = " ") %>%
  inner_join(tibble(month = c("January","February","March","April","May","June",
                              "July","August","September","October","November","December"),
                    m = c("01","02","03","04","05","06","07","08","09","10","11","12")), by = c("month")) %>%
  mutate(date = as.Date(paste0(year2,"-",m,"-",day))) %>%
  select(race, year, date, class, tour) %>%
  filter(date <= '2020-12-01') %>%
  
  mutate(date = ifelse(year == 2016 & race == "Santos Tour Down Under", as.Date('2016-01-19'), date)) %>%
  
  filter(!race %in% c("UCI Road World Championships - ITT (Men Elite)")) %>%
  
  mutate(class = ifelse(class == "CM", "WC", class))

#

matches <- fr %>%
  
  # ignore races where I know I lack matches
  #filter(!str_detect(race, "Giro Ciclistico")) %>%
  #filter(!str_detect(race, "Avenir")) %>%
  
  mutate(year = as.numeric(year)) %>%
  rename(fr_race = race) %>%
  
  inner_join(
    
    pcs %>%
      rename(pcs_race = race), by = c("year", "class")
    
  ) %>%
  
  mutate(sd_DL = stringdist::stringdist(tolower(pcs_race), tolower(fr_race), method = "dl") / nchar(pcs_race),
         sd_QG = stringdist::stringdist(tolower(pcs_race), tolower(fr_race), method = "qgram", q = 2) / nchar(pcs_race)) %>%
  
  group_by(fr_race, year) %>%
  mutate(rankDL = rank(sd_DL, ties.method = "min"),
         rankQG = rank(sd_QG, ties.method = "min")) %>%
  ungroup() %>%
  
  mutate(hm = (sd_DL + sd_QG) / 2) %>%
  
  group_by(fr_race, year) %>%
  mutate(rankHM = rank(hm, ties.method = "min")) %>%
  ungroup() %>%
  
  group_by(fr_race) %>% 
  mutate(no1 = sum(rankHM == 1, na.rm = T)) %>%
  ungroup()

#

pcs_fr_matches <- matches %>%
  
  filter(rankHM == 1 |
           ((tolower(fr_race) == "euroeyes cyclassics") & (tolower(pcs_race) == "cyclassics hamburg"))) %>%
  
  rbind(
    
    matches %>%
      mutate(hm = 0.75) %>%
      inner_join(
        read_csv('fr_pcs_matches.csv') %>%
          filter(match == TRUE) %>%
          select(-match), by = c("fr_race" = "fr", "pcs_race" = "pcs", "year")
        
      )) %>%
  
  filter(hm < 0.751) %>%
  
  anti_join(
    
    read_csv('fr_pcs_matches.csv') %>%
      filter(match == FALSE) %>%
      select(-match), by = c("fr_race" = "fr", "pcs_race" = "pcs", "year")
    
  ) %>%
  
  filter(year > 2012) %>%
  
  filter(date.x < (date.y + 5) & date.x > (date.y - 5)) %>%
  
  unique() %>%
  
  select(fr_race, pcs_race, year) %>%
  
  rbind(read_csv('fr_pcs_matches.csv') %>%
          filter(match == TRUE) %>%
          select(-match) %>%
          rename(fr_race = fr,
                 pcs_race = pcs)) %>%
  unique()


#

all_routes <- dbGetQuery(con, "SELECT alt, dist, url FROM fr_route_data") %>%    
  mutate(distances = as.numeric(dist),
         alt = as.numeric(alt)) %>%
  
  group_by(url, dist) %>%
  summarize(alt = mean(alt, na.rm = T)) %>%
  ungroup() %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT DISTINCT url, race, race_url, year
               FROM fr_stage_urls WHERE year IN ('2020', '2019')") %>%
      group_by(race, year, race_url) %>%
      mutate(stage = rank(year, ties.method = "first")) %>%
      ungroup(), by = c("url")) %>%
  
  arrange(year, race, stage, dist) %>%
  
  group_by(stage, year, race) %>%
  mutate(points = rank(dist, ties.method = "first"),
         length = max(dist, na.rm = T),
         highest_point = max(alt, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(grades = (alt - lag(alt)) / (1000 * (dist - lag(dist))),
         grades = ifelse(dist == 0, 0, grades),
         grades = ifelse(grades > 0.25, 0.25,
                         ifelse(grades < -0.25, -0.25, grades))) %>%
  
  rename(elevations = alt) %>%
  mutate(year = as.numeric(year)) %>%
  rename(distances = dist)

#
#
#
#

koms_list_tdf <- vector("list", 21)

for(S in 1:21) {

tdf14_2020 <- all_routes %>%
  filter(year == 2019 & race == "Giro d'Italia" & stage == S) %>%
  inner_join(pcs_fr_matches, by = c("race" = "fr_race", "year")) %>%
  inner_join(pcs, by = c("pcs_race" = "race", "year")) %>%
  
  filter(class %in% c("1.UWT", "1.HC", "WC", "CC", "2.UWT", "2.HC", "2.Pro", "1.Pro")) %>%
  
  select(-highest_point, -points) %>%
  
  mutate(left_km = length - distances) %>%
  
  mutate(every_km2 = floor(left_km/0.1)/10) %>%

  group_by(every_km2, length, year, pcs_race, stage) %>%
  summarize(
    
    left_km = min(left_km, na.rm = T),
    elevations = mean(elevations, na.rm = T)
    
  ) %>%
  ungroup() %>%
  
  mutate(every_km = floor(every_km2/0.25)/4) %>%
  mutate(everytenth = floor(every_km2/0.1)/10) %>%
  
  mutate(every_km = ifelse(left_km < 3, everytenth, every_km),
         gradient = (elevations - lead(elevations)) / ((lead(left_km)-left_km)*1000)) %>%
  
  group_by(every_km, length, year, pcs_race, stage) %>%
  summarize(min = min(elevations, na.rm = T),
            max = max(elevations, na.rm = T),
            gradient = mean(gradient, na.rm = T),
            elevations = mean(elevations, na.rm = T),
  ) %>%
  ungroup() %>%
  
  mutate(race = tolower(pcs_race)) %>%
  
  select(-pcs_race) %>%
  
  arrange(-every_km) %>%
  
  mutate(change_gradient = ifelse(gradient > 0.035 | gradient < -0.035,
                                  ifelse(gradient > 0.035 & lag(gradient < 0.035), "uphill",
                                          ifelse(gradient < -0.035 & lag(gradient > -0.035), "downhill", NA)), "flat")) %>%
  
  fill(change_gradient, .direction = "down") %>%
  
  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
  
  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
  
  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
  
  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%

  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
  
  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
  
  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
  
  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
  
  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
  
  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
  
  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
  
  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
  
  mutate(every_km3 = floor(every_km)) %>%
  mutate(everytenth = floor(every_km/0.5)/2) %>%
  
  mutate(every_km3 = ifelse(every_km < 3, everytenth, every_km3)) %>%
  
  mutate(segment_distance = every_km - lead(every_km),
         segment_distance = ifelse(is.na(segment_distance), lag(segment_distance), segment_distance)) %>%
 
  group_by(every_km = every_km3, length, year, race, stage) %>%
  summarize(min = min(elevations, na.rm = T),
            max = max(elevations, na.rm = T),
            gradient = sum(gradient * segment_distance, na.rm = T) / sum(segment_distance, na.rm = T),
            distance = sum(segment_distance, na.rm = T),
            elevations = mean(elevations, na.rm = T),
  ) %>%
  ungroup() %>%
  
  arrange(-every_km) %>%
  
  mutate(change_gradient = ifelse(gradient > 0.03 | gradient < -0.03,
                                  ifelse(gradient > 0.03 & lag(gradient < 0.03), "uphill",
                                         ifelse(gradient < -0.03 & lag(gradient > -0.03), "downhill", NA)), "flat")) %>%
  
  fill(change_gradient, .direction = "down") %>%
  
  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
  
  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
  
  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%
  
  mutate(change_gradient = ifelse(change_gradient != lag(change_gradient) & change_gradient != lead(change_gradient),
                                  ifelse(lag(change_gradient) == lead(change_gradient), lag(change_gradient), change_gradient), change_gradient)) %>%

  rownames_to_column() %>%
  
  mutate(grouping = ifelse(rowname == 1, 1, ifelse(change_gradient != lag(change_gradient), 1, NA)),
         rk = rank(grouping, ties.method = "first"),
         grouping = ifelse(is.na(grouping), NA, rk)) %>%
  
  fill(grouping, .direction = "down")

#

different_groupings <- tdf14_2020 %>%
  
  filter(change_gradient == "uphill") %>%
  
  group_by(grouping, stage_length = length, race, stage, year) %>%
  summarize(start_km = max(every_km, na.rm = T),
            end_km = min(every_km, na.rm = T),
            start_elev = min(elevations, na.rm = T),
            end_elev = max(elevations, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(length = start_km - end_km,
         gain = end_elev - start_elev,
         gradient = gain / (length * 1000)) %>%
  
  filter(length > 0) %>%
  filter(gradient > 0.035)

if(length(different_groupings$grouping) == 0) {
  
} else {

#

different_groupings <- cbind(different_groupings,
                             
                             model_category = mgcv::predict.gam(read_rds('model-climb-difficulty.rds'),
                                                      different_groupings %>%
                                                        mutate(vam_poly = ((gradient^2)*length)) %>%
                                                        mutate(alt = end_elev - 1000))) %>%
  mutate(perc_thru = 1 - (end_km / stage_length))

#

different_groupings <- cbind(
  
  different_groupings,
  
  power_required = mgcv::predict.gam(read_rds("Stored models/power-required-throughout-race-gam.rds"), different_groupings)) %>%
  
  cbind(best = mgcv::predict.gam(read_rds("Stored models/power-required-throughout-race-gam.rds"), tibble(perc_thru = 1))) %>%
  
  mutate(rel_to_best = power_required / best) %>%
  
  select(-best, -power_required) %>%
  
  rename(power_required = rel_to_best) %>%
  
  mutate(power_model_category = power_required * model_category)

#
  
  koms_list_tdf[[S]] <- different_groupings
  
}

}

#

all_tdf_2020_koms <- bind_rows(koms_list_tdf)

dbWriteTable(con, "climbs_from_telemetry", all_tdf_2020_koms, append = TRUE, row.names = F)

#
#
#

ggplot(tdf14_2020, 
       aes(x = every_km, y = elevations, color = change_gradient))+
  geom_point(size=1)+
  scale_x_reverse()+
  scale_color_manual(values = c("black", 'gray', 'red'))
                                  
#

library(bcp)

dat <- tdf14_2020$elevations

bcp_x <- bcp(dat, return.mcmc = TRUE)

cbind(tdf14_2020, prob = bcp_x$posterior.prob) -> xyz

#
#
#
#
#


tictoc::tic()

x2019 <- all_routes %>%
  filter(year %in% c(2019)) %>%
  inner_join(pcs_fr_matches, by = c("race" = "fr_race", "year")) %>%
  inner_join(pcs, by = c("pcs_race" = "race", "year")) %>%
  
  filter(class %in% c("1.UWT", "1.HC", "WC", "CC", "2.UWT", "2.HC", "2.Pro", "1.Pro")) %>%
  
  select(-highest_point, -points) %>%
  
  mutate(left_km = length - distances) %>%
  
  mutate(every_km = floor(left_km/0.5)/2) %>%
  mutate(everytenth = floor(left_km/0.1)/10) %>%
  
  select(-class, -race_url, -url, -stages) %>%
  
  mutate(every_km = ifelse(left_km < 1, everytenth, every_km)) %>%
  
  group_by(every_km, length, year, pcs_race, stage) %>%
  summarize(gradient = mean(grades, na.rm = T),
            min = min(elevations, na.rm = T),
            max = max(elevations, na.rm = T),
            #start = max(left_km, na.rm = T),
            #end = min(left_km, na.rm = T),
            #points = n()
            ) %>%
  ungroup() %>%
  
  mutate(race = tolower(pcs_race)) %>%
  
  select(-pcs_race)

tictoc::toc()

#

percentiles_x2019 <- x2019 %>%
  
  group_by(stage, race, year) %>%
  mutate(pct = round(percent_rank(every_km)/0.01)) %>%
  ungroup() %>%
  
  group_by(stage, race, year, pct, length) %>%
  summarize(gradient = mean(gradient, na.rm = T),
            elevation = mean((min+max)/2, na.rm = T),
            km = mean(every_km, na.rm = T)) %>%
  ungroup()

#
#
#
#
#

