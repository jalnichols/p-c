library(tidyverse)
library(rvest)
library(DBI)

options(tidyverse.quiet = TRUE)
options(readr.num_columns = 0)

dbDisconnect(con)

con <- DBI::dbConnect(RPostgres::Postgres(),
                      port = 5432,
                      host = 'localhost',
                      dbname = "cycling",
                      user = "postgres",
                      password = "braves")

#

# Scrape Current FR Races -------------------------------------------------

for(M in seq(8,10,1)) {

  Y = '2023'
  
  pg <- paste0("https://www.la-flamme-rouge.eu/maps/races?count=0&page=1&months%5B0%5D=", 
               M,
               "&year%5B0%5D=",
               Y, 
               "&years=&name=") %>% read_html
  
  pages <- pg %>%
    html_nodes(xpath = '//*[@id="page-body"]/div[10]/div') %>%
    html_text() %>%
    str_replace_all(" ", "") %>%
    str_split(pattern = "\n") %>%
    .[[1]] %>%
    enframe(name = NULL) %>%
    filter(value != "") %>%
    mutate(value = as.numeric(value)) %>%
    .$value
  
  races_list <- vector("list", length(pages))
  
  if(length(pages) == 1) {
    rws <- pg %>%
      html_nodes('tr') %>%
      html_nodes('td') %>%
      html_nodes('a')
    
    r <- rws %>%
      html_attr(name = "href") %>%
      enframe(name = NULL) %>% 
      filter(str_detect(value, "maps/races/view/")) %>% 
      separate(value, c("url", "trash"), sep = "=") %>%
      select(-trash) %>%
      mutate(url = paste0('https://www.la-flamme-rouge.eu', str_sub(url, 1, nchar(url) - 4)))
    
    df <- pg %>%
      html_nodes('table') %>%
      html_table(header = TRUE) %>%
      .[[1]] %>%
      as_tibble() %>%
      
      # attach races URL from above
      cbind(r) %>%
      janitor::clean_names() %>%
      select(date, race = name, stages, class = clas, url, cat) %>%
      filter(cat %in% c("ME", "MU"))
    
    races_list[[1]] <- df
    
  } else {
    
    for(n in 1:max(pages)) {
      
      pg <- paste0("https://www.la-flamme-rouge.eu/maps/races?count=0&page=", 
                   n, 
                   "&months%5B0%5D=", 
                   M,
                   "&year%5B0%5D=",
                   Y, 
                   "&years=&name=") %>% read_html
      
      rws <- pg %>%
        html_nodes('tr') %>%
        html_nodes('td') %>%
        html_nodes('a')
      
      r <- rws %>%
        html_attr(name = "href") %>%
        enframe(name = NULL) %>% 
        filter(str_detect(value, "maps/races/view/")) %>% 
        separate(value, c("url", "trash"), sep = "=") %>%
        select(-trash) %>%
        mutate(url = paste0('https://www.la-flamme-rouge.eu', str_sub(url, 1, nchar(url) - 4)))
      
      df <- pg %>%
        html_nodes('table') %>%
        html_table(header = TRUE) %>%
        .[[1]] %>%
        as_tibble() %>%
        
        # attach races URL from above
        cbind(r) %>%
        janitor::clean_names() %>%
        select(date, race = name, stages, class = clas, url, cat) %>%
        filter(cat %in% c("ME", "MU")) %>%
        mutate(class = as.character(class))
      
      races_list[[n]] <- df
    }
    
  }
  
  all_races_month <- bind_rows(races_list) %>%
    
    mutate(date = str_replace_all(date, "Friday", ""),
           date = str_replace_all(date, "Saturday", ""),
           date = str_replace_all(date, "Sunday", ""),
           date = str_replace_all(date, "Monday", ""),
           date = str_replace_all(date, "Tuesday", ""),
           date = str_replace_all(date, "Wednesday", ""),
           date = str_replace_all(date, "Thursday", ""),
           date = str_trim(date)) %>%
    
    mutate(twodates = ifelse(nchar(date) <= 20, 1, 2),
           start_date = ifelse(twodates == 1, date, str_trim(str_sub(date, 1, nchar(date)/2))),
           end_date = ifelse(twodates == 1, date, str_trim(str_sub(date, 1+(nchar(date)/2), nchar(date))))) %>%
    
    select(-date, -twodates) %>%
    
    mutate(start_date = lubridate::dmy(start_date),
           end_date = lubridate::dmy(end_date)) %>%
    
    filter(class %in% c("1.1", "2.1", "2.Pro", "1.Pro", "1.UWT", "2.UWT", "WC", "CC",
                        "1.2U", "2.2U", '1.HC', "2.HC", "CM")) #%>%
  
    #filter(class %in% c("CM")) %>%
    #filter(!str_detect(race, 'TTT'))
  
  #
  
  already <- dbGetQuery(con, "SELECT * FROM new_fr_races") %>%
    select(url)
  
  dbWriteTable(con, "new_fr_races", all_races_month %>% anti_join(already, by = c("url")), append = TRUE, row.names = FALSE)
  
  all_races_month <- all_races_month %>% anti_join(already, by = c("url"))
  
  print(all_races_month)
  
  #
  
  stages_list <- vector("list", length(all_races_month$url))
  
  for(x in 1:nrow(all_races_month)) {
    
    page <- all_races_month$url[[x]] %>%
      read_html()
    
    tab <- page %>%
      html_nodes('table') %>%
      html_table()
    
    if(length(tab) == 0) {} else {
    
      tab <- page %>%
        html_nodes('table') %>%
        html_table() %>%
        .[[1]]
      
      colnames(tab) <- tab[1,]
      
      tab <- tab %>% 
        filter(Num %in% c("P", "2-1", "2-2", "1-1", "1-2", "3-1", "3-2", "4-1", "4-2", "5-1", "5-2", as.character(seq(1,22,1)))) %>%
        janitor::clean_names() %>%
        select(stage = num,
               stageType = type,
               date,
               length,
               depart_and_arrive) %>%
        mutate(date = str_replace_all(date, "Friday", ""),
               date = str_replace_all(date, "Saturday", ""),
               date = str_replace_all(date, "Sunday", ""),
               date = str_replace_all(date, "Monday", ""),
               date = str_replace_all(date, "Tuesday", ""),
               date = str_replace_all(date, "Wednesday", ""),
               date = str_replace_all(date, "Thursday", ""),
               date = str_trim(date),
               date = lubridate::dmy(date),
               length = as.numeric(str_trim(str_replace(length, "Km", "")))) %>%
        separate(depart_and_arrive, c("depart", "arrive"), sep = ">") %>%
        mutate(depart = str_trim(depart),
               arrive = str_trim(arrive))
      
      d <- page %>%
        html_nodes('table') %>%
        html_nodes('a') %>%
        html_attr(name = "href") %>% 
        enframe(name = NULL) %>%
        mutate(value = str_replace(value, "sid=", "=")) %>%
        separate(value, c("url", "trash"), sep = "=") %>% 
        mutate(url = paste0('https://www.la-flamme-rouge.eu', str_sub(url, 1, nchar(url) - 1))) %>%
        mutate(url = str_replace(url, "viewtrack", "loadtrack")) %>%
        mutate(race = all_races_month$race[[x]],
               race_url = all_races_month$url[[x]])
      
      st <- cbind(d, tab)
      
      stages_list[[x]] <- st %>%
        select(-trash)
    
    }
    
    Sys.sleep(2)
    
  }
  
  # filter out races w/o data available yet (empty stages)
  
  stages_list <- stages_list[lengths(stages_list) != 0]
  
  stages_list <- stages_list %>%
    discard(function(x) nrow(x) == 0)
  
  #
  #
  #
  
  all_stages_monthly <- bind_rows(stages_list) %>%
    
    inner_join(all_races_month, by = c("race_url" = "url", "race"))
  
  #
  
  dbWriteTable(con, "new_fr_stages", all_stages_monthly, append = TRUE, row.names = FALSE)
  
  print(all_stages_monthly)
  
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

all_stages_monthly <- dbGetQuery(con, "SELECT DISTINCT * FROM new_fr_stages") %>%
  
  mutate(start_date = ifelse(race_url == "https://www.la-flamme-rouge.eu/maps/races/view/2022/3", as.Date("2022-08-19"), as.Date(start_date)),
         start_date = as.Date(start_date, origin = "1970-01-01"),
         start_date = ifelse(start_date < as.Date('1900-01-01'), as.Date(paste0(lubridate::year(date), "-", str_sub(start_date, 6, 10))), start_date),
         start_date = as.Date(start_date, origin = "1970-01-01")) %>%
  
  arrange(date) %>%
  
  mutate(id = str_replace(url, "https://www.la-flamme-rouge.eu/maps/loadtrack/", "")) %>%
  
  anti_join(fs::dir_info("FR-routes/backfill/") %>%
              mutate(id = str_replace(path, ".csv", ""),
                     id = str_replace(id, ".rds", ""),
                     id = str_replace(id, "FR-routes/backfill/fr-", ""),
                     id = str_replace(id, "climbs-", ""),
                     id = str_replace(id, "route-", ""),
                     id = str_replace(id, "cobbles-", "")) %>%
              select(id) %>%
              unique()) %>%
  
  filter(date > '2023-01-01')

#
#
#

for(r in 1:nrow(all_stages_monthly)) {
  
  race_profile <- all_stages_monthly$url[[r]] %>%
    jsonlite::fromJSON()
  
  route <- race_profile$altRoute %>%
    unnest(cols = "position") %>%
    mutate(distance = as.numeric(distance),
           k = as.numeric(k),
           A = as.numeric(A)) %>%
    rename(latitude = k,
           longitude = A)
  
  sprints <- race_profile$sprints
  
  if(length(sprints$stageclimbs) > 0) {
    
    climbs <- sprints$stageclimbs %>%
      #mutate(name = str_replace_all(name, "%u0219", "s"),
      #       name = URLdecode(name)) %>%
      unnest(cols = "location") %>%
      unnest(cols = "marker") %>%
      select(name, end_distance = distance, start_distance = startdistance,
             longitude = `F`, latitude = A, category, altitude) %>%
      mutate(name = str_trim(str_replace_all(name, "%20", " ")))
    
    write_rds(x = climbs,
              file = paste0("FR-routes/backfill/fr-climbs-", str_replace(all_stages_monthly$url[[r]], "https://www.la-flamme-rouge.eu/maps/loadtrack/", ""), ".rds"))
    
  }
  
  if(length(sprints$stagecobbles) > 0) {
    
    cobbles <- sprints$stagecobbles %>%
      #mutate(name = URLdecode(name)) %>%
      unnest(cols = "location") %>%
      unnest(cols = "marker") %>%
      select(name, end_distance = distance, start_distance = startdistance,
             longitude = `F`, latitude = A, altitude) %>%
      mutate(name = str_trim(str_replace_all(name, "%20", " ")))
    
    write_rds(x = cobbles,
              file = paste0("FR-routes/backfill/fr-cobbles-", str_replace(all_stages_monthly$url[[r]], "https://www.la-flamme-rouge.eu/maps/loadtrack/", ""), ".rds"))
    
  }

  write_rds(x = route,
              file = paste0("FR-routes/backfill/fr-route-", str_replace(all_stages_monthly$url[[r]], "https://www.la-flamme-rouge.eu/maps/loadtrack/", ""), ".rds"))
  
  print(all_stages_monthly[r,] %>% select(race, stage, date))
  Sys.sleep(40)
  
}

#
#
#


# Calculate Stage Chars ---------------------------------------------------


# gam mod
GAM_MOD <- read_rds('Stored models/model-climb-difficulty.rds')

# alt mod
model_data <- tibble(elev = seq(0,14000,1000), 
                     aap = c(0.999, 0.992, 0.983, 0.972, 0.959, 0.944, 0.927, 0.907, 0.886, 0.863, 0.837, 0.809, 0.78, 0.748, 0.714)) %>% 
  mutate(elev = elev * 0.3048)

mgcv::gam(aap ~ s(elev, k = 5), data = model_data) -> gam_mod

# power required model
power_req_mod <- read_rds("Stored models/power-required-throughout-race-gam.rds")

# from https://docs.google.com/spreadsheets/d/1-LdY9TWBxCFMLXM2HeFMuvTKcyOn0EzXLiYrlG_8kCo/edit#gid=0

weight_adv <- tibble(gradient = seq(-6,12,1),
                     difference = c(1.27, 1.16, 1.11, 1.08, 1.065, 1.054, 1.046, 1.041, 1.035, 
                                    1.032, 1.03, 1.028, 1.027, 1.026, 1.026, 1.025, 1.025, 1.025, 1.025))

drafting_bene <- tibble(gradient = seq(-4,12,1),
                        drafting_bene = c(1, 0.83, 0.61, 0.46, 0.34, 0.26, 0.17, 0.13, 0.095, 
                                          0.068, 0.052, 0.036, 0.027, 0.02, 0.015, 0.012, 0.008))

#

all_race_activities <- fs::dir_info("FR-routes/backfill/") %>%
  mutate(file_type = ifelse(str_detect(path, ".csv"), "csv",
                            ifelse(str_detect(path, ".rds"), "rds", "unk"))) %>%
  mutate(id = str_replace(path, ".csv", ""),
         id = str_replace(id, ".rds", ""),
         id = str_replace(id, "FR-routes/backfill/fr-", ""),
         id = str_replace(id, "climbs-", ""),
         id = str_replace(id, "route-", ""),
         id = str_replace(id, "cobbles-", "")) %>%
  select(id, file_type) %>%
  unique() %>%
  inner_join(dbGetQuery(con, "SELECT DISTINCT * FROM new_fr_stages") %>%
               mutate(id = str_replace(url, "https://www.la-flamme-rouge.eu/maps/loadtrack/", "")) %>%
               select(url, id, length, race, stage) %>%
               unique()) %>%
  
  anti_join(dbGetQuery(con, "SELECT DISTINCT activity_id as id FROM new_fr_stage_characteristics"))
  


extract_telemetry <- function(ID, file_type) {
  
  if(file_type == "csv") {
    FILE <- paste0("FR-routes/backfill/fr-route-", ID, ".csv")
    data_lists <- read_csv(FILE)
  } else if(file_type == "rds") {
    FILE <- paste0("FR-routes/backfill/fr-route-", ID, ".rds")
    data_lists <- read_rds(FILE)
  } else {}
  
  # clean this up before writing to DB
  
  df <- cbind(
    
    altitude = data_lists[["altitude"]],
    distance = data_lists[["distance"]],
    activity_id = as.character(ID)
    
  ) %>%
    
    as_tibble() %>%
    mutate(altitude = as.numeric(altitude),
           distance = as.numeric(distance),
           distance = distance * 1000) %>%
    mutate(altitude = round(altitude, 0))
  
  #
  
  vertical_gain <- df %>% 
    mutate(vg = altitude - lag(altitude), 
           vg = ifelse(is.na(vg), 0, ifelse(vg < 0, 0, vg))) %>% 
    mutate(cum_vg = cumsum(vg)) %>% 
    
    group_by(distance = floor(distance / 500) / 2, 
             activity_id) %>% 
    summarize(vertical_gain = mean(cum_vg, na.rm = T),
              total_vg = sum(vg, na.rm = T),
              altitude = max(altitude, na.rm = T)) %>%
    ungroup() %>%
    
    mutate(perc_thru = distance / (all_race_activities %>% filter(id == ID) %>% select(length) %>% .[[1]])) %>%
    
    mutate(power_required = mgcv::predict.gam(power_req_mod, .),
           best = mgcv::predict.gam(power_req_mod, tibble(perc_thru = 1)),
           rel_to_best = power_required / best,
           weighted_vg = total_vg * rel_to_best) %>%
    
    select(activity_id, distance, total_vg, weighted_vg, perc_thru)
  
  # set up data for stage characteristics code
  
  all_routes <- df %>%
    
    rename(dist = distance,
           alt = altitude) %>%
    
    mutate(dist = floor(dist / 50) * 0.05) %>%
    
    group_by(dist) %>%
    summarize(alt = median(alt, na.rm = T)) %>%
    ungroup() %>%
    
    arrange(dist) %>%
    
    mutate(points = rank(dist, ties.method = "first"),
           length = max(dist, na.rm = T),
           avg_alt = mean(alt, na.rm = T),
           highest_point = max(alt, na.rm = T)) %>%
    
    mutate(grades = (alt - lag(alt)) / (1000 * (dist - lag(dist))),
           grades = ifelse(dist == 0, 0, grades),
           grades = ifelse(grades > 0.25, 0.25,
                           ifelse(grades < -0.25, -0.25, grades))) %>%
    
    rename(elevations = alt) %>%
    rename(distances = dist)
  
  if(skip == 1) {
    
  } else {
    
    # calc stage chars
    
    final_1km <- all_routes %>%
      
      mutate(stage_end = max(length, na.rm = T)) %>%
      
      filter((stage_end - distances) < 1.1) %>%
      
      mutate(vert_gain = elevations - lag(elevations),
             vert_gain = ifelse(vert_gain > 0, vert_gain, 0)) %>%
      
      summarize(final_1km_elev = mean(elevations, na.rm = T),
                final_1km_vertgain = sum(vert_gain, na.rm = T),
                max_gradient = max(grades, na.rm = T),
                med_gradient = median(grades, na.rm = T),
                avg_gradient = mean(grades, na.rm = T),
                x25th_gradient = quantile(grades, probs = 0.75, na.rm = T),
                n = n()) %>%
      
      mutate(final_1km_gradient = (med_gradient + avg_gradient + x25th_gradient) / 3) %>%
      
      select(final_1km_elev, final_1km_gradient, final_1km_vertgain)
    #
    
    final_5km <- all_routes %>%
      
      mutate(stage_end = max(length, na.rm = T)) %>%
      
      filter((stage_end - distances) < 5.1) %>%
      
      mutate(vert_gain = elevations - lag(elevations),
             vert_gain = ifelse(vert_gain > 0, vert_gain, 0)) %>%
      
      summarize(final_5km_elev = mean(elevations, na.rm = T),
                final_5km_vertgain = sum(vert_gain, na.rm = T),
                max_gradient = max(grades, na.rm = T),
                med_gradient = median(grades, na.rm = T),
                avg_gradient = mean(grades, na.rm = T),
                x25th_gradient = quantile(grades, probs = 0.75, na.rm = T),
                n = n()) %>%
      
      mutate(final_5km_gradient = (med_gradient + avg_gradient + x25th_gradient) / 3) %>%
      
      select(final_5km_elev, final_5km_gradient, final_5km_vertgain)
    
    #
    
    percentage_climbing_in_final_climb <- all_routes %>%
      
      arrange(points) %>%
      
      mutate(stage_end = max(length, na.rm = T)) %>%
      
      mutate(final_20km = ifelse((stage_end - distances) < 20.6, grades, NA)) %>%
      
      mutate(distance_chunks = distances - lag(distances),
             distance_chunks = ifelse(distances == 0, NA, distance_chunks)) %>%
      
      mutate(vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * final_20km, 0),
             total_vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * grades, 0)) %>%
      
      summarize(final_20km_vert_gain = sum(vert_gain, na.rm = T),
                total_vert_gain = sum(total_vert_gain, na.rm = T)) %>%
      
      mutate(perc_gain_end = final_20km_vert_gain / total_vert_gain)
    
    #
    
    percentage_climbing_start <- all_routes %>%
      
      arrange(points) %>%
      
      mutate(stage_end = max(length, na.rm = T)) %>%
      
      mutate(first_30km = ifelse((distances) < 30.6, grades, NA)) %>%
      
      mutate(distance_chunks = distances - lag(distances),
             distance_chunks = ifelse(distances == 0, NA, distance_chunks)) %>%
      
      mutate(vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * first_30km, 0),
             total_vert_gain = ifelse(grades > 0.02, 1000 * distance_chunks * grades, 0)) %>%
      
      summarize(first_30km_vert_gain = sum(vert_gain, na.rm = T),
                total_vert_gain = sum(total_vert_gain, na.rm = T)) %>%
      
      mutate(perc_gain_start = first_30km_vert_gain / total_vert_gain)
    
    # data below taken from a paper showing aerobic power at certain elevation levels (in feet) compared to sea-level
    # eg, at 14,000 feet it's about 71% of at sea-level
    
    weighted_alt <- cbind(all_routes, pred = all_routes %>%
                            rename(elev = elevations) %>%
                            mgcv::predict.gam(object = gam_mod)) %>%
      summarize(weighted_altitude = mean(pred, na.rm = T))
    
    #
    
    lumpiness <- all_routes %>%
      
      arrange(points) %>%
      
      mutate(distance_chunks = distances - lag(distances),
             distance_chunks = ifelse(distances == 0, NA, distance_chunks)) %>%
      
      mutate(total_elev_change = ifelse(abs(grades) > 0.02, abs(elevations - lag(elevations)), 0),
             over_6_percent = ifelse(grades >= 0.06, 50, 0)) %>%
      
      summarize(total_elev_change = sum(total_elev_change, na.rm = T),
                stage_end = max(length, na.rm = T),
                over_6_percent = sum(over_6_percent/1000, na.rm = T),
                time_at_1500m = mean(elevations > 1499.99, na.rm = T)) %>%
      
      mutate(perc_elev_change = total_elev_change / (stage_end * 1000))
    
    #
    
    stage_characteristics <- all_routes %>%
      
      filter(!(is.na(points))) %>%
      filter(points == 1) %>%
      select(-points, -distances, -elevations, -grades) %>%
      
      mutate(activity_id = as.character(ID)) %>%
      
      cbind(
        
        lumpiness %>%
          select(-stage_end)
        
      ) %>%
      
      cbind(
        
        weighted_alt
        
      ) %>%
      cbind(
        
        percentage_climbing_in_final_climb
        
      ) %>%
      cbind(
        
        percentage_climbing_start %>%
          select(-total_vert_gain)
        
      ) %>%
      cbind(
        
        final_1km
        
      ) %>%
      cbind(
        
        final_5km
        
      ) %>%
      cbind(
        
        vertical_gain %>%
          select(activity_id, weighted_vg) %>%
          group_by(activity_id) %>%
          summarize(weighted_vg = sum(weighted_vg, na.rm = T)) %>%
          ungroup() %>%
          select(-activity_id)
        
      ) %>%
      
      cbind(all_routes %>%
              filter(!is.na(grades)) %>%
              mutate(grades = round(grades,2),
                     grades = ifelse(grades < -0.06, -0.06, ifelse(grades > 0.12, 0.12, grades))) %>%
              inner_join(weight_adv %>% mutate(grades = gradient/100), by = c("grades")) %>%
              cbind(weight_adv %>% filter(gradient == 0) %>% 
                      select(flat = difference)) %>% 
              mutate(rel = difference/flat) %>% 
              summarize(weight_adv = mean(rel, na.rm = T))) %>%
      
      cbind(all_routes %>%
              filter((distances / length) > 0.8) %>%
              filter(!is.na(grades)) %>%
              mutate(grades = round(grades,2),
                     grades = ifelse(grades < 0, 0, ifelse(grades > 0.12, 0.12, grades))) %>%
              inner_join(drafting_bene %>% mutate(grades = gradient/100), by = c("grades")) %>%
              summarize(drafting_bene = mean(drafting_bene, na.rm = T))) %>%
      
      filter(total_vert_gain >= 0) %>%
      
      gather(stat, value, -activity_id)
    
    # write to DB
    
    dbWriteTable(con, "new_fr_stage_characteristics", stage_characteristics, append = T, row.names = F)
    
  }
  
  #
  #
  # extract climbs
  #
  #
  
  if(skip == 0) {
    
    if(file.exists(paste0("FR-routes/backfill/fr-climbs-", ID, ".csv"))) {
      actual_climbs <- read_csv(paste0("FR-routes/backfill/fr-climbs-", ID, ".csv"))
    } else if(file.exists(paste0("FR-routes/backfill/fr-climbs-", ID, ".rds"))) {
      actual_climbs <- read_file(paste0("FR-routes/backfill/fr-climbs-", ID, ".rds"))
    } else {}
    
    if(is.data.frame(actual_climbs)) {
      
      #
      
      actual_climbs_telem <- all_routes %>%
        expand_grid(actual_climbs) %>%
        filter(distances >= start_distance & distances <= end_distance) %>%
        rowid_to_column() %>%
        
        group_by(name, start_distance, end_distance, category) %>%
        filter(max(rowid) == rowid | min(rowid) == rowid) %>%
        ungroup() %>%
        
        select(-c(points, avg_alt, highest_point, grades, longitude, latitude, altitude, rowid)) %>%
        
        group_by(name, start_distance, end_distance, category) %>%
        summarize(vertical_gain = max(elevations) - min(elevations),
                  distances = max(distances) - min(distances),
                  start_elev = min(elevations),
                  end_elev = max(elevations)) %>%
        ungroup() %>%
        
        mutate(gradient = vertical_gain / (distances * 1000))
      
      #
      # 
      # all_routes %>% 
      #   filter(distances %% 1 %in% c(0.25, 0.5, 0.75, 0)) %>%
      #   arrange(distances) %>%
      #   mutate(grades = (grades + lag(grades,1) + lag(grades,2) + lag(grades,3))/4) %>%
      #   ggplot(aes(x = distances, y = elevations, color = grades))+
      #   geom_line(size=1.5)+
      #   ggrepel::geom_label_repel(data = actual_climbs_telem,
      #                             aes(x = end_distance,
      #                                 y = end_elev,
      #                                 label = paste0(name,"\n",round(distances,1)," km ",round(gradient*100,1),"% ", category)),
      #                             color = "black", fill = "transparent")+
      #   geom_blank(aes(x = 0, y = 1000))+
      #   labs(x = "",
      #        y = "")+
      #   theme(panel.grid.minor = element_blank(),
      #         axis.text = element_text(size = 12))+
      #   scale_color_gradientn(colors = c("#4575b4", "#91bfdb", "#B8EAF7", "#FCD672", "#fc8d59", "#d73027"), guide = "none")
      # 
      # rN <- all_race_activities %>% filter(id == ID) %>% select(race, stage) %>%
      #   mutate(race = paste0(race,'-',stage)) %>%
      #   .[[1]]
      # 
      # ggsave(filename = paste0("FR-routes/profiles/profile-", rN, ".png"),
      #        height = 4, width = 8)
      
      # Generate new climbs
      
      tdf14_2020 <- all_routes %>%
        
        select(-highest_point, -points) %>%
        
        mutate(left_km = length - distances) %>%
        
        mutate(every_km2 = floor(left_km/0.1)/10) %>%
        
        group_by(every_km2, length) %>%
        summarize(
          
          left_km = min(left_km, na.rm = T),
          elevations = mean(elevations, na.rm = T)
          
        ) %>%
        ungroup() %>%
        
        arrange(every_km2) %>%
        
        mutate(every_km = floor(every_km2/0.25)/4) %>%
        
        mutate(every_km = ifelse(left_km < 0, everytenth, every_km),
               gradient = (elevations - lead(elevations)) / ((lead(left_km)-left_km)*1000)) %>%
        
        group_by(every_km, length) %>%
        summarize(min = min(elevations, na.rm = T),
                  max = max(elevations, na.rm = T),
                  gradient = mean(gradient, na.rm = T),
                  elevations = max(elevations, na.rm = T),
        ) %>%
        ungroup() %>%
        
        arrange(desc(every_km)) %>%
        
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
        
        mutate(segment_distance = every_km - lead(every_km),
               segment_distance = ifelse(is.na(segment_distance), lag(segment_distance), segment_distance)) %>%
        
        group_by(every_km, length) %>%
        summarize(min = min(elevations, na.rm = T),
                  max = max(elevations, na.rm = T),
                  gradient = sum(gradient * segment_distance, na.rm = T) / sum(segment_distance, na.rm = T),
                  distance = sum(segment_distance, na.rm = T),
                  elevations = mean(elevations, na.rm = T),
        ) %>%
        ungroup() %>%
        
        arrange(desc(every_km)) %>%
        
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
        
        group_by(grouping, stage_length = length) %>%
        summarize(start_km = max(every_km, na.rm = T),
                  end_km = min(every_km, na.rm = T),
                  start_elev = min(elevations, na.rm = T),
                  end_elev = max(elevations, na.rm = T)) %>%
        ungroup() %>%
        
        rowid_to_column() %>%
        
        expand_grid(actual_climbs_telem %>%
                      select(name, start_distance, end_distance)) %>%
        
        mutate(start_km = stage_length - start_km,
               end_km = stage_length - end_km) %>%
        
        filter(!((start_km) >= start_distance & (end_km) <= end_distance)) %>%
        
        group_by(rowid) %>%
        filter(n() == nrow(actual_climbs_telem)) %>%
        ungroup() %>%
        
        select(-c(name, start_distance, end_distance, rowid)) %>%
        
        unique() %>%
        
        mutate(length = end_km - start_km,
               gain = end_elev - start_elev,
               gradient = gain / (length * 1000)) %>%
        
        filter(length > 0) %>%
        filter(gradient > 0.025) %>%
        
        rbind(
          actual_climbs_telem %>%
            rename(start_km = start_distance,
                   end_km = end_distance,
                   gain = vertical_gain,
                   length = distances,
                   grouping = name) %>%
            mutate(stage_length = tdf14_2020$length[[1]]) %>%
            select(-category))
      
      #
      
      data_climbs <- cbind(different_groupings,
                           
                           model_category = mgcv::predict.gam(GAM_MOD,
                                                              different_groupings %>%
                                                                mutate(vam_poly = ((gradient^2)*length)) %>%
                                                                mutate(alt = end_elev - 1000))) %>%
        mutate(perc_thru = (end_km / stage_length))
      
      #
      
      final_data_climbs <- cbind(
        
        data_climbs,
        
        power_required = mgcv::predict.gam(power_req_mod, data_climbs)) %>%
        
        cbind(best = mgcv::predict.gam(power_req_mod, tibble(perc_thru = 1))) %>%
        
        mutate(rel_to_best = power_required / best) %>%
        
        select(-best, -power_required) %>%
        
        rename(power_required = rel_to_best) %>%
        
        mutate(power_model_category = power_required * model_category)
      
      #
      
      all_2021_koms <- final_data_climbs %>%
        select(-grouping, -stage_length, -start_km, -end_km, -start_elev) %>%
        rename(alt = end_elev) %>%
        mutate(alt = round(alt, 0),
               gain = round(gain, 0),
               gradient = round(gradient, 3),
               model_category = round(model_category, 1),
               perc_thru = round(perc_thru, 3),
               power_required = round(power_required, 2),
               power_model_category = round(power_model_category, 1),
               id = ID)
      
      dbWriteTable(con, "climbs_from_new_fr_telemetry", all_2021_koms, append = TRUE, row.names = F)
      
      #
      
      different_groupings <- tdf14_2020 %>%
        
        filter(change_gradient == "downhill") %>%
        
        group_by(grouping, stage_length = length) %>%
        summarize(start_km = max(every_km, na.rm = T),
                  end_km = min(every_km, na.rm = T),
                  start_elev = min(elevations, na.rm = T),
                  end_elev = max(elevations, na.rm = T)) %>%
        ungroup() %>%
        
        mutate(length = start_km - end_km,
               gain = start_elev - end_elev,
               gradient = gain / (length * 1000)) %>%
        
        filter(length > 0) %>%
        filter(gradient < -0.025)
      
      data_climbs <- cbind(different_groupings,
                           
                           model_category = -1 * mgcv::predict.gam(GAM_MOD,
                                                                   different_groupings %>%
                                                                     mutate(vam_poly = ((gradient^2)*length)) %>%
                                                                     mutate(alt = end_elev - 1000))) %>%
        mutate(perc_thru = 1 - (end_km / stage_length))
      
      #
      
      all_2021_koms <- data_climbs %>%
        select(-grouping, -stage_length, -start_km, -end_km, -start_elev) %>%
        rename(alt = end_elev) %>%
        mutate(alt = round(alt, 0),
               gain = round(gain, 0),
               gradient = round(gradient, 3),
               model_category = round(model_category, 1),
               perc_thru = round(perc_thru, 2),
               power_required = as.numeric(NA),
               power_model_category = as.numeric(NA),
               id = ID)
      
      dbWriteTable(con, "downhills_from_new_fr_telemetry", all_2021_koms, append = TRUE, row.names = F)
    }
  }
}

#
# 
# 

skip = 0

for(a in 1:length(all_race_activities$id)) {
  
  ID <- all_race_activities$id[[a]]
  file_type <- all_race_activities$file_type[[a]]
  
  safe_segments <- safely(.f = extract_telemetry, otherwise = print("error"))
  
  safe_segments(ID, file_type)
  
  print(a)
  
}

#
#
#

cobbles_func <- function(ID, file_type) {
   
  if(file_type == "csv") {
    FILE <- paste0("FR-routes/backfill/fr-route-", ID, ".csv")
    route_df <- read_csv(FILE)
  } else if(file_type == "rds") {
    FILE <- paste0("FR-routes/backfill/fr-route-", ID, ".rds")
    route_df <- read_rds(FILE)
  } else {}
  
  if(file.exists(paste0("FR-routes/backfill/fr-cobbles-", ID, ".csv"))) {
    
    if(file_type == "csv") {
      FILE <- paste0("FR-routes/backfill/fr-cobbles-", ID, ".csv")
      COBBLES <- read_csv(FILE)
    } else if(file_type == "rds") {
      FILE <- paste0("FR-routes/backfill/fr-cobbles-", ID, ".rds")
      COBBLES <- read_rds(FILE)
    } else {}
    
    COBBLES <- COBBLES %>%
      mutate(cobbles_dist = end_distance - start_distance,
             perc_thru = end_distance / max(route_df$distance))
    
    COBBLES <- cbind(COBBLES,
                     power_required = mgcv::predict.gam(power_req_mod, COBBLES)) %>%
      cbind(best = mgcv::predict.gam(power_req_mod, tibble(perc_thru = 1))) %>%
      
      mutate(rel_to_best = power_required / best) %>%
      
      select(-best, -power_required) %>%
      
      summarize(cobbles_km = sum(cobbles_dist),
                cobbles_perc = cobbles_km / max(route_df$distance)) %>%
      
      mutate(id = ID)
    
    dbWriteTable(con, "fr_cobbles_calculated", COBBLES, append = TRUE, row.names = FALSE)
    
  }

}

#

for(a in 1:length(all_race_activities$id)) {
  
  ID <- all_race_activities$id[[a]]
  file_type <- all_race_activities$file_type[[a]]
  
  safe_segments <- safely(.f = cobbles_func, otherwise = print("error"))
  
  safe_segments(ID, file_type)
  
  print(a)
  
}
