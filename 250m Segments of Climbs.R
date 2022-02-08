


all_stage_data <- dbGetQuery(con, "SELECT * FROM stage_data_perf WHERE year > 2019") %>%
  
  mutate(date = as.Date(date))

#

x250m_segments <- dbGetQuery(con, "SELECT * FROM strava_new_segment_creation_climbs WHERE year > 2019") %>%
  
  mutate(date = as.Date(date)) %>%
  
  inner_join(
    
    dbReadTable(con, "segments_from_strava_data") %>%
      mutate(date = as.Date(date),
             stage = as.character(stage)) %>%
      
      select(start_km_orig = start_km,
             end_km_orig = end_km,
             seg_length_orig = length,
             stage_length,
             gradient,
             stage, race, year, date) %>%
    
      group_by(stage, race, year) %>%
      filter(gradient >= 0.04) %>%
      unique() %>%
      
      mutate(start_km_orig = stage_length - start_km_orig,
             end_km_orig = stage_length - end_km_orig) %>%
      
      arrange(start_km_orig) %>%
      rowid_to_column() %>%
      
      mutate(rowid = rank(rowid, ties.method = "first")) %>%
      
      ungroup() %>%
      rename(original_segment = rowid) %>%
      mutate(kind = "expand_climbs") %>%
      mutate(start_prior_orig = start_km_orig,
             end_next_orig = end_km_orig), by = c("original_segment", "stage", "race", "year", "date")) %>%
  
  inner_join(telemetry_available %>%
               select(rider = pcs, rnk, activity_id, total_seconds), by = c("activity_id")) %>%
  
  mutate(rider = str_to_title(rider)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, weight FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)) %>%
      filter(!is.na(weight)) %>%
      group_by(rider) %>%
      summarize(weight = median(weight)) %>%
      ungroup(), by = c("rider"))

#
#
#

specific_race <- x250m_segments %>%
  
  filter(race == "saudi tour" & stage == 4 & year == 2022)

#
#
#

specific_race %>%
  
  filter(original_segment == 20) %>%
  
  mutate(time = should_be_distance / (segment_speed_kmh * 1000 / 3600)) %>%
  
  group_by(rider) %>%
  arrange(desc(end_next)) %>%
  mutate(cum_time = total_seconds - cumsum(segment_time)) %>%
  mutate(segs = n()) %>%
  ungroup() %>%
  
  filter(max(segs) == segs) %>%
  
  filter(rnk <= 45) %>%
  
  group_by(rowid, original_segment) %>%
  mutate(cum_time = mean(cum_time) - cum_time) %>%
  ungroup() %>%
  
  #filter(rider %in% c("Vine Jay", "Cherel Mikael", "Bettiol Alberto",
  #                   "Johannessen Tobias Halland", "Champoussin Clement")) %>%
  
  ggplot()+
  geom_path(aes(x = end_next, y = cum_time, color = rider), size = 1)+
  ggrepel::geom_label_repel(data = . %>% filter(rowid == min(rowid) | rowid == max(rowid)), 
                            aes(x = end_next, y = cum_time, label = rider, color = rider))+
  scale_x_continuous(breaks = seq(141,145,0.25),
                     labels = scales::number_format(accuracy = 0.25))+
  guides(color = FALSE)
  
#
#
#

specific_race %>%
  
  filter(original_segment == 3) %>%
  
  group_by(rider) %>%
  mutate(segs = n()) %>%
  ungroup() %>%
  
  filter(max(segs) == segs) %>%

  filter(!is.na(weight)) %>%
  
  mutate(Power = ifelse(Power == 0, NA, Power)) %>%
  
  group_by(rowid) %>%
  mutate(segment_gradient = median(segment_gradient)) %>%
  mutate(Power_to_Speed = mean((Power/weight) / segment_speed_kmh, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(ImpliedWattsKG = ifelse(is.na(Power),segment_speed_kmh * Power_to_Speed, Power/weight)) %>%
  
  filter(rnk <= 25) %>%

  arrange(rowid, rider, rnk) %>%
  
  ggplot()+
  
  geom_tile(aes(x = (start_prior+end_next)/2, y = reorder(rider, desc(rnk)),
                fill = ImpliedWattsKG))+
  
  #geom_text(aes(x = (start_prior+end_next)/2, y = reorder(rider, desc(rnk)),
  #              label = paste0(round(segment_gradient,3)*100,"%")))+
  
  theme(axis.ticks.y = element_blank(),
        panel.grid = element_blank())+
  
  scale_fill_viridis_c(option = "A")+
  
  labs(x = "KM thru race",
       y = "",
       fill = "Watts/KG",
       title = "Top finishers on Mont Bouquet",
       subtitle = "Average Power for 250m segments")
