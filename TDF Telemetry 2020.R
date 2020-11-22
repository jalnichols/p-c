library(tidyverse)
library(DBI)
library(RMySQL)

Sys.sleep(0)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

# now kick off everything

telemetry_api <- 'https://racecenter.letour.fr/api/telemetryCompetitor-2020'

STAGE <- 19

step = 1

while(step < 700) {

  json_df <- jsonlite::fromJSON(telemetry_api) %>%
    select(-YGPW) %>%
    unnest(cols = c(Riders)) %>%
    select(-LatLon)
  
  DBI::dbWriteTable(con, "telemetry_tdf2020", json_df, row.names = F, append = TRUE)

  if(min(json_df$kmToFinish) < 3) {

    Sys.sleep(3)

    step = step + 0.15

  } else {
    
    Sys.sleep(20)
    
    step = step + 1
    
   }
  
  print(min(json_df$kmToFinish))

}

#

all_stages <- dbReadTable(con, "telemetry_tdf2020") %>% unique()

#

riders <- 'https://racecenter.letour.fr/api/allCompetitors-2020' %>%
  readLines() %>%
  jsonlite::fromJSON() %>%
  select(Bib = bib, firstname, lastname, lastnameshort) %>%
  
  mutate(GC = ifelse(Bib %in% c("1", "11", "14", "22", "31", "51", '61', '71',
                                '81', '91', '94', '101', '104', '121',
                                '131', '161', '141'), "GC","Helper"))

#

st10 <- all_stages %>% filter(StageId == "0900")

#
#
#
#

sprint <- st10 %>% group_by(TimeStamp) %>% filter(min(kmToFinish)>0) %>% ungroup() %>% filter(kmToFinish < 0.51 & kmToFinish > 0)

sprint %>% group_by(Bib) %>% summarize(m = mean(kph, na.rm = T), Pos = median(Pos, na.rm = T), n = n(), leading = mean(Pos < 6, na.rm = T)) %>% ungroup() %>% arrange(-m) -> spr

final3km <- st10 %>% group_by(TimeStamp) %>% filter(min(kmToFinish)>0.5) %>% ungroup() %>% filter(kmToFinish > 0.5 & kmToFinish < 3.01)

final3km %>% group_by(Bib) %>% summarize(m = mean(kph, na.rm = T), medPos = median(Pos, na.rm = T), n = n(), leading = mean(Pos < 6, na.rm = T)) %>% ungroup() %>% arrange(-m) -> f3k

#
#
#

all_stages %>%
  
  inner_join(riders, by = c("Bib")) %>%
  
  filter(kmToFinish > 0) %>% 
  
  mutate(primoz = ifelse(Bib == "11", kmToFinish, NA)) %>% 
  
  group_by(TimeStamp) %>%
  mutate(primoz = mean(primoz, na.rm = T)) %>% 
  ungroup() %>%
  
  mutate(to_roglic = ifelse(abs(kmToFinish - primoz) < 0.2, 1, 
                            ifelse(kmToFinish < primoz, 1, 0)),
         near_roglic = ifelse(abs(kmToFinish - primoz) < 0.2, 1, 0)) %>% 
  
  filter(kmToFinish > 0) %>% 
  
  mutate(valid = ifelse(to_roglic == 1, kmToFinish, NA)) %>%
  
  group_by(Bib, firstname, lastnameshort, GC, StageId) %>% 
  summarize(to_roglic = mean(to_roglic, na.rm = T),
            near_roglic = mean(near_roglic, na.rm = T),
            furthest = min(valid, na.rm = T),
            stages = n_distinct(StageId),
            stamps = n()) %>%
  ungroup() %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT bib, rider FROM pcs_all_startlists WHERE race = 'tour de france' AND year = 2020"), by = c("Bib" = "bib")
    
  ) %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT rider, type, Date FROM clusters_riders WHERE Date BETWEEN '2020-08-27' AND '2020-09-20'") %>%
      
      inner_join(
        
        tibble(StageId = c('0400', '0600', '0700', '0800', '0900', '1200', '1300', '1400', '1500', '1600', '1700', '1800', '1900'),
               Date = c('2020-09-01', '2020-09-03', '2020-09-04', '2020-09-05', '2020-09-06',
                        '2020-09-10', '2020-09-11', '2020-09-12', '2020-09-13', '2020-09-15',
                        '2020-09-16', '2020-09-17', '2020-09-18')), by = c("Date")), 
    by = c("StageId", "rider")) -> survival_w_roglic

#

tdf_koms <- dbGetQuery(con, "SELECT * FROM climbs_from_telemetry WHERE Race = 'Tour de France' AND year = 2020") %>%
  
  mutate(StageId = ifelse(stage < 10, paste0("0", stage, "00"), paste0(stage, "00")))

#

All_list <- vector("list", 13)

Ss <- c("0900", "0800", "0600", "0400", "1300", '1400', '1500', '1600', '1700', '1800', '1200', '1900')

K = all_stages %>%
  
  group_by(Bib, StageId) %>%
  summarize(MX = max(kmToFinish)) %>%
  ungroup() %>%
  
  group_by(StageId) %>%
  summarize(MX = quantile(MX, probs = 0.9)) %>%
  ungroup() %>%
  
  filter(StageId %in% Ss)

riders_in_race <- tibble(StageId = Ss,
                         RidersTotal = c(168, 172, 172, 172, 160, 158, 157, 156, 152, 150, 161, 147))

for(S in 1:12) { 
  
  k = K %>%
    filter(StageId == Ss[[S]]) %>%
    select(MX) %>%
    .[[1]] %>%
    floor()
  
  list13 <- vector("list", k)
  
  for(x in 1:k) { 
    
    list13[[x]] <- expand_grid(
      
      ClusterType = survival_w_roglic$type %>% unique(),
      kmToFinish = x
      
    ) %>%
      
      mutate(perc = 0,
             StageId = Ss[[S]],
             n = NA) %>%
      
      rbind(
        
        survival_w_roglic %>%
          filter(StageId==Ss[[S]] & (to_roglic - near_roglic)<0.5) %>%
          group_by(ClusterType = type) %>%
          mutate(n=n()) %>% 
          ungroup() %>%
          
          filter(furthest < x) %>% 
          
          group_by(ClusterType) %>%
          summarize(perc = n() / mean(n, na.rm = T),
                    n=mean(n)) %>%
          ungroup() %>%
          mutate(kmToFinish = x,
                 StageId = Ss[[S]])) %>%
      
      group_by(ClusterType, kmToFinish) %>%
      filter(perc == max(perc, na.rm = T)) %>%
      ungroup()

  }
  
  df <- bind_rows(list13)
  
  ggplot()+
    
    geom_rect(data = tdf_koms %>%
                 filter(StageId == Ss[[S]]) %>%
                   filter(start_km < k),
               aes(xmin = start_km, xmax = end_km, ymin = 0, ymax = 1),
               
               fill = "yellow", color = "transparent", alpha = 0.5)+
    
    geom_line(data = df, aes(x = kmToFinish, y = perc, color = ClusterType), size=1)+
    
    scale_x_reverse(breaks = seq(0,ceiling(k/10)*10, ceiling(k/10)))+
    scale_y_continuous(labels = scales::percent)+
    labs(y = "Survival with Roglic", 
         subtitle = "climbs highlighted in yellow",
         title = paste0("Survival by Rider Type: Stage ", str_sub(Ss[[S]],1,2)))+
    scale_color_manual(values = c("#F02108", "gray40", "#F0A608",
                                  "#37B36C", "#16BEF2", "#162CF2"), name = "Rider Type")+
    theme(
      
      panel.grid.minor = element_blank(),
      axis.text = element_text(size=15)
      
    )
  
  ggsave(paste0("survival-prob-tdf-xxx-", Ss[[S]], ".png"), height = 7, width = 10)
  
  All_list[[S]] <- bind_rows(list13)
  
}

All <- bind_rows(All_list)

#
#
#
#


all_stages %>%
  
  inner_join(riders, by = c("Bib")) %>%
  
  filter(StageId %in% c("0900", "0800", "0600", "0400", "0200", "1300", '1500', '1600', '1700', '1800')) %>%
  
  mutate(primoz = ifelse(Bib == "11", kmToFinish, NA)) %>% 
  
  group_by(TimeStamp) %>%
  mutate(primoz = mean(primoz, na.rm = T)) %>% 
  ungroup() %>%
  
  mutate(to_roglic = ifelse(abs(kmToFinish - primoz) < 0.2, 1, 
                            ifelse(kmToFinish < primoz, 1, 0))) %>% 
  
  filter(Gradient > 3 & kmToFinish < 50 & kmToFinish > 0) %>% 
  
  group_by(Bib, firstname, lastnameshort, GC) %>% 
  summarize(to_roglic = mean(to_roglic, na.rm = T),
            stages = n_distinct(StageId),
            stamps = n()) %>%
  ungroup() -> inyellowpack

#

ggplot(inyellowpack %>% 
         filter(to_roglic > 0.66) %>%
         filter(!Bib %in% c("132", "134")), 
       aes(x = reorder(lastnameshort,to_roglic),
           y = to_roglic, 
           fill = as.factor(GC)))+
  geom_point(shape = 21, size = 3, color = "black")+
  coord_flip()+
  scale_fill_manual(values = c("#FFFF00", "gray"))+
  scale_y_continuous(labels = scales::percent_format(accuracy=1), breaks = seq(0.67, 1, 0.03))+
  
  labs(y = "% of climbing kilometers with Roglic",
       x = "",
       title = "Top GC contenders have spent >95% of critical moments together",
       subtitle = "on climbs, last 50km, stages 2, 4, 6, 8, 9")+
  
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15, face = "bold"))

ggsave("top-gc-with-roglic-thru-12.png", height = 6, width = 12)

#
#
#

climbing <- all_stages %>%

  mutate(within_01 = ifelse(kmToFinish < 0.1, TimeStamp, NA)) %>%
  
  group_by(StageId, Bib) %>%
  mutate(within_01 = min(within_01, na.rm = T)) %>%
  filter(TimeStamp <= within_01) %>%
  ungroup() %>%
    
  filter(Gradient > 0 & kmToFinish > 0) %>%

  # Orcieres Merlette St4
  # Lusette St6
  # Peyresource St8
  # Marie Blanque St9
  # Puy Mary and Neronne St13
  filter((StageId == "0400" & kmToFinish < 10.5 & kmToFinish > 0) |
           (StageId == "0200" & kmToFinish > 9 & kmToFinish < 14.5) |
           (StageId == "0600" & kmToFinish > 13.5 & kmToFinish < 25) |
           (StageId == "0800" & kmToFinish > 10 & kmToFinish < 19.5) |
           (StageId == "0900" & kmToFinish > 18.5 & kmToFinish < 26) |
           (StageId == "1300" & kmToFinish > 0 & kmToFinish < 5.4) |
           (StageId == '1500' & kmToFinish > 0 & kmToFinish < 17.7) |
           (StageId == '1600' & kmToFinish > 20 & kmToFinish < 33) |
           (StageId == '1700' & kmToFinish > 0 & kmToFinish < 22) |
           (StageId == '1800' & kmToFinish > 31.5 & kmToFinish < 37.5)) %>%
  
  group_by(Bib, StageId) %>%
  summarize(speed = mean(kph, na.rm = T),
            min = min(TimeStamp, na.rm=T),
            max = max(TimeStamp, na.rm = T),
            mindist = min(kmToFinish,na.rm=T),
            maxdist = max(kmToFinish, na.rm = T),
            n=n()) %>%
  ungroup() %>%
  
  mutate(seconds = max-min,
         meters = (maxdist - mindist)*1000,
         m_s = meters / seconds,
         kph = (meters / 1000) / (seconds / 60 / 60)) %>%
  
  group_by(StageId) %>%
  mutate(Rel = speed - mean(speed, na.rm = T),
         calc_Rel = kph / mean(kph, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(StageId) %>%
  filter((meters / max(meters, na.rm = T) > 0.90)) %>%
  mutate(rk = rank(-kph, ties.method = "first")) %>%
  mutate(x20th = ifelse(rk == 10, kph, NA)) %>%
  mutate(x20th = mean(x20th, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(RelTo20th = kph / x20th) %>%
  
  group_by(StageId) %>%
  mutate(max_meters = max(meters, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(implied_seconds = max_meters / m_s)

#

ggplot(
  
  climbing %>%
    inner_join(riders) %>%
    filter(Bib %in% c('11', '14', '61', '71', '81', '101', '161', '141', '121', '131', '94')),
  
  aes(x = implied_seconds / 60, y = kph, color = str_sub(StageId,1,2)))+
  
  geom_point(size=4, alpha = 0.5)+
  geom_smooth(method = "lm", alpha = 0.5)+
  
  labs(x = "climb duration (mins)",
       y = "climbing speed (km/h)",
       color = "Stage",
       title = "Climbing speeds in 2020 TDF")+
  
  theme(axis.text = element_text(size = 16))

#

ggplot(
  
  climbing %>%
    inner_join(riders) %>%
    
    mutate(top5 = ifelse(Bib %in% c('11', '131', '61', '101', '94'), 1, 0)) %>%
    
    group_by(GC, StageId) %>%
    filter(top5 == 1 | rank(-kph, ties.method = "first") <= 5) %>%
    ungroup(),
  
  aes(y = str_sub(StageId,1,2),  x = kph, color = GC))+

  geom_boxplot()+
  
  labs(y = "Stage",
       x = "climbing speed (km/h)",
       color = "Stage",
       title = "Climbing speeds in 2020 TDF")+
  
  theme(axis.text = element_text(size = 16))

#

top_groups <- climbing %>%
  inner_join(riders) %>%
  
  mutate(top5 = ifelse(Bib %in% c('11', '131', '61', '101', '94'), 1, 0)) %>%
  
  mutate(type = ifelse(top5 == 1, "Top 5\nFinishers",
                       ifelse(GC == "GC", "Other GC\nRiders", "Remaining\nPeloton"))) %>%
  
  group_by(type, StageId) %>%
  filter(top5 == 1 | rank(-kph, ties.method = "first") <= 5) %>%
  ungroup() %>%
  
  group_by(type, StageId) %>%
  summarize(avg = mean(kph, na.rm = T)) %>%
  ungroup()


top_groups$type <- factor(top_groups$type, levels = c("Top 5\nFinishers", "Other GC\nRiders", "Remaining\nPeloton"))

#

ggplot(
  
  top_groups,
  
  aes(y = str_sub(StageId,1,2),  x = avg, fill = type))+
  
  geom_point(size = 7, shape = 21, stroke = 2, color = "black", alpha = 0.5)+
  
  labs(y = "Stage",
       x = "climbing speed (km/h)",
       title = "Climbing speeds in 2020 TDF",
       subtitle = "Top 5 on each decisive climb from each group")+
  
  scale_x_continuous(breaks = c(17.5, 20, 22.5, 25, 27.5))+
  
  theme(axis.text = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"),
        legend.position = "bottom")+
  
  scale_fill_manual(values = c("gold", "gray30", "dark red"), name = "")

ggsave("climbing-speeds-tdf-2020.png", height = 7, width = 11)

#

interesting <- climbing %>%
  
  mutate(Bib = as.character(Bib)) %>%
  
  left_join(tibble(StageId = c('0200', '0200', '0200',
                               '0600', '0600', '0600',
                               '0800', '0800', '0800',
                               '0900', '0900',
                               '1300', '1300', '1300',
                               '1500', '1500', '1500',
                               '1600', '1600', '1600',
                               '1700', '1700', '1700',
                               '1800', '1800', '1800'),
                   
                   Bib = c('161', '204', '41',
                           '146', '132', '124',
                           '118', '106', '36',
                           '1', '204',
                           '24', '28', '76',
                           '16', '62', '141',
                           '24', '3', '58',
                           '141', '16', '3',
                           '3', '5', '18'),
                   Keep = 1), by = c("StageId", "Bib")) %>%
  
  filter(Keep == 1 | Bib %in% c('94', '11', '101', '131', '61')) %>%
  
  mutate(stage_winner = ifelse(StageId == '0200' & Bib == '41', 1,
                               ifelse(StageId == '0400' & Bib == '11', 1,
                                      ifelse(StageId == '0600' & Bib == '146', 1,
                                             ifelse(StageId == '0800' & Bib == '36', 1,
                                                    ifelse(StageId == '0900' & Bib == '131', 1,
                                                           ifelse(StageId == '1300' & Bib == '76', 1,
                                                                  ifelse(StageId == '1500' & Bib == '131', 1,
                                                                         ifelse(StageId == '1600' & Bib == '24', 1,
                                                                                ifelse(StageId == '1700' & Bib == '141', 1,
                                                                                       ifelse(StageId == '1800' & Bib == '5', 1, 0)))))))))))

#

ggplot(
  
  interesting %>%
    mutate(top5 = ifelse(is.na(Keep), "GC Top 5", "Other riders")),
  
  aes(y = str_sub(StageId,1,2),  x = kph, label = Bib, fill = top5, shape = as.factor(stage_winner)))+
  
  geom_point(size = 4, alpha = 0.5, color = "black")+
  ggrepel::geom_label_repel(color = "black", fill = "white", size = 4)+
  
  labs(y = "Stage",
       x = "climbing speed (km/h)",
       title = "Climbing speeds in 2020 TDF",
       subtitle = "Top 5 on each decisive climb from each group // stage winner as triangle")+
  
  scale_x_continuous(breaks = c(15, 17.5, 20, 22.5, 25, 27.5, 30))+
  
  scale_shape_manual(values = c(21, 24))+
  
  theme(axis.text = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold"))+
  
  scale_fill_manual(values = c("gold", "dark red"), name = "")+
  
  guides(shape = F)

#

ggsave("climbing-speeds-tdf-2020.png", height = 6.2, width = 11)

#

total <- climbing %>% 
  mutate(primoz = ifelse(Bib == '11', seconds, NA)) %>% 
  
  group_by(StageId) %>% 
  mutate(primoz = mean(primoz, na.rm = T)) %>% 
  ungroup() %>% 
  
  group_by(Bib) %>% 
  summarize(climbing_speed = mean(RelTo20th, na.rm = T), 
            n = n(), 
            seconds = sum(primoz, na.rm = T)) %>% 
  ungroup() %>% 
  
  mutate(primoz = ifelse(Bib == '11', climbing_speed, NA), 
         primoz = mean(primoz, na.rm = T)) %>%
  
  mutate(diff = climbing_speed - primoz, diff = seconds * diff)

#

ggplot(total %>% inner_join(riders) %>%
         filter(n==4) %>%
         filter(!Bib %in% c("132", "134")), 
       aes(x = climbing_speed-1,
           fill = as.factor(GC), group = paste0(lastnameshort, firstname)))+
  geom_histogram(binwidth = 0.02, color = "black")+
  scale_fill_manual(values = c("#FFFF00", "gray"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "relative climbing speed vs average",
       y = "",
       title = "best climbers have been ~27% better than average",
       subtitle = "final climbs of stages 4, 6, 8, 9")+
  
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15, face = "bold"), axis.text.y = element_blank(), axis.ticks.y = element_blank())

#

sprinting <- all_stages %>%
  
  filter(kmToFinish > 0) %>%
  
  filter((StageId == "0700" & kmToFinish < 2) |
           (StageId == "1000" & kmToFinish < 2) |
           (StageId == "1100" & kmToFinish < 2)) %>%
  
  group_by(Bib, StageId) %>%
  summarize(speed = mean(kph, na.rm = T),
            min = min(TimeStamp, na.rm=T),
            max = max(TimeStamp, na.rm = T),
            mindist = min(kmToFinish,na.rm=T),
            maxdist = max(kmToFinish, na.rm = T),
            n=n()) %>%
  ungroup() %>%
  
  mutate(seconds = max-min,
         meters = (maxdist - mindist)*1000,
         m_s = meters / seconds,
         kph = (meters / 1000) / (seconds / 60 / 60)) %>%
  
  group_by(StageId) %>%
  mutate(calc_Rel = kph - mean(kph, na.rm = T)) %>%
  ungroup()

#
#
#
#
#

final_sprint_11 <- all_stages %>%
  filter(StageId == "1100" & kmToFinish > 0 & kmToFinish < 3)

ggplot(final_sprint_11 %>%
         filter(Bib %in% c("43", "48", "151", "21", "18")),
       aes(x = Longitude, y = Latitude, color = as.factor(Bib)))+
  
  geom_point()+
  
  facet_wrap(~TimeStamp)

#

final_sprint_11 %>%
  filter(Bib %in% c("43", "48", "151", "21", "18", "211", "203")) %>%
  
  group_by(TimeStamp) %>%
  mutate(Rel = kmToFinish - min(kmToFinish, na.rm = T)) %>%
  ungroup() -> abc

#
#
#
#
#
#
#

puy_mary <- all_stages %>%
  
  inner_join(riders %>%
               mutate(Rider = paste0(firstname, " ", lastnameshort))) %>%

  filter(kmToFinish > 0) %>%
  
  filter((StageId == "1300" & kmToFinish > 0.1 & kmToFinish < 15)) %>%
  
  arrange(Rider, TimeStamp) %>%
  
  group_by(Rider, StageId) %>%
  mutate(dist = (lag(kmToFinish) - kmToFinish),
         midpt = ((kmToFinish)+(lag(kmToFinish)))/2,
         time = TimeStamp - lag(TimeStamp)) %>%
  ungroup() %>%
  
  mutate(kph_calc = dist / (time / 60 / 60)) %>%
  
  filter(GC == "GC" | Bib %in% c("24", "28", "76", "41", "97", "77", "171", "133",
                                 "56", "218", "44", "132")) %>%
  
  group_by(K = floor(midpt / 0.05)) %>%
  filter(n() > 5) %>%
  mutate(Rel = kph_calc - mean(kph_calc, na.rm = T)) %>%
  ungroup() %>%
  
  filter(Bib %in% c("76", "1", "11", "24", "131")) %>%
  
  filter(abs(Rel) < 15)

#

ggplot(puy_mary %>% filter(kmToFinish < 5.4),
       
       aes(x = kmToFinish, y = Rel, color = as.factor(lastnameshort)))+
  geom_hline(yintercept = 0)+
  #geom_line()+
  
  geom_smooth(se=F, method = "loess", span = 0.4, size = 1.5, alpha = 0.5)+
  
  #geom_point()+
  
  scale_color_manual(values = c("black", "blue", "#F179B1", "dark red",
                                "gold", "#37B36C"), name = "")+
  
  scale_x_reverse()+
  
  geom_point(data = puy_mary %>%
               filter(kmToFinish < 5.4) %>%
              select(kmToFinish, Gradient) %>%
              unique(), aes(x = kmToFinish, y = 9, fill = Gradient), alpha = 0.5, shape = 21, size = 4, color = "transparent")+
  
  scale_fill_gradient2(low = "blue", mid = "white", high = "red")


#
#
#
#
#
#
#
#

jumbo_on_climbs <- all_stages %>%
  
  mutate(within_01 = ifelse(kmToFinish < 0.1, TimeStamp, NA)) %>%
  
  group_by(StageId, Bib) %>%
  mutate(within_01 = min(within_01, na.rm = T)) %>%
  filter(TimeStamp <= within_01) %>%
  ungroup() %>%
  
  filter(Gradient > 0 & kmToFinish > 0) %>%
  
  # Orcieres Merlette St4
  # Lusette St6
  # Peyresource St8
  # Marie Blanque St9
  # Puy Mary and Neronne St13
  filter((StageId == "0400" & kmToFinish < 10.5 & kmToFinish > 0) |
           (StageId == "0200" & kmToFinish > 9 & kmToFinish < 14.5) |
           (StageId == "0600" & kmToFinish > 13.5 & kmToFinish < 25) |
           (StageId == "0800" & kmToFinish > 10 & kmToFinish < 19.5) |
           (StageId == "0900" & kmToFinish > 18.5 & kmToFinish < 26) |
           (StageId == "1300" & kmToFinish > 0 & kmToFinish < 5.4) |
           (StageId == '1500' & kmToFinish > 0 & kmToFinish < 17.7) |
           (StageId == '1600' & kmToFinish > 20 & kmToFinish < 33) |
           (StageId == '1700' & kmToFinish > 0 & kmToFinish < 22) |
           (StageId == '1800' & kmToFinish > 31.5 & kmToFinish < 37.5)) %>%
  
  filter((Bib >= 11 & Bib <= 18) | Bib %in% c(61, 101, 141, 131, 62, 63, 148, 1, 5, 81, 121)) %>%
  
  arrange(StageId, Bib, TimeStamp) %>%
  
  group_by(StageId, Bib) %>%
  mutate(dist_diff = 1000 * (lag(kmToFinish) - kmToFinish),
         time_diff = TimeStamp - lag(TimeStamp)) %>%
  ungroup() %>%
  
  filter(dist_diff >= 0 & time_diff > 0) %>%
  
  mutate(ms_diff = dist_diff / time_diff,
         ms_diff = ifelse(ms_diff > 10, 10, ms_diff)) %>%
  
  mutate(top5 = ifelse(Bib %in% c(11,131,141,101,61), ms_diff, NA)) %>%
  
  group_by(TimeStamp) %>%
  mutate(top5 = mean(top5, na.rm = T)) %>%
  ungroup() %>%
  
  filter(!is.na(top5)) %>% 
  
  select(top5, ms_diff, dist_diff, time_diff, Bib, kmToFinish, Gradient, StageId, TimeStamp) %>%
  
  mutate(VsPR = (ms_diff / top5)-1) %>%
  
  filter(VsPR < 1) %>%
  
  mutate(VsPR = ifelse(VsPR < -3, -3, VsPR)) %>%
  
  mutate(speed_last_5 = ((VsPR) + lag(VsPR, 1) + lag(VsPR, 2) + lag(VsPR, 3) + lag(VsPR, 4)) / 5,
         raw_speed_last_5 = ((ms_diff) + lag(ms_diff, 1) + lag(ms_diff, 2) + lag(ms_diff, 3) + lag(ms_diff, 4)) / 5,
         leaders_speed_last_5 = ((top5) + lag(top5, 1) + lag(top5, 2) + lag(top5, 3) + lag(top5, 4)) / 5,
         Gradient_last_5 = ((Gradient) + lag(Gradient, 1) + lag(Gradient, 2) + lag(Gradient, 3) + lag(Gradient)) / 5) %>%
  
  inner_join(riders)

#

top5_speed_vs_gradient <- lm(raw_speed_last_5 ~ Gradient_last_5, 
                             data = jumbo_on_climbs %>% 
                               filter(Bib %in% c('11','101','131','61','94')))
                               
summary(top5_speed_vs_gradient)

jumbo_on_climbs <- cbind(
  
  pred_speed_grade = predict(top5_speed_vs_gradient, jumbo_on_climbs),
  
  jumbo_on_climbs) %>%
  
  mutate(leaders_vs_expected = leaders_speed_last_5 / pred_speed_grade)

#

ggplot(jumbo_on_climbs %>% filter(StageId == '1500' & Bib %in% c('12','14','16','18', '62', '148')), 
       
       aes(x = (((TimeStamp / 3600)-444024) * 60)-25357, 
           y = speed_last_5, 
           color = as.factor(lastnameshort)))+
  
  geom_hline(yintercept = 0)+
  geom_line(size = 1, color = "black")+
  geom_point(size=2)+
  
  facet_wrap(~lastnameshort)+
  
  labs(x = "minutes from start of climb",
       y = "percent faster/slower than GC leaders",
       title = 'Domestiques on Grand Colombier',
       subtitle = "speed relative to GC leaders")+
  
  scale_y_continuous(labels = scales::percent)

#
#
#

ggplot(jumbo_on_climbs %>% filter(StageId == '1500' & Bib %in% c('1', '81')), 
       
       aes(x = (((TimeStamp / 3600)-444024) * 60)-25357, 
           y = speed_last_5, 
           color = as.factor(lastnameshort)))+
  
  geom_hline(yintercept = 0)+
  geom_line(size = 1, color = "black")+
  geom_point()+
  
  facet_wrap(~lastnameshort, ncol = 1)+
  
  labs(x = "minutes from start of climb",
       y = "percent faster/slower than GC leaders",
       title = 'Bernal & Quintana on Grand Colombier',
       subtitle = "speed relative to GC leaders")+
  
  scale_y_continuous(labels = scales::percent)+
  
  scale_color_manual(values = c("navy", "dark red"), guide=F)+
  
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
  )

#

ggplot(jumbo_on_climbs %>% filter(StageId == '1500' & Bib %in% c('11')) %>%
         
         mutate(mins_climb = (((TimeStamp / 3600)-444024) * 60)-25357) %>%
         
         mutate(section = ifelse(mins_climb < 24, 'WvA',
                                 ifelse(mins_climb < 30, 'Bennett',
                                        ifelse(mins_climb < 42, "TomDom", "Finish")))), 
       
       aes(x = mins_climb, 
           y = leaders_speed_last_5 * 3600 / 1000, 
           color = as.factor(section)))+
  
  geom_line(size = 1, color = "black")+
  geom_point(size=3)+
  
  labs(x = "minutes from start of climb",
       y = "KM / hour",
       title = 'GC group on Grand Colombier',
       subtitle = "rolling average of speed last 2 minutes")+
  
  guides(color = FALSE)+
  
  theme(axis.text = element_text(size = 16),
        plot.title = element_text(size = 24, face = "bold"),
        )

ggsave("gc-group-grand-colombier.png", height = 6.2, width = 11)

#

ggplot(jumbo_on_climbs %>%
         
         filter(StageId == '1800' & Bib %in% c('11')) %>%

         group_by(StageId) %>%
         mutate(mins_climb = (TimeStamp - min(TimeStamp, na.rm = T)) / 60) %>%
         ungroup(), 
       
       aes(x = mins_climb, 
           y = leaders_vs_expected))+
  
  geom_hline(yintercept = 1)+
  
  geom_point(size=3, color = "gold")+
  geom_smooth(color = "#404040", size = 2, se = F, span = 0.4)+
  
  scale_y_continuous(labels = scales::percent)+
  
  labs(x = "minutes from start of climb",
       y = "vs expected speed based on gradient",
       title = 'GC group on Stage 16 St. Nizier Climb',
       subtitle = "relative to expected speed (>100% is faster than expected)")+
  
  guides(color = FALSE)+
  
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
  )

ggsave("gc-group-4-orc-merl.png", height = 6.2, width = 11)

#
#
#
#
#

leaders_speed <- jumbo_on_climbs %>%
  
  filter(Bib %in% c(11, 131, 18)) %>%
  
  group_by(StageId, lastnameshort, Bib) %>%
  
  summarize(speed_own = mean(raw_speed_last_5, na.rm = T),
            vs_gradient = median(raw_speed_last_5 / pred_speed_grade, na.rm = T),
            measurements = n()) %>%
  ungroup()
