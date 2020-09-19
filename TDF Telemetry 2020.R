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
  
  mutate(GC = ifelse(Bib %in% c("1", "3", "11", "14", "22", "31", "51", '61', '71', '81',
                                '76', '81', '91', '94', '101', '104', '121',
                                '131', '161', '141', '218'), "GC","Helper"))

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
  filter((StageId == "0400" & kmToFinish < 10.5) |
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
  filter(abs((meters / max(meters))-1) < 0.3) %>%
  mutate(rk = rank(-kph, ties.method = "first")) %>%
  mutate(x20th = ifelse(rk == 10, kph, NA)) %>%
  mutate(x20th = mean(x20th, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(RelTo20th = kph / x20th)

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


