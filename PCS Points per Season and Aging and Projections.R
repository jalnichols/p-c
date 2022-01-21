
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

SEASON_DATE <- paste0(seq(2010,2021,1),"-12-31")

PAGES <- seq(0,3000,100)

#

for(s in 1:length(SEASON_DATE)) {
  
  for(p in 1:length(PAGES)) {
    
    pg <- paste0('https://www.procyclingstats.com/rankings.php?date=', SEASON_DATE[[s]], 
                 '&offset=', 
                 PAGES[[p]], 
                 '&p=me&s=season-individual') %>%
      read_html() 
    
    d <- pg %>%
      
      html_nodes("tbody") %>%
      html_nodes("tr") %>%
      html_nodes("td") %>%
      html_nodes("a") %>%
      html_attr(name = "href") %>% 
      enframe(name = NULL) %>%
      filter(str_detect(value, "rider/"))
    
    d2 <- pg %>%
      
      html_nodes("tbody") %>%
      html_table() %>%
      .[[1]]
    
    if(nrow(d) > 0) {
      
      
      results <- cbind(d2, d %>% rename(rider_url = value)) %>%
        rename(rider = X4, team = X5, PCS_PTS = X6) %>%
        select(rider, rider_url, team, PCS_PTS) %>%
        mutate(year = lubridate::year(SEASON_DATE[[s]]))
      
      dbWriteTable(con, "pcs_pts_riders_season", results, append = TRUE, row.names = FALSE)
      
      print(p)
      
      Sys.sleep(4)
      
    }
  }
  
  print(s)
  
}

#

riders_to_scrape <- dbReadTable(con, "pcs_pts_riders_season") %>%
  
  mutate(SEASON_DATE = as.Date(paste0(year, "-06-30"))) %>%

  left_join(dbGetQuery(con, "SELECT rider_url, date as dob FROM rider_attributes"), by = c("rider_url")) %>%
  
  mutate(age = as.numeric(SEASON_DATE-as.Date(dob))/365.25) %>%
  
  select(-dob, -SEASON_DATE) %>%
  unique()

#

yearly_differential <- riders_to_scrape %>%
  
  group_by(year) %>%
  summarize(avg_pts = mean(PCS_PTS)) %>%
  ungroup() %>%
  
  mutate(relative_change = avg_pts/mean(avg_pts, na.rm = T))

#

season_season <- riders_to_scrape %>%
  
  filter(!is.na(age)) %>%
  
  mutate(next_year = year+1) %>%

  select(-team) %>%

  inner_join(riders_to_scrape %>%
               
               filter(!is.na(age)) %>%
               select(-team, -rider), by = c("rider_url", "next_year" = "year")) %>%
  
  inner_join(yearly_differential %>% select(year, relative_change), by = c("year")) %>%
  
  inner_join(yearly_differential %>% select(year, relative_change), by = c("next_year" = 'year')) %>%
  
  mutate(PCS_PTS.x = relative_change.x * PCS_PTS.x,
         PCS_PTS.y = relative_change.y * PCS_PTS.y)

#

season_season %>% 
  group_by(f = round(age.x)) %>% 
  summarize(pcs_first = mean(PCS_PTS.x, na.rm = T),
            pcs_second = mean(PCS_PTS.y, na.rm = T),
            matches = n()) %>% 
  ungroup() %>% 
  
  mutate(delta = (pcs_second / pcs_first)) -> ss_aging

#

ss_aging %>%
  filter(matches >= 100) %>%
  
  ggplot(aes(x = pcs_first, xend = pcs_second, y = f, yend = f, color = delta))+
  
  geom_segment(arrow = arrow(length = unit(0.02, "npc")), size = 1.5)+
  geom_point(color = "black", size = 3)+
  labs(x = "PCS Points in Season",
       y = "Age",
       title = "Riders increase PCS points through age 26 on average",
       subtitle = "average decline post age 33 is 24% yearly")+
  scale_y_continuous(breaks = seq(19,37,2))+
  
  theme(panel.grid.minor.y = element_blank())+
  
  scale_color_gradientn(colors = c("#FF4900", "#FFC500", "gray50", "#84FF00", "#00FF29"),
                        values = c(0, 0.1, 0.25, 0.6, 1),
                        guide = FALSE)

#

library(mgcv)

#

GAM_SEASON2SEASON <- gam(PCS_PTS ~ s(age, k = 10) + rider,
                         data = riders_to_scrape %>%
                           group_by(rider) %>%
                           filter(n() >= 4) %>%
                           filter(!is.na(age)))

#

coefs <- summary(GAM_SEASON2SEASON)$p.table %>% as.data.frame() %>% rownames_to_column()

#

predicted_curve <- cbind(
  predicted = mgcv::predict.gam(GAM_SEASON2SEASON, tibble(age = seq(19,38,1), rider = "Hansen Adam")),
    tibble(age = seq(19,38,1))) %>%
  
  mutate(predicted = predicted + 0)

#

ggplot(predicted_curve, aes(x = age, y = predicted))+
  geom_point()+
  geom_line()

#

predicted_curve %>%
  filter(age > 18 & age < 39) %>%
  mutate(next_age = age+1) %>%
  inner_join(predicted_curve, by = c("next_age" = "age")) %>%
  
  mutate(delta = predicted.y / predicted.x) %>%
  
  ggplot(aes(x = predicted.x, xend = predicted.y, y = age, yend = age, color = delta))+
  
  geom_segment(arrow = arrow(length = unit(0.02, "npc")), size = 1.5)+
  geom_point(color = "black", size = 3)+
  labs(x = "PCS Points in Season",
       y = "Age",
       title = "Riders increase PCS points through age 27 on average",
       subtitle = "average decline post age 33 is 28% yearly")+
  scale_y_continuous(breaks = seq(19,37,2))+
  
  theme(panel.grid.minor.y = element_blank())+
  
  scale_color_gradientn(colors = c("dark red", "#FF4900", "#FFC500", "gray50", "#84FF00", "#00FF29"),
                        values = c(0, 0.15, 0.35, 0.67, 0.95, 1),
                        guide = FALSE)

#
#
#
#
#
#
#
#

# a very basic season to season model shows 80% of PCS Points are retained year over year

season3_season <- riders_to_scrape %>%
  
  inner_join(yearly_differential %>% select(year, relative_change), by = c("year")) %>%  
  
  mutate(PCS_PTS = relative_change * PCS_PTS) %>%
  
  arrange(rider, year) %>%
  
  group_by(rider) %>%
  mutate(y_correct = ((RcppRoll::roll_max(year, n = 3, fill = 0, align = "right")-
                            RcppRoll::roll_min(year,n = 3, fill = 0, align = "right")) == 2)) %>%
  
  mutate(last3 = RcppRoll::roll_mean(PCS_PTS, n = 3, align = "right", fill = NA)) %>%
  filter(y_correct == TRUE) %>%
  mutate(age = age - 1) %>%
  filter(!is.na(age)) %>%
  
  mutate(next_year = year+1) %>%
  
  select(-team, -y_correct) %>%
  
  inner_join(riders_to_scrape %>%
               
               filter(!is.na(age)) %>%
               select(-team, -rider), by = c("rider_url", "next_year" = "year"))

#

s3s_model <- season3_season %>%
  
  inner_join(dbGetQuery(con, "SELECT DISTINCT rider_url, team, level, Season 
                        FROM team_rosters_season"), by = c("rider_url", "next_year" = "Season")) %>%
  
  lm(PCS_PTS.y ~ PCS_PTS.x + level, 
     data = .)

summary(s3s_model)

s3s_gam <- mgcv::gam(PCS_PTS.y ~ s(PCS_PTS.x, k = 10) + PCS_PTS.x:rel_age + level,
                     data = season3_season %>%
                       
                       left_join(dbGetQuery(con, "SELECT DISTINCT rider_url, team, level, Season 
                        FROM team_rosters_season"), by = c("rider_url", "next_year" = "Season")) %>%
                       
                       mutate(rel_age = age.x - 27))

summary(s3s_gam)

#

s3gam_outputs <- expand_grid(PCS_PTS.x = seq(0,3500,100), rel_age = seq(-9,13,1), level = "WT") %>%
  mutate(PCS_PTS.y = mgcv::predict.gam(s3s_gam, .))

#

s3gam_outputs %>%
  filter(rel_age %in% c(-5,1,8)) %>%
  ggplot(aes(x = PCS_PTS.x, y = PCS_PTS.y, color = as.factor(rel_age+27)))+
  geom_point(size=2)+
  geom_abline(slope = 1, intercept = 0)+
  geom_abline(slope = 0.80, intercept = 0, color = "blue")+
  labs(x = "PCS PTS Year 1",
       y = "PCS PTS Year 2",
       title = "PCS PTS retention year to year",
       subtitle = "Peak age riders retain ~80% of points")+
  
  annotate("text", x = 500, y = 3000, label = "Black line is\n100% retention")+
  
  annotate("text", x = 1500, y = 3000, label = "Blue line is\n80% retention", color = "blue")+
  
  scale_color_manual(values = c("dark red", "#37B36C", "orange"), name = "Age")

#


apply_to_2021 <- dbGetQuery(con, "SELECT DISTINCT * FROM team_rosters_season WHERE Season = 2022") %>%
  unique() %>%
  left_join(dbGetQuery(con, "SELECT rider_url, date as dob FROM rider_attributes") %>%
              filter(!is.na(dob)) %>%
              unique(), by = c("rider_url")) %>%
  
  left_join(riders_to_scrape %>% filter(year >= 2019 & year < 2022) %>% 
              select(rider_url, year, PCS_PTS) %>%
              unique() %>%
              group_by(rider_url) %>%
              summarize(PCS_PTS = mean(PCS_PTS, na.rm = T)) %>%
              ungroup(), by = c("rider_url")) %>%
  mutate(rel_age = (as.numeric(as.Date('2022-06-30')-as.Date(dob))/365.25) - 27) %>%
  rename(PCS_PTS.x = PCS_PTS) %>%
  mutate(PCS_PTS.x = ifelse(is.na(PCS_PTS.x), 0, PCS_PTS.x)) %>%
  mutate(PCS_2022 = mgcv::predict.gam(s3s_gam, .))

#

apply_to_2021 %>% 
  group_by(team, level) %>% 
  summarize(x2021 = sum(PCS_PTS.x), 
            x2022 = sum(PCS_2022)) %>%
  ungroup() %>%
  
  mutate(team = str_trim(team)) %>%
  mutate(team_join = case_when(team == "AG2R Citroen Team" ~ "AG2R Citroen Team",
                               team == "Astana Qazaqstan Team" ~ "Astana - Premier Tech",
                               team == "Bahrain - Victorious" ~ "Bahrain - Victorious",
                               team == "BORA - hansgrohe" ~ "BORA - hansgrohe",
                               team == "Cofidis" ~ "Cofidis", 
                               team == "EF Education - Nippo" ~ "EF Education - Nippo",
                               team == "Groupama - FDJ" ~ "Groupama - FDJ",
                               team == "INEOS Grenadiers" ~ "INEOS Grenadiers",
                               team == "Intermarche - Wanty - Gobert Materiaux" ~ "Intermarche - Wanty - Gobert Materiaux", 
                               team == "Israel - Premier Tech" ~ "Israel Start-Up Nation",
                               team == "Lotto Soudal" ~ "Lotto Soudal",
                               team == "Movistar Team" ~ "Movistar Team",
                               team == "Quick-Step Alpha Vinyl Team" ~ "Deceuninck - Quick Step", 
                               team == "Team BikeExchange Jayco" ~ "Team BikeExchange",
                               team == "Team DSM" ~ "Team DSM",
                               team == "Team Jumbo-Visma" ~ "Team Jumbo-Visma", 
                               team == "Team Qhubeka NextHash" ~ "Team Qhubeka NextHash",
                               team == "Trek - Segafredo" ~ "Trek - Segafredo",
                               team == "UAE-Team Emirates" ~ "UAE-Team Emirates",
                               team == "Alpecin-Fenix" ~ "Alpecin-Fenix",
                               team == "B&B Hotels - KTM" ~ "B&B Hotels p/b KTM",
                               team == "Bardiani-CSF-Faizane" ~ "Bardiani-CSF-Faizane",
                               team == "Bingoal Pauwels Sauces WB" ~ "Bingoal - Wallonie Bruxelles",
                               team == "Burgos-BH" ~ "Burgos-BH",
                               team == "Caja Rural - Seguros RGA" ~ "Caja Rural - Seguros RGA",
                               team == "Drone Hopper -  Androni Giocattoli" ~ "Androni Giocattoli - Sidermec", 
                               team == "EOLO-Kometa" ~ "EOLO-Kometa",
                               team == "Equipo Kern Pharma" ~ "Equipo Kern Pharma", 
                               team == "Euskaltel - Euskadi" ~ "Euskaltel - Euskadi",
                               team == "Gazprom - RusVelo" ~ "Gazprom - RusVelo",
                               team == "Human Powered Health" ~ "Rally Cycling",
                               team == "Sport Vlaanderen - Baloise" ~ "Sport Vlaanderen - Baloise",
                               team == "Team Arkea Samsic" ~ "Team Arkea Samsic",
                               team == "Team Novo Nordisk" ~ "Team Novo Nordisk",
                               team == "Team TotalEnergies" ~ "Team TotalEnergies",
                               team == "Uno-X Pro Cycling Team" ~ "Uno-X Pro Cycling Team",
                               TRUE ~ "")) %>%
  left_join(
    dbGetQuery(con, "SELECT DISTINCT * FROM team_rosters_season WHERE Season = 2021") %>%
      unique() %>%
      left_join(riders_to_scrape %>% filter(year == 2021) %>% select(rider_url, PCS_PTS) %>% unique(), by = c("rider_url")) %>%
      mutate(PCS_PTS = ifelse(is.na(PCS_PTS), 0, PCS_PTS)) %>%
      group_by(team = str_trim(team)) %>%
      summarize(team2021 = sum(PCS_PTS)) %>%
      ungroup(), by = c("team_join" = "team")) %>%
  
  mutate(diff = sum(x2022)/sum(team2021),
         x2022 = x2022/diff) %>%
  mutate(delta = x2022/ x2021) %>%
  
  mutate(delta_2022_vs_actual = x2022 / team2021,
         delta_2021_vs_actual = x2021 / team2021) -> yearly_projs

#

s2s_model <- season_season %>%
  
  inner_join(dbGetQuery(con, "SELECT DISTINCT rider_url, team, level, Season 
                        FROM team_rosters_season"), by = c("rider_url", "next_year" = "Season")) %>%
  
  lm(PCS_PTS.y ~ PCS_PTS.x + level, 
     data = .)

summary(s2s_model)

s2s_gam <- mgcv::gam(PCS_PTS.y ~ s(PCS_PTS.x, k = 10) + PCS_PTS.x:rel_age + level,
                     data = season_season %>%
                       
                       left_join(dbGetQuery(con, "SELECT DISTINCT rider_url, team, level, Season 
                        FROM team_rosters_season"), by = c("rider_url", "next_year" = "Season")) %>%
                      
                       mutate(rel_age = age.x - 27))

summary(s2s_gam)
                       

#

gam_outputs <- expand_grid(PCS_PTS.x = seq(0,3500,100), rel_age = seq(-9,13,1), level = "WT") %>%
  mutate(PCS_PTS.y = mgcv::predict.gam(s2s_gam, .))

#

gam_outputs %>%
  filter(rel_age %in% c(-5,1,8)) %>%
  ggplot(aes(x = PCS_PTS.x, y = PCS_PTS.y, color = as.factor(rel_age+27)))+
  geom_point(size=2)+
  geom_abline(slope = 1, intercept = 0)+
  geom_abline(slope = 0.80, intercept = 0, color = "blue")+
  labs(x = "PCS PTS Year 1",
       y = "PCS PTS Year 2",
       title = "PCS PTS retention year to year",
       subtitle = "Peak age riders retain ~80% of points")+
  
  annotate("text", x = 500, y = 3000, label = "Black line is\n100% retention")+
  
  annotate("text", x = 1500, y = 3000, label = "Blue line is\n80% retention", color = "blue")+
  
  scale_color_manual(values = c("dark red", "#37B36C", "orange"), name = "Age")

#
#
#

apply_to_2021 <- dbGetQuery(con, "SELECT DISTINCT * FROM team_rosters_season WHERE Season = 2022") %>%
  unique() %>%
  left_join(dbGetQuery(con, "SELECT rider_url, date as dob FROM rider_attributes") %>%
              filter(!is.na(dob)) %>%
              unique(), by = c("rider_url")) %>%
  
  left_join(riders_to_scrape %>% filter(year == 2021) %>% select(rider_url, PCS_PTS) %>% unique(), by = c("rider_url")) %>%
  mutate(rel_age = (as.numeric(as.Date('2022-06-30')-as.Date(dob))/365.25) - 27) %>%
  rename(PCS_PTS.x = PCS_PTS) %>%
  mutate(PCS_PTS.x = ifelse(is.na(PCS_PTS.x), 0, PCS_PTS.x)) %>%
  mutate(PCS_2022 = mgcv::predict.gam(s2s_gam, .))

#

apply_to_2021 %>% 
  group_by(team, level) %>% 
  summarize(x2021 = sum(PCS_PTS.x), 
            x2022 = sum(PCS_2022)) %>%
  ungroup() %>%
  
  mutate(team = str_trim(team)) %>%
  mutate(team_join = case_when(team == "AG2R Citroen Team" ~ "AG2R Citroen Team",
                               team == "Astana Qazaqstan Team" ~ "Astana - Premier Tech",
                               team == "Bahrain - Victorious" ~ "Bahrain - Victorious",
                               team == "BORA - hansgrohe" ~ "BORA - hansgrohe",
                               team == "Cofidis" ~ "Cofidis", 
                               team == "EF Education - Nippo" ~ "EF Education - Nippo",
                               team == "Groupama - FDJ" ~ "Groupama - FDJ",
                               team == "INEOS Grenadiers" ~ "INEOS Grenadiers",
                               team == "Intermarche - Wanty - Gobert Materiaux" ~ "Intermarche - Wanty - Gobert Materiaux", 
                               team == "Israel - Premier Tech" ~ "Israel Start-Up Nation",
                               team == "Lotto Soudal" ~ "Lotto Soudal",
                               team == "Movistar Team" ~ "Movistar Team",
                               team == "Quick-Step Alpha Vinyl Team" ~ "Deceuninck - Quick Step", 
                               team == "Team BikeExchange Jayco" ~ "Team BikeExchange",
                               team == "Team DSM" ~ "Team DSM",
                               team == "Team Jumbo-Visma" ~ "Team Jumbo-Visma", 
                               team == "Team Qhubeka NextHash" ~ "Team Qhubeka NextHash",
                               team == "Trek - Segafredo" ~ "Trek - Segafredo",
                               team == "UAE-Team Emirates" ~ "UAE-Team Emirates",
                               team == "Alpecin-Fenix" ~ "Alpecin-Fenix",
                               team == "B&B Hotels - KTM" ~ "B&B Hotels p/b KTM",
                               team == "Bardiani-CSF-Faizane" ~ "Bardiani-CSF-Faizane",
                               team == "Bingoal Pauwels Sauces WB" ~ "Bingoal - Wallonie Bruxelles",
                               team == "Burgos-BH" ~ "Burgos-BH",
                               team == "Caja Rural - Seguros RGA" ~ "Caja Rural - Seguros RGA",
                               team == "Drone Hopper -  Androni Giocattoli" ~ "Androni Giocattoli - Sidermec", 
                               team == "EOLO-Kometa" ~ "EOLO-Kometa",
                               team == "Equipo Kern Pharma" ~ "Equipo Kern Pharma", 
                               team == "Euskaltel - Euskadi" ~ "Euskaltel - Euskadi",
                               team == "Gazprom - RusVelo" ~ "Gazprom - RusVelo",
                               team == "Human Powered Health" ~ "Rally Cycling",
                               team == "Sport Vlaanderen - Baloise" ~ "Sport Vlaanderen - Baloise",
                               team == "Team Arkea Samsic" ~ "Team Arkea Samsic",
                               team == "Team Novo Nordisk" ~ "Team Novo Nordisk",
                               team == "Team TotalEnergies" ~ "Team TotalEnergies",
                               team == "Uno-X Pro Cycling Team" ~ "Uno-X Pro Cycling Team",
                               TRUE ~ "")) %>%
  left_join(
    dbGetQuery(con, "SELECT DISTINCT * FROM team_rosters_season WHERE Season = 2021") %>%
      unique() %>%
      left_join(riders_to_scrape %>% filter(year == 2021) %>% select(rider_url, PCS_PTS) %>% unique(), by = c("rider_url")) %>%
      mutate(PCS_PTS = ifelse(is.na(PCS_PTS), 0, PCS_PTS)) %>%
      group_by(team = str_trim(team)) %>%
      summarize(team2021 = sum(PCS_PTS)) %>%
      ungroup(), by = c("team_join" = "team")) %>%
  
  mutate(diff = sum(x2022)/sum(team2021),
         x2022 = x2022/diff) %>%
  mutate(delta = x2022/ x2021) %>%
  
  mutate(delta_2022_vs_actual = x2022 / team2021,
         delta_2021_vs_actual = x2021 / team2021) -> yearly_projs

#

yearly_projs %>% 
  filter(team != "Team Novo Nordisk") %>% 
  ggplot(aes(x = delta_2021_vs_actual-1, y = delta-1, label = team, color = delta_2022_vs_actual-1))+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_text()+
  labs(x = "Delta due to transfers", y = "Delta due to projections/aging",
       title = "PCS Points Projections for 2022")+
  scale_color_gradientn(colors = c("#b2182b", "#ef8a62", "#FFB990", "#8ACBEF", "#67a9cf", "#2166ac"), 
                        values = c(0,0.1,0.25,0.5,0.65,1),
                        breaks = c(-0.2,0,0.2,0.6),
                        labels = scales::percent,
                        name = "Overall Delta")+
  theme(panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 15, face = "bold"),
        legend.position = "bottom")
