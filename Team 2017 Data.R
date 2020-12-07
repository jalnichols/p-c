
library(tidyverse)
library(rvest)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

teams_2017 <- 'https://www.procyclingstats.com/teams.php?s=worldtour&year=2017' %>% read_html() %>% html_nodes('div.w50') %>% html_nodes('a') %>% html_attr(name = "href") %>% enframe(name = NULL) %>% rename(team_url = value)

tm_list <- vector("list", length(teams_2017$team_url))

for(t in 1:length(teams_2017$team_url)) {
  
  
  page <- paste0("https://www.procyclingstats.com/", teams_2017$team_url[[t]]) %>%
    read_html()
  
  tm <- page %>%
    html_nodes('h1') %>%
    html_text() %>% 
    str_replace('»2017', '')
  
  df <- page %>%
    html_nodes('ul.riderlist') %>%
    html_nodes('li')
  
  r1 <- df %>% html_nodes('a') %>% html_attr(name = "href")
  r2 <- df %>% html_nodes('a') %>% html_text()
  
  riders_team <- cbind(r1,r2) %>%
    as_tibble() %>%
    mutate(team_url = teams_2017$team_url[[t]],
           team = tm)
  
  tm_list[[t]] <- riders_team
  
}

#

riders_2017 <- bind_rows(tm_list) %>%
  
  mutate(rider = iconv(r2, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
  separate(team, c("team", "level"), sep = '  \\(') %>%
  mutate(level = str_replace(level, "\\)", "")) %>%
  mutate(team = stringi::stri_trans_general(str = team, id = "Latin-ASCII")) %>%
  
  select(team, level, rider, rider_url = r1) %>%
  
  mutate(rider = str_to_title(tolower(rider)),
         rider = ifelse(rider == "O'connor Ben", "O'Connor Ben", rider),
         rider = ifelse(rider == "D'urbano Marco", "D'Urbano Marco", rider))

#

kmeans_model <- read_rds('Stored models/rider-clusters-kmeans-model.rds')

centers <- kmeans_model$centers %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(CL = rowname) %>%
  gather(stat, center, -CL)

#

prc <- dbGetQuery(con, "SELECT * FROM performance_rider_clustering WHERE Date = '2017-01-17'")

dist <- prc %>%
  select(-in_final_group, -rel_sof) %>%
  
  # filter out riders without enough races (max will normally be around 75 so 15+ races needed)
  group_by(Date) %>%
  filter(races > (max(races)/7)) %>%
  ungroup() %>%
  
  gather(stat, value, -rider, -Date, -races) %>%
  
  # scale each stat
  group_by(stat) %>%
  mutate(value = (value - mean(value, na.rm = T)) / sd(value, na.rm = T)) %>%
  ungroup() %>%
  
  # join with centers
  inner_join(centers, by = c("stat")) %>%
  
  # take sum squared euclidean distance
  group_by(rider, Date, races, CL) %>%
  summarize(SED = sum((value - center)^2, na.rm = T)) %>%
  ungroup() %>%
  
  # filter out smallest distance
  group_by(rider, Date, races) %>%
  filter(SED == min(SED, na.rm = T)) %>%
  ungroup() %>%
  
  select(-SED) %>%
  
  # match with correct cluster labels
  inner_join(dbReadTable(con, "kmeans_rider_clusters") %>%
               mutate(CL = as.character(CL)), by = c("CL"))

#
#
#
#
#

team_clusters_2017 <- riders_2017 %>%
  
  mutate(match_rider = tolower(rider)) %>%
  
  left_join(dist %>%
              mutate(rider = tolower(rider)) %>%
              select(-Date), by = c("match_rider" = "rider")) %>%
  
  left_join(prc %>%
              mutate(rider = tolower(rider)) %>%
              select(-Date, -races), by = c("match_rider" = "rider")) %>%
  
  select(-match_rider)

#
#
#
#
#

ggplot(team_clusters_2017 %>% 
         filter(str_trim(team) == 'BORA - hansgrohe') %>%
         mutate(type = ifelse(is.na(type), "Domestique", type)),
       
       aes(x = leader, y = weighted_pcd, label = rider, fill = type, size = exp(points)-0.13))+
  
  # scale sizes correctly
  geom_blank(data = team_clusters_2021 %>%
               filter(points == min(points, na.rm = T) | points == max(points, na.rm = T)),
             aes(x = leader, y = weighted_pcd, label = rider, fill = type, size = exp(points)-0.13))+

  geom_point(shape = 21, stroke = 0.75, color = "black")+
  
  ggrepel::geom_label_repel(size=3, color = "black", fill = "white")+
  
  scale_y_continuous(breaks = seq(0,20,4))+
  scale_x_continuous(labels = scales::percent)+
  theme(plot.title = element_text(face = "bold", size = 18), 
        axis.text = element_text(size = 15))+
  
  labs(x = "Leader: % of races as #1 on team", 
       y = "Parcours fit: climbing difficulty of better performances", 
       title = "Bora 2017 team plot", 
       size = "Success points",
       subtitle = "how often is rider the team leader / which parcours fit a rider")+
  expand_limits(y = c(0,16), x = c(0,0.6))+
  
  scale_fill_manual(values = c("#F02108", "gray40", "#F0A608",
                               "#37B36C", "#16BEF2", "#162CF2"), name = "Rider Type")+
  
  scale_color_manual(values = c("#F02108", "gray40", "#F0A608",
                                "#37B36C", "#16BEF2", "#162CF2"), name = "Rider Type")+
  
  scale_size_continuous(range = c(1,15))+
  guides(size = FALSE)

#
#
#
#
#

success_model <- dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, bunchsprint_impact
                                  FROM lme4_rider_success WHERE DATE = '2017-01-17'") %>%
  
  mutate(random_intercept = random_intercept - 5.2) %>%
  
  mutate(flat_BS = (random_intercept + bunchsprint_impact + (2 * pcd_impact)),
         flats = (random_intercept + (2 * pcd_impact)),
         hills = (random_intercept + (5 * pcd_impact)),
         climbs = (random_intercept + (10 * pcd_impact)),
         high_mtns = (random_intercept + (18 * pcd_impact))
  ) %>%
  
  mutate(flat_BS = exp(flat_BS) / (1+exp(flat_BS)),
         flats = exp(flats) / (1+exp(flats)),
         hills = exp(hills) / (1+exp(hills)),
         climbs = exp(climbs) / (1+exp(climbs)),
         high_mtns = exp(high_mtns) / (1+exp(high_mtns))) %>%
  
  select(rider, flat_BS:high_mtns)

#
#
#

teams_success_2017 <- riders_2017 %>%
  
  mutate(match_rider = tolower(rider)) %>%
  
  left_join(success_model %>%
              mutate(rider = tolower(rider)), by = c("match_rider" = "rider")) %>%
  
  select(-match_rider)

#
#
#
#
#

leader_model <- dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, bunchsprint_impact
                                  FROM lme4_rider_teamleader WHERE DATE = '2017-01-17'") %>%
  
  mutate(random_intercept = random_intercept - 2) %>%
  
  mutate(flat_BS = (random_intercept + bunchsprint_impact + (2 * pcd_impact)),
         flats = (random_intercept + (2 * pcd_impact)),
         hills = (random_intercept + (5 * pcd_impact)),
         climbs = (random_intercept + (10 * pcd_impact)),
         high_mtns = (random_intercept + (18 * pcd_impact))
  ) %>%
  
  mutate(flat_BS = exp(flat_BS) / (1+exp(flat_BS)),
         flats = exp(flats) / (1+exp(flats)),
         hills = exp(hills) / (1+exp(hills)),
         climbs = exp(climbs) / (1+exp(climbs)),
         high_mtns = exp(high_mtns) / (1+exp(high_mtns))) %>%
  
  select(rider, flat_BS:high_mtns)

#
#
#

teams_leader_2017 <- riders_2017 %>%
  
  mutate(match_rider = tolower(rider)) %>%
  
  left_join(leader_model %>%
              mutate(rider = tolower(rider)), by = c("match_rider" = "rider")) %>%
  
  select(-match_rider)

#
#
#
#

all_race_leader_model <- dbGetQuery(con, "SELECT rider, race, stage, year, class, date, team, bunch_sprint, pred_climb_difficulty,
                       rnk, tm_pos
                       FROM stage_data_perf
                       WHERE YEAR(Date) = '2017' AND Class NOT IN ('WC', 'CC', 'NC') AND time_trial = 0") %>%
  
  left_join(dbGetQuery(con, "SELECT rider, date, random_intercept, pcd_impact, bunchsprint_impact
                                  FROM lme4_rider_teamleader WHERE YEAR(DATE) = '2017'"), by = c("rider", "date")) %>%
  
  mutate(random_intercept = random_intercept - 2) %>%
  
  mutate(coef = random_intercept + (pcd_impact * pred_climb_difficulty) + (bunchsprint_impact * bunch_sprint),
         leader_prob = exp(coef)/(1+exp(coef)),
         leader_prob = ifelse(is.na(leader_prob), median(leader_prob, na.rm = T), leader_prob)) %>%
  
  group_by(date) %>%
  filter(mean(is.na(coef)) < 0.33) %>%
  ungroup() %>%
  
  group_by(stage, race, year, team, date, class) %>%
  mutate(adj_leader_prob = leader_prob / sum(leader_prob, na.rm = T)) %>%
  ungroup()

# calculate variance in team leader predictions vs actuals
# calculate variance by pred_climb_difficulty buckets

tm_ldr_mod <- glm(tm_ldr ~ leader_prob, 
                  data = all_race_leader_model %>% mutate(tm_ldr = ifelse(tm_pos==1,1,0)),
                  family = "binomial")

#

team_errors <- all_race_leader_model %>%
  
  group_by(team, race, stage, year, class, date) %>%
  summarize(error = mean((leader_prob - (tm_pos == 1))^2), riders = n(), max_prob = max(leader_prob, na.rm = T)) %>%
  ungroup()

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


rider_averages <- dbGetQuery(con, "SELECT * FROM performance_rider_clustering WHERE Date = '2017-01-17'") %>%
  
  gather(stat, value, rel_sof:in_final_group)

#

rider_stat_averages_2017 <- riders_2017 %>%
  
  mutate(match_rider = tolower(rider)) %>%
  
  left_join(rider_averages %>%
              mutate(rider = tolower(rider)), by = c("match_rider" = "rider")) %>%
  
  select(-match_rider)

