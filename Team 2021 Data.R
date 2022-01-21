
library(tidyverse)
library(rvest)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

teams_2021 <- 'https://www.procyclingstats.com/teams.php?year=2021&filter=Filter&s=worldtour' %>% 
  read_html() %>% 
  html_nodes('div.mt20') %>% 
  html_nodes('a') %>% 
  html_attr(name = "href") %>%
  enframe(name = NULL) %>% 
  rename(team_url = value)

tm_list <- vector("list", length(teams_2021$team_url))

for(t in 1:length(teams_2021$team_url)) {
  
  page <- paste0("https://www.procyclingstats.com/", teams_2021$team_url[[t]]) %>%
    read_html()
  
  tm <- page %>%
    html_nodes('h1') %>%
    html_text() %>% 
    str_replace('»2021', '')
  
  df <- page %>%
    html_nodes('ul.list') %>%
    html_nodes('li')
  
  r1 <- df %>% html_nodes('a') %>% html_attr(name = "href")
  r2 <- df %>% html_nodes('a') %>% html_text()
  
  riders_team <- cbind(r1,r2) %>%
    as_tibble() %>%
    mutate(team_url = teams_2021$team_url[[t]],
           team = tm) %>%
    filter(str_detect(r1, "rider/")) %>%
    unique()
  
  tm_list[[t]] <- riders_team
  
}

#

riders_2021 <- bind_rows(tm_list) %>%
  
  mutate(rider = iconv(r2, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
  separate(team, c("team", "level"), sep = '\\(') %>%
  mutate(level = str_replace(level, "\\)", "")) %>%
  mutate(team = stringi::stri_trans_general(str = team, id = "Latin-ASCII")) %>%
  
  select(team, level, rider, rider_url = r1) %>%
  
  mutate(rider = str_to_title(tolower(rider)))

#

riders_allseasons <- rbind(riders_2021 %>% mutate(Season = 2021),
                           riders_2022 %>% mutate(Season = 2022))

#

kmeans_model <- read_rds('Stored models/rider-clusters-kmeans-model.rds')

centers <- kmeans_model$centers %>%
  as_tibble() %>%
  rownames_to_column() %>%
  rename(CL = rowname) %>%
  gather(stat, center, -CL)

#

prc <- dbGetQuery(con, "SELECT * FROM performance_rider_clustering WHERE Date = '2021-10-11'")

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

team_clusters <- riders_allseasons %>%
  
  mutate(match_rider = tolower(rider)) %>%
  
  left_join(dist %>%
              mutate(rider = tolower(rider)) %>%
              select(-Date), by = c("match_rider" = "rider")) %>%
  
  left_join(prc %>%
              mutate(rider = tolower(rider)) %>%
              select(-Date, -races), by = c("match_rider" = "rider")) %>%
  
  select(-match_rider) %>%
  
  mutate(team = str_trim(team)) %>%
  
  mutate(prior_team = ifelse(Season == 2021, "",
                             case_when(team == "AG2R Citroen Team" ~ "AG2R Citroen Team",
                                       team == "Astana Qazaqstan Team" ~ "Astana - Premier Tech",
                                       team == "Bahrain - Victorious" ~ "Bahrain - Victorious",
                                       team == "BORA - hansgrohe" ~ "BORA - hansgrohe",
                                       team == "Cofidis" ~ "Cofidis", 
                                       team == "EF Education - Nippo" ~ "EF Education - Nippo",
                                       team == "Groupama - FDJ" ~ "Groupama - FDJ",
                                       team == "INEOS Grenadiers" ~ "INEOS Grenadiers",
                                       team == "Intermarche - Wanty - Gobert Materiaux" ~ "Intermarche - Wanty - Gobert Materiaux", 
                                       team == "Israel Start-Up Nation" ~ "Israel Start-Up Nation",
                                       team == "Lotto Soudal" ~ "Lotto Soudal",
                                       team == "Movistar Team" ~ "Movistar Team",
                                       team == "Quick Step-Alpha Vinyl" ~ "Deceuninck - Quick Step", 
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
                                       TRUE ~ ""))) %>%
  
  filter(!(Season == 2021 & ((rider == "Hirschi Marc" & team == "Team DSM") |
                               (rider == "Van Wilder Ilan" & team == "Deceuninck - Quick Step")))) %>%
  
  filter(!(Season == 2021 & team == "Team Qhubeka ASSOS")) %>%
  
  #left_join(
  #  
  #  dbReadTable(con, "pcs_pts_riders_season") %>%
  #     mutate(year = year+1) %>%
  #     select(rider, year, pcs_pts_per, pcs_pts), by = c("rider", "Season" = "year")) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT * FROM lme4_rider_logranks
               WHERE Date = '2021-10-24'"), by = c("rider")
    
  ) %>%
  
  mutate(pcd_impact = ifelse(pcd_impact > 0.15, 0.15, pcd_impact)) %>%
  
  mutate(rider_match = str_to_title(rider)) %>%
  
  left_join(
    
    dbGetQuery(con, "SELECT rider, date as dob FROM rider_attributes") %>%
      filter(!is.na(dob)) %>%
      mutate(rider = str_to_title(rider)), by = c("rider_match" = "rider")) %>%
  
  mutate(age = as.numeric(as.Date(paste0(Season,"-06-30"))-as.Date(dob))/365.25) %>%
  
  select(-rider_match, -dob) %>%
  
  mutate(actual_rider = rider,
         rider = "Hansen Adam",
         actual_age = age) %>%
  mutate(age = age-1) %>%
  mutate(predicted_aging = mgcv::predict.gam(read_rds("Stored models/aging_curve_gam.rds"), .)) %>%
  mutate(age = age+1) %>%
  mutate(predicted_aging2 = mgcv::predict.gam(read_rds("Stored models/aging_curve_gam.rds"), .)) %>%
  mutate(aging_delta = (predicted_aging2+100)/(predicted_aging+100)) %>%
  select(-age, -rider) %>%
  rename(age = actual_age,
         rider = actual_rider) %>%
  
  unique()

#
#
#

team_clusters %>% 
  unique() %>%
  filter(Season == 2022) %>%
  select(-CL) %>%
  left_join(
    team_clusters %>% 
      unique() %>%
      filter(Season == 2021) %>%
      select(rider, team2021 = team), by = c("rider")
  ) %>%
  mutate(type = ifelse(is.na(type), "Domestique", type),
         NewToTeam = ifelse(is.na(team2021), "New to Team",
                            ifelse(team2021 == prior_team, "From Last Year", "New to Team"))) %>% 
  
  group_by(team, NewToTeam, level) %>% 
  summarize(riders=  n(), 
            pcs_pts_per = mean(pcs_pts_per, na.rm = T),
            Pts = sum((exp(points)-0.135), na.rm = T), 
            PCD = mean(weighted_pcd, na.rm = T),
            LDR = mean(leader, na.rm = T)) %>%
  ungroup() -> team_stats

#
#
#
#
#

ggplot(team_clusters %>% 
         unique() %>%
         filter(str_trim(team) == 'Team DSM' & Season == 2022) %>%
         select(-CL) %>%
         left_join(
           team_clusters %>% 
             unique() %>%
             filter(Season == 2021) %>%
             select(rider, team2021 = team), by = c("rider")
         ) %>%
         mutate(type = ifelse(is.na(type), "Domestique", type),
                NewToTeam = ifelse(is.na(team2021), "New to Team",
                                   ifelse(team2021 == prior_team, "From Last Year", "New to Team"))) %>%
         rbind(
           
           team_clusters %>% 
             unique() %>%
             filter(str_trim(team) == 'Team DSM' & Season == 2021) %>%
             select(-CL) %>%
             left_join(
               team_clusters %>% 
                 unique() %>%
                 filter(Season == 2022) %>%
                 select(rider, team2021 = team), by = c("rider")
             ) %>%
             mutate(type = ifelse(is.na(type), "Domestique", type),
                    NewToTeam = ifelse(is.na(team2021), "Left Team",
                                       ifelse(team2021 == team, "From Last Year", "Left Team"))) %>%
             filter(NewToTeam == "Left Team")
           
         ),
       
       aes(x = leader, y = weighted_pcd, label = rider, 
           color = type, #size = (exp(points)-0.13)^2,
           size = 1^1.25))+
  
  # scale sizes correctly
  geom_blank(data = team_clusters %>%
               filter(rider %in% c('Van Aert Wout', 'Carretero Hector')))+

  geom_point(stroke = 0.75)+
  
  ggrepel::geom_label_repel(size=3, color = "black", fill = "white")+

  scale_y_continuous(breaks = seq(0,20,4))+
  scale_x_continuous(labels = scales::percent)+
  theme(plot.title = element_text(face = "bold", size = 18), 
        axis.text = element_text(size = 15))+
  
  labs(x = "Leader: % of races as #1 on team", 
       y = "Parcours fit: climbing difficulty of better performances", 
       size = "Success points",
       subtitle = "how often is rider the team leader / which parcours fit a rider\nSize of circle indicates PCS Points")+
  expand_limits(y = c(0,16), x = c(0,0.6))+
  
  scale_fill_manual(values = c("#F02108", "gray40", "#F0A608",
                                "#37B36C", "#16BEF2", "#162CF2"), name = "Rider Type")+
  
  scale_color_manual(values = c("#F02108", "gray40", "#F0A608",
                               "#37B36C", "#16BEF2", "#162CF2"), name = "Rider Type")+
  
  scale_size_continuous(range = c(2,15))+
  guides(size = FALSE)+
  
  facet_wrap(~NewToTeam)

#
#
#

all_teams <- team_clusters %>% filter(Season == 2022) %>% select(team) %>% unique() %>% .[[1]]

for(x in 1:length(all_teams)) {

  TN <- all_teams[[x]]
  
  data_for <- team_clusters %>% 
    unique() %>%
    filter(str_trim(team) == TN & Season == 2022) %>%
    select(-CL) %>%
    left_join(
      team_clusters %>% 
        unique() %>%
        filter(Season == 2021) %>%
        select(rider, team2021 = team), by = c("rider")
    ) %>%
    mutate(type = ifelse(is.na(type), "Domestique", type),
           NewToTeam = ifelse(is.na(team2021), "New to Team",
                              ifelse(team2021 == prior_team, "From Last Year", "New to Team"))) %>%
    rbind(
      
      team_clusters %>% 
        unique() %>%
        filter(str_trim(team) == TN & Season == 2021) %>%
        select(-CL) %>%
        left_join(
          team_clusters %>% 
            unique() %>%
            filter(Season == 2022) %>%
            select(rider, team2021 = team), by = c("rider")
        ) %>%
        mutate(type = ifelse(is.na(type), "Domestique", type),
               NewToTeam = ifelse(is.na(team2021), "Left Team",
                                  ifelse(team2021 == team, "From Last Year", "Left Team"))) %>%
        filter(NewToTeam == "Left Team")) %>%
    
    mutate(CL = ((random_intercept+(4.5*pcd_impact)+3.8)),
           BS = ((random_intercept+(1*pcd_impact)+bunchsprint_impact+3.8)),
           MT = ((random_intercept+(10*pcd_impact)+3.8))) %>%
    mutate(hm = 3 / ((1/CL)+(1/BS)+(1/MT))) %>% rowwise() %>% mutate(best = min(CL,BS,MT))
  
  new <- (data_for %>% filter(NewToTeam == "New to Team") %>% nrow()) / data_for %>% nrow()
  
  if(new < 0.33) {
    new=0.5
  } else{
    new=new/0.5
  }
  
  p <- ggplot(data_for %>%
                mutate(ability = hm),

              aes(x = bunchsprint_impact*-1, y = pcd_impact*-10, label = rider, 
                  fill = ability))+
    geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+
    
    annotate("text", x = 1.6, y = -1.25, label = "better\nsprinters", size=8, alpha = 0.5)+
    annotate("text", x = -0.6, y = 0.75, label = "better\nclimbers", size=8, alpha = 0.5)+ 
    annotate("text", x = -0.6, y = -1.25, label = "better\nclassics", size=8, alpha = 0.5)+ 
    geom_point(size=6, shape = 21, stroke=0.75, color="black", alpha=0.75)+
    geom_blank(data = team_clusters %>%
                 filter(rider %in% c('Evenepoel Remco', 'Bennett Sam',
                                     'Rivera Kevin', 'Merlier Tim',
                                     'Van Aert Wout')) %>%
                 mutate(CL = ((random_intercept+(4.5*pcd_impact)+3.8)),
                        BS = ((random_intercept+(1*pcd_impact)+bunchsprint_impact+3.8)),
                        MT = ((random_intercept+(10*pcd_impact)+3.8))) %>% 
                 mutate(hm = 3 / ((1/CL)+(1/BS)+(1/MT))) %>%
                 rowwise() %>% mutate(ability = min(CL,BS,MT),
                                      ability = hm))+
    ggrepel::geom_label_repel(size=3, color = "black", fill = "white", alpha=0.75)+
    
    scale_y_continuous(breaks = seq(-1.5,1,0.5))+
    scale_x_continuous(breaks = seq(-1,2,0.5))+
    scale_fill_gradientn(colors = c("#FF005C",
                                    "#FF0000",
                                    "#FF4500",
                                    "#FF6800",
                                    "#FF9600",
                                    #"#FFC100",
                                    "#FFE000",
                                    #"#FFFE00",
                                    "#FFFFB8",
                                    "#888888",
                                    "#000000"))+
    
    theme(plot.title = element_text(face = "bold", size = 18),
          axis.title = element_text(size = 14),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          strip.text = element_text(size=14, color = "white", face = "bold"),
          strip.background = element_rect(fill = "black"),
          panel.grid.major = element_line(color = "gray80", size = 0.25),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "gray90"),
          legend.position = "bottom",
          legend.text = element_blank())+
    
    labs(x = "Bunch Sprint Ability", 
         y = "Climbing Ability",
         fill = "Overall\nAbility",
         title = paste0(TN, " 2022 team plot"))+
    
    facet_grid(~NewToTeam, space = "free")
  
  #
  
  ggplotGrob(p) -> gp
  facet.columns <- gp$layout$l[grepl("panel-1-2", gp$layout$name)]
  gp$widths[facet.columns] <- gp$widths[facet.columns] * new
  
  ggsave(filename=paste0(TN, "-2022.png"), plot=gp, height = 7.5, width = 11)
 
  print(TN)
   
}
  
#
#
#
#
#

success_model <- dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, bunchsprint_impact
                                  FROM lme4_rider_success WHERE DATE = '2020-12-04'") %>%
  
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

teams_success_2021 <- riders_2021 %>%
  
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
                                  FROM lme4_rider_teamleader WHERE DATE = '2020-12-04'") %>%
  
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

teams_leader_2021 <- riders_2021 %>%
  
  mutate(match_rider = tolower(rider)) %>%
  
  left_join(leader_model %>%
              mutate(rider = tolower(rider)), by = c("match_rider" = "rider")) %>%
  
  select(-match_rider)

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

success_model_all <- dbGetQuery(con, "SELECT rider, random_intercept, pcd_impact, bunchsprint_impact, Date
                                  FROM lme4_rider_success") %>%
  
  mutate(random_intercept = random_intercept - 5) %>%
  
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
  
  select(rider, Date, flat_BS:high_mtns) %>%
  
  gather(parcours, success, flat_BS:high_mtns) %>%
  
  group_by(Date, parcours) %>%
  mutate(rank = rank(-success, ties.method = "min")) %>%
  ungroup()

#
#
#

teams_success_all_2021 <- riders_2021 %>%
  
  mutate(match_rider = tolower(rider)) %>%
  
  left_join(success_model_all %>%
              mutate(rider = tolower(rider)), by = c("match_rider" = "rider")) %>%
  
  select(-match_rider)

#

ggplot(teams_success_all_2021 %>%
         filter(rider %in% c("Froome Chris", "Thomas Geraint", "Roglic Primoz",
                             "Pogacar Tadej", "Pinot Thibaut", "Bernal Egan")),
       aes(x = as.Date(Date), y = climbs, color = rider))+
  
  geom_line(size=1)+
  
  scale_y_continuous(labels = scales::percent)+
  
  labs(x = "",
       y = "Bunch Sprint Success Model",
       title = "Bunch Sprint Expected Performance")

#
#
#

rider_averages <- dbGetQuery(con, "SELECT * FROM performance_rider_clustering WHERE Date = '2020-12-04'") %>%
  
  gather(stat, value, rel_sof:in_final_group)

#

rider_stat_averages_2021 <- riders_2021 %>%
  
  mutate(match_rider = tolower(rider)) %>%
  
  left_join(rider_averages %>%
              mutate(rider = tolower(rider)), by = c("match_rider" = "rider")) %>%
  
  select(-match_rider)

