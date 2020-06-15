
library(tidyverse)
library(RMySQL)

con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='cycling',
                 user='jalnichols',
                 password='braves')

#

race_data <- dbReadTable(con, "pcs_all_races") %>%
  mutate(Race = iconv(Race, from="UTF-8", to = "ASCII//TRANSLIT"),
         Circuit = str_sub(type, 53, nchar(type))) %>%
  separate(Circuit, c("Circuit", "delete"), sep = "&") %>%
  mutate(Circuit = str_replace(Circuit, "circuit=", "")) %>%
  
  inner_join(
    
    tibble(Tour = c("Missing", "Americas Tour", "Europe Tour", "World Championships", "World Tour", "Asia Tour",
                       "Oceania Tour", "Africa Tour", "Olympic Games"),
           Circuit = c("", "18", "13", "2", "1", "12", "14", "11", "3")), by = c("Circuit")
    
  ) %>%
  
  mutate(Tour = ifelse(str_detect(Race, "National Champ"), "National Championship", Tour)) %>%
  
  select(Race, Tour, URL = url, Year = year)

#

stage_data <- dbReadTable(con, "pcs_stage_data") %>%
  unique() %>%
  mutate(rider = ifelse(rider == "Gaudu  David ", "Gaudu David", rider),
         team = ifelse(str_detect(team, "Cofidis"), "Cofidis", team)) %>%
  
  mutate(team = iconv(team, from="UTF-8", to = "ASCII//TRANSLIT")) %>%
  
  left_join(
    
    read_csv("master-teams.csv"), by = c("team", "year")
    
  ) %>%
  
  filter(!(race %in% c("la poly normande"))) %>%
  
  left_join(
    
    race_data %>%
      mutate(Race = tolower(Race)), by = c("race" = "Race", "year" = "Year", "url" = "URL")
    
  ) %>%
  
  left_join(
    
    read_csv("cleaned-stage-type-pcs.csv"), by = c("stage", "race", "year")
    
  ) %>%
  
  mutate(stage_type = ifelse(is.na(new_st), stage_type, paste0("icon profile p", new_st))) %>%
  
  left_join(
    
    tibble(
      
      race = c("tour of china ii", "tour of china i", "the larry h.miller tour of utah",
               "tour of taiwan", "tour of indonesia", "tour of china i", "tour of qinghai lake",
               "trofeo serra de tramuntana, deia-lluc", "trofeo serra de tramuntana", "trofeo serra de tramuntana",
               "tour of qinghai lake", "tour of qinghai lake", "tour of qinghai lake", "tour of qinghai lake",
               "tour of utah", "tour of china i", "vuelta a castilla y leon", "int. osterreich-rundfahrt-tour of austria", "int. osterreich-rundfahrt-tour of austria", "tour of qinghai lake", "tour of qinghai lake",
               "tour of qinghai lake", "tour of china ii", "tour of china i", "tour de wallonie",
               "tour of iran (azarbaijan)", "tour of iran (azarbaijan)", "tour of iran (azarbaijan)",
               "tour of japan", "tour du gevaudan languedoc-roussillon", "vuelta a burgos",
               "tour of qinghai lake", "tour of qinghai lake", "tour of qinghai lake",
               "tour of qinghai lake", "usa pro challenge","amgen tour of california",
               "the larry h.miller tour of utah", "tour of china i", "tour de korea", "tour de korea",
               "tour de korea"),
      
      stage = c(4,6,5,3,3,3,4,1,1,1,3,4,6,7,2,3,2,3,8,3,6,7,4,5,2,2,2,3,4,1,4,3,6,8,10,
                7,7,5,6,3,4,6),
      
      year = c(2015,2014,2014,2020,2018,2016,2016,2013,2014,2015,2016,2016,2016,2016,2016,2016,
               2015,2015,2015,2015,2015,2015,2015,2015,2015,2016,2017,2017,2017,2014,2014,
               2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014),
      
      NEW = c("p2", "p2","p2","p2","p2","p2","p2","p4","p4","p4","p2","p2","p2","p2","p2","p2",
              "p2","p2","p2","p2","p2","p2","p2","p3","p2","p2","p2","p2","p2","p2","p2",
              "p2","p2","p2","p2","p2","p2","p2","p2","p2","p2","p3")
      
    ), by = c("year", "stage", "race")
    
  ) %>%
  
  mutate(stage_type = ifelse(is.na(NEW), stage_type, paste0("icon profile ", NEW))) %>%
  
  mutate(uphill_finish = ifelse(stage_type %in% c('icon profile p5', 'icon profile p3'), TRUE,
                                ifelse(summit_finish == 1 & missing_profile_data == 0, TRUE,
                                       ifelse(missing_profile_data == 0 & final_1km_gradient > 0.04, TRUE, FALSE))),
         uphill_finish = ifelse(missing_profile_data == 1 & is.na(final_1km_gradient) & stage_type == 'icon profile p0',
                                NA, uphill_finish))

#
#
#

missing_stage_types <- stage_data %>%
  
  filter(str_detect(parcours_value, "\\*") | stage_type == "icon profile p0") %>%
  filter(missing_profile_data == 1 & rnk == 1) %>%
  select(stage, race, year, class, parcours_value, stage_type) %>%
  
  # these are corrected later on
  filter(!race %in% 
           
           c("grand prix cycliste de quebec", 
             "grand prix cycliste de montreal", 
             "il lombardia",
             "la fleche wallonne",
             "clasica ciclista san sebastian",
             "amstel gold race",
             "liege - bastogne - liege",
             "milano-sanremo",
             "tre valli varesine",
             'strade bianche', 
             "giro dell'emilia",
             'chrono des nations',
             'japan cup cycle road race', 
             "le samyn",
             'trofeo laigueglia',
             "milano-torino")) %>%
  # remove the ones I manually adjust thru spreadsheet
  anti_join(
    
    read_csv("pred-climb-difficulty-replacements.csv"), by = c("stage", "race", "year"))

# Strength of Peloton -----------------------------------------------------

#
#
#
#
# Determine strength of peloton for each stage

strength_of_peloton <- stage_data %>%
  
  # most non-European UWT races are not UWT quality
  # demote down a level or two
  mutate(class = ifelse(race %in% c("gree-tour of guangxi", "presidential cycling tour of turkey",
                                    "tour of beijing", "gree - tour of guangxi"), "2.1",
                        ifelse(race %in% c("amgen tour of california", "tour de pologne",
                                           "cadel evans great road race", "santos tour down under"), "2.HC", class)),
         class = ifelse(race %in% c("tour of qinghai lake", "le tour de langkawi",
                                    "tour of hainan", "usa pro challenge", 
                                    "the larry h. miller tour of utah",
                                    "colorado classic"), "2.1", class)) %>%
  
  mutate(rnk = ifelse(is.na(rnk),200,rnk)) %>%
  
  # determine whether bunch sprint #1 same time as #25
  group_by(stage, race, year, url) %>%
  mutate(x25th = ifelse(rnk == 25, total_seconds <= win_seconds+5, NA),
         x25th = mean(x25th, na.rm = T)) %>%
  ungroup() %>%
  
  rename(bunch_sprint = x25th) %>%
  
  # set threshold for qualifying by class
  mutate(qual = str_sub(class, 3, nchar(class)),
         qual = ifelse(qual == "UWT", 9,
                       ifelse(qual == "HC", 7.5,
                              ifelse(qual == "Pro", 7.5,
                                     ifelse(qual == "1", 5, 3)))),
         qual = ifelse(grand_tour == 1, 12, qual),
         qual = ifelse(str_detect(race, "world champ"), 12, qual),
         
         # if it's a bunch sprint, qualifying is a third what it normally is
         # because sprinters would dominate this otherwise
         qual = ifelse(is.na(bunch_sprint), qual / 1.5,
                       ifelse(bunch_sprint == 1, qual / 3, qual))) %>%
  
  select(stage, race, year, rider, rnk, qual, class, tour = Tour, master_team, url) %>%
  
  unique() %>%
  
  # gives pts based on finish ONLY for those w/i threshold
  mutate(pts = 1 / (rnk + 3),
         pts = ifelse(rnk < (qual+1), pts, 0)) %>%
  
  # adjusts for tour strength
  mutate(pts = ifelse(tour %in% c("Oceania Tour", "Asia Tour"), pts * 0.5,
                      ifelse(tour %in% c("National Championships"), pts * 0.35,
                             ifelse(tour %in% c("Africa Tour"), pts * 0.2, pts)))) %>%
  
  mutate(year2 = ifelse(year == 2020, 2019, year)) %>%
  
  # average points per rider regressing to zero with 10 races
  group_by(rider, year2) %>%
  mutate(t5 = sum(pts*4, na.rm = T)/(n()+10),
         n=n()+10) %>%
  ungroup() 

#

by_tour <- strength_of_peloton %>%
  
  filter(!tour %in% c("Missing")) %>%
  
  group_by(rider, class) %>%
  summarize(pts = mean(pts, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  
  mutate(pts = ifelse(pts == 0, 0.001, pts)) %>%
  
  inner_join(
    
    strength_of_peloton %>%
      
      filter(!tour %in% c("Missing")) %>%
      
      group_by(rider, class) %>%
      summarize(pts = mean(pts, na.rm = T),
                n = n()) %>%
      ungroup() %>%
      mutate(pts = ifelse(pts == 0, 0.001, pts)), by = c("rider")
    
  ) %>%
  
  filter(class.x != class.y) %>%
  
  mutate(ratio = pts.x / pts.y,
         hm = (2) / ((1 / n.x) + (1 / n.y))) %>%
  
  group_by(class.x, class.y) %>%
  summarize(straight_avg = mean(ratio, na.rm = T),
            harmonic_mean = sum(hm * ratio, na.rm = T) / sum(hm, na.rm = T),
            n = sum(hm, na.rm = T)) %>%
  ungroup() %>% 
  
  mutate(blended = (straight_avg + harmonic_mean)/2) %>% 
  
  filter(!class.x %in% c("Olympic Games")) %>%
  filter(!class.y %in% c("Olympic Games")) %>% 
  
  group_by(class.y) %>% 
  mutate(rk = rank(blended, ties.method = 'min')) %>%
  ungroup()

# above analysis suggests WorldTour/World Champs level 1 | Europe HC/PRO/.1 + top Americas | Asia/Oceania | Natl Champs | Africa
# before downgrading the pts for the last four tours in the strength_of_peloton code

#

best_riders_sop <- strength_of_peloton %>%
  select(rider, year2, t5) %>% 
  unique() %>%
  
  group_by(year2) %>% 
  mutate(rk = rank(-t5, ties.method = "min")) %>% 
  ungroup()

#

best_possible_sop <- best_riders_sop %>%
  filter(rk < 151) %>% 
  group_by(year2) %>%
  summarize(top150 = sum(t5)) %>%
  ungroup()

#

individual_races_sop <- strength_of_peloton %>%
  inner_join(best_possible_sop, by = c("year2")) %>%
  
  # remove current race from calc
  mutate(t5 = ((t5*n) - (pts*2))/(n-1)) %>%
  
  # sum up total points rating for all of race
  group_by(race, stage, year, url) %>%
  summarize(total = sum(t5, na.rm = T),
            top150 = mean(top150, na.rm = T)) %>%
  ungroup() %>%

  mutate(elite_riders_worth = total / top150) %>%
  
  group_by(year) %>%
  mutate(sop = elite_riders_worth / max(elite_riders_worth, na.rm = T)) %>%
  ungroup() %>%
  
  select(race, stage, year, sop, url)

#

individual_races_team_sop <- strength_of_peloton %>%
  inner_join(best_possible_sop, by = c("year2")) %>%
  
  # remove current race from calc
  mutate(t5 = ((t5*n) - (pts*2))/(n-1)) %>%
  
  # sum up total points rating for all of race
  group_by(race, stage, year, master_team, url) %>%
  mutate(total = sum(t5, na.rm = T),
            top150 = mean(top150, na.rm = T)) %>%
  filter(rank(-t5, ties.method = "first") == 1) %>%
  ungroup() %>%
  
  group_by(race, stage, year, url) %>%
  mutate(best = max(total, na.rm = T),
         rk = rank(-total, ties.method = "min")) %>%
  ungroup() %>%
  
  mutate(rel_to_best = total / best)

#
#
#
#
#
#
#
# Extrapolate Climbing Data -----------------------------------------------

#
# this model predicts actual climb difficulty based on the parcours value in the PCS data
# this is good for stages that PCS rates in terms of parcours value, but which F-R doesn't have the climb
# data broken out into categorized climbs
#

pv_mod <- stage_data %>% 
  
  filter(rnk == 1 & time_trial == FALSE) %>% 
  
  mutate(est = ifelse(str_detect(parcours_value, "\\*"),1,0), 
         pv = as.numeric(parcours_value)) %>% 
  
  select(-gain_1st, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th, 
         -gc_seconds, -total_seconds, -win_seconds, -tm_pos, -rel_speed, 
         -top_variance, -variance, -speed) %>%
  
  filter(missing_profile_data == 0) %>% 
  filter((pv > 0 & act_climb_difficulty > 0) | 
           (pv==0 & act_climb_difficulty==0)) %>%
  
  lm(act_climb_difficulty ~ pv, data = .)

#

pv_data <- cbind(
  stage_data %>%
    filter(rnk == 1 & time_trial == FALSE) %>% 
  
  mutate(est = ifelse(str_detect(parcours_value, "\\*"),1,0), 
         pv = as.numeric(parcours_value)) %>% 
  
  select(-gain_1st, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th, 
         -gc_seconds, -total_seconds, -win_seconds, -tm_pos, -rel_speed, 
         -top_variance, -variance, -speed) %>%
  
  filter(est == 0),
  
  pred_pv = predict(
    
    pv_mod, 
    stage_data %>%
      filter(rnk == 1 & time_trial == FALSE) %>%  
      
      mutate(est = ifelse(str_detect(parcours_value, "\\*"),1,0), 
             pv = as.numeric(parcours_value)) %>% 
      
      select(-gain_1st, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th, 
             -gc_seconds, -total_seconds, -win_seconds, -tm_pos, -rel_speed, 
             -top_variance, -variance, -speed) %>%
      
      filter(est == 0)
    
  ))

# act_climb_difficulty = 1 + (0.063 * parcours_value) -- R^2 = 0.81

# for F-R profile data stages we can use stage type + the profile stats (which can be replicated from Strava data for
# stages which F-R doesn't have profile data

no_climbs_mod <- stage_data %>% 
  
  filter(rnk == 1 & time_trial == FALSE) %>% 
  
  mutate(est = ifelse(str_detect(parcours_value, "\\*"),1,0), 
         pv = as.numeric(parcours_value)) %>% 
  
  select(-gain_1st, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th, 
         -gc_seconds, -total_seconds, -win_seconds, -tm_pos, -rel_speed, 
         -top_variance, -variance, -speed) %>%
  
  filter(missing_profile_data == 0 & act_climb_difficulty > 0) %>%
  filter(!stage_type == "icon profile p0") %>%
  mutate(tvg = total_vert_gain / length) %>%
  lm(act_climb_difficulty ~ stage_type + uphill_finish + (tvg) +
       final_20km_vert_gain + time_at_1500m, data = .)

# R^2 = 0.79

no_climbs_data <- cbind(
  stage_data %>%
    filter(rnk == 1 & time_trial == FALSE) %>% 
    
    select(-gain_1st, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th, 
           -gc_seconds, -total_seconds, -win_seconds, -tm_pos, -rel_speed, 
           -top_variance, -variance, -speed) %>%
    
    filter(missing_profile_data == 0 & !stage_type == "icon profile p0"),
  
  pred_no_climbs = predict(
    
    no_climbs_mod, 
    stage_data %>%
      filter(rnk == 1 & time_trial == FALSE) %>% 
      
      select(-gain_1st, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th, 
             -gc_seconds, -total_seconds, -win_seconds, -tm_pos, -rel_speed, 
             -top_variance, -variance, -speed) %>%
      
      filter(missing_profile_data == 0 & !stage_type == "icon profile p0") %>%
      mutate(tvg = total_vert_gain / length)
    
  )) %>%
  mutate(pred_no_climbs = ifelse(pred_no_climbs < 1, 1, pred_no_climbs))

# R^2 = 0.78

# and for stages w/o F-R we can use stage_type to estimate act_climb_difficulty

icon_mod <- stage_data %>% 
  
  filter(rnk == 1 & time_trial == FALSE) %>% 
  
  mutate(est = ifelse(str_detect(parcours_value, "\\*"),1,0), 
         pv = as.numeric(parcours_value)) %>% 
  
  select(-gain_1st, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th, 
         -gc_seconds, -total_seconds, -win_seconds, -tm_pos, -rel_speed, 
         -top_variance, -variance, -speed) %>%
  
  filter(missing_profile_data == 0 & act_climb_difficulty > 0) %>%
  filter(!stage_type == "icon profile p0") %>%
  lm(act_climb_difficulty ~ stage_type, data = .)

# model here is R^2 only 0.62, p1 = 1, p2 = 2, p3 = 3, p4 = 8, p5 = 16

# because of lower R^2 we're only predicting it for estimated parcours values AND missing profile data

icon_data <- cbind(
  stage_data %>%
    filter(rnk == 1 & time_trial == FALSE) %>% 
    
    mutate(est = ifelse(str_detect(parcours_value, "\\*"),1,0), 
           pv = as.numeric(parcours_value)) %>% 
    
    select(-gain_1st, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th, 
           -gc_seconds, -total_seconds, -win_seconds, -tm_pos, -rel_speed, 
           -top_variance, -variance, -speed) %>%
    
    filter(!stage_type == "icon profile p0" & est == 1 & missing_profile_data == 1),
  
  pred_icon = predict(
    
    icon_mod, 
    stage_data %>%
      filter(rnk == 1 & time_trial == FALSE) %>% 
      
      mutate(est = ifelse(str_detect(parcours_value, "\\*"),1,0), 
             pv = as.numeric(parcours_value)) %>% 
      
      select(-gain_1st, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th, 
             -gc_seconds, -total_seconds, -win_seconds, -tm_pos, -rel_speed, 
             -top_variance, -variance, -speed) %>%
      
      filter(!stage_type == "icon profile p0" & est == 1 & missing_profile_data == 1)
    
  ))

#
# now combine all three methods together
#

three_methods <- rbind(
  
  no_climbs_data %>%
    select(race, stage, year, pred = pred_no_climbs) %>%
    mutate(type = "nc"),
  icon_data %>%
    select(race, stage, year, pred = pred_icon) %>%
    mutate(type = "ic"),
  pv_data %>%
    select(race, stage, year, pred = pred_pv) %>%
    mutate(type = "pv")
) %>%
  
  unique() %>%

  group_by(stage, race, year) %>%
  summarize(pred_climb_difficulty = mean(pred, na.rm = T)) %>%
  ungroup() %>%
  
  anti_join(
    
    read_csv("pred-climb-difficulty-replacements.csv"), by = c("stage", "race", "year")
    
  ) %>%
  
  rbind(
    
    read_csv("pred-climb-difficulty-replacements.csv")
    
  )

#

stage_data <- stage_data %>%
  select(-climbing_final_20km, -final_5km_elev, -final_5km_gradient, -final_1km_elev,
         -perc_gain_end, -time_at_1500m, -total_elev_change, -highest_point) %>%
  
  inner_join(
    stage_data %>%
      select(climbing_final_20km, final_5km_elev, final_5km_gradient, final_1km_elev,
             perc_gain_end, time_at_1500m, total_elev_change, highest_point,
             rider, stage, year, race, url) %>%
      group_by(rider, stage, year, race, url) %>%
      nest() %>%
      ungroup(), by = c("rider", "stage", "year", "race", "url")
    
  ) %>%
  
  left_join(
    
    three_methods, by = c("stage", "race", "year")
    
  ) %>%
  
  mutate(act_climb_difficulty = ifelse(act_climb_difficulty == 0, NA, act_climb_difficulty)) %>%
  
  mutate(pred_climb_difficulty = ifelse(is.na(act_climb_difficulty),
                                        ifelse(is.na(pred_climb_difficulty), NA, pred_climb_difficulty), 
                                        ifelse(is.na(pred_climb_difficulty), act_climb_difficulty,
                                               ((2*(act_climb_difficulty)) + (pred_climb_difficulty))/3))) %>%
  
  # some manual adjustments where older stage are being mis-represented
  mutate(pred_climb_difficulty = 
           ifelse(race == "grand prix cycliste de quebec", 2,
                  ifelse(race == "grand prix cycliste de montreal", 2, 
                         ifelse(race == "il lombardia",
                                ifelse(pred_climb_difficulty < 12, 12, pred_climb_difficulty),
                                ifelse(race == "la fleche wallonne",
                                       ifelse(pred_climb_difficulty < 7, 7, pred_climb_difficulty),
                                       ifelse(race == "clasica ciclista san sebastian",
                                              ifelse(pred_climb_difficulty < 6, 6, pred_climb_difficulty),
                                              ifelse(race == "amstel gold race",
                                                     ifelse(pred_climb_difficulty < 4, 4, pred_climb_difficulty),
                                                     ifelse(race == "liege - bastogne - liege",
                                                            ifelse(pred_climb_difficulty < 6, 6, pred_climb_difficulty),
                                                            ifelse(race == "milano-sanremo",
                                                                   ifelse(pred_climb_difficulty < 3, 3, pred_climb_difficulty),
                                                                   ifelse(race == "tre valli varesine", 3.5, 
                                                                          ifelse(race == 'strade bianche', 4.5,
                                                                                 ifelse(race == "giro dell'emilia", 7.5, pred_climb_difficulty))))))))))),
         pred_climb_difficulty = ifelse(race == 'chrono des nations', 1,
                                        ifelse(race == 'japan cup cycle road race', 5, 
                                               ifelse(race == "le samyn", 1.5,
                                                      ifelse(race == 'trofeo laigueglia', 6,
                                                             ifelse(race == "milano-torino", 11.5,
                                                             pred_climb_difficulty)))))) %>%
  
  # 2020 changes
  mutate(pred_climb_difficulty = ifelse(year == 2020,
                                        ifelse(race == "milano-torino", 2, pred_climb_difficulty), pred_climb_difficulty))


#
#
# STRENGTH OF FIELD
#
#

# Set up old SOP calc + Limits calc ---------------------------------------

# 
# riders_stages <- stage_data %>%
# 
#   filter(rnk < 6) %>%
#   
#   select(rider1 = rider, stage, race, year) %>%
#   unique() %>%
#   
#   inner_join(
#     
#     stage_data %>%
# 
#       filter(rnk < 6) %>%   
#       
#       select(rider2 = rider, stage, race, year) %>%
#       unique(), by = c("stage", "race", "year")	
#     
#   ) %>%
#   
#   filter(!(rider1 == rider2)) %>%
#   unique()
# 
# #
# #
# #
# 
# matches <- riders_stages %>%
# 
#   group_by(rider1, rider2) %>%
#   summarize(n = n()) %>%
#   ungroup() %>% 
#   
#   filter(n > 3) %>% 
#   
#   group_by(rider1) %>%
#   mutate(n1 = n()) %>%
#   ungroup() %>%
#   
#   group_by(rider2) %>%
#   mutate(n2 = n()) %>%
#   ungroup() %>%
#   
#   filter(n1 > 3 & n2 > 3) %>%
#   
#   select(rider1, rider2, n) %>%
#   
#   spread(rider2, n) %>%
#   
#   gather(rider2, n, -rider1) %>%
#   
#   mutate(n = ifelse(is.na(n), 0, n)) %>%
#   
#   spread(rider2, n)
# 
# #
# # PCA
# #
# 
# pxxx <- prcomp(matches[, 2:ncol(matches)], scale = TRUE, center = TRUE)
# 
# # two PCs capture 26%
# 
# # PC1 is the sprinter vs mountains classifier
# # PC2 is strength of competition
# # PC3 is like classics-esque
# 
# rxxx <- cbind(
#   matches %>%
#     select(rider1),
#   pxxx$x) %>%
# 
#   select(rider1, PC1, PC2, PC3, PC4, PC6) %>%
#   # the below sets the PC2 of strong to weak riders so that strong is always positive
#   mutate(AV = ifelse(rider1 == "Valverde Alejandro",
#                      ifelse(PC2 > 0, 1, -1), NA),
#          AV = mean(AV, na.rm = T),
#          PC2 = AV * PC2) %>%
#   select(-AV)
# 
# ggplot(rxxx, aes(x = PC1, y = PC2, label = rider1))+
#   geom_hline(yintercept = 0)+
#   geom_vline(xintercept = 0)+
#   geom_label()+
#   scale_fill_viridis_c()+
#   labs(x = "PC1: climber vs sprinter", y = "PC2: strong field vs weak field")
# 
# #
# 
# riders_top10s <- stage_data %>%
#   filter(rnk < 11) %>% 
#   
#   select(rider, stage, race, year, class, time_trial, one_day_race, grand_tour) %>%
#   unique() %>%
#   
#   inner_join(rxxx %>% 
#                select(rider = rider1, PC1, PC2, PC3, PC6)) %>%
#   group_by(stage, race, year, class, time_trial, one_day_race, grand_tour) %>%
#   summarize(type = sum(PC1, na.rm = T),
#             sof = sum(PC2, na.rm = T),
#             tt = sum(PC6, na.rm = T),
#             classics = sum(PC3, na.rm = T),
#             n = n()) %>%
#   ungroup()
# 
# #
# 
# fields <- stage_data %>%
#   
#   select(stage, race, year, rider, class, grand_tour, date, rnk, time_trial) %>%
#   unique() %>%
#   
#   inner_join(rxxx %>% 
#                select(rider = rider1, PC1, PC2)) %>% 
#   group_by(race, stage, year) %>% 
#   filter(rank(rnk, ties.method = "first") < 26) %>%
#   ungroup() %>%
#   
#   group_by(stage, race, year, class, grand_tour, date) %>%
#   summarize(tot = mean(PC2, na.rm = T), 
#             n = n()) %>%
#   ungroup() %>% 
#   
#   separate(class, c("type", "class"), sep = ".") %>%
#   
#   mutate(class = ifelse(is.na(class), "UWT", class), 
#          type = ifelse(type == "WT", NA, type)) %>%
#   
#   mutate(monument = ifelse(str_detect(race, "sanremo") | 
#                              str_detect(race, "tour des flandres") | 
#                              str_detect(race, "roubaix") | 
#                              str_detect(race, "bastogne") | 
#                              str_detect(race, "il lombardia"), TRUE, FALSE)) %>%
#   
#   mutate(tot = ifelse(n < 25, (tot * n) + (-1.5 * (25- n)), tot * 25))

#
#
# take in top 200 UWT riders

top_200_WT <- stage_data %>%
  
  inner_join(
    
    #fields %>% select(stage, race, year, tot),
    individual_races_sop %>%
      select(stage, race, year, tot = sop, url), by = c("stage", "race", "year", "url")
    
  ) %>%
  
  #filter(tot > 25) %>%
  
  group_by(rider) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  
  filter(rank(-count, ties.method = 'min') < 201)

# take in all events

perf_by_level_data <- stage_data %>%
  
  inner_join(
    
    #fields %>% select(stage, race, year, tot) %>% filter(year>2016),
    individual_races_sop %>%
      select(stage, race, year, tot = sop, url) %>%
      filter(year > 2016), by = c("stage", "race", "year", "url")
    
  ) %>%
  
  inner_join(
    
    top_200_WT %>%
      select(rider), by = c("rider")
    
  ) %>%
  
  select(race, year, sof = tot, stage, rnk, rider, gain_1st, url) %>%
  
  unique()

# set up model

model_list <- vector("list", 20)
glmer_list <- vector("list", 20)

for(m in 1:20) {
  
  d <- perf_by_level_data %>%
    
    mutate(top = ifelse(rnk < (m + 1), 1, 0))
  
  glm <- glm(top ~ sof, data = d, family = binomial(link = "logit"))
  
  model_list[[m]] <- cbind(fit = glm$fitted.values, 
                           glm$data %>% filter(!is.na(rnk))) %>%
    mutate(thresh = m)
  
  #glmer <- lme4::glmer(top ~ (1 | race:year), data = d, family = binomial(link = "logit"))
  
  #glmer_list[[m]] <- summary(glmer)$coefficients %>%
  #  as_tibble()
  
}

glm_data <- bind_rows(model_list) %>%
  select(sof, fit, thresh) %>%
  unique()

#

ggplot(glm_data, aes(x = sof, y = fit, color = as.factor(thresh)))+
  geom_vline(xintercept = 0.82)+
  geom_hline(yintercept = 0.1)+
  geom_point(size = 1)+
  geom_label(data = glm_data %>% group_by(thresh) %>% filter(sof == min(sof, na.rm = T)) %>% ungroup(),
             aes(x = sof, y = fit, label = thresh),
             size = 4)+
  #scale_color_manual(values = c("black", "orange"), guide = F)+
  scale_y_continuous(labels = scales::percent)+
  guides(color = F)

# this threshold is set using the average sof for Liege/Lombardia/Tour de France (240)
# this threshold is set using average sof for monuments+TDF (0.82)
# and finding where that indicates 8 places

limits <- glm_data %>% 
  filter(sof > 0.789 & sof < 0.851 & thresh == 8) %>%
  summarize(min(fit),
            max(fit),
            median(fit))

# this shows the threshold by sof for 10.7% chance (top 12 for average TDF and top 5 for average worst field)

filter(glm_data, fit > 0.105 & fit < 0.11) %>% 
  ggplot(aes(x = sof, y = thresh))+geom_point()+geom_smooth(method = "lm")+geom_smooth(color = 'red')

# this shows threshold by sof for 5% chance (top 5-6 for average TDF and top 2 for average worst field)

filter(glm_data, fit > 0.0475 & fit < 0.0525) %>% 
  ggplot(aes(x = sof, y = thresh))+geom_point()+geom_smooth(method = "lm")+geom_smooth(color = 'red')

# this shows threshold by sof for 9% chance (top 10 for average TDF and top 4 for average worst field)

filter(glm_data, fit > 0.0875 & fit < 0.0925) %>% 
  ggplot(aes(x = sof, y = thresh))+geom_point()+geom_smooth(method = "lm")+geom_smooth(color = 'red')

# this shows threshold by limits above

glm_data %>% filter(fit > (limits[, 3] - 0.003) & fit < (limits[, 3] + 0.003)) %>% 
  ggplot(aes(x = sof, y = thresh))+geom_point()+geom_smooth(method = "lm")+geom_smooth(color = 'red')+
  geom_vline(xintercept = 0.82)

#
#
# filter to TDF top 8 and scale down or up

limits_actual <- glm_data %>%
  
  filter(fit > (limits[, 3] - 0.003) & fit < (limits[, 3] + 0.003)) %>%
  
  loess(thresh ~ sof, data = .)

#
#
# actual limits

stage_data_perf <- stage_data %>%
  
  # the limit is predicted from the limits_actual model above

  inner_join(
    
    cbind(
      
      limit = predict(limits_actual, individual_races_sop %>% rename(sof = sop)),
      individual_races_sop %>% select(stage, race, year, sof = sop, url)
      
      
    ), by = c("stage", "race", "year", "url")
    
  ) %>%
  
  mutate(rnk = ifelse(is.na(rnk), 200, rnk)) %>%
  
  # round the limit to nearest integer
  mutate(limit = round(limit, 0),
         limit = ifelse(limit<1,1,limit)) %>%

  # success is any finish better than or equal to the limit
  mutate(success = ifelse(rnk < (limit + 1), 1, 0)) %>%
  
  # find the slowest success and scale off that
  mutate(success_time = ifelse(success == 1, total_seconds, NA)) %>%
  
  # find the slowest success
  group_by(stage, race, year, url) %>%
  mutate(success_time = max(success_time, na.rm = T)) %>%
  ungroup() %>%
  
  # bunch sprint or not?
  group_by(stage, race, year, url) %>%
  mutate(x25th = ifelse(rnk == 25, total_seconds == win_seconds, NA),
         x25th = mean(x25th, na.rm = T),
         x2nd = ifelse(rnk == 2, total_seconds >= (win_seconds + 5), NA),
         x2nd = mean(x2nd, na.rm = T)) %>%
  ungroup() %>%
  
  rename(bunch_sprint = x25th,
         solo = x2nd) %>%
  
  mutate(rel_success = total_seconds - success_time,

         # either way, it is adjusted for strength of peloton by dividing limit by 5
         points_finish = (1 / (rnk + 1)) * (limit / 5),
         points_finish = ifelse(success == 1, points_finish, 0)) %>%
  
  # leader vs domestique rating
  mutate(leader_rating = ifelse(tm_pos == 1, 2,
                                ifelse(tm_pos == 2, 1,
                                       ifelse(tm_pos == 3, 0.33, 0))),
         leader_rating = ifelse(rnk < 31, leader_rating, 0))

#
#
#
#
#
# Intensity (speed vs expected) -------------------------------------------

profile_data <- stage_data_perf %>%
  filter(rnk == 1) %>%
  select(stage, race, year, pred_climb_difficulty, act_climb_difficulty,
         speed, length, total_vert_gain, bunch_sprint, sof,
         grand_tour, time_trial, one_day_race, class) %>%
  unique() %>%
  filter(!is.na(total_vert_gain)) %>%
  filter(!is.na(pred_climb_difficulty))

# correlations

profile_data %>% select(-stage, -race, -year, -act_climb_difficulty, -class) %>% cor(.) -> cor_profile_data

# predict speed based on inputs

library(xgboost)

spd_data <- profile_data %>%
  
  filter(time_trial == 0)

# basic linear model

spd_lm <- lm(speed ~ pred_climb_difficulty + total_vert_gain * length +
               grand_tour + one_day_race, data = spd_data)

spd_preds <- cbind(
  
  spd_data,
  
  pred_speed = predict(spd_lm, spd_data)
  
) %>%
  
  mutate(spd_resid = speed - pred_speed)

# establish train and test sets

train <- sample(1:nrow(bs_data), nrow(bs_data) * 0.67)

# train

xgb.train <- xgb.DMatrix(
  
  data = as.matrix(bs_data[train, ] %>%
                     select(pred_climb_difficulty, length, one_day_race, uphill_finish, grand_tour)),
  
  label = bs_data[train, ]$bunch_sprint
  
)

# test

xgb.test <- xgb.DMatrix(
  
  data = as.matrix(bs_data[-train, ] %>%
                     select(pred_climb_difficulty, length, one_day_race, uphill_finish, grand_tour)),
  
  label = bs_data[-train, ]$bunch_sprint
  
)

# outline parameters

params <- list(
  
  booster = "gbtree",
  eta = 0.01,
  max_depth = 2,
  gamma = 0.33,
  subsample = 1,
  colsample_bytree = 1,
  objective = "reg:squarederror"
  
)

# run xgboost model

gbm_model <- xgb.train(params = params,
                       data = xgb.train,
                       nrounds = 10000,
                       nthreads = 4,
                       early_stopping_rounds = 10,
                       watchlist = list(val1 = xgb.train,
                                        val2 = xgb.test),
                       verbose = 1)

# this outputs GBM predictions for all data

gbm_predict = cbind(
  
  bs_data %>%
    select(stage, race, year, one_day_race, grand_tour, length, date, uphill_finish, pred_climb_difficulty,
           bunch_sprint) %>%
    mutate(length = length + 200),
  
  pred = predict(gbm_model, 
                 as.matrix(bs_data %>% select(pred_climb_difficulty, length, one_day_race, 
                                              uphill_finish, grand_tour), reshape=T)))

#
# AGING CURVE // we have age for 86% of stages/riders
#

# Aging Curve Modelling ---------------------------------------------------


perf_age_linked <- stage_data_perf %>%
  
  select(-data, -master_team, -uphill_finish, -summit_finish, -position_highest,
         -last_climb, -act_climb_difficulty, -raw_climb_difficulty, -number_cat_climbs,
         -concentration, -cat_climb_length, -final_20km_vert_gain, -final_1km_gradient, 
         -total_vert_gain, -perc_elev_change) %>%
  
  inner_join(
    
    dbGetQuery(con, "SELECT rider, date as dob FROM rider_attributes") %>%
      
      mutate(rider = str_to_title(rider)), by = c("rider")) %>%
  
  mutate(age = as.numeric(as.Date(date)-as.Date(dob))/365.25)

# BY SUCCESS PERCENTAGE

age_perf_binned <- perf_age_linked %>%
  
  group_by(age = round(age,0), rider) %>%
  summarize(success = mean(success, na.rm = T),
            races = n()) %>%
  ungroup() %>%
  
  arrange(rider, age) %>%

  group_by(rider) %>%
  filter(age == (lag(age)+1)) %>%
  mutate(delta = success - lag(success),
         prior_success = lag(success),
         hm = 2 / ((1 / races)+(1/lag(races)))) %>%
  ungroup() %>%
  
  filter(!is.na(delta)) %>%
  
  group_by(age) %>%
  summarize(delta = sum(hm * delta, na.rm = T)/sum(hm,na.rm = T),
            prior_success = sum(hm * prior_success, na.rm = T)/sum(hm, na.rm = T),
            hm = sum(hm, na.rm = T)) %>%
  ungroup()

#
# I define bunch sprint as 1st place getting same time as 25th place
#

# Bunch Sprint Modelling --------------------------------------------------


library(xgboost)

bs_data <- stage_data_perf %>%
  filter(time_trial == 0 & rnk == 1 & !is.na(bunch_sprint)) %>%
  mutate(length = length - 200) %>%
  
  select(-data) %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty))

# establish train and test sets

train <- sample(1:nrow(bs_data), nrow(bs_data) * 0.67)

# train

xgb.train <- xgb.DMatrix(
  
  data = as.matrix(bs_data[train, ] %>%
                     select(pred_climb_difficulty, length, one_day_race, uphill_finish, grand_tour)),
  
  label = bs_data[train, ]$bunch_sprint
  
)

# test

xgb.test <- xgb.DMatrix(
  
  data = as.matrix(bs_data[-train, ] %>%
                     select(pred_climb_difficulty, length, one_day_race, uphill_finish, grand_tour)),
  
  label = bs_data[-train, ]$bunch_sprint
  
)

# outline parameters

params <- list(
  
  booster = "gbtree",
  eta = 0.01,
  max_depth = 2,
  gamma = 0.33,
  subsample = 1,
  colsample_bytree = 1,
  objective = "reg:squarederror"
  
)

# run xgboost model

gbm_model <- xgb.train(params = params,
                       data = xgb.train,
                       nrounds = 10000,
                       nthreads = 4,
                       early_stopping_rounds = 10,
                       watchlist = list(val1 = xgb.train,
                                        val2 = xgb.test),
                       verbose = 1)

# this outputs GBM predictions for all data

gbm_predict = cbind(
  
  bs_data %>%
    select(stage, race, year, one_day_race, grand_tour, length, date, uphill_finish, pred_climb_difficulty,
           bunch_sprint) %>%
    mutate(length = length + 200),
  
  pred = predict(gbm_model, 
                 as.matrix(bs_data %>% select(pred_climb_difficulty, length, one_day_race, 
                                                        uphill_finish, grand_tour), reshape=T)))

#
#
#

who_is_in_bunch_sprints <- stage_data_perf %>%
  filter(time_trial == 0 & !is.na(bunch_sprint)) %>%
  mutate(length = length - 200) %>%
  
  select(-data) %>%
  select(-new_st, -missing_profile_data, -position_highest, -last_climb, -act_climb_difficulty, 
         -raw_climb_difficulty, -number_cat_climbs, -concentration, -cat_climb_length, 
         -final_1km_gradient, -total_vert_gain, -final_20km_vert_gain, -perc_elev_change,
         -gain_back_5, -back_5_seconds, -limit, -success_time, -solo, -rel_success, -url, 
         -summit_finish, -gc_seconds, -rel_speed, -top_variance, -variance) %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  
  filter(bunch_sprint == 1 & year > 2012) %>%
  
  mutate(in_bunch = ifelse(gain_1st <= 5, 1, 0),
         best_on_team = ifelse(in_bunch == 1 & tm_pos == 1, 1, 0),
         and_top_3 = ifelse(best_on_team == 1 & rnk <= 3, 1, 0),
         and_win = ifelse(best_on_team == 1 & rnk == 1, 1, 0),
         points_per_opp = ifelse(best_on_team == 1, points_finish, NA)) %>%
  
  group_by(rider, master_team) %>%
  summarize(in_bunch = mean(in_bunch, na.rm = T),
            sprints = n(),
            top_on_team = mean(best_on_team, na.rm = T),
            and_top_3 = mean(and_top_3, na.rm = T),
            points_per_opp = mean(points_per_opp, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(delivering = and_top_3 / top_on_team)

#
#
#

most_in_bunch_sprints <- stage_data_perf %>%
  filter(time_trial == 0 & !is.na(bunch_sprint)) %>%
  mutate(length = length - 200) %>%
  
  select(-data) %>%
  select(-new_st, -missing_profile_data, -position_highest, -last_climb, -act_climb_difficulty, 
         -raw_climb_difficulty, -number_cat_climbs, -concentration, -cat_climb_length, 
         -final_1km_gradient, -total_vert_gain, -final_20km_vert_gain, -perc_elev_change,
         -gain_back_5, -back_5_seconds, -limit, -success_time, -solo, -rel_success, -url, 
         -summit_finish, -gc_seconds, -rel_speed, -top_variance, -variance) %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  
  filter(year > 2012) %>%
  
  group_by(rider, master_team) %>%
  summarize(stages = n(),
            sof = mean(sof, na.rm = T),
            sprint_stages = mean(bunch_sprint == 1, na.rm = T)) %>%
  ungroup()

#

most_in_climbing_stages <- stage_data_perf %>%
  filter(time_trial == 0 & !is.na(bunch_sprint)) %>%
  mutate(length = length - 200) %>%
  
  select(-data) %>%
  select(-new_st, -missing_profile_data, -position_highest, -last_climb, -act_climb_difficulty, 
         -raw_climb_difficulty, -number_cat_climbs, -concentration, -cat_climb_length, 
         -final_1km_gradient, -total_vert_gain, -final_20km_vert_gain, -perc_elev_change,
         -gain_back_5, -back_5_seconds, -limit, -success_time, -solo, -rel_success, -url, 
         -summit_finish, -gc_seconds, -rel_speed, -top_variance, -variance) %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  
  filter(year > 2012) %>%
  
  group_by(rider) %>%
  summarize(stages = n(),
            sof = mean(sof, na.rm = T),
            climbing_stages = mean(bunch_sprint == 0 & pred_climb_difficulty >= 8, na.rm = T)) %>%
  ungroup()

#
#
#

when_leader <- stage_data_perf %>%
  filter(!is.na(bunch_sprint)) %>%
  mutate(length = length - 200) %>%
  
  select(-data) %>%
  select(-new_st, -missing_profile_data, -position_highest, -last_climb, -act_climb_difficulty, 
         -raw_climb_difficulty, -number_cat_climbs, -concentration, -cat_climb_length, 
         -final_1km_gradient, -total_vert_gain, -final_20km_vert_gain, -perc_elev_change,
         -gain_back_5, -back_5_seconds, -limit, -success_time, -solo, -rel_success, -url, 
         -summit_finish, -gc_seconds, -rel_speed, -top_variance, -variance) %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  
  filter(year > 2012) %>%
  mutate(points_per_opp = ifelse(tm_pos == 1, points_finish, NA)) %>%
  
  group_by(rider, 
           master_team, 
           type = ifelse(bunch_sprint == 0 & pred_climb_difficulty >= 8, "climbing", 
                         ifelse(bunch_sprint == 1, "sprints", 
                                ifelse(time_trial == 1, "time trial", "other")))) %>%
  summarize(stages = n(),
            sof = mean(sof, na.rm = T),
            median = median(tm_pos, na.rm = T),
            leader = mean(tm_pos == 1, na.rm = T),
            second = mean(tm_pos == 2, na.rm = T),
            opportunities = sum(tm_pos == 1, na.rm = T),
            points_per_opp = mean(points_per_opp, na.rm = T),
            points = mean(points_finish, na.rm = T)) %>%
  ungroup()

#
#
#

team_best <- stage_data_perf %>%
  filter(!is.na(bunch_sprint)) %>%
  mutate(length = length - 200) %>%
  
  select(-data) %>%
  select(-new_st, -missing_profile_data, -position_highest, -last_climb, -act_climb_difficulty, 
         -raw_climb_difficulty, -number_cat_climbs, -concentration, -cat_climb_length, 
         -final_1km_gradient, -total_vert_gain, -final_20km_vert_gain, -perc_elev_change,
         -gain_back_5, -back_5_seconds, -limit, -success_time, -solo, -rel_success, -URL, 
         -summit_finish, -gc_seconds, -rel_speed, -top_variance, -variance) %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  
  filter(year > 2017 & tm_pos == 1) %>%
  mutate(points_per_opp = ifelse(tm_pos == 1, points_finish, NA)) %>%
  
  group_by(master_team, 
           rider,
           type = ifelse(bunch_sprint == 0 & pred_climb_difficulty >= 8, "climbing", 
                         ifelse(bunch_sprint == 1, "sprints", 
                                ifelse(time_trial == 1, "time trial", "other")))) %>%
  summarize(stages = n(),
            sof = mean(sof, na.rm = T),
            median = median(tm_pos, na.rm = T),
            points_per_opp = mean(points_per_opp, na.rm = T),
            success = mean(success, na.rm = T)) %>%
  ungroup()

#

team_best <- stage_data_perf %>%
  filter(!is.na(bunch_sprint)) %>%
  mutate(length = length - 200) %>%
  
  select(-data) %>%
  select(-new_st, -missing_profile_data, -position_highest, -last_climb, -act_climb_difficulty, 
         -raw_climb_difficulty, -number_cat_climbs, -concentration, -cat_climb_length, 
         -final_1km_gradient, -total_vert_gain, -final_20km_vert_gain, -perc_elev_change,
         -gain_back_5, -back_5_seconds, -limit, -success_time, -solo, -rel_success, -URL, 
         -summit_finish, -gc_seconds, -rel_speed, -top_variance, -variance) %>%
  
  filter(!is.na(bunch_sprint)) %>%
  filter(!is.na(pred_climb_difficulty)) %>%
  
  filter(year > 2013) %>%
  mutate(points_per_opp = ifelse(tm_pos == 1, points_finish, NA)) %>%
  
  group_by(team, race, stage, year) %>%
  mutate(points_per_opp = max(points_per_opp, na.rm = T),
         first_rnk = min(rnk, na.rm = T)) %>%
  
  group_by(master_team, 
           rider,
           type = ifelse(bunch_sprint == 0 & pred_climb_difficulty >= 8, "climbing", 
                         ifelse(bunch_sprint == 1, "sprints", 
                                ifelse(time_trial == 1, "time trial", "other")))) %>%
  summarize(stages = n(), win = mean(first_rnk == 1, na.rm = T), podium = mean(first_rnk < 4, na.rm = T),
            sof = mean(sof, na.rm = T), pers_win = mean(rnk == 1, na.rm = T),
            
            median = median(tm_pos, na.rm = T),
            points_per_opp = mean(points_per_opp, na.rm = T),
            success = mean(success, na.rm = T)) %>%
  ungroup()

#

teammates_in_race <- stage_data_perf %>%
  filter(!is.na(bunch_sprint)) %>% 
  filter(!is.na(pred_climb_difficulty)) %>% 
  filter(year > 2018) %>% 
  filter(bunch_sprint == 1) %>% 
  filter(master_team == 'UAE Team') %>% 
  
  select(rider, rnk, stage, race, year) %>% 
  unique() %>% 
  
  group_by(stage, race, year) %>% 
  mutate(first_rnk = min(rnk, na.rm = T)) %>% 
  ungroup() %>% 
  
  inner_join(stage_data_perf %>%
               filter(!is.na(bunch_sprint)) %>% 
               filter(!is.na(pred_climb_difficulty)) %>% 
               filter(year > 2018) %>% 
               filter(bunch_sprint == 1) %>%
               filter(master_team == 'UAE Team') %>% 
               
               select(rider, rnk, stage, race, year) %>% 
               unique(), by = c("stage", "race", "year")) %>% 
  
  filter(rider.x != rider.y) %>% 
  
  group_by(rider.x, rider.y) %>% 
  summarize(win_rate = mean(first_rnk == 1, na.rm = T),
            stages = n(), 
            X_wins = mean(rnk.x == 1, na.rm = T), 
            Y_wins = mean(rnk.y == 1, na.rm = T)) %>% 
  ungroup()

#
# old logistic model
#

# bunch_sprint_model <- glm(bunch_sprint ~ pred_climb_difficulty + length + one_day_race + uphill_finish, 
#                           data = stage_data_perf %>%
#                             filter(time_trial == 0 & rnk == 1) %>%
#                             mutate(length = length - 200),
#                           family = "binomial")

# this is slightly sensitive to what place you use as bunch sprint qualifier
# eg 20th place makes the one day race penalty a bit smaller (as expected) but keeps other coefs the same

# bunch sprint probability is heavily affected by:
# One day race (-1.19 coef)
# Length (50 kms = -0.29 coef)
# Climb difficulty (5 difficulty points = -1.9 coef)
# Uphill Finish = -1.23 coef

# Performance by bunch sprint or not bunch sprint

bs_performance <- stage_data_perf %>%
  
  filter(time_trial == FALSE) %>%
  
  filter(!is.na(pred_climb_difficulty)) %>%
  filter(year > 2016) %>%
  
  mutate(win = ifelse(rnk == 1, 1, 0))

# Climbing Time Gained Model ----------------------------------------------


# the rationale is to award extra credit for climbing performance which gain big time on rivals
# and award less credit for climbing performance which do not gain much time or any on rivals

climbing_time_gained_model <- stage_data_perf %>%
  
  filter(success == 1 | rnk == (limit + 10)) %>%
  
  select(rider, race, stage, year, rnk, success, limit, rel_success,
         pred_climb_difficulty, success_time, total_seconds, win_seconds, points_finish) %>%
  
  mutate(vs_limit = limit + 1 - rnk) %>%
  filter(pred_climb_difficulty > 7.5)

# iterations were looked at including vs_limit & pred_climb_diff separately but simple interaction term is best

climbing_time_mod <- lm(rel_success ~ vs_limit:pred_climb_difficulty, 
                        
                        data = climbing_time_gained_model)

# model expected and calculate relative

climbing_relative <- cbind(
  
  climbing_time_gained_model,
  
  modeled = predict(climbing_time_mod, climbing_time_gained_model)) %>%
  
  mutate(relative_to_model = rel_success - modeled)

#
#
#
#
#
#
#
#
# Test Performance Section -----------------------------------------------------
#
#

stage_types <- stage_data_perf %>%
    filter(rnk == 1 & !is.na(pred_climb_difficulty)) %>%
  
  left_join(gbm_predict %>%
               select(stage, race, year, bs_pred = pred), by = c("stage","race","year")) %>%
  
  mutate(bs_pred = ifelse(time_trial == 1, 0, bs_pred)) %>%
  
  select(stage, race, year, date, pred_climb_difficulty, concentration, uphill_finish,
         bs_pred, one_day_race, time_trial, bunch_sprint) %>%
  
  # split into four categories, then split flat and big hills into STG vs ODR
  mutate(stage_taxonomy = 
           ifelse(time_trial == 1, "time_trial",
                  ifelse(pred_climb_difficulty < 3.99, "flat",
                         ifelse(pred_climb_difficulty < 9,
                                ifelse(is.na(concentration), "hills",
                                       ifelse(concentration < 5, "hills", "mountains")), "mountains"))),
         stage_taxonomy = ifelse(one_day_race == 1, paste0("ODR_", stage_taxonomy), paste0("STG_", stage_taxonomy)),
         stage_taxonomy = ifelse(str_detect(stage_taxonomy, "mountains") | 
                                   str_detect(stage_taxonomy, "time_trial"), 
                                 str_sub(stage_taxonomy, 5, nchar(stage_taxonomy)), stage_taxonomy)) %>%
  
  select(stage, race, year, date, stage_taxonomy, pred_climb_difficulty,
         concentration, uphill_finish, bunch_sprint, bs_pred)

# here I'm calculating the number of days where the cycling calendar halts
# between mid-late Oct and mid-Jan
# instead of maintaining that gap I replace it with 2 weeks worth of time

dates_year <- stage_data_perf %>%
  
  filter(rnk == 1) %>%
  mutate(date = as.Date(date)) %>%
  group_by(year) %>%
  summarize(first = min(date, na.rm = T),
            last = max(date, na.rm = T)) %>%
  ungroup() %>% 
  
  gather(signifier, date, -year) %>% 
  arrange(date) %>%
  mutate(take_out = ifelse(signifier == "last", 0, as.numeric(date - lag(date))), 
         take_out = ifelse(year == min(year), 0, take_out), 
         take_out = take_out - 14) %>% 
  arrange(-year) %>% 
  mutate(total_take_out = cumsum(take_out)-take_out) %>% 
  filter(signifier == "first") %>%
  select(year, total_take_out)

#

input_into_perf_model <- stage_data_perf %>%
  filter(year > 2013) %>%
  select(rider, tm_pos, master_team, success, points_finish, bunch_sprint, length, pred_climb_difficulty,
         date, year, race, year, stage, class, rnk, limit, leader_rating) %>%
  
  inner_join(
    
    stage_types %>%
      select(stage_taxonomy, stage, race, year, bs_pred), by = c("stage", "race", "year")
    
  ) %>% 
  filter(!is.na(date)) %>%
  mutate(date = as.Date(date)) %>%
  
  inner_join(dates_year, by = c("year")) %>%
  
  mutate(since = as.numeric(lubridate::today()-date)-total_take_out, 
         weight = 1 / (12 + since)) %>%
  
  select(-total_take_out) %>%
  
  mutate(FL = ifelse(stage_taxonomy %in% c("ODR_flat", "STG_flat"), leader_rating, NA),
         BH = ifelse(stage_taxonomy %in% c("ODR_hills", "STG_hills"), leader_rating, NA),
         MT = ifelse(stage_taxonomy %in% c("ODR_hills", "mountains", "STG_hills"), leader_rating, NA),
         TT = ifelse(stage_taxonomy %in% c("time_trial"), leader_rating, NA),
         FLw = ifelse(stage_taxonomy %in% c("ODR_flat", "STG_flat"), weight, NA),
         BHw = ifelse(stage_taxonomy %in% c("ODR_hills", "STG_hills"), weight, NA),
         MTw = ifelse(stage_taxonomy %in% c("ODR_hills", "mountains", "STG_hills"), weight, NA),
         TTw = ifelse(stage_taxonomy %in% c("time_trial"), weight, NA)) %>%
  
  arrange(rider, date)

#
# Overall performance averages for full sample
#

skills_cor_matrix <- cor(
  
  input_into_perf_model %>%
  
  group_by(rider, stage_taxonomy) %>% 
  summarize(weighted = sum(points_finish * weight, na.rm = T) / sum(weight, na.rm = T),
            races = n()) %>%
  ungroup() %>% 
  filter(races > 9) %>%
  select(-races) %>% 
  spread(stage_taxonomy, weighted) %>% 
  mutate(x = ODR_flat * STG_flat * mountains * time_trial * ODR_hills * STG_hills) %>% 
  filter(!is.na(x)) %>%
    .[, 2:7])

#

x2019_races <- stage_types %>%
  filter(year > 2016) %>%
  filter(!is.na(date)) %>%
  mutate(date = as.Date(date)) %>%
  select(date) %>%
  rbind(tibble(date = lubridate::today())) %>%
  unique()

#

tictoc::tic()

d_list <- vector("list", length(x2019_races$date))

dr_list <- vector("list", length(x2019_races$date))

for(d in 1:length(x2019_races$date)) {
  
  # filter to races
  fd <- input_into_perf_model %>%
    
    filter(as.Date(date) < x2019_races$date[[d]])
  
  # calc points
  df <- fd %>%
    
    select(-FL, -BH, -MT, -FLw, -BHw, -MTw, -TTw, -TT) %>%
    
    group_by(rider) %>%
    mutate(race_days_4m = sum(date > (x2019_races$date[[d]] - 121), na.rm = T)) %>%
    ungroup() %>%
    
    group_by(rider, stage_taxonomy) %>% 
    
    #mutate(prior_result = ifelse(stage_taxonomy %in% c("STG_flat","ODR_flat"), as.numeric(rnk==1), as.numeric(success==1)),
    #       prior_result = ifelse(is.na(prior_result), 0, prior_result),
    #       prior_result = ifelse(date > (x2019_races$date[[d]] - 31), prior_result, 0),
    #       prior_result = ifelse(date == max(date, na.rm = T), prior_result, NA)) %>%
    
    summarize(weighted = sum(points_finish * weight, na.rm = T) / (sum(weight, na.rm = T)),
              race_days_4m = mean(race_days_4m, na.rm = T),
              #prior_result = mean(prior_result, na.rm = T),
              races = n()) %>%
    ungroup()
  
  # calc leader rating
  #dr <- fd %>%
  #
  #  group_by(rider) %>%
  #  summarize(flat = sum(FLw * FL, na.rm = T) / sum(FLw, na.rm = T),
  #            big_hills = sum(BHw * BH, na.rm = T) / sum(BHw, na.rm = T),
  #            mountains = sum(MTw * MT, na.rm = T) / sum(MTw, na.rm = T),
  #            time_trial = sum(TTw * TT, na.rm = T) / sum(TTw, na.rm = T),) %>%
  #  ungroup()
  
  d_list[[d]] <- df %>%
    mutate(date = x2019_races$date[[d]])
  
  #dr_list[[d]] <- dr %>%
  #  mutate(date = x2019_races$date[[d]])
  
}

tictoc::toc()

#

daily_rankings <- bind_rows(d_list) %>%
  mutate(weighted = ifelse(is.na(weighted), 0, weighted),
         weighted = ifelse(stage_taxonomy == "time_trial",
                           ifelse(races < 5, ((weighted * races)) / ((5)), weighted),
                           ifelse(races < 20, ((weighted * races)) / ((20)), weighted)))

#

leader_ratings_daily <- bind_rows(dr_list) %>%
  gather(stage_taxonomy, leader_rating, -rider, -date) %>%
  mutate(leader_rating = ifelse(leader_rating == "NaN", 0, leader_rating)) %>%
  
  inner_join(
    
    daily_rankings %>%
      select(rider, stage_taxonomy, date, races), by = c("rider", "date", "stage_taxonomy")
    
  ) %>%
  
  # regress to mean of 0.2
  mutate(leader_rating = ifelse(races < 10, ((leader_rating * races) + ((10-races) * 0.20)) / ((10)), leader_rating))

#
#
# Bring in PCS game picks to evaluate

pcs_game_picks <- dbGetQuery(con, "SELECT * FROM pcs_game_picks") %>%
  
  group_by(stage, race, year) %>%
  mutate(selections = sum(number_picks, na.rm = T)) %>%
  ungroup() %>%
  
  filter(selections > 499) %>%
  
  # roughly 95% of slots are filled
  mutate(selections = selections / 0.9,
         rate = number_picks / selections * 5,
         rate = ifelse(rate > 0.98, 0.98, rate)) %>%
  
  select(-selections, -result, -url)

pcs_game_picks$picked_rider <-  str_to_title(tolower(pcs_game_picks$picked_rider))
pcs_game_picks$picked_rider <- iconv(pcs_game_picks$picked_rider, from="UTF-8", to = "ASCII//TRANSLIT")
pcs_game_picks$race <- str_to_title(tolower(pcs_game_picks$race))
pcs_game_picks$race <- iconv(pcs_game_picks$race, from="UTF-8", to = "ASCII//TRANSLIT")
pcs_game_picks$race <- tolower(pcs_game_picks$race)

#
#
# link actual results with leader rating and skill ratings

testing_performance <- stage_data_perf %>%
  
  select(rnk, stage, race, year, tm_pos, rider, team) %>%
  
  inner_join(stage_types %>%
               mutate(date = as.Date(date)), by = c("stage", "race", "year")) %>%
  
  filter(year > 2018) %>%
  
  left_join(daily_rankings, by = c("rider", "date", "stage_taxonomy")) %>%
  
  left_join(leader_ratings_daily %>%
              select(-races), by = c("rider", "date", "stage_taxonomy")) %>%
  
  left_join(pcs_game_picks %>%
              select(rider = picked_rider, stage, race, year, rate), by = c("stage", "year", "race", "rider")) %>%
  
  mutate(weighted = ifelse(is.na(weighted), 0, weighted),
         leader_rating = ifelse(is.na(leader_rating), 0.2, leader_rating),
         rate = ifelse(is.na(rate), 0, rate),
         race_days_4m = ifelse(is.na(race_days_4m), 0, race_days_4m)) %>%

  # only about 60% of stages have pcs game data
  inner_join(pcs_game_picks %>%
               select(stage, race, year) %>%
               unique(), by = c("stage", "race", "year")) %>%
  
  mutate(win = ifelse(rnk == 1, 1, 0)) %>%
  
  # only consider this factor starting in May and for mountain stages
  mutate(race_days_4m = ifelse(lubridate::month(date) > 4 & lubridate::month(date) < 11,
                               race_days_4m, 0),
         race_days_4m = ifelse(stage_taxonomy == "mountains", race_days_4m, race_days_4m)) %>%
  
  # using rel_tm as denoted by leader_rating improves each of model AUCs by ~ 0.01
  
  group_by(team, race, stage, year) %>%
  mutate(rel_tm = leader_rating - mean(leader_rating, na.rm = T),
         rel_tm = rank(-leader_rating, ties.method = "average"),
         skill_tm = sum(weighted, na.rm = T),
         tm = n()) %>%
  ungroup() %>%
  
  mutate(skill_tm = (skill_tm - weighted) / (tm - 1),
         skill_tm = ifelse(is.na(skill_tm), 0, skill_tm)) %>%
  select(-tm) %>%
  
  # adjust in stage
  group_by(race, stage, year) %>%
  mutate(pred_rnk = log(rank(-weighted, ties.method = "average")+1),
         race_days_rel = race_days_4m - mean(race_days_4m, na.rm = T),
         skill_tm = skill_tm - mean(skill_tm)) %>%
  ungroup()
  
#
#
# this one is good

the_rank_model <- testing_performance %>%
  
  group_by(stage_taxonomy) %>%
  do(broom::tidy(glm(win ~ pred_rnk, data = ., family = "binomial"))) %>%
  ungroup() %>%
  select(stage_taxonomy, term, estimate) %>%
  spread(term, estimate) %>%
  janitor::clean_names()

#
#
# this one is better

the_full_model <- testing_performance %>%
  
  group_by(stage_taxonomy) %>%
  do(broom::tidy(glm(win ~ pred_rnk + rel_tm, data = ., family = "binomial"))) %>%
  ungroup() %>%
  select(stage_taxonomy, term, estimate) %>%
  spread(term, estimate) %>%
  janitor::clean_names()

#
#
# full ensemble featuring position in team, skill rating, and pcs game rating

the_ensemble_model <- testing_performance %>%
  
  group_by(stage_taxonomy) %>%
  do(broom::tidy(glm(win ~ pred_rnk + rel_tm + rate, data = ., family = "binomial"))) %>%
  ungroup() %>%
  select(stage_taxonomy, term, estimate) %>%
  spread(term, estimate) %>%
  janitor::clean_names()

#
#
# full ensemble featuring position in team, skill rating, and pcs game rating

the_cadillac_model <- testing_performance %>%
  
  group_by(stage_taxonomy) %>%
  do(broom::tidy(glm(win ~ pred_rnk + rel_tm + rate + I(race_days_rel^2) + skill_tm, data = ., family = "binomial"))) %>%
  ungroup() %>%
  select(stage_taxonomy, term, estimate) %>%
  spread(term, estimate) %>%
  janitor::clean_names()

#
#
# just PCS Game

the_game_model <- testing_performance %>%
  
  group_by(stage_taxonomy) %>%
  do(broom::tidy(glm(win ~ rate, data = ., family = "binomial"))) %>%
  ungroup() %>%
  select(stage_taxonomy, term, estimate) %>%
  spread(term, estimate) %>%
  janitor::clean_names()

#

tests_taxonomy <- tibble(pred_rnk = seq(1,100,1),
                         j = 1) %>%
  inner_join(the_rank_model %>%
               rename(coef1 = pred_rnk) %>%
               mutate(j = 1), by = c("j")) %>%
  mutate(calc = (intercept + (log(pred_rnk+1) * coef1)),
         pred = exp(calc)/(1+exp(calc)))

#

ggplot(tests_taxonomy, 
       aes(x = pred_rnk, y = pred, color = stage_taxonomy))+
  geom_col()+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~stage_taxonomy, ncol = 1)

#
#
# Build predictions based on the model chosen

predictions <- testing_performance %>%
  
  inner_join(the_cadillac_model %>%
               rename(coef1 = pred_rnk,
                      coef2 = rel_tm,
                      coef3 = rate,
                      coef4 = i_race_days_rel_2,
                      coef5 = skill_tm) %>%
               mutate(coef4 = ifelse(is.na(coef4), 0, coef4)), by = c("stage_taxonomy")) %>%
  
  #inner_join(the_rank_model %>%
  #             rename(coef1 = pred_rnk), by = c("stage_taxonomy")) %>%
  
  #inner_join(the_full_model %>%
  #             rename(coef1 = pred_rnk,
  #                    coef2 = rel_tm), by = c("stage_taxonomy")) %>%
  
  mutate(calc = (intercept + 
                   (pred_rnk * coef1) + 
                   (coef2 * rel_tm) + 
                   (coef3 * rate) + 
                   (coef4 * (race_days_rel ^ 2)) +
                   (coef5 * (skill_tm))),
         pred = exp(calc)/(1+exp(calc))) %>%
  
  group_by(race, stage, year) %>%
  mutate(pred = pred - min(pred, na.rm = T),
         pred = pred / sum(pred, na.rm = T)) %>%
  ungroup()
  
#
#
# evaluate

ST = "mountains"

labels <- predictions %>% filter(stage_taxonomy == ST) %>% select(win) %>% as.list() %>% .[[1]]
scores <- predictions %>% filter(stage_taxonomy == ST) %>% select(pred) %>% as.list() %>% .[[1]]

roc_obj <- pROC::roc(labels, scores)
print(pROC::auc(roc_obj))
print(paste0("Brier score = ", mean((labels - scores)^2, na.rm = T)))

print(pROC::ggroc(roc_obj)+geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1)))

#
#
# XGBOOST PREDICTOR
#
#
  
library(xgboost)

# establish train and test sets

races <- predictions %>%
  select(stage, race, year) %>%
  unique()

train <- sample(1:nrow(races), nrow(races) * 0.8)

train_races <- races[train, ] %>%
  inner_join(predictions %>%
               select(stage, race, year) %>%
               rowid_to_column()) %>%
  select(rowid) %>%
  as.list() %>%
  .[[1]]

# train

xgb.train <- xgb.DMatrix(
  
  data = as.matrix(predictions[train_races, ] %>%
                     mutate(rdr_quad = race_days_rel ^ 2) %>%
                     select(pred_rnk, rate, rel_tm, skill_tm, rdr_quad, prior_result)),
  
  label = predictions[train_races, ]$win
  
)

# test

xgb.test <- xgb.DMatrix(
  
  data = as.matrix(predictions[-train, ] %>%
                     mutate(rdr_quad = race_days_rel ^ 2) %>%
                     select(pred_rnk, rate, rel_tm, skill_tm, rdr_quad, prior_result)),
  
  label = predictions[-train_races, ]$win
  
)

# outline parameters

params <- list(
  
  booster = "gbtree",
  eta = 0.0001,
  max_depth = 2,
  gamma = 0.33,
  subsample = 1,
  colsample_bytree = 1,
  objective = "reg:logistic"
  
)

# run xgboost model

gbm_w_model <- xgb.train(params = params,
                       data = xgb.train,
                       nrounds = 10000,
                       nthreads = 4,
                       early_stopping_rounds = 10,
                       watchlist = list(val1 = xgb.train,
                                        val2 = xgb.test),
                       verbose = 1)

# this outputs GBM predictions for all data

gbm_win_predict = cbind(
  
  predictions[-train, ] %>%
    mutate(rdr_quad = race_days_rel ^ 2) %>%
    select(pred_rnk, rate, rel_tm, skill_tm, rdr_quad, prior_result),
  
  pred = predict(gbm_w_model, 
                 as.matrix(predictions[-train, ] %>%
                             mutate(rdr_quad = race_days_rel ^ 2) %>%
                             select(pred_rnk, rate, rel_tm, skill_tm, rdr_quad, prior_result), reshape=T)))

#
#
# Climbing difficulty

# consider four parts:
# 1) concentration (toughest climb difficulty)
# 2) actual climb difficulty (sum of climb difficulty)
# 3) summit finish TRUE or FALSE
# 4) last climb (final climb difficulty)

stage_climb_difficulty <- stage_data_perf %>%
  
  select(stage, race, year, summit_finish, act_climb_difficulty = raw_climb_difficulty, last_climb, concentration) %>%
  
  unique() %>%
  
  gather(stat, value, -stage, -race, -year, -summit_finish) %>%
  
  group_by(stat) %>%
  mutate(sd = sd(value, na.rm = T),
         avg = mean(value, na.rm = T)) %>%
  ungroup() %>%
  
  # 20% are summit finishes, SD = 40%
  # average stage is 10 actual climb difficulty, SD = 11 (so TDF mountains are 2 sigmas)
  # average last climb is 3.5, sd = 4 (so cat 1 final climbs are 2 sigmas)
  # average toughest climb is 4.5, sd = 5 (so HC climbs are 2 sigmas)
  
  # calculate zscores and spread out
  mutate(zscr = (value - avg) / sd) %>%
  
  select(-value, -sd, -avg) %>%
  
  spread(stat, zscr) %>%
  
  # calculate ensemble score, scaling differently depending on whether it's a summit finish
  mutate(ensemble = ifelse(summit_finish == 1, (0.5 * last_climb) + (0.3 * act_climb_difficulty) + (0.2 * concentration),
                           (0.2 * last_climb) + (0.3 * act_climb_difficulty) + (0.5 * concentration)))

#
#
# combine into stage data

stage_data_perf <- stage_data_perf %>%
  
  inner_join(
    
    stage_climb_difficulty %>%
      select(stage, race, year, climb_score = ensemble), by = c("stage", "race", "year")
    
  )

#
#
# calculate max effort as function of climb score

climbing_records_model <- read_csv("climbing-records-2017-19.csv") %>%
  janitor::clean_names() %>%
  
  rename(length = km,
         gradient = grade) %>%
  
  lm(km_per_hr ~ length * gradient, data = .)

#

setup_stage_max_effort <- stage_data_perf %>%
  
  select(stage, race, year, concentration) %>%
  
  unique() %>%
  
    left_join(
      
      dbReadTable(con, "flamme_rouge_climbs") %>%
        mutate(year = as.numeric(year)) %>%
        
        mutate(race = tolower(race)) %>%
        
        left_join(dbReadTable(con, "flamme_rouge_characteristics") %>%
                    mutate(year = as.numeric(year)) %>%
                    select(race, stage, year, stage_length = length) %>%
                    mutate(race = tolower(race)), by = c("race", "stage", "year")) %>%
        
        group_by(stage, race, year) %>%
        mutate(position_highest = max(model_category, na.rm = T),
               last_climb = max(end_distance, na.rm = T)) %>%
        ungroup() %>%
        
        mutate(position_highest = ifelse(position_highest == model_category, end_distance / stage_length, NA),
               last_climb = ifelse(last_climb == end_distance, model_category, NA)) %>%
        
        mutate(summit_finish = ifelse(abs(end_distance - stage_length) < 4, TRUE, FALSE)) %>%
        mutate(summit_finish = ifelse(race == "tour de romandie" & stage == 4 &
                                        year == 2019 & climb_name == "Torgon", TRUE, summit_finish)) %>%
        
        # increase KOM points by 25% if summit finish
        mutate(basic_kom_points = model_category,
               kom_points = ifelse(summit_finish == TRUE, model_category * 1.25, model_category)) %>%
      
      group_by(stage, race, year) %>%
      filter(kom_points == max(kom_points, na.rm = T)) %>%
      ungroup(), by = c("stage", "race", "year")) %>%
  
  select(stage, race, year, concentration, length, gradient)

#

stage_max_effort <- cbind(
  
  setup_stage_max_effort,
  
  pred = predict(climbing_records_model, setup_stage_max_effort)
  
) %>%
  
  filter(!is.na(length)) %>%
  
  mutate(climbing_time = (length / pred) * 60) %>%
  
  select(-concentration) %>%
  
  inner_join(
    
    stage_climb_difficulty, by = c("stage", "race", "year")
    
  )

# stage max effort model (eg, translate climb_score to expected climbing time)
# eg, this stage has a 15 minute max effort

max_eff_mod <- lm(climbing_time ~ ensemble, data = stage_max_effort)

# re-combine with stage_data_perf

stage_data_perf <- stage_data_perf %>%
  
  left_join(
    
    cbind(
      
      stage_max_effort %>%
        select(-pred, -length, -gradient, -summit_finish, -act_climb_difficulty, -last_climb, -concentration),
      
      pred = predict(max_eff_mod, stage_max_effort)
      
    ) %>%
      select(stage, race, year, pred), by = c("stage", "race", "year")) %>%
  
  mutate(max_effort = ifelse(is.na(pred), 1, pred)) %>%
  
  select(-pred) %>%
  
  unique() %>%
  
  select(-gain_1st, -gain_3rd, -gain_5th, -gain_10th, -gain_20th, -gain_40th)

#
#
# rider climbing performance

library(brms)

in_contact <- stage_data_perf %>%
  
  mutate(rnk = ifelse(is.na(rnk), 150, rnk)) %>%
   
  mutate(W = ifelse(rnk == 1,1,0), 
         T3 = ifelse(rnk < 4,1,0),
         T5 = ifelse(rnk < 6,1,0),
         x2_5 = ifelse(rnk > 1 & rnk < 6,1,0),
         SUCC = success) %>% 
  
  group_by(rider) %>% 
  filter(n() > 49) %>%
  ungroup() %>%
  
  gather(category, result, (W:SUCC))

#

mod_gam <- brm(
  
  # does this model contain a random intercept for rider or just a random slope?
  bf(result ~ 
       s(max_effort, k = 5, by = rider) + 1),
  
  data = in_contact %>%
    filter(category == "SUCC"),
  
  family = bernoulli(),
  
  iter = 5000,
  warmup = 1000,
  chains = 1,
  cores = 1)



model <- lme4::glmer(result ~ (1 + max_effort | rider) + 1,
                      
                      family = binomial("logit"),
                      
                      data = in_contact %>%
                        filter(category == "SUCC"))

#

mod4 <- gamm4::gamm4(result ~ s(max_effort, k = 5) + 1,
                     
                     random = ~ (1 + max_effort | rider),
                     
                     family = binomial("logit"),
                     
                     data = in_contact %>%
                       filter(category == "SUCC"))

#

preds4 <- expand.grid(rider = ranef$rowname,
                      max_effort = c(0,10,20,30,40,50)) %>%
  
  cbind(
    
    coef = predict(mod4$gam,
            expand.grid(rider = ranef$rowname,
                        max_effort = c(0,10,20,30,40,50)))
    
  ) %>%
  
  mutate(pred = exp(coef)/(1+exp(coef)))

  
#

summary(mod_gam)
  
ranefs <- brms::ranef(mod_gam) %>%
  .[[1]] %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  janitor::clean_names() %>%
  mutate(prob = exp(estimate_intercept - 4.13)/(1+exp(estimate_intercept - 4.13)))
  

#

R = "Mollema Bauke"

feed_in <- stage_data_perf %>%
  
  mutate(rnk = ifelse(is.na(rnk), 150, rnk)) %>%
  
  mutate(W = ifelse(rnk == 1,1,0), 
         T3 = ifelse(rnk < 4,1,0),
         T5 = ifelse(rnk < 6,1,0),
         x2_5 = ifelse(rnk > 1 & rnk < 6,1,0)) %>% 
  
  group_by(rider) %>% 
  filter(n() > 99) %>%
  ungroup() %>%
  
  gather(category, result, W:x2_5) %>%
  
  filter(category == "T5" & rider == R) %>%
  
  inner_join(
    
    in_contact %>%
      filter(category == "T5" & rider == R) %>%
      spread(term, estimate) %>%
      janitor::clean_names(), by = c("rider", "category")
    
  ) %>%
  
  mutate(x = (log_max_effort * log(max_effort)) + (intercept),
         pred = exp(x)/(1+exp(x)))

#

ggplot() +
  
  geom_point(data = feed_in,
             aes(x = max_effort, y = result))+
  
  geom_point(data = feed_in,
             aes(x = max_effort, y = pred), color = "red")

#
#
#
#
#
#
# 

# Seconds gained/lost by success

seconds_by_position_mod <- stage_data_perf %>%
  
  mutate(pos_vs_succ = rnk - limit) %>%
  
  filter(!is.na(rnk)) %>%
  filter(pos_vs_succ < 200) %>%
  filter(rel_success > -5000) %>%
  
  lm(rel_success ~ pos_vs_succ + max_effort:pos_vs_succ + 0, data = .)

#

seconds_gained <- stage_data_perf %>%
  
  mutate(pos_vs_succ = rnk - limit) %>%

  filter(!is.na(rnk)) %>%
  filter(pos_vs_succ < 200) %>%
  filter(rel_success > -5000) %>%
  
  select(-variance, -top_variance, -rel_speed, -gain_1st, -gain_3rd,
         -gain_5th, -gain_10th, -gain_20th, -gain_40th) %>%
  
  cbind(
    
    pred = predict(seconds_by_position_mod,
                   
                   stage_data_perf %>%
                     
                     mutate(pos_vs_succ = rnk - limit) %>%

                     filter(!is.na(rnk)) %>%
                     filter(pos_vs_succ < 200) %>%
                     filter(rel_success > -5000))
    
  )

#
#
# seconds gained aggregate

seconds_gained_total <- seconds_gained %>%
  
  mutate(pred = ifelse(pred < 0, pred, 0)) %>%
  
  unique() %>%
  
  group_by(rider, year) %>%
  summarize(seconds_gained = mean(pred, na.rm = T),
            successes = sum(pred < 0, na.rm = T),
            races = n()) %>%
  ungroup() %>%
  
  mutate(succ_perc = successes / races)

#
#
#
#
#
#


# Rider vs Rider Model ----------------------------------------------------

rider_vs_rider_data <- stage_data %>%
  
  filter(year > 2016) %>%
  
  # replace DNFs with rnk > 5
  mutate(rnkx = ifelse(is.na(rnk), 100, rnk)) %>%
  
  mutate(time_trial = ifelse(str_detect(stage_name, "ITT") | stage_name == "Prologue", TRUE, FALSE),
         climbing = ifelse(raw_climb_difficulty > 15, TRUE, FALSE),
         grand_tour = ifelse(race %in% c("giro d'italia", "tour de france", "vuelta a espana"), TRUE, FALSE)) %>%
  
  filter(grand_tour == TRUE & (climbing == TRUE | time_trial == TRUE)) %>%
  
  group_by(rider, race, year) %>%
  mutate(med_tm_pos = median(tm_pos, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(rider) %>%
  #filter(mean(rnkx < 6, na.rm = T) > 0.05 & n() > 39 | mean(rnkx < 6, na.rm = T) > 0.1 & n() > 19) %>%
  filter(n() > 4 & sum(rnkx < 6, na.rm = T) > 0) %>%
  ungroup() %>%
  
  filter(!is.na(rnk)) %>%
  
  select(rider, total_seconds, race, stage, year, time_trial, climbing, grand_tour, med_tm_pos)

#

matched_comparison <- rider_vs_rider_data %>%
  
  inner_join(
    
    rider_vs_rider_data %>%
      select(-time_trial, -climbing, -grand_tour), by = c("stage", "race", "year")
    
  ) %>%
  
  filter(rider.x != rider.y) %>%
  
  mutate(diff = total_seconds.y > total_seconds.x,
         tm_pos_diff = med_tm_pos.y - med_tm_pos.x)

#

library(lme4)

# this model measures beating your opponents
binomial_model <- glmer(diff ~ (1 | rider.x) + (1 | rider.y) + tm_pos_diff, 
                              
                              family = "binomial",
                              
                              data = matched_comparison %>%
                                filter(grand_tour == TRUE))

summary(binomial_model)

# this method measures the percentage you're beating or losing to your opponents
ratio_model <- lmer(diff ~ (1 | rider.x) + (1 | rider.y) + tm_pos_diff, 
                    
                    data = matched_comparison %>%
                      filter(grand_tour == TRUE) %>%
                      mutate(diff = total_seconds.x - total_seconds.y,
                             diff = diff / total_seconds.x))

summary(ratio_model)

#

ranefs_binom <- ranef(binomial_model)[["rider.x"]] %>%
  rownames_to_column()

ranefs_time <- ranef(ratio_model)[["rider.x"]] %>%
  rownames_to_column()

#
#
# peak 2019

peak_season <- stage_data_perf %>%
  
  # replace DNFs with rnk > 5
  mutate(rnkx = ifelse(is.na(rnk), 100, rnk)) %>%
  
  mutate(time_trial = ifelse(str_detect(stage_name, "ITT") | stage_name == "Prologue", TRUE, FALSE),
         climbing = ifelse(raw_climb_difficulty > 24 | 
                             concentration > 6 | 
                             (concentration > 4 & raw_climb_difficulty > 15), TRUE, FALSE),
         grand_tour = ifelse(race %in% c("giro d'italia", "tour de france", "vuelta a espana"), TRUE, FALSE)) %>%
  
  filter((climbing == TRUE | time_trial == TRUE)) %>%
  
  group_by(rider, race, year) %>%
  mutate(med_tm_pos = median(tm_pos, na.rm = T)) %>%
  ungroup() %>%
  
  # join with sof for successes and those points are the points you earn for success
  inner_join(
    
    fields_success %>%
      select(stage, race, year, tot), by = c("stage", "race", "year")
    
  ) %>%
  
  # -5 is the stand-in for non-success as -4 is lowest successful
  # assign full points for 1st, half for 2nd, etc
  mutate(success_points = ifelse(success == 1, tot / rnk, -5)) %>%
  
  mutate(climbing = ifelse(time_trial == TRUE, FALSE, climbing),
         time_trial = ifelse(climbing == TRUE, FALSE, time_trial)) %>%
  
  group_by(rider, year, time_trial, climbing) %>%
  filter(rank(-success_points, ties.method = "first") < 4) %>%
  ungroup() %>%
  
  filter(!is.na(rnk)) %>%
  
  select(race, stage, year, rider, climbing, time_trial, success_points) %>%
  
  group_by(rider, year, time_trial, climbing) %>%
  summarize(points = sum(success_points, na.rm = T),
            stages = n()) %>%
  ungroup()

#

matched_peak <- peak_season %>%
  
  inner_join(
    
    peak_season %>%
      select(-time_trial, -climbing, -grand_tour), by = c("stage", "race", "year")
    
  ) %>%
  
  filter(rider.x != rider.y) %>%
  
  mutate(diff = total_seconds.y > total_seconds.x,
         tm_pos_diff = med_tm_pos.y - med_tm_pos.x)

#

# this model measures beating your opponents
binomial_p_model <- glmer(diff ~ (1 | rider.x) + (1 | rider.y) + tm_pos_diff, 
                        
                        family = "binomial",
                        
                        data = matched_peak)

summary(binomial_p_model)

# this method measures the percentage you're beating or losing to your opponents
ratio_p_model <- lmer(diff ~ (1 | rider.x) + (1 | rider.y) + tm_pos_diff, 
                    
                    data = matched_peak %>%
                      mutate(diff = total_seconds.x - total_seconds.y,
                             diff = diff / total_seconds.x))

summary(ratio_p_model)

#

ranefs_p_binom <- ranef(binomial_p_model)[["rider.x"]] %>%
  rownames_to_column()

ranefs_p_time <- ranef(ratio_p_model)[["rider.x"]] %>%
  rownames_to_column()

#
#
#
#
# Time lost to winner vs stage finish

time_lost_model <- lm(vs_winner ~ places_back + 0,
                      
                      data = stage_data %>%
                        
                        filter(raw_climb_difficulty > 24 | 
                                 concentration > 6 | 
                                 (concentration > 4 & raw_climb_difficulty > 15)) %>%
                        filter(year > 2016) %>%
                        
                        mutate(vs_winner = total_seconds - win_seconds,
                               places_back = rnk - 1) %>%
                        
                        filter(!is.na(rnk)) %>%
                        filter(places_back < 51)
                      
)

#

stage_data %>%
  
  filter(raw_climb_difficulty > 24 | 
           concentration > 6 | 
           (concentration > 4 & raw_climb_difficulty > 15)) %>%
  filter(year > 2016) %>%
  
  mutate(vs_winner = total_seconds - win_seconds,
         places_back = rnk - 1) %>%     filter(!is.na(rnk)) %>%
  filter(places_back < 51) %>% 
  
  ggplot(aes(x = places_back, y = vs_winner))+
  geom_point(alpha = 0.1)+
  geom_smooth(size = 1.5, color = "gold")+
  geom_smooth(size = 1.5, color = "red", method = "lm", formula = y ~ x + 0)+
  labs(x = "Places behind winner", y = "Seconds lost to winner", title = "Climbing stage separation")

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


# Metric Calculation for Sprint / Climb / TT ------------------------------

sprint_rankings <- stage_data_perf %>%
  
  mutate(x1 = ifelse(rnk == 1, total_seconds, NA),
         x50 = ifelse(rnk < 51, total_seconds, NA)) %>%
  
  group_by(stage, race, year) %>%
  mutate(best = min(x1, na.rm = T),
         x50th = max(x50, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(gap = (x50th - best)) %>%
  
  select(-best, -x50th, -x50, -x1) %>%
  
  # filtering to Gaviria, Sagan, Viviani, Groenewegen, Ewan, and Bennett
  # the average gap for their top 5 finishes is 33 seconds
  # 90% are inside 60 seconds from 1st to 50th
  filter(gap < 61) %>%
  
  mutate(date = as.Date(date))

#

dates <- stage_data_perf %>%
  
  filter(year > 2016) %>%
  filter(!is.na(date)) %>%
  select(date) %>%
  unique() %>%
  mutate(date = date + 1) %>%
  as.list() %>%
  .[[1]]

#

week_sprint_list <- vector("list", length(dates))

for(w in 1:length(dates)) {
  
  DATES <- as.Date(dates[[w]])
  
  df <- sprint_rankings %>%
    
    filter(date < DATES & date > (DATES - 731)) %>%
    
    mutate(weight = 1 / as.numeric((10 + (DATES - date)))) %>%
    
    group_by(rider) %>%
    summarize(raw_success = mean(success, na.rm = T),
              wt_success = sum(success * weight, na.rm = T) / sum(weight, na.rm = T),
              raw_win = mean(rnk == 1, na.rm = T),
              wt_win = sum((rnk == 1) * weight, na.rm = T) / sum(weight, na.rm = T),
              stages = n()) %>%
    ungroup() %>%
    
    mutate(daily_date = DATES)
  
  week_sprint_list[[w]] <- df
  
  print(w)
  
}

#
#
#

sprint_rankings_daily <- bind_rows(week_sprint_list) %>%
  
  # regress with 0.25 success in 7 results (3.6% success rate)
  mutate(regr_success = ((raw_success * stages) + 0.25) / (stages + 7),
         regr_wt_success = ((wt_success * stages) + 0.25) / (stages + 7),
         regr_win = ((raw_win * stages) + 0.25) / (stages + 35)) %>%
  
  select(-wt_win) %>%
  
  gather(metric, value, -rider, -daily_date, -stages) %>%
  
  group_by(metric, daily_date) %>%
  mutate(rank = rank(-value, ties.method = "min")) %>%
  ungroup()

#

dbWriteTable(con, "daily_sprint_rankings", sprint_rankings_daily %>%
               filter(metric %in% c("regr_win", "regr_success")), overwrite = TRUE, row.names = FALSE)

#
#
#

# Domestiques/Lead-out man rankings
# below almost certainly overrates team impact (eg, higher team coef for TT stages vs non-TT stages)

team_rider_performance <- stage_data %>%
  
  filter(!is.na(rnk)) %>%
  
  group_by(stage, race, year, team, master_team) %>%
  mutate(best = min(rnk, na.rm = T)) %>%
  ungroup() %>%
  
  #filter(rnk != 1) %>%
  
  filter(class %in% c("1.UWT", "2.UWT", "2.HC", "1.HC")) %>%
  
  mutate(team_win = ifelse(best == 1, 1, 0),
         leader = ifelse(rnk == best, 1, 0)) %>%
  
  filter(time_trial == 0) %>%
  
  lme4::glmer(team_win ~ (1 | master_team) + (1 | rider) + leader, family = "binomial", data = .)

#

riders <- lme4::ranef(team_rider_performance)[[1]] %>%
  rownames_to_column()

master_team <- lme4::ranef(team_rider_performance)[[2]] %>%
  rownames_to_column()


#
#
#
#
#

# Stage Similarity Data ---------------------------------------------------

stage_sim <- stage_data %>%
  filter(missing_profile_data == 0 & rnk == 1) %>%
  select(stage_name, stage, race, year, class, time_trial, grand_tour, one_day_race, 
         length, highest_point:summit_finish) %>%
  select(-perc_gain_end, -final_1km_elev, -final_5km_elev, -perc_elev_change,
         -total_elev_change) %>%
  filter(!(race %in% c("la poly normande"))) %>%
  filter(time_trial == 0)

pca_stages <- prcomp(stage_sim[, 9:24], scale = TRUE)

weightings_pca <- pca_stages$rotation

pc_s <- cbind(stage_sim[ ,1:8], pca_stages$x)

#PC1 is grand tour level mountain stages vs flat stages

#PC2 is summit finishes vs amstel / tour of flanders, lombardia, liege, san sebastian

#PC3 is colombia, tour of utah, and alpine stages vs amstel, fleche, liege, flanders, brabantse pijl

#
#
#
# Domestiques
#
#
#

domestique_rating <- stage_data %>%
  
  mutate(domestique_points = ifelse(tm_pos == 1, 2,
                                    ifelse(tm_pos == 2, 1,
                                           ifelse(tm_pos == 3, 0.33, 0)))) %>%
  
  mutate(domestique_points = ifelse(rnk < 31, domestique_points, 0)) %>%
  
  mutate(master_team = ifelse(is.na(master_team), team, master_team)) %>%
  
  group_by(rider, master_team, year) %>%
  summarize(rating = mean(domestique_points, na.rm = T),
            total_races = n()) %>%
  ungroup()

#
#
# RIDER SIMILARITY
#
#

rider_matches_sim <- stage_data %>%
  
  filter(rnk < 11) %>%
  
  group_by(rider) %>%
  filter(n() > 19) %>%
  ungroup() %>%
  
  select(rider) %>%
  unique() %>%
  
  inner_join(stage_data %>%
               filter(year > 2016) %>%
               select(race, stage, year, rider, rnk, total_seconds)) %>%
  
  inner_join(stage_data %>%
               
               filter(rnk < 11) %>%
               
               group_by(rider) %>%
               filter(n() > 19) %>%
               ungroup() %>%
               
               select(rider) %>%
               unique() %>%
               
               inner_join(stage_data %>%
                            filter(year > 2016) %>%
                            select(race, stage, year, rider, rnk, total_seconds)), by = c("race", "stage", "year")) %>%
  
  filter(!is.na(rnk.x)) %>%
  filter(!is.na(rnk.y)) %>%
  
  filter((rnk.x < 21 | rnk.y < 21) | (abs(total_seconds.x - total_seconds.y) > 29)) %>%
  
  filter(!(rider.x == rider.y)) %>%
  mutate(log_diff = (log(rnk.x) - log(rnk.y))^2,
         sqrt_time = sqrt(abs(total_seconds.x - total_seconds.y))) %>%
  
  group_by(rider1 = rider.x,
           rider2 = rider.y) %>%
  summarize(avg_log_square_diff = mean(log_diff, na.rm = T),
            avg_sqrt_seconds = mean(sqrt_time, na.rm = T),
            matches = n()) %>%
  ungroup() %>%
  
  filter(matches > 14) %>%
  
  mutate(value = (avg_log_square_diff - mean(avg_log_square_diff)) / sd(avg_log_square_diff)) %>%
  
  select(rider1,rider2,value) %>%
  
  spread(rider2, value) %>%
  gather(rider2, value, -rider1) %>%
  mutate(value = ifelse(is.na(value),0,value)) %>%
  spread(rider2, value)

# run principal components

prc <- prcomp(rider_matches_sim %>%
                select(-rider1))

prc_xxx <- cbind(rider_matches_sim %>%
                   select(rider1),
                 prc$x)

# kmeans set-up
factoextra::fviz_nbclust(scale(prc_xxx[,2:4]), kmeans, method = c("silhouette"))

km_list <- vector("list", 7)

for(k in 3:9) {
  
  km <- kmeans(scale(prc_xxx[, 2:4]), centers = k)
  
  km_xxx <- cbind(prc_xxx[ , 1:4],
                  cl = km$cluster) %>%
    as_tibble() %>%
    rename(rider = rider1) %>%
    inner_join(
      
      stage_data %>%
        filter(year >2016) %>%
        group_by(rider) %>%
        summarize(n_od = mean(rnk < 11 & one_day_race == TRUE, na.rm = T),
                  n_tt = mean(rnk < 11 & time_trial == TRUE, na.rm = T),
                  n_wt = mean(rnk < 11 & class %in% c("2.UWT", "1.UWT", "WC"), na.rm=T),
                  n_gt = mean(grand_tour == 1, na.rm = T),
                  n_fl = mean(rnk < 11 & !is.na(act_climb_difficulty) & act_climb_difficulty < 6, na.rm = T),
                  n_cl = mean(rnk < 11 & !is.na(act_climb_difficulty) & act_climb_difficulty > 24, na.rm = T),
                  n_hl = mean(rnk < 11 & !is.na(act_climb_difficulty) & act_climb_difficulty <24.1 & act_climb_difficulty>5.9, na.rm = T),
                  n = mean(rnk < 11, na.rm = T)) %>%
        ungroup(), by = c("rider")
      
    ) %>%
    
    mutate(clusters = k)
  
  km_list[[k]] <- km_xxx
  
}

all_kmeans <- bind_rows(km_list)

# plot kmeans rider plot

ggplot(all_kmeans %>%
         filter(clusters > 3), aes(x = PC3, y = PC2, color = as.factor(cl)))+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_point(size = 4)+
  geom_label(data = all_kmeans %>%
               filter(clusters > 3) %>%
               group_by(cl, clusters) %>%
               filter(rank(-n, ties.method = "first")<4) %>%
               ungroup() %>%
               ungroup(), aes(x = PC3, y = PC2, label = rider))+
  facet_wrap(~clusters)

# climb / flat ability of top 10
climb_flat <- stage_data %>% 
  filter(rnk<11) %>% 
  inner_join(all_kmeans %>% 
               filter(clusters == 8) %>%
               select(rider,PC2), by = c("rider")) %>%
  group_by(stage, race, year) %>% 
  summarize(m = mean(PC2, na.rm = T), 
            n = n()) %>% 
  mutate(m = (n*m)/(n+3)) %>%
  ungroup()

# correlation of success to PC2
corr_climb_flat <- stage_data %>%
  filter(!is.na(rnk)) %>%
  inner_join(all_kmeans %>% 
               filter(clusters == 8) %>%
               select(rider,PC2), by = c("rider")) %>%
  group_by(stage, race, year) %>%
  filter(n()>19) %>%
  do(broom::tidy(lm(log(rnk) ~ PC2, data = .))) %>%
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


# Climbing Performance ----------------------------------------------------

climbing_performance <- stage_data_perf %>%
  
  select(rnk, stage, race, year, tm_pos, rider, team, gain_1st, limit, success, sof) %>%
  
  inner_join(stage_types %>%
               mutate(date = as.Date(date)), by = c("stage", "race", "year")) %>%
  
  filter(year > 2016 & stage_taxonomy == "mountains") %>%
  
  arrange(rider, date) %>%
  
  group_by(rider) %>%
  mutate(prior_tm_pos = ((lag(tm_pos < 3,1))+(lag(tm_pos<3,2))+(lag(tm_pos<3,3))) / 3) %>%
  ungroup() %>%
  
  mutate(prior_tm_pos = ifelse(is.na(prior_tm_pos), 0.167, prior_tm_pos)) %>%
  
  left_join(daily_rankings, by = c("rider", "date", "stage_taxonomy")) %>%
  
  mutate(weighted = ifelse(is.na(weighted), 0, weighted),
         race_days_4m = ifelse(is.na(race_days_4m), 0, race_days_4m)) %>%
  
  # using rel_tm as denoted by leader_rating improves each of model AUCs by ~ 0.01
  
  group_by(team, race, stage, year) %>%
  mutate(skill_tm = sum(weighted, na.rm = T),
         tm = n()) %>%
  ungroup() %>%
  
  mutate(skill_tm = (skill_tm - weighted) / (tm - 1),
         skill_tm = ifelse(is.na(skill_tm), 0, skill_tm)) %>%
  select(-tm) %>%
  
  # adjust in stage
  group_by(race, stage, year) %>%
  mutate(pred_rnk = rank(-weighted, ties.method = "average"),
         race_days_rel = race_days_4m - mean(race_days_4m, na.rm = T),
         skill_tm = skill_tm - mean(skill_tm)) %>%
  ungroup() %>%
  
  mutate(top_3 = ifelse(pred_rnk < 4, gain_1st, NA)) %>%
  
  group_by(race, stage, year) %>%
  mutate(top_3 = mean(top_3, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(gain_top3 = gain_1st - top_3) %>%
  
  mutate(log_seconds = log10(gain_1st+1)) %>%
  
  group_by(rider) %>%
  mutate(rider_median = median(log_seconds, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(dnf_seconds = ifelse(rnk == 200, rider_median, NA),
         dnf_seconds = mean(dnf_seconds, na.rm = T),
         log_seconds = ifelse(rnk == 200, dnf_seconds, log_seconds)) %>%
  
  select(-rider_median, -dnf_seconds, -top_3) %>%
  
  mutate(specific_race = paste0(race,year))

#
#
#

rmse_list <- vector("list", 200)
pred_list <- vector("list", 200)
coef_list <- vector("list", 200)

for(x in 1:50) {

train_races <- climbing_performance %>%
  select(stage, race, year) %>%
  unique() %>%
  filter(percent_rank(runif(n())) < 0.8)

climb_mod <- lme4::lmer(log_seconds ~ (1 | rider) + 
                          sof + 
                          skill_tm + 
                          race_days_rel +
                          uphill_finish +
                          pred_climb_difficulty +
                          prior_tm_pos, 
                        
                        data = climbing_performance %>%
                          inner_join(train_races))

summary(climb_mod)

random_riders <- lme4::ranef(climb_mod)[[1]] %>% rownames_to_column()

preds_test <- cbind(
  
  climbing_performance %>%
    anti_join(train_races),
  
  pred = predict(climb_mod, climbing_performance %>%
                   anti_join(train_races), allow.new.levels = T)) %>%
  
  mutate(pred = 10 ^ pred,
         MSE = (gain_1st - pred)^2)

rmse_list[[x]] <- tibble(rmse = sqrt(mean(preds_test$MSE, na.rm = T)), it = x)
pred_list[[x]] <- preds_test %>% mutate(it = x)
coef_list[[x]] <- coef(climb_mod)[[1]] %>% rownames_to_column() %>% mutate(it = x)
#coef_list[[x]] <- coef(climb_mod) %>% enframe(name = NULL) %>% cbind(tibble(name = c("intercept","sof","skill_tm","weighted")))
  
}

#
#
#

p_rmse <- bind_rows(rmse_list)

p_pred <- bind_rows(pred_list) %>%
  
  group_by(rider) %>%
  summarize(pred = mean(pred, na.rm = T),
            n = n()) %>%
  ungroup()

p_coefs <- bind_rows(coef_list) %>%
  janitor::clean_names() %>%
  
  mutate(prob = 10^(intercept + (0.5 * sof) + (15 * pred_climb_difficulty) + (uphill_finish_true * 1))) %>%
  
  group_by(rider = rowname) %>%
  summarize(prob = mean(prob, na.rm = T)) %>%
  ungroup()

#

ggplot(input_into_perf_model %>% 
         mutate(rider = ifelse(rider == "Gaudu  David ", "Gaudu David", rider)) %>%
         filter(stage_taxonomy == "mountains" & year > 2016) %>%
         group_by(rider) %>% 
         summarize(success_points = mean(points_finish, na.rm = T), 
                   races = n()) %>% 
         ungroup() %>%
         inner_join(bind_rows(coef_list) %>%
                      janitor::clean_names() %>%
                      
                      mutate(prob = 10^(intercept + (0.5 * sof) + (15 * pred_climb_difficulty) + (uphill_finish_true * 1))) %>%
                      
                      group_by(rider = rowname) %>%
                      summarize(prob = mean(prob, na.rm = T)) %>%
                      ungroup()) %>%
         filter((success_points > 0.015 & prob < 181 & races > 9)), 
       aes(x = success_points, xend = 0, y = prob, yend = prob, label = rider))+
  geom_segment(alpha = 0.25)+
  geom_point(size = 2)+
  ggrepel::geom_label_repel()+
  labs(x = "Success points for high finishes",
       y = "average seconds lost per stage to winner",
       title = "Climbing performance (2017-20)")+
  scale_y_reverse(breaks = c(0,30,60,90,120,150,180))+
  scale_x_continuous(breaks = seq(0,0.2,0.02))+
  annotate(geom = "label", x = 0.1, y = 120, label = "Stage wins, but more time lost",
           color = "black", size = 5, fill = '#EECA4A')+
  annotate(geom = "label", x = 0.05, y = 30, label = "Less success, but less time lost",
           color = "black", size = 5, fill = '#EECA4A')

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

# Climbing performance including factoring in GC favorites

gc_results <- dbReadTable(con, "pcs_gc_results") %>%
  
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
         date = lubridate::dmy(date)) %>%
  unique() %>%
  
  filter(year > 2008)

#
#
#

R <- c("paris - nice", 'tirreno-adriatico', 'criterium du dauphine', 'tour de suisse',
                          'uae tour', 'volta ciclista a catalunya', 'itzulia basque country',
                          'vuelta ciclista al pais vasco', 'abu dhabi tour', 'vuelta al pais vasco',
                          'tour de la provence', 'giro del trentino', 'tour of the alps',
                          'tour des alpes maritimes et du var', 'volta a la comunitat valenciana',
                          'volta ao algarve em bicicleta', 'vuelta a andalucia ruta ciclista del sol',
                          "la route d'occitanie - la depeche du midi", "tour de l'ain",
                          'vuelta a burgos', "tour cycliste international du haut var",
                          'tour of oman', "la route d'occitanie", 'tour de romandie',
                          "amgen tour of california", "route du sud - la depeche du midi",
                          "giro del trentino-melinda", "tour de l\'ain")

GC_races <- stage_data_perf %>%
  
  filter(year > 2013) %>%
  filter(one_day_race == FALSE) %>%
  filter(race %in% R | grand_tour == TRUE) %>%
  filter(rnk == 1) %>%
  
  select(stage, race, year, date) %>%
  unique() %>%
  
  group_by(race, year) %>%
  summarize(start_date = min(date)) %>%
  ungroup()

gc_res_list <- vector("list", length(GC_races$race))

#

for(x in 1:length(gc_res_list)) {
  
  D <- as.Date(GC_races$start_date[[x]])
  
  gc_ratings <- gc_results %>%
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
  
  gc_res_list[[x]] <- gc_ratings %>%
    mutate(D = D,
           race = GC_races$race[[x]])
  
}

#

gc_ratings_going_into_race <- bind_rows(gc_res_list) %>%
  mutate(year = lubridate::year(D)) %>%
  inner_join(
    
    stage_data %>%
      filter(year > 2013) %>%
      filter(one_day_race == FALSE) %>%
      filter(race %in% R | grand_tour == TRUE) %>%
      select(race, year, rider, team) %>%
      unique(), by = c("rider", "year", "race")) %>%
  
  group_by(year, race) %>%
  mutate(rk = rank(-tot, ties.method = "min")) %>%
  ungroup() %>%
  
  mutate(best_ratio = ifelse(tot > 0, best / tot, NA)) %>%
  
  mutate(logtot = log(tot+1),
         logbest = log(best+1)) %>%
  
  group_by(race, year) %>%
  mutate(ztot = (logtot - mean(logtot, na.rm = T)) / sd(logtot, na.rm = T),
         zbest = (logbest - mean(logbest, na.rm = T)) / sd(logbest, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(zgc = (0.67 * ztot)+(0.33*zbest)) %>%
  
  group_by(race, year, team) %>%
  mutate(tm_gc_pts = tot / (sum(tot, na.rm = T))) %>%
  ungroup()

#
#
#

stage_by_stage_GC <- dbReadTable(con, "pcs_stage_by_stage_gc") %>%
  
  mutate(rider = str_to_title(tolower(rider)),
         gc_time = str_replace(gc_time, "//+", ""),
         gc_time = str_replace(gc_time, ",,", ""),
         old_gc_time = gc_time,
         gcn = nchar(gc_time)) %>% 
  
  mutate(duplic = ifelse(str_sub(gc_time, 1, floor(gcn/2)) == str_sub(gc_time, ceiling(gcn/2)+1, gcn), TRUE, FALSE),
         gc_time = ifelse(duplic == TRUE, str_sub(gc_time, 1, floor(gcn/2)), gc_time)) %>%
  
  separate(gc_time, into = c("hours","minutes", "seconds"), sep = ":") %>%
  mutate(h = is.na(seconds),
         seconds = ifelse(h == TRUE, minutes, seconds),
         minutes = ifelse(h == TRUE, hours, minutes),
         hours = ifelse(h == TRUE, 0, hours),
         seconds = as.numeric(seconds),
         minutes = as.numeric(minutes),
         hours = as.numeric(hours),
         total_seconds_back = (seconds + (minutes*60) + (hours*3600)),
         total_seconds_back = ifelse(gc_rnk == "1", 0, total_seconds_back)) %>%
  
  filter(!gc_rnk == "") %>%
  
  mutate(gc_rnk = as.numeric(gc_rnk)) %>%
  
  filter(!is.na(gc_rnk)) %>%
  
  group_by(race, year) %>%
  mutate(final = ifelse(max(stage, na.rm = T)==stage, gc_rnk, NA),
         stages_left = max(stage, na.rm = T)-stage) %>%
  ungroup() %>%
  
  group_by(rider, race, year) %>%
  mutate(final = mean(final, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(race, year) %>%
  mutate(final = ifelse(is.na(final) | final == "NaN", max(final, na.rm = T)+1, final)) %>%
  ungroup() %>%
  
  mutate(race = tolower(race)) %>%  
  
  left_join(
    
    stage_data_perf %>% 
      mutate(GC_stage = ifelse(time_trial == 1, 1, ifelse(pred_climb_difficulty > 7.99, 1, 0)),
             GC_stage = ifelse(is.na(GC_stage), 0, GC_stage)) %>%
      select(stage, race, year, GC_stage) %>%
      unique() %>%
      arrange(year, race, stage) %>%
      group_by(year, race) %>%
      mutate(GC_done = cumsum(GC_stage),
             GC_all = sum(GC_stage),
             GC_done = lag(GC_done)) %>%
      ungroup() %>%
      mutate(GC_done = ifelse(is.na(GC_done), 0, GC_done),
             percent_through = (GC_done / GC_all),
             percent_through = ifelse(GC_all == 0, 1, percent_through)), by = c("stage", "race", "year")
    
  ) %>%
  
  arrange(year, race, rider, stage) %>%
  mutate(percent_through = ifelse(race == "tour de france" & year == 2015 & stage == 9, lag(percent_through), percent_through)) %>%

  #mutate(percent_through = (1-(stages_left/stages))) %>%
  
  select(-GC_done, -GC_stage)

#
# gather stage by stage data, link with GC standings + GC ability ranks going into race
# model out win prob impact of those two numbers
# build naive probs to create new rankings of GC importance of each rider
#

GC_pred_data <- stage_data_perf %>%
  mutate(rider = str_to_title(tolower(rider))) %>%
  select(rnk, stage, race, year, tm_pos, rider, team, gain_1st, limit, success, sof, grand_tour, date) %>%
  
  inner_join(stage_data_perf %>%
               mutate(rider = str_to_title(tolower(rider))) %>%
               filter(rnk == 1 & !is.na(pred_climb_difficulty)) %>%
               
               filter((pred_climb_difficulty < 9 & concentration > 5) | (pred_climb_difficulty > 7.99)) %>%
               select(stage, race, year) %>%
               unique(), by = c("stage", "race", "year")) %>%
  
  filter(year > 2013) %>%
  
  inner_join(
    
    gc_ratings_going_into_race %>%
      mutate(rider = str_to_title(tolower(rider))) %>%
      select(race, year) %>%
      unique(), by = c("race", "year")
    
  ) %>%
  
  left_join(
    
    gc_ratings_going_into_race %>%
      mutate(rider = str_to_title(tolower(rider))) %>%
      select(rider, race, year, rk, gc_pts = tot, tm_gc_pts) %>%
      mutate(tm_gc_pts = ifelse(is.na(tm_gc_pts), 0, ifelse(tm_gc_pts == "NaN", 0, tm_gc_pts))), by = c("rider", "race", "year")
    
  ) %>%
  
  group_by(race, year) %>%
  mutate(rk = ifelse(is.na(rk), max(rk, na.rm = T), rk)) %>%
  ungroup() %>%
  
  mutate(rk = ifelse(rnk==200,NA,rk)) %>%
  
  left_join(
    
    rbind(
      stage_by_stage_GC %>%
        mutate(rider = str_to_title(tolower(rider))) %>%
        mutate(rider = ifelse(rider == "Gaudu  David ", "Gaudu David", rider)) %>%
        mutate(stage = stage+1) %>%
        select(gc_rnk, rider, race, year, stage, total_seconds_back, final, stages_left, GC_all, percent_through) %>%
        mutate(type = "in_prog"),
      stage_by_stage_GC %>%
        group_by(race,year) %>%
        filter(stage == min(stage)) %>%
        ungroup() %>%
        mutate(rider = str_to_title(tolower(rider))) %>%
        mutate(rider = ifelse(rider == "Gaudu  David ", "Gaudu David", rider)) %>%
        select(gc_rnk, rider, race, year, stage, total_seconds_back, final, stages_left, GC_all, percent_through) %>%
        mutate(type = "start")
    ) %>%
      arrange(rider, stage, race, year) %>%
      group_by(rider, race, year) %>%
      mutate(percent_through = lead(percent_through)) %>%
      ungroup(), by = c("rider", "race", "stage", "year")
    
  ) %>%
  
  mutate(best_fit = (percent_through * 0.6)+0.35,
         naive_prob = (best_fit * log10(gc_rnk)) + ((1-best_fit) * log10(rk)),
         naive_prob = ifelse(type == "start", ((1-best_fit) * log10(rk)), naive_prob),
         naive_prob = ifelse(percent_through == 0, (0.1 * log10(gc_rnk)) + ((0.9) * log10(rk)), naive_prob)) %>%
  
  mutate(old_rk = rk) %>%
  
  group_by(race, stage, year) %>%
  mutate(rk = rank(naive_prob, ties.method = "min"),
         old_rk = rank(old_rk, ties.method = "min")) %>%
  ungroup() %>%
  
  # where are you on team GC rank?
  group_by(race, stage, year, team) %>%
  mutate(tm_gc_pts = tm_gc_pts / sum(tm_gc_pts, na.rm = T)) %>%
  ungroup() %>%
  
  # what % of tot gc pts for rider
  group_by(race, stage, year) %>%
  mutate(proportion_gc_pts = gc_pts / sum(gc_pts, na.rm = T)) %>%
  ungroup() %>%
  
  # how many seconds back divided by GC stages left are you?
  mutate(GC_left = GC_all - (GC_all * percent_through),
         seconds_per_GC_stage_back = total_seconds_back / GC_left) %>%
  
  # log the ranks
  mutate(log_gc_rk = log10(gc_rnk+1),
         log_old_rk = log10(old_rk+1)) %>%
  
  # winner GC pts
  mutate(winner_gc_pts = ifelse(old_rk == 1, gc_pts, NA)) %>%
  group_by(race, year) %>%
  mutate(winner_gc_pts = max(winner_gc_pts, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(pts_to_winner = gc_pts - winner_gc_pts)

#
# GC time lost per GC stage by GC Ability Rk
#

GC_time_lost_model <- stage_data_perf %>%
  mutate(rider = str_to_title(tolower(rider))) %>%
  select(rnk, stage, race, year, tm_pos, rider, team, gain_1st, limit, success, sof, grand_tour, date) %>%
  
  inner_join(stage_data_perf %>%
               filter(rnk == 1) %>%
               group_by(race, year) %>%
               filter(stage == max(stage, na.rm = T)) %>%
               ungroup() %>%
               select(stage, race, year) %>%
               unique(), by = c("stage", "race", "year")) %>%
  
  filter(year > 2013) %>%
  
  inner_join(
    
    gc_ratings_going_into_race %>%
      mutate(rider = str_to_title(tolower(rider))) %>%
      select(race, year) %>%
      unique(), by = c("race", "year")
    
  ) %>%
  
  left_join(
    
    gc_ratings_going_into_race %>%
      mutate(rider = str_to_title(tolower(rider))) %>%
      select(rider, race, year, rk, gc_pts = tot, tm_gc_pts) %>%
      mutate(tm_gc_pts = ifelse(is.na(tm_gc_pts), 0, ifelse(tm_gc_pts == "NaN", 0, tm_gc_pts))), by = c("rider", "race", "year")
    
  ) %>%
  
  group_by(race, year) %>%
  mutate(rk = ifelse(is.na(rk), max(rk, na.rm = T), rk)) %>%
  ungroup() %>%
  
  mutate(rk = ifelse(rnk==200,NA,rk)) %>%
  
  left_join(
    
    rbind(
      stage_by_stage_GC %>%
        mutate(rider = str_to_title(tolower(rider))) %>%
        mutate(rider = ifelse(rider == "Gaudu  David ", "Gaudu David", rider)) %>%
        select(gc_rnk, rider, race, year, stage, total_seconds_back, final, stages_left, GC_all, percent_through) %>%
        mutate(type = "in_prog"),
      stage_by_stage_GC %>%
        group_by(race,year) %>%
        filter(stage == min(stage)) %>%
        ungroup() %>%
        mutate(rider = str_to_title(tolower(rider))) %>%
        mutate(rider = ifelse(rider == "Gaudu  David ", "Gaudu David", rider)) %>%
        select(gc_rnk, rider, race, year, stage, total_seconds_back, final, stages_left, GC_all, percent_through) %>%
        mutate(type = "start")
    ), by = c("rider", "race", "stage", "year")
    
  ) %>%
  
  mutate(old_rk = rk) %>%
  
  group_by(race, stage, year) %>%
  mutate(old_rk = rank(old_rk, ties.method = "min")) %>%
  ungroup() %>%
  
  # how many seconds back divided by GC stages left are you?
  mutate(seconds_per_GC_stage_back = total_seconds_back / GC_all) %>%
  
  # winner GC pts
  mutate(winner_gc_pts = ifelse(old_rk == 1, gc_pts, NA)) %>%
  group_by(race, year) %>%
  mutate(winner_gc_pts = max(winner_gc_pts, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(pts_to_winner = gc_pts - winner_gc_pts)

# generate log(x) ~ log(y) model
# R2 = 0.3 for log/log vs 0.2 for log/normal ------> with grand tour only
# R2 = 0.16 for log/log all races

log_log_mod <- lm(
  log(seconds_per_GC_stage_back+0.01) ~ 
    log(0.01+(pts_to_winner*-1)), 
  
  data = GC_time_lost_model %>% filter((gc_pts > 0 & !is.na(gc_pts) & grand_tour == 1)))

# generate predictions on all data

GC_time_lost_preds <- cbind(
  
  GC_pred_data,
  
  pred = predict(log_log_mod, GC_pred_data)) %>%
  
  mutate(predicted_seconds = exp(pred)) %>%
  
  select(rider, race, year, predicted_seconds) %>%
  unique()

#
#
#

# Model for Win Probability -----------------------------------------------

#
#
# take 75% of races into training set

train_races <- GC_pred_data %>%
  filter(GC_all > 3) %>%
  filter(type == 'in_prog') %>%
  filter(!is.na(gc_rnk)) %>%
  filter(!is.na(percent_through)) %>%
  select(race, year) %>%
  unique() %>%
  mutate(spec_race = paste0(race, year))

#
# XGB data
#

GC_mod_data <- GC_pred_data %>%
  
  filter(type == 'in_prog') %>%
  filter(!is.na(gc_rnk)) %>%
  filter(!is.na(percent_through)) %>%
  mutate(win = ifelse(final == 1, 1, 0)) %>%
  mutate(spec_race = paste0(race, year)) %>%
  
  inner_join(GC_time_lost_preds, by = c("race", "rider", "year")) %>%
  
  mutate(expected_GC_losses = GC_left * predicted_seconds,
         total_exp_seconds = expected_GC_losses + total_seconds_back)

# Current State of model
# I need to start eliminating people who are too far back
# eg, if you're already more seconds behind than predicted_seconds or 2x predicted seconds
# you get the eliminated = 1 marker (which will be highly predictive of NOT winning)
#
# also, a running performance vs expected where we compare where you should be based on GC_raced
# and where you are (which would catch Nibali TDF 2019 or Froome UAE Tour 2020)

xgb_mods <- vector("list", 100)
xgb_preds <- vector("list", 100)
xgb_imp <- vector("list", 100)

library(xgboost)

# run for 100 iterations

for(t in 1:100) {
  
  # generate random train set
  
  train_set <- sample(train_races$spec_race, 30)
  
  # XGBOOST
  
  xgb.train <- xgb.DMatrix(
    
    data = as.matrix(GC_mod_data %>%
                       filter(spec_race %in% train_set) %>%
                       select(#old_rk,
                         #log_old_rk,
                         GC_left,
                         seconds_per_GC_stage_back,
                         total_exp_seconds,
                         expected_GC_losses,
                         proportion_gc_pts,
                         #tm_gc_pts,
                         gc_pts
                       )),
    
    label = GC_mod_data %>%
      filter(spec_race %in% train_set) %>%
      .$win
    
  )
  
  # test
  
  xgb.test <- xgb.DMatrix(
    
    data = as.matrix(GC_mod_data %>%
                       filter(!spec_race %in% train_set) %>%
                       select(#old_rk,
                         #log_old_rk,
                         GC_left,
                         seconds_per_GC_stage_back,
                         total_exp_seconds,
                         expected_GC_losses,
                         proportion_gc_pts,
                         #tm_gc_pts,
                         gc_pts
                              )),
    
    label = GC_mod_data %>%
      filter(!spec_race %in% train_set) %>%
      .$win
    
  )
  
  # outline parameters
  
  params <- list(
    
    booster = "gbtree",
    eta = 0.3,
    max_depth = 4,
    gamma = 0.33,
    subsample = 0.8,
    colsample_bytree = 0.67,
    objective = "binary:logistic"
    
  )
  
  # run xgboost model
  
  gbm_w_model <- xgb.train(params = params,
                           data = xgb.train,
                           nrounds = 10000,
                           nthreads = 4,
                           early_stopping_rounds = 100,
                           eval_metric = "logloss",
                           watchlist = list(val1 = xgb.train,
                                            val2 = xgb.test),
                           verbose = 0)
  
  # this outputs GBM predictions for all data
  
  gbm_win_predict = cbind(
    
    GC_mod_data %>%
      filter(!spec_race %in% train_set),
    
    pred = predict(gbm_w_model, 
                   as.matrix(GC_mod_data %>%
                               filter(!spec_race %in% train_set) %>%
                               select(#old_rk,
                                 #log_old_rk,
                                 GC_left,
                                 seconds_per_GC_stage_back,
                                 total_exp_seconds,
                                 expected_GC_losses,
                                 proportion_gc_pts,
                                 #tm_gc_pts,
                                 gc_pts), reshape=T)))
  
  xgb_preds[[t]] <- gbm_win_predict %>%
    mutate(iter = t)
  
  xgb_mods[[t]] <- gbm_w_model$best_score %>%
    enframe(name = NULL) %>%
    mutate(iter = t)
  
  xgb_imp[[t]] <- xgb.importance(model = gbm_w_model) %>%
    mutate(iter = t)
  
}

# XG Boost calculation, feature importance, logloss

xgxgxg <- bind_rows(xgb_preds) %>%
  group_by(stage, race, year, rider, team,
           date, 
           old_rk,
           seconds_per_GC_stage_back,
           proportion_gc_pts,
           expected_GC_losses,
           total_seconds_back,
           tm_gc_pts,
           gc_pts,
           win, final) %>%
  summarize(pred = mean(pred, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(stage, race, year) %>%
  mutate(pred = ifelse(pred == min(pred, na.rm = T), 0, pred),
         pred = ifelse(pred < 0.0015, 0, pred)) %>%
  mutate(adj = pred / sum(pred, na.rm = T)) %>%
  ungroup()
  
# feature importance ranks things 1. pre-race GC, 2. current GC, 3. seconds/GC stage, 4. tm_gc_leader

feature_importance <- bind_rows(xgb_imp) %>%
  group_by(Feature) %>%
  summarize(Gain = mean(Gain, na.rm = T))

# model performance

bind_rows(xgb_mods) %>%
  summarize(mean(value))

#
#
#

xxx %>% 
  group_by(f = floor(win_prob / 0.05)*0.05) %>%
  summarize(rmse = sqrt(mean((win - win_prob)^2)),
            n = n()) %>%
  ungroup()

summary(lm(win ~ win_prob, xxx)) # SUMMARY LINEAR REGR

sqrt(mean((xxx$win - xxx$win_prob)^2)) # RMSE


#
#
#

preds <- cbind(pred = predict(secs_mod, GC_pred_data %>%
                                filter(GC_all >= 3) %>%
                                filter(type == 'in_prog') %>%
                                filter(!is.na(gc_rnk)) %>%
                                filter(!is.na(percent_through)) %>%
                                mutate(win = ifelse(final == 1, 1, 0))),
               GC_pred_data %>%
                 filter(GC_all >= 3) %>%
                 filter(type == 'in_prog') %>%
                 filter(!is.na(gc_rnk)) %>%
                 filter(!is.na(percent_through)) %>%
                 mutate(win = ifelse(final == 1, 1, 0))) %>%
  mutate(win_prob = exp(pred)/(1+exp(pred))) %>%
  
  group_by(stage, race, year) %>%
  mutate(win_prob = win_prob / sum(win_prob, na.rm = T)) %>%
  ungroup() %>%
  
  group_by(team, race, stage, year) %>%
  mutate(tm_gc_rk = rank(win_prob, ties.method = "min")) %>%
  ungroup() %>%
  
  group_by(f = floor(percent_through / 0.167)) %>%
  do(broom::tidy(glm(win ~ win_prob, data = ., family = "binomial"))) %>%
  ungroup()

# quick improvement on above model is to feed win prob into another model featuring
# the win prob rank on team... eg, when Bernal was up on Thomas going into stage 20 of TDF
# it wasn't 55-34 Bernal, it was probably more like 85%-4% Bernal
# 

#
# how good is naive prob ranking for GC importance at predicting races?
# 
#

win_prob_using_just_combined_rk <- GC_pred_data %>% 
  filter(GC_all > 3) %>%
  filter(type == "in_prog") %>%
  filter(!is.na(gc_rnk)) %>%
  filter(!is.na(percent_through)) %>%
  mutate(win = ifelse(final == 1, 1, 0)) %>%
  
  group_by(f = floor(percent_through / 0.167)) %>%
  do(broom::tidy(glm(win ~ rk, data = ., family = "binomial"))) %>%
  ungroup()

#
#
#

# in general these new rk using GC position + priors work great
# biggest limitation is in shorter races where a stage before GC competition starts might feature
# sprinters or big breakaways in the top 10
# this could be ameliorated by calculating percent_through as a percentage of GC relevant stages
# ITTs + mountains

climbing_performance_vs_GC <- GC_pred_data %>%  
  
  select(-naive_prob, -best_fit, -percent_through, -stages_left, -final, -type) %>%
  
  filter(!is.na(gc_rnk)) %>%
  
  group_by(team, stage, race, year) %>%
  mutate(tm_rk = rank(rk, ties.method = "min")) %>%
  mutate(rk = ifelse(tm_rk < 3, rk, NA)) %>%
  ungroup() %>%

  group_by(stage, race, year) %>%
  mutate(rk = rank(rk, ties.method = "min")) %>%
  ungroup() %>%

  mutate(GC_Group = ifelse(rk <= (limit * 1.5), 1, 0)) %>%
  
  mutate(gain_gc = ifelse(GC_Group == 1, gain_1st, NA)) %>%
  
  group_by(stage, race, year) %>%
  mutate(gain_gc50 = median(gain_gc, na.rm = T),
         gain_gc75 = quantile(gain_gc, probs = 0.25, na.rm = T)
         ) %>%
  ungroup() %>%
  
  mutate(gain_gc = (0.67 * gain_gc50)+(0.33 * gain_gc75)) %>%
  select(-gain_gc50, -gain_gc75) %>%
  mutate(gain_gc = gain_1st - gain_gc)

#
#
#

perf_vs_gc_by_race <- climbing_performance_vs_GC %>%
  
  mutate(gain_gc_no_brk = ifelse(gain_gc < -299, 0, gain_gc)) %>%
  
  group_by(rider, race, year, grand_tour) %>%
  summarize(mean = mean(gain_gc_no_brk, na.rm = T),
            minute_gains = mean(gain_gc < -59, na.rm = T),
            with_groups = sd(abs(gain_gc), na.rm = T),
            success = sum(success, na.rm = T),
            inGCgrp = mean(GC_Group, na.rm = T),
            stages = n()) %>%
  ungroup() %>%
  
  #filter(grand_tour == 1) %>%
  
  group_by(race, year) %>%
  filter(stages > (max(stages) / 2)) %>%
  ungroup() %>%
  
  group_by(race, year) %>%
  mutate(rk_in = rank(median, ties.method = "min")) %>%
  mutate(top_10 = ifelse(rk_in < 11, mean, NA)) %>%
  mutate(top_10 = mean(top_10, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(rel_t10 = mean - top_10)

#

climbing <- perf_vs_gc_by_race %>% 

  filter(year > 2013) %>%
  
  group_by(rider) %>% 
  summarize(perf = sum(rel_t10 * stages, na.rm = T) / sum(stages, na.rm = T), 
            with_groups = sum(with_groups * stages, na.rm = T) / sum(stages, na.rm = T), 
            inGCgrp = sum(inGCgrp * stages, na.rm = T) / sum(stages, na.rm = T),
            stages = sum(stages), 
            success = sum(success, na.rm = T),

            latest = max(year)) %>% 
  ungroup() %>%
  
  mutate(success = success / stages)
