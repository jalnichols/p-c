

library(tidyverse)
library(RMySQL)

s_con <- dbConnect(MySQL(),
                 host='localhost',
                 dbname='strava',
                 user='jalnichols',
                 password='braves')

con <- dbConnect(MySQL(),
                   host='localhost',
                   dbname='cycling',
                   user='jalnichols',
                   password='braves')

#

# I've set efficiency, gravity, weight of bike, CDA, Coef Rolling Resistance, Air Density below

all_riders <- dbGetQuery(s_con, "SELECT segment_name, distance_miles, elev_gain_feet, gradient, speed,
act.activity_name, act.Activity_date, professional_name, professional_id

FROM strava_segments seg

JOIN strava_activity_data act ON seg.activity_id = act.activity_id
                         
WHERE activity_date LIKE '%2019%'") %>%
  
  mutate(meters_gained = as.numeric(str_replace(str_replace(elev_gain_feet, "ft", ""), ",", ""))*0.3048,
         meters_gained = ifelse(str_sub(gradient,1,1)=="-", meters_gained * -1, meters_gained),
         distance_km = as.numeric(str_replace(distance_miles, "mi", ""))*1.609,
         m_s = as.numeric(str_replace(speed, "mi/h", ""))*1.609*1000/3600,
         km_h = m_s * 3600 / 1000,
         gradient = meters_gained / (distance_km * 1000)) %>%
  
  left_join(
    inner_join(
      dbGetQuery(con, "SELECT rider, height, weight FROM rider_attributes"), 
      read_csv("riders-strava-linked.csv") %>%
        mutate(pcs_name = iconv(pcs_name, from="UTF-8", to = "ASCII//TRANSLIT"),
               pcs_name = tolower(pcs_name)), by = c("rider"="pcs_name")), by = c("professional_name" = "name")) %>%
  
  mutate(weight = ifelse(is.na(weight), 65, weight)) %>%
  
  mutate(watts = ((1-0.025)^-1) * 
           (((9.8067) * sin(atan(gradient)) * (weight+8)) + 
              (9.8067 * cos(atan(gradient)) * (weight+8)*0.00325) + 
              (0.5*0.32*1.2*(m_s^2))) * 
           m_s,
         watts_per_kg = watts / weight)
         
