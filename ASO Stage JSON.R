
library(tidyverse)
library(rjson)
library(rvest)

#

tdf_stage_list <- vector("list", 21)

for(i in 325:345) {
  
  d <- paste0('https://fep-api.dimensiondata.com/v2/stages/v2/', i, '/classification/stage') %>%
    readLines() %>%
    fromJSON()
  
  tdf_stage_list[[i - 324]] <- d
  
}

#

tidy_tdf_list <- vector("list", length(tdf_stage_list))

for(s in 1:length(tdf_stage_list)) {
  
  if(length(tdf_stage_list[[s]]$Mountain) == 0) {
    
    
  } else {
  
  kom <- tdf_stage_list[[s]]$Mountain
  
  spr <- tdf_stage_list[[s]]$Sprint
  
  gc <- tdf_stage_list[[s]]$General
  
  info <- tibble(
    
    stage_distance = tdf_stage_list[[s]]$StageDistance,
    stage = tdf_stage_list[[s]]$StageNumber

  )
  
  #clean_KOM
  
  climb_list <- vector("list", 10)
  
  for(m in 1:length(kom)) {
    
    rider_list <- vector("list", 20)
    
    for(r in 1:length(kom[[m]]$Riders)) {
      
      rider_list[[r]] <- tibble(
        
        bib = kom[[m]]$Riders[[r]]$Bib,
        name = kom[[m]]$Riders[[r]]$ShortName,
        pos = kom[[m]]$Riders[[r]]$Position,
        pts = kom[[m]]$Riders[[r]]$Points
        
      )
      
    }
    
    climb_list[[m]] <- tibble(
      
      climb = kom[[m]]$PointName,
      distance_left = kom[[m]]$DistanceToFinish,
      riders = length(kom[[m]]$Riders)) %>%
      
      cbind(
        
        bind_rows(rider_list)
        
      )
    
  }
  
  tidy_tdf_list[[s]] <- bind_rows(climb_list) %>%
    
    cbind(info)
  
  }
  
}

#

tdf_koms <- bind_rows(tidy_tdf_list)