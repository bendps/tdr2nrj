rm(list=ls())

library(tidyverse); library(cowplot); library(lubridate)
library(ggthemes); library(viridis); library(scattermore)
library(ggplotlyExtra); library(zoo)

depth_path <- paste0("data/Adélie_data_18-19_EM/corrected")
depth_files <- list.files(depth_path, pattern = ".rds")

my_global <- tibble()
for(my_file in depth_files){
  my_track <- read_rds(paste0(depth_path,"/", my_file))
  
  #Calculate some extra parameters ####
  #(define bottom phase, wiggles, vertical speed and acceleration)
  #and most abundant behavior or hunting/not and it's proportion).
  
  my_track$hunt <- ifelse(my_track$StatesNames == "Hunting", 1, 0) #for future proportion of hunting
  
  hz_track <- my_track %>% group_by(x = ceiling(row_number()/25), TagID) %>% #Go from 25Hz to 1Hz
    summarise(Timestamp = min(Timestamp), prop_hunt = sum(hunt)/n(), correc_depth = max(correc_depth, na.rm = T),
              dive = max(dive, na.rm = T)) #Variables of interest
  
  #Create a vector to have the depth of t-1 in the same row to calculate the difference (vertical speed)
  prev_depth <- c(NA, hz_track$correc_depth)
  hz_track$prev_depth <- prev_depth[-length(prev_depth)]
  
  hz_track <- hz_track %>% mutate(vert_speed = correc_depth - prev_depth) #Calculate vertical speed
  
  #Same but the vertical acceleration
  prev_change <- c(NA, hz_track$vert_speed)
  hz_track$prev_change <- prev_change[-length(prev_change)]
  
  hz_track <- hz_track %>% mutate(vert_accel = vert_speed - prev_change) #Calculate vertical acceleration
  
  hz_track$hunting <- ifelse(hz_track$prop_hunt <= 0.5 , "Other", "Hunting") #Consider hunting only if state lasted more than half of a second
  
  hz_track$most_state <- my_track %>%
    group_by(x = ceiling(row_number()/25), TagID, StatesNames) %>%
    summarise(c=n()) %>% filter(row_number(desc(c))==1) %>%
    .$StatesNames 
  
  #Sd of the vertical acceleration/speed over 5 sec
  hz_track$sd_vert_accel <- rollapply(data = hz_track$vert_accel, width = 5, FUN = sd, fill = NA)
  hz_track$sd_vert_speed <- rollapply(data = hz_track$vert_speed, width = 5, FUN = sd, fill = NA)
  hz_track$mean_vert_accel <- rollapply(data = hz_track$vert_accel, width = 5, FUN = mean, fill = NA)
  hz_track$mean_vert_speed <- rollapply(data = hz_track$vert_speed, width = 5, FUN = mean, fill = NA)
  
  hz_track$trip_ID <- substr(my_file, 1, nchar(my_file)-4) #Add trip ID
  
  my_global <- rbind(my_global, hz_track)
  print(my_file)
}

no_dive <- my_global %>% filter(dive != 0) #Keep only while diving
no_dive$consecutive <- c(1,ifelse(diff(no_dive$Timestamp) == 1, 0, 1)) #To mark individual dives
no_dive$bouts <- cumsum(no_dive$consecutive) #Label bouts using cumulative sum

#Add max depth for each bouts
max_depth_df <- no_dive %>% group_by(bouts) %>% summarise(max_dive_depth = max(correc_depth), duration = n())
hist(max_depth_df$duration)
no_dive <- merge(no_dive, max_depth_df, by = "bouts")

no_dive$bottom_phase <- no_dive$correc_depth >= 0.8*no_dive$max_dive_depth

#Label phases
no_dive$phases <- ifelse(no_dive$bottom_phase, "Bottom", NA)
no_dive$index <- 1:nrow(no_dive)
bouts_limits <- tibble(beg = match(unique(no_dive$bouts),no_dive$bouts),
                       end = length(no_dive$bouts)-match(unique(no_dive$bouts),rev(no_dive$bouts))+1)
bouts_limits$bouts <- 1:nrow(bouts_limits)

for(ii in 1:max(no_dive$bouts)){
  temp_dive <- no_dive %>% filter(bouts == ii)
  if(nrow(temp_dive) >= 3){ #Deal with short dive around 2m lasting 2 sec 
    beg_bout <- bouts_limits$beg[which(bouts_limits$bouts == ii)]
    end_bout <- bouts_limits$end[which(bouts_limits$bouts == ii)]
    
    #First and last occurrence of phases 
    my_first <- temp_dive %>% group_by(phases) %>% summarise(index = min(index))
    my_last <- temp_dive %>% group_by(phases) %>% summarise(index = max(index))
    
    beg_bottom <- my_first$index[which(my_first$phases == "Bottom")]
    end_bottom <- my_last$index[which(my_first$phases == "Bottom")]
    
    
    no_dive$phases[beg_bout:(beg_bottom-1)] <- "Descent" #Everything before is descent phase
    no_dive$phases[(end_bottom+1):end_bout] <- "Ascent" #After the bottom phase its ascent
    
    #Also add extra label to identify the large wiggles inside the boundary of bottom phase
    no_phases <- temp_dive %>%
      filter(index >= beg_bottom, index <= end_bottom) %>%
      filter(is.na(phases))
    if(nrow(no_phases) > 0){
      no_dive$phases[no_phases$index] <- "OOBP"
    }
  }
  print(paste0(ii,"/", max(no_dive$bouts)))
}

no_dive <- no_dive %>% filter(!is.na(phases))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Format variables names
no_dive <- no_dive %>%
  rename(dive_id = bouts, row_index = x, tag_id = TagID, timestamp = Timestamp,
                                  hunt_proportion = prop_hunt, depth = correc_depth, track_index = dive,
                                  prev_vert_speed = prev_change, state = hunting, roll_mean_vert_accel = mean_vert_accel,
                                  roll_mean_vert_speed = mean_vert_speed, roll_sd_vert_accel = sd_vert_accel,
                                  roll_sd_vert_speed = sd_vert_speed, trip_id = trip_ID, state_change = consecutive,
                                  dive_duration = duration, phase = phases) %>%
  relocate(tag_id, trip_id, timestamp, track_index, max_dive_depth, dive_duration,
                                dive_id, state, most_state, hunt_proportion,
                                depth, vert_speed, roll_mean_vert_speed, roll_sd_vert_speed,
                                vert_accel, roll_mean_vert_accel, roll_sd_vert_accel,
                                phase) %>%
  select(-c(index, row_index, prev_depth, bottom_phase, prev_vert_speed))

no_dive$phase[which(no_dive$phase == "OOBP")] <- "Bottom"

#Correct if some files started underwater
dive_to_remove <- unique(no_dive$dive_id[which(is.na(no_dive$roll_mean_vert_accel))])
no_dive <- no_dive %>% filter(!dive_id %in% dive_to_remove)

saveRDS(no_dive, "data/Adélie_data_18-19_EM/dive_phases_adelie_2018_19.rds")
