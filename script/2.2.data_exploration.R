rm(list=ls())

library(tidyverse); library(cowplot); library(lubridate)
library(ggthemes); library(viridis); library(scattermore)
library(ggplotlyExtra); library(zoo)

depth_path <- paste0("data/Adélie_data_18-19_EM/corrected")
depth_files <- list.files(depth_path, pattern = ".rds")

#Check my depth vs. Marianna ####
recap_cor <- tibble(id = NA, cor = NA, .rows = length(depth_files))

for (i in 1:length(depth_files)) {
  my_track <- read_rds(paste0(depth_path,"/", depth_files[i]))
  my_cor <- cor.test(my_track$correc_depth, my_track$depth25Hz)
  
  recap_cor$id[i] <- my_track$TagID[1]
  recap_cor$cor[i] <- my_cor$estimate
  print(paste0(i,"/", length(depth_files)))
}

print(min(recap_cor$cor))

#Global look of the data (low-res) ####
my_track <- read_rds(paste0(depth_path,"/", depth_files[length(depth_files)]))

vedba_track_plot <- ggplot(my_track, aes(x = Timestamp, y = VeDBA)) + 
  geom_scattermore() +
  theme_bw()

depth_track_plot <- ggplot(my_track, aes(x = Timestamp, y = correc_depth)) + 
  geom_scattermore() +
  theme_bw() +
  scale_y_reverse()

temp_track_plot <- ggplot(my_track, aes(x = Timestamp, y = Temp)) + 
  geom_scattermore() +
  theme_bw()

plot_grid(vedba_track_plot, depth_track_plot, temp_track_plot, ncol = 1)

#Then we focus on 1 dive to speed up the process ####
my_dive <- my_track %>% filter(Timestamp >= ymd_hms("2019-01-09 13:46:00") & Timestamp <= ymd_hms("2019-01-09 13:49:30"))

vedba_dive_plot <- ggplot(my_dive, aes(x = Timestamp, y = VeDBA, col = StatesNames, group = 1)) + 
  geom_line() +
  theme_bw()

depth_dive_plot <- ggplot(my_dive %>% filter(!is.na(correc_depth)), aes(x = Timestamp, y = correc_depth, col = StatesNames, group = 1)) + 
  geom_line() +
  theme_bw() +
  scale_y_reverse()

pitch_dive_plot <- ggplot(my_dive, aes(x = Timestamp, y = Pitch, col = StatesNames, group = 1)) + 
  geom_line() +
  theme_bw()

plot_grid(vedba_dive_plot, depth_dive_plot, ncol = 1)

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

hz_track$hunting <- ifelse(hz_track$prop_hunt <= 0.04 , "Other", "Hunting") #Consider hunting only if state lasted more than 1/25 of a second

hz_track$most_state <- my_track %>%
  group_by(x = ceiling(row_number()/25), TagID, StatesNames) %>%
  summarise(c=n()) %>% filter(row_number(desc(c))==1) %>%
  .$StatesNames 

#Sd of the vertical acceleration/speed over 5 sec
hz_track$sd_vert_accel <- rollapply(data = hz_track$vert_accel, width = 5, FUN = sd, fill = NA)
hz_track$sd_vert_speed <- rollapply(data = hz_track$vert_speed, width = 5, FUN = sd, fill = NA)

hz_dive <- hz_track[which(hz_track$Timestamp >= ymd_hms("2019-01-09 13:46:30") & hz_track$Timestamp <= ymd_hms("2019-01-09 13:49:45")),]

vert_speed_dive_plot <- ggplot(hz_dive, aes(x = Timestamp, y = vert_speed, col = hunting, group = 1)) + 
  geom_line() +
  geom_hline(yintercept = 0, alpha = 0.5, linetype = "dashed") +
  #scale_color_viridis_d(option = "B") +
  theme_bw() +
  labs(x = "", y = "Vertical speed (m.s-1)", col = "States", title = hz_dive$TagID[1])

vert_accel_dive_plot <- ggplot(hz_dive, aes(x = Timestamp, y = vert_accel, col = hunting, group = 1)) + 
  geom_line() +
  geom_hline(yintercept = 0, alpha = 0.5, linetype = "dashed") +
  #scale_color_viridis_d(option = "B") +
  theme_bw() +
  labs(x = "", y = "Vertical acceleration (m.s-2)", col = "States")

sd_vert_accel_dive_plot <- ggplot(hz_dive, aes(x = Timestamp, y = sd_vert_accel, col = hunting, group = 1)) + 
  geom_line() +
  #scale_color_viridis_d(option = "B") +
  theme_bw() +
  labs(x = "", y = "SD vertical acceleration", col = "States")

sd_vert_speed_dive_plot <- ggplot(hz_dive, aes(x = Timestamp, y = sd_vert_speed, col = hunting, group = 1)) + 
  geom_line() +
  #scale_color_viridis_d(option = "B") +
  theme_bw() +
  labs(x = "", y = "SD vertical speed", col = "States")

depth_dive_plot <- ggplot(hz_dive, aes(x = Timestamp, y = correc_depth, col = hunting, group = 1)) + 
  geom_line() +
  theme_bw() +
  #scale_color_viridis_d(option = "B") +
  scale_y_reverse() +
  labs(x = "Timestamp", y = "Depth (m)", col = "States")

plot_grid(vert_speed_dive_plot, vert_accel_dive_plot, depth_dive_plot, ncol = 1)
plot_grid(sd_vert_speed_dive_plot, sd_vert_accel_dive_plot, depth_dive_plot, ncol = 1)

ggplot(hz_track, aes(x = hunting, y = vert_speed)) +
  geom_violin()
  #coord_cartesian(ylim = c(-1,1))

no_dive <- hz_track %>% filter(dive != 0)

hist_vert_speed <- ggplot(no_dive, aes(fill = hunting, x = vert_speed)) +
  geom_histogram(position = "identity", alpha = 0.5) 

hist_vert_accel <- ggplot(no_dive, aes(fill = hunting, x = vert_accel)) +
  geom_histogram(position = "identity", alpha = 0.5) 

hist_sd_vert_speed <- ggplot(no_dive, aes(fill = hunting, x = sd_vert_speed)) +
  geom_histogram(position = "identity", alpha = 0.5) 

hist_sd_vert_accel <- ggplot(no_dive, aes(fill = hunting, x = sd_vert_accel)) +
  geom_histogram(position = "identity", alpha = 0.5) 

plot_grid(hist_vert_accel, hist_vert_speed, hist_sd_vert_accel, hist_sd_vert_speed, ncol = 2)


