rm(list=ls())
library(tidyverse); library(cowplot); library(lubridate)
library(data.table); library(scattermore)

if(Sys.info()["sysname"] == "Linux"){
  my_root <- "/media/bdupuis/1091_PhD_BD/"
}else{ my_root <- "E:/"}

source(paste0(my_root,"Data/R_functions_toolbox.R"))

#Correct the 0m value of the depth####
depth_path <- paste0("data/Adélie_data_18-19_EM/raw")
depth_files <- list.files(depth_path, pattern = ".csv")

for (j in 1:length(depth_files)) {
  print(depth_files[j])
  print(paste0(j,"/", length(depth_files)))
  
  depth_data <- fread(paste0(depth_path,"/",depth_files[j]))
  
  depth_data$Timestamp <- dmy_hms(depth_data$Timestamp)
  
  tdr_data <- depth_data %>% filter(!is.na(depth))
  tdr_data$correc_depth <- clean0depth(tdr_data$depth)
  
  depth_data <- left_join(depth_data,tdr_data)
  
  depth_data$dive <- ifelse(depth_data$correc_depth >= 2, 1, 0) # Dive is 2M
  
  saveRDS(depth_data, paste0("data/Adélie_data_18-19_EM/corrected/", tools::file_path_sans_ext(depth_files[j]), ".rds"))
}
