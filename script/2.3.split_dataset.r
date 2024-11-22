rm(list=ls())
library(tidyverse)
library(stringr)
library(data.table)

ori_data <- read_rds("data/Adélie_data_18-19_EM/dive_phases_adelie_2018_19.rds")
ori_data$id <- str_remove(str_match(ori_data$tag_id, "F\\s*(.*?)\\s*_")[,1], "_")

sex_df <- fread("data/supp_olivia.csv")[,1:2] #Get sexes from Olivia Supplementary

ori_data <- left_join(ori_data, sex_df)
ori_data <- ori_data %>% filter(!is.na(sex)) #remove birds for which we do not have the sex for now 

#First we randomly sample half of the individuals for the RF, the other part, we keep it for the prediction afterwards + equilibrate sexes
all_m <- unique(ori_data$id[which(ori_data$sex == "M")])
all_f <- unique(ori_data$id[which(ori_data$sex == "F")])

id_m_unknow <- sample(all_m, length(all_m)*0.5, replace = FALSE)
id_f_unknow <- sample(all_f, length(all_f)*0.5, replace = FALSE)
id_unknown <- c(id_m_unknow, id_f_unknow)
id_rf <- unique(ori_data$id[which(!(ori_data$id %in% id_unknown))])

unknown_unbalanced <- ori_data %>% filter(id %in% id_unknown)
rf_unbalanced <- ori_data %>% filter(id %in% id_rf)

#Check if sexes are equilibrated
unknown_unbalanced %>%
  group_by(id, sex) %>%
  summarise() %>% 
  group_by(sex) %>% 
  summarise(n = n())

rf_unbalanced %>%
  group_by(id, sex) %>%
  summarise() %>% 
  group_by(sex) %>% 
  summarise(n = n())

#Save datasets
saveRDS(rf_unbalanced, "data/Adélie_data_18-19_EM/rf_unbalanced_for_train.rds")
#saveRDS(rf_balanced, "data/Adélie_data_18-19_EM/rf_balanced_500k.rds")
saveRDS(unknown_unbalanced, "data/Adélie_data_18-19_EM/unknown_unbalanced_for_prediction.rds")


