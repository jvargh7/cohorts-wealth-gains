early_life <- readRDS(paste0(path_harmonization_folder,"/philippines/working/early_life.RDS"))

philippines_dfa <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "cebu") %>% 
  dplyr::mutate(uncchdid = pin - 40000000) %>% 
  mutate(adeduyr = case_when(is.na(adeduyr) ~ adeduyrc3,
                             TRUE ~ adeduyr)) %>% 
  dplyr::select(-adeduyrc3)



philippines_pregnant <- readRDS(paste0(path_local_working,"/Processed Data/R datasets/cohorts_physical_health.RDS")) %>% 
  dplyr::filter(site == "CEBU") %>% 
  dplyr::select(site_pin,currpregnant) %>% 
  rename(pregnant2018 = currpregnant)
