early_life <- readRDS(paste0(path_harmonization_folder,"/south africa/working/early_life.RDS"))

adult_outcomes_eduyr <- readRDS(paste0(path_harmonization_folder,"/south africa/working/adultoutcomes.RDS")) %>% 
  dplyr::select(bttid,year,schooling) %>% 
  pivot_wider(names_from = "year",values_from="schooling",names_prefix = "eduyr") %>% 
  mutate(eduyr = case_when(is.na(eduyr2012) & !is.na(eduyr2018) ~ eduyr2018,
                           is.na(eduyr2018) & !is.na(eduyr2012) ~ eduyr2012,
                           !is.na(eduyr2012) & !is.na(eduyr2018) ~ eduyr2018,
                           TRUE ~ NA_real_
  )) %>% 
  dplyr::select(bttid,eduyr)

southafrica_dfa <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "soweto") %>% 
  dplyr::mutate(bttid = pin - 50000000) 

# ISSUE: This is for GATES wave
# BMI was measured at Y22
southafrica_pregnant <- readRDS(paste0(path_local_working,"/Processed Data/R datasets/cohorts_physical_health.RDS")) %>% 
  dplyr::filter(site == "BT20") %>% 
  dplyr::select(site_pin,currpregnant)
