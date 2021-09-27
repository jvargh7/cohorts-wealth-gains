early_life <- readRDS(paste0(path_harmonization_folder,"/brazil 1993/working/early_life.RDS"))

# adult_outcomes <- readRDS(paste0(path_harmonization_folder,"/brazil 1993/working/adultoutcomes.RDS")) %>% 
#   dplyr::select(nquest,schooling,pregnant_anthro,children) %>% 
#   dplyr::filter(!is.na(schooling)) %>% 
#   mutate(children = case_when(children > 0 ~ 1,
#                               children == 0 ~ 0,
#                               TRUE ~ NA_real_))

adult_outcomes <- readRDS(paste0(path_harmonization_folder,"/brazil 1993/working/adultoutcomes.RDS")) %>% 
  dplyr::filter(year %in% c(2011,2015)) %>% 
  dplyr::select(nquest,schooling,iq,bmi,srq,wellbeing,pregnant_anthro,children) %>% 
  group_by(nquest) %>% 
  summarize_all(~mean(.,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(children = case_when(children > 0 ~ 1,
                              children == 0 ~ 0,
                              TRUE ~ NA_real_))


brazil1993_dfa <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "pelotas1993") %>% 
  mutate(nquest = b9numero + 930000)

