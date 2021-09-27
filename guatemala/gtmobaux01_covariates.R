early_life <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/early_life.RDS"))

guatemala_dfa <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "guatemala") %>% 
  dplyr::mutate(id_uni = pin - 20000000) 


pregnant_anthro <- readRDS(paste0(path_gtml_earlier_data,"/bmi over waves.RDS")) %>% 
  dplyr::filter(year == 2016) %>% 
  dplyr::select(iduni,physiological_state)
