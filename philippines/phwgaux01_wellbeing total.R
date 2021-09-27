ph_wellbeing <- readRDS(paste0(path_cohorts_gates_folder, "/Data/Data Analysis/Interim Analyses/Processed Data/R datasets/cohorts_wellbeing.rds")) %>% 
  dplyr::filter(country == "Philippines") %>% 
  dplyr::select(uncchdid,contains("happiness"))


ph_lifesat <- readRDS(paste0(path_cohorts_gates_folder, "/Data/Data Analysis/Interim Analyses/Processed Data/R datasets/cohorts_wellbeing.rds")) %>% 
  dplyr::filter(country == "Philippines") %>% 
  dplyr::select(uncchdid,contains("gen_life_sat"))
