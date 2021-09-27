sa_wellbeing <- readRDS(paste0(path_cohorts_gates_folder, "/Data/Data Analysis/Interim Analyses/Processed Data/R datasets/cohorts_wellbeing.rds")) %>% 
  dplyr::filter(country == "South Africa") %>% 
  dplyr::select(bttid,contains("happiness"))
