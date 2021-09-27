adult_covariates <- readRDS(paste0(path_harmonization_folder,"/brazil 1993/working/adultoutcomes.RDS")) %>% 
  dplyr::filter(year %in% c(2011,2015)) %>% 
  dplyr::select(nquest,year,age,schooling,pregnant_anthro,children) %>% 
  pivot_wider(names_from = "year",values_from = c("age","schooling","pregnant_anthro","children"))
