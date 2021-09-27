source(paste0(path_mobility_repo,"/philippines/phaux01_covariates.R"))
source(paste0(path_replication_repo,"/gmethods/gmethods_functions.R"))
source(paste0(path_harmonization_repo,"/package/mode_impute.R"))

died2005_ids = haven::read_dta(paste0(path_harmonization_folder,"/philippines/ic masterlist/IC masterlist bi to 2009 2013_2_5 without identifiers.dta")) %>% 
  dplyr::filter(survey == 2005) %>% 
  dplyr::filter(uncchdid < 23310)%>% 
  dplyr::filter(crnoint == 10) %>% 
  dplyr::select(uncchdid) %>% 
  pull()

died2009_ids = haven::read_dta(paste0(path_harmonization_folder,"/philippines/ic masterlist/IC masterlist bi to 2009 2013_2_5 without identifiers.dta")) %>% 
  dplyr::filter(survey == 2009) %>% 
  dplyr::filter(uncchdid < 23310) %>% 
  dplyr::filter(crnoint == 10) %>% 
  dplyr::select(uncchdid) %>% 
  pull()

died2018_ids = haven::read_dta(paste0(path_harmonization_folder,"/philippines/2018/interviewstat-2018.dta")) %>% 
  dplyr::filter(intstat2018 == "died") %>% 
  dplyr::select(uncchdid) %>% 
  pull()

gains_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/gains_df.RDS"))
absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/absolute_df.RDS"))

# cstratum
# 1	Urban
# 2	Rural
ph_region <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/ic masterlist/IC masterlist bi to 2009 2013_2_5 without identifiers.dta")) %>% 
  dplyr::filter(survey %in% c(0.5)) %>% 
  dplyr::filter(!is.na(cstratum)) %>% 
  dplyr::select(uncchdid,cstratum)

ses_cs <- readRDS(paste0(path_harmonization_folder,"/philippines/working/pca_region.RDS")) %>% 
  dplyr::select(uncchdid,year,pc) %>% 
  pivot_wider(names_from="year",values_from="pc",names_prefix = "pc") %>% 
  dplyr::select(uncchdid,pc1983) %>% 
  left_join(ph_region %>% 
              rename(cstratum1983 = cstratum),
            by = "uncchdid")

participated2018 = absolute_df %>% 
  dplyr::filter(!is.na(bmi_ea)|!is.na(ravens_ea)|!is.na(srq_ea)|!is.na(happiness_ea)) %>% 
  dplyr::select(uncchdid) %>% 
  pull()
participated2005_09 = absolute_df %>% 
  dplyr::filter(!is.na(bmi_ya)|!is.na(stress_ya)) %>% 
  dplyr::select(uncchdid) %>% 
  pull()


alive_df <- early_life %>% 
  dplyr::filter(uncchdid %in% gains_df$uncchdid) %>% 
  left_join(ses_cs,by = "uncchdid") %>% 
  mutate_at(vars(cstratum1983,pc1983),~case_when(is.na(.) ~ Mode(.),
                                                 TRUE ~ .)) %>% 
  mutate(alive_ea = case_when(uncchdid %in% died2018_ids ~ 0,
                              TRUE ~ 1),
         alive_ya = case_when(uncchdid %in% unique(c(died2009_ids,
                                                     died2005_ids)) ~ 0,
                              TRUE ~ 1),
         
         participated_ea = case_when(uncchdid %in% participated2018 ~ 1,
                                     alive_ea == 1 ~ 0,
                                     alive_ea == 0 ~ NA_real_),
         participated_ya = case_when(uncchdid %in% participated2005_09 ~ 1,
                                     alive_ya == 1 ~ 0,
                                     alive_ya == 0 ~ NA_real_)
         
  )

# Pr[Alive = 1] --------

rhs_formula_a = "~ chsex + moscho + moage + pc1983 + cstratum1983 + chbirtho"
alive_df$c_alive_ea = censoring_weights(c_formula = paste0("alive_ea", rhs_formula_a),
                                        df = alive_df,
                                        type = "glm")
alive_df$c_alive_ya = censoring_weights(c_formula = paste0("alive_ya", rhs_formula_a),
                                        df = alive_df,
                                        type = "glm")


alive_df$c_participated_ea = alive_df$c_participated_ya = NA_real_

# Pr[Particiapted in wave = 1| Alive = 1] --------
alive_df[!is.na(alive_df$participated_ea),]$c_participated_ea = censoring_weights(c_formula = paste0("participated_ea", rhs_formula_a),
                                                                                  df = alive_df,
                                                                                  type = "glm")
alive_df[!is.na(alive_df$participated_ya),]$c_participated_ya = censoring_weights(c_formula = paste0("participated_ya", rhs_formula_a),
                                                                                  df = alive_df,
                                                                                  type = "glm")

saveRDS(alive_df,paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/alive_df.RDS"))

