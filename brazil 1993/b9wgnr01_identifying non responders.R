
source(paste0(path_mobility_repo,"/brazil 1993/b9aux01_covariates.R"))
source(paste0(path_replication_repo,"/gmethods/gmethods_functions.R"))
source(paste0(path_harmonization_repo,"/package/mode_impute.R"))

died2015_ids = read_dta(paste0(path_harmonization_folder,"/brazil 1993/dataset_2020_10_24.dta")) %>% 
  dplyr::filter(amostra22anos == 3) %>% 
  dplyr::select(nquest) %>% 
  pull()

died2011_ids = read_dta(paste0(path_harmonization_folder,"/brazil 1993/dataset_2020_10_24.dta")) %>% 
  dplyr::filter(amostra22anos == 3,data_obito <= "2012-01-01") %>% 
  dplyr::select(nquest) %>% 
  pull()

gains_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/gains_df.RDS"))
absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/absolute_df.RDS"))

participated2011_15 = absolute_df %>% 
  dplyr::filter(!is.na(bmi_ya)|!is.na(iq_ya)|!is.na(srq_ya)|!is.na(wellbeing_ya)) %>% 
  dplyr::select(nquest) %>% 
  pull()

alive_df <- early_life %>% 
  mutate(moemployedcurr = case_when(moemployedcurr %in% c("yes","leaning",
                                                          "student") ~ 1,
                                    moemployedcurr %in% c("no","retired") ~ 0,
                                    TRUE ~ NA_real_),
         moskincolor = case_when(moskincolor %in% c("white") ~ 1,
                                 moskincolor %in% c("black","other") ~ 0,
                                 TRUE ~ NA_real_)) %>% 
  mutate_at(vars(moage,moscho,
                 moemployedcurr,moskincolor),~case_when(is.na(.) ~ Mode(.),
                                                               TRUE ~ .)) %>% 

  mutate(
         alive_ya = case_when(nquest %in% unique(c(died2015_ids)) ~ 0,
                              TRUE ~ 1),
         
         participated_ya = case_when(nquest %in% participated2011_15 ~ 1,
                                     alive_ya == 1 ~ 0,
                                     alive_ya == 0 ~ NA_real_)
  )

# Pr[Alive = 1] --------
#  
rhs_formula_a = "~ chsex + moscho + moage + chbirtho + moemployedcurr + moskincolor"
alive_df$c_alive_ya = censoring_weights(c_formula = paste0("alive_ya", rhs_formula_a),
                                        df = alive_df,
                                        type = "glm")


alive_df$c_participated_ya = NA_real_

# Pr[Particiapted in wave = 1| Alive = 1] --------
alive_df[!is.na(alive_df$participated_ya),]$c_participated_ya = censoring_weights(c_formula = paste0("participated_ya", rhs_formula_a),
                                                                                  df = alive_df,
                                                                                  type = "glm")

saveRDS(alive_df,paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/alive_df.RDS"))




