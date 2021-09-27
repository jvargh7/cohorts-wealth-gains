source(paste0(path_mobility_repo,"/guatemala/gtaux01_covariates.R"))
source(paste0(path_incap_repo,"/ses/ses_response status.R"))
source(paste0(path_replication_repo,"/gmethods/gmethods_functions.R"))

gains_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/gains_df.RDS"))
absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/absolute_df.RDS"))

participated2002 = absolute_df %>% 
  dplyr::filter(!is.na(bmi_ea)|!is.na(ravens_ea)) %>% 
  dplyr::select(id_uni) %>% 
  pull()
participated1996 = absolute_df %>% 
  dplyr::filter(!is.na(bmi_ya)) %>% 
  dplyr::select(id_uni) %>% 
  pull()


alive_df <- early_life %>% 
  dplyr::filter(id_uni %in% gains_df$id_uni) %>% 
  mutate(alive_ea = case_when(id_uni %in% alive2002 ~ 1,
                               TRUE ~ 0),
         alive_ya = case_when(id_uni %in% alive1996 ~ 1,
                               TRUE ~ 0),
         
         participated_ea = case_when(id_uni %in% participated2002 ~ 1,
                                    alive_ea == 1 ~ 0,
                                    alive_ea == 0 ~ NA_real_),
         participated_ya = case_when(id_uni %in% participated1996 ~ 1,
                                    alive_ya == 1 ~ 0,
                                    alive_ya == 0 ~ NA_real_)
         
         ) %>% 
  left_join(gains_df %>% 
              dplyr::select(id_uni,pcall6775_1),
            by="id_uni")

# Pr[Alive = 1] --------

rhs_formula_a = "~ chsex + moscho_imputed + moage_imputed + gtatole + gtchbyear + pcall6775_1"
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

saveRDS(alive_df,paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/alive_df.RDS"))

