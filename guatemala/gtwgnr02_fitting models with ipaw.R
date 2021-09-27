
absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/absolute_df.RDS"))
cwealth <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/cwealth.RDS"))
mi_dfs <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/mi_dfs.RDS"))

formula_bmi_ea = "bmi_ea ~ wealth_child + cwealth_teen + cwealth_ya + cwealth_ea + moscho_sib + moage + gtatole*full + chbirtho + byear + male + eduyr + rural_ea + formal_ea"
formula_bmi_ya = "bmi_ya ~ wealth_child + cwealth_teen + cwealth_ya + moscho_sib + moage + gtatole*full + chbirtho + byear + male + eduyr + rural_ya"  # formal_ya
formula_ravens_ea = "ravens_ea ~ wealth_child + cwealth_teen + cwealth_ya + cwealth_ea + moscho_sib + moage + gtatole*full + chbirtho + byear + male + eduyr + rural_ea + formal_ea"
source(paste0(path_replication_repo,"/gmethods/gmethods_functions.R"))

library(MASS)

models_bmi_ea <- list()
models_bmi_ya <- list()
models_ravens_ea <- list()

# i = 4
for (i in 1:mi_dfs$m){
  
  # cat("\n Iteration ",i)
  
  model_df = absolute_df %>% 
    mutate(cwealth_teen = cwealth %>% dplyr::filter(lifestage=="teen") %>% dplyr::select(paste0("V",i)) %>% pull(),
           cwealth_ya = cwealth %>% dplyr::filter(lifestage=="ya") %>%  dplyr::select(paste0("V",i)) %>% pull(),
           cwealth_ea = cwealth %>% dplyr::filter(lifestage=="ea") %>% dplyr::select(paste0("V",i)) %>% pull(),
           moscho_sib = complete(mi_dfs,i)$moscho_sib,
           formal_ea = complete(mi_dfs,i)$formal_ea,
           formal_ya = complete(mi_dfs,i)$formal_ya,
           moage = complete(mi_dfs,i)$moage,
           chbirtho = complete(mi_dfs,i)$chbirtho,
           eduyr = complete(mi_dfs,i)$eduyr,
           rural = complete(mi_dfs,i)$rural
    ) %>% 
    mutate(ravens_ea = scale(ravens_ea))
  
  # Implementing IPAW procedure -----------
  alive_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/alive_df.RDS"))
  
  ipaw_model_df <- model_df %>% 
    left_join(alive_df %>% 
                dplyr::select(id_uni,alive_ea,alive_ya,
                              participated_ea,participated_ya,
                              c_alive_ea,c_alive_ya,
                              c_participated_ea,c_participated_ya),
              by = "id_uni") %>% 
    mutate(na_bmi_ya = case_when(!is.na(bmi_ya) ~ 1,
                                 participated_ya == 1 ~ 0,
                                 TRUE ~ NA_real_),
           na_bmi_ea = case_when(!is.na(bmi_ea) ~ 1,
                                 participated_ea == 1 ~ 0,
                                 TRUE ~ NA_real_),
           na_ravens_ea = case_when(!is.na(ravens_ea) ~ 1,
                                 participated_ea == 1 ~ 0,
                                 TRUE ~ NA_real_)) %>% 
    
    mutate(response_bmi_ya = 1, # Since this was the only outcome collected in YA
           response_bmi_ea = 1,
           response_ravens_ea = 1)
  rhs_formula_o = "~ male + moscho_sib + moage + gtatole + byear + wealth_child + chbirtho"
  
  # Pr[Response = 1| Alive = 1, Particiapted in wave = 1] ----
  
  if(mean(ipaw_model_df$na_bmi_ya,na.rm=TRUE) > 0.05 &
     mean(ipaw_model_df$na_bmi_ya,na.rm=TRUE) < 0.95) {
    ipaw_model_df[!is.na(ipaw_model_df$na_bmi_ya),]$response_bmi_ya <-
      censoring_weights(c_formula = paste0("na_bmi_ya",
                                           rhs_formula_o),
                        df = ipaw_model_df,
                        type = "glm")
  }
    
  
  if(mean(ipaw_model_df$na_bmi_ea,na.rm=TRUE) > 0.05 &
     mean(ipaw_model_df$na_bmi_ea,na.rm=TRUE) < 0.95) {
    ipaw_model_df[!is.na(ipaw_model_df$na_bmi_ea),]$response_bmi_ea <- 
      censoring_weights(c_formula = paste0("na_bmi_ea", 
                                           rhs_formula_o),
                        df = ipaw_model_df,
                        type = "glm")
  }
  
  
  if(mean(ipaw_model_df$na_ravens_ea,na.rm=TRUE) > 0.05 & 
     mean(ipaw_model_df$na_ravens_ea,na.rm=TRUE) < 0.95) {
    ipaw_model_df[!is.na(ipaw_model_df$na_ravens_ea),]$response_ravens_ea <- 
      censoring_weights(c_formula = paste0("na_ravens_ea", 
                                           rhs_formula_o),
                        df = ipaw_model_df,
                        type = "glm")
  }
  

  glm_bmi_ea <- MASS::rlm(as.formula(paste0(formula_bmi_ea)),
                   weights = c_alive_ea*c_participated_ea*response_bmi_ea,
                   wt.method = "case",
                   data = ipaw_model_df %>% 
                     mutate(bmi_ea = case_when(pregnant_ea == 1 ~ NA_real_,
                                               TRUE ~ bmi_ea)))

  glm_bmi_ya <- MASS::rlm(as.formula(paste0(formula_bmi_ya)),
                   weights = c_alive_ya*c_participated_ya*response_bmi_ya,
                   wt.method = "case",
                   data = ipaw_model_df %>% 
                     mutate(bmi_ya = case_when(pregnant_ya == 1 ~ NA_real_,
                                               TRUE ~ bmi_ya))
  )
  
  glm_ravens_ea <- MASS::rlm(as.formula(paste0(formula_ravens_ea)),
                      weights = c_alive_ea*c_participated_ea*response_ravens_ea,
                      wt.method = "case",
                      data = ipaw_model_df)
  
  models_bmi_ea[[i]] <- glm_bmi_ea
  models_bmi_ya[[i]] <- glm_bmi_ya
  models_ravens_ea[[i]] <- glm_ravens_ea
}








