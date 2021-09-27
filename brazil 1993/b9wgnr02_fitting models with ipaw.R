source(paste0(path_replication_repo,"/gmethods/gmethods_functions.R"))


absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/absolute_df.RDS"))
cwealth <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/cwealth.RDS"))
mi_dfs <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/mi_dfs.RDS"))

formula_bmi_ya = "bmi_ya ~ wealth_child + cwealth_teen + cwealth_ya + moscho + moage + momwhite + chbirtho + male + eduyr + white + children_2015 + formal_ya + married_ya"
formula_srq_ya = "srq_ya ~ wealth_child + cwealth_teen + cwealth_ya + moscho + moage + momwhite + chbirtho + male + eduyr + white + children_2015 + formal_ya + married_ya"
formula_iq_ya = "iq_ya ~ wealth_child + cwealth_teen + cwealth_ya + moscho + moage + momwhite + chbirtho + male + white + children_2011"
formula_wellbeing_ya = "wellbeing_ya ~ wealth_child + cwealth_teen + cwealth_ya + moscho + moage + momwhite + chbirtho + male + eduyr + white + children_2015 + formal_ya + married_ya"


models_bmi_ya <- list()
models_srq_ya <- list()
models_iq_ya <- list()
models_wellbeing_ya <- list()

# i = 5
for (i in 1:mi_dfs$m){
  
  # cat("\n Iteration ",i)
  
  model_df = absolute_df %>% 
    mutate(cwealth_teen = cwealth %>% dplyr::filter(lifestage=="teen") %>% dplyr::select(paste0("V",i)) %>% pull(),
           cwealth_ya = cwealth %>% dplyr::filter(lifestage=="ya") %>%  dplyr::select(paste0("V",i)) %>% pull(),
           wealth_child = complete(mi_dfs,i)$wealth_child,
           moscho = complete(mi_dfs,i)$moscho,
           momwhite = complete(mi_dfs,i)$momwhite,
           formal_ya = complete(mi_dfs,i)$formal_ya,
           married_ya = complete(mi_dfs,i)$married_ya,
           children_2015 = complete(mi_dfs,i)$children_2015,
           children_2011 = complete(mi_dfs,i)$children_2011,
           moage = complete(mi_dfs,i)$moage,
           chbirtho = complete(mi_dfs,i)$chbirtho,
           eduyr = complete(mi_dfs,i)$eduyr,
           rural = complete(mi_dfs,i)$rural
    ) %>% 
    mutate(iq_ya = scale(iq_ya),
           wellbeing_ya = scale(wellbeing_ya),
           srq_ya = scale(srq_ya))
  
  
  # Implementing IPAW procedure -----------
  alive_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/alive_df.RDS"))
  
  ipaw_model_df <- model_df %>% 
    left_join(alive_df %>% 
                dplyr::select(nquest,alive_ya,
                              participated_ya,
                              c_alive_ya,
                              c_participated_ya,
                              moemployedcurr),
              by = "nquest") %>% 
    mutate(na_bmi_ya = case_when(!is.na(bmi_ya) ~ 1,
                                 participated_ya == 1 ~ 0,
                                 TRUE ~ NA_real_),
           na_srq_ya = case_when(!is.na(srq_ya) ~ 1,
                                    participated_ya == 1 ~ 0,
                                    TRUE ~ NA_real_),
           
           na_wellbeing_ya = case_when(!is.na(wellbeing_ya) ~ 1,
                                       participated_ya == 1 ~ 0,
                                       TRUE ~ NA_real_),
           na_iq_ya = case_when(!is.na(iq_ya) ~ 1,
                                    participated_ya == 1 ~ 0,
                                    TRUE ~ NA_real_)) %>% 
    
    mutate(response_bmi_ya = 1, 
           response_srq_ya = 1,
           response_wellbeing_ya = 1,
           response_iq_ya = 1) # Since this was the only outcome collected in 2011
  rhs_formula_o = "~ male + moscho + moage + chbirtho + moemployedcurr + momwhite"
  
  # Pr[Response = 1| Alive = 1, Particiapted in wave = 1] ----
  
  if(mean(ipaw_model_df$na_bmi_ya,na.rm=TRUE) > 0.05 &
     mean(ipaw_model_df$na_bmi_ya,na.rm=TRUE) < 0.95) {
    ipaw_model_df[!is.na(ipaw_model_df$na_bmi_ya),]$response_bmi_ya <-
      censoring_weights(c_formula = paste0("na_bmi_ya",
                                           rhs_formula_o),
                        df = ipaw_model_df,
                        type = "glm")
  }
  
  if(mean(ipaw_model_df$na_srq_ya,na.rm=TRUE) > 0.05 &
     mean(ipaw_model_df$na_srq_ya,na.rm=TRUE) < 0.95) {
    ipaw_model_df[!is.na(ipaw_model_df$na_srq_ya),]$response_srq_ya <-
      censoring_weights(c_formula = paste0("na_srq_ya",
                                           rhs_formula_o),
                        df = ipaw_model_df,
                        type = "glm")
  }
  
  if(mean(ipaw_model_df$na_wellbeing_ya,na.rm=TRUE) > 0.05 &
     mean(ipaw_model_df$na_wellbeing_ya,na.rm=TRUE) < 0.95) {
    ipaw_model_df[!is.na(ipaw_model_df$na_srq_ya),]$response_wellbeing_ya <-
      censoring_weights(c_formula = paste0("na_wellbeing_ya",
                                           rhs_formula_o),
                        df = ipaw_model_df,
                        type = "glm")
  }
  if(mean(ipaw_model_df$na_iq_ya,na.rm=TRUE) > 0.05 &
     mean(ipaw_model_df$na_iq_ya,na.rm=TRUE) < 0.95) {
    ipaw_model_df[!is.na(ipaw_model_df$na_srq_ya),]$response_iq_ya <-
      censoring_weights(c_formula = paste0("na_iq_ya",
                                           rhs_formula_o),
                        df = ipaw_model_df,
                        type = "glm")
  }
  
  glm_bmi_ya <- MASS::rlm(as.formula(paste0(formula_bmi_ya)),
                          weights = c_alive_ya*c_participated_ya*response_bmi_ya,
                          wt.method = "case",
                          data = ipaw_model_df %>% 
                            mutate(bmi_ya = case_when(pregnant_ya == 1 ~ NA_real_,
                                                      TRUE ~ bmi_ya))
  )
  
  glm_srq_ya <- MASS::rlm(as.formula(paste0(formula_srq_ya)),
                             weights = c_alive_ya*c_participated_ya*response_srq_ya,
                             wt.method = "case",
                             data = ipaw_model_df)
  
  glm_wellbeing_ya <- MASS::rlm(as.formula(paste0(formula_wellbeing_ya)),
                          weights = c_alive_ya*c_participated_ya*response_wellbeing_ya,
                          wt.method = "case",
                          data = ipaw_model_df)
  
  glm_iq_ya <- MASS::rlm(as.formula(paste0(formula_iq_ya)),
                          weights = c_alive_ya*c_participated_ya*response_iq_ya,
                          wt.method = "case",
                          data = ipaw_model_df)
  

  models_bmi_ya[[i]] <- glm_bmi_ya
  models_srq_ya[[i]] <- glm_srq_ya
  models_iq_ya[[i]] <- glm_iq_ya
  models_wellbeing_ya[[i]] <- glm_wellbeing_ya
}








