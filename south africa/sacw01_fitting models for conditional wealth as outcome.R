
absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/absolute_df.RDS"))
cwealth <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/cwealth.RDS"))
mi_dfs <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/mi_dfs.RDS"))

formula_teen = "cwealth_ya ~ moscho + moage + chbirtho + male + black"
formula_ya = "cwealth_ya ~ moscho + moage + chbirtho + male + black + eduyr"  # + children_ya + formal_ya + married_ya
formula_ea = "cwealth_ea ~ moscho + moage + chbirtho + male + black + eduyr + children_ea + formal_ea + married_ea"

models_teen <- list()
models_ya <- list()
models_ea <- list()

for (i in 1:mi_dfs$m){
  
  # cat("\n Iteration ",i)
  
  model_df = absolute_df %>% 
    mutate(cwealth_teen = cwealth %>% dplyr::filter(lifestage=="teen") %>% dplyr::select(paste0("V",i)) %>% pull(),
           cwealth_ya = cwealth %>% dplyr::filter(lifestage=="ya") %>%  dplyr::select(paste0("V",i)) %>% pull(),
           cwealth_ea = cwealth %>% dplyr::filter(lifestage=="ea") %>%  dplyr::select(paste0("V",i)) %>% pull(),
           wealth_child = complete(mi_dfs,i)$wealth_child,
           moscho = complete(mi_dfs,i)$moscho,
           # formal_ya = complete(mi_dfs,i)$formal_ya,
           formal_ea = complete(mi_dfs,i)$formal_ea,
           married_ea = complete(mi_dfs,i)$married_ea,
           children_ya = complete(mi_dfs,i)$children_ya,
           children_ea = complete(mi_dfs,i)$children_ea,
           moage = complete(mi_dfs,i)$moage,
           chbirtho = complete(mi_dfs,i)$chbirtho,
           eduyr = complete(mi_dfs,i)$eduyr,
           rural_ya = complete(mi_dfs,i)$rural_ya,
           rural_ea = complete(mi_dfs,i)$rural_ea
    ) %>% 
    # %>% mutate(bmi_ya = case_when(pregnant_ya == 1 ~ NA_real_,
    #                                            TRUE ~ bmi_ya))
    dplyr::filter(!is.na(bmi_ya)|!is.na(ravens_ea)|!is.na(srq_ea)|!is.na(happiness_ea))
  
  glm_teen <- lm(as.formula(paste0(formula_teen)),
               family = gaussian(),
               data = model_df  
                 # %>% mutate(bmi_ya = case_when(pregnant_ya == 1 ~ NA_real_,
                 #                                            TRUE ~ bmi_ya))
                 # dplyr::filter(!is.na(bmi_ya)|!is.na(ravens_ea)|!is.na(srq_ea)|!is.na(happiness_ea))
  )
  
  glm_ya <- lm(as.formula(paste0(formula_ya)),
               family = gaussian(),
               data = model_df  
               # %>% mutate(bmi_ya = case_when(pregnant_ya == 1 ~ NA_real_,
               #                                            TRUE ~ bmi_ya))
                 # dplyr::filter(!is.na(bmi_ya))
  )
  
  glm_ea <- lm(as.formula(paste0(formula_ea)),
               family = gaussian(),
               data = model_df  
                 # dplyr::filter(!is.na(ravens_ea)|!is.na(srq_ea)|!is.na(happiness_ea))
               )
  
  models_teen[[i]] <- glm_teen
  models_ya[[i]] <- glm_ya
  models_ea[[i]] <- glm_ea
}








