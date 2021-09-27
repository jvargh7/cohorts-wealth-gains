
absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/absolute_df.RDS"))
cwealth <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/cwealth.RDS"))
mi_dfs <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/mi_dfs.RDS"))

formula_bmi_ya = "bmi_ya ~ wealth_child + cwealth_teen + cwealth_ya + moscho + moage + chbirtho + male + black + eduyr" 
formula_srq_ea = "srq_ea ~ wealth_child + cwealth_teen + cwealth_ya + cwealth_ea + moscho + moage + chbirtho + male + black + eduyr + children_ea + formal_ea + married_ea"
formula_ravens_ea = "ravens_ea ~ wealth_child + cwealth_teen + cwealth_ya + cwealth_ea + moscho + moage + chbirtho + male + black + eduyr + children_ea + formal_ea + married_ea"
formula_happiness_ea = "happiness_ea ~ wealth_child + cwealth_teen + cwealth_ya + cwealth_ea + moscho + moage + chbirtho + male + black + eduyr + children_ea + formal_ea + married_ea"


models_bmi_ya <- list()
models_srq_ea <- list()
models_ravens_ea <- list()
models_happiness_ea <- list()

for (i in 1:mi_dfs$m){
  
  # cat("\n Iteration ",i)
  
  model_df = absolute_df %>% 
    mutate(cwealth_teen = cwealth %>% dplyr::filter(lifestage=="teen") %>% dplyr::select(paste0("V",i)) %>% pull(),
           cwealth_ya = cwealth %>% dplyr::filter(lifestage=="ya") %>%  dplyr::select(paste0("V",i)) %>% pull(),
           cwealth_ea = cwealth %>% dplyr::filter(lifestage=="ea") %>%  dplyr::select(paste0("V",i)) %>% pull(),
           wealth_child = complete(mi_dfs,i)$wealth_child,
           moscho = complete(mi_dfs,i)$moscho,
           formal_ea = complete(mi_dfs,i)$formal_ea,
           married_ea = complete(mi_dfs,i)$married_ea,
           children_ya = complete(mi_dfs,i)$children_ya,
           children_ea = complete(mi_dfs,i)$children_ea,
           moage = complete(mi_dfs,i)$moage,
           chbirtho = complete(mi_dfs,i)$chbirtho,
           eduyr = complete(mi_dfs,i)$eduyr,
           rural = complete(mi_dfs,i)$rural
    ) %>% 
    mutate(ravens_ea = scale(ravens_ea),
           srq_ea = scale(srq_ea),
           happiness_ea = scale(happiness_ea))
  
  
  glm_bmi_ya <- lm(as.formula(paste0(formula_bmi_ya)),
                family = gaussian(),
                data = model_df 
                # %>% mutate(bmi_ya = case_when(pregnant_ya == 1 ~ NA_real_,
                #                                            TRUE ~ bmi_ya))
                )
  glm_srq_ea <- lm(as.formula(paste0(formula_srq_ea)),
                family = gaussian(),
                data = model_df)
  glm_ravens_ea <- lm(as.formula(paste0(formula_ravens_ea)),
                   family = gaussian(),
                   data = model_df)
  glm_happiness_ea <- lm(as.formula(paste0(formula_happiness_ea)),
                      family = gaussian(),
                      data = model_df)
  
  models_bmi_ya[[i]] <- glm_bmi_ya
  models_srq_ea[[i]] <- glm_srq_ea
  models_ravens_ea[[i]] <- glm_ravens_ea
  models_happiness_ea[[i]] <- glm_happiness_ea
}








