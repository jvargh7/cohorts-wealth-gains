
absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/absolute_df.RDS"))
cwealth <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/cwealth.RDS"))
mi_dfs <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/mi_dfs.RDS"))

formula_bmi_ea = "bmi_ea ~ wealth_child*male + cwealth_teen*male + cwealth_ya*male + cwealth_ea*male + moscho + moage + chbirtho + eduyr + rural_ea + formal_ea + children_ea + married_ea"
formula_bmi_ya = "bmi_ya ~ wealth_child*male + cwealth_teen*male + cwealth_ya*male + moscho + moage + chbirtho + eduyr + rural_ya + formal_ya"
formula_srq_ea = "srq_ea ~ wealth_child*male + cwealth_teen*male + cwealth_ya*male + cwealth_ea*male + moscho + moage + chbirtho + eduyr + rural_ea + formal_ea + children_ea + married_ea"
formula_stress_ya = "stress_ya ~ wealth_child*male + cwealth_teen*male + cwealth_ya*male + moscho + moage + chbirtho + eduyr + rural_ya + formal_ya"
formula_ravens_ea = "ravens_ea ~ wealth_child*male + cwealth_teen*male + cwealth_ya*male + cwealth_ea*male + moscho + moage + chbirtho + eduyr + rural_ea + formal_ea + children_ea + married_ea"
formula_happiness_ea = "happiness_ea ~ wealth_child*male + cwealth_teen*male + cwealth_ya*male + cwealth_ea*male + moscho + moage + chbirtho + eduyr + rural_ea + formal_ea + children_ea + married_ea"


models_bmi_ea <- list()
models_bmi_ya <- list()
models_srq_ea <- list()
models_stress_ya <- list()
models_ravens_ea <- list()
models_happiness_ea <- list()

for (i in 1:mi_dfs$m){
  
  # cat("\n Iteration ",i)
  
  model_df = absolute_df %>% 
    mutate(cwealth_teen = cwealth %>% dplyr::filter(lifestage=="teen") %>% dplyr::select(paste0("V",i)) %>% pull(),
           cwealth_ya = cwealth %>% dplyr::filter(lifestage=="ya") %>%  dplyr::select(paste0("V",i)) %>% pull(),
           cwealth_ea = cwealth %>% dplyr::filter(lifestage=="ea") %>%  dplyr::select(paste0("V",i)) %>% pull(),
           # moscho = complete(mi_dfs,i)$moscho,
           formal_ea = complete(mi_dfs,i)$formal_ea,
           children_ya = complete(mi_dfs,i)$children_ya,
           children_ea = complete(mi_dfs,i)$children_ea,
           married_ea = complete(mi_dfs,i)$married_ea,
           moage = complete(mi_dfs,i)$moage,
           chbirtho = complete(mi_dfs,i)$chbirtho,
           eduyr = complete(mi_dfs,i)$eduyr,
           rural_ea = complete(mi_dfs,i)$rural_ea,
           rural_ya = complete(mi_dfs,i)$rural_ya,
    ) %>% 
    mutate(ravens_ea = scale(ravens_ea),
           srq_ea = scale(srq_ea),
           happiness_ea = scale(happiness_ea),
           stress_ya = scale(stress_ya))
  
  
  glm_bmi_ea <- lm(as.formula(paste0(formula_bmi_ea)),
                   family = gaussian(),
                   data = model_df %>% mutate(bmi_ea = case_when(pregnant_ea == 1 ~ NA_real_,
                                                                 TRUE ~ bmi_ea)))
  glm_bmi_ya <- lm(as.formula(paste0(formula_bmi_ya)),
                   family = gaussian(),
                   data = model_df %>% mutate(bmi_ya = case_when(pregnant_ya == 1 ~ NA_real_,
                                                                 TRUE ~ bmi_ya))
  )
  
  glm_srq_ea <- lm(as.formula(paste0(formula_srq_ea)),
                   family = gaussian(),
                   data = model_df)
  glm_stress_ya <- lm(as.formula(paste0(formula_stress_ya)),
                      family = gaussian(),
                      data = model_df)
  glm_ravens_ea <- lm(as.formula(paste0(formula_ravens_ea)),
                      family = gaussian(),
                      data = model_df)
  glm_happiness_ea <- lm(as.formula(paste0(formula_happiness_ea)),
                         family = gaussian(),
                         data = model_df)
  
  models_bmi_ea[[i]] <- glm_bmi_ea
  models_bmi_ya[[i]] <- glm_bmi_ya
  models_srq_ea[[i]] <- glm_srq_ea
  models_stress_ya[[i]] <- glm_stress_ya
  models_ravens_ea[[i]] <- glm_ravens_ea
  models_happiness_ea[[i]] <- glm_happiness_ea
}








