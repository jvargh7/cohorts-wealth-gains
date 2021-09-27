
absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/absolute_df.RDS"))
cwealth <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/cwealth.RDS"))
mi_dfs <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/mi_dfs.RDS"))

formula_bmi_ea = "bmi_ea ~ wealth_child*male + cwealth_teen*male + cwealth_ya*male + cwealth_ea*male + moscho_sib + moage + gtatole*full + chbirtho + byear + eduyr + rural_ea + formal_ea"
formula_bmi_ya = "bmi_ya ~ wealth_child*male + cwealth_teen*male + cwealth_ya*male + moscho_sib + moage + gtatole*full + chbirtho + byear + eduyr + rural_ya"  # formal_ya
formula_ravens_ea = "ravens_ea ~ wealth_child*male + cwealth_teen*male + cwealth_ya*male + cwealth_ea*male + moscho_sib + moage + gtatole*full + chbirtho + byear + eduyr + rural_ea + formal_ea"


models_bmi_ea <- list()
models_bmi_ya <- list()
models_ravens_ea <- list()

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
    mutate(ravens_ea = scale(ravens_ea))  %>%
    mutate(bmi_ea = case_when(pregnant_ea == 1 ~ NA_real_,
                              TRUE ~ bmi_ea),
           bmi_ya = case_when(pregnant_ya == 1 ~ NA_real_,
                              TRUE ~ bmi_ya))
  
  
  glm_bmi_ea <- lm(as.formula(paste0(formula_bmi_ea)),
                   family = gaussian(),
                   data = model_df)
  
  glm_bmi_ya <- lm(as.formula(paste0(formula_bmi_ya)),
                   family = gaussian(),
                   data = model_df)
  
  glm_ravens_ea <- lm(as.formula(paste0(formula_ravens_ea)),
                      family = gaussian(),
                      data = model_df)
  
  models_bmi_ea[[i]] <- glm_bmi_ea
  models_bmi_ya[[i]] <- glm_bmi_ya
  models_ravens_ea[[i]] <- glm_ravens_ea
}








