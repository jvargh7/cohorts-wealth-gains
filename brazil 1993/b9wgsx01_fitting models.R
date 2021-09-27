
absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/absolute_df.RDS"))
cwealth <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/cwealth.RDS"))
mi_dfs <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/mi_dfs.RDS"))

formula_bmi_ya = "bmi_ya ~ wealth_child*male + cwealth_teen*male + cwealth_ya*male + moscho + moage + momwhite + chbirtho + eduyr + white + children_2015 + formal_ya + married_ya"
formula_srq_ya = "srq_ya ~ wealth_child*male + cwealth_teen*male + cwealth_ya*male + moscho + moage + momwhite + chbirtho + eduyr + white + children_2015 + formal_ya + married_ya"
formula_iq_ya = "iq_ya ~ wealth_child*male + cwealth_teen*male + cwealth_ya*male + moscho + moage + momwhite + chbirtho + white + children_2011"
formula_wellbeing_ya = "wellbeing_ya ~ wealth_child*male + cwealth_teen*male + cwealth_ya*male + moscho + moage + momwhite + chbirtho + eduyr + white + children_2015 + formal_ya + married_ya"


models_bmi_ya <- list()
models_srq_ya <- list()
models_iq_ya <- list()
models_wellbeing_ya <- list()

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
           srq_ya = scale(srq_ya)) %>% 
    mutate(bmi_ya = case_when(pregnant_ya == 1 ~ NA_real_,
                              TRUE ~ bmi_ya))
  
  
  glm_bmi_ya <- lm(as.formula(paste0(formula_bmi_ya)),
                   family = gaussian(),
                   data = model_df)
  glm_srq_ya <- lm(as.formula(paste0(formula_srq_ya)),
                   family = gaussian(),
                   data = model_df)
  glm_iq_ya <- lm(as.formula(paste0(formula_iq_ya)),
                  family = gaussian(),
                  data = model_df)
  glm_wellbeing_ya <- lm(as.formula(paste0(formula_wellbeing_ya)),
                         family = gaussian(),
                         data = model_df)
  
  models_bmi_ya[[i]] <- glm_bmi_ya
  models_srq_ya[[i]] <- glm_srq_ya
  models_iq_ya[[i]] <- glm_iq_ya
  models_wellbeing_ya[[i]] <- glm_wellbeing_ya
}








