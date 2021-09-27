
absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/absolute_df.RDS"))
cwealth <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/cwealth.RDS"))
mi_dfs <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/mi_dfs.RDS"))

formula_teen = "cwealth_teen ~ moscho + moage + momwhite + chbirtho + male + white"
formula_ya = "cwealth_ya ~ moscho + moage + momwhite + chbirtho + male + eduyr + white + children_2011 + formal_ya + married_ya"
# formula_ea = "cwealth_ea ~ wealth_child + cwealth_teen + moscho + moage + momwhite + chbirtho + male + eduyr + white + children_2015 + formal_ya"

models_teen <- list()
models_ya <- list()
# models_ea <- list()

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
    )
  
  glm_teen <- lm(as.formula(paste0(formula_teen)),
                 family = gaussian(),
                 data = model_df %>% 
                   mutate(bmi_ya = case_when(pregnant_ya == 1 ~ NA_real_,
                                             TRUE ~ bmi_ya)) %>% 
                   dplyr::filter(!is.na(bmi_ya)|!is.na(iq_ya)|!is.na(srq_ya)|!is.na(wellbeing_ya)))
  
  glm_ya <- lm(as.formula(paste0(formula_ya)),
                   family = gaussian(),
               data = model_df %>% 
                 mutate(bmi_ya = case_when(pregnant_ya == 1 ~ NA_real_,
                                           TRUE ~ bmi_ya)) %>% 
                 dplyr::filter(!is.na(bmi_ya)|!is.na(iq_ya)|!is.na(srq_ya)|!is.na(wellbeing_ya)))
  
  models_teen[[i]] <- glm_teen
  models_ya[[i]] <- glm_ya
}








