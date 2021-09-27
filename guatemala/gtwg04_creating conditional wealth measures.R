mi_dfs <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/mi_dfs.RDS"))

# CONDITIONAL WEALTH --------------
source(paste0(path_replication_repo,"/package/residuals_mi.R"))
source(paste0(path_replication_repo,"/package/rsquared_mi.R"))

model_cwealth_teen <- '
wealth_teen ~ wealth_child
'
result_cwealth_teen <- with(mi_dfs,lm(as.formula(model_cwealth_teen)))
cwealth_teen_imp <- residuals_mi(result_cwealth_teen,method="identity")

model_cwealth_ya <- '
wealth_ya ~ wealth_child + wealth_teen
'
result_cwealth_ya <- with(mi_dfs,lm(as.formula(model_cwealth_ya)))

cwealth_ya_imp <- residuals_mi(result_cwealth_ya,method="identity")


# 2002 ------
model_cwealth_ea <- '
wealth_ea ~ wealth_ya + wealth_teen + wealth_child
'
result_cwealth_ea <- with(mi_dfs,lm(as.formula(model_cwealth_ea)))

cwealth_ea_imp <- residuals_mi(result_cwealth_ea,method="identity")

cwealth <- bind_rows(
  cwealth_teen_imp %>% mutate(lifestage = "teen"),
  cwealth_ya_imp %>% mutate(lifestage = "ya"),
  cwealth_ea_imp %>% mutate(lifestage = "ea")
  
)


saveRDS(cwealth, paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/cwealth.RDS"))

