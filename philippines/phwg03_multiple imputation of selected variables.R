absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/absolute_df.RDS"))

before_imputation <- absolute_df %>% 
  rename(rural = rural2018) %>% 
  dplyr::select(uncchdid,
                contains("wealth_"),
                male,chbirtho,
                moage,moscho,
                
                eduyr,formal_ya,formal_ea,married_ea,
                children_ea,rural_ya,rural_ea,rural_teen,
                bmi_ea,bmi_ya, srq_ea,stress_ya, happiness_ea,ravens_ea,
                pregnant_ea,pregnant_ya)


# MULTIPLE IMPUTATION -------------

library(mice)

mi_iter = 10

source(paste0(path_replication_repo,"/package/residuals_mi.R"))
source(paste0(path_replication_repo,"/package/rsquared_mi.R"))

mi_null <- mice(before_imputation %>% 
                  dplyr::select(-bmi_ea,-bmi_ya,-srq_ea,-stress_ya,-ravens_ea,-happiness_ea,
                                -pregnant_ea,-pregnant_ya),
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

pred[c("uncchdid"),] <- 0
pred[,c("uncchdid")] <- 0

# method[which(method == "pmm")] <- "rf"

# We are not imputing rural because?

mi_dfs <- mice(before_imputation %>% 
                 dplyr::select(-bmi_ea,-bmi_ya,-srq_ea,-stress_ya,-ravens_ea,-happiness_ea,
                               -pregnant_ea,-pregnant_ya),
               method = method,
               pred = pred,
               m=mi_iter,maxit=50,seed=500)

saveRDS(mi_dfs, paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/mi_dfs.RDS"))
