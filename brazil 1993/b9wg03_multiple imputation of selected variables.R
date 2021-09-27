

absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/absolute_df.RDS"))
# Did not check if participants were alive


before_imputation <- absolute_df %>% 
  dplyr::select(nquest,
                contains("wealth_"),
                male,chbirtho,
                moage,moscho,
                momwhite,
                white,
                eduyr,formal_ya,married_ya,
                children_2015,children_2011,
                bmi_ya, iq_ya, srq_ya,wellbeing_ya,
                pregnant_ya)


# MULTIPLE IMPUTATION -------------

library(mice)

mi_iter = 10

source(paste0(path_replication_repo,"/package/residuals_mi.R"))
source(paste0(path_replication_repo,"/package/rsquared_mi.R"))

mi_null <- mice(before_imputation %>% 
                  dplyr::select(-bmi_ya,-srq_ya,-iq_ya,-wellbeing_ya,-pregnant_ya),
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

pred[c("nquest"),] <- 0
pred[,c("nquest")] <- 0

# method[which(method == "pmm")] <- "rf"

mi_dfs <- mice(before_imputation %>% 
                 dplyr::select(-bmi_ya,-srq_ya,-iq_ya,-wellbeing_ya,-pregnant_ya),
               method = method,
               pred = pred,
               m=mi_iter,maxit=50,seed=500)

saveRDS(mi_dfs, paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/mi_dfs.RDS"))
# mi_dfs <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/mi_dfs.RDS"))
