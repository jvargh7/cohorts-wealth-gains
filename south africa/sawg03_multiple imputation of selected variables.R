absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/absolute_df.RDS"))

before_imputation <- absolute_df %>% 
  dplyr::select(bttid,
                contains("wealth_"),
                male,chbirtho,
                moage,moscho,
                
                black,
                
                eduyr,formal_ea,married_ea,
                children_ea,
                bmi_ya, srq_ea, happiness_ea,ravens_ea,
                pregnant_ea)


# MULTIPLE IMPUTATION -------------

library(mice)

mi_iter = 10

source(paste0(path_replication_repo,"/package/residuals_mi.R"))
source(paste0(path_replication_repo,"/package/rsquared_mi.R"))

mi_null <- mice(before_imputation %>% 
                  dplyr::select(-bmi_ya,-srq_ea,-ravens_ea,-happiness_ea,-pregnant_ea),
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

pred[c("bttid"),] <- 0
pred[,c("bttid")] <- 0

# method[which(method == "pmm")] <- "rf"

mi_dfs <- mice(before_imputation %>% 
                 dplyr::select(-bmi_ya,-srq_ea,-ravens_ea,-happiness_ea,-pregnant_ea),
               method = method,
               pred = pred,
               m=mi_iter,maxit=50,seed=500)

saveRDS(mi_dfs, paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/mi_dfs.RDS"))
