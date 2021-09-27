

absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/absolute_df.RDS"))

before_imputation <- absolute_df %>% 
  dplyr::select(id_uni,d_id_unim,
                contains("wealth_"),
                male,chbirtho,gtatole,full,
                byear,moage,moht_sib,moscho_sib,
                
                eduyr,formal_ea,
                children_ea,rural_ea,rural_ya,
                bmi_ea,bmi_ya, ravens_ea,
                pregnant_ea,pregnant_ya)


# MULTIPLE IMPUTATION -------------

library(mice)

mi_iter = 10



mi_null <- mice(before_imputation %>% 
                  dplyr::select(-bmi_ea,-bmi_ya,-ravens_ea,-pregnant_ea,-pregnant_ya),
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

pred[c("id_uni","d_id_unim"),] <- 0
pred[,c("id_uni","d_id_unim")] <- 0

# We are not imputing rural because?

mi_dfs <- mice(before_imputation %>% 
                 dplyr::select(-bmi_ea,-bmi_ya,-ravens_ea,-pregnant_ea,-pregnant_ya),
               method = method,
               pred = pred,
               m=mi_iter,maxit=50,seed=500)

saveRDS(mi_dfs, paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/mi_dfs.RDS"))

plot(mi_dfs)
