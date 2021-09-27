source(paste0(path_gains_repo,"/brazil 1993/b9mobaux01_covariates.R"))


gains_df <- readRDS(paste0(path_harmonization_folder,"/brazil 1993/working/pca_df_unimputed.RDS")) %>%
  dplyr::select(nquest,year,pc) %>% 
  pivot_wider(names_from="year",values_from="pc",names_prefix = "pc") %>% 
  mutate(wealth_child = pc1997,
         
         year_child = 1997,
         wealth_teen = case_when(!is.na(pc2004) & !is.na(pc2008) ~ rowMeans(.[,c("pc2004","pc2008")],na.rm=TRUE),
                                 !is.na(pc2004) ~ pc2004,
                                 !is.na(pc2008) ~ pc2008,
                                 TRUE ~ NA_real_),
         year_teen = case_when(!is.na(pc2004) & !is.na(pc2008) ~ 2006,
                               !is.na(pc2004) ~ 2004,
                               !is.na(pc2008) ~ 2008,
                               TRUE ~ NA_real_),
         
         
         wealth_ya = case_when(!is.na(pc2011) & !is.na(pc2015) ~ rowMeans(.[,c("pc2011","pc2015")],na.rm=TRUE),
                               !is.na(pc2011) ~ pc2011,
                               !is.na(pc2015) ~ pc2015,
                               TRUE ~ NA_real_),
         year_ya = case_when(!is.na(pc2011) & !is.na(pc2015) ~ 2013,
                             !is.na(pc2011) ~ 2011,
                             !is.na(pc2015) ~ 2015,
                             TRUE ~ NA_real_)
         
  ) %>% 
  mutate(age_child = 4,
         age_teen = case_when(year_teen == 2006 ~ 13,
                              year_teen == 2004 ~ 11,
                              year_teen == 2008 ~ 15,
                              TRUE ~ NA_real_),
         age_ya = case_when(year_ya == 2013 ~ 20,
                              year_ya == 2011 ~ 18,
                              year_ya == 2015 ~ 22,
                              TRUE ~ NA_real_)
         )


# Summary of gains_df --------
library(compareGroups)
gains_df %>% 
  compareGroups(~age_child + age_teen + age_ya + wealth_child + wealth_teen + wealth_ya,
                data = .,method = c(2,2,2,1,1,1)) %>% 
  createTable(.,digits = 2,sd.type = 2,q.type = c(2,2)) %>% 
  export2xls(.,file=paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/summary of filtered ids.xlsx"))

saveRDS(gains_df,paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/gains_df.RDS"))
