

gains_df <- readRDS(paste0(path_harmonization_folder,"/philippines/working/pca_df_unimputed.RDS")) %>% 
  dplyr::select(uncchdid,year,pc) %>% 
  pivot_wider(names_from="year",values_from="pc",names_prefix = "pc") %>% 
  mutate(wealth_child = pc1983,
         year_child = 1983,
         wealth_teen = case_when(!is.na(pc1994) & !is.na(pc1998) & !is.na(pc1991) ~ rowMeans(.[,c("pc1994","pc1998","pc1991")],na.rm=TRUE),
                                 !is.na(pc1998) & !is.na(pc1991) ~ rowMeans(.[,c("pc1998","pc1991")],na.rm=TRUE),
                                 !is.na(pc1994) & !is.na(pc1991) ~ rowMeans(.[,c("pc1994","pc1991")],na.rm=TRUE),
                                 !is.na(pc1994) & !is.na(pc1998) ~ rowMeans(.[,c("pc1994","pc1998")],na.rm=TRUE),
                                 !is.na(pc1994) ~ pc1994,
                                 !is.na(pc1998) ~ pc1998,
                                 !is.na(pc1991) ~ pc1991,
                                 TRUE ~ NA_real_),
         year_teen = case_when(!is.na(pc1994) & !is.na(pc1998) & !is.na(pc1991) ~ 1994.3,
                               !is.na(pc1998) & !is.na(pc1991) ~ 1994.5,
                               !is.na(pc1994) & !is.na(pc1991) ~ 1992.5,
                               !is.na(pc1994) & !is.na(pc1998) ~ 1996,
                               !is.na(pc1994) ~ 1994,
                               !is.na(pc1998) ~ 1998,
                               !is.na(pc1991) ~ 1991,
                               TRUE ~ NA_real_),
         
         wealth_ya = case_when(!is.na(pc2005) & !is.na(pc2009) & !is.na(pc2002) ~  rowMeans(.[,c("pc2005","pc2009","pc2002")],na.rm=TRUE),
                               is.na(pc2009) & !is.na(pc2002) ~  rowMeans(.[,c("pc2009","pc2002")],na.rm=TRUE),
                               !is.na(pc2005) & !is.na(pc2002) ~  rowMeans(.[,c("pc2005","pc2002")],na.rm=TRUE),
                               !is.na(pc2005) & !is.na(pc2009) ~  rowMeans(.[,c("pc2005","pc2009")],na.rm=TRUE),
                               !is.na(pc2005) ~ pc2005,
                               !is.na(pc2009) ~ pc2009,
                               !is.na(pc2002) ~ pc2002,
                               TRUE ~ NA_real_),
         year_ya = case_when(!is.na(pc2005) & !is.na(pc2009) & !is.na(pc2002) ~  2005.3,
                             is.na(pc2009) & !is.na(pc2002) ~  2005.5,
                             !is.na(pc2005) & !is.na(pc2002) ~  2003.5,
                             !is.na(pc2005) & !is.na(pc2009) ~  2007,
                             !is.na(pc2005) ~ 2005,
                             !is.na(pc2009) ~ 2009,
                             !is.na(pc2002) ~ 2002,
                             TRUE ~ NA_real_),
         
         wealth_ea = pc2018,
         year_ea = case_when(!is.na(pc2018) ~ 2018,
                             TRUE ~ NA_real_) # Could be improved
         
  ) %>% 
  mutate(age_child = 0.5,
         age_teen = year_teen - 1983,
         age_ya = year_ya - 1983,
         age_ea = year_ea - 1983)


# Summary of gains_df --------
library(compareGroups)
gains_df %>% 
  compareGroups(~age_child + age_teen + age_ya + age_ea + wealth_child + wealth_teen + wealth_ya + wealth_ea,
                data = .,method = c(2,2,2,2,1,1,1,1),include.label = FALSE) %>% 
  createTable(.,digits = 2,sd.type = 2,q.type = c(2,2)) %>% 
  export2xls(.,file=paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/summary of filtered ids.xlsx"))

saveRDS(gains_df,paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/gains_df.RDS"))
