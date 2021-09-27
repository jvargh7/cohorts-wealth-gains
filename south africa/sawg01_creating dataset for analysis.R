
source(paste0(path_mobility_repo,"/south africa/saaux01_covariates.R"))

gains_df <- readRDS(paste0(path_harmonization_folder,"/south africa/working/pca_df_unimputed.RDS")) %>% 
  dplyr::select(bttid,year,pc) %>% 
  pivot_wider(names_from="year",values_from="pc",names_prefix = "pc") %>% 
  mutate(wealth_child = pc1990,
         year_child = case_when(!is.na(pc1990) ~ 1990,
                                TRUE ~ NA_real_),
         wealth_teen = case_when(!is.na(pc2002) & !is.na(pc2006) & !is.na(pc1997) ~  rowMeans(.[,c("pc2002","pc2006","pc1997")],na.rm=TRUE),
                                 !is.na(pc2006) & !is.na(pc1997) ~  rowMeans(.[,c("pc2006","pc1997")],na.rm=TRUE),
                                 !is.na(pc2002) & !is.na(pc1997) ~  rowMeans(.[,c("pc2002","pc1997")],na.rm=TRUE),
                                 !is.na(pc2002) & !is.na(pc2006) ~  rowMeans(.[,c("pc2002","pc2006")],na.rm=TRUE),
                                 !is.na(pc2002) ~ pc2002,
                                 !is.na(pc2006) ~ pc2006,
                                 !is.na(pc1997) ~ pc1997,
                                 TRUE ~ NA_real_),
         year_teen = case_when(!is.na(pc2002) & !is.na(pc2006) & !is.na(pc1997) ~ 2001.7 ,
                               !is.na(pc2006) & !is.na(pc1997) ~  2001.5,
                               !is.na(pc2002) & !is.na(pc1997) ~  1999.5,
                               !is.na(pc2002) & !is.na(pc2006) ~  2004,
                               !is.na(pc2002) ~ 2002,
                               !is.na(pc2006) ~ 2006,
                               !is.na(pc1997) ~ 1997,
                               TRUE ~ NA_real_),
         
         wealth_ya = pc2012,
         year_ya = case_when(!is.na(pc2012) ~ 2012,
                             TRUE ~ NA_real_),
         
         wealth_ea = pc2018,
         year_ea = case_when(!is.na(pc2018) ~ 2018,
                             TRUE ~ NA_real_)
         
  ) %>% 
  mutate(age_child = year_child - 1990,
         age_teen = year_teen - 1990,
         age_ya = year_ya - 1990,
         age_ea = year_ea - 1990)

# Summary of gains_df --------
library(compareGroups)
gains_df %>% 
  compareGroups(~age_child + age_teen + age_ya + age_ea + wealth_child + wealth_teen + wealth_ya + wealth_ea,
                data = .,method = c(2,2,2,2,1,1,1,1),include.label = FALSE) %>% 
  createTable(.,digits = 2,sd.type = 2,q.type = c(2,2)) %>% 
  export2xls(.,file=paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/summary of filtered ids.xlsx"))

saveRDS(gains_df,paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/gains_df.RDS"))
