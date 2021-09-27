
source(paste0(path_incap_repo,"/structural/classify_urban_rural.R"))
ses_masters <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/ses_masters for COHORTS.RDS"))

source(paste0(path_gains_repo,"/guatemala/gtmobaux01_covariates.R"))


gains_df <- ses_masters %>% 
  dplyr::select(id_uni,pcall1967_1,pcall1975_1,pcall6775_1,pcall1987_1,pcall1996_1,pcall2002_1,pcall2016_1,pcall2018_1) %>% 
  left_join(early_life <- readRDS(paste0(path_harmonization_folder,"/guatemala/working/early_life.RDS")) %>% 
              dplyr::select(id_uni,gtchbyear) %>% 
              mutate(gtchbyear = 1900 + gtchbyear),
            by = "id_uni") %>% 
  dplyr::filter(gtchbyear >= 1971) %>% 
  mutate(age_6775 = case_when(gtchbyear < 1971 ~ gtchbyear - 1967,
                              TRUE ~ gtchbyear - 1975),
         age_1987 = 1987 - gtchbyear,
         age_1996 = 1996 - gtchbyear,
         age_2002 = 2002 - gtchbyear,
         age_2016 = 2015.5 - gtchbyear,
         age_2018 = 2017.5 - gtchbyear,
         
         wealth_child = pcall6775_1,
         
         wealth_teen = case_when(age_1987 %in% c(6:17) ~ pcall1987_1,
                                 TRUE ~ NA_real_),
         year_teen = case_when(age_1987 %in% c(6:17) ~ 1987,
                               TRUE ~ NA_real_),
         
         wealth_ya = case_when(age_1987 %in% c(18:26) ~ pcall1987_1,
                               age_1996 %in% c(18:26) & age_2002 %in% c(18:26) ~ rowMeans(.[,c("pcall1996_1","pcall2002_1")],na.rm=TRUE),
                               age_2002 %in% c(18:26)~ pcall2002_1,
                               age_1996 %in% c(18:26) ~ pcall1996_1,
                               TRUE ~ NA_real_),
         
         year_ya = case_when(age_1987 %in% c(18:26) ~ 1987,
                             age_1996 %in% c(18:26) & age_2002 %in% c(18:26) ~ 1999,
                             age_2002 %in% c(18:26) ~ 2002,
                             age_1996 %in% c(18:26) ~ 1996,
                             TRUE ~ NA_real_),
         wealth_ea = case_when(
           age_1996 %in% c(27:40) & age_2002 %in% c(27:40) ~ rowMeans(.[,c("pcall1996_1","pcall2002_1")],na.rm=TRUE),
           age_2002 %in% c(27:40)~ pcall2002_1,
           age_1996 %in% c(27:40) ~ pcall1996_1,
           TRUE ~ NA_real_),
         year_ea = case_when(age_1996 %in% c(27:40) & age_2002 %in% c(27:40) ~ 1999,
                             age_2002 %in% c(27:40)~ 2002,
                             age_1996 %in% c(27:40) ~ 1996,
                             TRUE ~ NA_real_)
         
  ) %>% 
  mutate(
         age_child = age_6775,
         age_teen = case_when(year_teen == 1987 ~ age_1987,
                              TRUE ~ NA_real_),
         age_ya = case_when(year_ya == 1996 ~ age_1996,
                            year_ya == 1999 ~ (age_1996 + age_2002)/2,
                            year_ya == 2002 ~ age_2002,
                            TRUE ~ NA_real_),
         age_ea = case_when(year_ea == 1996 ~ age_1996,
                            year_ea == 1999 ~ (age_1996 + age_2002)/2,
                            year_ea == 2002 ~ age_2002,
                            TRUE ~ NA_real_)) %>%  
  dplyr::filter(!is.na(age_child),!is.na(age_teen),!is.na(age_ya),!is.na(age_ea))
# We don't do the above filtering because we assume that wealth at that age is actually available but the participant did not participate


# Summary of gains_df --------
library(compareGroups)
gains_df %>% 
  compareGroups(~age_child + age_teen + age_ya + age_ea + wealth_child + wealth_teen + wealth_ya + wealth_ea,
                data = .,method = c(2,2,2,2,1,1,1,1)) %>% 
  createTable(.,digits = 2,sd.type = 2,q.type = c(2,2)) %>% 
  export2xls(.,file=paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/summary of filtered ids.xlsx"))

saveRDS(gains_df,paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/gains_df.RDS"))
