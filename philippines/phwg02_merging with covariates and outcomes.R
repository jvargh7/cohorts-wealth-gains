
gains_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/gains_df.RDS"))

source(paste0(path_harmonization_repo,"/philippines/phses01_filtering rows.R"))
source(paste0(path_harmonization_repo,"/philippines/ph_region.R"))

ph_region <- ph_region %>% 
  pivot_wider(names_from="survey",values_from="cstratum",names_prefix = "cstratum")

source(paste0(path_gains_repo,"/philippines/phmobaux01_covariates.R"))
source(paste0(path_gains_repo,"/philippines/phwgaux01_wellbeing total.R"))
source(paste0(path_gains_repo,"/philippines/phwgaux03_perceived stress.R"))
source(paste0(path_gains_repo,"/philippines/phwgaux04_young adulthood covariates.R"))


absolute_df <- gains_df %>% 
  dplyr::select(uncchdid,starts_with("wealth"),starts_with("age")) %>% 
  left_join(ph_region,
            by="uncchdid") %>% 
  mutate_at(vars(starts_with("cstratum")),function(x) case_when(!is.na(x) ~ x - 1,
                                                                TRUE ~ NA_real_)) %>% 
  rename_at(vars(starts_with("cstratum")), ~str_replace(.,"cstratum","rural")) %>% 
  left_join(early_life %>% 
              # momarst: Only 2.44% are unmarried and 1.3% are missing
              dplyr::select(uncchdid,moage,moscho,chsex,chbirtho) %>% 
              mutate(
                chbirtho = case_when(chbirtho == "first" ~ 1,
                                     chbirtho == "second" ~ 2,
                                     chbirtho == "third" ~ 3,
                                     chbirtho == "fourth or more" ~ 4),
                chsex = case_when(chsex == "male" ~ 1,
                                  TRUE ~ 0)
              ) %>% 
              rename(
                male = chsex),
            by="uncchdid") %>% 
  left_join(philippines_dfa %>% 
              dplyr::select(uncchdid,adeduyr,adbmi,
                            # adbmic3,
                            adsrq,adravenstotscore,adchildren,ademployment,adrelstat) %>% 
              mutate(adchildren = case_when(adchildren == "yes" ~ 1,
                                            adchildren == "no" ~ 0,
                                            TRUE ~ NA_real_),
                     ademployment = case_when(ademployment == "formal" ~ 1,
                                              ademployment %in% c("informal","unemployed") ~ 0,
                                              TRUE ~ NA_real_),
                     adrelstat = case_when(adrelstat %in% c("married") ~ 1,
                                           adrelstat %in% c("dating","in a relationship",
                                                            "single") ~ 0,
                                           TRUE ~ NA_real_)
              ) %>% 
              rename(eduyr = adeduyr,
                     # Removing adbmic3 for now. Adding at a later step from raw data
                     # bmi_ya = adbmic3,
                     bmi_ea = adbmi,
                     srq_ea = adsrq,
                     ravens_ea = adravenstotscore,
                     children_ea = adchildren,
                     # nchildren = adnumchildren,
                     formal_ea = ademployment,
                     married_ea = adrelstat
              ), 
            by=c("uncchdid")
  ) %>% 
  # Young adult Perceived Stress -----------
  left_join(morbidity05 %>% 
              dplyr::select(uncchdid,stress05),
            by="uncchdid") %>% 
  left_join(morbidity09 %>% 
              dplyr::select(uncchdid,stress09),
            by="uncchdid") %>% 
  mutate(stress_ya = case_when(!is.na(stress09) & !is.na(stress05) ~ rowMeans(.[,c("stress09","stress05")],na.rm=TRUE),
                               !is.na(stress09) ~ stress09,
                               !is.na(stress05) ~ stress05,
                               TRUE ~ NA_real_),
         # rural_teen variable added -------
         rural_teen = case_when(!is.na(rural1998) & !is.na(rural1994) & !is.na(rural1991) ~ rural1991,
                                !is.na(rural1991) & !is.na(rural1994) ~ rural1991,
                                !is.na(rural1991) & !is.na(rural1998) ~ rural1991,
                                !is.na(rural1994) & !is.na(rural1998) ~ rural1994,
                                !is.na(rural1991) ~ rural1991,
                                !is.na(rural1994) ~ rural1994,
                                !is.na(rural1998) ~ rural1998,
                                TRUE ~ NA_real_),
         rural_ya = case_when(!is.na(rural2009) & !is.na(rural2005) & !is.na(rural2002) ~ rural2002,
                              !is.na(rural2002) & !is.na(rural2005) ~ rural2002,
                              !is.na(rural2002) & !is.na(rural2009) ~ rural2002,
                              !is.na(rural2005) & !is.na(rural2009) ~ rural2005,
                              !is.na(rural2002) ~ rural2002,
                              !is.na(rural2005) ~ rural2005,
                              !is.na(rural2009) ~ rural2009,
                              TRUE ~ NA_real_),
         rural_ea = rural2018) %>% 
  
  # Early adult wellbeing ------------
  left_join(ph_wellbeing %>% 
              dplyr::select(uncchdid,d_happiness_tot_imp) %>% 
              rename(happiness_ea = d_happiness_tot_imp),
            by=c("uncchdid")) %>% 
  # Early adult pregnancy ---------
  
  left_join(philippines_pregnant,
            by=c("uncchdid"="site_pin")) %>% 
  mutate(pregnant2018 = case_when(male == 1 ~ -1,
                                  pregnant2018 == "1,Yes" ~ 1,
                                  pregnant2018 == "0, No" ~ 0,
                                  TRUE ~ NA_real_)) %>% 
  rename(pregnant_ea = pregnant2018) %>% 
  
  # Young adult covariates -----------
  left_join(covariates05 %>% 
              dplyr::select(-sex),
            by="uncchdid") %>% 
  left_join(covariates09 %>% 
              dplyr::select(-icsex),
            by="uncchdid") %>% 
  mutate(formal_ya = case_when(!is.na(formal2005) & !is.na(formal2009) ~ formal2005,
                               !is.na(formal2005) ~ formal2005,
                               !is.na(formal2009) ~ formal2009,
                               TRUE ~ NA_real_),
         weight_ya = case_when(!is.na(weight2005) & !is.na(weight2009) ~ weight2005,
                               !is.na(weight2005) ~ weight2005,
                               !is.na(weight2009) ~ weight2009,
                               TRUE ~ NA_real_),
         bmi_ya = case_when(weight_ya < 0 | height2005 <0 ~ NA_real_,
                            !is.na(height2005) & !is.na(weight_ya) ~ weight_ya/(height2005/100)^2,
                            TRUE ~ NA_real_),
         
         pregnant_ya = case_when(male == 1 ~ -1,
                                 !is.na(weight2005) & !is.na(weight2009) ~ pregnant2005,
                                 !is.na(weight2005) ~ pregnant2005,
                                 !is.na(weight2009) ~ pregnant2009,
                                 TRUE ~ NA_real_)
         ) %>% 
  
  # dplyr::filter(!is.na(pc2018)) %>% 
  mutate_all(~as.numeric(.)) %>% 
  mutate_at(vars(children_ea,formal_ea,married_ea,pregnant_ea,
                 formal_ya,contains("rural")), 
            function(x) factor(x,levels=c(0,1),labels=c(0,1))) %>% 
  dplyr::filter(!is.na(bmi_ya)|!is.na(stress_ya)|!is.na(bmi_ea)|!is.na(srq_ea)|!is.na(ravens_ea)|!is.na(happiness_ea))

saveRDS(absolute_df, paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/absolute_df.RDS"))

