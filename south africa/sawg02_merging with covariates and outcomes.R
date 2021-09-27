gains_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/gains_df.RDS"))


source(paste0(path_gains_repo,"/south africa/samobaux01_covariates.R"))
source(paste0(path_gains_repo,"/south africa/sawgaux01_wellbeing total.R"))


absolute_df <- gains_df %>% 
  dplyr::select(bttid,starts_with("wealth"),starts_with("age")) %>% 
  
  left_join(early_life %>% 
              dplyr::select(bttid,moage,moscho,fascho,ethnicity,chsex,chbirtho) %>% 
              mutate(
                chbirtho = case_when(chbirtho == "first" ~ 1,
                                     chbirtho == "second" ~ 2,
                                     chbirtho == "third" ~ 3,
                                     chbirtho == "fourth or more" ~ 4),
                chsex = case_when(chsex == "male" ~ 1,
                                  TRUE ~ 0),
                ethnicity = case_when(ethnicity == "black" ~ 1,
                                      ethnicity %in% c("coloured","indian","white") ~ 0,
                                      TRUE ~ NA_real_)
              ) %>% 
              rename(
                black = ethnicity,
                male = chsex),
            by="bttid") %>% 
  left_join(adult_outcomes_eduyr,
            by = "bttid")  %>% 
  left_join(southafrica_dfa %>% 
              dplyr::select(bttid,adbmi,adsrq,adravenstotscore,
                            adchildren,adnumchildren,ademployment,
                            adrelstat) %>% 
              mutate(adchildren = case_when(adchildren == "yes" ~ 1,
                                            adchildren == "no" ~ 0,
                                            TRUE ~ NA_real_),
                     ademployment = case_when(ademployment == "formal" ~ 1,
                                              ademployment %in% c("informal","unemployed") ~ 0,
                                              TRUE ~ NA_real_),
                     adrelstat = case_when(adrelstat %in% c("married") ~ 1,
                                           adrelstat %in% c("dating","in a relationship",
                                                            "single") ~ 0,
                                           TRUE ~ NA_real_)),
            by = "bttid") %>% 
  left_join(sa_wellbeing %>% 
              dplyr::select(bttid,d_happiness_tot_imp), 
              by="bttid") %>% 
  left_join(southafrica_pregnant,
            by = c("bttid"="site_pin")) %>% 
  mutate(currpregnant = case_when(male == 1 ~ -1,
                                  currpregnant == "1,Yes" ~ 1,
                                  currpregnant == "0, No" ~ 0,
                                  TRUE ~ NA_real_)) %>% 
  rename(bmi_ya = adbmi,
         srq_ea = adsrq,
         ravens_ea = adravenstotscore,
         happiness_ea = d_happiness_tot_imp,
         pregnant_ea = currpregnant,
         children_ea = adchildren,
         nchildren_ea = adnumchildren,
         formal_ea = ademployment,
         married_ea = adrelstat) %>% 
  
  
  # dplyr::filter(!is.na(wealth_ya)) %>% 
  mutate_all(~as.numeric(.)) %>% 
  mutate_at(vars(children_ea,formal_ea,pregnant_ea,
                 married_ea), 
            function(x) factor(x,levels=c(0,1),labels=c(0,1))) %>% 
  dplyr::filter(!is.na(bmi_ya)|!is.na(srq_ea)|!is.na(ravens_ea)|!is.na(happiness_ea))



saveRDS(absolute_df, paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/absolute_df.RDS"))
