source(paste0(path_gains_repo,"/brazil 1993/b9mobaux01_covariates.R"))
source(paste0(path_gains_repo,"/brazil 1993/b9wgaux01_adult covariates by life stage.R"))
gains_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/gains_df.RDS"))

absolute_df <- gains_df %>%
  dplyr::select(nquest,starts_with("wealth"),starts_with("age")) %>% 
  left_join(early_life %>% 
              dplyr::select(nquest,moage,moscho,b9moskincolor,faage, fascho,adskincolor,chbirtho,chsex) %>% 
              mutate(
                chbirtho = case_when(chbirtho == "first" ~ 1,
                                     chbirtho == "second" ~ 2,
                                     chbirtho == "third" ~ 3,
                                     chbirtho == "fourth or more" ~ 4),
                chsex = case_when(chsex == "male" ~ 1,
                                  TRUE ~ 0),
                adskincolor = case_when(adskincolor == "white" ~ 1,
                                        adskincolor %in% c("black","other") ~ 0,
                                        TRUE ~ NA_real_),
                b9moskincolor = case_when(b9moskincolor == "white" ~ 1,
                                          b9moskincolor %in% c("black","other") ~ 0,
                                          TRUE ~ NA_real_)
              ) %>% 
              rename(
                male = chsex,
                white = adskincolor,
                momwhite = b9moskincolor),
            by="nquest") %>% 
  left_join(adult_outcomes %>% 
              dplyr::select(nquest,schooling,bmi,iq,srq,wellbeing) %>% 
              rename(
                eduyr = schooling,
                bmi_ya = bmi,
                iq_ya = iq,
                srq_ya = srq,
                wellbeing_ya = wellbeing),
            by = "nquest")  %>% 
  left_join(adult_covariates %>% 
              dplyr::select(nquest,children_2011,children_2015,pregnant_anthro_2015) %>% 
              rename(pregnant_anthro = pregnant_anthro_2015),
            by="nquest") %>% 
  
  left_join(brazil1993_dfa %>% 
              dplyr::select(nquest,ademployment,adrelstat),
            by="nquest") %>% 
  
  mutate(pregnant_anthro = case_when(male == 1 ~ -1,
                                     pregnant_anthro == 1 ~ 1,
                                     pregnant_anthro == 0 ~ 0,
                                     TRUE ~ NA_real_),
         ademployment = case_when(ademployment == "formal" ~ 1,
                                  ademployment %in% c("informal","unemployed") ~ 0,
                                  TRUE ~ NA_real_),
         adrelstat = case_when(adrelstat %in% c("married") ~ 1,
                               adrelstat %in% c("dating","in a relationship",
                                                "single") ~ 0,
                               TRUE ~ NA_real_)) %>%
  rename(pregnant_ya = pregnant_anthro,
         formal_ya = ademployment,
         married_ya = adrelstat) %>% 
  mutate_all(~as.numeric(.)) %>% 
  mutate_at(vars(children_2015,children_2011,married_ya,
                 formal_ya,white,momwhite,male,contains("rural")), function(x) factor(x,levels=c(0,1),labels=c(0,1))) %>% 
  dplyr::filter(!is.na(bmi_ya)|!is.na(iq_ya)|!is.na(srq_ya)|!is.na(wellbeing_ya))


saveRDS(absolute_df, paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/absolute_df.RDS"))
