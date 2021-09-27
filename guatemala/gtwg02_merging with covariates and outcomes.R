
source(paste0(path_gains_repo,"/guatemala/gtmobaux01_covariates.R"))
source(paste0(path_gains_repo,"/guatemala/gtwgaux01_human capital data.R"))
source(paste0(path_gains_repo,"/guatemala/gtwgaux04_96 cvd study.R"))
# hc_dfa <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/hc_dfa.RDS"))


gains_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/gains_df.RDS"))

# Why only 289 out of 461 participants in 96 CVD study are included:
# CVD study: Birth year from 1969 to 1977
# Our sub-cohort: Birth year from 1971 to 1975
# test <- f80903 %>% left_join(guatemala_dfa %>% dplyr::select(id_uni,gtchbyear), by = c("ID_UNI"="id_uni"))
# summary(test$gtchbyear)
# summary(gains_df$gtchbyear)

absolute_df <- gains_df %>% 
  dplyr::select(id_uni,contains("wealth_")) %>% 
  left_join(early_life %>% 
              dplyr::select(id_uni,gtvillage,chsex,chbirtho,gtatole,gtchatoleexposurestatus,
                            gtchbyear,
                            moage,moht_sib,moscho_sib,d_id_unim) %>% 
              mutate(gtchatoleexposurestatus = case_when(gtchatoleexposurestatus == "full" ~ 1,
                                                         TRUE ~ 0),
                     chbirtho = case_when(chbirtho == "first" ~ 1,
                                          chbirtho == "second" ~ 2,
                                          chbirtho == "third" ~ 3,
                                          chbirtho == "fourth or more" ~ 4),
                     chsex = case_when(chsex == "male" ~ 1,
                                       TRUE ~ 0),
                     gtchbyear = gtchbyear - 70 # Centered at median
              ) %>% 
              rename(full = gtchatoleexposurestatus,
                     male = chsex,
                     byear = gtchbyear),
            by="id_uni") %>% 
  # Merge with GTML DFA --------------
left_join(guatemala_dfa %>% 
            dplyr::select(id_uni,gtadeduyr1618,adeduyrc3,
                          adchildren,adnumchildren,ademployment,
                          gtadrelstat2018,gtadrelstat2016) %>% 
            mutate(
                   adchildren = case_when(adchildren == "yes" ~ 1,
                                          adchildren == "no" ~ 0,
                                          TRUE ~ NA_real_),
                   ademployment = case_when(ademployment == "formal" ~ 1,
                                            ademployment %in% c("informal","unemployed") ~ 0,
                                            TRUE ~ NA_real_),
                   # We use gtadrelstat2018 and gtadrelstat2016 since COHORTS DFA doesn't have combined variable -----
                   adrelstat = case_when(gtadrelstat2018 %in% c("married") ~ 1,
                                         gtadrelstat2018 %in% c("dating","in a relationship",
                                                                "single") ~ 0,
                                         gtadrelstat2016 %in% c("2, Married","1, United") ~ 1,
                                         gtadrelstat2016 %in% c("3, Separated or divorced",
                                                                "4, Widowed",
                                                                "5, Single") ~ 0,
                                         TRUE ~ NA_real_)) %>%
            dplyr::select(-gtadrelstat2018,-gtadrelstat2016) %>% 
            
            rename(
                   nchildren18 = adnumchildren,
                   children18 = adchildren,
                   formal18 = ademployment,
                   married18 = adrelstat), 
          by=c("id_uni")
) %>% 
  # Merge with CVD study
  left_join(cvd_data %>% 
              rename(bmi_ya = BMI,
                     # formal_ya = formal9699,
                     pregnant_ya = pregnant98,
                     rural_ya = rural98),
            by=c("id_uni"="ID_UNI")) %>% 
  
  # Merge with Human Capital DFA --------------
  left_join(hc_dfa %>% 
              rename(formal_ea = formal,
                     nchildren_ea = nchildren,
                     children_ea = children,
                     rural_ea = rural04,
                     # alive04 = alive,
                     pregnant_ea = pregnant0204,
                     bmi_ea = bmi04,
                     ravens_ea = ravens),
            by=c("id_uni"="iduni")) %>% 
  
  mutate(eduyr = case_when(!is.na(c3hgrade) ~ c3hgrade,
                           !is.na(adeduyrc3) ~ adeduyrc3,
                           !is.na(gtadeduyr1618) ~ gtadeduyr1618,
                           
                           TRUE ~ NA_real_)) %>% 
  mutate_at(vars(children_ea,contains("formal"),pregnant_ea,contains("rural")), function(x) factor(x,levels=c(0,1),labels=c(0,1))) %>% 
  dplyr::filter(alive02==1|alive96==1) %>% 
  dplyr::filter(!is.na(bmi_ya)|!is.na(bmi_ea)|!is.na(ravens_ea))

# absolute_df %>% group_by(rural) %>% summarize_at(vars(pcall1618_1),~mean(.,na.rm=TRUE))

saveRDS(absolute_df, paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/absolute_df.RDS"))

# Summary of absolute df -----------

absolute_df %>% 
  compareGroups(~wealth_child + wealth_teen + wealth_ya + wealth_ea + 
                  # formal_ya + rural_ya + children_ya + married_ya +
                  formal_ea + rural_ea + children_ea + 
                  # married_ea +
                  bmi_ea + bmi_ya + ravens_ea,
                data = .,method = c(1,1,1,1,
                                    # 3,3,3,
                                    3,3,3,
                                    1,1,2)) %>% 
  createTable(.,digits = 2,sd.type = 2,q.type = c(2,2)) %>% 
  export2xls(.,file=paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/summary of alive.xlsx"))


