
# Life status -------------
mastercontrol = read_dta(paste0(path_incap_ses_box,"/BDGT_oriente/MasterID/data/stata_clean/mastercontrol17Apr06.dta")) %>% 
  dplyr::filter(master == 1)

alive2016 <- read_dta(paste0(path_gtml_redcap_data,"/Gates_data_2019-07-12.dta")) %>% 
  dplyr::filter(edo_vida2015 %in% c(1,4,6)) %>% 
  dplyr::select(iduni) %>% 
  pull()
hc_form08 <- read_sas(paste0(path_incap_ses_box,"/RSPH Research/Organized data from old waves/Human Capital/Form 08 (Anthropometry)/form08_apr06.sas7bdat")) %>%
  rename(id_uni = IDUNI) %>%
  # hc_form08 <- read_sas(paste0(path_incap_ses_box,"/RSPH Research/Organized data from old waves/Human Capital/Form 08 (Anthropometry)/ffm_apr06.sas7bdat")) %>%
  dplyr::filter(id_uni %in% mastercontrol$id_uni) %>% 
  dplyr::select(id_uni) %>% 
  pull()
c0411z02 <- read_dta(paste0(path_incap_ses_box,"/BDGT_oriente/Census/acumulado/data/stata_clean/c0411z.dta")) %>%
  dplyr::select(id_uni,master,ev2002,pr2002,en2002) %>%
  dplyr::filter(ev2002==1,master==1)  %>% 
  dplyr::select(id_uni)  %>% 
  pull()

c0411z96 <- read_dta(paste0(path_incap_ses_box,"/BDGT_oriente/Census/acumulado/data/stata_clean/c0411z.dta")) %>%
  dplyr::select(id_uni,master,ev1996,pr1996,en1996) %>%
  dplyr::filter(ev1996==1,master==1)  %>% 
  dplyr::select(id_uni)  %>% 
  pull()

alive1996 <- c(hc_form08,c0411z96,c0411z02,alive2016) %>% unique(.)
alive2002 <- c(hc_form08,c0411z02,alive2016) %>% unique(.)



# Ravens -----------
raven_scoreapr06 <- read_sas(paste0(path_incap_ses_box,"/RSPH Research/Organized data from old waves/Human Capital/Form 05 (Raven)/raven_scoreapr06.sas7bdat"))  %>%
  dplyr::select(ID_UNI,SCORE_A,SCORE_B,SCORE_C) %>% 
  rename(iduni = ID_UNI) %>% 
  mutate(ravens = rowSums(.[,c("SCORE_A","SCORE_B","SCORE_C")],na.rm=TRUE))

# Pregnant -------------
pregbirth_apr06 = read_sas(paste0(path_incap_ses_box,
                                  "/RSPH Research/Organized data from old waves/Human Capital/Form 09 (Reproductive history)/pregbirth_apr06.sas7bdat"))

pregnant_in_hc = pregbirth_apr06 %>% 
  dplyr::filter(CURR_PREG == 1) %>% 
  dplyr::select(ID_UNI) %>% 
  pull()

# Children ----------

f0901_apr06 <- read_dta(paste0(path_incap_ses_box,
                               "/BDGT_oriente/Study02/data/stata_clean/f0901_apr06.dta")) %>% 
  dplyr::select(id_uni,f0901bp09) %>% 
  rename(nchildren = f0901bp09) %>% 
  mutate(children = case_when(nchildren > 0 ~ 1,
                              nchildren == 0 ~ 0,
                              TRUE ~ NA_real_))

# Anthro -------------  
incap_student <- read_sas(paste0(path_incap_ses_box,"/RSPH Research/Requested data sets/Carmen Ng U Penn/incap_student.sas7bdat")) %>% 
  rename(iduni = ID_UNI) %>% 
  dplyr::select(iduni,rururb,SEXHCI01,c3hgrade,age04,age98,
                height98,weight98,height04,weight04) %>% 
  mutate(bmi98 = case_when(!is.na(height98) & !is.na(weight98) ~ weight98/(height98/100)^2,
                           TRUE ~ NA_real_),
         bmi04 = case_when(!is.na(height04) & !is.na(weight04) ~ weight04/(height04/100)^2,
                           TRUE ~ NA_real_),
         rural04 = case_when(rururb == "" ~ NA_real_,
                             rururb == "rural" ~ 1,
                             rururb == "urban" ~ 0,
                             TRUE ~ NA_real_)
         ) %>% 
  dplyr::select(-rururb,-weight98,-weight04) %>% 
  mutate(
         pregnant0204 = case_when(iduni %in% pregnant_in_hc ~ 1,
                                TRUE ~ NA_real_))


# Overall form for employment Form12Apr06-----------
form12Apr06 <- read_dta(paste0(path_incap_ses_box,
                               "/BDGT_oriente/Study02/data/stata_clean/form12Apr06.dta")) %>% 
  mutate(lastweek_paid_work = case_when(f12p06a == 1 ~ 1,
                               f12p06a == 2 ~ 0,
                               TRUE ~ NA_real_),
         lastweek_agriculture = case_when(f12p06b == 1 ~ 1,
                                       f12p06b == 0 ~ 0,
                                       TRUE ~ NA_real_),
         lastweek_business = case_when(f12p06c == 1 ~ 1,
                                       f12p06c == 0 ~ 0,
                                       TRUE ~ NA_real_),
         lastyear_paid_work = case_when(f12p07a == 1 ~ 1,
                                        f12p07a == 2 ~ 0,
                                        TRUE ~ NA_real_),
         lastyear_agriculture = case_when(f12p07b == 1 ~ 1,
                                        f12p07b == 2 ~ 0,
                                        TRUE ~ NA_real_),
         lastyear_business = case_when(f12p07c == 1 ~ 1,
                                       f12p07c == 0 ~ 0,
                                       TRUE ~ NA_real_)
         )

# Form specific for formal employment form12a1Apr06 ------------

# form12a1Apr06 <- read_dta(paste0(path_incap_ses_box,
#                                  "/BDGT_oriente/Study02/data/stata_clean/form12a1Apr06.dta")) %>% 
#   mutate(lastweek_work = case_when(f12a1p01 == 1 ~ 1,
#                                    f12a1p02 == 2 ~ 0,
#                                    TRUE ~ NA_real_),
#          lastyear_work = case_when(f12a1p04 == 1 ~ 1,
#                                    f12a1p04 == 2 ~ 0,
#                                    TRUE ~ NA_real_),
#          official_work = case_when(f12a1p11 %in% c(1:3) ~ 1,
#                                    f12a1p11 == 4 ~ 0,
#                                    TRUE ~ NA_real_),
#          official_work_lastyear = case_when(official_work == 1 & lastyear_work == 1 ~ 1,
#                                             official_work == 0 | lastyear_work == 0 ~ 0,
#                                             TRUE ~ NA_real_)
#          ) %>% 
#   group_by(id_uni) %>% 
#   dplyr::summarise(official_work_lastyear = sum(official_work_lastyear,na.rm=TRUE)) %>% 
#   ungroup() %>% 
#   mutate(official_work_lastyear = case_when(official_work_lastyear > 0 ~ 1,
#                                             TRUE ~ 0))


form12a1Apr06 <- read_dta(paste0(path_incap_ses_box,
                                 "/BDGT_oriente/Study02/data/stata_clean/form12a1Apr06.dta")) %>%
  mutate(lastweek_work = case_when(f12a1p01 == 1 ~ 1,
                                   f12a1p02 == 2 ~ 0,
                                   TRUE ~ NA_real_),
         lastyear_work = case_when(f12a1p04 == 1 ~ 1,
                                   f12a1p04 == 2 ~ 0,
                                   TRUE ~ NA_real_),
         official_work = case_when(f12a1p11 %in% c(1:3) ~ 1,
                                   f12a1p11 == 4 ~ 0,
                                   TRUE ~ NA_real_),
         official_work_lastyear = case_when(official_work == 1 & lastyear_work == 1 ~ 1,
                                            official_work == 0 | lastyear_work == 0 ~ 0,
                                            TRUE ~ NA_real_)
  ) %>%
  group_by(id_uni) %>%
  dplyr::summarise(official_work_lastyear = sum(official_work_lastyear,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(official_work_lastyear = case_when(official_work_lastyear > 0 ~ 1,
                                            TRUE ~ 0))


# Putting it together --------------
hc_dfa <- incap_student %>% 
  left_join(raven_scoreapr06 %>% 
              dplyr::select(iduni,ravens),
            by = "iduni") %>% 
  # Covariates: employment, rural residence, children (yes/no)
  left_join(form12Apr06 %>% 
              dplyr::select(id_uni,contains("lastweek")),
            by = c("iduni"="id_uni")) %>% 
  left_join(form12a1Apr06,
            by = c("iduni"="id_uni")) %>%
  mutate(formal = case_when(official_work_lastyear == 1 ~ 1,
                            lastweek_paid_work == 0 ~ 0,
                            lastweek_agriculture == 1 | lastweek_business == 1 ~ 0,
                            TRUE ~ NA_real_)) %>% 
  dplyr::select(-contains("lastweek"),-official_work_lastyear) %>% 
  left_join(f0901_apr06,
            by=c("iduni"="id_uni")) %>% 
# Additional: participation status in 2002-04 (alive, non-response, died)
  mutate(alive02 = case_when(iduni %in% alive2002 ~ 1,
                           TRUE ~ 0),
         alive96 = case_when(iduni %in% alive1996 ~ 1,
                             TRUE ~ 0)) %>% 
  
  mutate(pregnant0204 = case_when(pregnant0204 == 1 ~ 1,
                                alive02 == 1 & SEXHCI01 == 1 ~ -1,
                                alive02== 1 & (!is.na(bmi04)|!is.na(ravens)) & SEXHCI01 == 2 ~ 0,
                                alive02 == 0 ~ NA_real_,
                                TRUE ~ NA_real_))

saveRDS(hc_dfa,paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/hc_dfa.RDS"))
rm(f0901_apr06,form12a1Apr06,form12Apr06,incap_student,mastercontrol,pregbirth_apr06,raven_scoreapr06)
