f80903 <- read_sas(paste0(path_incap_ses_box,"/RSPH Research/Labkey Sandbox/Generational 1998-99/Cardio/f80903.sas7bdat")) %>% 
  mutate(date_bmi = ymd(paste0("19",ANOE,sprintf("%02d",MESE),sprintf("%02d",DIAE))))

# Gonzalez Casanova, Ines - INCAP shared data\Organized data from old waves\Generational 1998-99\forms\Physical Activity.pdf
f80504 <- read_sas(paste0(path_incap_ses_box,"/RSPH Research/Labkey Sandbox/Generational 1998-99/Cardio/f80504.sas7bdat")) %>% 
  dplyr::select(ID_UNI,V101) %>% 
  mutate(formal9699 = case_when(V101 %in% c() ~ 1,
                                V101 %in% c() ~ 0,
                                TRUE ~ NA_real_))

lifestyl <- read_sas(paste0(path_incap_ses_box,"/RSPH Research/Labkey Sandbox/Generational 1998-99/Cardio/Lifestyl.sas7bdat")) %>% 
  dplyr::select(ID_UNI,RESIDEN3) %>% 
  mutate(rural98 = 1 - RESIDEN3)
# RESIDEN3 was chosen instead of RESIDENC and RESIDEN2
# Ramirez-Zea 2010 JNutr Table 2 has 119 people from outside the study villages
# We assume everyone outside study villages was urban

pg2anth_all <- read_sas(paste0(path_incap_ses_box,"/RSPH Research/Labkey Sandbox/Generational 1998-99/Form 6/pg2anth_all.sas7bdat")) %>% 
  dplyr::select(ID_UNIM,starts_with("DOI5B")) %>% 
  # Check Ge1996-99.doc; 
  # starts_with("DOI5B"): Anthro of women who are pregnant
  # starts_with("DOI5C"): Anthro of women who are breastfeeding
  pivot_longer(cols=-ID_UNIM,names_to="variable",values_to="doi") %>% 
  dplyr::filter(!is.na(doi)) %>% 
  dplyr::filter(year(doi) %in% c(1998,1999)) %>% 
  group_by(ID_UNIM) %>% 
  dplyr::summarize(min = min(doi),
                   max = max(doi)) %>% 
  ungroup() %>% 
  mutate(diff = max - min) %>% 
  left_join(f80903 %>% 
              dplyr::select(ID_UNI,date_bmi),
            by=c("ID_UNIM"="ID_UNI"))

pregnant_in_CVD = pg2anth_all %>% 
  dplyr::filter(!is.na(date_bmi)) %>% 
  dplyr::select(ID_UNIM) %>% 
  pull()

cvd_data <- f80903 %>% 
  mutate(pregnant98 = case_when(ID_UNI %in% pregnant_in_CVD ~ 1,
                                SEXO == 1 ~ -1,
                                SEXO ==2 ~ 0,
                                TRUE ~ NA_real_))  %>% 
  dplyr::select(ID_UNI, BMI,pregnant98) %>% 
  left_join(lifestyl %>% 
              dplyr::select(ID_UNI,rural98),
            by = "ID_UNI")
