
mastercontrol = read_dta(paste0(path_incap_ses_box,"/BDGT_oriente/MasterID/data/stata_clean/mastercontrol17Apr06.dta")) %>% 
  dplyr::filter(master == 1)

adol1dec05 <- read_sas(paste0(path_incap_ses_box,"/BDGT_oriente/Study87_88/data/summary/adol1dec05.sas7bdat")) %>% 
  rename(iduni = ID_UNI)

cogtraj_final <- read_sas(paste0(path_incap_shared_data,"/Students/Ines/cogtraj_final.sas7bdat")) %>% 
  dplyr::select(id_uni,raven1)

adol1dec05 <- adol1dec05 %>% 
  dplyr::filter(iduni %in% mastercontrol$id_uni) %>% 
  dplyr::select(iduni,BMI)
