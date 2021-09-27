
# 2005 -------------

covariates05 <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/2005/child/employ.dta")) %>% 
  dplyr::select(uncchdid,sex,mainjob,mainemp) %>% 
  mutate(formal2005 = case_when(mainemp %in% c(4:6) & mainjob %in% c(1:239,600:623,671:679,901:906,
                                                                     991) ~ 1,
                                mainemp %in% c(1:3,7) & mainjob %in% c(1:239) ~ 1,
                                mainemp %in% c(1:3,7) ~ 0,
                                TRUE ~ NA_real_)) %>% 
  dplyr::select(-mainjob,-mainemp)   %>% 
  group_by(uncchdid,sex) %>% 
  dplyr::summarize(formal2005 = case_when(sum(formal2005,na.rm=TRUE) > 0 ~ 1,
                                   sum(formal2005,na.rm=TRUE) == 0 ~ 0,
                                   TRUE ~ NA_real_)) %>% 
  full_join(bind_rows(haven::read_dta(paste0(path_harmonization_folder,"/philippines/2005/child/anthdiet.dta"))) %>% 
              rename(weight2005 = weight,
                     height2005 = height) %>% 
              dplyr::select(uncchdid,weight2005,height2005),
            by="uncchdid") %>% 
  full_join(haven::read_dta(paste0(path_harmonization_folder,"/philippines/2005/child/preghist.dta")) %>% 
              group_by(uncchdid) %>% 
              dplyr::filter(pregord == max(pregord)) %>% 
              mutate(pregnant2005 = case_when(
                                              pregterm == 6 ~ 1,
                                              pregterm %in% c(1:5,7) ~ 0,
                                              TRUE ~ NA_real_
              )) %>% 
              dplyr::select(uncchdid,pregnant2005) %>% 
              group_by(uncchdid) %>% 
              dplyr::summarise(pregnant2005 = case_when(sum(pregnant2005,na.rm=TRUE) >0 ~ 1,
                                                        sum(pregnant2005,na.rm=TRUE) == 0 ~ 0,
                                                        TRUE ~ NA_real_)) %>% 
              ungroup() ,
              
            by = "uncchdid") %>% 
  mutate(pregnant2005 = case_when(sex == 1 ~ -1,
                                  is.na(pregnant2005) ~ 0,
                                  TRUE ~ pregnant2005))









# 2009 -----------------

covariates09 <- bind_rows(haven::read_dta(paste0(path_harmonization_folder,"/philippines/2009/males/icwork.dta")),
                          haven::read_dta(paste0(path_harmonization_folder,"/philippines/2009/females/icwork.dta"))) %>% 
  dplyr::select(uncchdid,icsex,mainjob) %>% 
  mutate(formal2009 = case_when(mainjob %in% c(4) ~ 1,
                                mainjob %in% c(0:3,5) ~ 0,
                                TRUE ~ NA_real_)) %>% 
  dplyr::select(-mainjob) %>% 
  full_join(# Doesn't include Males
  haven::read_dta(paste0(path_harmonization_folder,"/philippines/2009/females/preghist.dta")) %>% 
  mutate(pregnant2009 = case_when(
                                  pregterm == 6 ~ 1,
                                  pregterm %in% c(1:5,7) ~ 0,
                                  TRUE ~ NA_real_
  )) %>% 
  dplyr::select(uncchdid,pregnant2009) %>% 
    group_by(uncchdid) %>% 
    dplyr::summarise(pregnant2009 = case_when(sum(pregnant2009,na.rm=TRUE) >0 ~ 1,
                                              sum(pregnant2009,na.rm=TRUE) == 0 ~ 0,
                                              TRUE ~ NA_real_)) %>% 
    ungroup(),
  by="uncchdid") %>% 
  
  full_join(bind_rows(haven::read_dta(paste0(path_harmonization_folder,"/philippines/2009/males/anthbp.dta")),
                      haven::read_dta(paste0(path_harmonization_folder,"/philippines/2009/females/anthbp.dta"))) %>% 
              mutate(weight2009 = rowMeans(.[,c("WEIGHT1","WEIGHT2","WEIGHT3")],na.rm=TRUE)) %>% 
              dplyr::select(uncchdid,weight2009),
              by="uncchdid")
