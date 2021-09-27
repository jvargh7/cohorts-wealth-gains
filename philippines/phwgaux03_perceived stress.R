
# PSS14 https://www.gem-measures.org/public/DownloadMeasure.aspx?mid=103
# Reverse code: 4,5,6,7,9,10

# upset: 14.1
# control: 14.2
# nervous: 14.3
# success: 14.4
# effectiv: 14.5
# confiden: 14.6
# thingsgo: 14.7
# copthing: 14.8
# irritate: 14.9
# topthing: 14.10


morbidity05 <- haven::read_dta(paste0(path_harmonization_folder,"/philippines/2005/child/moreheal.dta")) %>% 
  dplyr::select(uncchdid,upset:topthing) %>% 
  mutate_at(vars(success,effectiv,confiden,thingsgo,irritate,topthing),function(x) 4-x) %>% 
  mutate(stress05 = rowSums(select(.,-uncchdid)))

morbidity09 <- bind_rows(haven::read_dta(paste0(path_harmonization_folder,"/philippines/2009/females/morbidty.dta")),
                         haven::read_dta(paste0(path_harmonization_folder,"/philippines/2009/males/morbidty.dta")))%>% 
  dplyr::select(uncchdid,upset:topthing) %>% 
  mutate_at(vars(success,effectiv,confiden,thingsgo,irritate,topthing),function(x) 4-x) %>% 
  mutate(stress09 = rowSums(select(.,-uncchdid)))
