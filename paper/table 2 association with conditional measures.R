b9_df <- read_csv(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/results_cwealth associations.csv")) %>% 
  mutate(site = "Brazil")
gt_df <- read_csv(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/results_cwealth associations.csv")) %>% 
  mutate(site = "Guatemala")
ph_df <- read_csv(paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/results_cwealth associations.csv")) %>% 
  mutate(site = "Philippines")
sa_df <- read_csv(paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/results_cwealth associations.csv"))%>% 
  mutate(site = "South Africa")

coefs_df <- bind_rows(b9_df,
                      gt_df,
                      ph_df,
                      sa_df) %>% 
  dplyr::filter(iv %in% c("Maternal schooling",
                          "Sex = Male",
                          "Birth order (numeric)",
                          "Attained schooling")|
                  str_detect(iv,"Rural")|
                  str_detect(iv,"Formal employment")) %>% 
  pivot_longer(cols=-one_of("site","iv"),names_to="outcome",values_to="coefficient")  %>% 
  separate(coefficient,into=c("coef","lci","uci"),sep="(\\(|,)") %>% 
  dplyr::mutate(uci = str_replace(uci,"\\)","")) %>% 
  mutate_at(vars(coef,lci,uci),~as.numeric(.)) %>% 
  mutate(iv = case_when(str_detect(iv,"Formal employment") ~ "Formal employment",
                        str_detect(iv,"Rural") ~ "Rural residence",
                        TRUE ~ iv)) %>% 
  mutate(iv = factor(iv,levels=c("Maternal schooling",
                                 "Sex = Male",
                                 "Birth order (numeric)",
                                 "Attained schooling","Rural residence",
                                 "Formal employment"),ordered=TRUE))

coefs_df %>% 
  dplyr::filter(!is.na(coef)) %>% 
  mutate(summary = paste0(coef," (",lci,", ",uci,")")) %>% 
  
  mutate(age_category = case_when(str_detect(outcome," TEEN") ~ "1_School Age",
                                  str_detect(outcome," YA") ~ "2_Young Adult",
                                  TRUE ~ "3_Early Adult")
         
         ) %>% 
  dplyr::select(iv, site, age_category,summary) %>% 
  pivot_wider(names_from = c("site","age_category"),values_from="summary") %>% 
  mutate(iv = factor(as.character(iv),levels=c("Maternal schooling",
                                               "Sex = Male",
                                               "Birth order (numeric)",
                                               "Attained schooling","Rural residence",
                                               "Formal employment"),ordered=TRUE)) %>% 
  arrange(iv) %>% 
  mutate_at(vars(matches("(1|2)")),function(x) case_when(is.na(x) ~ "",
                                                         TRUE ~ x)) %>% 
  dplyr::select(iv,matches("Brazil"),matches("Guatemala"),matches("Philippines"),matches("South Africa")) %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/table 2.csv"),row.names = FALSE)
