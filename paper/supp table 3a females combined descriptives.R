b9_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/absolute_df.RDS")) %>% 
  dplyr::filter(!is.na(bmi_ya)|!is.na(iq_ya)|!is.na(srq_ya)|!is.na(wellbeing_ya),male==0) %>% 
  mutate(site = "Brazil") %>% 
  mutate_at(vars(male),~as.character(.) %>% as.numeric(.)) %>% 
  rename(children_ya = children_2015)

gt_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/absolute_df.RDS")) %>% 
  dplyr::filter(!is.na(bmi_ea)|!is.na(ravens_ea),male==0) %>% 
  rename(moscho = moscho_sib) %>% 
  mutate(site = "Guatemala")

ph_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/absolute_df.RDS")) %>% 
  dplyr::filter(!is.na(bmi_ea)|!is.na(ravens_ea)|!is.na(srq_ea)|!is.na(happiness_ea),male==0) %>% 
  mutate(site = "Philippines") %>% 
  rename(
    srq_ya = stress_ya)

sa_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/absolute_df.RDS")) %>% 
  dplyr::filter(!is.na(bmi_ya)|!is.na(ravens_ea)|!is.na(srq_ea)|!is.na(happiness_ea),male==0) %>% 
  mutate(site = "South Africa")

library(compareGroups)
bind_rows(b9_df,
          gt_df,
          ph_df,
          sa_df) %>% 
  mutate(bmi_ya = case_when(pregnant_ya == 1 ~ NA_real_,
                            
                            TRUE ~ bmi_ya),
         bmi_ea = case_when(pregnant_ea == 1 ~ NA_real_,
                            TRUE ~ bmi_ea)) %>%
  compareGroups(site ~ moscho + moage + 
                  momwhite +
                  chbirtho + male + gtatole + full +
                  white +
                  black + eduyr + formal_ya + pregnant_ya + children_ya + rural_ya + 
                  formal_ea + pregnant_ea + children_ea + rural_ea + 
                  wealth_child + wealth_teen + wealth_ya + wealth_ea +
                  bmi_ya + iq_ya + srq_ya + wellbeing_ya +
                  bmi_ea + ravens_ea + srq_ea + happiness_ea,
                data=.,
                method = c(2,1,
                           3,
                           2,3,3,3,
                           3,
                           3,2,3,3,3,3,
                           3,3,3,3,
                           
                           1,1,1,1,
                           1,2,2,2,
                           1,2,2,2),include.miss = TRUE,include.label = FALSE,simplify = FALSE) %>%
  createTable(.,digits=1,type = 1,show.n = TRUE,q.type = c(1,2),sd.type = 2) %>% 
  export2xls(.,file=paste0(path_dissertation,"/aim 2/working/cohorts gains/supplementary table 3A revised compareGroups.xlsx"))

library(arsenal)

# https://thatdatatho.com/easily-create-descriptive-summary-statistic-tables-r-studio/
table1_control = tableby.control(test=F,total=F,
                                 numeric.stats = c("meansd","medianq1q3","N","Nmiss2"),
                                 cat.stats = c("countpct","N","Nmiss2"),
                                 stats.labels = list(
                                   meansd = "'Mean (SD)",
                                   medianq1q3 = "'Median (Q1, Q3)",
                                   N = "'N",
                                   Nmiss2 = "'Missing"
                                 ))

bind_rows(b9_df,
          gt_df,
          ph_df,
          sa_df) %>% 
  mutate(bmi_ya = case_when(pregnant_ya == 1 ~ NA_real_,
                            TRUE ~ bmi_ya),
         bmi_ea = case_when(pregnant_ea == 1 ~ NA_real_,
                            TRUE ~ bmi_ea)) %>% 
  mutate_at(vars(male,gtatole,full,black,pregnant_ya,white),~as.character(.)) %>% 
  tableby(site ~ moscho + moage + momwhite + chbirtho + male + gtatole + full +
            white + 
            black + eduyr + formal_ya + pregnant_ya + children_ya + rural_ya + 
            formal_ea + pregnant_ea + children_ea + rural_ea + 
            wealth_child + wealth_teen + wealth_ya + wealth_ea +
            bmi_ya + iq_ya + srq_ya + wellbeing_ya +
            bmi_ea + ravens_ea + srq_ea + happiness_ea,data=.,
          control=table1_control,digits=1, digits.p=2, digits.pct=1) %>% 
  summary(.,text=TRUE) %>% 
  xlsx::write.xlsx(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/supplementary table 3A revised arsenal.xlsx"),sheetName = "arsenal table1")

