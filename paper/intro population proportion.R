
pop_estimates <- read_csv(paste0(path_dissertation,
                                 "/aim 2/working/cohorts gains/global/Data_Extract_From_Population_estimates_and_projections/02e7b2cf-70f9-40c5-8001-8c9bc2a97efb_Data.csv")) %>% 
  dplyr::select(one_of(c('Country Code','Series Code')),contains("2020")) %>% 
  mutate_at(vars(one_of('2020 [YR2020]')),~as.numeric(.)) %>% 
  rename(y2020 = '2020 [YR2020]') %>% 
  dplyr::filter(!is.na(y2020)) %>% 
  pivot_wider(names_from = 'Series Code',values_from='y2020') %>% 
  mutate_at(vars(-one_of("Country Code")),~as.numeric(.)) %>% 
  mutate(total = SP.POP.TOTL,
         a15to64 = SP.POP.1564.TO,
         a18to19f = case_when(is.na(SP.POP.AG18.FE.IN) | is.na(SP.POP.AG19.FE.IN) ~ SP.POP.1519.FE*0.4,
                              TRUE ~ SP.POP.AG18.FE.IN + SP.POP.AG19.FE.IN),
         a18to19m = case_when(is.na(SP.POP.AG18.MA.IN) | is.na(SP.POP.AG19.MA.IN) ~ SP.POP.1519.MA*0.4,
                              TRUE ~ SP.POP.AG18.MA.IN + SP.POP.AG19.MA.IN),
         a20to39 = rowSums(.[,regexpr("(2024|2529|3034|3539)",colnames(.))>0],na.rm=TRUE)
         ) %>% 
  
  mutate(n_yaea = (a18to19f + a18to19m + a20to39),
         pct_yaea = (a18to19f + a18to19m + a20to39)*100/a15to64)
  
wb_class <- readxl::read_excel(paste0(path_dissertation,"/aim 2/working/cohorts gains/global/CLASS.xlsx"))

estimates_by_class <- pop_estimates %>% 
  left_join(wb_class,by=c('Country Code'='Code')) %>% 
  group_by_at(vars(one_of('Income group'))) %>% 
  summarize(n_yaea = sum(n_yaea,na.rm=TRUE),
            n_15to64 = sum(a15to64,na.rm=TRUE)) %>% 
  mutate(pct_yaea = n_yaea/n_15to64)

lmics <- estimates_by_class %>% 
  rename(group = 'Income group') %>% 
  dplyr::filter(group %in% c("Low income","Lower middle income","Upper middle income")) %>% 
  summarize(n_yaea = sum(n_yaea,na.rm=TRUE),
            n_15to64 = sum(n_15to64,na.rm=TRUE)) %>% 
  mutate(pct_yaea = n_yaea/n_15to64)
