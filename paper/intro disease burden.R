# http://ghdx.healthdata.org/gbd-results-tool?params=gbd-api-2019-permalink/42deafcb7858692beadb39254bb9176b

# IHME-GBD_2019_DATA-66fa6d80-1.csv has level 2 causes 
# IHME-GBD_2019_DATA-404c8f91-1.csv has level 1 causes

gbd <- read_csv(paste0(path_dissertation,"/aim 2/working/cohorts gains/global/IHME-GBD_2019_DATA-66fa6d80-1/IHME-GBD_2019_DATA-66fa6d80-1.csv")) %>% 
  dplyr::filter(metric_name == "Number",measure_name == "DALYs (Disability-Adjusted Life Years)",location_name !="World Bank Income Levels") %>% 
  mutate(outcome = case_when(cause_name %in% c("Diabetes and kidney diseases",
                                                      "Cardiovascular diseases") ~ "Cardio-metabolic",
                             cause_name %in% c("Mental disorders","Neurological disorders") ~ "Mental health",
                             cause_name == "All causes" ~ "All causes",
                             TRUE ~ NA_character_)) %>% 
  dplyr::filter(!is.na(outcome)) %>% 
  group_by(location_name,age_name,outcome) %>% 
  summarize(val = sum(val)) %>% 
  dplyr::select(location_name,age_name,outcome,val) %>% 
  pivot_wider(names_from='age_name',values_from='val') %>% 
  rename(all_ages = 'All Ages',
         '15 to 49' = '15-49 years') %>% 
  rename_at(vars(matches("(to|plus)")),~paste0("y",str_replace_all(.," ","_"))) %>% 
  mutate(y18_to_19 = y15_to_19*0.4,
         y18_to_39 = y18_to_19 + y20_to_24 + y25_to_29 + y30_to_34 + y35_to_39,
         y15_to_64 = y15_to_49 + y50_to_54 + y55_to_59 + y60_to_64) %>% 
  dplyr::select(location_name,outcome,y18_to_39,y15_to_64) %>% 
  mutate(p18_to_39 = y18_to_39/y15_to_64)


lmics <- gbd %>% 
  dplyr::filter(location_name %in% c("World Bank Low Income",
                                     "World Bank Lower Middle Income",
                                     "World Bank Upper Middle Income")) %>% 
  group_by(outcome) %>% 
  summarize(y18_to_39 = sum(y18_to_39),
            y15_to_64 = sum(y15_to_64)) %>% 
  mutate(p18_to_39 = y18_to_39/y15_to_64)
  
  
# unique(gbd$cause_name)
# [1] "Chronic respiratory diseases"                 "Digestive diseases"                          
# [3] "Other non-communicable diseases"              "Skin and subcutaneous diseases"              
# [5] "Neglected tropical diseases and malaria"      "Cardiovascular diseases"                     
# [7] "Musculoskeletal disorders"                    "All causes"                                  
# [9] "Nutritional deficiencies"                     "Self-harm and interpersonal violence"        
# [11] "Transport injuries"                           "Unintentional injuries"                      
# [13] "Neoplasms"                                    "Neurological disorders"                      
# [15] "Mental disorders"                             "Sense organ diseases"                        
# [17] "HIV/AIDS and sexually transmitted infections" "Respiratory infections and tuberculosis"     
# [19] "Enteric infections"                           "Other infectious diseases"                   
# [21] "Maternal and neonatal disorders"              "Substance use disorders"                     
# [23] "Diabetes and kidney diseases" 