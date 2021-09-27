b9_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/alive_df.RDS")) %>% 
  mutate(lifestatus = case_when(participated_ya == 1 ~ 3,
                                alive_ya == 1 ~ 2,
                                alive_ya == 0 ~ 1,
                                TRUE ~ NA_real_)) %>% 
  mutate(chsex = case_when(chsex == "male" ~ 1,
                           chsex == "female" ~ 0,
                           TRUE ~ NA_real_),
         chbirtho = case_when(chbirtho == "first" ~ 1,
                              chbirtho == "second" ~ 2,
                              chbirtho == "third" ~ 3,
                              chbirtho == "fourth or more" ~ 4))  %>% 
  rename(
         male = chsex,
         id = nquest) %>%
  dplyr::select(id,moscho,moage,moemployedcurr,moskincolor,
                male,chbirtho,lifestatus)

gt_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/alive_df.RDS")) %>% 
  mutate(lifestatus = case_when(participated_ea == 1 ~ 3,
                                alive_ea == 1 ~ 2,
                                alive_ea == 0 ~ 1,
                                TRUE ~ NA_real_)) %>% 
  mutate(chsex = case_when(chsex == "male" ~ 1,
                           chsex == "female" ~ 0,
                           TRUE ~ NA_real_),
         chbirtho = case_when(chbirtho == "first" ~ 1,
                              chbirtho == "second" ~ 2,
                              chbirtho == "third" ~ 3,
                              chbirtho == "fourth or more" ~ 4)) %>% 
  dplyr::select(-moscho) %>% 
  rename(wealth_child = pcall6775_1,
         male = chsex,
         moscho = moscho_sib,
         id = id_uni)  %>%
  dplyr::select(id,moscho,moage,wealth_child,
                male,chbirtho,gtatole,lifestatus)

ph_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/alive_df.RDS")) %>% 
  mutate(lifestatus = case_when(participated_ea == 1 ~ 3,
                                alive_ea == 1 ~ 2,
                                alive_ea == 0 ~ 1,
                                TRUE ~ NA_real_)) %>% 
  mutate(chsex = case_when(chsex == "male" ~ 1,
                           chsex == "female" ~ 0,
                           TRUE ~ NA_real_),
         chbirtho = case_when(chbirtho == "first" ~ 1,
                              chbirtho == "second" ~ 2,
                              chbirtho == "third" ~ 3,
                              chbirtho == "fourth or more" ~ 4),
         cstratum1983 = cstratum1983 - 1)  %>% 
  rename(wealth_child = pc1983,
         male = chsex,
         rural_child = cstratum1983,
         id = uncchdid) %>%
  dplyr::select(id,moscho,moage,wealth_child,
                male,chbirtho,rural_child,lifestatus)

sa_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/alive_df.RDS")) %>% 
  mutate(chsex = case_when(chsex == "male" ~ 1,
                           chsex == "female" ~ 0,
                           TRUE ~ NA_real_),
         chbirtho = case_when(chbirtho == "first" ~ 1,
                              chbirtho == "second" ~ 2,
                              chbirtho == "third" ~ 3,
                              chbirtho == "fourth or more" ~ 4),
         ethnicity = case_when(ethnicity == "black" ~ 1,
                               ethnicity %in% c("coloured","indian","white") ~ 0,
                               TRUE ~ NA_real_))  %>% 
  mutate(lifestatus = case_when(participated_ea == 1 ~ 3,
                                TRUE ~ 2))  %>% 
  rename(wealth_child = pc1990,
         male = chsex,
         black = ethnicity,
         id = bttid)  %>%
  dplyr::select(id,moscho,moage,wealth_child,
                male,chbirtho,black,lifestatus)

# STABLE 1-------

stable1_df <- bind_rows(b9_df %>% 
                          mutate(site = "Brazil 1993"),
                        gt_df %>% 
                          mutate(site = "Guatemala"),
                        ph_df %>% 
                          mutate(site = "Philippines"),
                        sa_df %>% 
                          mutate(site = "South Africa"))  %>% 
  mutate(lifestatus = factor(lifestatus,levels=c(1,2,3),labels=c("Died","Did not participate",
                                                                 "Provided Outcome")))
library(compareGroups)
compareGroups(data=stable1_df%>% dplyr::filter(site=="Brazil 1993"),
                            lifestatus ~ moscho + moage + moemployedcurr + moskincolor +
                              chbirtho + male + 
                              gtatole + 
                              wealth_child + rural_child + 
                              black,
                            method = c(2,1,3,3,
                                       2,3,
                                       3,
                                       2,3,
                                       3),include.miss = TRUE,simplify = TRUE) %>% 
  createTable(.,digits=1,q.type = c(2,2),sd.type = 2,type = 1) %>% 
  export2xls(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/supplementary table 1A Brazil 1993.xlsx"))

compareGroups(data=stable1_df%>% dplyr::filter(site=="Guatemala"),
              lifestatus ~ moscho + moage + moemployedcurr + moskincolor +
                chbirtho + male + 
                gtatole + 
                wealth_child + rural_child + 
                black,
              method = c(2,1,3,3,
                         2,3,
                         3,
                         2,3,
                         3),include.miss = TRUE,simplify = TRUE) %>% 
  createTable(.,digits=1,q.type = c(2,2),sd.type = 2,type = 1) %>% 
  export2xls(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/supplementary table 1B Guatemala.xlsx"))


compareGroups(data=stable1_df%>% dplyr::filter(site=="Philippines"),
              lifestatus ~ moscho + moage + moemployedcurr + moskincolor +
                chbirtho + male + 
                gtatole + 
                wealth_child + rural_child + 
                black,
              method = c(2,1,3,3,
                         2,3,
                         3,
                         2,3,
                         3),include.miss = TRUE,simplify = TRUE) %>% 
  createTable(.,digits=1,q.type = c(2,2),sd.type = 2,type = 1) %>% 
  export2xls(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/supplementary table 1C Philippines.xlsx"))

compareGroups(data=stable1_df%>% dplyr::filter(site=="South Africa"),
              lifestatus ~ moscho + moage + moemployedcurr + moskincolor +
                chbirtho + male + 
                gtatole + 
                wealth_child + rural_child + 
                black,
              method = c(2,1,3,3,
                         2,3,
                         3,
                         2,3,
                         3),include.miss = TRUE,simplify = TRUE) %>% 
  createTable(.,digits=1,q.type = c(2,2),sd.type = 2,type = 1) %>% 
  export2xls(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/supplementary table 1D South Africa.xlsx"))
