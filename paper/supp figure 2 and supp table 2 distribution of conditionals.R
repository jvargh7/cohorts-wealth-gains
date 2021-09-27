library(mice)

create_df <- function(site,id_var){
  cwealth <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/",site,"/cwealth.RDS"))
  mi_dfs <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/",site,"/mi_dfs.RDS"))
  absolute_df <- readRDS(paste0(path_dissertation,"/aim 2/working/cohorts gains/",site,"/absolute_df.RDS"))
  
  wealth_lifecourse = complete(mi_dfs, 'long') %>% 
    dplyr::select(one_of(id_var),matches("wealth_")) %>% 
    mutate_all(~as.numeric(.)) %>% 
    group_by(across(one_of(id_var))) %>%
    dplyr::summarize(across(matches("wealth_"),
                        .fns = list(mean=mean,var=var),
                        .names = "{col}_{fn}")) %>% 
    rename(id = eval(id_var))
  
  
  
  cwealth_lifecourse = cwealth %>% 
    mutate(gain_mean = apply(.[,regexpr("V",colnames(.))>0],1,mean),
           gain_var = apply(.[,regexpr("V",colnames(.))>0],1,var)) %>% 
    dplyr::select(lifestage,gain_mean,gain_var) %>% 
    mutate(id = rep(complete(mi_dfs,1)[,sym(id_var) %>% as.character(.)],times=length(unique(cwealth$lifestage)))) %>% 
    pivot_wider(names_from="lifestage",values_from=c("gain_mean","gain_var"))
  
  lifecourse_wealth = left_join(wealth_lifecourse,
                                cwealth_lifecourse,
                                by=c("id")) %>% 
    right_join(absolute_df %>% 
                 mutate(non_na_outcome = apply(select(.,one_of(c("bmi_ya","bmi_ea",
                                                                 "ravens_ya","ravens_ea","happiness_ea","wellbeing_ya","iq_ya",
                                                                 "srq_ea","srq_ya","stress_ya"))),
                                               1,function(x) sum(!is.na(x)))) %>% 
                 dplyr::filter(non_na_outcome > 0) %>% 
                 dplyr::select(one_of(id_var)) %>% 
                 rename(id = eval(id_var)),
               by=c("id"))
  
  return(lifecourse_wealth)
}


b9_df <- create_df("brazil 1993","nquest")
gt_df <- create_df("guatemala","id_uni") 
ph_df <- create_df("philippines","uncchdid")
sa_df <- create_df("south africa","bttid")


sfig2_df <- bind_rows(b9_df %>% 
                        mutate(site = "Brazil"),
                      gt_df %>% 
                        mutate(site = "Guatemala"),
                      ph_df %>% 
                        mutate(site = "Philippines"),
                      sa_df %>% 
                        mutate(site = "South Africa")) %>% 
  dplyr::select(-contains("_var")) %>% 
  rename_all(~str_replace_all(.,"(wealth_|_mean)","")) %>% 
  pivot_longer(names_to="lifestage",values_to="wealth",
               cols=one_of(c("child","teen","ya","ea",
                             "gain_teen","gain_ya","gain_ea"))) %>% 
  mutate(lifestage = factor(lifestage,levels=c("child","teen","ya","ea",
                                               "gain_teen","gain_ya","gain_ea"),
                            labels=c("Wealth in childhood",
                                     "Wealth in 6-17y",
                                     "Wealth in 18-26y",
                                     "Wealth in 27-36y",
                                     "Conditional wealth 6-17y",
                                     "Conditional wealth 18-26y",
                                     "Conditional wealth 27-36y")))

# FIG 2 ---------------
sfig2_df %>% 
  dplyr::filter(!lifestage %in% c("Wealth in 6-17y",
                                  "Wealth in 18-26y",
                                  "Wealth in 27-36y")) %>% 
  ggplot(data=.,aes(x=lifestage,y=wealth,fill=site))+
  geom_boxplot() +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(legend.position="bottom",
        axis.text.x = element_text(size=8)) +
  scale_fill_manual(name="",values=c("purple","red","darkblue","green4")) +
  scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n")) 

# Summary of conditionals ------
(stab2_medians <- sfig2_df %>% 
   dplyr::filter(!lifestage %in% c("Wealth in 6-17y",
                                   "Wealth in 18-26y",
                                   "Wealth in 27-36y")) %>% 
   
   ungroup() %>% 
   group_by(lifestage,site) %>% 
   dplyr::summarize(median = quantile(wealth,p=0.5,na.rm=TRUE),
                    q25 = quantile(wealth,p=0.25,na.rm=TRUE),
                    q75 = quantile(wealth,p=0.75,na.rm=TRUE)) %>% 
   mutate(iqr = q75 - q25) %>% 
   mutate(summary = paste0(round(median,2)," [",round(q25,2),", ",round(q75,2),"]")) %>% 
   dplyr::select(-median,-q25,-q75,-iqr) %>% 
   pivot_wider(names_from="lifestage",values_from="summary")) %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/supplementary table 2 medians.csv"),row.names = FALSE)


(stab2_means <- sfig2_df %>% 
    dplyr::filter(!lifestage %in% c("Wealth in 6-17y",
                                    "Wealth in 18-26y",
                                    "Wealth in 27-36y")) %>% 
    
    ungroup() %>% 
    group_by(lifestage,site) %>% 
    dplyr::summarize(mean = mean(wealth,na.rm=TRUE),
                     sd = sd(wealth,na.rm=TRUE)) %>% 
    mutate(summary = paste0(round(mean,2),", ",round(sd,2)) %>% as.character(.))) %>% 
  dplyr::select(-mean,-sd) %>% 
  pivot_wider(names_from="lifestage",values_from="summary") %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/supplementary table 2 mean.csv"),row.names = FALSE)

(stab2_sd <- bind_rows(b9_df %>% 
            mutate(site = "Brazil"),
          gt_df %>% 
            mutate(site = "Guatemala"),
          ph_df %>% 
            mutate(site = "Philippines"),
          sa_df %>% 
            mutate(site = "South Africa")) %>% 
  dplyr::select(id,site,contains("_var")) %>% 
    rename_all(~str_replace_all(.,"(wealth_|_var)","")) %>% 
  pivot_longer(names_to="lifestage",values_to="var_wealth",cols=one_of(c("child_var","gain_var_teen","gain_var_ya","gain_var_ea"))) %>% 
  mutate(lifestage = factor(lifestage,levels=c("child_var","gain_var_teen","gain_var_ya","gain_var_ea"),
                            labels=c("Wealth in childhood",
                                     "Conditional wealth 6-17y",
                                     "Conditional wealth 18-26y",
                                     "Conditional wealth 27-36y"))) %>% 
  ungroup() %>% 
  group_by(lifestage,site) %>% 
  dplyr::summarize(mean = mean(var_wealth,na.rm=TRUE) %>% sqrt(.)) %>% 
  mutate(summary = paste0(round(mean,2)) %>% as.character(.)) %>% 
  dplyr::select(-mean) %>% 
  pivot_wider(names_from="lifestage",values_from="summary")) %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/supplementary table 2 wealth average sd.csv"),row.names = FALSE) 
  

# Share of variance: between vs within ---------
stab2_sd %>% 
  pivot_longer(cols=-site,names_to = "lifestage",values_to="betweenvar") %>% 
  left_join(stab2_means,
            by=c("lifestage","site")) %>% 
  mutate(withinvar = sd^2,
         betweenvar = as.numeric(betweenvar)^2) %>% 
  mutate(prop_between = 100*(betweenvar/(withinvar + betweenvar))) %>% 
  dplyr::select(site,lifestage,prop_between) %>% 
  pivot_wider(names_from="lifestage",values_from="prop_between")


# Share of variance: conditional vs actual ----------

(stab2_transfer <- sfig2_df %>% 
   dplyr::filter(!lifestage %in% c("Wealth in childhood")) %>% 
   
   ungroup() %>% 
   group_by(lifestage,site) %>% 
   dplyr::summarize(
                    sd = sd(wealth,na.rm=TRUE)) %>% 
   ungroup() %>% 
   mutate(age = case_when(str_detect(lifestage,"6-17y") ~ "teen",
                          str_detect(lifestage,"18-26y") ~ "ya",
                          str_detect(lifestage,"27-36y") ~ "ea"),
          measure = case_when(str_detect(lifestage,"Conditional") ~ "conditional",
                              str_detect(lifestage,"Wealth") ~ "wealth",
                              TRUE ~ NA_character_),
          var = sd^2) %>% 
  dplyr::select(site,var,age,measure) %>% 
  pivot_wider(names_from="measure",values_from="var") %>% 
  dplyr::mutate(proportion = conditional*100/wealth %>% round(.,3)) %>% 
   dplyr::select(site,age,proportion) %>% 
   pivot_wider(names_from="age",values_from=proportion)) %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/supplementary table 2 transfer.csv"),row.names = FALSE)


# Test for heterogeneity of variances -----------

sites = unique(sfig2_df$site)
stages = unique(sfig2_df$lifestage)


levene_output = expand.grid(sites,stages,stages) %>% 
  as.data.frame() %>% 
  dplyr::filter(Var2 !=Var3) %>% 
  dplyr::filter(! (Var1=="Brazil" & Var2=="Conditional wealth 27-36y"),
                ! (Var1=="Brazil" & Var3=="Conditional wealth 27-36y")) %>% 
  mutate(min = apply(.[,c("Var2","Var3")],1,min),
         max = apply(.[,c("Var2","Var3")],1,max)) %>% 
  dplyr::distinct(Var1,min,max,.keep_all = TRUE) %>% 
  dplyr::select(-min,-max) %>% 
  mutate(var1 = NA,
         var2 = NA,
         df_group = NA,
         df_free = NA,
         F_statistic = NA,
         p_value = NA)

for (i in 1:nrow(levene_output)){
  s = levene_output[i,]$Var1
  v2 = levene_output[i,]$Var2 %>% as.character()
  v3 = levene_output[i,]$Var3 %>% as.character()
  
  df = sfig2_df %>% 
    dplyr::filter(site == s,lifestage %in% c(v2,v3))
  
  
  
  test_result = car::leveneTest(wealth ~ lifestage,data=df)
  
  levene_output[i,]$var1 = var(df[df$lifestage==v2,]$wealth)
  levene_output[i,]$var2 = var(df[df$lifestage==v3,]$wealth)
  
  levene_output[i,]$df_group = test_result$Df[1]
  levene_output[i,]$df_free = test_result$Df[2]
  levene_output[i,]$F_statistic = test_result$`F value`[1]
  levene_output[i,]$p_value = test_result$`Pr(>F)`[1] %>% round(.,3)
  
  
}

levene_output %>% 
  mutate(res = paste0("F = ",round(F_statistic,1),", p = ",round(p_value,3))) %>% 

write.csv(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/levene_output.csv"),row.names = FALSE)

# Bartlett's test -----------
bartlett_output = expand.grid(sites,stages,stages) %>% 
  as.data.frame() %>% 
  dplyr::filter(Var2 !=Var3) %>% 
  dplyr::filter(! (Var1=="Brazil" & Var2=="Conditional wealth 27-36y"),
                ! (Var1=="Brazil" & Var3=="Conditional wealth 27-36y")) %>% 
  mutate(min = apply(.[,c("Var2","Var3")],1,min),
         max = apply(.[,c("Var2","Var3")],1,max)) %>% 
  dplyr::distinct(Var1,min,max,.keep_all = TRUE) %>% 
  dplyr::select(-min,-max) %>% 
  mutate(var1 = NA,
         var2 = NA,
         statistic = NA,
         p_value = NA)

for (i in 1:nrow(bartlett_output)){
  s = bartlett_output[i,]$Var1
  v2 = bartlett_output[i,]$Var2 %>% as.character()
  v3 = bartlett_output[i,]$Var3 %>% as.character()
  
  df = sfig2_df %>% 
    dplyr::filter(site == s,lifestage %in% c(v2,v3))
  
  
  
  test_result = bartlett.test(wealth ~ lifestage,data=df)
  
  bartlett_output[i,]$var1 = var(df[df$lifestage==v2,]$wealth)
  bartlett_output[i,]$var2 = var(df[df$lifestage==v3,]$wealth)
  
  bartlett_output[i,]$statistic = test_result$statistic
  bartlett_output[i,]$p_value = test_result$p.value
  
  
}

bartlett_output %>% 
  mutate(res = paste0("K = ",round(statistic,1),", p = ",round(p_value,3))) %>% 
  
  write.csv(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/bartlett_output.csv"),row.names = FALSE)

# SFIG 2 -------------
sfig2_df %>% 
  ggplot(data=.,aes(x=lifestage,y=wealth))+
  geom_path(aes(group=id),col="grey80",alpha=0.1) +
  geom_point(col="grey") +
  geom_hline(yintercept=0,col="red",linetype=2) +
  theme_bw()  +
  scale_y_continuous(limits=c(-4,4)) +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14)) +
  ylab("Asset index (z-scores)") +
  xlab("") +
  facet_wrap(~site) +
  scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
