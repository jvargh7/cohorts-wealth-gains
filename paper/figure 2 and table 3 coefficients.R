
b9_df <- read_csv(paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/results_conditionals.csv")) %>% 
  mutate(site = "Brazil")  %>% 
  rename_all(~str_replace(.," (EA|YA)",""))
gt_df <- read_csv(paste0(path_dissertation,"/aim 2/working/cohorts gains/guatemala/results_conditionals.csv")) %>% 
  mutate(site = "Guatemala") %>% 
  dplyr::select(-one_of("BMI YA")) %>% 
  rename_all(~str_replace(.," (EA|YA)",""))
ph_df <- read_csv(paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/results_conditionals.csv")) %>% 
  mutate(site = "Philippines") %>% 
  dplyr::select(-one_of("BMI YA","STRESS (z-scores) YA")) %>% 
  rename_all(~str_replace(.," (EA|YA)",""))
sa_df <- read_csv(paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/results_conditionals.csv"))%>% 
  mutate(site = "South Africa") %>% 
  rename_all(~str_replace(.," (EA|YA)",""))

library(scales)
coefs_df <- bind_rows(b9_df,
                      gt_df,
                      ph_df,
                      sa_df) %>% 
  dplyr::filter(iv %in% c("Maternal schooling","Attained schooling","Wealth in childhood",
                          "Conditional wealth 6-17y",
                          "Conditional wealth 18-26y",
                          "Conditional wealth 27-36y")|
                  str_detect(iv,"Formal employment")) %>% 
  pivot_longer(cols=-one_of("site","iv"),names_to="outcome",values_to="coefficient")  %>% 
  separate(coefficient,into=c("coef","lci","uci"),sep="(\\(|,)")%>% 
  dplyr::mutate(uci = str_replace(uci,"\\)","")) %>% 
  mutate_at(vars(coef,lci,uci),~as.numeric(.)) %>% 
  mutate(iv = case_when(str_detect(iv,"Formal employment") ~ "Formal employment",
                        TRUE ~ iv)) %>% 
  mutate(iv = factor(iv,levels=c("Maternal schooling","Wealth in childhood",
                                 "Conditional wealth 6-17y","Conditional wealth 18-26y",
                                 "Conditional wealth 27-36y","Attained schooling","Formal employment"),ordered=TRUE))%>% 
  
  mutate(
    outcome = case_when(str_detect(outcome,"BMI") ~ "1, BMI",
                        str_detect(outcome,"IQ") ~ "2, INTELLIGENCE",
                        str_detect(outcome,"STRESS") ~ "3, DISTRESS",
                        str_detect(outcome,"SRQ") ~ "3, DISTRESS",
                        str_detect(outcome,"WELLBEING") ~ "4, WELLBEING",
                        TRUE ~ NA_character_
    ))

coefs_df %>% 
  dplyr::filter(!is.na(coef)) %>% 
  mutate(summary = paste0(coef," (",lci,", ",uci,")"))  %>% 
  dplyr::select(iv, site, outcome,summary) %>% 
  pivot_wider(names_from = c("site"),values_from="summary") %>% 
  mutate(iv = factor(as.character(iv),levels=c("Maternal schooling","Wealth in childhood",
                                               "Conditional wealth 6-17y","Conditional wealth 18-26y",
                                               "Conditional wealth 27-36y","Attained schooling","Formal employment"),ordered=TRUE)) %>% 
  arrange(outcome,iv) %>% 
  mutate_at(vars(matches("(1|2)")),function(x) case_when(is.na(x) ~ "",
                                                         TRUE ~ x)) %>% 
  dplyr::select(iv,outcome,matches("Brazil"),matches("Guatemala"),matches("Philippines"),matches("South Africa")) %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/table 3.csv"),row.names = FALSE)



# TRIAL FIGURES to figure out limits --------
coefs_df %>% 
  dplyr::filter(!is.na(coef)) %>% 
  ggplot(data=.,aes(x=iv,y=coef,ymin=lci,ymax=uci,col=site)) +
  geom_point(position=position_dodge(width=0.5),shape=15) +
  geom_errorbar(position=position_dodge(width=0.5),width=0) +
  facet_wrap(outcome~.,scales = "free_y") +
  geom_hline(yintercept=0,col="red",linetype=2) +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(legend.position="bottom",
        axis.text.x = element_text(size=8)) +
  scale_color_manual(name="",values=c("purple","red","darkblue","green4")) +
  scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))


# Plotting ----------
# https://stackoverflow.com/questions/51735481/ggplot2-change-axis-limits-for-each-individual-facet-panel
# devtools::install_github("zeehio/facetscales")
# library(facetscales)
# scales_y <- list(
#   `BMI` = scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 1)),
#   `SRQ-20` = scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)),
#   `IQ z-scores` = scale_y_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.25)),
#   `WELLBEING z-scores` = scale_y_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, 0.25))
# )


coef_plot <- function(df,ylims=c(-1,2),y_label=""){
  
  ggplot(data=df,aes(x=iv,y=coef,ymin=lci,ymax=uci,col=site,shape = site)) +
    geom_point(position=position_dodge(width=0.5)) +
    geom_errorbar(position=position_dodge(width=0.5),width=0) +
    geom_hline(yintercept=0,col="red",linetype=2) +
    ylab(y_label) +
    xlab("") +
    theme_bw() +
    theme(legend.position="bottom",
          
          axis.text.x = element_text(size=8)) +
    scale_y_continuous(limits = ylims) +
    scale_color_manual(name="",values=c("purple","red","darkblue","green4")) +
    scale_shape_manual(name="",values=c(1,2,3,4)) +
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n")) %>% 
    return(.)
  
}

# ADULT ------------
bmi_a <- coefs_df %>% 
  dplyr::filter(outcome == "1, BMI") %>% 
  coef_plot(.,ylims=c(-2.5,2.5),y_label="kg/m2")
iq_a <- coefs_df %>% 
  dplyr::filter(outcome == "2, INTELLIGENCE") %>% 
  coef_plot(.,ylims=c(-0.3,0.65),y_label="z-scores")
stress_a <- coefs_df %>% 
  dplyr::filter(outcome == "3, DISTRESS") %>% 
  coef_plot(.,ylims=c(-0.3,0.2),y_label="z-scores")
wellbeing_a <- coefs_df %>% 
  dplyr::filter(outcome == "4, WELLBEING") %>% 
  coef_plot(.,ylims=c(-0.2,0.4),y_label="z-scores")

library(ggpubr)
ggarrange(bmi_a,
          iq_a,
          stress_a,
          wellbeing_a,
          common.legend = TRUE,legend = "bottom",ncol = 2,nrow=2,labels = "AUTO")

