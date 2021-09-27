source(paste0(path_incap_repo,"/rally/clean_glm_result.R"))
source(paste0(path_replication_repo,"/package/clean_mi_conditionalregression.R"))


source(paste0(path_gains_repo,"/south africa/sawg05_fitting models.R"))
source(paste0(path_gains_repo,"/south africa/sawgaux02_display results.R"))



bind_rows(clean_mi_conditionalregression(models_bmi_ya,link = "lmer identity") %>% 
            mutate(Outcome = "BMI YA"),
          clean_mi_conditionalregression(models_srq_ea,link = "lmer identity") %>% 
            mutate(Outcome = "SRQ20 z-scores EA"),
          clean_mi_conditionalregression(models_ravens_ea,link = "lmer identity") %>% 
            mutate(Outcome = "IQ z-scores EA"),
          clean_mi_conditionalregression(models_happiness_ea,link = "lmer identity") %>% 
            mutate(Outcome = "WELLBEING z-scores EA")
) %>% 
  dplyr::select(Outcome,iv,Coefficient) %>% 
  dplyr::filter(iv!="(Intercept)") %>% 
  display_results_sa() %>% 
  arrange(iv) %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/results_conditionals.csv"),row.names = FALSE)


# Coefficient Plot -----------

bind_rows(clean_mi_conditionalregression(models_bmi_ya,link = "lmer identity") %>% 
            mutate(Outcome = "BMI YA (kg/m2)"),
          clean_mi_conditionalregression(models_srq_ea,link = "lmer identity") %>% 
            mutate(Outcome = "SRQ20 (z-scores) EA"),
          clean_mi_conditionalregression(models_ravens_ea,link = "lmer identity") %>% 
            mutate(Outcome = "IQ (z-scores) EA"),
          clean_mi_conditionalregression(models_happiness_ea,link = "lmer identity") %>% 
            mutate(Outcome = "WELLBEING (z-scores) EA")
) %>% 
  dplyr::select(iv,Outcome,theta_D,lci,uci) %>% 
  dplyr::filter(iv %in% c("wealth_child","cwealth_teen","cwealth_ya","cwealth_ea",
                          "eduyr","formal1")) %>% 
  display_results_sa(.,type="output") %>% 
  ggplot(data=.,aes(x=theta_D,xmin=lci,xmax=uci,y=iv)) +
  geom_point() +
  geom_errorbar(width = 0) +
  facet_grid(~Outcome,scales="free_x") +
  theme_bw() +
  geom_vline(aes(xintercept=0),col="red",linetype=2) +
  ylab("") + xlab("Estimate (95% CI)")

