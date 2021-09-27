source(paste0(path_incap_repo,"/rally/clean_glm_result.R"))
source(paste0(path_replication_repo,"/package/clean_mi_conditionalregression.R"))


source(paste0(path_gains_repo,"/philippines/phcw01_fitting models for conditional wealth as outcome.R"))
source(paste0(path_gains_repo,"/philippines/phwgaux02_display results.R"))



bind_rows(clean_mi_conditionalregression(models_teen,link = "lmer identity") %>% 
            mutate(Outcome = "Conditional Wealth TEEN"),
          clean_mi_conditionalregression(models_ya,link = "lmer identity") %>% 
            mutate(Outcome = "Conditional Wealth YA"),
          clean_mi_conditionalregression(models_ea,link = "lmer identity") %>% 
            mutate(Outcome = "Conditional Wealth EA")
) %>% 
  dplyr::select(Outcome,iv,Coefficient) %>% 
  dplyr::filter(iv!="(Intercept)") %>% 
  display_results_ph() %>% 
  arrange(iv) %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/philippines/results_cwealth associations.csv"),row.names = FALSE)


# Coefficient Plot -----------

bind_rows(clean_mi_conditionalregression(models_teen,link = "lmer identity") %>% 
            mutate(Outcome = "Conditional Wealth TEEN"),
          clean_mi_conditionalregression(models_ya,link = "lmer identity") %>% 
            mutate(Outcome = "Conditional Wealth YA"),
          clean_mi_conditionalregression(models_ea,link = "lmer identity") %>% 
            mutate(Outcome = "Conditional Wealth EA")
) %>% 
  dplyr::select(iv,Outcome,theta_D,lci,uci) %>% 
  dplyr::filter(iv %in% c("moscho_sib","male","chbirtho","wealth_child","cwealth_teen",
                          "eduyr","formal_ya1")) %>% 
  display_results_ph(.,type="output") %>% 
  ggplot(data=.,aes(x=theta_D,xmin=lci,xmax=uci,y=iv)) +
  geom_point() +
  geom_errorbar(width = 0) +
  facet_grid(~Outcome,scales="free_x") +
  theme_bw() +
  geom_vline(aes(xintercept=0),col="red",linetype=2) +
  ylab("") + xlab("Estimate (95% CI)")

