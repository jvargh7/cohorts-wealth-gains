source(paste0(path_incap_repo,"/rally/clean_glm_result.R"))
source(paste0(path_replication_repo,"/package/clean_mi_conditionalregression.R"))


source(paste0(path_gains_repo,"/brazil 1993/b9cw01_fitting models for conditional wealth as outcome.R"))
source(paste0(path_gains_repo,"/brazil 1993/b9wgaux02_display results.R"))



bind_rows(clean_mi_conditionalregression(models_teen,link = "lmer identity") %>% 
            mutate(Outcome = "Conditional Wealth TEEN"),
          clean_mi_conditionalregression(models_ya,link = "lmer identity") %>% 
            mutate(Outcome = "Conditional Wealth YA")
) %>% 
  dplyr::select(Outcome,iv,Coefficient) %>% 
  dplyr::filter(iv!="(Intercept)") %>% 
  display_results_b9() %>% 
  arrange(iv) %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/results_cwealth associations.csv"),row.names = FALSE)


# Coefficient Plot -----------

bind_rows(clean_mi_conditionalregression(models_teen,link = "lmer identity") %>% 
            mutate(Outcome = "Conditional Wealth TEEN"),
          clean_mi_conditionalregression(models_ya,link = "lmer identity") %>% 
            mutate(Outcome = "Conditional Wealth YA")
) %>% 
  dplyr::select(iv,Outcome,theta_D,lci,uci) %>% 
  dplyr::filter(iv %in% c("moscho","male1","chbirtho","wealth_child","cwealth_teen",
                          "eduyr","formal_ya1")) %>% 
  display_results_b9(.,type="output") %>% 
  ggplot(data=.,aes(x=theta_D,xmin=lci,xmax=uci,y=iv)) +
  geom_point() +
  geom_errorbar(width = 0) +
  facet_grid(~Outcome,scales="free_x") +
  theme_bw() +
  geom_vline(aes(xintercept=0),col="red",linetype=2) +
  ylab("") + xlab("Estimate (95% CI)")

